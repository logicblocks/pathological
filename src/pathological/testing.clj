(ns pathological.testing
  (:refer-clojure :exclude [name])
  (:require
    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.utils :as u]
    [pathological.attribute-specs :as as])
  (:import
    [com.google.common.jimfs Configuration
                             Feature
                             Jimfs
                             PathNormalization
                             PathType Configuration$Builder]
    [java.util UUID]))

(def ^:dynamic *features*
  {:links                   Feature/LINKS
   :symbolic-links          Feature/SYMBOLIC_LINKS
   :secure-directory-stream Feature/SECURE_DIRECTORY_STREAM
   :file-channel            Feature/FILE_CHANNEL})

(def ^:dynamic *attribute-views*
  {:basic "basic"
   :owner "owner"
   :posix "posix"
   :unix  "unix"
   :dos   "dos"
   :acl   "acl"
   :user  "user"})

(def ^:dynamic *path-normalizations*
  {:case-fold-ascii   PathNormalization/CASE_FOLD_ASCII
   :case-fold-unicode PathNormalization/CASE_FOLD_UNICODE
   :none              PathNormalization/NONE
   :nfc               PathNormalization/NFC
   :nfd               PathNormalization/NFD})

(def ^:dynamic *path-types*
  {:unix    (PathType/unix)
   :windows (PathType/windows)})

(defn random-file-system-name []
  (str (UUID/randomUUID)))

(defn lookup-for [var]
  (fn [value] (or (get var value) value)))

(def ->feature (lookup-for *features*))
(def ->attribute-view (lookup-for *attribute-views*))
(def ->path-type (lookup-for *path-types*))
(def ->path-normalization (lookup-for *path-normalizations*))

(defn ->attribute-views [attribute-views]
  (let [attribute-views (map ->attribute-view attribute-views)
        [first & others] attribute-views]
    [first (into-array String others)]))

(defn ->features [features]
  (let [features (map ->feature features)
        features (into-array Feature features)]
    features))

(defn ->roots [[first-root & other-roots]]
  [first-root (into-array String other-roots)])

(defn ->path-normalizations [path-normalizations]
  (let [path-normalizations (map ->path-normalization path-normalizations)
        [first & others] path-normalizations]
    [first (into-array PathNormalization others)]))

(defn configuration
  [{:keys [path-type
           roots
           working-directory
           name-canonical-normalization
           name-display-normalization
           path-equality-uses-canonical-form?
           attribute-views
           default-attribute-values
           features
           block-size
           max-size
           max-cache-size]
    :or   {name-canonical-normalization       #{}
           name-display-normalization         #{}
           default-attribute-values           {}
           path-equality-uses-canonical-form? false}}]
  (let [path-type (->path-type path-type)
        features (->features features)

        [first-root other-roots] (->roots roots)

        [first-attribute-view other-attribute-views]
        (->attribute-views attribute-views)

        [first-canonical-normalization other-canonical-normalizations]
        (->path-normalizations name-canonical-normalization)

        [first-display-normalization other-display-normalizations]
        (->path-normalizations name-display-normalization)

        ^Configuration$Builder
        builder
        (cond-> (Configuration/builder path-type)
          features (.setSupportedFeatures features)
          block-size (.setBlockSize block-size)
          max-size (.setMaxSize max-size)
          max-cache-size (.setMaxCacheSize max-cache-size)
          working-directory (.setWorkingDirectory working-directory)
          first-root
          (.setRoots first-root other-roots)
          first-attribute-view
          (.setAttributeViews first-attribute-view other-attribute-views)
          first-canonical-normalization
          (.setNameCanonicalNormalization
            first-canonical-normalization other-canonical-normalizations)
          first-display-normalization
          (.setNameDisplayNormalization
            first-display-normalization other-display-normalizations)
          path-equality-uses-canonical-form?
          (.setPathEqualityUsesCanonicalForm
            path-equality-uses-canonical-form?))

        builder
        (reduce
          (fn [builder [attribute-spec value]]
            (.setDefaultAttributeValue builder
              (as/->attribute-spec-string attribute-spec)
              (u/->attribute-value attribute-spec value)))
          builder
          default-attribute-values)]
    (.build builder)))

(def unix-defaults
  {:path-type         :unix
   :roots             ["/"]
   :working-directory "/"
   :attribute-views   #{:basic :owner :posix :unix}
   :features          #{:links
                        :symbolic-links
                        :secure-directory-stream
                        :file-channel}})

(def osx-defaults
  {:path-type                    :unix
   :roots                        ["/"]
   :working-directory            "/"
   :attribute-views              #{:basic}
   :name-display-normalization   #{:nfc}
   :name-canonical-normalization #{:nfd :case-fold-ascii}
   :features                     #{:links
                                   :symbolic-links
                                   :file-channel}})

(def windows-defaults
  {:path-type                          :windows
   :roots                              ["C:\\\\"]
   :working-directory                  "C:\\\\"
   :name-canonical-normalization       #{:case-fold-ascii}
   :path-equality-uses-canonical-form? true
   :attribute-views                    #{:basic}
   :features                           #{:links
                                         :symbolic-links
                                         :file-channel}})

(defn new-in-memory-file-system
  [options]
  (let [{:keys [name working-directory contents]
         :or   {name     (random-file-system-name)
                contents []}} options

        configuration (configuration options)
        file-system (Jimfs/newFileSystem name configuration)
        working-path (p/path file-system working-directory)]
    (f/populate-file-tree working-path contents)
    file-system))

(defn new-unix-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge unix-defaults overrides)))

(defn new-osx-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge osx-defaults overrides)))

(defn new-windows-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge windows-defaults overrides)))
