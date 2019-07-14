(ns pathological.testing
  (:require
    [pathological.files :as f]
    [pathological.paths :as p])
  (:import
    [com.google.common.jimfs Configuration
                             Feature
                             Jimfs
                             PathNormalization
                             PathType]
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
   :unix  "unix"})

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
           path-equality-uses-canonical-form?
           attribute-views
           features]
    :or   {name-canonical-normalization       #{}
           path-equality-uses-canonical-form? false}}]
  (let [path-type (->path-type path-type)
        features (->features features)

        [first-root other-roots] (->roots roots)

        [first-attribute-view other-attribute-views]
        (->attribute-views attribute-views)

        [first-path-normalization other-path-normalizations]
        (->path-normalizations name-canonical-normalization)

        builder
        (-> (Configuration/builder path-type)
          (.setWorkingDirectory working-directory)
          (.setPathEqualityUsesCanonicalForm path-equality-uses-canonical-form?)
          (.setSupportedFeatures features))

        builder (if first-root
                  (.setRoots builder first-root other-roots)
                  builder)
        builder (if first-attribute-view
                  (.setAttributeViews builder
                    first-attribute-view other-attribute-views)
                  builder)
        builder (if first-path-normalization
                  (.setNameCanonicalNormalization builder
                    first-path-normalization other-path-normalizations)
                  builder)]
    (.build builder)))

(def unix-configuration
  (configuration
    {:path-type         :unix
     :roots             ["/"]
     :working-directory "/"
     :attribute-views   #{:basic :owner :posix :unix}
     :features          #{:links
                          :symbolic-links
                          :secure-directory-stream
                          :file-channel}}))

(def windows-configuration
  (configuration
    {:path-type                          :windows
     :roots                              ["C:\\"]
     :working-directory                  "C:\\"
     :name-canonical-normalization       #{:case-fold-ascii}
     :path-equality-uses-canonical-form? true
     :attribute-views                    #{:basic}
     :features                           #{:links
                                           :symbolic-links
                                           :file-channel}}))

(defn new-in-memory-file-system
  ([] (new-in-memory-file-system (random-file-system-name)))
  ([name] (new-in-memory-file-system name unix-configuration))
  ([name configuration] (new-in-memory-file-system name configuration []))
  ([name configuration definition]
   (let [file-system (Jimfs/newFileSystem name configuration)]
     (f/populate-file-tree (p/path file-system "/") definition)
     file-system)))
