(ns pathological.testing
  (:refer-clojure :exclude [name])
  (:require
    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.utils :as u]
    [pathological.file-systems :as fs]
    [pathological.attribute-specs :as as])
  (:import
    [java.util UUID]
    [java.io IOException]
    [java.net URI]
    [java.lang.reflect Method]
    [com.google.common.jimfs Configuration
     Configuration$Builder
     Feature
     Jimfs
     JimfsFileSystems
     PathNormalization
     PathType]
    [org.mockito Mockito]
    [org.mockito.stubbing Answer Stubber]
    [org.mockito.invocation InvocationOnMock]
    [java.nio.file AccessMode
     CopyOption
     DirectoryStream$Filter
     LinkOption
     OpenOption
     Path]
    [java.nio.file.attribute FileAttribute]
    [java.util.concurrent ExecutorService]
    [clojure.lang Reflector]
    [java.nio.file.spi FileSystemProvider]))

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
          features
          (.setSupportedFeatures features)

          block-size
          (.setBlockSize block-size)

          max-size
          (.setMaxSize max-size)

          max-cache-size
          (.setMaxCacheSize max-cache-size)

          working-directory
          (.setWorkingDirectory working-directory)

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

        ^Configuration$Builder
        builder
        (reduce
          (fn [^Configuration$Builder builder [attribute-spec value]]
            (.setDefaultAttributeValue builder
              (as/->attribute-spec-string attribute-spec)
              (u/->attribute-value attribute-spec value)))
          builder
          default-attribute-values)]
    (.build builder)))

(def ^:private provider-methods
  {'java.nio.file.spi.FileSystemProvider#newInputStream
   {:method-name          'newInputStream
    :arguments-signatures [[Path [OpenOption]]]
    :matching-calls       #{#'pathological.files/new-input-stream
                            #'pathological.files/new-buffered-reader
                            #'pathological.files/read-all-lines
                            #'pathological.files/lines-stream
                            #'pathological.files/lines
                            #'pathological.files/copy}}

   'java.nio.file.spi.FileSystemProvider#newOutputStream
   {:method-name          'newOutputStream
    :arguments-signatures [[Path [OpenOption]]]
    :matching-calls       #{#'pathological.files/new-output-stream
                            #'pathological.files/new-buffered-writer
                            #'pathological.files/write-lines
                            #'pathological.files/copy}}

   'java.nio.file.spi.FileSystemProvider#newFileChannel
   {:method-name          'newFileChannel
    :arguments-signatures [[Path #{OpenOption} [FileAttribute]]]
    :matching-calls       #{#'pathological.files/lines-stream
                            #'pathological.files/lines}}

   'java.nio.file.spi.FileSystemProvider#newAsynchronousFileChannel
   {:method-name          'newAsynchronousFileChannel
    :arguments-signatures [[Path #{OpenOption} ExecutorService [FileAttribute]]]
    :matching-calls       #{}}

   'java.nio.file.spi.FileSystemProvider#newByteChannel
   {:method-name          'newByteChannel
    :arguments-signatures [[Path #{OpenOption} [FileAttribute]]]
    :matching-calls       #{#'pathological.files/create-file
                            #'pathological.files/create-temp-file
                            #'pathological.files/read-all-bytes}}

   'java.nio.file.spi.FileSystemProvider#newDirectoryStream
   {:method-name          'newDirectoryStream
    :arguments-signatures [[Path DirectoryStream$Filter]]
    :matching-calls       #{#'pathological.files/find-stream
                            #'pathological.files/find
                            #'pathological.files/walk-stream
                            #'pathological.files/walk
                            #'pathological.files/walk-file-tree
                            #'pathological.files/list-stream
                            #'pathological.files/list
                            #'pathological.files/new-directory-stream}}

   'java.nio.file.spi.FileSystemProvider#createDirectory
   {:method-name          'createDirectory
    :arguments-signatures [[Path [FileAttribute]]]
    :matching-calls       #{#'pathological.files/create-directory
                            #'pathological.files/create-directories
                            #'pathological.files/create-temp-directory}}

   'java.nio.file.spi.FileSystemProvider#createSymbolicLink
   {:method-name          'createSymbolicLink
    :arguments-signatures [[Path Path [FileAttribute]]]
    :matching-calls       #{#'pathological.files/create-symbolic-link}}

   'java.nio.file.spi.FileSystemProvider#createLink
   {:method-name          'createLink
    :arguments-signatures [[Path Path]]
    :matching-calls       #{#'pathological.files/create-link}}

   'java.nio.file.spi.FileSystemProvider#delete
   {:method-name          'delete
    :arguments-signatures [[Path]]
    :matching-calls       #{#'pathological.files/delete
                            #'pathological.files/delete-recursively}}

   'java.nio.file.spi.FileSystemProvider#deleteIfExists
   {:method-name          'deleteIfExists
    :arguments-signatures [[Path]]
    :matching-calls       #{#'pathological.files/delete-if-exists}}

   'java.nio.file.spi.FileSystemProvider#readSymbolicLink
   {:method-name          'readSymbolicLink
    :arguments-signatures [[Path]]
    :matching-calls       #{#'pathological.files/read-symbolic-link}}

   'java.nio.file.spi.FileSystemProvider#copy
   {:method-name          'copy
    :arguments-signatures [[Path Path [CopyOption]]]
    :matching-calls       #{#'pathological.files/copy
                            #'pathological.files/copy-recursively}}

   'java.nio.file.spi.FileSystemProvider#move
   {:method-name          'move
    :arguments-signatures [[Path Path [CopyOption]]]
    :matching-calls       #{#'pathological.files/move
                            #'pathological.files/move-recursively}}

   'java.nio.file.spi.FileSystemProvider#isSameFile
   {:method-name          'isSameFile
    :arguments-signatures [[Path Path]]
    :matching-calls       #{#'pathological.files/same-file?}}

   'java.nio.file.spi.FileSystemProvider#isHidden
   {:method-name          'isHidden
    :arguments-signatures [[Path]]
    :matching-calls       #{#'pathological.files/hidden?}}

   'java.nio.file.spi.FileSystemProvider#checkAccess
   {:method-name          'checkAccess
    :arguments-signatures [[Path [AccessMode]]]
    :matching-calls       #{#'pathological.files/exists?
                            #'pathological.files/not-exists?
                            #'pathological.files/readable?
                            #'pathological.files/writable?
                            #'pathological.files/executable?}}

   'java.nio.file.spi.FileSystemProvider#getFileAttributeView
   {:method-name          'getFileAttributeView
    :arguments-signatures [[Path Class [LinkOption]]]
    :matching-calls       #{#'pathological.files/set-posix-file-permissions
                            #'pathological.files/read-file-attribute-view}}

   'java.nio.file.spi.FileSystemProvider#readAttributes
   {:method-name          'readAttributes
    :arguments-signatures [[Path Class [LinkOption]]
                           [Path String [LinkOption]]]
    :matching-calls       #{#'pathological.files/find-stream
                            #'pathological.files/find
                            #'pathological.files/walk-stream
                            #'pathological.files/walk
                            #'pathological.files/walk-file-tree
                            #'pathological.files/size
                            #'pathological.files/read-posix-file-permissions
                            #'pathological.files/read-owner
                            #'pathological.files/set-owner
                            #'pathological.files/read-last-modified-time
                            #'pathological.files/set-last-modified-time
                            #'pathological.files/read-attribute
                            #'pathological.files/read-attributes
                            #'pathological.files/regular-file?
                            #'pathological.files/directory?
                            #'pathological.files/symbolic-link?}}

   'java.nio.file.spi.FileSystemProvider#setAttributes
   {:method-name          'setAttributes
    :arguments-signatures [[Path String Object [LinkOption]]]
    :matching-calls       #{#'pathological.files/set-attribute}}})

(defn- normalise-file-attribute-value [attribute-spec value]
  (u/<-attribute-value attribute-spec
    (u/->attribute-value attribute-spec value)))

(defn- normalise-file-attributes-map [file-system initial-map]
  (reduce-kv
    (fn [normalised-map key value]
      (let [attribute-spec (as/->attribute-spec key)
            value (if (fn? value) (value file-system) value)]
        (assoc normalised-map
          attribute-spec
          (normalise-file-attribute-value attribute-spec value))))
    {}
    initial-map))

(defn- normalise-erroring-argument [file-system value type]
  (condp = type
    Path (str value)
    [FileAttribute] (normalise-file-attributes-map file-system value)
    value))

(defn- normalise-erroring-argument-seq [file-system values types]
  (let [values
        (if (vector? (last types))
          (let [non-varargs-count (dec (count types))
                non-varargs (take non-varargs-count values)
                varargs (drop non-varargs-count values)]
            (cond
              (empty? varargs) non-varargs
              (map? (first varargs)) (concat non-varargs varargs)
              :else (concat non-varargs [varargs])))
          values)]
    (mapv (partial normalise-erroring-argument file-system) values types)))

(defn- normalise-invocation-argument [value type]
  (condp = type
    Path (str value)
    Class (u/<-file-attribute-view-class value)
    #{OpenOption} (u/<-open-options-array value)
    [OpenOption] (u/<-open-options-array value)
    [CopyOption] (u/<-copy-options-array value)
    [LinkOption] (u/<-link-options-array value)
    [AccessMode] (u/<-access-modes-array value)
    [FileAttribute] (u/<-file-attributes-array value)
    value))

(defn- normalise-invocation-argument-array [values types]
  (let [values
        (if (vector? (last types))
          (let [non-varargs-count (dec (count types))
                non-varargs (take non-varargs-count values)
                varargs (drop non-varargs-count values)]
            (concat non-varargs [varargs]))
          values)
        normalised (mapv normalise-invocation-argument values types)
        normalised (if (and (seq normalised) (map? (first (last normalised))))
                     (concat (butlast normalised) [(first (last normalised))])
                     normalised)]
    normalised))

(defn- same-arguments [erroring-arguments invocation-arguments]
  (= (take (count erroring-arguments) invocation-arguments)
    erroring-arguments))

(defn- matches-erroring-argument-specs?
  [erroring-argument-specs invocation-arguments]
  (some
    (fn [erroring-arguments]
      (same-arguments erroring-arguments invocation-arguments))
    erroring-argument-specs))

(defn- should-error? [erroring-argument-specs invocation-arguments]
  (or (empty? erroring-argument-specs)
    (matches-erroring-argument-specs?
      erroring-argument-specs
      invocation-arguments)))

(defn- throw-on-match [file-system erroring-argument-specs argument-types]
  ^Stubber
  (Mockito/doAnswer
    (reify Answer
      (answer [_ ^InvocationOnMock invocation]
        (let [method (.getMethod invocation)
              method-name (.getName method)
              invocation-arguments (.getArguments invocation)
              invocation-arguments
              (when (seq erroring-argument-specs)
                (normalise-invocation-argument-array
                  invocation-arguments argument-types))
              erroring-argument-specs
              (when (seq erroring-argument-specs)
                (mapv
                  #(normalise-erroring-argument-seq
                     file-system % argument-types)
                  erroring-argument-specs))]
          (if (should-error? erroring-argument-specs invocation-arguments)
            (throw (IOException.
                     (str "Errored executing: '" method-name
                       "' with arguments: " invocation-arguments
                       " as requested.")))
            (.callRealMethod invocation)))))))

(defn- matchers-for [argument-types]
  (map
    (fn [_] (Mockito/any))
    argument-types))

(defn- on-call
  [file-system-provider method argument-types what-to-do]
  (Reflector/invokeInstanceMethod
    (.when ^Stubber what-to-do file-system-provider)
    (clojure.core/name method)
    (into-array Object (matchers-for argument-types))))

(defn- lookup-provider-methods-for [method]
  (if (contains? provider-methods method)
    [(get provider-methods method)]
    (filter
      (fn [{:keys [matching-calls]}]
        (contains? matching-calls method))
      (vals provider-methods))))

(defn- configure-error-on
  [file-system [method argument-specs]]
  (let [provider-methods-for-method (lookup-provider-methods-for method)]
    (doseq
     [{:keys [method-name arguments-signatures]} provider-methods-for-method
      argument-signature arguments-signatures]
      (on-call (fs/provider file-system) method-name argument-signature
        (throw-on-match file-system argument-specs argument-signature)))))

(defn- new-errorable-file-system-provider [file-system]
  ^FileSystemProvider (Mockito/spy (fs/provider file-system)))

(defn- consolidate-error-definitions [error-definitions]
  (letfn [(aggregate-argument-specs [error-definition]
            (remove nil? (mapv second error-definition)))]
    (->> error-definitions
      (group-by first)
      (reduce-kv
        (fn [acc method error-definitions]
          (assoc acc
            method (aggregate-argument-specs error-definitions)))
        {}))))

(defn- configure-errors-on [file-system error-definitions]
  (let [error-definitions (consolidate-error-definitions error-definitions)]
    (run! #(configure-error-on file-system %)
      error-definitions)
    file-system))

(defn- new-jimfs-file-system [provider uri configuration]
  (let [method
        (first
          (filter
            #(= (.getName ^Method %)
               (clojure.core/name 'newFileSystem))
            (.getDeclaredMethods JimfsFileSystems)))]
    (.setAccessible ^Method method true)
    (.invoke ^Method method nil
      (into-array Object [provider uri configuration]))))

(defn new-in-memory-file-system [options]
  (let [{:keys [name working-directory contents error-on]
         :or   {name     (random-file-system-name)
                contents []
                error-on #{}}} options

        configuration (configuration options)
        file-system (Jimfs/newFileSystem name configuration)
        file-system (if (seq error-on)
                      (new-jimfs-file-system
                        (new-errorable-file-system-provider file-system)
                        (URI. Jimfs/URI_SCHEME name nil nil)
                        configuration)
                      file-system)]
    (f/populate-file-tree (p/path file-system working-directory) contents)
    (configure-errors-on file-system error-on)))

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

(defn new-unix-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge unix-defaults overrides)))

(defn new-osx-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge osx-defaults overrides)))

(defn new-windows-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge windows-defaults overrides)))
