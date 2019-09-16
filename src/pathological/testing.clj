(ns pathological.testing
  (:refer-clojure :exclude [name class])
  (:require
    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.utils :as u]
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
     JimfsFileSystem
     JimfsFileSystemProvider
     JimfsFileSystems
     PathNormalization
     PathType]
    [org.mockito Mockito Matchers]
    [org.mockito.stubbing Answer Stubber]
    [org.mockito.invocation InvocationOnMock]))

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

(defn- provider [^JimfsFileSystem file-system]
  ^JimfsFileSystemProvider (.provider file-system))

(defn- throw-if-requested [definitions names]
  ^Stubber
  (Mockito/doAnswer
    (reify Answer
      (answer [_ ^InvocationOnMock invocation]
        (let [method (.getMethod invocation)]
          (if (some #(contains? definitions %) names)
            (throw (IOException.
                     (str "Errored executing: '" (.getName method)
                       "' as requested.")))
            (.callRealMethod invocation)))))))

(defn- spy-for [thing]
  (Mockito/spy thing))

(defn- on-call [^JimfsFileSystemProvider spy call]
  (let [arg-count (count (second call))
        arg-matchers (repeat arg-count '(Matchers/any))
        what-to-do ^Stubber (nth call 2)]
    `(.
       ^JimfsFileSystemProvider
       (.when
         ^Stubber ~what-to-do
         ^JimfsFileSystemProvider ~spy)
       ~(first call)
       ~@arg-matchers)))

(defmacro ^:private configure-on [spy & calls]
  `(do
     ~@(map #(on-call spy %) calls)
     ~spy))

(defn- new-jimfs-file-system [provider uri configuration]
  (let [method
        (first (filter #(= (.getName ^Method %) "newFileSystem")
                 (.getDeclaredMethods JimfsFileSystems)))]
    (.setAccessible ^Method method true)
    (.invoke ^Method method nil
      (into-array Object [provider uri configuration]))))

(defn- new-erroring-jimfs-file-system-provider
  [jimfs-file-system-provider definitions]
  (let [spy (spy-for jimfs-file-system-provider)]
    (configure-on spy
      (newInputStream [path args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newInputStream
            #'pathological.files/new-input-stream
            #'pathological.files/new-buffered-reader
            #'pathological.files/read-all-lines
            #'pathological.files/lines-stream
            #'pathological.files/lines
            #'pathological.files/copy}))

      (newOutputStream [path args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newOutputStream
            #'pathological.files/new-output-stream
            #'pathological.files/new-buffered-writer
            #'pathological.files/write-lines
            #'pathological.files/copy}))

      (newFileChannel [path options args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newFileChannel
            #'pathological.files/lines-stream
            #'pathological.files/lines}))

      (newAsynchronousFileChannel [path options executor args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newAsynchronousFileChannel}))

      (newByteChannel [path options args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newByteChannel
            #'pathological.files/create-file
            #'pathological.files/create-temp-file
            #'pathological.files/read-all-bytes}))

      (newDirectoryStream [dir filter]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#newDirectoryStream
            #'pathological.files/find-stream
            #'pathological.files/find
            #'pathological.files/walk-stream
            #'pathological.files/walk
            #'pathological.files/walk-file-tree
            #'pathological.files/list-stream
            #'pathological.files/list
            #'pathological.files/new-directory-stream}))

      (createDirectory [dir args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#createDirectory
            #'pathological.files/create-directory
            #'pathological.files/create-directories
            #'pathological.files/create-temp-directory}))

      (createSymbolicLink [link target args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#createSymbolicLink
            #'pathological.files/create-symbolic-link}))

      (createLink [link existing]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#createLink
            #'pathological.files/create-link}))

      (delete [path]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#delete
            #'pathological.files/delete
            #'pathological.files/delete-recursively}))

      (deleteIfExists [path]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#deleteIfExists
            #'pathological.files/delete-if-exists}))

      (readSymbolicLink [path]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#readSymbolicLink
            #'pathological.files/read-symbolic-link}))

      (copy [source target args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#copy
            #'pathological.files/copy
            #'pathological.files/copy-recursively}))

      (move [source target args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#move
            #'pathological.files/move
            #'pathological.files/move-recursively}))

      (isSameFile [first second]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#isSameFile
            #'pathological.files/same-file?}))

      (isHidden [path]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#isHidden
            #'pathological.files/hidden?}))

      (checkAccess [path args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#checkAccess
            #'pathological.files/exists?
            #'pathological.files/not-exists?
            #'pathological.files/readable?
            #'pathological.files/writable?
            #'pathological.files/executable?}))

      (getFileAttributeView [path type args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#getFileAttributeView
            #'pathological.files/set-posix-file-permissions
            #'pathological.files/read-file-attribute-view}))

      (readAttributes [path type-or-attributes args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#readAttributes
            #'pathological.files/find-stream
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
            #'pathological.files/symbolic-link?}))

      (setAttribute [path attribute value args]
        (throw-if-requested definitions
          #{'java.nio.file.spi.FileSystemProvider#readAttributes
            #'pathological.files/set-attribute})))))

(defn- new-erroring-jimfs-file-system
  [file-system name configuration definitions]
  (let [uri (URI. "jimfs" name nil nil)
        provider (new-erroring-jimfs-file-system-provider
                   (provider file-system) definitions)]
    (new-jimfs-file-system provider uri configuration)))

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
  (let [{:keys [name working-directory contents error-on]
         :or   {name     (random-file-system-name)
                contents []
                error-on #{}}} options

        configuration (configuration options)
        file-system (Jimfs/newFileSystem name configuration)
        working-path (p/path file-system working-directory)]
    (f/populate-file-tree working-path contents)
    (if (seq error-on)
      (new-erroring-jimfs-file-system file-system name configuration error-on)
      file-system)))

(defn new-unix-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge unix-defaults overrides)))

(defn new-osx-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge osx-defaults overrides)))

(defn new-windows-in-memory-file-system [& {:as overrides}]
  (new-in-memory-file-system (merge windows-defaults overrides)))
