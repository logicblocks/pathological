(ns pathological.file-systems
  (:import
    [java.nio.file FileSystem FileSystems]))

(defn ^FileSystem default-file-system
  "Returns the default file system for the JVM.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystems.html#getDefault%28%29)
  for more details."
  []
  (FileSystems/getDefault))

(def ^{:dynamic true :tag FileSystem} *file-system*
  "The file system to use in various calls where an explicit file system is
  optional, e.g., [[pathological.paths/path]],
  [[pathological.path-matchers/path-matcher]],
  [[pathological.principals/->user-principal]] and
  [[pathological.principals/->group-principal]]

  By default, the [[default-file-system]] is used. However this var is dynamic
  and can be altered / rebound to any file system instance."
  (default-file-system))

(defn open?
  "Returns true if the file system is open, false otherwise.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#isOpen%28%29)
  for more details."
  [^FileSystem file-system]
  (.isOpen file-system))

(defn read-only?
  "Returns true if the file system is read only, false otherwise.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#isReadOnly%28%29)
  for more details."
  [^FileSystem file-system]
  (.isReadOnly file-system))

(defn close
  "Closes the file system.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#close%28%29)
  for more details."
  [^FileSystem file-system]
  (.close file-system))

(defn provider
  "Returns the provider for the file system.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#provider%28%29)
  for more details."
  [^FileSystem file-system]
  (.provider file-system))

(defn file-stores
  "Returns the collection of file stores associated with the file system.

  The file stores are returned as materialised records as specified in
  [[pathological.file-stores/->file-store]].

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getFileStores%28%29)
  for more details."
  [^FileSystem file-system]
  (map (requiring-resolve 'pathological.file-stores/->file-store)
    (.getFileStores file-system)))

(defn root-directories
  "Returns a set of paths representing the root directories of the file
  system.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getRootDirectories%28%29)
  for more details."
  [^FileSystem file-system]
  (set (.getRootDirectories file-system)))

(defn separator
  "Returns the name separator used by the file system.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getSeparator%28%29)
  for more details."
  [^FileSystem file-system]
  (.getSeparator file-system))

(defn supported-file-attribute-views
  "Returns a set of keywords representing the supported file attribute views
  for the file system.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#supportedFileAttributeViews%28%29)
  for more details."
  [^FileSystem file-system]
  (set (map keyword (.supportedFileAttributeViews file-system))))
