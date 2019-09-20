(ns pathological.file-systems
  (:import
    [java.nio.file FileSystem FileSystems]))

(defn ^FileSystem default-file-system []
  (FileSystems/getDefault))

(def ^{:dynamic true :tag FileSystem} *file-system* (default-file-system))

(defn open?
  [^FileSystem file-system]
  (.isOpen file-system))

(defn read-only?
  [^FileSystem file-system]
  (.isReadOnly file-system))

(defn close
  [^FileSystem file-system]
  (.close file-system))

(defn provider
  [^FileSystem file-system]
  (.provider file-system))

(defn file-stores
  [^FileSystem file-system]
  (map (requiring-resolve 'pathological.file-stores/->file-store)
    (.getFileStores file-system)))

(defn root-directories
  [^FileSystem file-system]
  (set (.getRootDirectories file-system)))

(defn separator
  [^FileSystem file-system]
  (.getSeparator file-system))

(defn supported-file-attribute-views
  [^FileSystem file-system]
  (set (map keyword (.supportedFileAttributeViews file-system))))
