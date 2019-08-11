(ns pathological.file-systems
  (:import [java.nio.file FileSystem FileSystems]))

(defn ^FileSystem default-file-system []
  (FileSystems/getDefault))

(def ^{:dynamic true :tag FileSystem} *file-system* (default-file-system))

(defn file-stores
  [^FileSystem file-system]
  (.getFileStores file-system))

(defn root-directories
  [^FileSystem file-system]
  (.getRootDirectories file-system))

(defn supported-file-attribute-views
  [^FileSystem file-system]
  (into #{} (map keyword (.supportedFileAttributeViews file-system))))
