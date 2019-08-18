(ns pathological.file-systems
  (:require
    [pathological.file-stores :as fst])
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

(defn file-stores
  [^FileSystem file-system]
  (map fst/->file-store (.getFileStores file-system)))

(defn root-directories
  [^FileSystem file-system]
  (.getRootDirectories file-system))

(defn separator
  [^FileSystem file-system]
  (.getSeparator file-system))

(defn supported-file-attribute-views
  [^FileSystem file-system]
  (into #{} (map keyword (.supportedFileAttributeViews file-system))))

