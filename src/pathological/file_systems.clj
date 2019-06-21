(ns pathological.file-systems
  (:import [java.nio.file FileSystem FileSystems]))

(defn ^FileSystem default-file-system []
  (FileSystems/getDefault))

(def ^{:dynamic true :tag FileSystem} *file-system* (default-file-system))
