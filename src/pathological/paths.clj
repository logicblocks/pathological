(ns pathological.paths
  (:require
    [pathological.utils :refer [->varargs-array]]
    [pathological.file-systems :refer [*file-system*]]
    [pathological.path-matchers :as pm])
  (:import
    [java.nio.file FileSystem
                   Path]))

(defprotocol ^:private BasePath
  (->path ^Path [this paths]))

(extend-type FileSystem
  BasePath
  (->path ^Path [^FileSystem this [path & paths]]
    (.getPath this path (->varargs-array String paths))))

(extend-type String
  BasePath
  (->path ^Path [^String this paths]
    (.getPath *file-system* this (->varargs-array String paths))))

(extend-type Path
  BasePath
  (->path ^Path [^Path this paths]
    (if (seq paths)
      (.getPath (.getFileSystem this)
        (str this)
        (->varargs-array String (map str paths)))
      this)))

(defn path [& [base-path & paths]]
  (->path base-path paths))

(defn file-system [^Path path]
  (.getFileSystem path))

(defn parent [^Path path]
  (.getParent path))

(defn normalize [^Path path]
  (.normalize path))

(defn relativize [^Path base-path ^Path path]
  (.relativize base-path path))

(defn matches? [^Path path pattern]
  (pm/matches? (pm/path-matcher pattern) path))
