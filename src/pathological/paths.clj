(ns pathological.paths
  (:refer-clojure :exclude [name resolve])
  (:require
    [pathological.utils
     :refer [->varargs-array
             ->link-options-array]]
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

(defn subpath [^Path path from to]
  (.subpath path from to))

(defn file-system [^Path path]
  (.getFileSystem path))

(defn root [^Path path]
  (.getRoot path))

(defn parent [^Path path]
  (.getParent path))

(defn file-name [^Path path]
  (.getFileName path))

(defn name-count [^Path path]
  (.getNameCount path))

(defn name [^Path path index]
  (.getName path index))

(defn names [^Path path]
  (iterator-seq
    (.iterator path)))

(defn normalize [^Path path]
  (.normalize path))

(defmulti resolve
  (fn [_ other] (type other)))

(defmethod resolve Path
  [^Path base ^Path other]
  (.resolve base other))

(defmethod resolve String
  [^Path base ^String other]
  (.resolve base other))

(defmulti resolve-sibling
  (fn [_ other] (type other)))

(defmethod resolve-sibling Path
  [^Path base ^Path other]
  (.resolveSibling base other))

(defmethod resolve-sibling String
  [^Path base ^String other]
  (.resolveSibling base other))

(defn relativize [^Path base-path ^Path path]
  (.relativize base-path path))

(defmulti starts-with?
  (fn [_ other] (type other)))

(defmethod starts-with? Path
  [^Path path ^Path other]
  (.startsWith path other))

(defmethod starts-with? String
  [^Path path ^String other]
  (.startsWith path other))

(defmulti ends-with?
  (fn [_ other] (type other)))

(defmethod ends-with? Path
  [^Path path ^Path other]
  (.endsWith path other))

(defmethod ends-with? String
  [^Path path ^String other]
  (.endsWith path other))

(defn absolute?
  [^Path path]
  (.isAbsolute path))

(defn matches?
  [^Path path pattern]
  (pm/matches? (pm/path-matcher pattern) path))

(defn ->absolute-path
  [^Path path]
  (.toAbsolutePath path))

(defn ->real-path
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (.toRealPath path link-options)))

(defn ->file
  [^Path path]
  (.toFile path))

(defn ->uri
  [^Path path]
  (.toUri path))