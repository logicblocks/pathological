(ns pathological.paths
  (:refer-clojure :exclude [name resolve])
  (:require
    [clojure.java.io :as io]

    [pathological.utils :as u]
    [pathological.file-systems :as fs]
    [pathological.path-matchers :as pm]
    [pathological.file-stores :as fst])
  (:import
    [java.nio.file FileSystem Path Files]))

(defprotocol ^:private Pathable
  (->path ^Path [this paths]))

(extend-type FileSystem
  Pathable
  (->path ^Path [^FileSystem this [path & paths]]
    (.getPath this path (u/->varargs-array String paths))))

(extend-type String
  Pathable
  (->path ^Path [^String this paths]
    (.getPath fs/*file-system* this (u/->varargs-array String paths))))

(extend-type Path
  Pathable
  (->path ^Path [^Path this paths]
    (if (seq paths)
      (.getPath (.getFileSystem this)
        (str this)
        (u/->varargs-array String (map str paths)))
      this)))

(defn- opts->open-options-array [opts]
  (let [opts-map (when opts (apply hash-map opts))
        open-options (if (:append opts-map) [:append] [])
        open-options (u/->open-options-array open-options)]
    open-options))

(extend Path
  io/IOFactory
  (assoc io/default-streams-impl
    :make-input-stream
    (fn [^Path path opts]
      (io/make-input-stream
        (Files/newInputStream path (opts->open-options-array opts)) opts))
    :make-output-stream
    (fn [^Path path opts]
      (io/make-output-stream
        (Files/newOutputStream path (opts->open-options-array opts)) opts))))

(defn path [& [base-path & paths]]
  (->path base-path paths))

(defn subpath [^Path path from to]
  (.subpath path from to))

(defn file-system [^Path path]
  (.getFileSystem path))

(defn file-store [^Path path]
  (fst/->file-store (Files/getFileStore path)))

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
  [^Path path syntax-and-pattern]
  (pm/matches? (pm/path-matcher syntax-and-pattern) path))

(defn ->absolute-path
  [^Path path]
  (.toAbsolutePath path))

(defn ->real-path
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (u/->link-options-array options)]
    (.toRealPath path link-options)))

(defn ->file
  [^Path path]
  (.toFile path))

(defn ->uri
  [^Path path]
  (.toUri path))
