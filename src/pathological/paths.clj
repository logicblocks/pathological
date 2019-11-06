(ns pathological.paths
  (:refer-clojure :exclude [name resolve])
  (:require
    [clojure.java.io :as io]

    [pathological.utils :as u]
    [pathological.file-systems :as fs]
    [pathological.path-matchers :as pm]
    [pathological.file-stores :as fst])
  (:import
    [java.nio.file FileSystem Path Files Paths]
    [java.net URI]))

(defprotocol Pathable
  "A protocol for converting things to [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)s.

  Used by [[path]] with default implementations for:

    - [FileSystem](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html),
    - [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html),
    - [String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html),
    - [URI](https://docs.oracle.com/javase/7/docs/api/java/net/URI.html)

  See [[path]] for more details."
  (->path ^Path [this names]
    "Produces a path for the instance and sequence of path name elements."))

(extend-type FileSystem
  Pathable
  (->path ^Path [^FileSystem this [name & names]]
    (.getPath this name (u/->varargs-array String names))))

(extend-type String
  Pathable
  (->path ^Path [^String this names]
    (.getPath fs/*file-system* this (u/->varargs-array String names))))

(extend-type Path
  Pathable
  (->path ^Path [^Path this names]
    (if (seq names)
      (.getPath (.getFileSystem this)
        (str this)
        (u/->varargs-array String (map str names)))
      this)))

(extend-type URI
  Pathable
  (->path ^Path [^URI this names]
    (let [path (Paths/get this)]
      (if (seq names)
        (.getPath (.getFileSystem path)
          (str path)
          (u/->varargs-array String (map str names)))
        path))))

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

(defn path
  "Creates a [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)
  instance.

  Receives a [[Pathable]] instance and zero or more path name elements. By
  default, [[Pathable]] is implemented on:

    - [FileSystem](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html):
      in which case the name elements constitute the full path on the provided
      file system,
    - [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html):
      in which case the name elements are appended to the path retaining the
      file system from the path,
    - [String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html):
      in which case the name elements are appended to the path represented by
      the string, using [[pathological.file-systems/*file-system*]] as the file
      system for the path,
    - [URI](https://docs.oracle.com/javase/7/docs/api/java/net/URI.html): in
      which case the scheme of the URI is used to look up the file system in
      the installed providers and the name elements are appended to the URI
      to determine the path."
  [& [pathable & names]]
  (->path pathable names))

(defn subpath
  "Returns the subpath within `path` between the `from` index inclusive and the
  `to` index exclusive.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#subpath%28int,%20int%29)
  for more details."
  [^Path path from to]
  (.subpath path from to))

(defn file-system
  "Returns the [FileSystem](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html)
  associated with the `path`.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#getFileSystem%28%29)
  for more details."
  [^Path path]
  (.getFileSystem path))

(defn file-store
  "Returns the [FileStore](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileStore.html)
  on which the `path` is stored.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#getFileStore%28java.nio.file.Path%29)"
  [^Path path]
  (fst/->file-store (Files/getFileStore path)))

(defn root
  "Returns the root component of the `path` as a
  [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)
  instance.

  Returns `null` if there is no root component.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#getRoot%28%29)
  for more details."
  [^Path path]
  (.getRoot path))

(defn parent
  "Returns the parent path for the `path`, i.e., the path without the name
  element furthest to the right, as a
  [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)
  instance.

  Returns `null` if there is no parent path.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#getParent%28%29)
  for more details."
  [^Path path]
  (.getParent path))

(defn file-name
  "Returns the file or directory name for the `path`, i.e., the name element
  furthers to the right, as a
  [Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)
  instance.

  Returns `null` if there is no file or directory name.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#getFileName%28%29)
  for more details."
  [^Path path]
  (.getFileName path))

(defn name-count
  "Returns the number of name elements in the `path`.

  Returns `0` if the `path` is a root component.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#getFileName%28%29)
  for more details."
  [^Path path]
  (.getNameCount path))

(defn name
  "Returns the name element in the `path` at the `index`.

  `index` represents the offset of the name element from the root and must be
  non-negative and less than the number of elements or an
  [IllegalArgumentException](...) is thrown.

  See [the Java documentation](...)
  for more details."
  [^Path path index]
  (.getName path index))

(defn names
  "Returns a seq over all of the name elements in the `path`, iterating
  forwards through the path from the root component.

  See [the Java documentation](...)
  for more details."
  [^Path path]
  (iterator-seq
    (.iterator path)))

(defn normalize
  [^Path path]
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
