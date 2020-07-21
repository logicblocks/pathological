(ns pathological.path-matchers
  (:require
   [pathological.file-systems :as fs])
  (:import
   [java.nio.file FileSystem PathMatcher]))

(defn path-matcher
  "Constructs a [PathMatcher](https://docs.oracle.com/javase/7/docs/api/java/nio/file/PathMatcher.html)
  instance for the file system and pattern.

  If no file system is provided the file system bound to
  [[pathological.file-systems/*file-system*]] is used.

  The pattern argument consists of a syntax and a pattern string separated by
  a colon. By default, `glob` and `regex` syntaxes are supported although
  file system implementations may support others.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getPathMatcher%28java.lang.String%29)
  for more details of acceptable patterns."
  ([pattern] (path-matcher fs/*file-system* pattern))
  ([^FileSystem file-system pattern]
   (.getPathMatcher file-system pattern)))

(defn matches?
  "Returns true if the path satisfies the path matcher, false otherwise.

  See [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/PathMatcher.html#matches%28java.nio.file.Path%29)
  for more details."
  [^PathMatcher path-matcher path]
  (.matches path-matcher path))
