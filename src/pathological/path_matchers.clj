(ns pathological.path-matchers
  (:require
    [pathological.file-systems :as fs])
  (:import
    [java.nio.file FileSystem PathMatcher]))

(defn path-matcher
  ([pattern] (path-matcher fs/*file-system* pattern))
  ([^FileSystem file-system pattern]
    (.getPathMatcher file-system pattern)))

(defn matches? [^PathMatcher path-matcher path]
  (.matches path-matcher path))
