(ns support.io
  (:import [java.io StringWriter StringReader]))

(defn new-string-writer []
  (StringWriter.))

(defn new-string-reader
  ([] (new-string-reader ""))
  ([contents]
   (StringReader. contents)))
