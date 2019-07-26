(ns support.strings
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]))

(defn pstr [value]
  (with-out-str
    (pprint value)))

(defn join-lines [& rest]
  (string/join (format "%n") rest))
