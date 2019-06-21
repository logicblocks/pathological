(ns derivative.test-support
  (:require
    [clojure.string :as string]

    [pathological.files :as f]
    [pathological.paths :as p]))

(defn multiline-str [& strings]
  (string/join (format "%n") strings))

(defn with-empty-directory [path]
  #(do (f/delete-recursively (p/path path)) (%)))
