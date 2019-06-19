(ns derivative.test-support
  (:require
    [clojure.string :as string]

    [me.raynes.fs
     :refer [delete-dir]]))

(defn multiline-str [& strings]
  (string/join "\n" strings))

(defn with-empty-directory [path]
  #(do (delete-dir path) (%)))
