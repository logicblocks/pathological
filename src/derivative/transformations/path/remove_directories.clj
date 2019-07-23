(ns derivative.transformations.path.remove-directories
  (:require
    [pathological.paths :as paths]))

(defn remove-directories [base-path path count]
  (apply paths/path base-path
    (drop count (paths/names path))))
