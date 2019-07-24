(ns derivative.transformations.path.find-and-replace
  (:require
    [pathological.paths :as paths]

    [derivative.transformations.content.find-and-replace :as content]))

(defn find-and-replace [configuration]
  (let [find-and-replacer (content/find-and-replace configuration)]
    (fn [path]
      (let [file-system (paths/file-system path)
            path-string (str path)
            path-transformed (find-and-replacer path-string)]
        (paths/path file-system path-transformed)))))
