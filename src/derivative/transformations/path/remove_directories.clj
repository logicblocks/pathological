(ns derivative.transformations.path.remove-directories
  (:require
    [pathological.paths :as paths]))

(defn remove-directories [{:keys [count]}]
  (fn [path]
    (let [file-system (paths/file-system path)
          after-removal
          (drop (min count (paths/name-count path))
            (paths/names path))]
      (if (empty? after-removal)
        (paths/path file-system "")
        (apply paths/path after-removal)))))
