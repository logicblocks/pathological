(ns derivative.transformations.find-and-replace
  (:require
    [pathological.paths :as paths]
    [pathological.file-systems :as file-systems]

    [derivative.specs.paths :as path-specs]
    [derivative.transformations.core
     :refer [apply-transformation]]
    [derivative.transformations.content.find-and-replace
     :refer [find-and-replace]]))

(defmethod apply-transformation :find-and-replace
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [in]} configuration

        working-directory-path (paths/path file-system working-directory)

        file-paths (path-specs/expand-paths working-directory-path in)

        context {:var vars}
        find-and-replacer (find-and-replace
                            (assoc configuration :context context))]
    (doseq [file-path file-paths]
      (let [initial-content (slurp file-path)

            transformed-content
            (find-and-replacer initial-content)]
        (spit file-path transformed-content)))))
