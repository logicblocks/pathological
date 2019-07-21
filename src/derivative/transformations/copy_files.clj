(ns derivative.transformations.copy-files
  (:require
    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.path-specs :as path-specs]
    [derivative.transformations.core :refer [apply-transformation]]))

(defmethod apply-transformation :copy
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [from to strip-names]
         :or   {strip-names 0}} configuration

        working-directory-path (paths/path file-system working-directory)

        from-paths (path-specs/expand-paths working-directory-path from)]
    (doseq [from-path from-paths]
      (let [stripped-from-path
            (apply paths/path working-directory-path
              (drop strip-names (paths/names from-path)))

            to-path
            (path-specs/resolve-path working-directory-path to
              stripped-from-path)
            to-parent (paths/parent to-path)]
        (when to-parent
          (files/create-directories to-parent))
        (when-not (files/directory? from-path)
          (files/copy from-path to-path))))))
