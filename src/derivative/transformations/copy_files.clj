(ns derivative.transformations.copy-files
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.path-specs :as path-specs]
    [derivative.transformations.core :refer [apply-transformation]]
    [pathological.files :as f]))

(defn determine-to-path [base-path to-path-spec from-path]
  (cond
    (path-specs/file-syntax? to-path-spec)
    (paths/path base-path (path-specs/strip-syntax to-path-spec))

    (path-specs/directory-syntax? to-path-spec)
    (paths/path base-path (path-specs/strip-syntax to-path-spec) from-path)))

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
            (apply paths/path
              (concat [working-directory-path]
                (drop strip-names (paths/names from-path))))

            to-path
            (determine-to-path working-directory-path to stripped-from-path)]
        (files/create-directories (paths/parent to-path))
        (when-not (f/directory? from-path)
          (files/copy from-path to-path))))))
