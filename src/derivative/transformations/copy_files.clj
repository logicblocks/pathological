(ns derivative.transformations.copy-files
  (:require
    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.specs.paths :as path-specs]

    [derivative.transformations.core :refer [apply-transformation]]
    [derivative.transformations.path.remove-directories
     :refer [remove-directories]]
    [derivative.transformations.path.find-and-replace
     :refer [find-and-replace]]))

(defmulti ->transformation
  (fn [transform] (:type transform)))

(defmethod ->transformation :remove-directories
  [{:keys [configuration]}]
  (remove-directories configuration))

(defmethod ->transformation :find-and-replace
  [{:keys [configuration]}]
  (find-and-replace configuration))

(defn with-context [context]
  (fn [transform]
    (update-in transform [:configuration]
      assoc :context context)))

(defmethod apply-transformation :copy-files
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [from to transform]
         :or   {transform []}} configuration

        working-directory-path (paths/path file-system working-directory)

        from-paths (path-specs/expand-paths working-directory-path from)

        context {:var vars}
        transform-path
        (apply comp
          (map (comp ->transformation (with-context context)) transform))]
    (doseq [from-path from-paths]
      (let [transformed-path (transform-path from-path)

            to-path
            (path-specs/resolve-path
              working-directory-path to transformed-path)

            to-parent (paths/parent to-path)]
        (when to-parent
          (files/create-directories to-parent))
        (when-not (files/directory? from-path)
          (files/copy from-path to-path))))))
