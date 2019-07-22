(ns derivative.specs.paths
  (:require
    [pathological.paths :as paths]
    [pathological.files :as files]

    [derivative.specs.core :as specs]))

(def file-syntax? #(specs/syntax? % :file))
(def directory-syntax? #(specs/syntax? % :directory))

(defn resolve-path [base-path path-spec & other-paths]
  (let [file-system (paths/file-system base-path)
        path-pattern (specs/strip-syntax path-spec)]
    (paths/normalize
      (cond
        (file-syntax? path-spec)
        (paths/resolve base-path (paths/path file-system path-pattern))

        (directory-syntax? path-spec)
        (paths/resolve base-path
          (apply paths/path (paths/path file-system path-pattern) other-paths))

        :else
        (throw (IllegalArgumentException.
                 (str "Cannot resolve path spec with syntax: '"
                   (name (specs/syntax path-spec)) ":'.")))))))

(defn expand-paths [base-path path-spec]
  (cond
    (file-syntax? path-spec)
    [(resolve-path base-path path-spec)]

    (directory-syntax? path-spec)
    (files/walk
      (resolve-path base-path path-spec))

    :else
    (files/find base-path
      (fn [path _] (paths/matches? path path-spec)))))
