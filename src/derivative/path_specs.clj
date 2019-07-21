(ns derivative.path-specs
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]))

(defn syntax [path-spec]
  (keyword (first (string/split path-spec #":"))))

(defn syntax? [path-spec syntax]
  (string/starts-with? path-spec (str (name syntax) ":")))

(defn file-syntax? [path-spec] (syntax? path-spec :file))
(defn directory-syntax? [path-spec] (syntax? path-spec :directory))

(defn strip-syntax [path-spec]
  (string/replace path-spec #"^[-a-z]*?:" ""))

(defn resolve-path [base-path path-spec & other-paths]
  (let [file-system (paths/file-system base-path)
        path-pattern (strip-syntax path-spec)]
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
                   (name (syntax path-spec)) ":'.")))))))

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
