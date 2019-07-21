(ns derivative.path-specs
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]))

(defn syntax? [path-spec syntax]
  (string/starts-with? path-spec (str (name syntax) ":")))

(defn file-syntax? [path-spec] (syntax? path-spec :file))
(defn directory-syntax? [path-spec] (syntax? path-spec :directory))

(defn strip-syntax [path-spec]
  (string/replace path-spec #"^[-a-z]*?:" ""))

(defn expand-paths [base-path path-spec]
  (let [file-system (paths/file-system base-path)]
    (cond
      (file-syntax? path-spec)
      [(paths/path file-system (strip-syntax path-spec))]

      (directory-syntax? path-spec)
      (files/walk
        (paths/path file-system (strip-syntax path-spec)))

      :else
      (files/find base-path
        (fn [path _] (paths/matches? path path-spec))))))

(defn resolve-path [])
