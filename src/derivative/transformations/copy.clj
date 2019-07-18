(ns derivative.transformations.copy
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.transformations.core :refer [apply-transformation]]
    [pathological.files :as f]))

(defn pattern-type? [pattern type]
  (string/starts-with? pattern (str (name type) ":")))

(defn file? [pattern] (pattern-type? pattern :file))
(defn directory? [pattern] (pattern-type? pattern :directory))

(defn determine-from-paths [search-path pattern]
  (cond
    (file? pattern)
    [(paths/path search-path (string/replace pattern "file:" ""))]

    :else
    (files/find search-path (fn [path _] (paths/matches? path pattern)))))

(defn determine-to-path [search-path pattern]
  (paths/path search-path (string/replace pattern "file:" "")))

(defmethod apply-transformation :copy
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [from to]} configuration

        working-directory-path (paths/path file-system working-directory)

        from-paths (determine-from-paths working-directory-path from)
        to-path (determine-to-path working-directory-path to)]
    (doseq [from-path from-paths]
      (f/create-directories (.getParent to-path))
      (f/copy from-path to-path))))
