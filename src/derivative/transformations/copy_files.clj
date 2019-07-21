(ns derivative.transformations.copy-files
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.transformations.core :refer [apply-transformation]]
    [pathological.paths :as p]))

(defn pattern-type? [pattern type]
  (string/starts-with? pattern (str (name type) ":")))

(defn file-syntax? [pattern] (pattern-type? pattern :file))
(defn directory-syntax? [pattern] (pattern-type? pattern :directory))

(defn strip-syntax [path-spec]
  (string/replace path-spec #"^[-a-z]*?:" ""))

(defn determine-from-paths [base-path path-spec]
  (cond
    (file-syntax? path-spec)
    [(paths/path base-path (strip-syntax path-spec))]

    (directory-syntax? path-spec)
    (files/find
      (paths/path base-path (strip-syntax path-spec))
      (fn [path _] (not (files/directory? path))))

    :else
    (files/find base-path (fn [path _] (paths/matches? path path-spec)))))

(defn determine-to-path [base-path to-path-spec from-path]
  (cond
    (file-syntax? to-path-spec)
    (paths/path base-path (strip-syntax to-path-spec))

    (directory-syntax? to-path-spec)
    (paths/path base-path (strip-syntax to-path-spec) from-path)))

(defmethod apply-transformation :copy
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [from to strip-names]
         :or   {strip-names 0}} configuration

        working-directory-path (paths/path file-system working-directory)

        from-paths (determine-from-paths working-directory-path from)]
    (doseq [from-path from-paths]
      (let [stripped-from-path
            (apply p/path
              (concat [working-directory-path]
                (drop (inc strip-names) (p/names from-path))))

            to-path
            (determine-to-path working-directory-path to stripped-from-path)]
        (files/create-directories (paths/parent to-path))
        (files/copy from-path to-path)))))
