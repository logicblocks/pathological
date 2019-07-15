(ns derivative.transformations.find-and-replace
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.transformations.core :refer [apply-transformation]]
    [derivative.templating.core :as templates])
  (:import [java.util.regex Pattern]))

(def ^:dynamic *line-separator*
  (format "%n"))

(defn join-lines [coll]
  (string/join *line-separator* coll))

(defn determine-inputs [search-path pattern]
  (if (string/starts-with? pattern "path:")
    [(paths/path search-path (string/replace pattern "path:" ""))]
    (files/find search-path (fn [path _] (paths/matches? path pattern)))))

(defn build-match-map [match]
  (into {}
    (map-indexed
      (fn [index item] [(keyword (str "$" index)) item])
      match)))

(defmethod apply-transformation :find-and-replace
  [{:keys [configuration]}
   {:keys [directories vars file-system]
    :or   {vars        {}
           file-system (file-systems/default-file-system)}}]
  (let [{:keys [find replace in]} configuration
        {:keys [source target]} directories

        source-directory-path (paths/path file-system source)
        target-directory-path (paths/path file-system target)

        input-file-paths (determine-inputs source-directory-path in)

        context {:var vars}]
    (doseq [input-file-path input-file-paths]
      (let [output-file-path
            (paths/path target-directory-path
              (paths/relativize source-directory-path input-file-path))

            initial-content
            (join-lines (files/read-all-lines input-file-path))

            find-pattern
            (if (string? find)
              (re-pattern (Pattern/quote (templates/render find context)))
              (re-pattern
                (templates/render
                  (-> (.pattern find)
                    (string/replace "\\{\\{" "{{")
                    (string/replace "\\}\\}" "}}"))
                  context)))

            replace-fn
            #(templates/render replace
               (assoc context :match (build-match-map %)))

            transformed-content
            (string/replace initial-content find-pattern replace-fn)]
        (files/write-lines output-file-path
          (string/split-lines transformed-content))))))

; Refactoring ideas:
;   spit and slurp in pathological
;   context management
;   templating
;     regular expression handling
;     interpolation
