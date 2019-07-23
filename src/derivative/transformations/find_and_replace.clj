(ns derivative.transformations.find-and-replace
  (:require
    [clojure.string :as string]

    [pathological.paths :as paths]
    [pathological.files :as files]
    [pathological.file-systems :as file-systems]

    [derivative.specs.core :as specs]
    [derivative.specs.paths :as path-specs]
    [derivative.specs.content :as content-specs]
    [derivative.templating.core :as templates]
    [derivative.transformations.core :refer [apply-transformation]])
  (:import [java.util.regex Pattern]))

(defn with-match-map [context match]
  (assoc context
    :match
    (into {}
      (map-indexed
        (fn [index item] [(keyword (str "$" index)) item])
        match))))

(defn find-and-replace [find replace context]
  (let [find-rendered (templates/render (specs/strip-syntax find) context)
        find-pattern (re-pattern
                       (if (content-specs/string-syntax? find)
                         (Pattern/quote find-rendered)
                         find-rendered))

        replace-fn
        #(templates/render (specs/strip-syntax replace)
           (with-match-map context %))]
    (fn [content]
      (string/replace content find-pattern replace-fn))))

(defmethod apply-transformation :find-and-replace
  [{:keys [configuration]}
   {:keys [vars file-system working-directory]
    :or   {vars              {}
           file-system       (file-systems/default-file-system)
           working-directory "."}}]
  (let [{:keys [find replace in]} configuration

        working-directory-path (paths/path file-system working-directory)

        file-paths (path-specs/expand-paths working-directory-path in)

        context {:var vars}
        find-and-replacer (find-and-replace find replace context)]
    (doseq [file-path file-paths]
      (let [initial-content (slurp file-path)

            transformed-content
            (find-and-replacer initial-content)]
        (spit file-path transformed-content)))))
