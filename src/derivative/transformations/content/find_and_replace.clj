(ns derivative.transformations.content.find-and-replace
  (:require
    [clojure.string :as string]

    [derivative.specs.core :as specs]
    [derivative.specs.content :as content-specs]
    [derivative.templating.core :as templates]
    [derivative.transformations.core :refer [apply-transformation]])
  (:import
    [java.util.regex Pattern]))

(defn with-match-map [context match]
  (assoc context
    :match
    (into {}
      (map-indexed
        (fn [index item] [(keyword (str "$" index)) item])
        match))))

(defn find-and-replace [{:keys [find replace context]}]
  (fn [content]
    (let [find-rendered (templates/render (specs/strip-syntax find) context)
          find-pattern (re-pattern
                         (if (content-specs/string-syntax? find)
                           (Pattern/quote find-rendered)
                           find-rendered))

          replace-fn
          #(templates/render (specs/strip-syntax replace)
             (with-match-map context %))]
      (string/replace content find-pattern replace-fn))))
