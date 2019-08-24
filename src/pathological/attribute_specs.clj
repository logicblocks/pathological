(ns pathological.attribute-specs
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as string]))

(defn- camel->kebab-keyword [value]
  (let [camel->kebab (requiring-resolve 'pathological.utils/camel->kebab)]
    (keyword (camel->kebab value))))

(defn- kebab->camel-string [value]
  (let [kebab->camel (requiring-resolve 'pathological.utils/kebab->camel)]
    (str (kebab->camel value))))

(defn ->attribute-spec [value]
  (if-not (map? value)
    (let [just-one? (fn [coll] (= (count coll) 1))

          components (string/split value #":")
          [view name-spec] (if (just-one? components)
                             [:basic (first components)]
                             [(camel->kebab-keyword (first components))
                              (second components)])
          names (set (map camel->kebab-keyword (string/split name-spec #",")))
          name (when (just-one? names) (first names))]
      (if name
        {:view view
         :name name}
        {:view  view
         :names names}))
    value))

(defn ->attribute-spec-string [value]
  (let [attribute-spec (->attribute-spec value)
        names (or (:names attribute-spec) #{(:name attribute-spec)})

        view-string (kebab->camel-string (:view attribute-spec))
        name-strings (map kebab->camel-string names)]
    (str view-string ":" (string/join "," name-strings))))

(defn view [attribute-spec] (:view (->attribute-spec attribute-spec)))
(defn name [attribute-spec] (:name (->attribute-spec attribute-spec)))
(defn names [attribute-spec] (:names (->attribute-spec attribute-spec)))

(defn view? [attribute-spec view-or-views]
  (let [actual-view (view attribute-spec)
        test-views (if (seq? view-or-views)
                     (set (map keyword view-or-views))
                     #{(keyword view-or-views)})]
    (test-views actual-view)))

(defn name? [attribute-spec name-or-names]
  (let [actual-name (name attribute-spec)
        test-names (if (seqable? name-or-names)
                     (set (map keyword name-or-names))
                     #{(keyword name-or-names)})]
    (test-names actual-name)))
