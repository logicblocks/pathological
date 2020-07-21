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

(defn ->attribute-spec
  "Converts the provided attribute spec string to a normalised attribute
  spec, a map with keys `:view` and `:name` for a single attribute or
  `:view` and `:names` for multiple attributes, with keyword values representing
  the corresponding attribute(s).

  Multiple formats for the attribute spec string are supported:

    - `\"attribute-view:attribute-name\"`
    - `\"attribute-view:attribute-1-name,attribute-2-name\"`
    - `\"attribute-name\"`
    - `\"attribute-1-name,attribute-2-name\"`

  In the case that no attribute view is provided, `:basic` is inferred.
  Attribute view and name(s) can be provided in camel case or kebab case and
  will always be converted to kebab cased keywords.

  The attribute view or name can also be the wildcard `\"*\"` representing all
  views or names respectively. Wildcard attribute specs are used for attribute
  conversions. See [[pathological.utils/->attribute-value]] and
  [[pathological.utils/<-attribute-value]] for more details.

  If the provided value is already a map, it is returned unaltered."
  [value]
  (if-not (map? value)
    (let [just-one? (fn [coll] (= (count coll) 1))

          components (string/split (clojure.core/name value) #":")
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

(defn ->attribute-spec-string
  "Converts the provided attribute spec or attribute spec string to the correct
  format to be passed to the underlying Java methods acting on file attributes.
  Provided attribute specs can be for a single attribute or for
  multiple attributes and can include wildcards. In the case that no
  attribute view is provided, `:basic` is inferred."
  [value]
  (let [attribute-spec (->attribute-spec value)
        names (or (:names attribute-spec) #{(:name attribute-spec)})

        view-string (kebab->camel-string (:view attribute-spec))
        name-strings (map kebab->camel-string names)]
    (str view-string ":" (string/join "," name-strings))))

(defn view
  "Returns the view, as a keyword, from the provided attribute spec or
  attribute spec string. If no view is present, `:basic` is inferred."
  [attribute-spec]
  (:view (->attribute-spec attribute-spec)))

(defn name
  "Returns the name, as a keyword, from the provided single attribute attribute
  spec or attribute spec string. If the attribute spec is for multiple
  attributes, `nil` is returned."
  [attribute-spec]
  (:name (->attribute-spec attribute-spec)))

(defn names
  "Returns the names, as a set of keywords, from the provided multiple attribute
  attribute spec or attribute spec string. If the attribute spec is for a single
  attribute, `nil` is returned."
  [attribute-spec]
  (:names (->attribute-spec attribute-spec)))

(defn view?
  "Checks if the provided attribute spec has any of the provided views. The
  second argument can be either a single view or a collection of views. If no
  view is present, `:basic` is inferred. Either `nil` or the matching view is
  returned."
  [attribute-spec view-or-views]
  (let [actual-view (view attribute-spec)
        test-views (if (seqable? view-or-views)
                     (set (map keyword view-or-views))
                     #{(keyword view-or-views)})]
    (test-views actual-view)))

(defn name?
  "Checks if the provided attribute spec is for a single attribute with name
  in any of the provided names. The second argument can be either a single name
  or a collection of names. Either `nil` or the matching name is returned."
  [attribute-spec name-or-names]
  (let [actual-name (name attribute-spec)
        test-names (if (seqable? name-or-names)
                     (set (map keyword name-or-names))
                     #{(keyword name-or-names)})]
    (test-names actual-name)))
