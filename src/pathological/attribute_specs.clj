(ns pathological.attribute-specs
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as string]))

(defn view [attribute-spec]
  (if (string/includes? attribute-spec ":")
    (keyword (first (string/split attribute-spec #":")))
    :basic))

(defn name [attribute-spec]
  (let [camel->kebab (requiring-resolve 'pathological.utils/camel->kebab)]
    (if (string/includes? attribute-spec ":")
      (keyword (camel->kebab (second (string/split attribute-spec #":"))))
      (keyword (camel->kebab attribute-spec)))))

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
