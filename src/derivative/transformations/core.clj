(ns derivative.transformations.core)

(defmulti apply-transformation
  (fn [transformation _] (:type transformation)))
