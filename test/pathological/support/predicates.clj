(ns pathological.support.predicates)

(def uuid-pattern
  (re-pattern
    (str "^(\\{){0,1}[0-9a-fA-F]{8}\\-"
      "[0-9a-fA-F]{4}\\-"
      "[0-9a-fA-F]{4}\\-"
      "[0-9a-fA-F]{4}\\-"
      "[0-9a-fA-F]{12}(\\}){0,1}$")))

(defn uuid-string? [value]
  (re-matches uuid-pattern value))
