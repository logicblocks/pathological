(ns pathological.test-support.data
  (:import
    [java.util UUID]))

(defn random-uuid []
  (str (UUID/randomUUID)))