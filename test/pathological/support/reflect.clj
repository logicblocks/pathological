(ns pathological.support.reflect
  (:refer-clojure :exclude [class])
  (:require
   [pathological.utils :as u])
  (:import
   [java.lang.reflect Method Field]))

(defn ^Class class [^Object object]
  (.getClass object))

(defn ^"[Ljava.lang.reflect.Method;" declared-methods [^Class klass]
  (.getDeclaredMethods klass))

(defn ^"[Ljava.lang.reflect.Field;" declared-fields [^Class klass]
  (.getDeclaredFields klass))

(defn ^"[Ljava.lang.reflect.Method;" methods-by-name
  [^"[Ljava.lang.reflect.Method;" methods method-name]
  (let [normalised-method-name (u/kebab->camel method-name)]
    (filter
      #(= (.getName ^Method %) normalised-method-name)
      methods)))

(defn ^"[Ljava.lang.reflect.Field;" fields-by-name
  [^"[Ljava.lang.reflect.Field;" fields field-name]
  (let [normalised-field-name (u/kebab->camel field-name)]
    (filter
      #(= (.getName ^Field %) normalised-field-name)
      fields)))

(defn ^Method first-method [^"[Ljava.lang.reflect.Method;" methods]
  (first methods))

(defn ^Field first-field [^"[Ljava.lang.reflect.Field;" fields]
  (first fields))

(defn ^"[Ljava.lang.Object;" array-of-objects [args]
  (into-array Object args))

(defn invoke [object method-name & args]
  (let [klass (class object)
        declared-methods (declared-methods klass)
        matching-methods (methods-by-name declared-methods method-name)
        matching-method (first-method matching-methods)

        args-array (array-of-objects args)]
    (.setAccessible matching-method true)
    (.invoke matching-method object args-array)))

(defn get-field [object field-name]
  (let [klass (class object)
        declared-fields (declared-fields klass)
        matching-fields (fields-by-name declared-fields field-name)
        matching-field (first-field matching-fields)]
    (.setAccessible matching-field true)
    (.get matching-field object)))