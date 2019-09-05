(ns pathological.support.reflect
  (:refer-clojure :exclude [class])
  (:require
    [pathological.utils :as u])
  (:import
    [java.lang.reflect Method Field]))

(defn class [object]
  ^Class
  (.getClass object))

(defn declared-methods [klass]
  ^"[Ljava.lang.reflect.Method;"
  (.getDeclaredMethods klass))

(defn declared-fields [klass]
  ^"[Ljava.lang.reflect.Field;"
  (.getDeclaredFields klass))

(defn methods-by-name [^"[Ljava.lang.reflect.Method;" methods method-name]
  (let [normalised-method-name (u/kebab->camel method-name)]
    ^"[Ljava.lang.reflect.Method;"
    (filter
      #(= (.getName ^Method %) normalised-method-name)
      methods)))

(defn fields-by-name [^"[Ljava.lang.reflect.Field;" fields field-name]
  (let [normalised-field-name (u/kebab->camel field-name)]
    ^"[Ljava.lang.reflect.Field;"
    (filter
      #(= (.getName ^Field %) normalised-field-name)
      fields)))

(defn first-method [^"[Ljava.lang.reflect.Method;" methods]
  ^Method (first methods))

(defn first-field [^"[Ljava.lang.reflect.Field;" fields]
  ^Field (first fields))

(defn array-of-objects [args]
  ^"[Ljava.lang.Object;" (into-array Object (rest args)))

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