(ns support.assertions
  (:require
    [clojure.test :as test]))

(declare
  clean-exit
  dirty-exit
  dirty-exit-with-code

  standard-output-including)

(defmacro has
  ([form] `(has ~form nil))
  ([form msg] `(test/try-expr ~msg ~form)))

(defn test-event [type message expected actual]
  {:type     type
   :message  message
   :expected expected
   :actual   actual})

(defn test-event-factory [type]
  (fn [message & {:keys [expected actual]}]
    (test-event type message expected actual)))

(def pass (test-event-factory :pass))
(def fail (test-event-factory :fail))

(defmethod test/assert-expr 'standard-output-including [msg form]
  (let [output-fragment (nth form 1)
        standard-streams (nth form 2)]
    (test/assert-predicate msg
      `(clojure.string/includes?
         (str (:output ~standard-streams))
         ~output-fragment))))

(defn assert-exit-code [expression requirement msg form]
  `(let [exit-code# ~expression
         result# (~requirement exit-code#)
         actual# (if (= exit-code# 0)
                   `(~(symbol :clean-exit) ~'~expression)
                   `(~(symbol :dirty-exit-with-code)
                      ~exit-code# ~'~expression))]
     (if result#
       (test/do-report (pass ~msg
                         :expected '~form
                         :actual '~form))
       (test/do-report (fail ~msg
                         :expected '~form
                         :actual actual#)))
     result#))

(defmethod test/assert-expr 'clean-exit [msg form]
  (let [expression (nth form 1)
        requirement `(fn [exit-code#] (= exit-code# 0))]
    (assert-exit-code expression requirement msg form)))

(defmethod test/assert-expr 'dirty-exit [msg form]
  (let [expression (nth form 1)
        requirement `(fn [exit-code#] (not= exit-code# 0))]
    (assert-exit-code expression requirement msg form)))

(defmethod test/assert-expr 'dirty-exit-with-code [msg form]
  (let [expression (nth form 2)
        requirement `(fn [exit-code#] (= exit-code# ~(nth form 1)))]
    (assert-exit-code expression requirement msg form)))
