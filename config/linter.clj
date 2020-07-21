(disable-warning
  {:linter :constant-test
   :for-macro 'clojure.core/or
   :if-inside-macroexpansion-of #{'pathological.utils/->varargs-array}
   :within-depth 3})
