(ns derivative.transformations.copy-test
  (:require
    [clojure.test :refer :all]

    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]]

    [derivative.transformations.core :as transformations]
    [derivative.transformations.copy]))

(deftest copies-one-file
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:source
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :copy
         :configuration {:from "file:source/test.rb"
                         :to   "file:target/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content (f/read-all-lines (p/path file-system "target/test.rb"))]
    (is (= final-content
          ["def thing_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))
