(ns derivative.specs.core-test
  (:require
    [clojure.test :refer :all]

    [derivative.specs.core :as s]))

(deftest syntax
  (is (= :file (s/syntax "file:some/important/file.txt")))
  (is (= :directory (s/syntax "directory:/some/important/directory")))
  (is (= :glob (s/syntax "glob:**some/important/**file*")))
  (is (= :regex (s/syntax "regex:.*\\/(.*)\\.txt"))))

(deftest syntax?
  (is (true? (s/syntax? "file:some/important/file.txt" :file)))
  (is (true? (s/syntax? "directory:/some/important/directory" "directory")))
  (is (false? (s/syntax? "file:some/important/file.txt" :glob)))
  (is (false? (s/syntax? "directory:/some/important/directory" "regex"))))

(deftest strip-syntax
  (is (= "/some/important/directory"
        (s/strip-syntax "directory:/some/important/directory")))
  (is (= "some/important/file.txt"
        (s/strip-syntax "file:some/important/file.txt")))
  (is (= "some/things/**/*.txt"
        (s/strip-syntax "glob:some/things/**/*.txt")))
  (is (= "some/things/[^/]*?/(.*).txt"
        (s/strip-syntax "regex:some/things/[^/]*?/(.*).txt")))
  (is (= "some_thing"
        (s/strip-syntax "string:some_thing"))))
