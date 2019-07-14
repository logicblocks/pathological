(ns pathological.file-systems-test
  (:require
    [clojure.test :refer :all]

    [pathological.file-systems :as fs])
  (:import
    [java.nio.file FileSystems]))

(deftest default-file-system
  (testing "returns default file system"
    (is (= (FileSystems/getDefault)
          (fs/default-file-system)))))

(deftest file-system-var
  (testing "uses default file system by default"
    (is (= (FileSystems/getDefault)
          fs/*file-system*))))
