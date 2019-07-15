(ns pathological.file-systems-test
  (:require
    [clojure.test :refer :all]

    [pathological.file-systems :as fs]
    [pathological.paths :as p]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration
             windows-configuration]])
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

(deftest root-directories
  (testing "returns the only root directory when only one"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :roots ["/"]))]
      (is (= #{(p/path test-file-system "/")}
            (fs/root-directories test-file-system)))))

  (testing "returns all root directories when many"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :roots ["C://" "D://"]))]
      (is (= #{(p/path test-file-system "C://")
               (p/path test-file-system "D://")}
            (fs/root-directories test-file-system))))))
