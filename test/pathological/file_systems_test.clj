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
    [java.nio.file FileSystems FileSystem]))

(deftest default-file-system
  (testing "returns default file system"
    (is (= (FileSystems/getDefault)
          (fs/default-file-system)))))

(deftest file-system-var
  (testing "uses default file system by default"
    (is (= (FileSystems/getDefault)
          fs/*file-system*))))

(deftest file-stores
  (testing "returns the file stores of the provided file system"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          file-stores
          (.getFileStores ^FileSystem test-file-system)]
      (is (= file-stores
            (fs/file-stores test-file-system))))))

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

(deftest supported-file-attribute-views
  (testing "returns the set of supported file attribute views"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic :owner}))]
      (is (= #{:basic :owner}
            (fs/supported-file-attribute-views test-file-system))))))
