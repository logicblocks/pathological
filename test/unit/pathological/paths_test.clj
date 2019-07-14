(ns pathological.paths-test
  (:require
    [clojure.test :refer :all]

    [pathological.paths :as p]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system]])
  (:import
    [java.nio.file FileSystem
                   FileSystems]))

(defn empty-string-array []
  (make-array String 0))

(defn string-array [& args]
  (into-array String args))

(deftest path
  (let [^FileSystem test-file-system-1
        (new-in-memory-file-system (random-file-system-name))

        ^FileSystem test-file-system-2
        (new-in-memory-file-system (random-file-system-name))]

    (testing "uses supplied file system to build path"
      (is (= (.getPath test-file-system-1 "first"
               (string-array "second" "third"))
            (p/path test-file-system-1 "first/second/third"))))

    (testing "uses bound file system when none supplied"
      (is (= (.getPath test-file-system-1 "first"
               (string-array "second" "third"))
            (with-bindings
              {#'pathological.file-systems/*file-system* test-file-system-1}
              (p/path "first/second/third")))))

    (testing "uses default file system when none supplied or bound"
      (is (= (.getPath (FileSystems/getDefault) "first"
               (string-array "second" "third"))
            (p/path "first/second/third"))))

    (testing "uses file system from supplied path"
      (is (= (.getPath test-file-system-2 "first"
               (string-array "second" "third"))
            (p/path (.getPath test-file-system-2 "first" (empty-string-array))
              "second/third"))))

    (testing "constructs path from many names using supplied file system"
      (is (= (.getPath test-file-system-1 "first"
               (string-array "second" "third"))
            (p/path test-file-system-1 "first" "second" "third"))))

    (testing "constructs path from many names using bound file system"
      (is (= (.getPath test-file-system-1 "first"
               (string-array "second" "third"))
            (with-bindings
              {#'pathological.file-systems/*file-system* test-file-system-1}
              (p/path "first" "second" "third")))))

    (testing "constructs path from many names using default file system"
      (is (= (.getPath (FileSystems/getDefault) "first"
               (string-array "second" "third"))
            (p/path "first" "second" "third"))))

    (testing "constructs path from many names using file system of first path"
      (is (= (.getPath test-file-system-2 "first"
               (string-array "second" "third"))
            (p/path (.getPath test-file-system-2 "first" (empty-string-array))
              "second" "third"))))

    (testing "constructs path from many paths using file system of first path"
      (is (= (.getPath test-file-system-2 "first"
               (string-array "second" "third"))
            (p/path
              (.getPath test-file-system-2 "first" (empty-string-array))
              (.getPath test-file-system-1 "second" (empty-string-array))
              (.getPath test-file-system-2 "third" (empty-string-array))))))))

(deftest normalize
  (testing "returns path with redundancies removed"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          absolute-path
          (p/path test-file-system "/some/nested/../directory/./file.txt")]
      (is (= (p/path test-file-system "/some/directory/file.txt")
            (p/normalize absolute-path))))))

(deftest relativize
  (testing "returns path relative to supplied directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          absolute-path
          (p/path test-file-system "/some/nested/directory/file.txt")
          base-path
          (p/path test-file-system "/some/nested/")]
      (is (= (p/path test-file-system "directory/file.txt")
            (p/relativize base-path absolute-path))))))

(deftest matches?
  (testing "returns true if path matches pattern"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          pattern "glob:**/*.html"
          path (p/path test-file-system "/directory/index.html")]
      (is (true? (p/matches? path pattern)))))

  (testing "returns false if path does not match pattern"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          pattern "glob:**/*.txt"
          path (p/path test-file-system "/directory/index.html")]
      (is (false? (p/matches? path pattern))))))
