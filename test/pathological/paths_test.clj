(ns pathological.paths-test
  (:refer-clojure :exclude [name resolve spit slurp])
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as io]

    [pathological.paths :as p]
    [pathological.files :as f]
    [pathological.file-systems :as fs]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]])
  (:import
    [java.nio.file FileSystem
                   FileSystems]
    [java.net URI]))

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

(deftest subpath
  (testing "returns subpath between indices"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= (p/path test-file-system "nested/directory")
            (p/subpath path 1 3))))))

(deftest file-system
  (testing "returns the file system of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/path")]
      (is (= test-file-system
            (p/file-system path))))))

(deftest file-store
  (testing "returns the file store of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          file-store
          (first (fs/file-stores test-file-system))

          path (p/path test-file-system "/some/path")]
      (is (= file-store
            (p/file-store path))))))

(deftest spit
  (testing "supports spit on paths"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (clojure.core/spit path content)

      (is (= ["Line 1" "Line 2"]
            (f/read-all-lines path))))))

(deftest slurp
  (testing "supports slurp on paths"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["Line 1" "Line 2"]]
      (f/write-lines path content)

      (is (= "Line 1\nLine 2\n"
            (clojure.core/slurp path))))))

(deftest normalize
  (testing "returns path with redundancies removed"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          absolute-path
          (p/path test-file-system "/some/nested/../directory/./file.txt")]
      (is (= (p/path test-file-system "/some/directory/file.txt")
            (p/normalize absolute-path))))))

(deftest resolve
  (testing "resolves one path relative to another"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          base-path (p/path test-file-system "/some/nested")
          relative-path (p/path test-file-system "directory/file.txt")]
      (is (= (p/path test-file-system "/some/nested/directory/file.txt")
            (p/resolve base-path relative-path)))))

  (testing "resolves a string path relative to a path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          base-path (p/path test-file-system "/some/nested")
          relative-path "directory/file.txt"]
      (is (= (p/path test-file-system "/some/nested/directory/file.txt")
            (p/resolve base-path relative-path))))))

(deftest resolve-sibling
  (testing "resolves one path relative to another"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          base-path (p/path test-file-system "/some/nested/file.txt")
          relative-path (p/path test-file-system "directory/file.txt")]
      (is (= (p/path test-file-system "/some/nested/directory/file.txt")
            (p/resolve-sibling base-path relative-path)))))

  (testing "resolves a string path relative to a path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          base-path (p/path test-file-system "/some/nested/file.txt")
          relative-path "directory/file.txt"]
      (is (= (p/path test-file-system "/some/nested/directory/file.txt")
            (p/resolve-sibling base-path relative-path))))))

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

(deftest root
  (testing "gets the root for the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= (p/path test-file-system "/")
            (p/root path))))))

(deftest parent
  (testing "gets the parent directory for the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= (p/path test-file-system "/some/nested/directory")
            (p/parent path))))))

(deftest file-name
  (testing "gets the file name for the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= (p/path test-file-system "file.txt")
            (p/file-name path))))))

(deftest name-count
  (testing "gets the number of names in the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= 4 (p/name-count path))))))

(deftest name
  (testing "gets the name with the provided index"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= (p/path test-file-system "nested")
            (p/name path 1))))))

(deftest names
  (testing "gets all names in the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (= [(p/path test-file-system "some")
              (p/path test-file-system "nested")
              (p/path test-file-system "directory")
              (p/path test-file-system "file.txt")]
            (p/names path))))))

(deftest starts-with?
  (testing "returns true if path starts with other path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          other-path (p/path test-file-system "/some/nested")]
      (is (true? (p/starts-with? test-path other-path)))))

  (testing "returns false if path does not start with other path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          other-path (p/path test-file-system "/other/directory")]
      (is (false? (p/starts-with? test-path other-path)))))

  (testing "returns true if path starts with string"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          path-as-string "/some/nested"]
      (is (true? (p/starts-with? test-path path-as-string)))))

  (testing "returns false if path does not start with string"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          path-as-string "/other/directory"]
      (is (false? (p/starts-with? test-path path-as-string))))))

(deftest ends-with?
  (testing "returns true if path ends with other path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          other-path (p/path test-file-system "directory/file.txt")]
      (is (true? (p/ends-with? test-path other-path)))))

  (testing "returns false if path does not end with other path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          other-path (p/path test-file-system "other/file.html")]
      (is (false? (p/ends-with? test-path other-path)))))

  (testing "returns true if path ends with string"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          path-as-string "directory/file.txt"]
      (is (true? (p/ends-with? test-path path-as-string)))))

  (testing "returns false if path does not end with string"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          test-path (p/path test-file-system "/some/nested/directory/file.txt")
          path-as-string "other/file.html"]
      (is (false? (p/ends-with? test-path path-as-string))))))

(deftest absolute?
  (testing "returns true when path is absolute"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/nested/directory/file.txt")]
      (is (true? (p/absolute? path)))))

  (testing "returns false when path is absolute"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "directory/file.txt")]
      (is (false? (p/absolute? path))))))

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

(deftest ->uri
  (testing "converts the path to a URI"
    (let [test-file-system-name (random-file-system-name)
          test-file-system
          (new-in-memory-file-system test-file-system-name)

          path (p/path test-file-system "/directory/index.html")]
      (is (= (URI. (str "jimfs://" test-file-system-name
                     "/directory/index.html"))
            (p/->uri path))))))

(deftest ->absolute-path
  (testing "converts the path to an absolute path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "directory/index.html")]
      (is (= (p/path test-file-system "/directory/index.html")
            (p/->absolute-path path))))))

(deftest ->real-path
  (testing "converts the path to a real path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration)
            [[:directory
              [:index.html {:content ["<html></html>"]}]
              [:nested {:type :directory}]]])

          path (p/path test-file-system "directory/nested/../index.html")]
      (is (= (p/path test-file-system "/directory/index.html")
            (p/->real-path path)))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration)
            [[:directory
              [:index.html {:type   :symbolic-link
                            :target "/directory/other.html"}]
              [:other.html {:content ["<html></html>"]}]
              [:nested {:type :directory}]]])

          path (p/path test-file-system "directory/nested/../index.html")]
      (is (= (p/path test-file-system "/directory/other.html")
            (p/->real-path path)))))

  (testing "does not follow symbolic links when specified"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration)
            [[:directory
              [:index.html {:type   :symbolic-link
                            :target "/directory/other.html"}]
              [:other.html {:content ["<html></html>"]}]
              [:nested {:type :directory}]]])

          path (p/path test-file-system "directory/nested/../index.html")]
      (is (= (p/path test-file-system "/directory/index.html")
            (p/->real-path path :no-follow-links))))))

(deftest ->file
  (testing "converts the path to a file"
    (let [path (p/path "directory/nested/index.html")
          file (io/file "directory/nested/index.html")]
      (is (= file (p/->file path))))))
