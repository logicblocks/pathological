(ns derivative.path-specs-test
  (:refer-clojure :exclude [resolve])
  (:require
    [clojure.test :refer :all]

    [pathological.paths :as p]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]]

    [derivative.path-specs :as ps]))

(deftest syntax?
  (is (true? (ps/syntax? "file:some/important/file.txt" :file)))
  (is (true? (ps/syntax? "directory:/some/important/directory" "directory")))
  (is (false? (ps/syntax? "file:some/important/file.txt" :glob)))
  (is (false? (ps/syntax? "directory:/some/important/directory" "regex"))))

(deftest file-syntax?
  (is (true? (ps/file-syntax? "file:some/important/file.txt")))
  (is (false? (ps/file-syntax? "directory:/some/important/directory"))))

(deftest directory-syntax?
  (is (true? (ps/directory-syntax? "directory:/some/important/directory")))
  (is (false? (ps/directory-syntax? "file:some/important/file.txt"))))

(deftest strip-syntax
  (is (= "/some/important/directory"
        (ps/strip-syntax "directory:/some/important/directory")))
  (is (= "some/important/file.txt"
        (ps/strip-syntax "file:some/important/file.txt")))
  (is (= "some/things/**/*.txt"
        (ps/strip-syntax "glob:some/things/**/*.txt")))
  (is (= "some/things/[^/]*?/(.*).txt"
        (ps/strip-syntax "regex:some/things/[^/]*?/(.*).txt"))))

(deftest expand-paths
  (testing "returns a single path when syntax is file"
    (let [file-system
          (new-in-memory-file-system (random-file-system-name))

          base-path
          (p/path file-system ".")]
      (is (= [(p/path file-system "some/important/file.txt")]
            (ps/expand-paths base-path "file:some/important/file.txt")))
      (is (= [(p/path file-system "./some/important/file.txt")]
            (ps/expand-paths base-path "file:./some/important/file.txt")))
      (is (= [(p/path file-system "/some/important/file.txt")]
            (ps/expand-paths base-path "file:/some/important/file.txt")))))

  (testing "returns all paths in a directory when syntax is directory"
    (let [file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :working-directory "/base-directory-1")
            [[:base-directory-1
              [:directory-1
               [:file-1.rb {:type :file}]
               [:file-2.txt {:type :file}]
               [:subdirectory-1
                [:file-3.txt {:type :file}]
                [:file-4.rb {:type :file}]]]]
             [:base-directory-2
              [:directory-2
               [:file-5.rb {:type :file}]
               [:file-6.txt {:type :file}]
               [:subdirectory-2
                [:file-7.txt {:type :file}]
                [:file-8.rb {:type :file}]]]]])

          base-path
          (p/path file-system ".")]
      (is (= [(p/path file-system "directory-1")
              (p/path file-system "directory-1/file-1.rb")
              (p/path file-system "directory-1/file-2.txt")
              (p/path file-system "directory-1/subdirectory-1")
              (p/path file-system "directory-1/subdirectory-1/file-3.txt")
              (p/path file-system "directory-1/subdirectory-1/file-4.rb")]
            (ps/expand-paths base-path "directory:directory-1")))

      (is (= [(p/path file-system "./directory-1")
              (p/path file-system "./directory-1/file-1.rb")
              (p/path file-system "./directory-1/file-2.txt")
              (p/path file-system "./directory-1/subdirectory-1")
              (p/path file-system "./directory-1/subdirectory-1/file-3.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-4.rb")]
            (ps/expand-paths base-path "directory:./directory-1")))

      (is (= [(p/path file-system "/base-directory-2/directory-2")
              (p/path file-system "/base-directory-2/directory-2/file-5.rb")
              (p/path file-system "/base-directory-2/directory-2/file-6.txt")
              (p/path file-system
                "/base-directory-2/directory-2/subdirectory-2")
              (p/path file-system
                "/base-directory-2/directory-2/subdirectory-2/file-7.txt")
              (p/path file-system
                "/base-directory-2/directory-2/subdirectory-2/file-8.rb")]
            (ps/expand-paths base-path
              "directory:/base-directory-2/directory-2")))))

  (testing
    (str "returns all matching paths relative to the base directory "
      "when syntax is glob")
    (let [file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :working-directory "/base-directory-1")
            [[:base-directory-1
              [:directory-1
               [:file-1.rb {:type :file}]
               [:file-2.txt {:type :file}]
               [:subdirectory-1
                [:file-3.txt {:type :file}]
                [:file-4.rb {:type :file}]]]]
             [:base-directory-2
              [:directory-2
               [:file-5.rb {:type :file}]
               [:file-6.txt {:type :file}]
               [:subdirectory-2
                [:file-7.txt {:type :file}]
                [:file-8.rb {:type :file}]]]]])

          base-path
          (p/path file-system ".")]
      (is (= [(p/path file-system "./directory-1/file-1.rb")
              (p/path file-system "./directory-1/file-2.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-3.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-4.rb")]
            (ps/expand-paths base-path "glob:**directory-1**file*")))

      (is (= [(p/path file-system "./directory-1/file-2.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-3.txt")]
            (ps/expand-paths base-path "glob:./directory-1/**file*.txt")))))

  (testing
    (str "returns all matching paths relative to the base directory "
      "when syntax is regex")
    (let [file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :working-directory "/base-directory-1")
            [[:base-directory-1
              [:directory-1
               [:file-1.rb {:type :file}]
               [:file-2.txt {:type :file}]
               [:subdirectory-1
                [:file-3.txt {:type :file}]
                [:file-4.rb {:type :file}]]]]
             [:base-directory-2
              [:directory-2
               [:file-5.rb {:type :file}]
               [:file-6.txt {:type :file}]
               [:subdirectory-2
                [:file-7.txt {:type :file}]
                [:file-8.rb {:type :file}]]]]])

          base-path
          (p/path file-system ".")]
      (is (= [(p/path file-system "./directory-1/file-1.rb")
              (p/path file-system "./directory-1/file-2.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-3.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-4.rb")]
            (ps/expand-paths base-path "regex:.*file.*")))

      (is (= [(p/path file-system "./directory-1/file-2.txt")
              (p/path file-system "./directory-1/subdirectory-1/file-3.txt")]
            (ps/expand-paths base-path "regex:\\.\\/.*file.*\\.txt"))))))

#_(deftest resolve-path)
