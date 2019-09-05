(ns pathological.path-matchers-test
  (:require
    [clojure.test :refer :all]

    [pathological.file-systems :as fs]
    [pathological.path-matchers :as pm]
    [pathological.paths :as p]
    [pathological.testing :as t]))

(deftest path-matcher
  (testing "uses the default file system when none specified"
    (let [unix-test-file-system (t/new-unix-in-memory-file-system)
          windows-test-file-system (t/new-windows-in-memory-file-system)]
      (with-bindings {#'fs/*file-system* windows-test-file-system}
        (let [path-matcher (pm/path-matcher "glob:C:\\\\*.html")

              matching-windows-path
              (p/path windows-test-file-system "C:\\index.html")
              mismatching-windows-path
              (p/path windows-test-file-system "C:\\data.csv")

              matching-unix-path
              (p/path unix-test-file-system "/index.html")
              mismatching-unix-path
              (p/path unix-test-file-system "/data.csv")]

          (is (true? (pm/matches? path-matcher matching-windows-path)))
          (is (false? (pm/matches? path-matcher mismatching-windows-path)))
          (is (false? (pm/matches? path-matcher matching-unix-path)))
          (is (false? (pm/matches? path-matcher mismatching-unix-path)))))))

  (testing "uses the specified file system when supplied"
    (let [unix-test-file-system (t/new-unix-in-memory-file-system)
          windows-test-file-system (t/new-windows-in-memory-file-system)]
      (with-bindings {#'fs/*file-system* unix-test-file-system}
        (let [path-matcher
              (pm/path-matcher windows-test-file-system "glob:C:\\\\*.html")

              matching-windows-path
              (p/path windows-test-file-system "C:\\index.html")
              mismatching-windows-path
              (p/path windows-test-file-system "C:\\data.csv")

              matching-unix-path
              (p/path unix-test-file-system "/index.html")
              mismatching-unix-path
              (p/path unix-test-file-system "/data.csv")]

          (is (true? (pm/matches? path-matcher matching-windows-path)))
          (is (false? (pm/matches? path-matcher mismatching-windows-path)))
          (is (false? (pm/matches? path-matcher matching-unix-path)))
          (is (false? (pm/matches? path-matcher mismatching-unix-path))))))))
