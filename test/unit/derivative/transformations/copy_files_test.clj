(ns derivative.transformations.copy-files-test
  (:require
    [clojure.test :refer :all]

    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]]

    [derivative.transformations.core :as transformations]
    [derivative.transformations.copy-files]))

(deftest copies-one-file-to-another-file
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
    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

(deftest copies-one-file-to-directory
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
                         :to   "directory:target/"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content (f/read-all-lines
                        (p/path file-system "target/source/test.rb"))]
    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

(deftest copies-directory-of-files-to-directory
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:source
            [:file1.rb {:content ["def thing_doer(arg1, arg2)"
                                  "  arg1 * arg2"
                                  "end"]}]
            [:file2.rb {:content ["def other_thing_doer(arg1, arg2)"
                                  "  arg1 + arg2"
                                  "end"]}]]])

        transformation
        {:type          :copy
         :configuration {:from "directory:source/"
                         :to   "directory:target/"}}]
    (transformations/apply-transformation transformation
      {:file-system file-system})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines (p/path file-system "target/source/file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines (p/path file-system "target/source/file2.rb"))))))

(deftest allows-directories-to-be-stripped
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:source
            [:file1.rb {:content ["def thing_doer(arg1, arg2)"
                                  "  arg1 * arg2"
                                  "end"]}]
            [:file2.rb {:content ["def other_thing_doer(arg1, arg2)"
                                  "  arg1 + arg2"
                                  "end"]}]]])

        transformation
        {:type          :copy
         :configuration {:from "directory:source/"
                         :to   "directory:target/"
                         :strip-names 1}}]
    (transformations/apply-transformation transformation
      {:file-system file-system})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines (p/path file-system "target/file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines (p/path file-system "target/file2.rb"))))))

; :transform
; validations