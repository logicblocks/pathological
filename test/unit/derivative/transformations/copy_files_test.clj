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
        {:type          :copy-files
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
        {:type          :copy-files
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
        {:type          :copy-files
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

(deftest copies-all-files-matching-glob-to-directory
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
                                  "end"]}]
            [:file3.html {:content ["<html></html>"]}]]])

        transformation
        {:type          :copy-files
         :configuration {:from "glob:./source/*.rb"
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
          (f/read-all-lines (p/path file-system "target/source/file2.rb"))))
    (is (false? (f/exists? (p/path file-system "target/source/file3.html"))))))

(deftest allows-directories-to-be-removed
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
        {:type :copy-files
         :configuration
               {:from      "directory:source/"
                :to        "directory:target/"
                :transform [{:type          :remove-directories
                             :configuration {:count 1}}]}}]
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

(deftest allows-find-and-replace-rename-during-copy
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:directory1
            [:directory2
             [:file1.rb {:content ["def thing_doer(arg1, arg2)"
                                   "  arg1 * arg2"
                                   "end"]}]
             [:file2.rb {:content ["def other_thing_doer(arg1, arg2)"
                                   "  arg1 + arg2"
                                   "end"]}]]]])

        transformation
        {:type :copy-files
         :configuration
               {:from      "directory:directory1/"
                :to        "directory:directory3/"
                :transform [{:type          :find-and-replace
                             :configuration {:find    "directory2"
                                             :replace "directory4"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory4/file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory4/file2.rb"))))))

(deftest applies-many-path-transforms-in-order
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:directory1
            [:directory2
             [:file1.rb {:content ["def thing_doer(arg1, arg2)"
                                   "  arg1 * arg2"
                                   "end"]}]
             [:file2.rb {:content ["def other_thing_doer(arg1, arg2)"
                                   "  arg1 + arg2"
                                   "end"]}]]]])

        transformation
        {:type :copy-files
         :configuration
               {:from      "directory:directory1/"
                :to        "directory:directory3/"
                :transform [{:type          :remove-directories
                             :configuration {:count 2}}
                            {:type          :find-and-replace
                             :configuration {:find    ".rb"
                                             :replace ".ruby"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines (p/path file-system "directory3/file1.ruby"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines (p/path file-system "directory3/file2.ruby"))))))

; regex
; vars

; validation
; empty directories
