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
                             :configuration {:find    "string:directory2"
                                             :replace "string:directory4"}}]}}]
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

(deftest allows-variable-interpolation-in-path-during-copy
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
                :transform [{:type :find-and-replace
                             :configuration
                                   {:find    "string:directory2"
                                    :replace "string:{{var.new_dir}}"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system
       :vars        {:new_dir "new-directory"}})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/new-directory/file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/new-directory/file2.rb"))))))

(deftest allows-regular-expression-match-and-replacement-in-path-during-copy
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
                :transform [{:type :find-and-replace
                             :configuration
                                   {:find    "regex:file(\\d+)"
                                    :replace "string:thing{{match.$1}}"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/thing1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/thing2.rb"))))))

(deftest allows-variables-to-be-interpolated-into-finders-for-path-when-string
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
                :transform [{:type :find-and-replace
                             :configuration
                                   {:find    "string:{{var.old_directory}}"
                                    :replace "string:new-directory"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system
       :vars        {:old_directory "directory2"}})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/new-directory/file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/new-directory/file2.rb"))))))

(deftest allows-variables-to-be-interpolated-into-finders-for-path-when-regex
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
                :transform [{:type :find-and-replace
                             :configuration
                                   {:find    "regex:{{var.old_file}}(\\d+)"
                                    :replace "string:thing{{match.$1}}"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system
       :vars        {:old_file "file"}})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/thing1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/thing2.rb"))))))

(deftest provides-functions-to-manipulate-replacements-for-path
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
               {:from "directory:directory1/"
                :to   "directory:directory3/"
                :transform
                      [{:type :find-and-replace
                        :configuration
                              {:find    "string:file"
                               :replace (str "string:"
                                          "{{#snake_case}}"
                                          "{{var.new_file}}"
                                          "{{/snake_case}}")}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system
       :vars        {:new_file "new-file"}})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/new_file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/new_file2.rb"))))))

(deftest provides-functions-to-manipulate-finders-for-path
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:directory1
            [:directory2
             [:old_file1.rb {:content ["def thing_doer(arg1, arg2)"
                                       "  arg1 * arg2"
                                       "end"]}]
             [:old_file2.rb {:content ["def other_thing_doer(arg1, arg2)"
                                       "  arg1 + arg2"
                                       "end"]}]]]])

        transformation
        {:type :copy-files
         :configuration
               {:from "directory:directory1/"
                :to   "directory:directory3/"
                :transform
                      [{:type :find-and-replace
                        :configuration
                              {:find    (str "string:"
                                          "{{#snake_case}}"
                                          "{{var.old_file}}"
                                          "{{/snake_case}}")
                               :replace "string:new-file"}}]}}]
    (transformations/apply-transformation transformation
      {:file-system file-system
       :vars        {:old_file "old-file"}})

    (is (= ["def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/new-file1.rb"))))
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 + arg2"
            "end"]
          (f/read-all-lines
            (p/path file-system
              "directory3/directory1/directory2/new-file2.rb"))))))

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
                             :configuration {:find    "string:.rb"
                                             :replace "string:.ruby"}}]}}]
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
