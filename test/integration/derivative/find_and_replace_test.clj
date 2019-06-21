(ns derivative.find-and-replace-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.io
     :refer [make-parents
             delete-file]]

    [derivative.test-support
     :refer [multiline-str
             with-empty-directory]]

    [derivative.core :as derivative]))

(let [work-dir "work"]
  (use-fixtures :each
    (with-empty-directory work-dir))

  (deftest finds-and-replaces-one-occurrence-in-single-file-in-place
    (let [file-path "work/test.rb"

          initial-content
          (multiline-str
            "def thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"
            "")

          _ (make-parents file-path)
          _ (spit file-path initial-content)

          pipeline
          [{:type          :find-and-replace
            :configuration {:find    "thing"
                            :replace "other_thing"
                            :in      (str "path:./" file-path)}}]

          _ (derivative/derive pipeline
              {:source-directory "."
               :target-directory "."})

          final-content (slurp file-path)]
      (is (= final-content
            (multiline-str
              "def other_thing_doer(arg1, arg2)"
              "  arg1 * arg2"
              "end"
              "")))))

  (deftest finds-and-replaces-many-occurrences-in-single-file-in-place
    (let [file-path "work/test.rb"

          initial-content
          (multiline-str
            "class Whatever"
            "  def read_thing(arg1, arg2)"
            "    read_file('thing', arg1, arg2)"
            "  end"
            ""
            "  def write_thing(arg1, arg2)"
            "    write_file('thing', arg1, arg2)"
            "  end"
            "end"
            "")

          _ (make-parents file-path)
          _ (spit file-path initial-content)

          pipeline
          [{:type          :find-and-replace
            :configuration {:find    "thing"
                            :replace "other_thing"
                            :in      (str "path:./" file-path)}}]

          _ (derivative/derive pipeline
              {:source-directory "."
               :target-directory "."})

          final-content (slurp file-path)]
      (is (= final-content
            (multiline-str
              "class Whatever"
              "  def read_other_thing(arg1, arg2)"
              "    read_file('other_thing', arg1, arg2)"
              "  end"
              ""
              "  def write_other_thing(arg1, arg2)"
              "    write_file('other_thing', arg1, arg2)"
              "  end"
              "end"
              "")))))

  (deftest finds-and-replaces-many-occurrences-in-many-files-in-place
    (let [file-1-path "work/lib/whatever.rb"
          file-2-path "work/bin/run"

          initial-content-1
          (multiline-str
            "class Whatever"
            "  def read_thing(arg1, arg2)"
            "    read_file('thing', arg1, arg2)"
            "  end"
            ""
            "  def write_thing(arg1, arg2)"
            "    write_file('thing', arg1, arg2)"
            "  end"
            "end"
            "")

          initial-content-2
          (multiline-str
            "require_relative '../lib/whatever'"
            ""
            "Whatever.new.read_thing('first', 'second')"
            "")

          _ (make-parents file-1-path)
          _ (make-parents file-2-path)
          _ (spit file-1-path initial-content-1)
          _ (spit file-2-path initial-content-2)

          pipeline
          [{:type          :find-and-replace
            :configuration {:find    "thing"
                            :replace "other_thing"
                            :in      "glob:./work/**/*"}}]

          _ (derivative/derive pipeline
              {:source-directory "."
               :target-directory "."})

          final-content-1 (slurp file-1-path)
          final-content-2 (slurp file-2-path)]
      (is (= final-content-1
            (multiline-str
              "class Whatever"
              "  def read_other_thing(arg1, arg2)"
              "    read_file('other_thing', arg1, arg2)"
              "  end"
              ""
              "  def write_other_thing(arg1, arg2)"
              "    write_file('other_thing', arg1, arg2)"
              "  end"
              "end"
              "")))
      (is (= final-content-2
            (multiline-str
              "require_relative '../lib/whatever'"
              ""
              "Whatever.new.read_other_thing('first', 'second')"
              ""))))))
