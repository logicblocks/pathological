(ns derivative.find-and-replace-test
  (:require
    [clojure.test :refer :all]
    [clojure.string :as string]
    [clojure.java.io
     :refer [make-parents
             delete-file]]

    [me.raynes.fs
     :refer [delete-dir]]

    [derivative.core :as derivative]))

(defn multiline-str [& strings]
  (string/join "\n" strings))

(defn with-empty-work-directory []
  #(do (delete-dir "work") (%)))

(use-fixtures :each
  (with-empty-work-directory))

(deftest finds-and-replaces-one-occurrence-in-single-file-in-place
  (let [source-file-path "work/test.rb"
        target-file-path "work/test.rb"

        source-content
        (multiline-str
          "def thing_doer(arg1, arg2)"
          "  arg1 * arg2"
          "end")

        _ (make-parents source-file-path)
        _ (spit source-file-path source-content)

        definition
        [{:type          :find-and-replace
          :scope         :file
          :source        source-file-path
          :target        target-file-path
          :configuration {:find    "thing"
                          :replace "other_thing"}}]

        _ (derivative/derive definition ".")

        target-content (slurp target-file-path)]
    (is (= target-content
          (multiline-str
            "def other_thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end")))))

(deftest finds-and-replaces-many-occurrences-in-single-file-in-place
  (let [source-file-path "work/test.rb"
        target-file-path "work/test.rb"

        source-content
        (multiline-str
          "class Whatever"
          "  def open_thing(arg1, arg2)"
          "    read_file('thing', arg1, arg2)"
          "  end"
          ""
          "  def write_thing(arg1, arg2)"
          "    write_file('thing', arg1, arg2)"
          "  end"
          "end")

        _ (make-parents source-file-path)
        _ (spit source-file-path source-content)

        definition
        [{:type :find-and-replace
          :scope :file
          :source "work/test.rb"
          :target "work/test.rb"
          :configuration {:find "thing"
                          :replace "other_thing"}}]

        _ (derivative/derive definition ".")

        target-content (slurp target-file-path)]
    (is (= target-content
          (multiline-str
            "class Whatever"
            "  def open_other_thing(arg1, arg2)"
            "    read_file('other_thing', arg1, arg2)"
            "  end"
            ""
            "  def write_other_thing(arg1, arg2)"
            "    write_file('other_thing', arg1, arg2)"
            "  end"
            "end")))))
