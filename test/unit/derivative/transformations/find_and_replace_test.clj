(ns derivative.transformations.find-and-replace-test
  (:require
    [clojure.test :refer :all]

    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]]

    [derivative.transformations.core :as transformations]
    [derivative.transformations.find-and-replace]))

(deftest finds-and-replaces-one-occurrence-in-single-file-in-place
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    "thing"
                         :replace "other_thing"
                         :in      "path:work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def other_thing_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))

(deftest finds-and-replaces-many-occurrences-in-single-file-in-place
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["class Whatever"
               "  def read_thing(arg1, arg2)"
               "    read_file('thing', arg1, arg2)"
               "  end"
               ""
               "  def write_thing(arg1, arg2)"
               "    write_file('thing', arg1, arg2)"
               "  end"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    "thing"
                         :replace "other_thing"
                         :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["class Whatever"
           "  def read_other_thing(arg1, arg2)"
           "    read_file('other_thing', arg1, arg2)"
           "  end"
           ""
           "  def write_other_thing(arg1, arg2)"
           "    write_file('other_thing', arg1, arg2)"
           "  end"
           "end"]))))

(deftest finds-and-replaces-many-occurrences-in-many-files-in-place
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:lib
             [:whatever.rb
              {:content
               ["class Whatever"
                "  def read_thing(arg1, arg2)"
                "    read_file('thing', arg1, arg2)"
                "  end"
                ""
                "  def write_thing(arg1, arg2)"
                "    write_file('thing', arg1, arg2)"
                "  end"
                "end"]}]]
            [:bin
             [:run
              {:content
               ["require_relative '../lib/whatever'"
                ""
                "Whatever.new.read_thing('first', 'second')"]}]]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    "thing"
                         :replace "other_thing"
                         :in      "glob:./work/**/*"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system
             :directories {:source "." :target "."}})

        final-content-1
        (f/read-all-lines (p/path file-system "work/lib/whatever.rb"))
        final-content-2
        (f/read-all-lines (p/path file-system "work/bin/run"))]
    (is (= final-content-1
          ["class Whatever"
           "  def read_other_thing(arg1, arg2)"
           "    read_file('other_thing', arg1, arg2)"
           "  end"
           ""
           "  def write_other_thing(arg1, arg2)"
           "    write_file('other_thing', arg1, arg2)"
           "  end"
           "end"]))
    (is (= final-content-2
          ["require_relative '../lib/whatever'"
           ""
           "Whatever.new.read_other_thing('first', 'second')"]))))

(deftest finds-and-replaces-based-on-regular-expression-when-supplied
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    #"arg(\d+)"
                         :replace "argument_{{match.$1}}"
                         :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def thing_doer(argument_1, argument_2)"
           "  argument_1 * argument_2"
           "end"]))))

(deftest allows-variables-to-be-interpolated-into-replacements
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    "thing"
                         :replace "{{var.name}}"
                         :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "stuff"}
             :file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def stuff_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))

(deftest allows-variables-to-be-interpolated-into-finders-when-string
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def old_name_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    "{{var.old-name}}"
                         :replace "new_name"
                         :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:old-name "old_name"}
             :file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def new_name_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))

(deftest allows-variables-to-be-interpolated-into-finders-when-regex
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type          :find-and-replace
         :configuration {:find    #"\{\{var.arg-prefix\}\}(\d+)"
                         :replace "argument_{{match.$1}}"
                         :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:arg-prefix "arg"}
             :file-system file-system
             :directories {:source "." :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def thing_doer(argument_1, argument_2)"
           "  argument_1 * argument_2"
           "end"]))))

(deftest provides-functions-to-manipulate-replacements
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type :find-and-replace
         :configuration
               {:find    "thing"
                :replace "{{#snake_case}}{{var.name}}{{/snake_case}}"
                :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "someThing"}
             :file-system file-system
             :directories {:source "."
                           :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def some_thing_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))

(deftest provides-functions-to-manipulate-finders
  (let [file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:work
            [:test.rb
             {:content
              ["def some_thing_doer(arg1, arg2)"
               "  arg1 * arg2"
               "end"]}]]])

        transformation
        {:type :find-and-replace
         :configuration
               {:find    "{{#snake_case}}{{var.name}}{{/snake_case}}"
                :replace "other_thing"
                :in      "path:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "someThing"}
             :file-system file-system
             :directories {:source "."
                           :target "."}})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= final-content
          ["def other_thing_doer(arg1, arg2)"
           "  arg1 * arg2"
           "end"]))))
