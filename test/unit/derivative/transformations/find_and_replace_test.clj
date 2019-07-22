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
         :configuration {:find    "string:thing"
                         :replace "string:other_thing"
                         :in      "file:work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

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
         :configuration {:find    "string:thing"
                         :replace "string:other_thing"
                         :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["class Whatever"
            "  def read_other_thing(arg1, arg2)"
            "    read_file('other_thing', arg1, arg2)"
            "  end"
            ""
            "  def write_other_thing(arg1, arg2)"
            "    write_file('other_thing', arg1, arg2)"
            "  end"
            "end"]
          final-content))))

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
         :configuration {:find    "string:thing"
                         :replace "string:other_thing"
                         :in      "glob:./work/**/*"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content-1
        (f/read-all-lines (p/path file-system "work/lib/whatever.rb"))
        final-content-2
        (f/read-all-lines (p/path file-system "work/bin/run"))]
    (is (=
          ["class Whatever"
           "  def read_other_thing(arg1, arg2)"
           "    read_file('other_thing', arg1, arg2)"
           "  end"
           ""
           "  def write_other_thing(arg1, arg2)"
           "    write_file('other_thing', arg1, arg2)"
           "  end"
           "end"]
          final-content-1))
    (is (= ["require_relative '../lib/whatever'"
            ""
            "Whatever.new.read_other_thing('first', 'second')"]
          final-content-2))))

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
         :configuration {:find    "regex:arg(\\d+)"
                         :replace "string:argument_{{match.$1}}"
                         :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def thing_doer(argument_1, argument_2)"
            "  argument_1 * argument_2"
            "end"]
          final-content))))

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
         :configuration {:find    "string:thing"
                         :replace "string:{{var.name}}"
                         :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "stuff"}
             :file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def stuff_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

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
         :configuration {:find    "string:{{var.old-name}}"
                         :replace "string:new_name"
                         :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:old-name "old_name"}
             :file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def new_name_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

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
         :configuration {:find    "regex:{{var.arg-prefix}}(\\d+)"
                         :replace "string:argument_{{match.$1}}"
                         :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:arg-prefix "arg"}
             :file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def thing_doer(argument_1, argument_2)"
            "  argument_1 * argument_2"
            "end"]
          final-content))))

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
               {:find    "string:thing"
                :replace "string:{{#snake_case}}{{var.name}}{{/snake_case}}"
                :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "someThing"}
             :file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def some_thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))

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
               {:find    "string:{{#snake_case}}{{var.name}}{{/snake_case}}"
                :replace "string:other_thing"
                :in      "file:./work/test.rb"}}

        _ (transformations/apply-transformation transformation
            {:vars        {:name "someThing"}
             :file-system file-system})

        final-content (f/read-all-lines (p/path file-system "work/test.rb"))]
    (is (= ["def other_thing_doer(arg1, arg2)"
            "  arg1 * arg2"
            "end"]
          final-content))))
