(ns cli_plan_test
  (:require
    [clojure.test :refer :all]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration]]

    [derivative.cli :as cli]

    [support.ansi :as ansi]
    [support.strings :as strings]
    [support.streams :as streams]
    [support.assertions :refer :all]))

(deftest outputs-a-diff-for-creating-a-new-file-from-an-existing-file
  (let [standard-streams (streams/new-string-streams)

        configuration
        {:pipeline [{:type          :copy-files
                     :configuration {:from "template/example.rb"
                                     :to   "src/example.rb"}}]}

        content ["def thing_doer (arg1, arg2)"
                 "  arg1 * arg2"
                 "end"]

        file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (unix-configuration)
          [[:configuration.clj
            {:content [(strings/pstr configuration)]}]
           [:template
            [:example.rb
             {:content content}]]])

        exit-code (cli/execute
                    ["plan" "--configuration" "./configuration.clj"]
                    {:file-system      file-system
                     :standard-streams standard-streams})]

    (is (clean-exit exit-code))
    (has (standard-output-including
           (strings/join-lines
             (ansi/-> "diff --git a/src/example.rb b/src/example.rb" :bold)
             (ansi/-> "new file mode 100644" :bold)
             (ansi/-> "index 0000000..eb3514c" :bold)
             (ansi/-> "--- /dev/null" :bold)
             (ansi/-> "+++ b/src/example.rb" :bold)
             (ansi/-> "@@ -0,0 +1,4 @@" :cyan-fg)
             (ansi/-> "+def thing_doer (arg1, arg2)" :green-fg)
             (ansi/-> "+  arg1 * arg2" :green-fg)
             (ansi/-> "+end" :green-fg)
             (ansi/-> "+" :green-fg))
           standard-streams))))
