(defproject io.logicblocks/pathological "0.1.15"
  :description "A thin but complete wrapper of Java NIO2 for Clojure."
  :url "https://github.com/logicblocks/pathological"

  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}

  :plugins [[lein-cloverage "1.1.1"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-eftest "0.5.8"]
            [lein-codox "0.10.7"]
            [lein-cljfmt "0.6.4"]
            [lein-kibit "0.1.6"]
            [lein-bikeshed "0.5.1"]]

  :profiles
  {:provided {:dependencies
              [[com.google.jimfs/jimfs "1.1"]
               [org.mockito/mockito-core "3.0.0"]]}
   :shared   [:provided
              {:dependencies
               [[org.clojure/clojure "1.10.1"]
                [nrepl "0.6.0"]
                [org.apache.tika/tika-core "1.22"]
                [org.apache.tika/tika-parsers "1.22"
                 :exclusions [com.google.guava/guava]]
                [ch.qos.logback/logback-classic "1.2.3"]
                [eftest "0.5.8"]]}]
   :dev      [:shared {:source-paths ["dev"]
                       :eftest       {:multithread? false}}]
   :test     [:shared {:eftest {:multithread? false}}]}

  :cloverage
  {:ns-exclude-regex [#"^user"]}

  :codox
  {:namespaces  [#"^pathological\."]
   :metadata    {:doc/format :markdown}
   :output-path "docs"
   :doc-paths   ["docs"]
   :source-uri  "https://github.com/logicblocks/pathological/blob/{version}/{filepath}#L{line}"}

  :cljfmt {:indents ^:replace {#".*" [[:inner 0]]}}

  :deploy-repositories
  {"releases" {:url "https://repo.clojars.org" :creds :gpg}}

  :release-tasks
  [["shell" "git" "diff" "--exit-code"]
   ["change" "version" "leiningen.release/bump-version" "release"]
   ["codox"]
   ["changelog" "release"]
   ["shell" "sed" "-E" "-i" "" "s/\"[0-9]+\\.[0-9]+\\.[0-9]+\"/\"${:version}\"/g" "README.md"]
   ["shell" "git" "add" "."]
   ["vcs" "commit"]
   ["vcs" "tag"]
   ["deploy"]
   ["change" "version" "leiningen.release/bump-version"]
   ["vcs" "commit"]
   ["vcs" "tag"]
   ["vcs" "push"]]

  :aliases {"test"      ["with-profile" "test" "eftest" ":all"]
            "precommit" ["do"
                         ["check"]
                         ["kibit"]
                         ["cljfmt" "fix"]
                         ["bikeshed"
                          "--name-collisions" "false"
                          "--verbose" "true"]
                         ["test"]]})
