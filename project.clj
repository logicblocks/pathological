(defproject io.logicblocks/pathological "0.0.1"
  :description "A complete wrapper of Java NIO2 for Clojure."
  :url "https://github.com/logicblocks/pathological"

  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}

  :dependencies [[hbs "1.0.3"]
                 [com.google.jimfs/jimfs "1.1"]]

  :plugins [[lein-cloverage "1.1.1"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-eftest "0.5.8"]
            [lein-codox "0.10.7"]]

  :profiles
  {:shared {:dependencies   [[org.clojure/clojure "1.10.1"]
                             [eftest "0.5.8"]]
            :resource-paths ["test_resources"]}
   :dev    [:shared {:source-paths ["dev"]}]
   :test   [:shared {:test-paths ^:replace ["test/unit"]
                     :eftest     {:multithread? false}}]}

  :cloverage
  {:ns-exclude-regex [#"^user"]}

  :codox
  {:namespaces  [#"^pathological\."]
   :output-path "docs"
   :source-uri  "https://github.com/logicblocks/derivative/blob/{version}/{filepath}#L{line}"}

  :deploy-repositories
  {"releases" {:url "https://repo.clojars.org" :creds :gpg}}

  :release-tasks
  [["shell" "git" "diff" "--exit-code"]
   ["change" "version" "leiningen.release/bump-version" "release"]
   ["changelog" "release"]
   ["codox"]
   ["vcs" "commit"]
   ["vcs" "tag"]
   ["deploy"]
   ["change" "version" "leiningen.release/bump-version"]
   ["vcs" "commit"]
   ["vcs" "tag"]
   ["vcs" "push"]]

  :aliases {"test" ["with-profile" "test" "eftest" ":all"]})
