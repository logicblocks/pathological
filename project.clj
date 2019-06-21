(defproject logicblocks/derivative "0.0.1"
  :description "General purpose in place scaffolding and source code rewrite tool."
  :url "https://github.com/logicblocks/derivative"
  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[clj-commons/fs "1.5.0"]
                 [com.google.jimfs/jimfs "1.1"]]
  :plugins [[lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-eftest "0.5.3"]]
  :profiles {:shared      {:dependencies   [[org.clojure/clojure "1.10.0"]
                                            [eftest "0.5.3"]]
                           :resource-paths ["test_resources"]}
             :dev         [:shared {:source-paths ["dev"]}]
             :unit        [:shared {:test-paths ^:replace ["test/unit"]
                                    :eftest     {:multithread? false}}]
             :integration [:shared {:test-paths ^:replace ["test/integration"]
                                    :eftest     {:multithread? false}}]}
  :eftest {:multithread? false}
  :test-paths ["test/unit" "test/integration"]
  :deploy-repositories {"releases" {:url   "https://repo.clojars.org"
                                    :creds :gpg}}
  :release-tasks [["shell" "git" "diff" "--exit-code"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["vcs" "push"]]
  :aliases {"test" ["do"
                    ["with-profile" "unit" "eftest" ":all"]
                    ["with-profile" "integration" "eftest" ":all"]]})
