(defproject logicblocks/derivative "0.0.1"
  :description "General purpose in place scaffolding and source code rewrite tool."
  :url "https://github.com/logicblocks/derivative"
  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[cljstache "2.0.4"]
                 [camel-snake-kebab "672421b575737c5496b7ddcfb83cf150b0d0bc75"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :plugins [[lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-eftest "0.5.3"]
            [reifyhealth/lein-git-down "0.3.5"]]
  :profiles {:shared      {:dependencies   [[org.clojure/clojure "1.10.0"]
                                            [eftest "0.5.3"]
                                            [com.google.jimfs/jimfs "1.1"]]
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
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {camel-snake-kebab {:coordinates clj-commons/camel-snake-kebab}}
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
