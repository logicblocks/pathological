(defproject io.logicblocks/pathological "0.1.22-RC3"
  :description "A thin but complete wrapper of Java NIO2 for Clojure."
  :url "https://github.com/logicblocks/pathological"

  :license {:name "The MIT License"
            :url  "https://opensource.org/licenses/MIT"}

  :plugins [[lein-cloverage "1.2.4"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.7.0"]
            [lein-changelog "0.3.2"]
            [lein-cprint "1.3.3"]
            [lein-eftest "0.6.0"]
            [lein-codox "0.10.8"]
            [lein-cljfmt "0.9.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.4.0"]]

  :profiles
  {:provided
   {:dependencies
    [[com.google.jimfs/jimfs "1.3.0"]
     [org.mockito/mockito-core "5.4.0"]]}

   :shared
   ^{:pom-scope :test}
   {:dependencies [[org.clojure/clojure "1.11.1"]
                   [org.clojure/tools.trace "0.7.11"]

                   [nrepl "1.1.0"]

                   [org.apache.tika/tika-core "1.28.5"]
                   [org.apache.tika/tika-java7 "1.28.5"]
                   [org.apache.tika/tika-parsers "1.28.5"
                    :exclusions [com.google.guava/guava]]

                   [ch.qos.logback/logback-classic "1.4.14"]

                   [eftest "0.6.0"]]}

   :dev
   [:provided :shared {:source-paths ["dev"]
                       :eftest       {:multithread? false}}]

   :test
   [:provided :shared {:eftest {:multithread? false}}]

   :prerelease
   {:release-tasks
    [["shell" "git" "diff" "--exit-code"]
     ["change" "version" "leiningen.release/bump-version" "rc"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["vcs" "commit" "Pre-release version %s [skip ci]"]
     ["vcs" "tag"]
     ["deploy"]]}

   :release
   {:release-tasks
    [["shell" "git" "diff" "--exit-code"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["codox"]
     ["changelog" "release"]
     ["shell" "sed" "-E" "-i.bak" "s/\"[0-9]+\\.[0-9]+\\.[0-9]+\"/\"${:version}\"/g" "README.md"]
     ["shell" "rm" "-f" "README.md.bak"]
     ["shell" "git" "add" "."]
     ["vcs" "commit" "Release version %s [skip ci]"]
     ["vcs" "tag"]
     ["deploy"]
     ["change" "version" "leiningen.release/bump-version" "patch"]
     ["change" "version" "leiningen.release/bump-version" "rc"]
     ["change" "version" "leiningen.release/bump-version" "release"]
     ["vcs" "commit" "Pre-release version %s [skip ci]"]
     ["vcs" "tag"]
     ["vcs" "push"]]}}

  :cloverage
  {:ns-exclude-regex [#"^user"]}

  :codox
  {:namespaces  [#"^pathological\."]
   :metadata    {:doc/format :markdown}
   :output-path "docs"
   :doc-paths   ["docs"]
   :source-uri  "https://github.com/logicblocks/pathological/blob/{version}/{filepath}#L{line}"}

  :cljfmt {:indents {#".*"     [[:inner 0]]
                     defrecord [[:block 1] [:inner 1]]
                     deftype   [[:block 1] [:inner 1]]}}

  :bikeshed {:name-collisions false
             :long-lines      false}

  :eastwood {:config-files ["config/linter.clj"]}

  :deploy-repositories
  {"releases"  {:url "https://repo.clojars.org" :creds :gpg}
   "snapshots" {:url "https://repo.clojars.org" :creds :gpg}})
