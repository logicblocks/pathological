(ns derivative.cli
  (:require
    [clojure.tools.cli :as cli]

    [derivative.cli.plan :as plan]))

(defn subcommand-name [arguments]
  (keyword (first arguments)))

(defn subcommand-lookup [subcommands]
  (fn [name]
    (get subcommands name)))

(def subcommands
  {:plan #'plan/execute})

(def subcommand (subcommand-lookup subcommands))

(defn global-cli-options []
  [])

(defn execute [arguments dependencies]
  (let [{:keys [arguments]}
        (cli/parse-opts arguments (global-cli-options) :in-order true)

        subcommand-name (subcommand-name arguments)
        subcommand (subcommand subcommand-name)]
    (if subcommand
      (subcommand (drop 1 arguments) dependencies)))
  0)
