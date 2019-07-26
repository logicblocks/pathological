(ns derivative.cli.plan
  (:require
    [clojure.tools.cli :as cli]

    [pathological.paths :as paths]
    [pathological.files :as files]))

(defn plan-cli-options [dependencies]
  [["-c" "--configuration PATH"
    "Configuration file containing derivation definitions"
    :parse-fn (fn [path] (paths/path (:file-system dependencies) path))
    :validate [(fn [path] (files/exists? path))]]])

(defn execute [arguments dependencies]
  (let [{:keys [options]}
        (cli/parse-opts arguments (plan-cli-options dependencies)
          :in-order true)

        configuration-path (:configuration options)
        configuration-contents (slurp configuration-path)
        pipeline (read-string configuration-contents)]
    ))
