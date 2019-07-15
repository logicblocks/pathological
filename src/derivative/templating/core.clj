(ns derivative.templating.core
  (:require
    [hbs.core :as templates]
    [hbs.helper :as template-helpers]

    [derivative.templating.helpers :as template-helper-fns]))

(defn registry [& {:as helpers}]
  (reduce
    (fn [registry [name helper]]
      (template-helpers/register-helper! registry name helper))
    (templates/registry (templates/classpath-loader))
    helpers))

(def ^:dynamic *registry*
  (registry
    "snake_case" template-helper-fns/snake-case
    "camel_case" template-helper-fns/camel-case
    "upper_camel_case" template-helper-fns/upper-camel-case
    "kebab_case" template-helper-fns/kebab-case
    "upper_case" template-helper-fns/upper-case
    "lower_case" template-helper-fns/lower-case

    "increment" template-helper-fns/increment
    "decrement" template-helper-fns/decrement

    "random_password" template-helper-fns/random-password))

(defn render
  ([template context]
   (render *registry* template context))
  ([registry template context]
   (templates/render registry template context)))
