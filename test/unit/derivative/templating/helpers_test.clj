(ns derivative.templating.helpers-test
  (:require
    [clojure.test :refer :all]
    [clojure.string :as string]

    [hbs.core :as tpl]
    [hbs.helper :as tpl-helper]

    [derivative.templating.helpers :as tpl-helper-fns]))

(def prepend
  (tpl-helper/helper [context options]
    (tpl-helper/safe-str
      (str (tpl-helper/param options 0)
        (tpl-helper/block-body options context)))))

(defn registry [& {:as helpers}]
  (reduce
    (fn [registry [name helper]]
      (tpl-helper/register-helper! registry name helper))
    (tpl/registry (tpl/classpath-loader))
    helpers))

(defn renderer [registry]
  (fn [& template-fragments]
    (tpl/render registry (string/join template-fragments) {})))

(deftest snake-case
  (let [registry
        (registry
          "snake_case" tpl-helper-fns/snake-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "thing" (render "{{#snake_case}}thing{{/snake_case}}")))
    (is (= "the_thing" (render "{{#snake_case}}the_thing{{/snake_case}}")))
    (is (= "the_thing" (render "{{#snake_case}}the-thing{{/snake_case}}")))
    (is (= "the_thing" (render "{{#snake_case}}theThing{{/snake_case}}")))
    (is (= "the_thing" (render "{{#snake_case}}TheThing{{/snake_case}}")))
    (is (= "about_the_thing"
          (render
            "{{#snake_case}}"
            "{{#prepend this \"about-\"}}"
            "the-thing"
            "{{/prepend}}"
            "{{/snake_case}}")))))

(deftest camel-case
  (let [registry
        (registry
          "camel_case" tpl-helper-fns/camel-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "thing" (render "{{#camel_case}}thing{{/camel_case}}")))
    (is (= "theThing" (render "{{#camel_case}}the_thing{{/camel_case}}")))
    (is (= "theThing" (render "{{#camel_case}}the-thing{{/camel_case}}")))
    (is (= "theThing" (render "{{#camel_case}}theThing{{/camel_case}}")))
    (is (= "theThing" (render "{{#camel_case}}TheThing{{/camel_case}}")))
    (is (= "aboutTheThing"
          (render
            "{{#camel_case}}"
            "{{#prepend this \"about-\"}}"
            "the-thing"
            "{{/prepend}}"
            "{{/camel_case}}")))))

(deftest upper-camel-case
  (let [registry
        (registry
          "upper_camel_case" tpl-helper-fns/upper-camel-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "Thing"
          (render "{{#upper_camel_case}}thing{{/upper_camel_case}}")))
    (is (= "TheThing"
          (render "{{#upper_camel_case}}the_thing{{/upper_camel_case}}")))
    (is (= "TheThing"
          (render "{{#upper_camel_case}}the-thing{{/upper_camel_case}}")))
    (is (= "TheThing"
          (render "{{#upper_camel_case}}theThing{{/upper_camel_case}}")))
    (is (= "TheThing"
          (render "{{#upper_camel_case}}TheThing{{/upper_camel_case}}")))
    (is (= "AboutTheThing"
          (render
            "{{#upper_camel_case}}"
            "{{#prepend this \"about-\"}}"
            "the-thing"
            "{{/prepend}}"
            "{{/upper_camel_case}}")))))

(deftest kebab-case
  (let [registry
        (registry
          "kebab_case" tpl-helper-fns/kebab-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "thing" (render "{{#kebab_case}}thing{{/kebab_case}}")))
    (is (= "the-thing" (render "{{#kebab_case}}the_thing{{/kebab_case}}")))
    (is (= "the-thing" (render "{{#kebab_case}}the-thing{{/kebab_case}}")))
    (is (= "the-thing" (render "{{#kebab_case}}theThing{{/kebab_case}}")))
    (is (= "the-thing" (render "{{#kebab_case}}TheThing{{/kebab_case}}")))
    (is (= "about-the-thing"
          (render
            "{{#kebab_case}}"
            "{{#prepend this \"about-\"}}"
            "the-thing"
            "{{/prepend}}"
            "{{/kebab_case}}")))))

(deftest upper-case
  (let [registry
        (registry
          "upper_case" tpl-helper-fns/upper-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "THING" (render "{{#upper_case}}thing{{/upper_case}}")))
    (is (= "THE_THING" (render "{{#upper_case}}the_thing{{/upper_case}}")))
    (is (= "THE-THING" (render "{{#upper_case}}the-thing{{/upper_case}}")))
    (is (= "THETHING" (render "{{#upper_case}}theThing{{/upper_case}}")))
    (is (= "THETHING" (render "{{#upper_case}}TheThing{{/upper_case}}")))
    (is (= "ABOUT-THE-THING"
          (render
            "{{#upper_case}}"
            "{{#prepend this \"about-\"}}"
            "the-thing"
            "{{/prepend}}"
            "{{/upper_case}}")))))

(deftest lower-case
  (let [registry
        (registry
          "lower_case" tpl-helper-fns/lower-case
          "prepend" prepend)
        render (renderer registry)]
    (is (= "thing" (render "{{#lower_case}}THING{{/lower_case}}")))
    (is (= "the_thing" (render "{{#lower_case}}THE_THING{{/lower_case}}")))
    (is (= "the-thing" (render "{{#lower_case}}THE-THING{{/lower_case}}")))
    (is (= "thething" (render "{{#lower_case}}theThing{{/lower_case}}")))
    (is (= "thething" (render "{{#lower_case}}TheThing{{/lower_case}}")))
    (is (= "about-the-thing"
          (render
            "{{#lower_case}}"
            "{{#prepend this \"ABOUT-\"}}"
            "THE-THING"
            "{{/prepend}}"
            "{{/lower_case}}")))))

(deftest increment
  (let [registry
        (registry
          "increment" tpl-helper-fns/increment
          "prepend" prepend)
        render (renderer registry)]
    (is (= "1" (render "{{#increment}}0{{/increment}}")))
    (is (= "2" (render "{{#increment}}1{{/increment}}")))
    (is (= "-1" (render "{{#increment}}-2{{/increment}}")))
    (is (= "1" (render "{{increment 0}}")))
    (is (= "1" (render "{{increment this 0}}")))
    (is (= "3"
          (render
            "{{#increment}}"
            "{{#increment}}"
            "1"
            "{{/increment}}"
            "{{/increment}}")))))

(deftest decrement
  (let [registry
        (registry
          "decrement" tpl-helper-fns/decrement
          "prepend" prepend)
        render (renderer registry)]
    (is (= "1" (render "{{#decrement}}2{{/decrement}}")))
    (is (= "0" (render "{{#decrement}}1{{/decrement}}")))
    (is (= "-1" (render "{{#decrement}}0{{/decrement}}")))
    (is (= "1" (render "{{decrement 2}}")))
    (is (= "1" (render "{{decrement this 2}}")))
    (is (= "2"
          (render
            "{{#decrement}}"
            "{{#decrement}}"
            "4"
            "{{/decrement}}"
            "{{/decrement}}")))))

(deftest random-password
  (let [registry
        (registry
          "random_password" tpl-helper-fns/random-password
          "prepend" prepend)
        render (renderer registry)

        password-1 (render "{{random_password 64}}")
        password-2 (render "{{random_password this 64}}")
        password-3 (render "{{random_password 32}}")]
    (is (= (count password-1) 64))
    (is (= (count password-2) 64))
    (is (= (count password-3) 32))

    (doseq [password [password-1 password-2 password-3]]
      (is (some? (re-find #"[a-z]" password)))
      (is (some? (re-find #"[A-Z]" password)))
      (is (some? (re-find #"[0-9]" password)))
      (is (some? (re-find #"[ !\"#$%&'()\*\+,-\./:;<=>\?@\[\\\]\^_`\{\|\}\~]"
                   password))))

    (is (not (= password-1 password-2)))))
