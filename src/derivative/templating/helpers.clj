(ns derivative.templating.helpers
  (:require
    [clojure.string :as strings]
    [clojure.edn :as edn]
    [camel-snake-kebab.core :as cases]
    [secure-rand.core :as secure]
    [hbs.helper :as template])
  (:import [com.github.jknack.handlebars Options]))

; strings
(def snake-case
  (template/helper [context options]
    (template/safe-str
      (cases/->snake_case_string
        (template/block-body options context)))))

(def camel-case
  (template/helper [context options]
    (template/safe-str
      (cases/->camelCaseString
        (template/block-body options context)))))

(def upper-camel-case
  (template/helper [context options]
    (template/safe-str
      (cases/->PascalCaseString
        (template/block-body options context)))))

(def kebab-case
  (template/helper [context options]
    (template/safe-str
      (cases/->kebab-case-string
        (template/block-body options context)))))

(def upper-case
  (template/helper [context options]
    (template/safe-str
      (strings/upper-case
        (template/block-body options context)))))

(def lower-case
  (template/helper [context options]
    (template/safe-str
      (strings/lower-case
        (template/block-body options context)))))

; numbers
(def increment
  (template/helper [context options]
    (let [value
          (cond
            (or (number? context) (string? context))
            context

            (not (empty? (template/block-body options context)))
            (template/block-body options context)

            :else
            (template/param options 0))]
      (template/safe-str
        (inc (edn/read-string (str value)))))))

(def decrement
  (template/helper [context options]
    (let [value
          (cond
            (or (number? context) (string? context))
            context

            (not (empty? (template/block-body options context)))
            (template/block-body options context)

            :else
            (template/param options 0))]
      (template/safe-str
        (dec (edn/read-string (str value)))))))

; crypto
(defn- char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def ^:dynamic *password-symbols*
  [\! \" \# \$ \% \&
   \' \( \) \* \+ \,
   \- \. \/ \: \; \<
   \= \> \? \@ \[ \\
   \] \^ \_ \` \{ \|
   \} \~])

(def ^:private lowers (char-range \a \z))
(def ^:private uppers (char-range \A \Z))
(def ^:private numbers (char-range \0 \9))

(defn- char-requirements []
  (let [lower (secure/rand-nth lowers)
        upper (secure/rand-nth uppers)
        number (secure/rand-nth numbers)
        symbol (secure/rand-nth *password-symbols*)]
    [lower upper number symbol]))

(defn- char-extras [quantity]
  (let [chars (concat lowers uppers numbers *password-symbols*)]
    (take quantity (repeatedly #(secure/rand-nth chars)))))

(defn- randomised-string [chars]
  (strings/join (shuffle chars)))

(defn- rand-password [length]
  (let [reqs (char-requirements)
        rest (char-extras (- length (count reqs)))]
    (randomised-string (concat reqs rest))))

(def random-password
  (template/helper [context ^Options options]
    (let [length
          (cond
            (number? context) context
            (string? context) (read-string context)
            :else (.param options 0 64))
          password (rand-password length)]
      (template/safe-str password))))
