(ns derivative.templating.helpers
  (:refer-clojure :exclude [hash])
  (:require
    [clojure.string :as strings]
    [clojure.edn :as edn]
    [camel-snake-kebab.core :as cases]
    [secure-rand.core :as secure]
    [hbs.helper :as template])
  (:import [com.github.jknack.handlebars Options]))

(defprotocol HandlebarsExtendedOptions
  (param [this idx default])
  (hash [this key default]))

(extend-protocol HandlebarsExtendedOptions
  Options
  (param [this idx default]
    (.param this idx default))
  (hash [this idx default]
    (.hash this idx default)))

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

(defn- char-requirements [& {:as requirements}]
  (let [lower (when (:lowers? requirements) (secure/rand-nth lowers))
        upper (when (:uppers? requirements) (secure/rand-nth uppers))
        number (when (:numbers? requirements) (secure/rand-nth numbers))
        symbol (when (:symbols? requirements)
                 (secure/rand-nth *password-symbols*))]
    (remove nil? [lower upper number symbol])))

(defn- char-extras [quantity & {:as requirements}]
  (let [chars (remove nil?
                (concat
                  (when (:lowers? requirements) lowers)
                  (when (:uppers? requirements) uppers)
                  (when (:numbers? requirements) numbers)
                  (when (:symbols? requirements) *password-symbols*)))]
    (take quantity (repeatedly #(secure/rand-nth chars)))))

(defn- randomised-string [chars]
  (strings/join (shuffle chars)))

(defn- rand-password [length & rest]
  (let [reqs (apply char-requirements rest)
        rest (apply char-extras (concat [(- length (count reqs))] rest))]
    (randomised-string (concat reqs rest))))

(def random-password
  (template/helper [context ^Options options]
    (let [length
          (cond
            (number? context) context
            (string? context) (read-string context)
            :else (param options 0 64))

          lowers? (hash options "lowers" true)
          uppers? (hash options "uppers" true)
          numbers? (hash options "numbers" true)
          symbols? (hash options "symbols" true)]
      (when-not (or lowers? uppers? numbers? symbols?)
        (throw (IllegalArgumentException.
                 "No characters included in password generation.")))
      (template/safe-str
        (rand-password length
          :lowers? lowers?
          :uppers? uppers?
          :numbers? numbers?
          :symbols? symbols?)))))
