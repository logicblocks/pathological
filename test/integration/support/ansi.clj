(ns support.ansi
  (:refer-clojure :exclude [->])
  (:require [clojure.string :as string]))

(declare
  reset
  bold faint normal
  italicised not-italicised
  underlined doubly-underlined not-underlined
  slowly-blinking rapidly-blinking steady
  negative positive
  concealed revealed
  crossed-out not-crossed-out
  default-fg black-fg red-fg green-fg yellow-fg
  blue-fg magenta-fg cyan-fg white-fg
  default-bg black-bg red-bg green-bg yellow-bg
  blue-bg magenta-bg cyan-bg white-bg)

(declare
  reset-escape-sequence
  bold-escape-sequence
  faint-escape-sequence
  normal-escape-sequence
  italicised-escape-sequence
  not-italicised-escape-sequence
  underlined-escape-sequence
  doubly-underlined-escape-sequence
  not-underlined-escape-sequence
  slowly-blinking-escape-sequence
  rapidly-blinking-escape-sequence
  steady-escape-sequence
  negative-escape-sequence
  positive-escape-sequence
  concealed-escape-sequence
  revealed-escape-sequence
  crossed-out-escape-sequence
  not-crossed-out-escape-sequence
  default-fg-escape-sequence
  black-fg-escape-sequence
  red-fg-escape-sequence
  green-fg-escape-sequence
  yellow-fg-escape-sequence
  blue-fg-escape-sequence
  magenta-fg-escape-sequence
  cyan-fg-escape-sequence
  white-fg-escape-sequence
  default-bg-escape-sequence
  black-bg-escape-sequence
  red-bg-escape-sequence
  green-bg-escape-sequence
  yellow-bg-escape-sequence
  blue-bg-escape-sequence
  magenta-bg-escape-sequence
  cyan-bg-escape-sequence
  white-bg-escape-sequence)

(def ^:const csi
  "The Control Sequence Introducer (CSI): `ESC [`."
  "\u001b[")

(def ^:const sgr-suffix
  "The Select Graphic Rendition (SGR) suffix: m."
  "m")

(def ^:const reset-escape-sequence
  "The SGI escape sequence to reset to defaults."
  (str csi 0 sgr-suffix))

(def ^:const aspect-codes
  "Codes for Select Graphic Rendition (SGR) display aspects."
  {:bold              1
   :faint             2
   :normal            22

   :italicised        3
   :not-italicised    23

   :underlined        4
   :doubly-underlined 21
   :not-underlined    24

   :slowly-blinking   5
   :rapidly-blinking  6
   :steady            25

   :negative          7
   :positive          27

   :concealed         8
   :revealed          28

   :crossed-out       9
   :not-crossed-out   29

   :default-fg        39
   :black-fg          30
   :red-fg            31
   :green-fg          32
   :yellow-fg         33
   :blue-fg           34
   :magenta-fg        35
   :cyan-fg           36
   :white-fg          37

   :default-bg        49
   :black-bg          40
   :red-bg            41
   :green-bg          42
   :yellow-bg         43
   :blue-bg           44
   :magenta-bg        45
   :cyan-bg           46
   :white-bg          47})

(defmacro ^:private def-sgr-escape-sequence
  "Defines a constant for an SGR escape sequence."
  [aspect-name aspect-code]
  `(def ~(vary-meta
           (symbol (str (name aspect-name) "-escape-sequence"))
           assoc :const true)
     ~(format "The ANSI escape sequence for the SGR aspect '%s'." aspect-name)
     (str csi ~aspect-code sgr-suffix)))

(defmacro ^:private def-sgr-fn
  "Defines a function that applies the specified SGR aspect to its argument,
  resetting to the default on completion."
  [aspect-name aspect-code]
  (let [arg 'text]
    `(defn ~(symbol aspect-name)
       ~(format
          (str
            "Wraps the provided text with the relevant ANSI escape sequences "
            "to apply the SGR aspect '%s'.")
          aspect-name)
       [~arg]
       (str
         csi ~aspect-code sgr-suffix
         ~arg
         reset-escape-sequence))))

(defmacro ^:private def-all
  "Defines escape sequences and functions for all SGR aspects."
  []
  `(do
     ~@(map (fn [[aspect-name aspect-code]]
              `(do
                 (def-sgr-escape-sequence ~aspect-name ~aspect-code)
                 (def-sgr-fn ~aspect-name ~aspect-code)))
         aspect-codes)))

(def-all)

(defn render
  "Renders the provided text applying all provided SGR aspects."
  [text & aspects]
  (string/join
    (concat
      (map (fn [aspect]
             (var-get
               (ns-resolve
                 'support.ansi
                 (symbol (str (name aspect) "-escape-sequence")))))
        aspects)
      [text reset-escape-sequence])))

(def -> render)
