(ns derivative.specs.core
  (:require
    [clojure.string :as string]))

(defn syntax [spec]
  (keyword (first (string/split spec #":"))))

(defn syntax? [spec syntax]
  (string/starts-with? spec (str (name syntax) ":")))

(defn strip-syntax [spec]
  (string/replace spec #"^[-a-z]*?:" ""))

