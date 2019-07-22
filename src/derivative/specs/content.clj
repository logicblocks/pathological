(ns derivative.specs.content
  (:require
    [derivative.specs.core :as specs]))

(def string-syntax? #(specs/syntax? % :string))
