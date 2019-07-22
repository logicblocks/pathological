(ns derivative.specs.content
  (:require
    [derivative.specs.core :as specs]))

(defn string-syntax? [content-spec] (specs/syntax? content-spec :string))
