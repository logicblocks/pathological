(ns derivative.core
  (:refer-clojure :exclude [derive])
  (:require
    [clojure.string :as s]
    [me.raynes.fs :as fs]))

(defmulti apply-transformation
  (fn [transformation _] (:type transformation)))

(defmethod apply-transformation :find-and-replace
  [{:keys [source target configuration]} work-dir-path]
  (fs/with-cwd work-dir-path
    (let [source-content (slurp source)
          target-content (s/replace source-content
                           (:find configuration)
                           (:replace configuration))]
      (spit target target-content))))

(defn derive [pipeline work-dir-path]
  (let [transformation (first pipeline)]
    (apply-transformation transformation work-dir-path)))

; load tree of sources
; assemble plan of changes
; transform tree to targets
; write targets

; scope: file, line, word, character, ast, ...

; terminology:
;   definition - a type of operation to be performed
;     type: the identifier for this definition
;     scope: the unit to operate on,
;            e.g., directory, file, line, word, character, ast
;     configuration-spec: spec for the permitted configuration object
;   transformation - an instance of a definition, concretely configured
;     configuration:
;   pipeline - a set of transformations to be applied sequentially
;     steps:
;   derivation - an application of a pipeline to a directory of files
;     pipeline:
;     source-dir-path:
;     target-dir-path:
