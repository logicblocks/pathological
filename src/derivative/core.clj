(ns derivative.core
  (:refer-clojure :exclude [derive])
  (:require
    [clojure.string :as s]

    [pathological.paths :as p]
    [pathological.files :as f]))

(def ^:dynamic *line-separator*
  (format "%n"))

(defn join-lines [coll]
  (s/join *line-separator* coll))

(defn determine-inputs [search-path pattern]
  (if (s/starts-with? pattern "path:")
    [(p/path search-path (s/replace pattern "path:" ""))]
    (f/find search-path (fn [path _] (p/matches? path pattern)))))

(defmulti apply-transformation
  (fn [transformation _] (:type transformation)))

(defmethod apply-transformation :find-and-replace
  [{:keys [configuration]}
   {:keys [source-directory target-directory]}]
  (let [{:keys [find replace in]} configuration
        source-directory-path (p/path source-directory)
        target-directory-path (p/path target-directory)
        input-file-paths (determine-inputs source-directory-path in)]
    (doseq [input-file-path input-file-paths]
      (let [output-file-path
            (p/path target-directory-path
              (p/relativize source-directory-path input-file-path))

            initial-content (join-lines (f/read-all-lines input-file-path))
            transformed-content (s/replace initial-content find replace)]
        (f/write-lines output-file-path (s/split-lines transformed-content))))))

(defn derive [pipeline options]
  (let [transformation (first pipeline)]
    (apply-transformation transformation options)))

; validate configuration
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
;     source-directory:
;     target-directory:

; variables
;   should be able to provide variables for use in transformations

; find-and-replace
;   acts on content of matching files
;   looks for find, could be string or regex
;   replaces with replace, can interpolate match groups
;   limit to between points in file?
; copy-files
;   recursively copies files from source directory to target directory
;   takes inclusion, exclusion paths / matchers, one or more
;   able to transform names on the way
; move-files
;   recursively moves files from source directory to target directory
;   takes inclusion, exclusion paths / matchers, one or more
;   able to transform names on the way
; insert-at-point
; delete-at-point
;
; insert-syntax
; delete-syntax