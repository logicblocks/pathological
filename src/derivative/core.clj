(ns derivative.core
  (:refer-clojure :exclude [derive])
  (:require
    [clojure.string :as string]

    [cljstache.core :as templates]

    [pathological.paths :as paths]
    [pathological.files :as files])
  (:import [java.util.regex Pattern]))

(def ^:dynamic *line-separator*
  (format "%n"))

(defn join-lines [coll]
  (string/join *line-separator* coll))

(defn determine-inputs [search-path pattern]
  (if (string/starts-with? pattern "path:")
    [(paths/path search-path (string/replace pattern "path:" ""))]
    (files/find search-path (fn [path _] (paths/matches? path pattern)))))

(defn build-match-map [match]
  (into {}
    (map-indexed
      (fn [index item] [(keyword (str "$" index)) item])
      match)))

(defmulti apply-transformation
  (fn [transformation _] (:type transformation)))

(defmethod apply-transformation :find-and-replace
  [{:keys [configuration]}
   {:keys [source-directory target-directory]}]
  (let [{:keys [find replace in]} configuration
        source-directory-path (paths/path source-directory)
        target-directory-path (paths/path target-directory)
        input-file-paths (determine-inputs source-directory-path in)]
    (doseq [input-file-path input-file-paths]
      (let [output-file-path
            (paths/path target-directory-path
              (paths/relativize source-directory-path input-file-path))

            initial-content
            (join-lines (files/read-all-lines input-file-path))

            find-pattern
            (if (string? find)
              (re-pattern (Pattern/quote find))
              find)

            replace-fn
            #(templates/render replace {:matches (build-match-map %)})

            transformed-content
            (string/replace initial-content find-pattern replace-fn)]
        (files/write-lines output-file-path
          (string/split-lines transformed-content))))))

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