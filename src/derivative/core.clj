(ns derivative.core
  (:refer-clojure :exclude [derive])
  (:require
    [derivative.transformations.core :refer [apply-transformation]]
    [derivative.transformations.find-and-replace]))

(defn derive [pipeline options]
  (let [transformation (first pipeline)]
    (apply-transformation transformation options)))

; validate configuration
; load tree of sources
; assemble plan of changes
; transform tree to targets
; write targets

; scope: file, line, word, character, ast, ...
;   may help to optimise streaming nature

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
;   allow replace to point to a file?
;   replace in replace template?
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

; potentially build an overlay filesystem

; overall we are transforming the file system
; multiple ways we can do this
;   path-transformation:
;     creating files/directories,
;     deleting files/directories,
;     copying files/directories,
;     moving files/directories
;   content-transformation:
;     replacing or updating the contents of files

; stages:
;   determine all paths affected by each transformation:
;     not trivial as some transformations introduce new paths
;     potentially each transformation can return the paths it:
;       creates
;       deletes
;       modifies
;
