(ns pathological.utils
  (:import
    [java.nio.file FileVisitOption
                   LinkOption
                   OpenOption
                   StandardOpenOption FileVisitResult]
    [java.nio.file.attribute FileAttribute]
    [java.nio.charset StandardCharsets]))

(def ^{:dynamic true} *charsets*
  {:us-ascii   StandardCharsets/US_ASCII
   :iso-8859-1 StandardCharsets/ISO_8859_1
   :utf-8      StandardCharsets/UTF_8
   :utf-16be   StandardCharsets/UTF_16BE
   :utf-16le   StandardCharsets/UTF_16LE
   :utf-16     StandardCharsets/UTF_16})

(def ^{:dynamic true} *open-options*
  {:read              StandardOpenOption/READ
   :write             StandardOpenOption/WRITE
   :append            StandardOpenOption/APPEND
   :truncate-existing StandardOpenOption/TRUNCATE_EXISTING
   :create            StandardOpenOption/CREATE
   :create-new        StandardOpenOption/CREATE_NEW
   :delete-on-close   StandardOpenOption/DELETE_ON_CLOSE
   :sparse            StandardOpenOption/SPARSE
   :sync              StandardOpenOption/SYNC
   :dsync             StandardOpenOption/DSYNC
   :no-follow-links   LinkOption/NOFOLLOW_LINKS})

(def ^{:dynamic true} *link-options*
  {:no-follow-links LinkOption/NOFOLLOW_LINKS})

(def ^{:dynamic true} *file-visit-options*
  {:follow-links FileVisitOption/FOLLOW_LINKS})

(def ^{:dynamic true} *file-visit-results*
  {:continue      FileVisitResult/CONTINUE
   :terminate     FileVisitResult/TERMINATE
   :skip-subtree  FileVisitResult/SKIP_SUBTREE
   :skip-siblings FileVisitResult/SKIP_SIBLINGS})

(defn ->option [var]
  (fn [value] (or (get var value) value)))

(defn ->charset [value]
  (or (get *charsets* value) value))

(def ->open-option (->option *open-options*))
(def ->link-option (->option *link-options*))
(def ->file-visit-option (->option *file-visit-options*))

(defmacro ->varargs-array [type args]
  `(into-array ~type (or ~args [])))

(defmacro ->file-attributes-array [args]
  `(->varargs-array FileAttribute ~args))

(defmacro ->open-options-array [args]
  `(->varargs-array OpenOption (map ->open-option ~args)))

(defmacro ->link-options-array [args]
  `(->varargs-array LinkOption (map ->link-option ~args)))

(defmacro ->file-visit-options-array [args]
  `(->varargs-array FileVisitOption (map ->file-visit-option ~args)))

(defn ->file-visit-options-set [options]
  (into #{} (map ->file-visit-option options)))

(defn ->file-visit-result [control]
  (or (get *file-visit-results* control)
    (throw (AssertionError. (str "Invalid control: " control)))))
