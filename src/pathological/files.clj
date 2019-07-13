(ns pathological.files
  (:refer-clojure :exclude [find])
  (:require
    [pathological.paths :as p]
    [pathological.utils
     :refer [->charset
             ->file-attributes-array
             ->open-options-array
             ->link-options-array
             ->file-visit-options-array
             ->file-visit-options-set
             ->file-visit-result]])
  (:import
    [java.nio.file Files
                   FileVisitor
                   Path]
    [java.nio.charset Charset
                      StandardCharsets]
    [java.nio.file.attribute PosixFilePermissions]
    [java.util.function BiPredicate]))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path (->charset charset))))

(defn write-lines
  ([^Path path ^Iterable lines]
   (write-lines path lines :utf-8))
  ([^Path path ^Iterable lines charset & options]
   (let [^Charset charset (->charset charset)
         ^"[Ljava.nio.file.OpenOption;"
         open-options (->open-options-array options)]
     (Files/write path lines charset open-options))))

(defn create-directories
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (->file-attributes-array options)]
    (Files/createDirectories path file-attributes)))

(defn create-directory
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (->file-attributes-array options)]
    (Files/createDirectory path file-attributes)))

(defn create-file
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (->file-attributes-array options)]
    (Files/createFile path file-attributes)))

(defn create-symbolic-link
  [^Path link ^Path target & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (->file-attributes-array options)]
    (Files/createSymbolicLink link target file-attributes)))

(defn create-link
  [^Path link ^Path target]
  (Files/createLink link target))

(defn delete
  [^Path path]
  (Files/delete path))

(defn read-symbolic-link
  [^Path path]
  (Files/readSymbolicLink path))

(deftype FnBackedBiPredicate
  [predicate-fn]

  BiPredicate
  (test [_ path basic-file-attributes]
    (predicate-fn path basic-file-attributes)))

(defn find
  [^Path path matcher
   & {:keys [file-visit-options
             maximum-depth]
      :or   {file-visit-options []
             maximum-depth      Integer/MAX_VALUE}}]
  (let [file-visit-options (->file-visit-options-array file-visit-options)
        matcher (->FnBackedBiPredicate matcher)]
    (iterator-seq
      (.iterator
        (Files/find path maximum-depth matcher file-visit-options)))))

(defn exists?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (Files/exists path link-options)))

(defn regular-file?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (Files/isRegularFile path link-options)))

(defn directory?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (Files/isDirectory path link-options)))

(defn symbolic-link?
  [^Path path]
  (Files/isSymbolicLink path))

(defn same-file?
  [^Path path-1 ^Path path-2]
  (Files/isSameFile path-1 path-2))

(defn posix-file-permissions [string]
  (PosixFilePermissions/fromString string))

(defn posix-file-permission-attributes [string]
  (PosixFilePermissions/asFileAttribute
    (posix-file-permissions string)))

(defn- invoke-visitor-and-accumulate [visit-fn accumulator-atom path & args]
  (let [result @accumulator-atom
        {:keys [result control]
         :or   {result  result
                control :continue}}
        (apply visit-fn (concat [result path] args))]
    (swap! accumulator-atom (constantly result))
    (->file-visit-result control)))

(deftype FnBackedFileVisitor
  [pre-visit-directory-fn
   post-visit-directory-fn
   visit-file-fn
   visit-file-failed-fn
   accumulator-atom]

  FileVisitor
  (preVisitDirectory [_ directory basic-file-attributes]
    (invoke-visitor-and-accumulate
      pre-visit-directory-fn accumulator-atom directory basic-file-attributes))

  (postVisitDirectory [_ file exception]
    (invoke-visitor-and-accumulate
      post-visit-directory-fn accumulator-atom file exception))

  (visitFile [_ file basic-file-attributes]
    (invoke-visitor-and-accumulate
      visit-file-fn accumulator-atom file basic-file-attributes))

  (visitFileFailed [_ file exception]
    (invoke-visitor-and-accumulate
      visit-file-fn accumulator-atom file exception)))

(defn walk-file-tree
  [^Path path
   & {:keys [pre-visit-directory-fn
             post-visit-directory-fn
             visit-file-fn
             visit-file-failed-fn
             file-visit-options
             maximum-depth
             initial-value]
      :or   {pre-visit-directory-fn  (fn [_ _ _] {:control :continue})
             post-visit-directory-fn (fn [_ _ _] {:control :continue})
             visit-file-fn           (fn [_ _ _] {:control :continue})
             visit-file-failed-fn    (fn [_ _ exception] (throw exception))
             file-visit-options      []
             maximum-depth           Integer/MAX_VALUE
             initial-value           nil}}]
  (let [accumulator-atom (atom initial-value)
        file-visit-options (->file-visit-options-set file-visit-options)]
    (Files/walkFileTree path file-visit-options maximum-depth
      (->FnBackedFileVisitor
        pre-visit-directory-fn
        post-visit-directory-fn
        visit-file-fn
        visit-file-failed-fn
        accumulator-atom))
    @accumulator-atom))

(defn- parse-definition [[first second :as definition]]
  (cond
    (and (map? first) (:type first)) [first second]
    (vector? first) [{:type :directory} definition]
    (map? first) [(assoc first :type :file) second]))

(defn populate-file-tree
  [^Path path definition]
  (doseq [[name & rest] definition
          :let [[attributes rest] (parse-definition rest)
                path (p/path path (clojure.core/name name))]]
    (condp = (:type attributes)
      :directory
      (do
        (create-directory path)
        (when (seq rest)
          (populate-file-tree path rest)))

      :file
      (do
        (create-file path)
        (when (:content attributes)
          (write-lines path (:content attributes))))

      :symbolic-link
      (do
        (assert (:target attributes)
          (str "Attribute :target missing for path " path))
        (create-symbolic-link path
          (p/path (p/file-system path) (:target attributes))))

      :link
      (do
        (assert (:target attributes)
          (str "Attribute :target missing for path " path))
        (create-link path
          (p/path (p/file-system path) (:target attributes)))))))

(defn delete-recursively
  [^Path path]
  (letfn [(delete-fn [_ path _] (delete path))]
    (if (exists? path)
      (walk-file-tree path
        :visit-file-fn delete-fn
        :post-visit-directory-fn delete-fn))))
