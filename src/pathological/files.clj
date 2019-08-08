(ns pathological.files
  (:refer-clojure :exclude [find])
  (:require
    [pathological.paths :as p]
    [pathological.utils
     :refer [charset?
             ->charset
             ->copy-options-array
             ->file-attributes-array
             ->open-options-array
             ->link-options-array
             ->file-visit-options-array
             ->file-visit-options-set
             ->file-visit-result
             <-posix-file-permission
             ->posix-file-permission
             ->file-time]]
    [pathological.paths :as paths]
    [pathological.file-systems :as file-systems]
    [pathological.principals :as principals])
  (:import
    [java.nio.file Files
                   FileVisitor
                   Path]
    [java.nio.charset Charset
                      StandardCharsets]
    [java.nio.file.attribute PosixFilePermissions
                             UserPrincipal]
    [java.util.function BiPredicate]
    [java.io InputStream OutputStream]))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path (->charset charset))))

(defn read-all-bytes
  [^Path path]
  (Files/readAllBytes path))

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

(defn delete-if-exists
  [^Path path]
  (Files/deleteIfExists path))

(defmulti ^:private do-copy
  (fn [source destination _] [(type source) (type destination)]))

(defmethod do-copy [Path Path]
  [^Path source ^Path destination
   ^"[Ljava.nio.file.CopyOption;" copy-options]
  (Files/copy source destination copy-options))

(defmethod do-copy [InputStream Path]
  [^InputStream source ^Path destination
   ^"[Ljava.nio.file.CopyOption;" copy-options]
  (Files/copy source destination copy-options))

(defmethod do-copy [Path OutputStream]
  [^Path source ^OutputStream destination _]
  (Files/copy source destination))

(defn copy
  [source destination & options]
  (let [^"[Ljava.nio.file.CopyOption;"
        copy-options (->copy-options-array options)]
    (do-copy source destination copy-options)))

(defn move
  [source destination & options]
  (let [^"[Ljava.nio.file.CopyOption;"
        copy-options (->copy-options-array options)]
    (Files/move source destination copy-options)))

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

(defn not-exists?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (Files/notExists path link-options)))

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

(defn hidden?
  [^Path path]
  (Files/isHidden path))

(defn readable?
  [^Path path]
  (Files/isReadable path))

(defn writable?
  [^Path path]
  (Files/isWritable path))

(defn executable?
  [^Path path]
  (Files/isExecutable path))

(defn ->posix-file-permissions
  [string]
  (into #{}
    (map <-posix-file-permission
      (PosixFilePermissions/fromString string))))

(defn ->posix-file-permissions-string [permissions]
  (PosixFilePermissions/toString
    (into #{}
      (map ->posix-file-permission permissions))))

(defn ->posix-file-permissions-attribute [string-or-set]
  (let [keyword-set
        (if (string? string-or-set)
          (->posix-file-permissions string-or-set)
          string-or-set)

        posix-file-permission-set
        (into #{} (map ->posix-file-permission keyword-set))]
    (PosixFilePermissions/asFileAttribute posix-file-permission-set)))

(defn read-posix-file-permissions [path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (into #{}
      (map <-posix-file-permission
        (Files/getPosixFilePermissions path link-options)))))

(defn set-posix-file-permissions [path permissions]
  (let [permission-set
        (into #{} (map ->posix-file-permission permissions))]
    (Files/setPosixFilePermissions path permission-set)))

(defrecord BasicUserPrincipal [name underlying]
  UserPrincipal
  (getName [_] name))

(defn ->user-principal
  ([name] (->user-principal file-systems/*file-system* name))
  ([file-system name]
   (let [underlying
         (principals/lookup-principal-by-name file-system name)]
     (->BasicUserPrincipal name underlying))))

(defn read-owner
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)
        user-principle (Files/getOwner path link-options)]
    (->BasicUserPrincipal (.getName user-principle) user-principle)))

(defn set-owner
  [^Path path user-principal]
  (Files/setOwner path user-principal))

(defn read-last-modified-time
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (->link-options-array options)]
    (.toString (Files/getLastModifiedTime path link-options))))

(defn set-last-modified-time
  [^Path path last-modified]
  (Files/setLastModifiedTime path (->file-time last-modified)))

(defn new-input-stream
  [^Path path & options]
  (let [^"[Ljava.nio.file.OpenOption;"
        open-options (->open-options-array options)]
    (Files/newInputStream path open-options)))

(defn new-output-stream
  [^Path path & options]
  (let [^"[Ljava.nio.file.OpenOption;"
        open-options (->open-options-array options)]
    (Files/newOutputStream path open-options)))

(defn new-buffered-reader
  ([^Path path] (new-buffered-reader path :utf-8))
  ([^Path path charset]
   (Files/newBufferedReader path (->charset charset))))

(defn new-buffered-writer
  ([^Path path & others]
   (let [[first & rest] others
         charset (->charset (if (charset? first) first :utf-8))
         ^"[Ljava.nio.file.OpenOption;"
         open-options (->open-options-array
                        (if (charset? first) rest others))]
     (Files/newBufferedWriter path charset open-options))))

(defn walk
  [^Path path
   & {:keys [file-visit-options
             maximum-depth]
      :or   {file-visit-options []
             maximum-depth      Integer/MAX_VALUE}}]
  (let [^"[Ljava.nio.file.FileVisitOption;"
        file-visit-options (->file-visit-options-array file-visit-options)]
    (iterator-seq
      (.iterator
        (Files/walk path maximum-depth file-visit-options)))))

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
        (create-directories path)
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

(defn copy-recursively
  [^Path source ^Path destination]
  (letfn [(rebase [path]
            (paths/resolve destination (paths/relativize source path)))
          (create-directory-fn [_ directory _]
            (create-directories (rebase directory)))
          (copy-fn [_ file _]
            (copy file (rebase file)))]
    (if (exists? source)
      (walk-file-tree source
        :pre-visit-directory-fn create-directory-fn
        :visit-file-fn copy-fn))))

(defn move-recursively
  [^Path source ^Path destination]
  (letfn [(rebase [path]
            (paths/resolve destination (paths/relativize source path)))
          (create-directory-fn [_ directory _]
            (create-directories (rebase directory)))
          (delete-directory-fn [_ directory _]
            (delete directory))
          (move-fn [_ file _]
            (move file (rebase file)))]
    (if (exists? source)
      (walk-file-tree source
        :pre-visit-directory-fn create-directory-fn
        :visit-file-fn move-fn
        :post-visit-directory-fn delete-directory-fn))))
