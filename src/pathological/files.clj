(ns pathological.files
  (:refer-clojure :exclude [find list])
  (:require
    [pathological.paths :as paths]
    [pathological.file-systems :as file-systems]
    [pathological.principals :as principals]
    [pathological.attributes :as attributes]
    [pathological.utils :as utils])
  (:import
    [java.nio.file DirectoryStream$Filter
                   Files
                   FileVisitor
                   Path]
    [java.nio.charset Charset
                      StandardCharsets]
    [java.nio.file.attribute PosixFilePermissions
                             UserPrincipal]
    [java.util.function BiPredicate]
    [java.io InputStream OutputStream]))

(defn create-directories
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createDirectories path file-attributes)))

(defn create-directory
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createDirectory path file-attributes)))

(defn create-file
  [^Path path & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createFile path file-attributes)))

(defn create-symbolic-link
  [^Path link ^Path target & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createSymbolicLink link target file-attributes)))

(defn create-link
  [^Path link ^Path target]
  (Files/createLink link target))

(defmulti ^:private do-create-temp-file
  (fn [first second & _]
    [(type first) (type second)]))

(defmethod ^:private do-create-temp-file [Path String]
  [^Path path prefix suffix & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createTempFile path prefix suffix file-attributes)))

(defmethod ^:private do-create-temp-file [String String]
  [prefix suffix & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createTempFile prefix suffix file-attributes)))

(defn create-temp-file
  [& args]
  (apply do-create-temp-file args))

(defmulti ^:private do-create-temp-directory
  (fn [first & _] (type first)))

(defmethod ^:private do-create-temp-directory Path
  [^Path path prefix & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createTempDirectory path prefix file-attributes)))

(defmethod ^:private do-create-temp-directory String
  [prefix & options]
  (let [^"[Ljava.nio.file.attribute.FileAttribute;"
        file-attributes (utils/->file-attributes-array options)]
    (Files/createTempDirectory prefix file-attributes)))

(defn create-temp-directory
  [& args]
  (apply do-create-temp-directory args))

(defn read-symbolic-link
  [^Path path]
  (Files/readSymbolicLink path))

(defn write-lines
  ([^Path path ^Iterable lines]
   (write-lines path lines :utf-8))
  ([^Path path ^Iterable lines charset & options]
   (let [^Charset charset (utils/->charset charset)
         ^"[Ljava.nio.file.OpenOption;"
         open-options (utils/->open-options-array options)]
     (Files/write path lines charset open-options))))

(defn read-all-bytes
  [^Path path]
  (Files/readAllBytes path))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path (utils/->charset charset))))

(defn lines-stream
  ([^Path path]
   (Files/lines path))
  ([^Path path charset]
   (Files/lines path (utils/->charset charset))))

(defn lines
  ([^Path path]
   (utils/stream-seq (lines-stream path)))
  ([^Path path charset]
   (utils/stream-seq (lines-stream path charset))))

(deftype FnBackedBiPredicate
  [predicate-fn]

  BiPredicate
  (test [_ path basic-file-attributes]
    (predicate-fn path basic-file-attributes)))

(defn find-stream
  [^Path path matcher
   & {:keys [file-visit-options
             maximum-depth]
      :or   {file-visit-options []
             maximum-depth      Integer/MAX_VALUE}}]
  (let [file-visit-options (utils/->file-visit-options-array file-visit-options)
        matcher (->FnBackedBiPredicate matcher)]
    (Files/find path maximum-depth matcher file-visit-options)))

(defn find
  [^Path path matcher & options]
  (utils/stream-seq (apply find-stream path matcher options)))

(defn list-stream
  [^Path path]
  (Files/list path))

(defn list
  [^Path path]
  (utils/stream-seq (list-stream path)))

(defn delete
  [^Path path]
  (Files/delete path))

(defn delete-if-exists
  [^Path path]
  (Files/deleteIfExists path))

(defmulti ^:private do-copy
  (fn [source destination _] [(type source) (type destination)]))

(defmethod ^:private do-copy [Path Path]
  [^Path source ^Path destination
   ^"[Ljava.nio.file.CopyOption;" copy-options]
  (Files/copy source destination copy-options))

(defmethod ^:private do-copy [InputStream Path]
  [^InputStream source ^Path destination
   ^"[Ljava.nio.file.CopyOption;" copy-options]
  (Files/copy source destination copy-options))

(defmethod ^:private do-copy [Path OutputStream]
  [^Path source ^OutputStream destination _]
  (Files/copy source destination))

(defn copy
  [source destination & options]
  (let [^"[Ljava.nio.file.CopyOption;"
        copy-options (utils/->copy-options-array options)]
    (do-copy source destination copy-options)))

(defn move
  [source destination & options]
  (let [^"[Ljava.nio.file.CopyOption;"
        copy-options (utils/->copy-options-array options)]
    (Files/move source destination copy-options)))

(defn size
  [^Path path]
  (Files/size path))

(defn ->posix-file-permissions
  [string]
  (into #{}
    (map utils/<-posix-file-permission
      (PosixFilePermissions/fromString string))))

(defn ->posix-file-permissions-string [permissions]
  (PosixFilePermissions/toString
    (into #{}
      (map utils/->posix-file-permission permissions))))

(defn ->posix-file-permissions-attribute [string-or-set]
  (let [keyword-set
        (if (string? string-or-set)
          (->posix-file-permissions string-or-set)
          string-or-set)

        posix-file-permission-set
        (into #{} (map utils/->posix-file-permission keyword-set))]
    (PosixFilePermissions/asFileAttribute posix-file-permission-set)))

(defn read-posix-file-permissions [path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
    (into #{}
      (map utils/<-posix-file-permission
        (Files/getPosixFilePermissions path link-options)))))

(defn set-posix-file-permissions [path permissions]
  (let [permission-set
        (into #{} (map utils/->posix-file-permission permissions))]
    (Files/setPosixFilePermissions path permission-set)))

(defn read-owner
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)
        user-principle (Files/getOwner path link-options)]
    (principals/<-user-principal user-principle)))

(defn set-owner
  [^Path path user-principal]
  (Files/setOwner path user-principal))

(defn read-last-modified-time
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
    (.toString (Files/getLastModifiedTime path link-options))))

(defn set-last-modified-time
  [^Path path last-modified-time]
  (Files/setLastModifiedTime path (utils/->file-time last-modified-time)))

(defn read-file-attribute-view
  [^Path path type & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)
        type-class (utils/->file-attribute-view-class type)
        factory (attributes/->file-attributes-factory type)]
    (factory path (Files/getFileAttributeView path type-class link-options))))

(defn probe-content-type
  [^Path path]
  (Files/probeContentType path))

(defn exists?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
    (Files/exists path link-options)))

(defn not-exists?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
    (Files/notExists path link-options)))

(defn regular-file?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
    (Files/isRegularFile path link-options)))

(defn directory?
  [^Path path & options]
  (let [^"[Ljava.nio.file.LinkOption;"
        link-options (utils/->link-options-array options)]
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

(deftype FnBackedDirectoryStreamFilter
  [filter-fn]

  DirectoryStream$Filter
  (accept [_ path]
    (filter-fn path)))

(defn new-directory-stream
  ([^Path path]
   (Files/newDirectoryStream path))
  ([^Path path glob-or-filter]
   (if (instance? String glob-or-filter)
     (Files/newDirectoryStream path ^String glob-or-filter)
     (Files/newDirectoryStream path
       ^DirectoryStream$Filter
       (->FnBackedDirectoryStreamFilter glob-or-filter)))))

(defn new-input-stream
  [^Path path & options]
  (let [^"[Ljava.nio.file.OpenOption;"
        open-options (utils/->open-options-array options)]
    (Files/newInputStream path open-options)))

(defn new-output-stream
  [^Path path & options]
  (let [^"[Ljava.nio.file.OpenOption;"
        open-options (utils/->open-options-array options)]
    (Files/newOutputStream path open-options)))

(defn new-buffered-reader
  ([^Path path] (new-buffered-reader path :utf-8))
  ([^Path path charset]
   (Files/newBufferedReader path (utils/->charset charset))))

(defn new-buffered-writer
  ([^Path path & args]
   (let [[first & rest] args
         charset (utils/->charset (if (utils/charset? first) first :utf-8))
         ^"[Ljava.nio.file.OpenOption;"
         open-options (utils/->open-options-array
                        (if (utils/charset? first) rest args))]
     (Files/newBufferedWriter path charset open-options))))

(defn walk-stream
  [^Path path
   & {:keys [file-visit-options
             maximum-depth]
      :or   {file-visit-options []
             maximum-depth      Integer/MAX_VALUE}}]
  (let [^"[Ljava.nio.file.FileVisitOption;"
        file-visit-options (utils/->file-visit-options-array file-visit-options)]
    (Files/walk path maximum-depth file-visit-options)))

(defn walk
  [^Path path & options]
  (utils/stream-seq (apply walk-stream path options)))

(defn- invoke-visitor-and-accumulate [visit-fn accumulator-atom path & args]
  (let [result @accumulator-atom
        {:keys [result control]
         :or   {result  result
                control :continue}}
        (apply visit-fn (concat [result path] args))]
    (swap! accumulator-atom (constantly result))
    (utils/->file-visit-result control)))

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
        file-visit-options (utils/->file-visit-options-set file-visit-options)]
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
                path (paths/path path (clojure.core/name name))]]
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
          (paths/path (paths/file-system path) (:target attributes))))

      :link
      (do
        (assert (:target attributes)
          (str "Attribute :target missing for path " path))
        (create-link path
          (paths/path (paths/file-system path) (:target attributes)))))))

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
