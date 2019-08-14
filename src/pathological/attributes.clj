(ns pathological.attributes
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as string]

    [pathological.utils
     :refer [->file-time
             <-file-time
             <-posix-file-permission
             ->posix-file-permissions
             <-posix-file-permissions
             <-acl-entry
             ->byte-buffer]]
    [pathological.principals
     :refer [<-user-principal
             <-group-principal]])
  (:import [java.nio.file Path]
           [java.nio ByteBuffer]))

(defn- camel->kebab [value]
  (string/lower-case
    (string/replace value
      #"([a-z0-9])([A-Z])"
      "$1-$2")))

(defn view [attribute]
  (if (string/includes? attribute ":")
    (keyword (first (string/split attribute #":")))
    :basic))

(defn name [attribute]
  (if (string/includes? attribute ":")
    (keyword (camel->kebab (second (string/split attribute #":"))))
    (keyword (camel->kebab attribute))))

(defn view? [attribute v]
  (let [actual-view (view attribute)
        test-views (if (seq? v) (into #{} (map keyword v)) #{(keyword v)})]
    (test-views actual-view)))

(defn name? [attribute n]
  (let [actual-name (name attribute)
        test-names (if (seqable? n) (into #{} (map keyword n)) #{(keyword n)})]
    (test-names actual-name)))

(defn ->value [attribute value]
  (cond
    (view? attribute :user)
    (->byte-buffer value)

    (and
      (view? attribute :basic)
      (name? attribute
        #{:creation-time
          :last-access-time
          :last-modified-time}))
    (->file-time value)

    (and
      (view? attribute :posix)
      (name? attribute :permissions))
    (->posix-file-permissions value)

    :default value))

(defn <-value [attribute value]
  (cond
    (and
      (view? attribute :basic)
      (name? attribute
        #{:creation-time
          :last-access-time
          :last-modified-time}))
    (<-file-time value)

    (and
      (view? attribute :posix)
      (name? attribute :permissions))
    (<-posix-file-permissions value)

    (and
      (view? attribute :acl)
      (name? attribute :acl))
    (into [] (map <-acl-entry value))

    (name? attribute :owner)
    (<-user-principal value)

    (name? attribute :group)
    (<-group-principal value)

    :default value))

(defprotocol ReloadFileAttributes
  (reload [_]))

(defprotocol UpdateFileTimes
  (set-times [_ last-modified-time last-access-time create-time])
  (set-last-modified-time [_ last-modified-time])
  (set-last-access-time [_ last-access-time])
  (set-creation-time [_ creation-time]))

(defprotocol UpdateFileOwner
  (set-owner [_ owner]))

(defprotocol UpdateFileGroup
  (set-group [_ group]))

(defprotocol UpdatePosixFilePermissions
  (set-permissions [_ permissions]))

(defprotocol UpdateFileAcl
  (set-acl [_ acl-entries]))

(defprotocol UpdateDosFileAttributes
  (set-read-only [_ value])
  (set-hidden [_ value])
  (set-archive [_ value])
  (set-system [_ value]))

(defprotocol UpdateUserDefinedFileAttributes
  (write-attribute [_ name value])
  (delete-attribute [_ name]))

(defrecord BasicFileAttributes
  [path
   file-key
   size
   last-modified-time
   last-access-time
   creation-time
   regular-file?
   directory?
   symbolic-link?
   other?
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateFileTimes
  (set-times [_ last-modified-time last-access-time creation-time])
  (set-last-modified-time [_ last-modified-time])
  (set-last-access-time [_ last-access-time])
  (set-creation-time [_ creation-time]))

(defrecord OwnerFileAttributes
  [path
   owner
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateFileOwner
  (set-owner [_ owner]))

(defrecord PosixFileAttributes
  [path
   file-key
   size
   owner
   group
   permissions
   last-modified-time
   last-access-time
   creation-time
   regular-file?
   directory?
   symbolic-link?
   other?
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateFileTimes
  (set-times [_ last-modified-time last-access-time creation-time])
  (set-last-modified-time [_ last-modified-time])
  (set-last-access-time [_ last-access-time])
  (set-creation-time [_ creation-time])

  UpdateFileOwner
  (set-owner [_ owner])

  UpdateFileGroup
  (set-group [_ group])

  UpdatePosixFilePermissions
  (set-permissions [_ permissions]))

(defrecord DosFileAttributes
  [path
   file-key
   size
   last-modified-time
   last-access-time
   creation-time
   regular-file?
   directory?
   symbolic-link?
   other?
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateFileTimes
  (set-times [_ last-modified-time last-access-time creation-time])
  (set-last-modified-time [_ last-modified-time])
  (set-last-access-time [_ last-access-time])
  (set-creation-time [_ creation-time])

  UpdateDosFileAttributes
  (set-read-only [_ value])
  (set-hidden [_ value])
  (set-archive [_ value])
  (set-system [_ value]))

(defrecord UserDefinedFileAttributes
  [path
   attributes
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateUserDefinedFileAttributes
  (write-attribute [_ name value])
  (delete-attribute [_ name]))

(defrecord AclFileAttributes
  [path
   owner
   acl
   delegate]

  ReloadFileAttributes
  (reload [_])

  UpdateFileOwner
  (set-owner [_ owner])

  UpdateFileAcl
  (set-acl [_ acl-entries]))

(defn ->basic-file-attributes
  [^Path path ^java.nio.file.attribute.BasicFileAttributeView view]
  (when view
    (let [^java.nio.file.attribute.BasicFileAttributes
          attributes (.readAttributes view)]
      (map->BasicFileAttributes
        {:path               path
         :file-key           (.fileKey attributes)
         :size               (.size attributes)
         :last-modified-time (<-file-time (.lastModifiedTime attributes))
         :last-access-time   (<-file-time (.lastAccessTime attributes))
         :creation-time      (<-file-time (.creationTime attributes))
         :regular-file?      (.isRegularFile attributes)
         :directory?         (.isDirectory attributes)
         :symbolic-link?     (.isSymbolicLink attributes)
         :other?             (.isOther attributes)
         :delegate           view}))))

(defn ->owner-file-attributes
  [^Path path ^java.nio.file.attribute.FileOwnerAttributeView view]
  (when view
    (let [owner (<-user-principal (.getOwner view))]
      (map->OwnerFileAttributes
        {:path     path
         :owner    owner
         :delegate view}))))

(defn ->posix-file-attributes
  [^Path path ^java.nio.file.attribute.PosixFileAttributeView view]
  (when view
    (let [^java.nio.file.attribute.PosixFileAttributes
          posix-attributes (.readAttributes view)]
      (map->PosixFileAttributes
        {:path               path
         :file-key           (.fileKey posix-attributes)
         :size               (.size posix-attributes)
         :owner              (<-user-principal (.owner posix-attributes))
         :group              (<-group-principal (.group posix-attributes))
         :permissions        (into #{}
                               (map <-posix-file-permission
                                 (.permissions posix-attributes)))
         :last-modified-time (<-file-time (.lastModifiedTime posix-attributes))
         :last-access-time   (<-file-time (.lastAccessTime posix-attributes))
         :creation-time      (<-file-time (.creationTime posix-attributes))
         :regular-file?      (.isRegularFile posix-attributes)
         :directory?         (.isDirectory posix-attributes)
         :symbolic-link?     (.isSymbolicLink posix-attributes)
         :other?             (.isOther posix-attributes)
         :delegate           view}))))

(defn ->dos-file-attributes
  [^Path path ^java.nio.file.attribute.DosFileAttributeView view]
  (when view
    (let [^java.nio.file.attribute.DosFileAttributes
          dos-attributes (.readAttributes view)]
      (map->DosFileAttributes
        {:path               path
         :file-key           (.fileKey dos-attributes)
         :size               (.size dos-attributes)
         :last-modified-time (<-file-time (.lastModifiedTime dos-attributes))
         :last-access-time   (<-file-time (.lastAccessTime dos-attributes))
         :creation-time      (<-file-time (.creationTime dos-attributes))
         :regular-file?      (.isRegularFile dos-attributes)
         :directory?         (.isDirectory dos-attributes)
         :symbolic-link?     (.isSymbolicLink dos-attributes)
         :other?             (.isOther dos-attributes)
         :read-only?         (.isReadOnly dos-attributes)
         :hidden?            (.isHidden dos-attributes)
         :archive?           (.isArchive dos-attributes)
         :system?            (.isSystem dos-attributes)
         :delegate           view}))))

(defn ->user-defined-file-attributes
  [^Path path ^java.nio.file.attribute.UserDefinedFileAttributeView view]
  (when view
    (let [names (.list view)
          attributes
          (into {}
            (mapv
              (fn [name]
                (let [byte-buffer (ByteBuffer/allocate (.size view name))]
                  (.read view name byte-buffer)
                  [name (.array byte-buffer)]))
              names))]
      (map->UserDefinedFileAttributes
        {:path       path
         :attributes attributes
         :delegate   view}))))

(defn ->acl-file-attributes
  [^Path path ^java.nio.file.attribute.AclFileAttributeView view]
  (when view
    (let [owner (<-user-principal (.getOwner view))
          acl (mapv (fn [entry] (<-acl-entry entry)) (.getAcl view))]
      (map->AclFileAttributes
        {:path     path
         :owner    owner
         :acl      acl
         :delegate view}))))

(def ^:dynamic *file-attributes-factories*
  {:basic ->basic-file-attributes
   :owner ->owner-file-attributes
   :posix ->posix-file-attributes
   :dos   ->dos-file-attributes
   :user  ->user-defined-file-attributes
   :acl   ->acl-file-attributes})

(defn ->file-attributes-factory [type]
  (get *file-attributes-factories* type
    (fn [_ view] view)))
