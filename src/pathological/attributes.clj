(ns pathological.attributes
  (:require
    [pathological.utils
     :refer [<-file-time
             <-posix-file-permission]]
    [pathological.principals
     :refer [<-user-principal
             <-group-principal]])
  (:import [java.nio.file Path]))

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

(defprotocol UpdateDosFileAttributes
  (set-read-only [_ value])
  (set-hidden [_ value])
  (set-archive [_ value])
  (set-system [_ value]))

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

(defn ->basic-file-attributes
  [^Path path ^java.nio.file.attribute.BasicFileAttributeView view]
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
       :delegate           view})))

(defn ->owner-file-attributes
  [^Path path ^java.nio.file.attribute.FileOwnerAttributeView view]
  (let [owner (.getOwner view)]
    (map->OwnerFileAttributes
      {:path     path
       :owner    owner
       :delegate view})))

(defn ->posix-file-attributes
  [^Path path ^java.nio.file.attribute.PosixFileAttributeView view]
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
       :delegate           view})))

(defn ->dos-file-attributes
  [^Path path ^java.nio.file.attribute.DosFileAttributeView view]
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
       :delegate           view})))

(def ^:dynamic *file-attributes-factories*
  {:basic ->basic-file-attributes
   :owner ->owner-file-attributes
   :posix ->posix-file-attributes
   :dos   ->dos-file-attributes})

(defn ->file-attributes-factory [type]
  (get *file-attributes-factories* type
    (fn [_ view] view)))
