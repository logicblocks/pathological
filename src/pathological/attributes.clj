(ns pathological.attributes
  (:refer-clojure :exclude [name])
  (:require
    [pathological.principals :as pr]
    [pathological.utils :as u]
    [pathological.attribute-specs :as as])
  (:import
    [java.nio.file Path]
    [java.nio ByteBuffer]
    [java.nio.file.attribute BasicFileAttributes
     DosFileAttributes
     FileOwnerAttributeView
     PosixFileAttributes]))

(declare
  ->basic-file-attribute-view
  ->owner-file-attribute-view
  ->posix-file-attribute-view
  ->dos-file-attribute-view
  ->user-defined-file-attribute-view
  ->acl-file-attribute-view)

(defn register-conversion [direction selector-spec converter]
  (let [selector-view (first selector-spec)
        selector-name (second selector-spec)

        conversions-var
        (cond
          (= direction :to) #'u/*->attribute-value-conversions*
          (= direction :from) #'u/*<-attribute-value-conversions*
          :else (throw
                  (IllegalArgumentException.
                    (format
                      "Unknown direction: %s. Must be one of [:to :from]."
                      direction))))]
    (alter-var-root
      conversions-var
      #(assoc-in % [selector-view selector-name] converter))))

(defprotocol ReloadFileAttributes
  (reload [view]))

(defprotocol UpdateFileTimes
  (set-times [view last-modified-time last-access-time creation-time])
  (set-last-modified-time [view last-modified-time])
  (set-last-access-time [view last-access-time])
  (set-creation-time [view creation-time]))

(defprotocol UpdateFileOwner
  (set-owner [view owner]))

(defprotocol UpdateFileGroup
  (set-group [view group]))

(defprotocol UpdatePosixFilePermissions
  (set-permissions [_ permissions]))

(defprotocol UpdateFileAcl
  (set-acl [view acl-entries]))

(defprotocol UpdateDosFileAttributes
  (set-read-only [view value])
  (set-hidden [view value])
  (set-system [view value])
  (set-archive [view value]))

(defprotocol UpdateUserDefinedFileAttributes
  (write-attribute [view name value])
  (delete-attribute [view name]))

(defrecord BasicFileAttributeView
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
  (reload [view]
    (->basic-file-attribute-view path (:delegate view)))

  UpdateFileTimes
  (set-times
    [view new-last-modified-time new-last-access-time new-creation-time]
    (.setTimes
      ^java.nio.file.attribute.BasicFileAttributeView (:delegate view)
      (u/->file-time new-last-modified-time)
      (u/->file-time new-last-access-time)
      (u/->file-time new-creation-time))
    (reload view))
  (set-last-modified-time [view new-last-modified-time]
    (set-times view new-last-modified-time nil nil))
  (set-last-access-time [view new-last-access-time]
    (set-times view nil new-last-access-time nil))
  (set-creation-time [view new-creation-time]
    (set-times view nil nil new-creation-time)))

(defrecord OwnerFileAttributeView
           [path
            owner
            delegate]

  ReloadFileAttributes
  (reload [view]
    (->owner-file-attribute-view path (:delegate view)))

  UpdateFileOwner
  (set-owner [view new-owner]
    (.setOwner
      ^FileOwnerAttributeView (:delegate view)
      new-owner)
    (reload view)))

(defrecord PosixFileAttributeView
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
  (reload [view]
    (->posix-file-attribute-view path (:delegate view)))

  UpdateFileTimes
  (set-times
    [view new-last-modified-time new-last-access-time new-creation-time]
    (.setTimes
      ^java.nio.file.attribute.PosixFileAttributeView (:delegate view)
      (u/->file-time new-last-modified-time)
      (u/->file-time new-last-access-time)
      (u/->file-time new-creation-time))
    (reload view))
  (set-last-modified-time [view new-last-modified-time]
    (set-times view new-last-modified-time nil nil))
  (set-last-access-time [view new-last-access-time]
    (set-times view nil new-last-access-time nil))
  (set-creation-time [view new-creation-time]
    (set-times view nil nil new-creation-time))

  UpdateFileOwner
  (set-owner [view new-owner]
    (.setOwner
      ^java.nio.file.attribute.PosixFileAttributeView (:delegate view)
      new-owner)
    (reload view))

  UpdateFileGroup
  (set-group [view new-group]
    (.setGroup
      ^java.nio.file.attribute.PosixFileAttributeView (:delegate view)
      new-group)
    (reload view))

  UpdatePosixFilePermissions
  (set-permissions [view new-permissions]
    (.setPermissions
      ^java.nio.file.attribute.PosixFileAttributeView (:delegate view)
      (u/->posix-file-permissions new-permissions))
    (reload view)))

(defrecord DosFileAttributeView
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
  (reload [view]
    (->dos-file-attribute-view path (:delegate view)))

  UpdateFileTimes
  (set-times
    [view new-last-modified-time new-last-access-time new-creation-time]
    (.setTimes
      ^java.nio.file.attribute.DosFileAttributeView (:delegate view)
      (u/->file-time new-last-modified-time)
      (u/->file-time new-last-access-time)
      (u/->file-time new-creation-time))
    (reload view))
  (set-last-modified-time [view new-last-modified-time]
    (set-times view new-last-modified-time nil nil))
  (set-last-access-time [view new-last-access-time]
    (set-times view nil new-last-access-time nil))
  (set-creation-time [view new-creation-time]
    (set-times view nil nil new-creation-time))

  UpdateDosFileAttributes
  (set-read-only [view new-value]
    (.setReadOnly
      ^java.nio.file.attribute.DosFileAttributeView (:delegate view)
      new-value)
    (reload view))
  (set-hidden [view new-value]
    (.setHidden
      ^java.nio.file.attribute.DosFileAttributeView (:delegate view)
      new-value)
    (reload view))
  (set-system [view new-value]
    (.setSystem
      ^java.nio.file.attribute.DosFileAttributeView (:delegate view)
      new-value)
    (reload view))
  (set-archive [view new-value]
    (.setArchive
      ^java.nio.file.attribute.DosFileAttributeView (:delegate view)
      new-value)
    (reload view)))

(defrecord UserDefinedFileAttributeView
           [path
            attributes
            delegate]

  ReloadFileAttributes
  (reload [view]
    (->user-defined-file-attribute-view path (:delegate view)))

  UpdateUserDefinedFileAttributes
  (write-attribute [view name value]
    (.write
      ^java.nio.file.attribute.UserDefinedFileAttributeView (:delegate view)
      (clojure.core/name name)
      (u/->byte-buffer value))
    (reload view))
  (delete-attribute [view name]
    (.delete
      ^java.nio.file.attribute.UserDefinedFileAttributeView (:delegate view)
      (clojure.core/name name))
    (reload view)))

(defrecord AclFileAttributeView
           [path
            owner
            acl
            delegate]

  ReloadFileAttributes
  (reload [view]
    (->acl-file-attribute-view path (:delegate view)))

  UpdateFileOwner
  (set-owner [view new-owner]
    (.setOwner
      ^java.nio.file.attribute.AclFileAttributeView (:delegate view)
      new-owner)
    (reload view))

  UpdateFileAcl
  (set-acl [view new-acl-entries]
    (.setAcl
      ^java.nio.file.attribute.AclFileAttributeView (:delegate view)
      (map u/->acl-entry new-acl-entries))
    (reload view)))

(defn ->basic-file-attribute-view
  [^Path path ^java.nio.file.attribute.BasicFileAttributeView view]
  (when view
    (let [^BasicFileAttributes attributes (.readAttributes view)]
      (map->BasicFileAttributeView
        {:path               path
         :file-key           (.fileKey attributes)
         :size               (.size attributes)
         :last-modified-time (u/<-file-time (.lastModifiedTime attributes))
         :last-access-time   (u/<-file-time (.lastAccessTime attributes))
         :creation-time      (u/<-file-time (.creationTime attributes))
         :regular-file?      (.isRegularFile attributes)
         :directory?         (.isDirectory attributes)
         :symbolic-link?     (.isSymbolicLink attributes)
         :other?             (.isOther attributes)
         :delegate           view}))))

(defn ->owner-file-attribute-view
  [^Path path ^FileOwnerAttributeView view]
  (when view
    (let [owner (pr/<-user-principal (.getOwner view))]
      (map->OwnerFileAttributeView
        {:path     path
         :owner    owner
         :delegate view}))))

(defn ->posix-file-attribute-view
  [^Path path ^java.nio.file.attribute.PosixFileAttributeView view]
  (when view
    (let [^PosixFileAttributes posix-attributes (.readAttributes view)]
      (map->PosixFileAttributeView
        {:path               path
         :file-key           (.fileKey posix-attributes)
         :size               (.size posix-attributes)
         :owner              (pr/<-user-principal (.owner posix-attributes))
         :group              (pr/<-group-principal (.group posix-attributes))
         :permissions        (u/<-posix-file-permissions
                               (.permissions posix-attributes))
         :last-modified-time (u/<-file-time
                               (.lastModifiedTime posix-attributes))
         :last-access-time   (u/<-file-time (.lastAccessTime posix-attributes))
         :creation-time      (u/<-file-time (.creationTime posix-attributes))
         :regular-file?      (.isRegularFile posix-attributes)
         :directory?         (.isDirectory posix-attributes)
         :symbolic-link?     (.isSymbolicLink posix-attributes)
         :other?             (.isOther posix-attributes)
         :delegate           view}))))

(defn ->dos-file-attribute-view
  [^Path path ^java.nio.file.attribute.DosFileAttributeView view]
  (when view
    (let [^DosFileAttributes dos-attributes (.readAttributes view)]
      (map->DosFileAttributeView
        {:path               path
         :file-key           (.fileKey dos-attributes)
         :size               (.size dos-attributes)
         :last-modified-time (u/<-file-time (.lastModifiedTime dos-attributes))
         :last-access-time   (u/<-file-time (.lastAccessTime dos-attributes))
         :creation-time      (u/<-file-time (.creationTime dos-attributes))
         :regular-file?      (.isRegularFile dos-attributes)
         :directory?         (.isDirectory dos-attributes)
         :symbolic-link?     (.isSymbolicLink dos-attributes)
         :other?             (.isOther dos-attributes)
         :read-only?         (.isReadOnly dos-attributes)
         :hidden?            (.isHidden dos-attributes)
         :archive?           (.isArchive dos-attributes)
         :system?            (.isSystem dos-attributes)
         :delegate           view}))))

(defn ->user-defined-file-attribute-view
  [^Path path ^java.nio.file.attribute.UserDefinedFileAttributeView view]
  (when view
    (let [names (.list view)
          attributes
          (into {}
            (mapv
              (fn [name]
                (let [byte-buffer (ByteBuffer/allocate (.size view name))]
                  (.read view name byte-buffer)
                  [(keyword name) (.array byte-buffer)]))
              names))]
      (map->UserDefinedFileAttributeView
        {:path       path
         :attributes attributes
         :delegate   view}))))

(defn ->acl-file-attribute-view
  [^Path path ^java.nio.file.attribute.AclFileAttributeView view]
  (when view
    (let [owner (pr/<-user-principal (.getOwner view))
          acl (mapv u/<-acl-entry (.getAcl view))]
      (map->AclFileAttributeView
        {:path     path
         :owner    owner
         :acl      acl
         :delegate view}))))

(def ^:dynamic *file-attribute-view-factories*
  {:basic ->basic-file-attribute-view
   :owner ->owner-file-attribute-view
   :posix ->posix-file-attribute-view
   :dos   ->dos-file-attribute-view
   :user  ->user-defined-file-attribute-view
   :acl   ->acl-file-attribute-view})

(defn ->file-attribute-view-factory [type]
  (get *file-attribute-view-factories* type
    (fn [_ view] view)))
