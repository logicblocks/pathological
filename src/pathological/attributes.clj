(ns pathological.attributes
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as string]
    [clojure.set :refer [map-invert]]

    [pathological.principals :as pr]

    [pathological.utils :as u])
  (:import
    [java.util Set]
    [java.nio.file Path]
    [java.nio.file.attribute AclEntry
                             AclEntryFlag
                             AclEntryPermission
                             AclEntryType]
    [java.nio ByteBuffer]))

(def acl-entry-types
  {:allow AclEntryType/ALLOW
   :deny  AclEntryType/DENY
   :audit AclEntryType/AUDIT
   :alarm AclEntryType/ALARM})

(def acl-entry-permissions
  {:read-data              AclEntryPermission/READ_DATA
   :write-data             AclEntryPermission/WRITE_DATA
   :append-data            AclEntryPermission/APPEND_DATA
   :read-named-attributes  AclEntryPermission/READ_NAMED_ATTRS
   :write-named-attributes AclEntryPermission/WRITE_NAMED_ATTRS
   :execute                AclEntryPermission/EXECUTE
   :delete-child           AclEntryPermission/DELETE_CHILD
   :read-attributes        AclEntryPermission/READ_ATTRIBUTES
   :write-attributes       AclEntryPermission/WRITE_ATTRIBUTES
   :delete                 AclEntryPermission/DELETE
   :read-acl               AclEntryPermission/READ_ACL
   :write-acl              AclEntryPermission/WRITE_ACL
   :write-owner            AclEntryPermission/WRITE_OWNER
   :synchronize            AclEntryPermission/SYNCHRONIZE
   :list-directory         AclEntryPermission/LIST_DIRECTORY
   :add-file               AclEntryPermission/ADD_FILE
   :add-subdirectory       AclEntryPermission/ADD_SUBDIRECTORY})

(def acl-entry-flags
  {:file-inherit         AclEntryFlag/FILE_INHERIT
   :directory-inherit    AclEntryFlag/DIRECTORY_INHERIT
   :no-propagate-inherit AclEntryFlag/NO_PROPAGATE_INHERIT
   :inherit-only         AclEntryFlag/INHERIT_ONLY})

(declare
  ->basic-file-attributes
  ->owner-file-attributes
  ->posix-file-attributes
  ->dos-file-attributes
  ->user-defined-file-attributes
  ->acl-file-attributes)

(defn view [attribute-spec]
  (if (string/includes? attribute-spec ":")
    (keyword (first (string/split attribute-spec #":")))
    :basic))

(defn name [attribute-spec]
  (if (string/includes? attribute-spec ":")
    (keyword (u/camel->kebab (second (string/split attribute-spec #":"))))
    (keyword (u/camel->kebab attribute-spec))))

(defn view? [attribute-spec view-or-views]
  (let [actual-view (view attribute-spec)
        test-views (if (seq? view-or-views)
                     (into #{} (map keyword view-or-views))
                     #{(keyword view-or-views)})]
    (test-views actual-view)))

(defn name? [attribute-spec name-or-names]
  (let [actual-name (name attribute-spec)
        test-names (if (seqable? name-or-names)
                     (into #{} (map keyword name-or-names))
                     #{(keyword name-or-names)})]
    (test-names actual-name)))

(defn ->acl-entry-type [value]
  (get acl-entry-types value value))

(defn <-acl-entry-type [value]
  (get (map-invert acl-entry-types) value))

(defn ->acl-entry-permission [value]
  (get acl-entry-permissions value value))

(defn <-acl-entry-permission [value]
  (get (map-invert acl-entry-permissions) value))

(defn ->acl-entry-flag [value]
  (get acl-entry-flags value value))

(defn <-acl-entry-flag [value]
  (get (map-invert acl-entry-flags) value))

(defn ->acl-entry [value]
  (if-not (instance? AclEntry value)
    (let [{:keys [type principal permissions flags]
           :or   {permissions #{}
                  flags       #{}}} value]
      (-> (AclEntry/newBuilder)
        (.setType (->acl-entry-type type))
        (.setPrincipal principal)
        (.setPermissions
          ^Set (into #{} (map ->acl-entry-permission permissions)))
        (.setFlags
          ^Set (into #{} (map ->acl-entry-flag flags)))
        (.build)))
    value))

(defn <-acl-entry [^AclEntry entry]
  (let [type (<-acl-entry-type (.type entry))
        principal (pr/<-user-principal (.principal entry))
        permissions (into #{} (map <-acl-entry-permission (.permissions entry)))
        flags (into #{} (map <-acl-entry-flag (.flags entry)))]
    {:type type
     :principal principal
     :permissions permissions
     :flags flags}))

(defn ->value [attribute-spec value]
  (cond
    (view? attribute-spec :user)
    (u/->byte-buffer value)

    (name? attribute-spec
      #{:creation-time
        :last-access-time
        :last-modified-time})
    (u/->file-time value)

    (and
      (view? attribute-spec :posix)
      (name? attribute-spec :permissions))
    (u/->posix-file-permissions value)

    :default value))

(defn <-value [attribute-spec value]
  (cond
    (name? attribute-spec
      #{:creation-time
        :last-access-time
        :last-modified-time})
    (u/<-file-time value)

    (and
      (view? attribute-spec :posix)
      (name? attribute-spec :permissions))
    (u/<-posix-file-permissions value)

    (and
      (view? attribute-spec :acl)
      (name? attribute-spec :acl))
    (mapv <-acl-entry value)

    (name? attribute-spec :owner)
    (pr/<-user-principal value)

    (name? attribute-spec :group)
    (pr/<-group-principal value)

    :default value))

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
  (reload [view]
    (->basic-file-attributes path (:delegate view)))

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

(defrecord OwnerFileAttributes
  [path
   owner
   delegate]

  ReloadFileAttributes
  (reload [view]
    (->owner-file-attributes path (:delegate view)))

  UpdateFileOwner
  (set-owner [view new-owner]
    (.setOwner
      ^java.nio.file.attribute.FileOwnerAttributeView (:delegate view)
      new-owner)
    (reload view)))

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
  (reload [view]
    (->posix-file-attributes path (:delegate view)))

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
  (reload [view]
    (->dos-file-attributes path (:delegate view)))

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

(defrecord UserDefinedFileAttributes
  [path
   attributes
   delegate]

  ReloadFileAttributes
  (reload [view]
    (->user-defined-file-attributes path (:delegate view)))

  UpdateUserDefinedFileAttributes
  (write-attribute [view name value]
    (.write
      ^java.nio.file.attribute.UserDefinedFileAttributeView (:delegate view)
      name
      (u/->byte-buffer value))
    (reload view))
  (delete-attribute [view name]
    (.delete
      ^java.nio.file.attribute.UserDefinedFileAttributeView (:delegate view)
      name)
    (reload view)))

(defrecord AclFileAttributes
  [path
   owner
   acl
   delegate]

  ReloadFileAttributes
  (reload [view]
    (->acl-file-attributes path (:delegate view)))

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
      (map ->acl-entry new-acl-entries))
    (reload view)))

(defn ->basic-file-attributes
  [^Path path ^java.nio.file.attribute.BasicFileAttributeView view]
  (when view
    (let [^java.nio.file.attribute.BasicFileAttributes
          attributes (.readAttributes view)]
      (map->BasicFileAttributes
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

(defn ->owner-file-attributes
  [^Path path ^java.nio.file.attribute.FileOwnerAttributeView view]
  (when view
    (let [owner (pr/<-user-principal (.getOwner view))]
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
         :owner              (pr/<-user-principal (.owner posix-attributes))
         :group              (pr/<-group-principal (.group posix-attributes))
         :permissions        (u/<-posix-file-permissions
                               (.permissions posix-attributes))
         :last-modified-time (u/<-file-time (.lastModifiedTime posix-attributes))
         :last-access-time   (u/<-file-time (.lastAccessTime posix-attributes))
         :creation-time      (u/<-file-time (.creationTime posix-attributes))
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
    (let [owner (pr/<-user-principal (.getOwner view))
          acl (mapv <-acl-entry (.getAcl view))]
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
