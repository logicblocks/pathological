(ns pathological.utils
  (:require
    [clojure.set :refer [map-invert]]

    [pathological.principals :as principals])
  (:import
    [java.util Set]
    [java.time Instant]
    [java.nio ByteBuffer]
    [java.nio.charset StandardCharsets Charset]
    [java.nio.file CopyOption
                   FileVisitOption
                   FileVisitResult
                   LinkOption
                   OpenOption
                   StandardOpenOption
                   StandardCopyOption]
    [java.nio.file.attribute AclEntry
                             AclEntryFlag
                             AclEntryPermission
                             AclEntryType
                             AclFileAttributeView
                             BasicFileAttributeView
                             DosFileAttributeView
                             FileAttribute
                             FileOwnerAttributeView
                             FileTime
                             PosixFileAttributeView
                             PosixFilePermission
                             PosixFilePermissions
                             UserDefinedFileAttributeView]))

(def ^:dynamic *charsets*
  {:us-ascii   StandardCharsets/US_ASCII
   :iso-8859-1 StandardCharsets/ISO_8859_1
   :utf-8      StandardCharsets/UTF_8
   :utf-16be   StandardCharsets/UTF_16BE
   :utf-16le   StandardCharsets/UTF_16LE
   :utf-16     StandardCharsets/UTF_16})

(def ^:dynamic *open-options*
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

(def ^:dynamic *copy-options*
  {:replace-existing StandardCopyOption/REPLACE_EXISTING
   :copy-attributes  StandardCopyOption/COPY_ATTRIBUTES
   :atomic-move      StandardCopyOption/ATOMIC_MOVE
   :no-follow-links  LinkOption/NOFOLLOW_LINKS})

(def ^:dynamic *link-options*
  {:no-follow-links LinkOption/NOFOLLOW_LINKS})

(def ^:dynamic *file-visit-options*
  {:follow-links FileVisitOption/FOLLOW_LINKS})

(def ^:dynamic *file-visit-results*
  {:continue      FileVisitResult/CONTINUE
   :terminate     FileVisitResult/TERMINATE
   :skip-subtree  FileVisitResult/SKIP_SUBTREE
   :skip-siblings FileVisitResult/SKIP_SIBLINGS})

(def ^:dynamic *file-attribute-view-classes*
  {:basic BasicFileAttributeView
   :owner FileOwnerAttributeView
   :acl   AclFileAttributeView
   :dos   DosFileAttributeView
   :posix PosixFileAttributeView
   :user  UserDefinedFileAttributeView})

(def posix-file-permissions
  {:owner-read     PosixFilePermission/OWNER_READ
   :owner-write    PosixFilePermission/OWNER_WRITE
   :owner-execute  PosixFilePermission/OWNER_EXECUTE
   :group-read     PosixFilePermission/GROUP_READ
   :group-write    PosixFilePermission/GROUP_WRITE
   :group-execute  PosixFilePermission/GROUP_EXECUTE
   :others-read    PosixFilePermission/OTHERS_READ
   :others-write   PosixFilePermission/OTHERS_WRITE
   :others-execute PosixFilePermission/OTHERS_EXECUTE})

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

(defn ->posix-file-permission [value]
  (if-not (instance? PosixFilePermission value)
    (get posix-file-permissions value)
    value))

(defn <-posix-file-permission [value]
  (get (map-invert posix-file-permissions) value))

(defn <-posix-file-permissions-string [string]
  (into #{}
    (map <-posix-file-permission
      (PosixFilePermissions/fromString string))))

(defn ->posix-file-permissions-string [permissions]
  (PosixFilePermissions/toString
    (into #{}
      (map ->posix-file-permission permissions))))

(defn ->posix-file-permissions [string-or-set]
  (let [keyword-set
        (if (string? string-or-set)
          (<-posix-file-permissions-string string-or-set)
          string-or-set)

        posix-file-permission-set
        (into #{} (map ->posix-file-permission keyword-set))]
    posix-file-permission-set))

(defn ->posix-file-permissions-attribute [string-or-set]
    (PosixFilePermissions/asFileAttribute
      (->posix-file-permissions string-or-set)))

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

(defn ->acl-entry [{:keys [type principal permissions flags]
                    :or {permissions #{}
                         flags #{}}}]
  (-> (AclEntry/newBuilder)
    (.setType (->acl-entry-type type))
    (.setPrincipal principal)
    (.setPermissions ^Set (into #{} (map ->acl-entry-permission permissions)))
    (.setFlags ^Set (into #{} (map ->acl-entry-flag flags)))
    (.build)))

(defn <-acl-entry [^AclEntry entry]
  (let [type (<-acl-entry-type (.type entry))
        principal (principals/<-user-principal (.principal entry))
        permissions (into #{} (map <-acl-entry-permission (.permissions entry)))
        flags (into #{} (map <-acl-entry-flag (.flags entry)))]
    {:type type
     :principal principal
     :permissions permissions
     :flags flags}))

(defn ->file-time [value]
  (if-not (instance? FileTime value)
    (FileTime/from (Instant/parse value))
    value))

(defn <-file-time [value]
  (str value))

(defn ->lookup-fn [var]
  (fn [value] (get var value value)))

(defn ->charset [value]
  (get *charsets* value value))

(defn charset? [value]
  (and value
    (or (instance? Charset value)
      (contains? *charsets* value))))

(defn ->bytes
  ([value]
   (.getBytes value))
  ([value charset]
   (.getBytes value (->charset charset))))

(defn ->byte-buffer
  ([value]
   (->byte-buffer value :utf-8))
  ([value charset]
   (cond
     (instance? ByteBuffer value) value
     (bytes? value) (ByteBuffer/wrap value)
     :default
     (ByteBuffer/wrap
       (.getBytes
         (str value)
         ^Charset (->charset charset))))))

(defn <-byte-buffer [value]
  (.array value))

(def ->open-option (->lookup-fn *open-options*))
(def ->copy-option (->lookup-fn *copy-options*))
(def ->link-option (->lookup-fn *link-options*))
(def ->file-visit-option (->lookup-fn *file-visit-options*))

(defmacro ->varargs-array [type args]
  `(into-array ~type (or ~args [])))

(defmacro ->file-attributes-array [args]
  `(->varargs-array FileAttribute ~args))

(defmacro ->open-options-array [args]
  `(->varargs-array OpenOption (map ->open-option ~args)))

(defmacro ->copy-options-array [args]
  `(->varargs-array CopyOption (map ->copy-option ~args)))

(defmacro ->link-options-array [args]
  `(->varargs-array LinkOption (map ->link-option ~args)))

(defmacro ->file-visit-options-array [args]
  `(->varargs-array FileVisitOption (map ->file-visit-option ~args)))

(defn ->file-visit-options-set [options]
  (into #{} (map ->file-visit-option options)))

(defn ->file-visit-result [control]
  (or (get *file-visit-results* control)
    (throw (AssertionError. (str "Invalid control: " control)))))

(defn ->file-attribute-view-class [type]
  (get *file-attribute-view-classes* type type))

(defn stream-seq [stream]
  (iterator-seq (.iterator stream)))
