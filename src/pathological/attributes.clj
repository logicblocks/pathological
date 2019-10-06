(ns pathological.attributes
  (:refer-clojure :exclude [name])
  (:require
    [pathological.principals :as pr]
    [pathological.utils :as u])
  (:import
    [java.nio.file Path]
    [java.nio ByteBuffer]
    [java.nio.file.attribute BasicFileAttributes
     DosFileAttributes
     FileOwnerAttributeView
     PosixFileAttributes]))

(defn register-conversion
  "Registers an attribute conversion to or from the specified attribute.

  Takes 3 arguments:

    - A direction, one of `:to` or `:from`,
    - A selector spec, a tuple of the view and name of the attribute, as
      keywords,
    - A converter function of arity 1, taking the value to convert.

  Either the view or the name can be the wildcard, `:*` matching all
  attribute views or names respectively. Conversions registered for attributes
  that already have conversions defined take precedence.

  In terms of conversion precedence taking wildcards into account, the order of
  precedence is:

    1. Specific conversions,
    2. Wildcard name conversions,
    3. Wildcard view conversions,
    4. All wildcard conversions."
  [direction selector-spec converter]
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

(declare
  ->basic-file-attribute-view
  ->owner-file-attribute-view
  ->posix-file-attribute-view
  ->dos-file-attribute-view
  ->user-defined-file-attribute-view
  ->acl-file-attribute-view)

(defprotocol ReloadFileAttributes
  "A protocol allowing file attribute views to be reloaded from the backing
  file system."
  (reload [view]
    "Reloads all attributes of the view."))

(defprotocol UpdateFileTimes
  "A protocol allowing file times to be updated individually or all at once.

  In each case, the time argument(s) should be strings or
  [FileTime](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/FileTime.html)
  instances. To aid in constructing times, see
  [[pathological.utils/->file-time]].

  For more details, see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/BasicFileAttributeView.html#setTimes%28java.nio.file.attribute.FileTime%2C%20java.nio.file.attribute.FileTime%2C%20java.nio.file.attribute.FileTime%29)."
  (set-times [view last-modified-time last-access-time creation-time]
    "Sets the last modified, last access and creation times on the path to which
    the view corresponds. If any time is `nil`, it should be left unmodified.")
  (set-last-modified-time [view last-modified-time]
    "Sets the last modified time on the path to which the view corresponds.
    If the time is `nil`, it should be left unmodified.")
  (set-last-access-time [view last-access-time]
    "Sets the last access time on the path to which the view corresponds.
    If the time is `nil`, it should be left unmodified.")
  (set-creation-time [view creation-time]
    "Sets the creation time on the path to which the view corresponds.
    If the time is `nil`, it should be left unmodified."))

(defprotocol UpdateFileOwner
  "A protocol allowing file owners to be updated.

  For more details, see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/FileOwnerAttributeView.html#setOwner%28java.nio.file.attribute.UserPrincipal%29)."
  (set-owner [view owner]
    "Sets the owner on the path to which the view corresponds.

    The owner argument must implement
    [UserPrincipal](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/UserPrincipal.html).
    To aid in constructing such an instance, see
    [[pathological.principals/->user-principal]]."))

(defprotocol UpdateFileGroup
  "A protocol allowing file groups to be updated.

  For more details, see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/PosixFileAttributeView.html#setGroup%28java.nio.file.attribute.GroupPrincipal%29)."
  (set-group [view group]
    "Sets the group on the path to which the view corresponds.

    The group argument must implement
    [GroupPrincipal](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/GroupPrincipal.html).
    To aid in constructing such an instance, see
    [[pathological.principals/->group-principal]]."))

(defprotocol UpdatePosixFilePermissions
  "A protocol allowing file permissions to be updated.

  For more details see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/PosixFileAttributeView.html#setPermissions%28java.util.Set%29)."
  (set-permissions [view permissions]
    "Sets the permissions on the path to which the view corresponds.

    The permissions argument should be a permission string or permission set.
    See [[pathological.utils/->posix-file-permissions]] for more details on
    permissible values."))

(defprotocol UpdateFileAcl
  "A protocol allowing the access control list (ACL) of a file to be updated.

  For more details see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/AclFileAttributeView.html#setAcl%28java.util.List%29)."
  (set-acl [view acl-entries]
    "Sets the ACL of the path to which the view corresponds to the set of ACL
    entries.

    Each ACL entry should be an ACL entry map. See
    [[pathological.utils/->acl-entry]] for more details on permissible
    values."))

(defprotocol UpdateDosFileAttributes
  "A protocol allowing DOS file attributes to be updated.

  For more details see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/DosFileAttributeView.html)."
  (set-read-only [view value]
    "Sets the read-only attribute on the path to which the view corresponds to
    the boolean value.")
  (set-hidden [view value]
    "Sets the hidden attribute on the path to which the view corresponds to
    the boolean value.")
  (set-system [view value]
    "Sets the system attribute on the path to which the view corresponds to
    the boolean value.")
  (set-archive [view value]
    "Sets the archive attribute on the path to which the view corresponds to
    the boolean value."))

(defprotocol UpdateUserDefinedFileAttributes
  "A protocol allowing user defined file attributes to be updated.

  For more details see [the Java documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/UserDefinedFileAttributeView.html)."
  (write-attribute [view name value]
    "Writes the user defined attribute with the given name to the value on the
    path to which the view corresponds.

    The attribute value can be a string, an array of bytes or a byte buffer.
    See [[pathological.utils/->byte-buffer]] for more details on permissible
    values.")
  (delete-attribute [view name]
    "Deletes the user defined attribute with the given name from the path to
    which the view corresponds."))

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

(defn ->basic-file-attribute-map
  "Extracts all attributes from the
  [BasicFileAttributes](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/BasicFileAttributes.html)
  instance into a map.

  The returned map includes:

    - `:file-key`: an object that uniquely identifies the file,
    - `:size`: the size of the file in bytes,
    - `:last-modified-time`: the last modified time of the file, as a string,
    - `:last-access-time`: the last access time of the file, as a string,
    - `:creation-time`: the creation time of the file, as a string,
    - `:regular-file?`: whether or not the file is a regular file, as a boolean,
    - `:directory?`: whether or not the file is a directory, as a boolean,
    - `:symbolic-link?`: whether or not the file is a symbolic link, as a
      boolean,
    - `:other?`: whether the file is something other than a regular file,
      directory or symbolic link, as a boolean.

  For more details on each attribute, see [the Java documentation for BasicFileAttributes](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/BasicFileAttributes.html)."
  [^BasicFileAttributes attributes]
  {:file-key           (.fileKey attributes)
   :size               (.size attributes)
   :last-modified-time (u/<-file-time (.lastModifiedTime attributes))
   :last-access-time   (u/<-file-time (.lastAccessTime attributes))
   :creation-time      (u/<-file-time (.creationTime attributes))
   :regular-file?      (.isRegularFile attributes)
   :directory?         (.isDirectory attributes)
   :symbolic-link?     (.isSymbolicLink attributes)
   :other?             (.isOther attributes)})

(defn ->basic-file-attribute-view
  "Converts the [BasicFileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/BasicFileAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:file-key`: an object that uniquely identifies the file,
    - `:size`: the size of the file in bytes,
    - `:last-modified-time`: the last modified time of the file, as a string,
    - `:last-access-time`: the last access time of the file, as a string,
    - `:creation-time`: the creation time of the file, as a string,
    - `:regular-file?`: whether or not the file is a regular file, as a boolean,
    - `:directory?`: whether or not the file is a directory, as a boolean,
    - `:symbolic-link?`: whether or not the file is a symbolic link, as a
      boolean,
    - `:other?`: whether the file is something other than a regular file,
      directory or symbolic link, as a boolean,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileAttributes]] and
  [[UpdateFileTimes]]."
  [^Path path ^java.nio.file.attribute.BasicFileAttributeView view]
  (when view
    (let [^BasicFileAttributes attributes (.readAttributes view)]
      (map->BasicFileAttributeView
        (merge
          (->basic-file-attribute-map attributes)
          {:path               path
           :delegate           view})))))

(defn ->owner-file-attribute-view
  "Converts the [FileOwnerAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/FileOwnerAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:owner`: the user principal that owns the file,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileAttributes]] and
  [[UpdateFileOwner]]."
  [^Path path ^FileOwnerAttributeView view]
  (when view
    (let [owner (pr/<-user-principal (.getOwner view))]
      (map->OwnerFileAttributeView
        {:path     path
         :owner    owner
         :delegate view}))))

(defn ->posix-file-attribute-view
  "Converts the [PosixFileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/PosixFileAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:file-key`: an object that uniquely identifies the file,
    - `:size`: the size of the file in bytes,
    - `:owner`: the user principal acting as owner of the file,
    - `:group`: the group principal acting as group for the file,
    - `:permissions`: the POSIX permissions associated with the file,
    - `:last-modified-time`: the last modified time of the file, as a string,
    - `:last-access-time`: the last access time of the file, as a string,
    - `:creation-time`: the creation time of the file, as a string,
    - `:regular-file?`: whether or not the file is a regular file, as a boolean,
    - `:directory?`: whether or not the file is a directory, as a boolean,
    - `:symbolic-link?`: whether or not the file is a symbolic link, as a
      boolean,
    - `:other?`: whether the file is something other than a regular file,
      directory or symbolic link, as a boolean,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileAttributes]], [[UpdateFileTimes]],
  [[UpdateFileOwner]], [[UpdateFileGroup]] and [[UpdatePosixFilePermissions]]."
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
  "Converts the [DosFileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/DosFileAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:file-key`: an object that uniquely identifies the file,
    - `:size`: the size of the file in bytes,
    - `:last-modified-time`: the last modified time of the file, as a string,
    - `:last-access-time`: the last access time of the file, as a string,
    - `:creation-time`: the creation time of the file, as a string,
    - `:regular-file?`: whether or not the file is a regular file, as a boolean,
    - `:directory?`: whether or not the file is a directory, as a boolean,
    - `:symbolic-link?`: whether or not the file is a symbolic link, as a
      boolean,
    - `:other?`: whether the file is something other than a regular file,
      directory or symbolic link, as a boolean,
    - `:read-only?`: whether or not the file is read only, as a boolean,
    - `:hidden?`: whether or not the file is hidden, as a boolean,
    - `:archive?`: whether or not the file is an archive, as a boolean,
    - `:system?`: whether or not the file is a system file, as a boolean,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileAttributes]], [[UpdateFileTimes]]
  and [[UpdateDosFileAttributes]]."
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
  "Converts the [UserDefinedFileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/UserDefinedFileAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:attributes`: a map of user defined file attributes with keyword keys
      and byte array values,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileAttributes]] and
  [[UpdateUserDefinedFileAttributes]]."
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
  "Converts the [AclFileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/AclFileAttributeView.html)
  instance to a materialised record.

  The returned record includes:

    - `:path`: the path to the file,
    - `:owner`: the user principal acting as owner of the file,
    - `:acl`: the set of ACL entries associated with the file,
    - `:delegate`: the underlying instance.

  The `:acl` attribute in the returned record is a set of ACL entry maps. See
  [[pathological.utils/<-acl-entry]] for more details on ACL entry maps.

  The returned record implements [[ReloadFileAttributes]] and
  [[UpdateFileAcl]]."
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
  "A mapping of available file attribute view types to factory functions for
  materialised records.

  By default, includes factories for `:basic`, `:owner`, `:posix`, `:dos`,
  `:user` and `:acl` file attribute views. Since the file attribute view
  framework is extensible and implementation dependent in Java NIO2, this var is
  dynamic and can be altered / rebound to support additional file attribute
  views."
  {:basic ->basic-file-attribute-view
   :owner ->owner-file-attribute-view
   :posix ->posix-file-attribute-view
   :dos   ->dos-file-attribute-view
   :user  ->user-defined-file-attribute-view
   :acl   ->acl-file-attribute-view})

(defn ->file-attribute-view-factory
  "Returns a file attribute view factory for building materialised records
  for the supplied file attribute view type when available. If no factory is
  available, an identity factory is returned which leaves the provided view
  unchanged.

  See [[*file-attribute-view-factories*]] for details on supported types."
  [type]
  (get *file-attribute-view-factories* type
    (fn [_ view] view)))
