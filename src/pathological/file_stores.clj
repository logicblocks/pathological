(ns pathological.file-stores
  (:refer-clojure :exclude [type])
  (:require
    [pathological.utils :as u])
  (:import
    [clojure.lang Keyword]))

(declare ->file-store)

(defprotocol FileAttributeViewSupport
  "A protocol allowing the file attribute view support of a file store to be
  determined."
  (supports-file-attribute-view [file-store class-or-type]
    "Returns true if the file store supports the file attribute view
    identified by the class or type; returns false otherwise.

    The class or type argument should be:

      - a [Class](https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html)
        instance corresponding to a class implementing
        [FileAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/FileAttributeView.html),
      - a string representing the view name of the file attribute view, defined
        by the underlying file system implementation.
      - a keyword identifying the file attribute view, supported by
        [[pathological.attributes/*file-attribute-view-factories*]]."))

(defprotocol FileStoreAttributeViewSupport
  "A protocol allowing the file store attribute view support of a file store
  to be determined, allowing attributes to be read in the case that file store
  attribute views are supported.

  By default, Java NIO2 does not include any file store attribute views.
  However, the file store attribute view framework is extensible and
  implementation dependent so this protocol is provided in the case that a file
  system is in use that does provide file store attribute views."
  (read-file-store-attribute-view [file-store class-or-type]
    "Reads a file store attribute view of the given class or type from the
    file store.

    The class or type argument should be:

      - a [Class](https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html)
        instance corresponding to a class implementing
        [FileStoreAttributeView](https://docs.oracle.com/javase/7/docs/api/java/nio/file/attribute/FileStoreAttributeView.html),
      - a string representing the view name of the file store attribute view,
        defined by the underlying file system implementation.
      - a keyword identifying the file store attribute view, supported by
        [[*file-store-attribute-view-factories*]].")
  (read-attribute [file-store attribute-spec]
    "Reads the attribute defined by the given attribute spec from the file
    store.

    The attribute spec should be as defined in
    [[pathological.attribute-specs/->attribute-spec]]."))

(defprotocol ReloadFileStoreAttributes
  "A protocol allowing file store attribute views to be reloaded from the
  backing file system."
  (reload [view]
    "Reloads all attributes of the view."))

(defmulti ^:private do-supports-file-attribute-view
  (fn [_ class-or-type]
    (clojure.core/type class-or-type)))

(defmethod ^:private do-supports-file-attribute-view String
  [^java.nio.file.FileStore file-store ^String class-or-type]
  (.supportsFileAttributeView file-store class-or-type))

(defmethod ^:private do-supports-file-attribute-view Class
  [^java.nio.file.FileStore file-store ^Class class-or-type]
  (.supportsFileAttributeView file-store class-or-type))

(defmethod ^:private do-supports-file-attribute-view Keyword
  [^java.nio.file.FileStore file-store class-or-type]
  (.supportsFileAttributeView file-store (name class-or-type)))

(def ^:dynamic *file-store-attribute-view-factories*
  "A mapping of available file store attribute view types to factory functions
  for materialised records.

  By default, there are no factories defined since Java NIO2 does not include
  any file store attribute views. However, since the file store attribute view
  framework is extensible and implementation dependent in Java NIO2, this var is
  dynamic and can be altered / rebound to support additional file store
  attribute views."
  {})

(defn ->file-store-attribute-view-factory
  "Returns a file store attribute view factory for building materialised records
  for the supplied file store attribute view type when available. If no factory
  is available, an identity factory is returned which leaves the provided view
  unchanged.

  See [[*file-store-attribute-view-factories*]] for details on supported types."
  [type]
  (get *file-store-attribute-view-factories* type
    (fn [_ view] view)))

(defrecord FileStore
           [name
            type
            read-only?
            total-space
            usable-space
            unallocated-space
            block-size
            delegate]

  ReloadFileStoreAttributes
  (reload [_]
    (->file-store delegate))

  FileAttributeViewSupport
  (supports-file-attribute-view [_ class-or-type]
    (let [class-or-type (u/->file-attribute-view-class class-or-type)]
      (do-supports-file-attribute-view delegate class-or-type)))

  FileStoreAttributeViewSupport
  (read-file-store-attribute-view [_ class-or-type]
    (let [type-class (u/->file-store-attribute-view-class class-or-type)
          factory (->file-store-attribute-view-factory type)]
      (factory (.getFileStoreAttributeView
                 ^java.nio.file.FileStore delegate type-class))))

  (read-attribute [_ attribute-spec]
    (let [value (.getAttribute
                  ^java.nio.file.FileStore delegate attribute-spec)]
      (u/<-attribute-value attribute-spec value))))

(defn ->file-store
  "Converts the [FileStore](https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileStore.html)
  instance to a materialised record.

  The returned record includes:

    - `:name`: the name of the file store,
    - `:type`: the type of the file store,
    - `:read-only?`: whether or not the file store is read only, as a boolean,
    - `:total-space`: the total space in the file store, in bytes,
    - `:usable-space`: the usable space for this JVM in the file store, in
      bytes,
    - `:unallocated-space`: the unallocated space in the file store, in bytes,
    - `:block-size`: the block size of the file store, when available, in bytes;
      `nil` when not available,
    - `:delegate`: the underlying instance.

  The returned record implements [[ReloadFileStoreAttributes]],
  [[FileAttributeViewSupport]] and [[FileStoreAttributeViewSupport]]."
  [^java.nio.file.FileStore file-store]
  (let [block-size
        (try (.getBlockSize file-store)
             (catch UnsupportedOperationException _ nil))]
    (map->FileStore
      {:name              (.name file-store)
       :type              (.type file-store)
       :read-only?        (.isReadOnly file-store)
       :total-space       (.getTotalSpace file-store)
       :usable-space      (.getUsableSpace file-store)
       :unallocated-space (.getUnallocatedSpace file-store)
       :block-size        block-size
       :delegate          file-store})))
