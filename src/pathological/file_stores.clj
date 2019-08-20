(ns pathological.file-stores
  (:require
    [pathological.utils :as u])
  (:import
    [clojure.lang Keyword]))

(declare ->file-store)

(defprotocol FileAttributeViewSupport
  (supports-file-attribute-view [file-store class-or-name]))

(defprotocol FileStoreAttributeViewSupport
  (read-file-store-attribute-view [file-store class-or-name])
  (read-attribute [file-store attribute-spec]))

(defprotocol ReloadFileStoreAttributes
  (reload [view]))

(defmulti ^:private do-supports-file-attribute-view
  (fn [_ class-or-name]
    (type class-or-name)))

(defmethod ^:private do-supports-file-attribute-view String
  [^java.nio.file.FileStore file-store ^String class-or-name]
  (.supportsFileAttributeView file-store class-or-name))

(defmethod ^:private do-supports-file-attribute-view Class
  [^java.nio.file.FileStore file-store ^Class class-or-name]
  (.supportsFileAttributeView file-store class-or-name))

(defmethod ^:private do-supports-file-attribute-view Keyword
  [^java.nio.file.FileStore file-store class-or-name]
  (.supportsFileAttributeView file-store (name class-or-name)))

(def ^:dynamic *file-store-attributes-factories*
  {})

(defn ->file-store-attributes-factory [type]
  (get *file-store-attributes-factories* type
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
  (supports-file-attribute-view [_ class-or-name]
    (let [class-or-name (u/->file-attribute-view-class class-or-name)]
      (do-supports-file-attribute-view delegate class-or-name)))

  FileStoreAttributeViewSupport
  (read-file-store-attribute-view [_ class-or-name]
    (let [type-class (u/->file-store-attribute-view-class class-or-name)
          factory (->file-store-attributes-factory type)]
      (factory (.getFileStoreAttributeView
                 ^java.nio.file.FileStore delegate type-class))))

  (read-attribute [_ attribute-spec]
    (let [value (.getAttribute
                  ^java.nio.file.FileStore delegate attribute-spec)]
      (u/<-attribute-value attribute-spec value))))

(defn ->file-store [^java.nio.file.FileStore file-store]
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
