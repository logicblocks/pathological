(ns pathological.file-stores
  (:import
    [java.nio.file FileStore]))

(defprotocol FileAttributeViewSupport
  (supports-file-attribute-view [file-store class-or-name]))

(defprotocol FileStoreAttributeViewSupport
  (read-file-store-attribute-view [file-store class-or-name])
  (read-attribute [file-store attribute-spec]))

(defrecord BasicFileStore
  [name
   type
   read-only?
   total-space
   usable-space
   unallocated-space
   block-size
   delegate])

(defn ->file-store [^FileStore file-store]
  (let [block-size
        (try (.getBlockSize file-store)
          (catch UnsupportedOperationException _ nil))]
    (map->BasicFileStore
      {:name              (.name file-store)
       :type              (.type file-store)
       :read-only?        (.isReadOnly file-store)
       :total-space       (.getTotalSpace file-store)
       :usable-space      (.getUsableSpace file-store)
       :unallocated-space (.getUnallocatedSpace file-store)
       :block-size        block-size
       :delegate          file-store})))
