(ns pathological.support.jimfs
  (:refer-clojure :exclude [class name])
  (:require
   [pathological.utils :as u]

   [pathological.support.reflect :as reflect])
  (:import
   [com.google.common.jimfs JimfsFileSystem]
   [java.nio.file.attribute FileAttribute]))

(defn name [^JimfsFileSystem file-system]
  (-> file-system
    (reflect/invoke :get-uri)
    (reflect/invoke :get-authority)))

(defn path-service [^JimfsFileSystem file-system]
  (reflect/invoke file-system :get-path-service))

(defn file-store [^JimfsFileSystem file-system]
  (reflect/invoke file-system :get-file-store))

(defn default-file-view [^JimfsFileSystem file-system]
  (reflect/invoke file-system :get-default-view))

(defn path-type [^JimfsFileSystem file-system]
  (reflect/get-field (path-service file-system) :type))

(defn name-display-normalization [^JimfsFileSystem file-system]
  (reflect/get-field (path-service file-system) :display-normalizations))

(defn name-canonical-normalization [^JimfsFileSystem file-system]
  (reflect/get-field (path-service file-system) :canonical-normalizations))

(defn path-equality-uses-canonical-form? [^JimfsFileSystem file-system]
  (boolean
    (reflect/get-field
      (path-service file-system) :equality-uses-canonical-form)))

(defn disk [^JimfsFileSystem file-system]
  (reflect/get-field (file-store file-system) :disk))

(defn block-size [^JimfsFileSystem file-system]
  (reflect/get-field (disk file-system) :block-size))

(defn max-block-count [^JimfsFileSystem file-system]
  (reflect/get-field (disk file-system) :max-block-count))

(defn max-cached-block-count [^JimfsFileSystem file-system]
  (reflect/get-field (disk file-system) :max-cached-block-count))

(defn max-size [^JimfsFileSystem file-system]
  (* (block-size file-system) (max-block-count file-system)))

(defn max-cache-size [^JimfsFileSystem file-system]
  (* (block-size file-system) (max-cached-block-count file-system)))

(defn supported-features [^JimfsFileSystem file-system]
  (reflect/get-field (file-store file-system) :supported-features))

(defn default-attribute-values [^JimfsFileSystem file-system]
  (let [default-values (-> (file-store file-system)
                         (reflect/get-field :attributes)
                         (reflect/get-field :default-values))]
    (mapv
      (fn [^FileAttribute value]
        (u/->file-attribute
          (.name value)
          (u/<-attribute-value (.name value) (.value value))))
      default-values)))

(defn working-directory [^JimfsFileSystem file-system]
  (reflect/get-field (default-file-view file-system) :working-directory-path))
