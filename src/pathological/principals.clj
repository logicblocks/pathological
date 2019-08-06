(ns pathological.principals
  (:require
    [pathological.file-systems :as file-systems])
  (:import [java.nio.file FileSystem]))

(defn lookup-principal-by-name
  ([name] (lookup-principal-by-name file-systems/*file-system* name))
  ([^FileSystem file-system name]
   (let [lookup-service (.getUserPrincipalLookupService file-system)
         user-principal (.lookupPrincipalByName lookup-service name)]
     user-principal)))
