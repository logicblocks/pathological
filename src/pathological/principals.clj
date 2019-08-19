(ns pathological.principals
  (:require
    [pathological.file-systems :as fs])
  (:import
    [java.nio.file FileSystem]
    [java.nio.file.attribute UserPrincipal GroupPrincipal]))

(defrecord BasicUserPrincipal [name underlying]
  UserPrincipal
  (getName [_] name))

(defrecord BasicGroupPrincipal [name underlying]
  GroupPrincipal
  (getName [_] name))

(defn lookup-principal-by-name
  ([name] (lookup-principal-by-name fs/*file-system* name))
  ([^FileSystem file-system name]
   (let [lookup-service (.getUserPrincipalLookupService file-system)
         user-principal (.lookupPrincipalByName lookup-service name)]
     user-principal)))

(defn lookup-principal-by-group-name
  ([name] (lookup-principal-by-group-name fs/*file-system* name))
  ([^FileSystem file-system name]
   (let [lookup-service (.getUserPrincipalLookupService file-system)
         group-principal (.lookupPrincipalByGroupName lookup-service name)]
     group-principal)))

(defn ->user-principal
  ([name] (->user-principal fs/*file-system* name))
  ([file-system name]
   (let [underlying
         (lookup-principal-by-name file-system name)]
     (->BasicUserPrincipal name underlying))))

(defn <-user-principal
  ([principal]
   (if-not (instance? BasicUserPrincipal principal)
     (->BasicUserPrincipal (.getName principal) principal)
     principal)))

(defn ->group-principal
  ([name] (->group-principal fs/*file-system* name))
  ([file-system name]
   (let [underlying
         (lookup-principal-by-group-name file-system name)]
     (->BasicGroupPrincipal name underlying))))

(defn <-group-principal
  ([principal]
   (if-not (instance? BasicGroupPrincipal principal)
     (->BasicGroupPrincipal (.getName principal) principal)
     principal)))
