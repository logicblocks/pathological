(ns pathological.principals
  (:require
    [pathological.file-systems :as fs])
  (:import
    [java.nio.file FileSystem]))

(defrecord UserPrincipal [name underlying]
  java.nio.file.attribute.UserPrincipal
  (getName [_] name))

(defrecord GroupPrincipal [name underlying]
  java.nio.file.attribute.GroupPrincipal
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
     (->UserPrincipal name underlying))))

(defn <-user-principal
  ([principal]
   (if-not (instance? UserPrincipal principal)
     (->UserPrincipal (.getName principal) principal)
     principal)))

(defn ->group-principal
  ([name] (->group-principal fs/*file-system* name))
  ([file-system name]
   (let [underlying
         (lookup-principal-by-group-name file-system name)]
     (->GroupPrincipal name underlying))))

(defn <-group-principal
  ([principal]
   (if-not (instance? GroupPrincipal principal)
     (->GroupPrincipal (.getName principal) principal)
     principal)))
