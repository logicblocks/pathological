(ns pathological.attribute-specs-test
  (:require
    [clojure.test :refer :all]

    [pathological.attribute-specs :as as]))

(deftest ->attribute-spec
  (testing "leaves existing attribute spec map untouched"
    (is (= {:view :basic :name :last-access-time}
          (as/->attribute-spec {:view :basic :name :last-access-time})))
    (is (= {:view :posix :name :permissions}
          (as/->attribute-spec {:view :posix :name :permissions})))
    (is (= {:view :user :name :*}
          (as/->attribute-spec {:view :user :name :*})))
    (is (= {:view :* :name :creation-time}
          (as/->attribute-spec {:view :* :name :creation-time}))))

  (testing "converts attribute spec string to map"
    (is (= {:view :basic :name :last-access-time}
          (as/->attribute-spec "basic:last-access-time")))
    (is (= {:view :basic :name :last-modified-time}
          (as/->attribute-spec "basic:lastModifiedTime")))
    (is (= {:view :basic :name :creation-time}
          (as/->attribute-spec "creationTime")))
    (is (= {:view :custom-view :name :important-attribute}
          (as/->attribute-spec "customView:importantAttribute")))
    (is (= {:view :* :name :important-attribute}
          (as/->attribute-spec "*:importantAttribute")))
    (is (= {:view :posix :name :*}
          (as/->attribute-spec "posix:*"))))

  (testing "converts attribute spec with multiple names to map"
    (is (= {:view :posix :names #{:last-modified-time :last-access-time}}
          (as/->attribute-spec
            "posix:last-modified-time,last-access-time")))
    (is (= {:view :posix :names #{:creation-time :last-access-time}}
          (as/->attribute-spec
            "posix:creationTime,lastAccessTime")))
    (is (= {:view :basic :names #{:creation-time :last-modified-time}}
          (as/->attribute-spec
            "creationTime,lastModifiedTime")))))

(deftest ->attribute-spec-string
  (testing "leaves existing camel case attribute spec string untouched"
    (is (= "basic:lastAccessTime"
          (as/->attribute-spec-string "basic:lastAccessTime")))
    (is (= "user:*"
          (as/->attribute-spec-string "user:*")))
    (is (= "*:creationTime"
          (as/->attribute-spec-string "*:creationTime"))))

  (testing "converts existing kebab case attribute spec string to camel case"
    (is (= "basic:lastAccessTime"
          (as/->attribute-spec-string "basic:last-access-time")))
    (is (= "customView:*"
          (as/->attribute-spec-string "custom-view:*")))
    (is (= "*:creationTime"
          (as/->attribute-spec-string "*:creation-time"))))

  (testing "converts attribute spec map to string"
    (is (= "basic:lastAccessTime"
          (as/->attribute-spec-string
            {:view :basic :name :last-access-time})))
    (is (= "customView:importantAttribute"
          (as/->attribute-spec-string
            {:view :custom-view :name :important-attribute})))
    (is (= "*:importantAttribute"
          (as/->attribute-spec-string
            {:view :* :name :important-attribute})))
    (is (= "posix:*"
          (as/->attribute-spec-string
            {:view :posix :name :*}))))

  (testing "converts attribute spec map with multiple names to string"
    (is (= "posix:lastAccessTime,lastModifiedTime"
          (as/->attribute-spec-string
            {:view  :posix
             :names (sorted-set :last-modified-time :last-access-time)})))
    (is (= "posix:creationTime,lastAccessTime"
          (as/->attribute-spec-string
            {:view  :posix
             :names (sorted-set :creation-time :last-access-time)})))))

(deftest view
  (testing "returns the view from an attribute spec map"
    (is (= :posix (as/view {:view :posix :name :permissions})))
    (is (= :user (as/view {:view :user :name :*})))
    (is (= :* (as/view {:view :* :name :creation-time}))))

  (testing "returns the view from a kebab case attribute spec string"
    (is (= :posix (as/view "posix:permissions")))
    (is (= :custom-view (as/view "custom-view:*")))
    (is (= :* (as/view "*:creation-time"))))

  (testing "returns the view from a camel case attribute spec string"
    (is (= :custom-view (as/view "customView:importantAttribute")))
    (is (= :custom-view (as/view "customView:*")))
    (is (= :* (as/view "*:creationTime")))))

(deftest name
  (testing "returns the name from an attribute spec map"
    (is (= :creation-time (as/name {:view :basic :name :creation-time})))
    (is (= :* (as/name {:view :user :name :*})))
    (is (= :creation-time (as/name {:view :* :name :creation-time}))))

  (testing "returns the name from a kebab case attribute spec string"
    (is (= :creation-time (as/name "basic:creation-time")))
    (is (= :* (as/name "custom-view:*")))
    (is (= :creation-time (as/name "*:creation-time"))))

  (testing "returns the name from a camel case attribute spec string"
    (is (= :important-attribute (as/name "customView:importantAttribute")))
    (is (= :* (as/name "customView:*")))
    (is (= :creation-time (as/name "*:creationTime"))))

  (testing "returns nil when attribute spec has multiple names"
    (is (nil? (as/name {:view :user :names #{:custom-attribute-1
                                             :custom-attribute-2}})))
    (is (nil? (as/name "user:customAttribute1,customAttribute2")))
    (is (nil? (as/name "user:custom-attribute-1,custom-attribute-2")))))

(deftest names
  (testing "returns the names from an attribute spec map"
    (is (= #{:creation-time :last-access-time}
          (as/names {:view :basic
                     :names #{:creation-time :last-access-time}})))
    (is (= #{:creation-time :last-access-time}
          (as/names {:view :*
                     :names #{:creation-time :last-access-time}}))))

  (testing "returns the names from a kebab case attribute spec string"
    (is (= #{:creation-time :last-access-time}
          (as/names "basic:creation-time,last-access-time")))
    (is (= #{:creation-time :last-access-time}
          (as/names "*:creation-time,last-access-time")))
    (is (= #{:creation-time :last-access-time}
          (as/names "creation-time,last-access-time"))))

  (testing "returns the names from a camel case attribute spec string"
    (is (= #{:creation-time :last-access-time}
          (as/names "basic:creationTime,lastAccessTime")))
    (is (= #{:creation-time :last-access-time}
          (as/names "*:creationTime,lastAccessTime")))
    (is (= #{:creation-time :last-access-time}
          (as/names "creationTime,lastAccessTime"))))

  (testing "returns nil when attribute spec has single name"
    (is (nil? (as/names {:view :user :name :custom-attribute-1})))
    (is (nil? (as/names "user:customAttribute1")))
    (is (nil? (as/names "user:custom-attribute-1")))))
