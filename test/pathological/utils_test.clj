(ns pathological.utils-test
  (:require
    [clojure.test :refer :all]

    [pathological.utils :as u]
    [pathological.attributes :as a])
  (:import [java.nio.file.attribute PosixFilePermission
                                    PosixFilePermissions]))

(defmacro with-var-root-reset [var & body]
  `(let [before# (var-get ~var)
         result# (do ~@body)]
     (alter-var-root ~var (constantly before#))
     result#))

(deftest <-posix-file-permissions-string
  (testing "returns posix file permission set for provided string"
    (is (= #{} (u/<-posix-file-permissions-string "---------")))
    (is (= #{:owner-read
             :owner-write
             :owner-execute
             :group-read
             :others-read}
          (u/<-posix-file-permissions-string "rwxr--r--")))
    (is (= #{:owner-read
             :owner-write
             :owner-execute
             :group-read
             :group-write
             :group-execute
             :others-read
             :others-write
             :others-execute}
          (u/<-posix-file-permissions-string "rwxrwxrwx")))))

(deftest ->posix-file-permissions-string
  (testing "returns posix file permissions string for provided keyword set"
    (is (= "---------" (u/->posix-file-permissions-string #{})))
    (is (= "rwxr--r--"
          (u/->posix-file-permissions-string
            #{:owner-read
              :owner-write
              :owner-execute
              :group-read
              :others-read})))
    (is (= "rwxrwxrwx"
          (u/->posix-file-permissions-string
            #{:owner-read
              :owner-write
              :owner-execute
              :group-read
              :group-write
              :group-execute
              :others-read
              :others-write
              :others-execute})))))

(deftest ->posix-file-permissions-attributes
  (testing "returns posix file permissions attribute for provided string"
    (is (= (.value (PosixFilePermissions/asFileAttribute #{}))
          (.value (u/->posix-file-permissions-attribute "---------"))))
    (is (= (.value (PosixFilePermissions/asFileAttribute
                     #{PosixFilePermission/OWNER_READ
                       PosixFilePermission/OWNER_WRITE
                       PosixFilePermission/OWNER_EXECUTE
                       PosixFilePermission/GROUP_READ
                       PosixFilePermission/OTHERS_READ}))
          (.value (u/->posix-file-permissions-attribute "rwxr--r--"))))
    (is (= (.value (PosixFilePermissions/asFileAttribute
                     #{PosixFilePermission/OWNER_READ
                       PosixFilePermission/OWNER_WRITE
                       PosixFilePermission/OWNER_EXECUTE
                       PosixFilePermission/GROUP_READ
                       PosixFilePermission/GROUP_WRITE
                       PosixFilePermission/GROUP_EXECUTE
                       PosixFilePermission/OTHERS_READ
                       PosixFilePermission/OTHERS_WRITE
                       PosixFilePermission/OTHERS_EXECUTE}))
          (.value (u/->posix-file-permissions-attribute "rwxrwxrwx")))))

  (testing "returns posix file permissions attribute for provided set"
    (is (= (.value (PosixFilePermissions/asFileAttribute #{}))
          (.value (u/->posix-file-permissions-attribute #{}))))
    (is (= (.value (PosixFilePermissions/asFileAttribute
                     #{PosixFilePermission/OWNER_READ
                       PosixFilePermission/OWNER_WRITE
                       PosixFilePermission/OWNER_EXECUTE
                       PosixFilePermission/GROUP_READ
                       PosixFilePermission/OTHERS_READ}))
          (.value (u/->posix-file-permissions-attribute
                    #{:owner-read
                      :owner-write
                      :owner-execute
                      :group-read
                      :others-read}))))
    (is (= (.value (PosixFilePermissions/asFileAttribute
                     #{PosixFilePermission/OWNER_READ
                       PosixFilePermission/OWNER_WRITE
                       PosixFilePermission/OWNER_EXECUTE
                       PosixFilePermission/GROUP_READ
                       PosixFilePermission/GROUP_WRITE
                       PosixFilePermission/GROUP_EXECUTE
                       PosixFilePermission/OTHERS_READ
                       PosixFilePermission/OTHERS_WRITE
                       PosixFilePermission/OTHERS_EXECUTE}))
          (.value (u/->posix-file-permissions-attribute
                    #{:owner-read
                      :owner-write
                      :owner-execute
                      :group-read
                      :group-write
                      :group-execute
                      :others-read
                      :others-write
                      :others-execute}))))))

(deftest ->attribute-value
  (testing "converts user attribute value to byte buffer"
    (is (= (u/->byte-buffer "hello")
          (u/->attribute-value "user:thing" "hello")))
    (is (= (u/->byte-buffer "goodbye")
          (u/->attribute-value "user:wat" (u/->bytes "goodbye")))))

  (testing "converts time attribute value to file time"
    (is (= (u/->file-time "2019-08-02T10:01:01Z")
          (u/->attribute-value "creationTime" "2019-08-02T10:01:01Z")))
    (is (= (u/->file-time "2019-06-05T18:20:21Z")
          (u/->attribute-value "posix:lastAccessTime" "2019-06-05T18:20:21Z")))
    (is (= (u/->file-time "2019-01-12T20:08:10Z")
          (u/->attribute-value
            "basic:lastModifiedTime" "2019-01-12T20:08:10Z"))))

  (testing "converts posix permission attribute value to posix file permissions"
    (is (= (u/->posix-file-permissions "rwxr--r--")
          (u/->attribute-value "posix:permissions" "rwxr--r--")))
    (is (= (u/->posix-file-permissions "r--r--r--")
          (u/->attribute-value "posix:permissions"
            #{:owner-read :group-read :others-read}))))

  (testing "leaves attribute value unchanged by default"
    (is (= "original value"
          (u/->attribute-value "unknown:attribute" "original value"))))

  (testing "allows conversions to be registered for specific attributes"
    (with-var-root-reset #'u/*->attribute-value-conversions*
      (a/register-conversion :to [:view :attribute-name]
        (fn [value] (* value 2)))
      (is (= 4 (u/->attribute-value "view:attributeName" 2))))))
