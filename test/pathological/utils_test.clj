(ns pathological.utils-test
  (:require
    [clojure.test :refer :all]

    [pathological.utils :as u])
  (:import [java.nio.file.attribute PosixFilePermission
            PosixFilePermissions]))

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
