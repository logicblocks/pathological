(ns pathological.attributes-test
  (:require
    [clojure.test :refer :all]

    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.principals :as pr]
    [pathological.attributes :as a]
    [pathological.utils :as u]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration
             windows-configuration]])
  (:import [java.util Arrays]))

(deftest basic-set-last-modified-time
  (testing "sets string last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-last-modified-time initial-view value)]
      (is (= value (f/read-attribute path "basic:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-last-modified-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "basic:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest basic-set-last-access-time
  (testing "sets string last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-last-access-time initial-view value)]
      (is (= value (f/read-attribute path "basic:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-last-access-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "basic:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest basic-set-creation-time
  (testing "sets string creation time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-creation-time initial-view value)]
      (is (= value (f/read-attribute path "basic:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-creation-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "basic:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view))))))

(deftest basic-set-times
  (testing "sets string times on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-times initial-view
                         last-modified-time last-access-time creation-time)]
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "basic:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time])))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)
          updated-view (a/set-times initial-view
                         (u/->file-time last-modified-time)
                         (u/->file-time last-access-time)
                         (u/->file-time creation-time))]

      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "basic:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time]))))))

(deftest basic-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :basic)

          _ (f/set-attribute path "basic:lastAccessTime" value)

          updated-view (a/reload initial-view)]
      (is (not= value (:last-access-time initial-view)))
      (is (= value (:last-access-time updated-view))))))

(deftest owner-set-owner
  (testing "sets user principle owner on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->user-principal test-file-system "some-user")

          initial-view (f/read-file-attribute-view path :owner)
          updated-view (a/set-owner initial-view value)]
      (is (= value (f/read-attribute path "owner:owner")))
      (is (= value (:owner updated-view))))))

(deftest owner-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->user-principal test-file-system "some-user")

          initial-view (f/read-file-attribute-view path :owner)

          _ (f/set-attribute path "owner:owner" value)

          updated-view (a/reload initial-view)]
      (is (not= value (:owner initial-view)))
      (is (= value (:owner updated-view))))))

(deftest posix-set-last-modified-time
  (testing "sets string last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-last-modified-time initial-view value)]
      (is (= value (f/read-attribute path "posix:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-last-modified-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "posix:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest posix-set-last-access-time
  (testing "sets string last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-last-access-time initial-view value)]
      (is (= value (f/read-attribute path "posix:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-last-access-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "posix:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest posix-set-creation-time
  (testing "sets string creation time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-creation-time initial-view value)]
      (is (= value (f/read-attribute path "posix:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-creation-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "posix:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view))))))

(deftest posix-set-times
  (testing "sets string times on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-times initial-view
                         last-modified-time last-access-time creation-time)]
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "posix:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time])))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-times initial-view
                         (u/->file-time last-modified-time)
                         (u/->file-time last-access-time)
                         (u/->file-time creation-time))]

      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "posix:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time]))))))

(deftest posix-set-owner
  (testing "sets user principle owner on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->user-principal test-file-system "some-user")

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-owner initial-view value)]
      (is (= value (f/read-attribute path "posix:owner")))
      (is (= value (:owner updated-view))))))

(deftest posix-set-group
  (testing "sets group principle group on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->group-principal test-file-system "some-group")

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-group initial-view value)]
      (is (= value (f/read-attribute path "posix:group")))
      (is (= value (:group updated-view))))))

(deftest posix-set-permissions
  (testing "sets string permissions on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value "rwxr-xr-x"
          expected #{:owner-read
                     :owner-write
                     :owner-execute
                     :group-read
                     :group-execute
                     :others-read
                     :others-execute}

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-permissions initial-view value)]
      (is (= expected (f/read-attribute path "posix:permissions")))
      (is (= expected (:permissions updated-view)))))

  (testing "sets keyword set permissions on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value #{:owner-read
                  :owner-write
                  :owner-execute
                  :group-read
                  :group-execute
                  :others-read
                  :others-execute}

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-permissions initial-view value)]
      (is (= value (f/read-attribute path "posix:permissions")))
      (is (= value (:permissions updated-view)))))

  (testing "sets permission set permissions on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (u/->posix-file-permissions "rwxr--r--")
          expected #{:owner-read
                     :owner-write
                     :owner-execute
                     :group-read
                     :others-read}

          initial-view (f/read-file-attribute-view path :posix)
          updated-view (a/set-permissions initial-view value)]
      (is (= expected (f/read-attribute path "posix:permissions")))
      (is (= expected (:permissions updated-view))))))

(deftest posix-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->user-principal test-file-system "some-user")

          initial-view (f/read-file-attribute-view path :posix)

          _ (f/set-attribute path "posix:owner" value)

          updated-view (a/reload initial-view)]
      (is (not= value (:owner initial-view)))
      (is (= value (:owner updated-view))))))

(deftest dos-set-last-modified-time
  (testing "sets string last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-last-modified-time initial-view value)]
      (is (= value (f/read-attribute path "dos:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last modified time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-last-modified-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "dos:lastModifiedTime")))
      (is (= value (:last-modified-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest dos-set-last-access-time
  (testing "sets string last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-last-access-time initial-view value)]
      (is (= value (f/read-attribute path "dos:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-last-access-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "dos:lastAccessTime")))
      (is (= value (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))
      (is (= (:creation-time initial-view)
            (:creation-time updated-view))))))

(deftest dos-set-creation-time
  (testing "sets string creation time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-creation-time initial-view value)]
      (is (= value (f/read-attribute path "dos:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view)))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-creation-time initial-view
                         (u/->file-time value))]
      (is (= value (f/read-attribute path "dos:creationTime")))
      (is (= value (:creation-time updated-view)))
      (is (= (:last-access-time initial-view)
            (:last-access-time updated-view)))
      (is (= (:last-modified-time initial-view)
            (:last-modified-time updated-view))))))

(deftest dos-set-times
  (testing "sets string times on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-times initial-view
                         last-modified-time last-access-time creation-time)]
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "dos:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time])))))

  (testing "sets file time last access time on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          creation-time "2019-05-04T22:10:10Z"
          last-modified-time "2019-05-04T22:40:10Z"
          last-access-time "2019-05-04T23:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)
          updated-view (a/set-times initial-view
                         (u/->file-time last-modified-time)
                         (u/->file-time last-access-time)
                         (u/->file-time creation-time))]

      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (f/read-attributes path
              "dos:creationTime,lastAccessTime,lastModifiedTime")))
      (is (= {:creation-time      creation-time
              :last-modified-time last-modified-time
              :last-access-time   last-access-time}
            (select-keys updated-view [:creation-time
                                       :last-modified-time
                                       :last-access-time]))))))

(deftest dos-set-read-only
  (let [test-file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (windows-configuration
            :attribute-views #{:dos}))

        path (p/path test-file-system "C:\\file")

        _ (f/create-file path)

        value true

        initial-view (f/read-file-attribute-view path :dos)
        updated-view (a/set-read-only initial-view value)]
    (is (= true (f/read-attribute path "dos:readonly")))
    (is (= true (:read-only? updated-view)))))

(deftest dos-set-hidden
  (let [test-file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (windows-configuration
            :attribute-views #{:dos}))

        path (p/path test-file-system "C:\\file")

        _ (f/create-file path)

        value true

        initial-view (f/read-file-attribute-view path :dos)
        updated-view (a/set-hidden initial-view value)]
    (is (= true (f/read-attribute path "dos:hidden")))
    (is (= true (:hidden? updated-view)))))

(deftest dos-set-system
  (let [test-file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (windows-configuration
            :attribute-views #{:dos}))

        path (p/path test-file-system "C:\\file")

        _ (f/create-file path)

        value true

        initial-view (f/read-file-attribute-view path :dos)
        updated-view (a/set-system initial-view value)]
    (is (= true (f/read-attribute path "dos:system")))
    (is (= true (:system? updated-view)))))

(deftest dos-set-archive
  (let [test-file-system
        (new-in-memory-file-system
          (random-file-system-name)
          (windows-configuration
            :attribute-views #{:dos}))

        path (p/path test-file-system "C:\\file")

        _ (f/create-file path)

        value true

        initial-view (f/read-file-attribute-view path :dos)
        updated-view (a/set-archive initial-view value)]
    (is (= true (f/read-attribute path "dos:archive")))
    (is (= true (:archive? updated-view)))))

(deftest dos-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          value "2019-05-04T22:10:10Z"

          initial-view (f/read-file-attribute-view path :dos)

          _ (f/set-attribute path "dos:creationTime" value)

          updated-view (a/reload initial-view)]
      (is (not= value (:creation-time initial-view)))
      (is (= value (:creation-time updated-view))))))

(deftest user-defined-write-attribute
  (testing "writes string custom attribute on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          name "important"
          ^bytes value "123456"

          initial-view (f/read-file-attribute-view path :user)
          updated-view (a/write-attribute initial-view name value)]
      (is (Arrays/equals
            ^bytes (u/->bytes value)
            ^bytes (f/read-attribute path "user:important")))
      (is (Arrays/equals
            ^bytes (u/->bytes value)
            ^bytes (get-in updated-view [:attributes name])))))

  (testing "writes bytes custom attribute on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          name "important"
          ^bytes value (u/->bytes "123456")

          initial-view (f/read-file-attribute-view path :user)
          updated-view (a/write-attribute initial-view name value)]
      (is (Arrays/equals
            value
            ^bytes (f/read-attribute path "user:important")))
      (is (Arrays/equals
            value
            ^bytes (get-in updated-view [:attributes name])))))

  (testing "writes byte buffer custom attribute on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          name "important"
          value "123456"
          byte-buffer (u/->byte-buffer value)

          initial-view (f/read-file-attribute-view path :user)
          updated-view (a/write-attribute initial-view name byte-buffer)]
      (is (Arrays/equals
            ^bytes (u/->bytes value)
            ^bytes (f/read-attribute path "user:important")))
      (is (Arrays/equals
            ^bytes (u/->bytes value)
            ^bytes (get-in updated-view [:attributes name]))))))

(deftest user-defined-delete-attribute
  (testing "deletes custom attribute on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          name "important"
          ^bytes value (u/->bytes "123456")

          _ (f/set-attribute path "user:important" value)

          initial-view (f/read-file-attribute-view path :user)
          updated-view (a/delete-attribute initial-view name)]
      (is (Arrays/equals
            value
            ^bytes (get-in initial-view [:attributes name])))
      (is (not (nil? updated-view)))
      (is (nil? (get-in updated-view [:attributes name]))))))

(deftest user-defined-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          name "important"
          ^bytes value (u/->bytes "123456")

          initial-view (f/read-file-attribute-view path :user)

          _ (f/set-attribute path "user:important" value)

          updated-view (a/reload initial-view)]
      (is (not
            (Arrays/equals
              value
              ^bytes (get-in initial-view [:attributes name]))))
      (is (Arrays/equals
            value
            ^bytes (get-in updated-view [:attributes name]))))))

(deftest acl-set-owner
  (testing "sets user principle owner on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          value (pr/->user-principal test-file-system "some-user")

          initial-view (f/read-file-attribute-view path :acl)
          updated-view (a/set-owner initial-view value)]
      (is (= value (f/read-attribute path "acl:owner")))
      (is (= value (:owner updated-view))))))

(deftest acl-set-acl
  (testing "sets acl entry list acl on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          user (pr/->user-principal test-file-system "some-user")
          value [(a/->acl-entry
                   {:type        :allow
                    :principal   user
                    :permissions #{:read-attributes :write-attributes}})
                 (a/->acl-entry
                   {:type        :deny
                    :principal   user
                    :permissions #{:delete}
                    :flags       #{:file-inherit :directory-inherit}})]
          expected [{:type        :allow
                     :principal   user
                     :permissions #{:read-attributes :write-attributes}
                     :flags       #{}}
                    {:type        :deny
                     :principal   user
                     :permissions #{:delete}
                     :flags       #{:file-inherit :directory-inherit}}]

          initial-view (f/read-file-attribute-view path :acl)
          updated-view (a/set-acl initial-view value)]
      (is (= expected (f/read-attribute path "acl:acl")))
      (is (= expected (:acl updated-view)))))

  (testing "sets map list acl on path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          user (pr/->user-principal test-file-system "some-user")
          value [{:type        :allow
                  :principal   user
                  :permissions #{:read-attributes :write-attributes}
                  :flags       #{}}
                 {:type        :deny
                  :principal   user
                  :permissions #{:delete}
                  :flags       #{:file-inherit :directory-inherit}}]

          initial-view (f/read-file-attribute-view path :acl)
          updated-view (a/set-acl initial-view value)]
      (is (= value (f/read-attribute path "acl:acl")))
      (is (= value (:acl updated-view))))))

(deftest acl-reload
  (testing "returns an updated view"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "C:\\file")

          _ (f/create-file path)

          user (pr/->user-principal test-file-system "some-user")
          value [(a/->acl-entry
                   {:type        :allow
                    :principal   user
                    :permissions #{:read-attributes :write-attributes}})
                 (a/->acl-entry
                   {:type        :deny
                    :principal   user
                    :permissions #{:delete}
                    :flags       #{:file-inherit :directory-inherit}})]
          expected [{:type        :allow
                     :principal   user
                     :permissions #{:read-attributes :write-attributes}
                     :flags       #{}}
                    {:type        :deny
                     :principal   user
                     :permissions #{:delete}
                     :flags       #{:file-inherit :directory-inherit}}]

          initial-view (f/read-file-attribute-view path :acl)

          _ (f/set-attribute path "acl:acl" value)

          updated-view (a/reload initial-view)]
      (is (not= expected (:acl initial-view)))
      (is (= expected (:acl updated-view))))))
