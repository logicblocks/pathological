(ns pathological.file-stores-test
  (:require
    [clojure.test :refer :all]

    [pathological.file-stores :as fst]
    [pathological.file-systems :as fs]
    [pathological.files :as f]
    [pathological.paths :as p]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system
             unix-configuration
             windows-configuration]])
  (:import [java.nio.file.attribute AclFileAttributeView
                                    BasicFileAttributeView]))

(deftest supports-file-attribute-view
  (testing "for keyword"
    (testing "returns true when file attribute view is supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (true? (fst/supports-file-attribute-view
                     file-store :owner)))))

    (testing "returns false when file attribute view is not supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (false? (fst/supports-file-attribute-view
                      file-store :acl))))))

  (testing "for string"
    (testing "returns true when file attribute view is supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (true? (fst/supports-file-attribute-view
                     file-store "owner")))))

    (testing "returns false when file attribute view is not supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (false? (fst/supports-file-attribute-view
                      file-store "acl"))))))

  (testing "for class"
    (testing "returns true when file attribute view is supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (true? (fst/supports-file-attribute-view
                     file-store BasicFileAttributeView)))))

    (testing "returns false when file attribute view is not supported"
      (let [test-file-system
            (new-in-memory-file-system
              (random-file-system-name)
              (unix-configuration
                :attribute-views #{:basic :owner :posix}))

            file-store (first (fs/file-stores test-file-system))]
        (is (false? (fst/supports-file-attribute-view
                      file-store AclFileAttributeView)))))))

(deftest reload
  (testing "returns an updated file store"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic :owner :posix}))

          file-store-before (first (fs/file-stores test-file-system))
          _ (f/write-lines (p/path test-file-system "/file")
              ["Line 1" "Line 2"])
          file-store-after (fst/reload file-store-before)]
      (is (< (:usable-space file-store-after)
            (:usable-space file-store-before)))
      (is (< (:unallocated-space file-store-after)
            (:unallocated-space file-store-before)))
      (is (= (select-keys file-store-after
               [:name :type :read-only? :total-space :block-size])
            (select-keys file-store-before
              [:name :type :read-only? :total-space :block-size]))))))
