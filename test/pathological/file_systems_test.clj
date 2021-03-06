(ns pathological.file-systems-test
  (:require
   [clojure.test :refer :all]

   [pathological.file-systems :as fs]
   [pathological.file-stores :as fst]
   [pathological.paths :as p]
   [pathological.testing :as t])
  (:import
   [java.nio.file FileSystem
    FileStore
    FileSystems]))

(deftest default-file-system
  (testing "returns default file system"
    (is (= (FileSystems/getDefault)
          (fs/default-file-system)))))

(deftest file-system-var
  (testing "uses default file system by default"
    (is (= (FileSystems/getDefault)
          fs/*file-system*))))

(deftest provider
  (testing "returns the provider for the file system"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (is (= (.provider test-file-system)
            (fs/provider test-file-system))))))

(deftest close
  (testing "closes the file system"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (is (true? (.isOpen test-file-system)))

      (fs/close test-file-system)

      (is (false? (.isOpen test-file-system))))))

(deftest open?
  (testing "returns true when the file system is open"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (is (true? (fs/open? test-file-system)))))

  (testing "returns false when the file system is closed"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (.close test-file-system)
      (is (false? (fs/open? test-file-system))))))

(deftest read-only?
  ; No easy way to test the read only case

  (testing "returns false when the file system is not read only"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (is (false? (fs/read-only? test-file-system))))))

(deftest file-stores
  (testing "returns the file stores of the provided file system"
    (let [test-file-system (t/new-unix-in-memory-file-system)

          file-stores (.getFileStores ^FileSystem test-file-system)
          ^FileStore file-store (first file-stores)]
      (is (= [(fst/map->FileStore
                {:name              (.name file-store)
                 :type              (.type file-store)
                 :read-only?        (.isReadOnly file-store)
                 :total-space       (.getTotalSpace file-store)
                 :usable-space      (.getUsableSpace file-store)
                 :unallocated-space (.getUnallocatedSpace file-store)
                 :block-size        nil
                 :delegate          nil})]
            (map #(assoc % :delegate nil)
              (fs/file-stores test-file-system)))))))

(deftest root-directories
  (testing "returns the only root directory when only one"
    (let [test-file-system
          (t/new-unix-in-memory-file-system
            :roots ["/"])]
      (is (= #{(p/path test-file-system "/")}
            (fs/root-directories test-file-system)))))

  (testing "returns all root directories when many"
    (let [test-file-system
          (t/new-windows-in-memory-file-system
            :roots ["C://" "D://"])]
      (is (= #{(p/path test-file-system "C://")
               (p/path test-file-system "D://")}
            (fs/root-directories test-file-system))))))

(deftest separator
  (testing "returns unix separator"
    (let [test-file-system (t/new-unix-in-memory-file-system)]
      (is (= "/" (fs/separator test-file-system)))))

  (testing "returns windows separator"
    (let [test-file-system (t/new-windows-in-memory-file-system)]
      (is (= "\\" (fs/separator test-file-system))))))

(deftest supported-file-attribute-views
  (testing "returns the set of supported file attribute views"
    (let [test-file-system
          (t/new-unix-in-memory-file-system
            :attribute-views #{:basic :owner})]
      (is (= #{:basic :owner}
            (fs/supported-file-attribute-views test-file-system))))))
