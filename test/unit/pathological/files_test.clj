(ns pathological.files-test
  (:refer-clojure :exclude [find])
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as io]
    [clojure.string :as string]

    [pathological.files :as f]
    [pathological.paths :as p]
    [pathological.utils :as u]

    [pathological.testing
     :refer [random-file-system-name
             new-in-memory-file-system]])
  (:import
    [java.nio.file Files Path LinkOption NoSuchFileException]
    [java.nio.file.attribute PosixFilePermissions]
    [java.nio.charset StandardCharsets]))

(deftest create-directories
  (testing "creates all directories in path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/very/nested/path/")]
      (f/create-directories path)

      (is (true? (Files/exists path (u/->link-options-array []))))))

  (testing "applies supplied file attributes when creating directories"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some/very/nested/path/")]
      (f/create-directories path
        (f/posix-file-permissions-attribute "rwxrw-rw-"))

      (is (true? (Files/exists path (u/->link-options-array []))))

      (let [posix-file-permissions
            (Files/getPosixFilePermissions path (u/->link-options-array []))

            posix-file-permission-string
            (PosixFilePermissions/toString posix-file-permissions)]
        (is (= "rwxrw-rw-" posix-file-permission-string))))))

(deftest create-directory
  (testing "creates single directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/path/")]
      (f/create-directory path)

      (is (true? (Files/exists path (u/->link-options-array []))))))

  (testing "applies supplied file attributes when creating directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/path/")]
      (f/create-directory path
        (f/posix-file-permissions-attribute "rwxrw-rw-"))

      (is (true? (Files/exists path (u/->link-options-array []))))

      (let [posix-file-permissions
            (Files/getPosixFilePermissions path (u/->link-options-array []))

            posix-file-permission-string
            (PosixFilePermissions/toString posix-file-permissions)]
        (is (= "rwxrw-rw-" posix-file-permission-string))))))

(deftest create-file
  (testing "creates a file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (true? (Files/exists path (u/->link-options-array []))))))

  (testing "applies supplied file attributes when creating file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path
        (f/posix-file-permissions-attribute "rwxrw-rw-"))

      (is (true? (Files/exists path (u/->link-options-array []))))

      (let [posix-file-permissions
            (Files/getPosixFilePermissions path (u/->link-options-array []))

            posix-file-permission-string
            (PosixFilePermissions/toString posix-file-permissions)]
        (is (= "rwxrw-rw-" posix-file-permission-string))))))

(deftest create-symbolic-link
  (testing "creates a symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)

      (f/create-symbolic-link link-path target-path)

      (is (true? (Files/exists link-path
                   (u/->link-options-array [LinkOption/NOFOLLOW_LINKS]))))
      (is (true? (Files/isSymbolicLink link-path)))))

  (testing "applies supplied file attributes when creating symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)

      (f/create-symbolic-link link-path target-path
        (f/posix-file-permissions-attribute "rwxrw-rw-"))

      (is (true? (Files/exists link-path
                   (u/->link-options-array [LinkOption/NOFOLLOW_LINKS]))))
      (is (true? (Files/isSymbolicLink link-path)))

      (let [posix-file-permissions
            (Files/getPosixFilePermissions link-path
              (u/->link-options-array [LinkOption/NOFOLLOW_LINKS]))

            posix-file-permission-string
            (PosixFilePermissions/toString posix-file-permissions)]
        (is (= "rwxrw-rw-" posix-file-permission-string))))))

(deftest create-link
  (testing "creates a link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)

      (f/create-link link-path target-path)

      (is (true? (Files/exists link-path
                   (u/->link-options-array [LinkOption/NOFOLLOW_LINKS]))))
      (is (true? (Files/isSameFile link-path target-path)))))

  (testing "fails if the link target does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (is (thrown? NoSuchFileException
            (f/create-link link-path target-path))))))

(deftest delete
  (testing "deletes a file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path)

      (f/delete path)

      (is (false? (f/exists? path)))))

  (testing "deletes an empty directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/directory")]
      (f/create-directory path)

      (f/delete path)

      (is (false? (f/exists? path))))))

(deftest copy
  (testing "copies a file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")
          destination-path (p/path test-file-system "/target")]
      (f/create-file source-path)
      (f/write-lines source-path content)

      (f/copy source-path destination-path)

      (is (= content (f/read-all-lines destination-path)))))

  (testing "copies from an input stream"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          destination-path (p/path test-file-system "/target")]
      (with-open [input-stream
                  (io/input-stream
                    (.getBytes (str (string/join "\n" content) "\n")))]
        (f/copy input-stream destination-path))

      (is (= content (f/read-all-lines destination-path)))))

  (testing "copies to an output stream"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")]
      (with-open [output-stream (java.io.ByteArrayOutputStream.)]
        (f/create-file source-path)
        (f/write-lines source-path content)

        (f/copy source-path output-stream)

        (is (= (str (string/join "\n" content) "\n")
              (.toString output-stream))))))

  (testing "copies file attributes when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          source-path (p/path test-file-system "/source")
          target-path (p/path test-file-system "/target")]
      (f/create-file source-path
        (f/posix-file-permissions-attribute "rwxrw-rw-"))

      (f/copy source-path target-path :copy-attributes)

      (is (= "rwxrw-rw-" (f/posix-file-permissions-string target-path))))))

(deftest read-symbolic-link
  (testing "returns the path of the link target"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)
      (f/create-symbolic-link link-path target-path)

      (is (= target-path (f/read-symbolic-link link-path))))))

(deftest find
  (testing "returns a seq of matching paths"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:matching-path-1 {:content ["Line 1" "Line 2"]}]
                [:matching-path-2 {:content ["Line 3" "Line 4"]}]
                [:other-path-1 {:content ["Line 5" "Line 6"]}]]
               [:directory-2
                [:matching-path-3 {:content ["Line 7" "Line 8"]}]
                [:other-path-2 {:content ["Line 9" "Line 10"]}]]])

          matches (f/find root-path
                    (fn [path _] (p/matches? path "glob:**/matching-*")))]
      (is (seq? matches))
      (is (= [(p/path test-file-system "/directory-1/matching-path-1")
              (p/path test-file-system "/directory-1/matching-path-2")
              (p/path test-file-system "/directory-2/matching-path-3")]
            matches))))

  (testing "honours file visit options when supplied"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:subdirectory-1
                 [:matching-path-1 {:content ["Line 1" "Line 2"]}]
                 [:matching-path-2 {:content ["Line 3" "Line 4"]}]
                 [:other-path-1 {:content ["Line 5" "Line 6"]}]]
                [:subdirectory-2
                 [:matching-path-3 {:content ["Line 7" "Line 8"]}]
                 [:other-path-2 {:content ["Line 9" "Line 10"]}]]]
               [:directory-2 {:type :symbolic-link
                              :target "/directory-1/subdirectory-1"}]])

          matches (f/find root-path
                    (fn [path _] (p/matches? path "glob:**/matching-*"))
                    :file-visit-options #{:follow-links})]
      (is (= [(p/path test-file-system
                "/directory-1/subdirectory-1/matching-path-1")
              (p/path test-file-system
                "/directory-1/subdirectory-1/matching-path-2")
              (p/path test-file-system
                "/directory-1/subdirectory-2/matching-path-3")
              (p/path test-file-system
                "/directory-2/matching-path-1")
              (p/path test-file-system
                "/directory-2/matching-path-2")]
            matches))))

  (testing "honours maximum depth option when supplied"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:matching-path-1 {:content ["Line 1" "Line 2"]}]
                [:matching-path-2 {:content ["Line 3" "Line 4"]}]
                [:other-path-1 {:content ["Line 5" "Line 6"]}]
                [:subdirectory-1
                 [:matching-path-3 {:content ["Line 7" "Line 8"]}]
                 [:other-path-2 {:content ["Line 9" "Line 10"]}]]]])

          matches (f/find root-path
                    (fn [path _] (p/matches? path "glob:**/matching-*"))
                    :maximum-depth 2)]
      (is (= [(p/path test-file-system
                "/directory-1/matching-path-1")
              (p/path test-file-system
                "/directory-1/matching-path-2")]
            matches)))))

(deftest exists?
  ; TODO: test for symbolic link handling

  (testing "returns true when the file exists"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (true? (f/exists? path)))))

  (testing "returns false when the file does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (is (false? (f/exists? path))))))

(deftest regular-file?
  ; TODO: test for symbolic link handling

  (testing "returns true when path is a regular file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (true? (f/regular-file? path)))))

  (testing "returns false when path is not a regular file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-directory")]
      (f/create-directory path)

      (is (false? (f/regular-file? path))))))

(deftest directory?
  ; TODO: test for symbolic link handling

  (testing "returns true when path is a directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-directory")]
      (f/create-directory path)

      (is (true? (f/directory? path)))))

  (testing "returns false when path is not a directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (false? (f/directory? path))))))

(deftest symbolic-link?
  (testing "returns true when path is a symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)
      (f/create-symbolic-link link-path target-path)

      (is (true? (f/symbolic-link? link-path)))))

  (testing "returns false when path is not a symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/not-link")]
      (f/create-file path)

      (is (false? (f/symbolic-link? path))))))

(deftest same-file?
  (testing "returns true when both paths are the same"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path-1 (p/path test-file-system "/file")
          path-2 (p/path test-file-system "/file")]
      (f/create-file path-1)

      (is (true? (f/same-file? path-1 path-2)))))

  (testing "returns true when both paths point at the same file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path-1 (p/path test-file-system "/file")
          path-2 (p/path test-file-system "/link")]
      (f/create-file path-1)
      (f/create-link path-2 path-1)

      (is (true? (f/same-file? path-1 path-2)))))

  (testing "returns false when paths point at different files"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path-1 (p/path test-file-system "/file1")
          path-2 (p/path test-file-system "/file2")]
      (f/create-file path-1)
      (f/create-file path-2)

      (is (false? (f/same-file? path-1 path-2))))))

(deftest hidden?
  (testing "returns true when path represents hidden file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/.file.txt")]
      (is (true? (f/hidden? path)))))

  (testing "returns true when path represents hidden directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/.file/")]
      (is (true? (f/hidden? path)))))

  (testing "returns false when path represents standard file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (is (false? (f/hidden? path))))))

(deftest write-lines
  (testing "writes the provided lines to the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/path/to/file")
          content ["line 1" "line 2" "line 3"]]
      (Files/createDirectories (.getParent path) (u/->file-attributes-array []))

      (f/write-lines path content)

      (is (= content (Files/readAllLines path)))))

  (testing "uses the provided charset when writing"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/path/to/file")
          content ["line 1" "line 2" "line 3"]]
      (Files/createDirectories (.getParent path) (u/->file-attributes-array []))

      (f/write-lines path content :utf-16)

      (is (= content (Files/readAllLines path (StandardCharsets/UTF_16))))))

  (testing "uses the provided file options when writing"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          ^Path path (p/path test-file-system "/path/to/file")

          initial-content ["initial 1" "initial 2"]
          additional-content ["additional 1" "additional 2" "additional 3"]

          ^"[Ljava.nio.file.OpenOption;"
          default-options (u/->open-options-array [])]
      (Files/createDirectories (.getParent path) (u/->file-attributes-array []))
      (Files/write path initial-content default-options)

      (f/write-lines path additional-content :utf-8 :write :append)

      (is (= (concat initial-content additional-content)
            (Files/readAllLines path))))))

(deftest read-lines
  (testing "reads all lines from the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          ^Path path (p/path test-file-system "/path/to/file")
          content ["line 1" "line 2" "line 3"]

          ^"[Ljava.nio.file.OpenOption;"
          default-options (u/->open-options-array [])]
      (Files/createDirectories (.getParent path) (u/->file-attributes-array []))
      (Files/write path content default-options)

      (is (= content (f/read-all-lines path)))))

  (testing "reads using the supplied charset"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          ^Path path (p/path test-file-system "/path/to/file")
          content ["line 1" "line 2" "line 3"]

          ^"[Ljava.nio.file.OpenOption;"
          default-options (u/->open-options-array [])]
      (Files/createDirectories (.getParent path) (u/->file-attributes-array []))
      (Files/write path content StandardCharsets/UTF_16 default-options)

      (is (= content (f/read-all-lines path :utf-16))))))

(deftest new-input-stream
  (testing "returns an input stream for the provided path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["Line 1" "Line 2"]]
      (f/write-lines path content)

      (is (= "Line 1\nLine 2\n"
            (slurp (f/new-input-stream path)))))))

(deftest new-output-stream
  (testing "returns an output stream for the provided path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (spit (f/new-output-stream path) content)

      (is (= ["Line 1" "Line 2"]
            (f/read-all-lines path)))))

  (testing "allows open options to be provided"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          original-content ["Line 1" "Line 2"]
          new-content "Line 3\nLine 4\n"]
      (f/write-lines path original-content)

      (spit (f/new-output-stream path :append) new-content)

      (is (= ["Line 1" "Line 2" "Line 3" "Line 4"]
            (f/read-all-lines path))))))

(deftest populate-file-tree
  (testing "creates top level file with contents"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file {:content ["line 1" "line 2"]}]]
          path (p/path test-file-system "/file")]
      (f/populate-file-tree root-path definition)

      (is (f/regular-file? path))
      (is (= ["line 1" "line 2"]
            (f/read-all-lines path)))))

  (testing "creates top level files with content"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file1 {:content ["line 1" "line 2"]}]
           [:file2 {:content ["line 3" "line 4"]}]]
          path-1 (p/path test-file-system "/file1")
          path-2 (p/path test-file-system "/file2")]
      (f/populate-file-tree root-path definition)

      (is (f/regular-file? path-1))
      (is (= ["line 1" "line 2"]
            (f/read-all-lines path-1)))
      (is (f/regular-file? path-1))
      (is (= ["line 3" "line 4"]
            (f/read-all-lines path-2)))))

  (testing "creates top level symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file {:content ["line 1" "line 2"]}]
           [:symlink {:type :symbolic-link :target "/file"}]]
          file-path (p/path test-file-system "/file")
          symlink-path (p/path test-file-system "/symlink")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/symbolic-link? symlink-path)))
      (is (= file-path (f/read-symbolic-link symlink-path)))))

  (testing "creates top level symbolic links"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file1 {:content ["line 1" "line 2"]}]
           [:file2 {:content ["line 3" "line 4"]}]
           [:symlink1 {:type :symbolic-link :target "/file1"}]
           [:symlink2 {:type :symbolic-link :target "/file2"}]]
          file-1-path (p/path test-file-system "/file1")
          file-2-path (p/path test-file-system "/file2")
          symlink-1-path (p/path test-file-system "/symlink1")
          symlink-2-path (p/path test-file-system "/symlink2")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/symbolic-link? symlink-1-path)))
      (is (= file-1-path (f/read-symbolic-link symlink-1-path)))
      (is (true? (f/symbolic-link? symlink-2-path)))
      (is (= file-2-path (f/read-symbolic-link symlink-2-path)))))

  (testing "throws exception when no target supplied on symlink"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file {:content ["line 1" "line 2"]}]
           [:symlink {:type :symbolic-link}]]]
      (is (thrown? AssertionError
            (f/populate-file-tree root-path definition)))))

  (testing "creates top level link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file {:content ["line 1" "line 2"]}]
           [:link {:type :link :target "/file"}]]
          file-path (p/path test-file-system "/file")
          link-path (p/path test-file-system "/link")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/exists? link-path)))
      (is (true? (f/same-file? link-path file-path)))))

  (testing "creates top level links"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:file1 {:content ["line 1" "line 2"]}]
           [:file2 {:content ["line 3" "line 4"]}]
           [:link1 {:type :link :target "/file1"}]
           [:link2 {:type :link :target "/file2"}]]
          file-1-path (p/path test-file-system "/file1")
          file-2-path (p/path test-file-system "/file2")
          link-1-path (p/path test-file-system "/link1")
          link-2-path (p/path test-file-system "/link2")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/exists? link-1-path)))
      (is (true? (f/same-file? link-1-path file-1-path)))
      (is (true? (f/exists? link-2-path)))
      (is (true? (f/same-file? link-2-path file-2-path)))))

  (testing "creates top level directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some-directory {:type :directory}]]]
      (f/populate-file-tree root-path definition)

      (is (true? (f/directory?
                   (p/path test-file-system "/some-directory"))))))

  (testing "creates top level directories"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some-directory-1 {:type :directory}]
           [:some-directory-2 {:type :directory}]]]
      (f/populate-file-tree root-path definition)

      (is (true? (f/directory?
                   (p/path test-file-system "/some-directory-1"))))
      (is (true? (f/directory?
                   (p/path test-file-system "/some-directory-2"))))))

  (testing "creates nested file with contents"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some [:path [:to [:file {:content ["line 1" "line 2"]}]]]]]]
      (f/populate-file-tree root-path definition)

      (is (= ["line 1" "line 2"]
            (f/read-all-lines
              (p/path test-file-system "/some/path/to/file"))))))

  (testing "creates nested files with contents"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some [:path [:to
                          [:file-1 {:content ["line 1" "line 2"]}]
                          [:file-2 {:content ["line 3" "line 4"]}]]]]]]
      (f/populate-file-tree root-path definition)

      (is (= ["line 1" "line 2"]
            (f/read-all-lines
              (p/path test-file-system "/some/path/to/file-1"))))
      (is (= ["line 3" "line 4"]
            (f/read-all-lines
              (p/path test-file-system "/some/path/to/file-2"))))))

  (testing "creates nested symbolic link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some
            [:directory
             [:file {:content ["line 1" "line 2"]}]]
            [:other
             [:directory
              [:symlink {:type   :symbolic-link
                         :target "/some/directory/file"}]]]]]
          file-path (p/path test-file-system "/some/directory/file")
          symlink-path
          (p/path test-file-system "/some/other/directory/symlink")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/symbolic-link? symlink-path)))
      (is (= file-path (f/read-symbolic-link symlink-path)))))

  (testing "creates nested symbolic links"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some
            [:directory
             [:file1 {:content ["line 1" "line 2"]}]
             [:file2 {:content ["line 3" "line 4"]}]]
            [:other
             [:directory
              [:symlink1 {:type   :symbolic-link
                          :target "/some/directory/file1"}]
              [:symlink2 {:type   :symbolic-link
                          :target "/some/directory/file2"}]]]]]
          file-1-path (p/path test-file-system "/some/directory/file1")
          file-2-path (p/path test-file-system "/some/directory/file2")
          symlink-1-path
          (p/path test-file-system "/some/other/directory/symlink1")
          symlink-2-path
          (p/path test-file-system "/some/other/directory/symlink2")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/symbolic-link? symlink-1-path)))
      (is (= file-1-path (f/read-symbolic-link symlink-1-path)))
      (is (true? (f/symbolic-link? symlink-2-path)))
      (is (= file-2-path (f/read-symbolic-link symlink-2-path)))))

  (testing "creates nested link"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some
            [:directory
             [:file {:content ["line 1" "line 2"]}]]
            [:other
             [:directory
              [:link {:type   :link
                      :target "/some/directory/file"}]]]]]
          file-path (p/path test-file-system "/some/directory/file")
          link-path
          (p/path test-file-system "/some/other/directory/link")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/exists? link-path)))
      (is (true? (f/same-file? file-path link-path)))))

  (testing "creates nested links"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          definition
          [[:some
            [:directory
             [:file1 {:content ["line 1" "line 2"]}]
             [:file2 {:content ["line 3" "line 4"]}]]
            [:other
             [:directory
              [:link1 {:type   :link
                       :target "/some/directory/file1"}]
              [:link2 {:type   :link
                       :target "/some/directory/file2"}]]]]]
          file-1-path (p/path test-file-system "/some/directory/file1")
          file-2-path (p/path test-file-system "/some/directory/file2")
          link-1-path (p/path test-file-system "/some/other/directory/link1")
          link-2-path (p/path test-file-system "/some/other/directory/link2")]
      (f/populate-file-tree root-path definition)

      (is (true? (f/exists? link-1-path)))
      (is (true? (f/same-file? file-1-path link-1-path)))
      (is (true? (f/exists? link-2-path)))
      (is (true? (f/same-file? file-2-path link-2-path))))))

(deftest walk-file-tree
  (testing "walks top level files and returns accumulated result"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:file1 {:content ["Item 1"]}]
         [:file2 {:content ["Item 2"]}]
         [:file3 {:content ["Item 3"]}]])

      (let [result (f/walk-file-tree root-path
                     :visit-file-fn
                     (fn [accumulator path _]
                       {:control :continue
                        :result  (assoc accumulator
                                   (str path) (f/read-all-lines path))}))]
        (is (= {"/file1" ["Item 1"]
                "/file2" ["Item 2"]
                "/file3" ["Item 3"]}
              result)))))

  (testing "walks directories depth first"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [->visit-fn (fn [key]
                         (fn [accumulator path _]
                           {:control :continue
                            :result  (conj accumulator [key (str path)])}))
            result (f/walk-file-tree root-path
                     :initial-value []
                     :visit-file-fn (->visit-fn :file)
                     :pre-visit-directory-fn (->visit-fn :pre)
                     :post-visit-directory-fn (->visit-fn :post))]
        (is (= [[:pre "/"]
                [:pre "/directory1"]
                [:file "/directory1/file1"]
                [:file "/directory1/file2"]
                [:post "/directory1"]
                [:pre "/directory2"]
                [:file "/directory2/file3"]
                [:post "/directory2"]
                [:post "/"]]
              result)))))

  (testing "terminates when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [->visit-fn (fn [key]
                         (fn [accumulator path _]
                           {:control :continue
                            :result  (conj accumulator [key (str path)])}))
            result (f/walk-file-tree root-path
                     :initial-value []
                     :visit-file-fn (->visit-fn :file)
                     :pre-visit-directory-fn (->visit-fn :pre)
                     :post-visit-directory-fn
                     (fn [_ _ _] {:control :terminate}))]
        (is (= [[:pre "/"]
                [:pre "/directory1"]
                [:file "/directory1/file1"]
                [:file "/directory1/file2"]]
              result)))))

  (testing "skips entire subtree when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [->visit-fn (fn [key]
                         (fn [accumulator path _]
                           {:control :continue
                            :result  (conj accumulator [key (str path)])}))
            result (f/walk-file-tree root-path
                     :initial-value []
                     :visit-file-fn (->visit-fn :file)
                     :pre-visit-directory-fn
                     (fn [accumulator path _]
                       (if (= (str path) "/directory1")
                         {:control :skip-subtree}
                         {:control :continue
                          :result  (conj accumulator [:pre (str path)])}))
                     :post-visit-directory-fn (->visit-fn :post))]
        (is (= [[:pre "/"]
                [:pre "/directory2"]
                [:file "/directory2/file3"]
                [:post "/directory2"]
                [:post "/"]]
              result)))))

  (testing "skips all siblings when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [->visit-fn (fn [key]
                         (fn [accumulator path _]
                           {:control :continue
                            :result  (conj accumulator [key (str path)])}))
            result (f/walk-file-tree root-path
                     :initial-value []
                     :visit-file-fn
                     (fn [accumulator path _]
                       (if (= (str path) "/directory1/file1")
                         {:control :skip-siblings
                          :result  (conj accumulator [:file (str path)])}
                         {:control :continue
                          :result  (conj accumulator [:file (str path)])}))
                     :pre-visit-directory-fn (->visit-fn :pre)
                     :post-visit-directory-fn (->visit-fn :post))]
        (is (= [[:pre "/"]
                [:pre "/directory1"]
                [:file "/directory1/file1"]
                [:post "/directory1"]
                [:pre "/directory2"]
                [:file "/directory2/file3"]
                [:post "/directory2"]
                [:post "/"]]
              result)))))

  (testing "assumes continue when no control returned"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/delete-me")]
      (f/create-directory root-path)
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [delete-fn (fn [_ path _]
                        (f/delete path))
            result (f/walk-file-tree root-path
                     :visit-file-fn delete-fn
                     :post-visit-directory-fn delete-fn)]
        (is (nil? result))
        (is (false?
              (f/exists? (p/path test-file-system
                           "/delete-me/directory1"))))
        (is (false?
              (f/exists? (p/path test-file-system
                           "/delete-me/directory1/file1"))))
        (is (false?
              (f/exists? (p/path test-file-system
                           "/delete-me/directory1/file2"))))
        (is (false?
              (f/exists? (p/path test-file-system
                           "/delete-me/directory2"))))
        (is (false?
              (f/exists? (p/path test-file-system
                           "/delete-me/directory2/file3")))))))

  (testing "follows symlinks when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2 {:type :symbolic-link :target "/directory1"}]])

      (let [->visit-fn (fn [key]
                         (fn [accumulator path _]
                           {:control :continue
                            :result  (conj accumulator [key (str path)])}))
            result (f/walk-file-tree
                     (p/path test-file-system "/directory2")
                     :initial-value []
                     :file-visit-options [:follow-links]
                     :visit-file-fn (->visit-fn :file)
                     :pre-visit-directory-fn (->visit-fn :pre)
                     :post-visit-directory-fn (->visit-fn :post))]
        (is (= [[:pre "/directory2"]
                [:file "/directory2/file1"]
                [:file "/directory2/file2"]
                [:post "/directory2"]]
              result)))))

  ; TODO: test basic file attributes as map
  ; TODO: exception cases, visit file failed
  ; TODO: test maximum depth
  ; TODO: deal with pre-existing files and directories
  )

(deftest walk
  (testing "returns a seq over top level files"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:file1 {:content ["Item 1"]}]
         [:file2 {:content ["Item 2"]}]
         [:file3 {:content ["Item 3"]}]])

      (let [paths (f/walk root-path)]
        (is (= [(p/path root-path)
                (p/path root-path "file1")
                (p/path root-path "file2")
                (p/path root-path "file3")]
              paths)))))

  (testing "returns a seq over nested directories depth first"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (let [paths (f/walk root-path)]
        (is (= [(p/path root-path)
                (p/path root-path "directory1")
                (p/path root-path "directory1/file1")
                (p/path root-path "directory1/file2")
                (p/path root-path "directory2")
                (p/path root-path "directory2/file3")]
              paths)))))

  (testing "follows symlinks when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")]
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2 {:type :symbolic-link :target "/directory1"}]])

      (let [paths (f/walk (p/path test-file-system "/directory2")
                     :file-visit-options [:follow-links])]
        (is (= [(p/path root-path "/directory2")
                (p/path root-path "/directory2/file1")
                (p/path root-path "/directory2/file2")]
              paths)))))

  ; TODO: test maximum depth
  )

(deftest delete-recursively
  (testing "deletes all files in a directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/root")]
      (f/create-directory root-path)
      (f/populate-file-tree root-path
        [[:directory1
          [:file1 {:content ["Item 1"]}]
          [:file2 {:content ["Item 2"]}]]
         [:directory2
          [:file3 {:content ["Item 3"]}]]])

      (f/delete-recursively root-path)

      (is (false? (f/exists? root-path)))
      (is (false? (f/exists? (p/path root-path "/directory1"))))
      (is (false? (f/exists? (p/path root-path "/directory1/file1"))))
      (is (false? (f/exists? (p/path root-path "/directory1/file2"))))
      (is (false? (f/exists? (p/path root-path "/directory2"))))
      (is (false? (f/exists? (p/path root-path "/directory2/file3")))))))

; new-directory-stream
; new-byte-channel
; new-buffered-reader
; new-buffered-writer

; create-temp-file
; create-temp-directory

; file-store

; delete-if-exists
; move

; move-recursively
; copy-recursively

; hidden?
; readable?
; writeable?
; executable?

; ->posix-file-permissions
; ->posix-file-permissions-attribute
; ->posix-file-permissions-string
; read-attribute
; read-attributes
; set-attribute
; read-posix-file-permissions
; set-posix-file-permissions
; read-owner
; set-owner
; read-last-modified-time
; set-last-modified-time