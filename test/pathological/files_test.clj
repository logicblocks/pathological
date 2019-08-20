(ns pathological.files-test
  (:refer-clojure :exclude [find list])
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as io]
    [clojure.string :as string]

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
  (:import
    [java.util Arrays]
    [java.time Instant]
    [java.time.temporal ChronoUnit]
    [java.io ByteArrayOutputStream BufferedReader BufferedWriter]

    [java.nio.charset StandardCharsets Charset]
    [java.nio.file Files Path LinkOption NoSuchFileException]
    [java.nio.file.attribute AclFileAttributeView
     BasicFileAttributes
     BasicFileAttributeView
     DosFileAttributeView
     DosFileAttributes
     FileOwnerAttributeView
     PosixFileAttributes
     PosixFileAttributeView
     PosixFilePermissions
     UserDefinedFileAttributeView]))

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
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

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
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

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
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

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
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

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

(deftest create-temp-file
  (testing "creates a temporary file in the specified path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name)
            (unix-configuration)
            [[:temporary {:type :directory}]])

          prefix "pre-"
          suffix "-post"

          path (p/path test-file-system "/temporary")

          temp-path-1 (f/create-temp-file path prefix suffix)
          temp-path-2 (f/create-temp-file path prefix suffix)]
      (is (true? (f/exists? temp-path-1)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-1)) prefix)))
      (is (true? (string/ends-with? (str (p/file-name temp-path-1)) suffix)))

      (is (true? (f/exists? temp-path-2)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-2)) prefix)))
      (is (true? (string/ends-with? (str (p/file-name temp-path-2)) suffix)))

      (is (not= temp-path-1 temp-path-2))))

  (testing "applies the provided file attributes when using a path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name)
            (unix-configuration)
            [[:temporary {:type :directory}]])

          prefix "pre-"
          suffix "-post"

          path (p/path test-file-system "/temporary")

          temp-path (f/create-temp-file path prefix suffix
                      (u/->posix-file-permissions-attribute "rwxr--r--"))]
      (is (=
            #{:owner-read :owner-write :owner-execute :group-read :others-read}
            (f/read-posix-file-permissions temp-path)))))

  (testing
   "creates a temporary file in the default file system default location"
    (let [prefix "pre-"
          suffix "-post"

          temp-path-1 (f/create-temp-file prefix suffix)
          temp-path-2 (f/create-temp-file prefix suffix)]
      (is (true? (f/exists? temp-path-1)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-1)) prefix)))
      (is (true? (string/ends-with? (str (p/file-name temp-path-1)) suffix)))

      (is (true? (f/exists? temp-path-2)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-2)) prefix)))
      (is (true? (string/ends-with? (str (p/file-name temp-path-2)) suffix)))

      (is (not= temp-path-1 temp-path-2))))

  (testing "applies the provided file attributes when using default file system"
    (let [prefix "pre-"
          suffix "-post"

          temp-path (f/create-temp-file prefix suffix
                      (u/->posix-file-permissions-attribute "rwxr--r--"))]
      (is (=
            #{:owner-read :owner-write :owner-execute :group-read :others-read}
            (f/read-posix-file-permissions temp-path))))))

(deftest create-temp-directory
  (testing "creates a temporary directory in the specified path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name)
            (unix-configuration)
            [[:temporary {:type :directory}]])

          prefix "pre-"

          path (p/path test-file-system "/temporary")

          temp-path-1 (f/create-temp-directory path prefix)
          temp-path-2 (f/create-temp-directory path prefix)]
      (is (true? (f/exists? temp-path-1)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-1)) prefix)))

      (is (true? (f/exists? temp-path-2)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-2)) prefix)))

      (is (not= temp-path-1 temp-path-2))))

  (testing "applies the provided file attributes when using a path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name)
            (unix-configuration)
            [[:temporary {:type :directory}]])

          prefix "pre-"

          path (p/path test-file-system "/temporary")

          temp-path (f/create-temp-directory path prefix
                      (u/->posix-file-permissions-attribute "rwxr--r--"))]
      (is (=
            #{:owner-read :owner-write :owner-execute :group-read :others-read}
            (f/read-posix-file-permissions temp-path)))))

  (testing
   "creates a temporary directory in the default file system default location"
    (let [prefix "pre-"

          temp-path-1 (f/create-temp-directory prefix)
          temp-path-2 (f/create-temp-directory prefix)]
      (is (true? (f/exists? temp-path-1)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-1)) prefix)))

      (is (true? (f/exists? temp-path-2)))
      (is (true? (string/starts-with? (str (p/file-name temp-path-2)) prefix)))

      (is (not= temp-path-1 temp-path-2))))

  (testing "applies the provided file attributes when using default file system"
    (let [prefix "pre-"

          temp-path (f/create-temp-directory prefix
                      (u/->posix-file-permissions-attribute "rwxr--r--"))]
      (is (=
            #{:owner-read :owner-write :owner-execute :group-read :others-read}
            (f/read-posix-file-permissions temp-path))))))

(deftest read-symbolic-link
  (testing "returns the path of the link target"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          target-path (p/path test-file-system "/target")
          link-path (p/path test-file-system "/link")]
      (f/create-file target-path)
      (f/create-symbolic-link link-path target-path)

      (is (= target-path (f/read-symbolic-link link-path))))))

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

(deftest read-all-bytes
  (testing "reads all bytes from the file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/path")
          content ["Line 1" "Line 2"]]
      (f/write-lines path content)

      (is (= "Line 1\nLine 2\n"
            (String. ^bytes (f/read-all-bytes path)))))))

(deftest read-all-lines
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

(deftest lines
  (testing "returns a seq over all lines from path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["Line 1" "Line 2" "Line 3"]]
      (f/write-lines path content)

      (let [result (f/lines path)]
        (is (seq? result))
        (is (= ["Line 1" "Line 2" "Line 3"] result)))))

  (testing "uses charset of UTF-8 by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["\u4839" "\u3284"]]
      (f/write-lines path content :utf-8)

      (is (= ["\u4839" "\u3284"] (f/lines path)))))

  (testing "uses provided charset when specified"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["\u4839" "\u3284"]]
      (f/write-lines path content :utf-16be)

      (is (= ["\u4839" "\u3284"] (f/lines path :utf-16be))))))

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
               [:directory-2 {:type   :symbolic-link
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

(deftest list
  (testing "returns a seq over all entries in a directory"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          directory-path (p/path test-file-system "/directory-1")

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:path-1 {:content ["Line 1" "Line 2"]}]
                [:path-2 {:content ["Line 3" "Line 4"]}]
                [:path-3 {:content ["Line 5" "Line 6"]}]
                [:subdirectory-1
                 [:path-4 {:content ["Line 7" "Line 8"]}]
                 [:path-5 {:content ["Line 9" "Line 10"]}]]]])

          results (f/list directory-path)]
      (is (seq? results))
      (is (= [(p/path test-file-system
                "/directory-1/path-1")
              (p/path test-file-system
                "/directory-1/path-2")
              (p/path test-file-system
                "/directory-1/path-3")
              (p/path test-file-system
                "/directory-1/subdirectory-1")]
            results)))))

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

      (is (false? (f/exists? path)))))

  (testing "throws if a path does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (is (thrown? NoSuchFileException
            (f/delete path))))))

(deftest delete-if-exists
  (testing "deletes a file when it exists and returns true"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path)

      (is (true? (f/delete-if-exists path)))
      (is (false? (f/exists? path)))))

  (testing "deletes an empty directory when it exists and returns true"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-directory path)

      (is (true? (f/delete-if-exists path)))
      (is (false? (f/exists? path)))))

  (testing "returns false when path does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (is (false? (f/delete-if-exists path))))))

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
                    (u/->bytes (str (string/join "\n" content) "\n")))]
        (f/copy input-stream destination-path))

      (is (= content (f/read-all-lines destination-path)))))

  (testing "copies to an output stream"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")]
      (with-open [output-stream (ByteArrayOutputStream.)]
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
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

      (f/copy source-path target-path :copy-attributes)

      (is (= "rwxrw-rw-" (u/->posix-file-permissions-string
                           (f/read-posix-file-permissions target-path)))))))

(deftest move
  (testing "moves a file"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")
          destination-path (p/path test-file-system "/target")]
      (f/create-file source-path)
      (f/write-lines source-path content)

      (f/move source-path destination-path)

      (is (= content (f/read-all-lines destination-path)))
      (is (false? (f/exists? source-path)))))

  (testing "retains file attributes"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          source-path (p/path test-file-system "/source")
          destination-path (p/path test-file-system "/target")]
      (f/create-file source-path
        (u/->posix-file-permissions-attribute "rwxrw-rw-"))

      (f/move source-path destination-path)

      (is (= "rwxrw-rw-" (u/->posix-file-permissions-string
                           (f/read-posix-file-permissions destination-path))))
      (is (false? (f/exists? source-path)))))

  (testing "replaces existing file when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")
          destination-path (p/path test-file-system "/target")]
      (f/create-file source-path)
      (f/write-lines source-path content)

      (f/create-file destination-path)

      (f/move source-path destination-path :replace-existing)

      (is (= content (f/read-all-lines destination-path)))
      (is (false? (f/exists? source-path)))))

  (testing "uses an atomic move when requested"
    ; TODO: work out how to test this.

    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          content ["Line 1" "Line 2"]
          source-path (p/path test-file-system "/source")
          destination-path (p/path test-file-system "/target")]
      (f/create-file source-path)
      (f/write-lines source-path content)

      (f/move source-path destination-path :atomic-move)

      (is (= content (f/read-all-lines destination-path)))
      (is (false? (f/exists? source-path))))))

(deftest size
  (testing "returns the size of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")
          content ["Line 1" "Line 2"]]
      (f/write-lines path content)

      (is (= 14 (f/size path))))))

(deftest read-posix-file-permissions
  (testing "returns the posix file permissions on the provided path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path
        (u/->posix-file-permissions-attribute "rwxr--r--"))

      (is (= (u/<-posix-file-permissions-string "rwxr--r--")
            (f/read-posix-file-permissions path)))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")]
      (f/create-symbolic-link link target
        (u/->posix-file-permissions-attribute "r--------"))
      (f/create-file target
        (u/->posix-file-permissions-attribute "rwxr--r--"))

      (is (= #{:owner-read
               :owner-write
               :owner-execute
               :group-read
               :others-read}
            (f/read-posix-file-permissions link)))))

  (testing "does not follow symbolic links when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")]
      (f/create-symbolic-link link target
        (u/->posix-file-permissions-attribute "r--------"))
      (f/create-file target
        (u/->posix-file-permissions-attribute "rwxr--r--"))

      (is (= (u/<-posix-file-permissions-string "r--------")
            (f/read-posix-file-permissions link :no-follow-links))))))

(deftest set-posix-file-permissions
  (testing "sets posix file permissions on path using permissions set"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path
        (u/->posix-file-permissions-attribute #{:owner-read}))

      (f/set-posix-file-permissions path
        #{:owner-read :owner-write :owner-execute})

      (is (= #{:owner-read :owner-write :owner-execute}
            (f/read-posix-file-permissions path))))))

(deftest read-owner
  (testing "reads the owner of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path)

      (let [user-principal (f/read-owner path)]
        (is (= "user" (:name user-principal)))
        (is (= (Files/getOwner path (u/->link-options-array []))
              (:underlying user-principal))))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")]
      (f/create-file target)
      (f/create-symbolic-link link target)

      (Files/setOwner target (pr/->user-principal test-file-system "other"))

      (is (= "other" (:name (f/read-owner link))))))

  (testing "does not follow symbolic links when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")]
      (f/create-file target)
      (f/create-symbolic-link link target)

      (Files/setOwner target (pr/->user-principal test-file-system "other"))

      (is (= "user" (:name (f/read-owner link :no-follow-links)))))))

(deftest set-owner
  (testing "sets the owner of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")]
      (f/create-file path)

      (f/set-owner path (pr/->user-principal test-file-system "other"))

      (is (= "other" (:name (f/read-owner path))))))

  (testing "follows symbolic links"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")]
      (f/create-file target)
      (f/create-symbolic-link link target)

      (f/set-owner link (pr/->user-principal test-file-system "other"))

      (is (= "user" (:name (f/read-owner link :no-follow-links))))
      (is (= "other" (:name (f/read-owner link)))))))

(deftest read-last-modified-time
  (testing "returns the last modified time of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")

          before-instant (.truncatedTo (Instant/now) ChronoUnit/SECONDS)
          _ (f/create-file path)
          last-modified (f/read-last-modified-time path)
          after-instant (Instant/now)

          last-modified-instant (Instant/parse last-modified)]
      (is (true?
            (or (.equals last-modified-instant before-instant)
              (.isAfter last-modified-instant before-instant))))
      (is (true?
            (or (.equals last-modified-instant after-instant)
              (.isBefore last-modified-instant after-instant))))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")

          _ (f/create-symbolic-link link target)

          before-instant (.truncatedTo (Instant/now) ChronoUnit/SECONDS)
          _ (f/create-file target)
          last-modified (f/read-last-modified-time link)
          after-instant (Instant/now)

          last-modified-instant (Instant/parse last-modified)]
      (is (true?
            (or (.equals last-modified-instant before-instant)
              (.isAfter last-modified-instant before-instant))))
      (is (true?
            (or (.equals last-modified-instant after-instant)
              (.isBefore last-modified-instant after-instant))))))

  (testing "does not follow symbolic links when required"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link (p/path test-file-system "/link")
          target (p/path test-file-system "/target")

          before-instant (.truncatedTo (Instant/now) ChronoUnit/SECONDS)
          _ (f/create-symbolic-link link target)
          after-instant (Instant/now)

          _ (f/create-file target)
          last-modified (f/read-last-modified-time link :no-follow-links)
          last-modified-instant (Instant/parse last-modified)]
      (is (true?
            (or (.equals last-modified-instant before-instant)
              (.isAfter last-modified-instant before-instant))))
      (is (true?
            (or (.equals last-modified-instant after-instant)
              (.isBefore last-modified-instant after-instant)))))))

(deftest set-last-modified-time
  (testing "sets the last modified time of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file")
          last-modified "2019-05-04T22:10:10Z"]
      (f/create-file path)
      (f/set-last-modified-time path last-modified)

      (is (= last-modified
            (f/read-last-modified-time path))))))

(deftest read-file-attribute-view
  ; TODO: test link options

  (testing "returns a view over basic file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          ^BasicFileAttributeView
          underlying-view (Files/getFileAttributeView
                            path BasicFileAttributeView
                            (u/->link-options-array []))
          ^BasicFileAttributes
          underlying-attributes (.readAttributes underlying-view)

          attributes (f/read-file-attribute-view path :basic)]
      (is (= (a/map->BasicFileAttributeView
               {:path               path
                :file-key           (.fileKey underlying-attributes)
                :size               (.size underlying-attributes)
                :last-modified-time (str (.lastModifiedTime
                                           underlying-attributes))
                :last-access-time   (str (.lastAccessTime
                                           underlying-attributes))
                :creation-time      (str (.creationTime
                                           underlying-attributes))
                :regular-file?      (.isRegularFile underlying-attributes)
                :directory?         (.isDirectory underlying-attributes)
                :symbolic-link?     (.isSymbolicLink underlying-attributes)
                :other?             (.isOther underlying-attributes)})
            (assoc attributes :delegate nil)))
      (is (instance? BasicFileAttributeView
            (:delegate attributes)))))

  (testing "returns a view over owner file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          ^FileOwnerAttributeView
          underlying-view (Files/getFileAttributeView
                            path FileOwnerAttributeView
                            (u/->link-options-array []))

          attributes (f/read-file-attribute-view path :owner)]
      (is (= (a/map->OwnerFileAttributeView
               {:path  path
                :owner (pr/<-user-principal (.getOwner underlying-view))})
            (assoc attributes :delegate nil)))
      (is (instance? FileOwnerAttributeView
            (:delegate attributes)))))

  (testing "returns a view over posix file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          _ (f/create-file path)

          ^PosixFileAttributeView
          underlying-view (Files/getFileAttributeView
                            path PosixFileAttributeView
                            (u/->link-options-array []))

          ^PosixFileAttributes
          underlying-attributes (.readAttributes underlying-view)

          attributes (f/read-file-attribute-view path :posix)]
      (is (= (a/map->PosixFileAttributeView
               {:path               path
                :file-key           (.fileKey underlying-attributes)
                :size               (.size underlying-attributes)
                :owner              (pr/<-user-principal
                                      (.owner underlying-attributes))
                :group              (pr/<-group-principal
                                      (.group underlying-attributes))
                :permissions        (into #{}
                                      (map u/<-posix-file-permission
                                        (.permissions underlying-attributes)))
                :last-modified-time (u/<-file-time
                                      (.lastModifiedTime
                                        underlying-attributes))
                :last-access-time   (u/<-file-time
                                      (.lastAccessTime
                                        underlying-attributes))
                :creation-time      (u/<-file-time
                                      (.creationTime
                                        underlying-attributes))
                :regular-file?      (.isRegularFile underlying-attributes)
                :directory?         (.isDirectory underlying-attributes)
                :symbolic-link?     (.isSymbolicLink underlying-attributes)
                :other?             (.isOther underlying-attributes)})
            (assoc attributes :delegate nil)))
      (is (instance? PosixFileAttributeView
            (:delegate attributes)))))

  (testing "returns a view over dos file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C://file")

          _ (f/create-file path)

          ^DosFileAttributeView
          underlying-view (Files/getFileAttributeView
                            path DosFileAttributeView
                            (u/->link-options-array []))

          ^DosFileAttributes
          underlying-attributes (.readAttributes underlying-view)

          attributes (f/read-file-attribute-view path :dos)]
      (is (= (a/map->DosFileAttributeView
               {:path               path
                :file-key           (.fileKey underlying-attributes)
                :size               (.size underlying-attributes)
                :last-modified-time (u/<-file-time
                                      (.lastModifiedTime
                                        underlying-attributes))
                :last-access-time   (u/<-file-time
                                      (.lastAccessTime
                                        underlying-attributes))
                :creation-time      (u/<-file-time
                                      (.creationTime
                                        underlying-attributes))
                :regular-file?      (.isRegularFile underlying-attributes)
                :directory?         (.isDirectory underlying-attributes)
                :symbolic-link?     (.isSymbolicLink underlying-attributes)
                :other?             (.isOther underlying-attributes)
                :read-only?         (.isReadOnly underlying-attributes)
                :hidden?            (.isHidden underlying-attributes)
                :archive?           (.isArchive underlying-attributes)
                :system?            (.isSystem underlying-attributes)})
            (assoc attributes :delegate nil)))
      (is (instance? DosFileAttributeView
            (:delegate attributes)))))

  (testing "returns a view over user file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          ^bytes important-thing-1 (u/->bytes "123456")
          ^bytes important-thing-2 (u/->bytes "789012")

          _ (f/create-file path)
          _ (f/set-attribute path "user:important.thing1" important-thing-1)
          _ (f/set-attribute path "user:important.thing2" important-thing-2)

          attributes (f/read-file-attribute-view path :user)]
      (is (Arrays/equals important-thing-1
            ^bytes (get-in attributes [:attributes "important.thing1"])))
      (is (Arrays/equals important-thing-2
            ^bytes (get-in attributes [:attributes "important.thing2"])))
      (is (= path (:path attributes)))
      (is (instance? UserDefinedFileAttributeView
            (:delegate attributes)))))

  (testing "returns a view over acl file attributes"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/directory")
          user (pr/->user-principal test-file-system "some-user")

          _ (f/create-file path)
          _ (f/set-attribute path "acl:acl"
              [(u/->acl-entry
                 {:type        :allow
                  :principal   user
                  :permissions #{:read-attributes :write-attributes}})
               (u/->acl-entry
                 {:type        :deny
                  :principal   user
                  :permissions #{:delete}
                  :flags       #{:file-inherit :directory-inherit}})])

          ^AclFileAttributeView
          underlying-view (Files/getFileAttributeView
                            path AclFileAttributeView
                            (u/->link-options-array []))

          attributes (f/read-file-attribute-view path :acl)]
      (is (= (a/map->AclFileAttributeView
               {:path  path
                :owner (pr/<-user-principal
                         (.getOwner underlying-view))
                :acl   [{:type        :allow
                         :principal   user
                         :permissions #{:read-attributes :write-attributes}
                         :flags       #{}}
                        {:type        :deny
                         :principal   user
                         :permissions #{:delete}
                         :flags       #{:file-inherit :directory-inherit}}]})
            (assoc attributes :delegate nil)))
      (is (instance? AclFileAttributeView
            (:delegate attributes))))))

(deftest read-attribute
  ; TODO: test link options

  (testing "gets string user defined attribute from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value "important-value"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (Arrays/equals
            ^bytes (u/->bytes value)
            ^bytes (f/read-attribute path attribute-spec)))))

  (testing "gets bytes user defined attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value (u/->bytes "important-value" :utf-16be)]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (Arrays/equals
            ^bytes value
            ^bytes (f/read-attribute path attribute-spec)))))

  (testing "gets byte buffer user defined attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value (u/->byte-buffer "important-value" :utf-16be)]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (Arrays/equals
            ^bytes (u/<-byte-buffer value)
            ^bytes (f/read-attribute path attribute-spec)))))

  (testing "gets file time basic attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          attribute-spec "basic:creationTime"
          value (u/->file-time "2019-05-04T22:10:10Z")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= "2019-05-04T22:10:10Z"
            (f/read-attribute path attribute-spec)))))

  (testing "gets timestamp string basic attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          attribute-spec "basic:lastAccessTime"
          value "2019-05-04T22:10:10Z"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= "2019-05-04T22:10:10Z"
            (f/read-attribute path attribute-spec)))))

  (testing "gets user principal owner attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          attribute-spec "owner:owner"
          value (pr/->user-principal test-file-system "some-user")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value (f/read-attribute path attribute-spec)))))

  (testing "gets group principal posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:group"
          value (pr/->group-principal test-file-system "some-group")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value (f/read-attribute path attribute-spec)))))

  (testing "gets java permission set posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value (u/->posix-file-permissions "rwxr--r--")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/<-posix-file-permissions-string "rwxr--r--")
            (f/read-attribute path attribute-spec)))))

  (testing "gets keyword permission set posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value #{:owner-read
                  :owner-write
                  :group-read
                  :group-write
                  :others-read}]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value (f/read-attribute path attribute-spec)))))

  (testing "gets string permissions posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value "rwxr--r--"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/<-posix-file-permissions-string "rwxr--r--")
            (f/read-attribute path attribute-spec)))))

  (testing "gets acl attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")
          user (pr/->user-principal test-file-system "some-user")

          attribute-spec "acl:acl"
          value [(u/->acl-entry
                   {:type        :allow
                    :principal   user
                    :permissions #{:read-attributes :write-attributes}})
                 (u/->acl-entry
                   {:type        :deny
                    :principal   user
                    :permissions #{:delete}
                    :flags       #{:file-inherit :directory-inherit}})]]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= [{:type        :allow
               :principal   user
               :permissions #{:read-attributes :write-attributes}
               :flags       #{}}
              {:type        :deny
               :principal   user
               :permissions #{:delete}
               :flags       #{:file-inherit :directory-inherit}}]
            (f/read-attribute path attribute-spec)))))

  (testing "gets boolean dos attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          attribute-spec "dos:hidden"
          value true]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value (f/read-attribute path attribute-spec))))))

(deftest read-attributes
  ; TODO: test link options

  (testing "gets user defined attributes from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attributes-spec "user:custom1,custom2"

          attribute-1-spec "user:custom1"
          value-1 "important-value-1"

          attribute-2-spec "user:custom2"
          value-2 "important-value-2"

          _ (f/create-file path)

          _ (f/set-attribute path attribute-1-spec value-1)
          _ (f/set-attribute path attribute-2-spec value-2)

          attributes (f/read-attributes path attributes-spec)]
      (is (Arrays/equals
            ^bytes (u/->bytes value-1)
            ^bytes (:custom1 attributes)))
      (is (Arrays/equals
            ^bytes (u/->bytes value-2)
            ^bytes (:custom2 attributes)))))

  (testing "gets basic attributes from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          attributes-spec "creationTime,lastAccessTime"

          attribute-1-spec "basic:creationTime"
          value-1 (u/->file-time "2019-05-04T22:10:10Z")

          attribute-2-spec "basic:lastAccessTime"
          value-2 (u/->file-time "2019-05-04T22:11:00Z")

          _ (f/create-file path)

          _ (f/set-attribute path attribute-1-spec value-1)
          _ (f/set-attribute path attribute-2-spec value-2)

          attributes (f/read-attributes path attributes-spec)]
      (is (= "2019-05-04T22:10:10Z"
            (:creation-time attributes)))
      (is (= "2019-05-04T22:11:00Z"
            (:last-access-time attributes)))))

  (testing "gets owner attribute from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          attributes-spec "owner:owner"

          attribute-spec "owner:owner"
          value (pr/->user-principal test-file-system "some-user")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= {:owner value} (f/read-attributes path attributes-spec)))))

  (testing "gets posix attributes from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attributes-spec "posix:group,permissions"

          attribute-1-spec "posix:group"
          value-1 (pr/->group-principal test-file-system "some-group")

          attribute-2-spec "posix:permissions"
          value-2 (u/->posix-file-permissions "rwxr--r--")

          _ (f/create-file path)

          _ (f/set-attribute path attribute-1-spec value-1)
          _ (f/set-attribute path attribute-2-spec value-2)]
      (is (= {:group       value-1
              :permissions (u/<-posix-file-permissions-string "rwxr--r--")}
            (f/read-attributes path attributes-spec)))))

  (testing "gets acl attributes from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")
          user (pr/->user-principal test-file-system "some-user")

          attributes-spec "acl:*"

          attribute-1-spec "acl:acl"
          value-1 [(u/->acl-entry
                     {:type        :allow
                      :principal   user
                      :permissions #{:read-attributes :write-attributes}})
                   (u/->acl-entry
                     {:type        :deny
                      :principal   user
                      :permissions #{:delete}
                      :flags       #{:file-inherit :directory-inherit}})]

          attribute-2-spec "acl:owner"
          value-2 (pr/->user-principal test-file-system "some-user")

          _ (f/create-file path)

          _ (f/set-attribute path attribute-1-spec value-1)
          _ (f/set-attribute path attribute-2-spec value-2)

          attributes (f/read-attributes path attributes-spec)]
      (is (= {:acl   [{:type        :allow
                       :principal   user
                       :permissions #{:read-attributes :write-attributes}
                       :flags       #{}}
                      {:type        :deny
                       :principal   user
                       :permissions #{:delete}
                       :flags       #{:file-inherit :directory-inherit}}]
              :owner value-2}
            attributes))))

  (testing "bets dos attributes from the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          attributes-spec "dos:hidden,system"

          attribute-1-spec "dos:hidden"
          value-1 true

          attribute-2-spec "dos:system"
          value-2 false

          _ (f/create-file path)

          _ (f/set-attribute path attribute-1-spec value-1)
          _ (f/set-attribute path attribute-2-spec value-2)

          attributes (f/read-attributes path attributes-spec)]
      (is (= {:hidden true :system false}
            attributes)))))

(deftest set-attribute
  ; TODO: test link options

  (testing "sets string user defined attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value "important-value"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value
            (String. ^bytes (Files/getAttribute path attribute-spec
                              (u/->link-options-array [])))))))

  (testing "sets bytes user defined attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value (u/->bytes "important-value" :utf-16be)]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (=
            (String. ^bytes value ^Charset (u/->charset :utf-16be))
            (String.
              ^bytes (Files/getAttribute path attribute-spec
                       (u/->link-options-array []))
              ^Charset (u/->charset :utf-16be))))))

  (testing "sets byte buffer user defined attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:user}))

          path (p/path test-file-system "/file")

          attribute-spec "user:custom"
          value (u/->byte-buffer "important-value" :utf-16be)]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (=
            (String.
              ^bytes (u/<-byte-buffer value)
              ^Charset (u/->charset :utf-16be))
            (String.
              ^bytes (Files/getAttribute path attribute-spec
                       (u/->link-options-array []))
              ^Charset (u/->charset :utf-16be))))))

  (testing "sets file time basic attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          attribute-spec "basic:creationTime"
          value (u/->file-time "2019-05-04T22:10:10Z")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/->file-time "2019-05-04T22:10:10Z")
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets timestamp string basic attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:basic}))

          path (p/path test-file-system "/file")

          attribute-spec "basic:lastAccessTime"
          value "2019-05-04T22:10:10Z"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/->file-time "2019-05-04T22:10:10Z")
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets user principal owner attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:owner}))

          path (p/path test-file-system "/file")

          attribute-spec "owner:owner"
          value (pr/->user-principal test-file-system "some-user")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (:underlying value)
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets group principal posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:group"
          value (pr/->group-principal test-file-system "some-group")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (:underlying value)
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets java permission set posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value (u/->posix-file-permissions "rwxr--r--")]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets keyword permission set posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value #{:owner-read
                  :owner-write
                  :group-read
                  :group-write
                  :others-read}]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/->posix-file-permissions value)
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets string permissions posix attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:posix}))

          path (p/path test-file-system "/file")

          attribute-spec "posix:permissions"
          value "rwxr--r--"]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= (u/->posix-file-permissions value)
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets acl attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (unix-configuration
              :attribute-views #{:acl}))

          path (p/path test-file-system "/file")
          user (pr/->user-principal test-file-system "some-user")

          attribute-spec "acl:acl"
          value [(u/->acl-entry
                   {:type        :allow
                    :principal   user
                    :permissions #{:read-attributes :write-attributes}})
                 (u/->acl-entry
                   {:type        :deny
                    :principal   user
                    :permissions #{:delete}
                    :flags       #{:file-inherit :directory-inherit}})]]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value
            (Files/getAttribute path attribute-spec
              (u/->link-options-array []))))))

  (testing "sets boolean dos attribute on the path"
    (let [test-file-system
          (new-in-memory-file-system
            (random-file-system-name)
            (windows-configuration
              :attribute-views #{:dos}))

          path (p/path test-file-system "C:\\file")

          attribute-spec "dos:hidden"
          value true]
      (f/create-file path)

      (f/set-attribute path attribute-spec value)

      (is (= value
            (Files/getAttribute path attribute-spec
              (u/->link-options-array [])))))))

(deftest probe-content-type
  (testing "returns the content type of the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.html")
          content ["<html></html>"]]
      (f/write-lines path content)

      (is (= "text/html"
            (f/probe-content-type path))))))

(deftest exists?
  ; TODO: test for symbolic link handling

  (testing "returns true when the path exists"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (true? (f/exists? path)))))

  (testing "returns false when the file does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (is (false? (f/exists? path)))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link1 (p/path test-file-system "/link1")
          link2 (p/path test-file-system "/link2")
          target1 (p/path test-file-system "/target1")
          target2 (p/path test-file-system "/target2")]
      (f/create-file target1)
      (f/create-symbolic-link link1 target1)
      (f/create-symbolic-link link2 target2)

      (is (true? (f/exists? link1)))
      (is (false? (f/exists? link2)))))

  (testing "does not follow symbolic links when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link1 (p/path test-file-system "/link1")
          link2 (p/path test-file-system "/link2")
          target1 (p/path test-file-system "/target1")]
      (f/create-symbolic-link link1 target1)

      (is (true? (f/exists? link1 :no-follow-links)))
      (is (false? (f/exists? link2 :no-follow-links))))))

(deftest not-exists?
  (testing "returns true when the path does not exist"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (is (true? (f/not-exists? path)))))

  (testing "returns false when the path exists"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/some-file")]
      (f/create-file path)

      (is (false? (f/not-exists? path)))))

  (testing "follows symbolic links by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link1 (p/path test-file-system "/link1")
          link2 (p/path test-file-system "/link2")
          target1 (p/path test-file-system "/target1")
          target2 (p/path test-file-system "/target2")]
      (f/create-file target1)
      (f/create-symbolic-link link1 target1)
      (f/create-symbolic-link link2 target2)

      (is (false? (f/not-exists? link1)))
      (is (true? (f/not-exists? link2)))))

  (testing "does not follow symbolic links when requested"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          link1 (p/path test-file-system "/link1")
          link2 (p/path test-file-system "/link2")
          target1 (p/path test-file-system "/target1")]
      (f/create-symbolic-link link1 target1)

      (is (false? (f/not-exists? link1 :no-follow-links)))
      (is (true? (f/not-exists? link2 :no-follow-links))))))

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

(deftest readable?
  (testing "returns true when path is readable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (f/create-file path)

      (is (true? (f/readable? path)))))

  (testing "returns false when path is not readable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (is (false? (f/readable? path))))))

(deftest writable?
  (testing "returns true when path is writable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (f/create-file path)

      (is (true? (f/writable? path)))))

  (testing "returns false when path is not writable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (is (false? (f/writable? path))))))

(deftest executable?
  (testing "returns true when path is executable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (f/create-file path)

      (is (true? (f/executable? path)))))

  (testing "returns false when path is not executable"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")]
      (is (false? (f/executable? path))))))

(deftest new-directory-stream
  (testing "returns a directory stream for the provided path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          directory-path (p/path test-file-system "/directory-1")

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:path-1 {:content ["Line 1" "Line 2"]}]
                [:path-2 {:content ["Line 3" "Line 4"]}]
                [:path-3 {:content ["Line 5" "Line 6"]}]
                [:subdirectory-1
                 [:path-4 {:content ["Line 7" "Line 8"]}]
                 [:path-5 {:content ["Line 9" "Line 10"]}]]]])]
      (with-open [directory-stream (f/new-directory-stream directory-path)]
        (is (= [(p/path test-file-system "/directory-1/path-1")
                (p/path test-file-system "/directory-1/path-2")
                (p/path test-file-system "/directory-1/path-3")
                (p/path test-file-system "/directory-1/subdirectory-1")]
              (seq directory-stream))))))

  (testing (str "returns a directory stream for the provided path "
             "filtered using the supplied glob")
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          directory-path (p/path test-file-system "/directory-1")

          glob "path*"

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:path-1 {:content ["Line 1" "Line 2"]}]
                [:path-2 {:content ["Line 3" "Line 4"]}]
                [:path-3 {:content ["Line 5" "Line 6"]}]
                [:subdirectory-1
                 [:path-4 {:content ["Line 7" "Line 8"]}]
                 [:path-5 {:content ["Line 9" "Line 10"]}]]]])]
      (with-open [directory-stream
                  (f/new-directory-stream directory-path glob)]
        (is (= [(p/path test-file-system "/directory-1/path-1")
                (p/path test-file-system "/directory-1/path-2")
                (p/path test-file-system "/directory-1/path-3")]
              (seq directory-stream))))))

  (testing (str "returns a directory stream for the provided path "
             "filtered using the supplied function")
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/")
          directory-path (p/path test-file-system "/directory-1")

          filter (fn [path]
                   (or
                     (= (p/path test-file-system "/directory-1/path-1") path)
                     (= (p/path test-file-system "/directory-1/path-3") path)))

          _ (f/populate-file-tree root-path
              [[:directory-1
                [:path-1 {:content ["Line 1" "Line 2"]}]
                [:path-2 {:content ["Line 3" "Line 4"]}]
                [:path-3 {:content ["Line 5" "Line 6"]}]
                [:subdirectory-1
                 [:path-4 {:content ["Line 7" "Line 8"]}]
                 [:path-5 {:content ["Line 9" "Line 10"]}]]]])]
      (with-open [directory-stream
                  (f/new-directory-stream directory-path filter)]
        (is (= [(p/path test-file-system "/directory-1/path-1")
                (p/path test-file-system "/directory-1/path-3")]
              (seq directory-stream)))))))

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

(deftest new-buffered-reader
  (testing "returns a new buffered reader over the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (spit path content)

      (with-open [reader (f/new-buffered-reader path)]
        (is (instance? BufferedReader reader))
        (is (= content (slurp reader))))))

  (testing "uses charset of UTF-8 by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["\u4839" "\u3284"]]
      (f/write-lines path content :utf-8)

      (with-open [reader (f/new-buffered-reader path)]
        (is (= "\u4839\n\u3284\n" (slurp reader))))))

  (testing "uses provided charset when specified"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content ["\u4839" "\u3284"]]
      (f/write-lines path content :utf-16be)

      (with-open [reader (f/new-buffered-reader path :utf-16be)]
        (is (= "\u4839\n\u3284\n" (slurp reader)))))))

(deftest new-buffered-writer
  (testing "returns a new buffered writer over the path"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (with-open [writer (f/new-buffered-writer path)]
        (is (instance? BufferedWriter writer))
        (spit writer content))

      (is (= ["Line 1" "Line 2"]
            (f/read-all-lines path)))))

  (testing "uses charset of UTF-8 by default"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (with-open [writer (f/new-buffered-writer path)]
        (spit writer content))

      (is (= ["Line 1" "Line 2"]
            (f/read-all-lines path :utf-8)))))

  (testing "uses provided charset when specified"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          content "Line 1\nLine 2\n"]
      (with-open [writer (f/new-buffered-writer path :utf-16be)]
        (spit writer content))

      (is (= ["Line 1" "Line 2"]
            (f/read-all-lines path :utf-16be)))))

  (testing "supports open options"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          initial-content ["Line 1" "Line 2"]
          additional-content "Line 3\nLine 4\n"]
      (f/write-lines path initial-content)
      (with-open [writer (f/new-buffered-writer path :append)]
        (spit writer additional-content))

      (is (= ["Line 1" "Line 2" "Line 3" "Line 4"]
            (f/read-all-lines path)))))

  (testing "supports charset and open options"
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          path (p/path test-file-system "/file.txt")
          initial-content ["Line 1" "Line 2"]
          additional-content "Line 3\nLine 4\n"]
      (f/write-lines path initial-content :utf-16be)
      (with-open [writer (f/new-buffered-writer path :utf-16be :append)]
        (spit writer additional-content))

      (is (= ["Line 1" "Line 2" "Line 3" "Line 4"]
            (f/read-all-lines path :utf-16be))))))

(deftest walk
  ; TODO: test maximum depth

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
              paths))))))

(deftest walk-file-tree
  ; TODO: test basic file attributes as map
  ; TODO: exception cases, visit file failed
  ; TODO: test maximum depth
  ; TODO: deal with pre-existing files and directories

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
              result))))))

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

(deftest delete-recursively
  ; TODO: what should happen when one delete fails?

  (testing "recursively deletes all files/directories in a path"
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

(deftest copy-recursively
  ; TODO: What should happen when one copy fails?
  ; TODO: Test copy options

  (testing (str
             "recursively copies all files/directories in a source path to a "
             "destination path")
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/root")
          source-path (p/path root-path "source")
          target-path (p/path root-path "target")]
      (f/create-directory root-path)
      (f/populate-file-tree root-path
        [[:source
          [:directory1
           [:file1 {:content ["Item 1"]}]
           [:file2 {:content ["Item 2"]}]]
          [:directory2
           [:file3 {:content ["Item 3"]}]]]])

      (f/copy-recursively source-path target-path)

      (is (true? (f/exists? source-path)))
      (is (true? (f/exists? (p/path source-path "/directory1"))))
      (is (true? (f/exists? (p/path source-path "/directory1/file1"))))
      (is (true? (f/exists? (p/path source-path "/directory1/file2"))))
      (is (true? (f/exists? (p/path source-path "/directory2"))))
      (is (true? (f/exists? (p/path source-path "/directory2/file3"))))

      (is (true? (f/exists? target-path)))
      (is (true? (f/exists? (p/path target-path "/directory1"))))
      (is (true? (f/exists? (p/path target-path "/directory1/file1"))))
      (is (true? (f/exists? (p/path target-path "/directory1/file2"))))
      (is (true? (f/exists? (p/path target-path "/directory2"))))
      (is (true? (f/exists? (p/path target-path "/directory2/file3"))))
      (is (= ["Item 1"]
            (f/read-all-lines (p/path target-path "/directory1/file1"))))
      (is (= ["Item 2"]
            (f/read-all-lines (p/path target-path "/directory1/file2"))))
      (is (= ["Item 3"]
            (f/read-all-lines (p/path target-path "/directory2/file3")))))))

(deftest move-recursively
  ; TODO: What should happen when one move fails?
  ; TODO: Test copy options

  (testing (str
             "recursively moves all files/directories in a source path to "
             "a destination path")
    (let [test-file-system
          (new-in-memory-file-system (random-file-system-name))

          root-path (p/path test-file-system "/root")
          source-path (p/path root-path "source")
          target-path (p/path root-path "target")]
      (f/create-directory root-path)
      (f/populate-file-tree root-path
        [[:source
          [:directory1
           [:file1 {:content ["Item 1"]}]
           [:file2 {:content ["Item 2"]}]]
          [:directory2
           [:file3 {:content ["Item 3"]}]]]])

      (f/move-recursively source-path target-path)

      (is (false? (f/exists? source-path)))
      (is (false? (f/exists? (p/path source-path "/directory1"))))
      (is (false? (f/exists? (p/path source-path "/directory1/file1"))))
      (is (false? (f/exists? (p/path source-path "/directory1/file2"))))
      (is (false? (f/exists? (p/path source-path "/directory2"))))
      (is (false? (f/exists? (p/path source-path "/directory2/file3"))))

      (is (true? (f/exists? target-path)))
      (is (true? (f/exists? (p/path target-path "/directory1"))))
      (is (true? (f/exists? (p/path target-path "/directory1/file1"))))
      (is (true? (f/exists? (p/path target-path "/directory1/file2"))))
      (is (true? (f/exists? (p/path target-path "/directory2"))))
      (is (true? (f/exists? (p/path target-path "/directory2/file3"))))
      (is (= ["Item 1"]
            (f/read-all-lines (p/path target-path "/directory1/file1"))))
      (is (= ["Item 2"]
            (f/read-all-lines (p/path target-path "/directory1/file2"))))
      (is (= ["Item 3"]
            (f/read-all-lines (p/path target-path "/directory2/file3")))))))

; new-byte-channel
