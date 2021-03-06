(ns pathological.testing-test
  (:refer-clojure :exclude [class name])
  (:require
   [clojure.test :refer :all]

   [pathological.testing :as t]
   [pathological.paths :as p]
   [pathological.utils :as u]
   [pathological.principals :as pr]
   [pathological.file-systems :as fs]

   [pathological.support.jimfs :as jimfs]
   [pathological.support.predicates :as predicates]
   [pathological.files :as f])
  (:import
   [com.google.common.jimfs Feature
    PathNormalization
    PathType]
   [java.io IOException]))

(declare thrown?)

(deftest builds-default-unix-filesystem-with-random-name
  (let [file-system (t/new-unix-in-memory-file-system)]
    (is (predicates/uuid-string? (jimfs/name file-system)))
    (is (not=
          (jimfs/name file-system)
          (jimfs/name (t/new-unix-in-memory-file-system))))
    (is (= #{(p/path file-system "/")}
          (fs/root-directories file-system)))
    (is (= #{:basic :owner :posix :unix}
          (fs/supported-file-attribute-views file-system)))
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS
             Feature/SECURE_DIRECTORY_STREAM
             Feature/FILE_CHANNEL}
          (jimfs/supported-features file-system)))
    (is (= "/" (str (jimfs/working-directory file-system))))
    (is (= #{} (jimfs/name-display-normalization file-system)))
    (is (= #{} (jimfs/name-canonical-normalization file-system)))
    (is (false? (jimfs/path-equality-uses-canonical-form? file-system)))
    (is (= 8192 (jimfs/block-size file-system)))
    (is (= 4294967296 (jimfs/max-size file-system)))
    (is (= 4294967296 (jimfs/max-cache-size file-system)))
    (is (= [(u/->file-attribute "owner:owner"
              (pr/->user-principal file-system "user"))
            (u/->file-attribute "posix:group"
              (pr/->group-principal file-system "group"))
            (u/->file-attribute "posix:permissions" "rw-r--r--")]
          (jimfs/default-attribute-values file-system)))))

(deftest builds-unix-filesystem-with-specified-name
  (let [file-system-name (t/random-file-system-name)
        file-system (t/new-unix-in-memory-file-system
                      :name file-system-name)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= file-system-name (jimfs/name file-system)))))

(deftest builds-unix-filesystem-with-specified-working-directory
  (let [file-system (t/new-unix-in-memory-file-system
                      :working-directory "/work")]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= "/work" (str (jimfs/working-directory file-system))))))

(deftest builds-unix-filesystem-with-specified-attribute-views
  (let [file-system (t/new-unix-in-memory-file-system
                      :attribute-views #{:basic :acl :owner})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{:basic :acl :owner}
          (fs/supported-file-attribute-views file-system)))))

(deftest builds-unix-filesystem-with-specified-features
  (let [file-system (t/new-unix-in-memory-file-system
                      :features #{:links :symbolic-links})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS}
          (jimfs/supported-features file-system)))))

(deftest builds-unix-filesystem-with-specified-block-size
  (let [file-system (t/new-unix-in-memory-file-system
                      :block-size 4096)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 4096
          (jimfs/block-size file-system)))))

(deftest builds-unix-filesystem-with-specified-max-size
  (let [file-system (t/new-unix-in-memory-file-system
                      :max-size 2147483648)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-size file-system)))))

(deftest builds-unix-filesystem-with-specified-max-cache-size
  (let [file-system (t/new-unix-in-memory-file-system
                      :max-cache-size 2147483648)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-cache-size file-system)))))

(deftest builds-unix-filesystem-with-specified-name-canonical-normalization
  (let [file-system (t/new-unix-in-memory-file-system
                      :name-canonical-normalization #{:case-fold-ascii})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-canonical-normalization file-system)))))

(deftest builds-unix-filesystem-with-specified-name-display-normalization
  (let [file-system (t/new-unix-in-memory-file-system
                      :name-display-normalization #{:case-fold-ascii})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-display-normalization file-system)))))

(deftest builds-unix-filesystem-with-equality-based-on-canonical-form
  (let [file-system (t/new-unix-in-memory-file-system
                      :path-equality-uses-canonical-form? true)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (true? (jimfs/path-equality-uses-canonical-form? file-system)))))

(deftest builds-unix-filesystem-with-default-attribute-values
  (let [file-system (t/new-unix-in-memory-file-system
                      :default-attribute-values
                      {"owner:owner"       "some-user"
                       "posix:permissions" "rwxr-xr-x"})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= [(u/->file-attribute "owner:owner"
              (pr/->user-principal file-system "some-user"))
            (u/->file-attribute "posix:group"
              (pr/->group-principal file-system "group"))
            (u/->file-attribute "posix:permissions" "rwxr-xr-x")]
          (jimfs/default-attribute-values file-system)))))

(deftest builds-unix-filesystem-with-specified-contents-in-working-directory
  (let [file-system (t/new-unix-in-memory-file-system
                      :working-directory "/work"
                      :contents
                      [[:file-1 {:content "Line 1"}]
                       [:directory-1
                        [:file-2 {:content "Line 2"}]]])]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= ["Line 1"]
          (f/read-all-lines (p/path file-system "/work/file-1"))))
    (is (= ["Line 2"]
          (f/read-all-lines
            (p/path file-system "/work/directory-1/file-2"))))))

(deftest builds-unix-filesystem-throwing-errors-as-specified
  (let [file-system (t/new-unix-in-memory-file-system
                      :error-on [[#'pathological.files/create-directory]
                                 [#'pathological.files/create-file]])]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/test"))))
    (is (thrown? IOException
          (f/create-file (p/path file-system "/test"))))

    (f/create-symbolic-link
      (p/path file-system "/test")
      (p/path file-system "/other"))

    (is (true? (f/exists? (p/path file-system "/test")
                 :no-follow-links)))))

(deftest builds-default-osx-filesystem-with-random-name
  (let [file-system (t/new-osx-in-memory-file-system)]
    (is (predicates/uuid-string? (jimfs/name file-system)))
    (is (not=
          (jimfs/name file-system)
          (jimfs/name (t/new-osx-in-memory-file-system))))
    (is (= #{(p/path file-system "/")}
          (fs/root-directories file-system)))
    (is (= #{:basic}
          (fs/supported-file-attribute-views file-system)))
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS
             Feature/FILE_CHANNEL}
          (jimfs/supported-features file-system)))
    (is (= "/" (str (jimfs/working-directory file-system))))
    (is (= #{PathNormalization/NFC}
          (jimfs/name-display-normalization file-system)))
    (is (= #{PathNormalization/NFD PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-canonical-normalization file-system)))
    (is (false? (jimfs/path-equality-uses-canonical-form? file-system)))
    (is (= 8192 (jimfs/block-size file-system)))
    (is (= 4294967296 (jimfs/max-size file-system)))
    (is (= 4294967296 (jimfs/max-cache-size file-system)))
    (is (= [] (jimfs/default-attribute-values file-system)))))

(deftest builds-osx-filesystem-with-specified-name
  (let [file-system-name (t/random-file-system-name)
        file-system (t/new-osx-in-memory-file-system
                      :name file-system-name)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= file-system-name (jimfs/name file-system)))))

(deftest builds-osx-filesystem-with-specified-working-directory
  (let [file-system (t/new-osx-in-memory-file-system
                      :working-directory "/work")]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= "/work" (str (jimfs/working-directory file-system))))))

(deftest builds-osx-filesystem-with-specified-attribute-views
  (let [file-system (t/new-osx-in-memory-file-system
                      :attribute-views #{:basic :acl :owner})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{:basic :acl :owner}
          (fs/supported-file-attribute-views file-system)))))

(deftest builds-osx-filesystem-with-specified-features
  (let [file-system (t/new-osx-in-memory-file-system
                      :features #{:links :symbolic-links})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS}
          (jimfs/supported-features file-system)))))

(deftest builds-osx-filesystem-with-specified-block-size
  (let [file-system (t/new-osx-in-memory-file-system
                      :block-size 4096)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 4096
          (jimfs/block-size file-system)))))

(deftest builds-osx-filesystem-with-specified-max-size
  (let [file-system (t/new-osx-in-memory-file-system
                      :max-size 2147483648)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-size file-system)))))

(deftest builds-osx-filesystem-with-specified-max-cache-size
  (let [file-system (t/new-osx-in-memory-file-system
                      :max-cache-size 2147483648)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-cache-size file-system)))))

(deftest builds-osx-filesystem-with-specified-name-canonical-normalization
  (let [file-system (t/new-osx-in-memory-file-system
                      :name-canonical-normalization #{:case-fold-ascii})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-canonical-normalization file-system)))))

(deftest builds-osx-filesystem-with-specified-name-display-normalization
  (let [file-system (t/new-osx-in-memory-file-system
                      :name-display-normalization #{:case-fold-ascii})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-display-normalization file-system)))))

(deftest builds-osx-filesystem-with-equality-based-on-canonical-form
  (let [file-system (t/new-osx-in-memory-file-system
                      :path-equality-uses-canonical-form? true)]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (true? (jimfs/path-equality-uses-canonical-form? file-system)))))

(deftest builds-osx-filesystem-with-default-attribute-values
  (let [file-system (t/new-osx-in-memory-file-system
                      :attribute-views #{:basic :posix :owner}
                      :default-attribute-values
                      {"owner:owner"       "some-user"
                       "posix:permissions" "rwxr-xr-x"})]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= [(u/->file-attribute "owner:owner"
              (pr/->user-principal file-system "some-user"))
            (u/->file-attribute "posix:group"
              (pr/->group-principal file-system "group"))
            (u/->file-attribute "posix:permissions" "rwxr-xr-x")]
          (jimfs/default-attribute-values file-system)))))

(deftest builds-osx-filesystem-with-specified-contents-in-working-directory
  (let [file-system (t/new-osx-in-memory-file-system
                      :working-directory "/work"
                      :contents
                      [[:file-1 {:content "Line 1"}]
                       [:directory-1
                        [:file-2 {:content "Line 2"}]]])]
    (is (= (PathType/unix) (jimfs/path-type file-system)))
    (is (= ["Line 1"]
          (f/read-all-lines (p/path file-system "/work/file-1"))))
    (is (= ["Line 2"]
          (f/read-all-lines
            (p/path file-system "/work/directory-1/file-2"))))))

(deftest builds-osx-filesystem-throwing-errors-as-specified
  (let [file-system (t/new-osx-in-memory-file-system
                      :error-on [[#'pathological.files/create-directory]
                                 [#'pathological.files/create-file]])]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/test"))))
    (is (thrown? IOException
          (f/create-file (p/path file-system "/test"))))

    (f/create-symbolic-link
      (p/path file-system "/test")
      (p/path file-system "/other"))

    (is (true? (f/exists? (p/path file-system "/test")
                 :no-follow-links)))))

(deftest builds-default-windows-filesystem-with-random-name
  (let [file-system (t/new-windows-in-memory-file-system)]
    (is (predicates/uuid-string? (jimfs/name file-system)))
    (is (not=
          (jimfs/name file-system)
          (jimfs/name (t/new-windows-in-memory-file-system))))
    (is (= #{(p/path file-system "C:\\")}
          (fs/root-directories file-system)))
    (is (= #{:basic}
          (fs/supported-file-attribute-views file-system)))
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS
             Feature/FILE_CHANNEL}
          (jimfs/supported-features file-system)))
    (is (= "C:\\" (str (jimfs/working-directory file-system))))
    (is (= #{} (jimfs/name-display-normalization file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-canonical-normalization file-system)))
    (is (true? (jimfs/path-equality-uses-canonical-form? file-system)))
    (is (= 8192 (jimfs/block-size file-system)))
    (is (= 4294967296 (jimfs/max-size file-system)))
    (is (= 4294967296 (jimfs/max-cache-size file-system)))
    (is (= [] (jimfs/default-attribute-values file-system)))))

(deftest builds-windows-filesystem-with-specified-name
  (let [file-system-name (t/random-file-system-name)
        file-system (t/new-windows-in-memory-file-system
                      :name file-system-name)]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= file-system-name (jimfs/name file-system)))))

(deftest builds-windows-filesystem-with-specified-roots
  (let [file-system (t/new-windows-in-memory-file-system
                      :roots #{"C:\\" "D:\\"})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{(p/path file-system "C:\\")
             (p/path file-system "D:\\")}
          (fs/root-directories file-system)))))

(deftest builds-windows-filesystem-with-specified-working-directory
  (let [file-system (t/new-windows-in-memory-file-system
                      :working-directory "C:\\work")]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= "C:\\work" (str (jimfs/working-directory file-system))))))

(deftest builds-windows-filesystem-with-specified-attribute-views
  (let [file-system (t/new-windows-in-memory-file-system
                      :attribute-views #{:basic :acl :owner})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{:basic :acl :owner}
          (fs/supported-file-attribute-views file-system)))))

(deftest builds-windows-filesystem-with-specified-features
  (let [file-system (t/new-windows-in-memory-file-system
                      :features #{:links :symbolic-links})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{Feature/LINKS
             Feature/SYMBOLIC_LINKS}
          (jimfs/supported-features file-system)))))

(deftest builds-windows-filesystem-with-specified-block-size
  (let [file-system (t/new-windows-in-memory-file-system
                      :block-size 4096)]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= 4096
          (jimfs/block-size file-system)))))

(deftest builds-windows-filesystem-with-specified-max-size
  (let [file-system (t/new-windows-in-memory-file-system
                      :max-size 2147483648)]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-size file-system)))))

(deftest builds-windows-filesystem-with-specified-max-cache-size
  (let [file-system (t/new-windows-in-memory-file-system
                      :max-cache-size 2147483648)]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= 2147483648
          (jimfs/max-cache-size file-system)))))

(deftest builds-windows-filesystem-with-specified-name-canonical-normalization
  (let [file-system (t/new-windows-in-memory-file-system
                      :name-canonical-normalization #{:case-fold-ascii})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-canonical-normalization file-system)))))

(deftest builds-windows-filesystem-with-specified-name-display-normalization
  (let [file-system (t/new-windows-in-memory-file-system
                      :name-display-normalization #{:case-fold-ascii})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= #{PathNormalization/CASE_FOLD_ASCII}
          (jimfs/name-display-normalization file-system)))))

(deftest builds-windows-filesystem-with-equality-based-on-canonical-form
  (let [file-system (t/new-windows-in-memory-file-system
                      :path-equality-uses-canonical-form? true)]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (true? (jimfs/path-equality-uses-canonical-form? file-system)))))

(deftest builds-windows-filesystem-with-default-attribute-values
  (let [file-system (t/new-windows-in-memory-file-system
                      :attribute-views #{:basic :posix :owner}
                      :default-attribute-values
                      {"owner:owner"       "some-user"
                       "posix:permissions" "rwxr-xr-x"})]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= [(u/->file-attribute "owner:owner"
              (pr/->user-principal file-system "some-user"))
            (u/->file-attribute "posix:group"
              (pr/->group-principal file-system "group"))
            (u/->file-attribute "posix:permissions" "rwxr-xr-x")]
          (jimfs/default-attribute-values file-system)))))

(deftest builds-windows-filesystem-with-specified-contents-in-working-directory
  (let [file-system (t/new-windows-in-memory-file-system
                      :working-directory "C:\\work"
                      :contents
                      [[:file-1 {:content "Line 1"}]
                       [:directory-1
                        [:file-2 {:content "Line 2"}]]])]
    (is (= (PathType/windows) (jimfs/path-type file-system)))
    (is (= ["Line 1"]
          (f/read-all-lines (p/path file-system "C:\\work\\file-1"))))
    (is (= ["Line 2"]
          (f/read-all-lines
            (p/path file-system "C:\\work\\directory-1\\file-2"))))))

(deftest builds-windows-filesystem-throwing-errors-as-specified
  (let [file-system (t/new-windows-in-memory-file-system
                      :error-on [[#'pathological.files/create-directory]
                                 [#'pathological.files/create-file]])]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "C:\\test"))))
    (is (thrown? IOException
          (f/create-file (p/path file-system "C:\\test"))))

    (f/create-symbolic-link
      (p/path file-system "C:\\test")
      (p/path file-system "C:\\other"))

    (is (true? (f/exists? (p/path file-system "C:\\test")
                 :no-follow-links)))))

(deftest builds-erroring-filesystem-on-specified-files-ns-calls
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on [[#'pathological.files/create-directory]
                        [#'pathological.files/create-file]]}))]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/test"))))
    (is (thrown? IOException
          (f/create-file (p/path file-system "/test"))))

    (f/create-symbolic-link
      (p/path file-system "/test")
      (p/path file-system "/other"))

    (is (true? (f/exists? (p/path file-system "/test")
                 :no-follow-links)))))

(deftest builds-erroring-filesystem-on-specified-provider-calls
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on
             [['java.nio.file.spi.FileSystemProvider#createDirectory]
              ['java.nio.file.spi.FileSystemProvider#newByteChannel]]}))]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/test"))))
    (is (thrown? IOException
          (f/create-file (p/path file-system "/test"))))

    (f/create-symbolic-link
      (p/path file-system "/test")
      (p/path file-system "/other"))

    (is (true? (f/exists? (p/path file-system "/test")
                 :no-follow-links)))))

(deftest builds-erroring-filesystem-by-matching-arguments-on-files-ns
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on
             [[#'pathological.files/create-directory ["/directory-1"]]
              [#'pathological.files/create-directory ["/directory-2"]]]}))]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/directory-1"))))
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/directory-2"))))

    (f/create-directory (p/path file-system "/directory-3"))

    (is (true? (f/exists? (p/path file-system "/directory-3"))))))

(deftest builds-erroring-filesystem-by-matching-arguments-on-provider-calls
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on
             [['java.nio.file.spi.FileSystemProvider#createDirectory
               ["/directory-1"]]
              ['java.nio.file.spi.FileSystemProvider#createDirectory
               ["/directory-2"]]]}))]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/directory-1"))))
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/directory-2"))))

    (f/create-directory (p/path file-system "/directory-3"))

    (is (true? (f/exists? (p/path file-system "/directory-3"))))))

(deftest builds-erroring-filesystem-matching-file-attributes
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on
             [[#'pathological.files/create-directory
               ["/directory-1"
                {"posix:permissions" "rwxr-xr-x"
                 "owner:owner"       (fn [file-system]
                                       (pr/->user-principal
                                         file-system "some-user"))}]]]}))]
    (is (thrown? IOException
          (f/create-directory (p/path file-system "/directory-1")
            {"posix:permissions" "rwxr-xr-x"
             "owner:owner"       (pr/->user-principal "some-user")})))

    (f/create-directory (p/path file-system "/directory-1")
      {"posix:permissions" "r--r--r--"})

    (is (true? (f/exists? (p/path file-system "/directory-1"))))))

(deftest builds-erroring-filesystem-matching-copy-option
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:contents
             [[:file-1 {:contents ["Line 1" "Line 2"]}]]
             :error-on
             [[#'pathological.files/copy
               ["/file-1" "/file-2" :replace-existing :atomic-move]]]}))]
    (is (thrown? IOException
          (f/copy (p/path file-system "/file-1") (p/path file-system "/file-2")
            :replace-existing :atomic-move)))

    (f/copy (p/path file-system "/file-1") (p/path file-system "/file-2")
      :replace-existing)

    (is (true? (f/exists? (p/path file-system "/file-2"))))))

(deftest builds-erroring-filesystem-matching-open-option
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:error-on
             [[#'pathological.files/new-output-stream
               ["/file-1" :write :truncate-existing]]]}))]
    (is (thrown? IOException
          (f/new-output-stream
            (p/path file-system "/file-1")
            :write :truncate-existing)))

    (f/create-file (p/path file-system "/file-1"))
    (spit (f/new-output-stream
            (p/path file-system "/file-1")
            :write :append)
      "Hello")

    (is (true? (f/exists? (p/path file-system "/file-1"))))))

(deftest builds-erroring-filesystem-matching-class-and-link-options
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:contents
             [[:file-1 {:content ["Line 1" "Line 2"]}]]
             :error-on
             [[#'pathological.files/read-file-attribute-view
               ["/file-1" :basic :no-follow-links]]]}))]
    (is (thrown? IOException
          (f/read-file-attribute-view
            (p/path file-system "/file-1")
            :basic :no-follow-links)))

    (is (some? (f/read-file-attribute-view
                 (p/path file-system "/file-1")
                 :basic)))))

(deftest builds-erroring-filesystem-matching-access-mode
  (let [file-system
        (t/new-in-memory-file-system
          (merge t/unix-defaults
            {:contents
             [[:file-1 {:content ["Line 1" "Line 2"]}]]
             :error-on
             [['java.nio.file.spi.FileSystemProvider#checkAccess
               ["/file-1" :read]]]}))]
    ; would be true if checkAccess didn't fail
    (is (false? (f/readable? (p/path file-system "/file-1"))))

    (is (true? (f/writable? (p/path file-system "/file-1"))))))
