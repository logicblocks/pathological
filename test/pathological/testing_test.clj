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
    [pathological.support.predicates :as predicates])
  (:import
    [com.google.common.jimfs Feature
                             PathType]))

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

(deftest uses-specified-file-system-name-when-provided
  (let [file-system-name (t/random-file-system-name)
        file-system (t/new-unix-in-memory-file-system
                      :name file-system-name)]
    (is (= file-system-name (jimfs/name file-system)))))
