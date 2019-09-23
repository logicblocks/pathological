# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com)
and this project adheres to 
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## [Unreleased]

## [0.1.13] — 2019-09-23
### Added
- support for copy options to `copy-recursively` and `move-recursively`
  in `pathological.files`.
  
### Fixed
- bug in `walk-file-tree` in `pathological.files` where `:visit-file-failed-fn`
  was not being called.

## [0.1.12] — 2019-09-22
### Added
- argument matching for errors in file systems created by
  `pathological.testing`.
- error handling on `delete-recursively`, `copy-recursively`, `move-recursively`
  and `populate-file-tree` in `pathological.files` via an `:on-error` option
  taking `:skip` or `:throw` values.

## [0.1.11] — 2019-09-16
### Added
- support for configurable errors in file systems created by 
  `pathological.testing`.

### Changed
- `pathological.testing` dependencies (`com.google.jimfs/jimfs` and 
  `org.mockito/mockito-core`) are now in provided scope. They are only
  required if the `pathological.testing` namespace is used.

## [0.1.10] — 2019-09-02
### Added
- support for character sets other than UTF-8 in `populate-file-tree`.
- support for entry by entry overrides of existing entry handling in
  `populate-file-tree`.
- support for handling type mismatches in `populate-file-tree`.
- support for appending to existing files in `populate-file-tree`.
- support for keyword `:on-exists` values in `populate-file-tree`.

### Fixed
- issue where options passed to `populate-file-tree` would not be applied to
  nested entries.
  
### Changed
- `:on-entry-exists` and `:on-directory-exists` to `:on-exists` taking a map
  from new type to existing type on `populate-file-tree` (breaking).
- `:merge` on-exists handling type to `:append` (breaking).

## [0.1.9] — 2019-08-28
### Added
- support for setting file attributes on files, symbolic links and directories
  in `populate-file-tree`.
- support `String`, `InputStream` and `Reader` content in `populate-file-tree`.

## [0.1.8] — 2019-08-26
### Added
- support for specifying handling when a file / symbolic link / link already
  exists in `populate-file-tree` using `:on-entry-exists` option; 
  possible values are `:throw`, `:overwrite` or `:skip`.
- support for specifying handling when a directory exists in 
  `populate-file-tree` using `:on-directory-exists` option;
  possible values are `:throw`, `:overwrite`, `:merge` or `:skip`.

### Changed
- `BasicFileAttributes` now passed as a map to `pre-visit-directory-fn` and 
  `visit-file-fn` on `walk`.

## [0.1.7] — 2019-08-25
### Added
- implementations for mutative methods on attribute views.
- `open?`, `read-only?`, `close` and `separator` functions in 
  `pathological.file-systems`.
- `supports-file-attribute-view`, `read-file-store-attribute-view` and
  `read-attribute` on file stores.
- support for registering custom attribute converters.
- support for attribute specs to be provided as kebab case strings or as maps.
- support for building paths from URIs. 

### Changed
- `BasePath` protocol to `Pathable`.
- return `BasicFileStore` record from `file-stores` in 
  `pathological.file-systems`.
- `BasicUserPrincipal` -> `UserPrincipal`
- `BasicGroupPrincipal` -> `GroupPrincipal`
- `*FileAttributes` -> `*FileAttributeViews`
- `BasicFileStore` -> `FileStore`

## [0.1.6] — 2019-08-15
### Added
- `read-file-attribute-view` method with corresponding records for default view
  types.
- `set-attribute` method.
- `read-attribute` method.
- `read-attributes` method.

## [0.1.5] — 2019-08-11
### Added
- `lines` method.
- `lines-stream` method.
- `file-store` method to `pathological.paths`.
- `file-stores` method to `pathological.file-systems`.
- `supported-file-attribute-views` method to `pathological.file-systems`.

## [0.1.4] — 2019-08-10
### Added
- `read-all-bytes` method.
- `create-temp-file` method.
- `create-temp-directory ` method.
- `size` method.
- `list` method.
- `find-stream` method.
- `walk-stream` method.
- `list-stream` method.
- `probe-content-type` method.

## [0.1.3] — 2019-08-08
### Added
- `not-exists?` method.
- `read-owner` method.
- `set-owner` method.
- `read-last-modified-time` method.
- `set-last-modified-time` method.
- `new-buffered-reader` method.
- `new-buffered-writer` method.

## [0.1.2] — 2019-08-05
### Added
- `delete-if-exists` method.
- `read-posix-file-permissions` method.
- `set-posix-file-permissions` method.

### Changed
- `->posix-file-permissions`, `->posix-file-permissions-string` and 
  `->posix-file-permissions-attribute` now deal in sets of keywords.

## [0.1.1] — 2019-08-04
### Fixed
- Documentation source URI now points to correct repository.

## [0.1.0] — 2019-08-04
### Added
- Documentation is now generated into `docs` on release.

## [0.0.1] — 2019-08-04
### Changed
- Preparing for initial release.


[0.0.1]: https://github.com/logicblocks/pathological/compare/0.0.1...0.0.1
[0.1.0]: https://github.com/logicblocks/pathological/compare/0.0.1...0.1.0
[0.1.1]: https://github.com/logicblocks/pathological/compare/0.1.0...0.1.1
[0.1.2]: https://github.com/logicblocks/pathological/compare/0.1.1...0.1.2
[0.1.3]: https://github.com/logicblocks/pathological/compare/0.1.2...0.1.3
[0.1.4]: https://github.com/logicblocks/pathological/compare/0.1.3...0.1.4
[0.1.5]: https://github.com/logicblocks/pathological/compare/0.1.4...0.1.5
[0.1.6]: https://github.com/logicblocks/pathological/compare/0.1.5...0.1.6
[0.1.7]: https://github.com/logicblocks/pathological/compare/0.1.6...0.1.7
[0.1.8]: https://github.com/logicblocks/pathological/compare/0.1.7...0.1.8
[0.1.9]: https://github.com/logicblocks/pathological/compare/0.1.8...0.1.9
[0.1.10]: https://github.com/logicblocks/pathological/compare/0.1.9...0.1.10
[0.1.11]: https://github.com/logicblocks/pathological/compare/0.1.10...0.1.11
[0.1.12]: https://github.com/logicblocks/pathological/compare/0.1.11...0.1.12
[0.1.13]: https://github.com/logicblocks/pathological/compare/0.1.12...0.1.13
[Unreleased]: https://github.com/logicblocks/pathological/compare/0.1.13...HEAD
