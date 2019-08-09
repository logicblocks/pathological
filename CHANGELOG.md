# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com)
and this project adheres to 
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## [Unreleased]
### Added
- `read-all-bytes` method.
- `create-temp-file` method.
- `create-temp-directory ` method.

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
[Unreleased]: https://github.com/logicblocks/pathological/compare/0.1.3...HEAD
