# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- support for OTP 25, 26, 27, 28 and 29
- `ex_doc`-based documentation with EEP-48 (`-moduledoc`/`-doc`) attributes
- dev tooling: `erlfmt`, `rebar3_hank` and `elvis` (via `rebar3_lint`)

### Changed

- CI to GitHub Actions with an OTP 24-29 matrix (replacing the container build)
- build system to the current rebar3-based Makefile / `rebar.config`

### Removed

- the microbenchmark escript
- the maintenance notice (the library is maintained again)

## [1.2.0] - 2021-05-13

### Added

- OTP 24 to CI targets

### Changed

- CI from Travis to GitHub Actions

### Removed

- compatibility with OTP 19
- compatibility with OTP 20
- compatibility with OTP 21

## [1.1.1] - 2020-05-26

### Fixed

- outdated README

## [1.1.0] - 2020-05-26

### Removed

- compatibility with OTP 18

## [1.0.3] - 2019-11-11

### Changed

- generated documentation as to (tentatively) make it prettier

## [1.0.2] - 2019-01-19

### Fixed

- unwarranted import of rebar3_hex plugin in library consumers

## [1.0.1] - 2018-06-17

### Fixed

- OTP 21 compatibility

## [1.0.0] - 2018-06-17

### Added

- continuous sampling of arbitrary event types within explicit categories
