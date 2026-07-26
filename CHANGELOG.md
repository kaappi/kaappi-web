# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

## [Unreleased]

## [0.1.1] - 2026-07-26

### Fixed
- Session ids are now drawn from the OS-entropy-seeded random source
  instead of a clock-seeded LCG. Two requests arriving in the same
  microsecond previously received identical session ids, letting one
  request silently adopt the other's session (seen as the flaky
  "profile no session" failure in nightly CI).

## [0.1.0] - 2026-07-26

### Added
- Declarative routing — `routes` with `GET`/`POST`/`PUT`/`DELETE`/`PATCH`/
  `HEAD`
- Response helpers — `json-response`, `text-response`, `html-response`,
  `redirect`, `no-content`
- Request utilities — `param`, `param/number`, `request-json`
- Cookie handling and session middleware (`wrap-session`), with
  authentication helpers
- Pure Scheme, built on kaappi-http and kaappi-json
- CI workflow for automated testing
