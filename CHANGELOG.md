# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1] - 2026-02-03

### Fixed

- **Codegen**: Preserve camelCase attribute names (e.g., `firstName` no longer becomes `firstname`)
- **Codegen**: Generate Scala 3 enums for Cedar enum entities with `fromString`, `values`, and proper `EntityValue` integration
- **Codegen**: Handle `SetOf` pattern match exhaustivity for nested sets

## [0.1.0] - 2026-01-31

### Added

- Initial public release
- Cedar schema parser with full Cedar 3.x support
- Type-safe Scala code generation from Cedar schemas
- Authorization engine with batch processing
- Entity store with caching support (Caffeine)
- Policy and schema validation
- Observability modules (OpenTelemetry tracing, audit logging)
- SBT plugin for build-time code generation
- Smithy integration (optional)

[unreleased]: https://github.com/DevNico/cedar4s/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/DevNico/cedar4s/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/DevNico/cedar4s/releases/tag/v0.1.0
