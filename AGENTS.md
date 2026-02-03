# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## Project Overview

cedar4s is a type-safe Cedar authorization library for Scala. It generates type-safe
Scala code from Cedar schema files and provides an effect-polymorphic runtime for Cedar
policy evaluation.

**Documentation:** [https://devnico.github.io/cedar4s/](https://devnico.github.io/cedar4s/)

**Key Features:**
- Type-safe entity IDs using newtypes prevent mixing entity types at compile time
- Automatic entity hierarchy resolution via EntityStore
- Effect polymorphic - works with Future, IO, ZIO, etc.
- Code generation from Cedar schemas (.cedarschema files)
- Integration with Cedar-Java authorization engine (via JNI)

## Build Commands

### Core Development

```bash
# Compile all modules
sbt compile

# Run tests for all modules
sbt test

# Run tests for specific module
sbt core/test
sbt client/test
sbt codegen/test

# Run a single test class
sbt "core/testOnly cedar4s.entities.EntityStoreTest"

# Cross-compile for different Scala versions
sbt +test                    # Test on all configured Scala versions
sbt ++2.13.16 core/test      # Test on specific Scala version
sbt ++3.7.0 client/test
```

### Publishing & Plugin Development

```bash
# Publish locally for testing
sbt publishLocal

# Publish sbt plugin (requires Scala 2.12)
sbt "++2.12.20; sbt-cedar4s/publishLocal"

# Run benchmarks
sbt "bench/Jmh/run"
sbt "benchIntegration/Jmh/run"
```

### Code Generation

```bash
# Regenerate code from schema in examples
sbt "examples/docshare/compile"  # Triggers cedarCodegen task
sbt "examples/http4s/compile"
sbt "examples/play-framework/compile"

# Run scripted tests for sbt plugin
sbt "sbt-cedar4s/scripted"
```

### Website/Documentation

```bash
# Build documentation site (Docusaurus)
cd website
npm install
npm start        # Development server
npm run build    # Production build
```

## Module Architecture

The project is organized into these modules:

### Core Modules (Production)

- **`types`** - Shared Cedar types (CedarEntityUid, Bijection, Newtype)
  - Cross-compiled for Scala 2.12, 2.13, and 3.x
  - Uses Scala version-specific source directories for Newtype (opaque types in Scala 3, type alias in Scala 2)

- **`schema`** - Cedar schema parser and AST
  - Cross-compiled for Scala 2.12, 2.13, and 3.x
  - Parses JSON and Cedar schema formats
  - Uses cedar-java JNI for Cedar-to-JSON conversion

- **`core`** - Cedar authorization DSL and type classes
  - Contains `CedarSession`, `AuthCheck`, `EntityStore`, `EntityFetcher`, `PrincipalResolver`
  - Effect-polymorphic via custom capability type classes (Functor, Applicative, Monad, Sync, Concurrent)
  - Cross-compiled for Scala 2.13 and 3.x

- **`client`** - Cedar engine wrapper and runtime
  - `CedarEngine[F]` wraps cedar-java authorization engine
  - `CedarRuntime` produces request-scoped `CedarSession` instances
  - `CedarSessionRunner` implements authorization orchestration
  - Cross-compiled for Scala 2.13 and 3.x

- **`codegen`** - Cedar schema to Scala code generator
  - Transforms schema AST to intermediate representation (IR)
  - Renders Scala and Smithy code from IR
  - Cross-compiled for Scala 2.12, 2.13, and 3.x

- **`sbt-cedar4s`** - SBT plugin for code generation
  - Must be Scala 2.12 (SBT requirement)
  - Integrates codegen into SBT build via `cedarCodegen` task
  - Wires into `Compile/sourceGenerators` for automatic code generation

- **`caffeine`** - Caffeine-based entity cache implementation
  - Provides `CaffeineEntityCache` for caching Cedar entities
  - Cross-compiled for Scala 2.13 and 3.x

- **`observability-audit`** - Audit logging for Cedar authorization decisions
  - Provides `AuditInterceptor`, `AuditLogger` interfaces
  - Implementations: `JsonAuditLogger`, `Slf4jAuditLogger`, `FileAuditLogger`
  - Cross-compiled for Scala 2.13 and 3.x

- **`observability-otel`** - OpenTelemetry tracing integration
  - Provides `OpenTelemetryInterceptor` for authorization span creation
  - Configurable attribute filtering and span naming strategies
  - Cross-compiled for Scala 2.13 and 3.x

### Development Modules

- **`bench`** - JMH benchmarks for performance testing
- **`bench-integration`** - Integration benchmarks with PostgreSQL via Testcontainers

## Key Architectural Concepts

See the [full documentation](https://devnico.github.io/cedar4s/) for detailed architectural information.

**Session-Based Authorization:**
- `CedarRuntime` configured once at startup with engine, entity store, and principal resolver
- Per-request `CedarSession` created with principal
- Implicit session used for DSL authorization checks
- See `docs/RFC-DSL-Refactor.md` for ongoing refactor plans

**Code Generation:**
- Cedar schema (.cedarschema) → IR → Scala code
- Generated code in `target/scala-X.Y.Z/src_managed/main/`
- Includes: Unified entrypoint, entity IDs, contexts, fetchers

**Entity Loading:**
- Registry pattern for entity fetchers
- EntityStore handles routing, hierarchies, caching, batching

**Effect Polymorphism:**
- Custom capability type classes (Functor, Applicative, Monad, etc.)
- Works with Future, IO, ZIO, etc.

## Important Files & Locations

- **Schema Files**: `.cedarschema` files in `examples/*/src/main/resources/schema/`
- **Policy Files**: `.cedar` files in `examples/*/src/main/resources/policies/`
- **Generated Code**: `target/scala-X.Y.Z/src_managed/main/` (gitignored)
- **RFCs**: `docs/RFC-*.md` (design documents for major changes)
- **Todos**: `TODOS.md` (tracks completed and open work items)

## Development Notes

**Cedar-Java JNI:**
- Use `classifier "uber"` to include native libraries
- Set `Test/classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat` and `Test/fork := true`

**SBT Plugin:**
- Publish locally: `sbt "++2.12.20; sbt-cedar4s/publishLocal"`
- Test with scripted tests

**Testing:**
- MUnit test framework with ScalaCheck for property-based testing

## Common Patterns

See the [full documentation](https://devnico.github.io/cedar4s/) for detailed patterns and examples.

### Quick Reference

```scala
// Create runtime (application startup)
val runtime = CedarRuntime(engine, entityStore, principalResolver)

// Create session (per request)
given session: CedarSession[F] = runtime.session(Principal.User(userId))

// Authorization checks
Document.View.on(docId).require
(Folder.View.on(folderId) & Document.View.on(docId)).require
session.filterAllowed(documentIds)(id => Document.View.on(id))
```

## Related Resources

- [Cedar Documentation](https://www.cedarpolicy.com/)
- [Project Documentation](https://devnico.github.io/cedar4s/)
- Cedar-Java: [com.cedarpolicy:cedar-java](https://central.sonatype.com/artifact/com.cedarpolicy/cedar-java)
