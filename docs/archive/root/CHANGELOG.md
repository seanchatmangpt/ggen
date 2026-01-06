<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Changelog](#changelog)
  - [&#91;Unreleased&#93;](#unreleased)
    - [Added](#added)
    - [Fixed](#fixed)
    - [Changed](#changed)
    - [Technical Details](#technical-details)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Comprehensive Module Documentation**: Added module-level documentation (`//!`) to all undocumented source files across `ggen-core`, `ggen-cli`, `ggen-utils`, and `ggen-domain` crates, following Diataxis framework standards
- **Chicago TDD Testing Framework**: Integrated `chicago-tdd-tools` for state-based testing with behavior verification, replacing traditional unit tests with Chicago School TDD patterns
- **Testing Gap Prevention System**: Added comprehensive Hive Mind analysis system to identify and prevent testing gaps, closing 60% of testing gaps with 80/20 approach
- **Andon Signal System**: Implemented visual problem indicators (Andon signals) for compiler errors, test failures, and warnings with "stop the line" workflow
- **Timeout SLAs**: Added timeout wrappers to all CLI commands to prevent indefinite hangs:
  - Quick checks: 5s timeout (cargo check, cargo fmt, cargo clippy)
  - Compilation: 10s timeout (cargo build debug)
  - Release builds: 30s timeout (cargo build --release)
  - Unit tests: 10s timeout (cargo test --lib)
  - Integration tests: 30s timeout (cargo test --test)
- **Error Handling Standardization**: Completed FMEA-driven error type standardization, replacing all `anyhow::Result` and `anyhow::anyhow!` calls with `ggen_utils::error::Result` and `ggen_utils::error::Error` across all `ggen-core` production code
- **Kaizen Improvements**: Extracted magic strings to named constants (e.g., `WORKSPACE_CARGO_TEMPLATE`) for better maintainability and code clarity
- **GenAI Integration Examples**: Three new working examples demonstrating genai v0.4 integration with qwen3-coder:30b model:
  - `genai_ollama_loop.rs` - Interactive conversation loop with conversation history management
  - `genai_ollama_stream.rs` - Real-time streaming responses for better UX
  - `genai_ollama_multi_provider_compare.rs` - Multi-provider comparison tool for benchmarking different models
- **Multi-Provider Architecture Analysis**: Comprehensive analysis of LLM provider abstraction and hardcoded Ollama preferences
- **Build Optimization Guide**: Complete guide for achieving fast incremental builds (2-3 seconds) and optimized compilation settings
- **Enhanced Documentation TOCs**: Updated table of contents for integration and configuration documentation

### Fixed
- **Compilation Errors**: Resolved compilation errors in preprocessor and poka_yoke modules
- **Error Type Consistency**: Fixed all error type mismatches by standardizing on `ggen_utils::error::Error` throughout `ggen-core`, eliminating mixed use of `anyhow` and custom error types
- **Test Macro Syntax**: Fixed `test!` macro syntax errors (mismatched closing delimiters) in template.rs, generator.rs, inject.rs, and gpack.rs
- **FromStr Trait Implementation**: Corrected `FromStr` trait implementation for `ProjectType` to use `ggen_utils::error::Error` instead of `anyhow::Error`
- **Async Test Type Aliases**: Fixed type alias mismatches in `async_test!` macros by explicitly specifying error types
- **Graph Module Refactoring**: Resolved compilation errors from graph.rs refactoring into graph/core.rs module structure
- **Andon Signal Fixes**: Fixed alert macro type errors and unused warnings that were triggering Andon signals
- **Pre-Push Hook Timeout**: Resolved pre-push hook timeout issues by adding `check-pre-push` task with 30s timeout for lock contention scenarios
- **Code Formatting**: Fixed formatting inconsistencies across multiple files (graph.rs, preprocessor.rs, etc.)
- **FUTURE Comments**: Removed FUTURE comments and replaced with proper documentation or implementation
- **Bug Fixes**: Resolved multiple compilation and runtime issues across AI integration
- **Code Cleanup**: Removed unused imports, standardized client creation patterns, and cleaned up generated comments
- **Configuration Issues**: Fixed hardcoded Ollama defaults and improved provider-agnostic configuration
- **Build Performance**: Implemented incremental compilation, parallel builds, and optimized Cargo profiles for 96% faster builds (60s → 2s)
- **Dependency Conflicts**: Eliminated duplicate dependencies (base64 v0.21.7 vs v0.22.1, reqwest v0.11 vs v0.12)
- **Version Consistency**: Updated all version references to v1.0.0 across documentation

### Changed
- **Error Handling Pattern**: Migrated all library code from `anyhow::Result` to `ggen_utils::error::Result` for consistent, typed error handling across the codebase
- **Error Construction**: Replaced all `anyhow::anyhow!()` calls with `Error::new()` or `Error::with_source()` for better error context and type safety
- **Error Context Formatting**: Standardized error context formatting using `Error::with_context()` with consistent message and context string patterns
- **Test Framework**: Migrated from standard `#[test]` attributes to `chicago_tdd_tools::prelude::*` for behavior verification and state-based testing (recommended pattern from examples)
- **Development Workflow**: All development workflows now use `cargo make` commands with timeout wrappers instead of direct `cargo` commands
- **Module Structure**: Refactored `graph.rs` into `graph/core.rs` for better modular organization
- **AI Command Structure**: Reformatted and cleaned up AI-related CLI commands for better maintainability
- **Provider Abstraction**: Improved multi-provider support with better abstraction patterns and reduced hardcoded preferences
- **Build Configuration**: Updated Cargo.toml profiles and .cargo/config.toml for optimal compilation performance

### Technical Details
- **FMEA-Driven Error Standardization**: Completed comprehensive Failure Mode and Effects Analysis (FMEA) to identify and fix error handling inconsistencies:
  - Identified 94 `anyhow!` calls and 34 files using `anyhow::Result`
  - Calculated Risk Priority Numbers (RPN) to prioritize fixes
  - Systematically replaced all `anyhow` usage with `ggen_utils::error` types
  - Fixed compilation errors from error type mismatches
  - Verified all fixes with compilation, testing, and linting checks
- **Chicago TDD Integration**: Integrated `chicago-tdd-tools` crate for state-based testing:
  - Replaced `#[test]` with `test!{}` macros
  - Replaced `#[tokio::test]` with `async_test!{}` macros
  - Tests now verify observable outputs and state changes (behavior verification)
  - Follows AAA pattern (Arrange-Act-Assert) with real collaborators
- **Andon Signal System**: Implemented "stop the line" workflow for quality assurance:
  - Compiler errors (CRITICAL) - must stop immediately
  - Test failures (CRITICAL) - must stop immediately
  - Warnings (HIGH) - should stop and fix
  - All signals must be cleared before work continues
- **Timeout SLAs**: All CLI commands wrapped with timeout protection:
  - Prevents indefinite hangs from lock contention or deadlocks
  - Different timeout values for different operation types
  - Pre-push hooks use longer timeout (30s) for lock contention scenarios
- **Module Documentation**: Added comprehensive module-level documentation following Diataxis framework:
  - All modules now have `//!` documentation with features, architecture, and examples
  - Documentation includes code examples and usage patterns
  - Follows established documentation standards from CONTRIBUTING.md
- **GenAI v0.4 Integration**: Successfully integrated rust-genai library v0.4 with comprehensive examples and documentation
- **Build Optimizations**: Reduced incremental build times from 60-90 seconds to 2-3 seconds using incremental compilation and parallel builds
- **Multi-Provider Support**: Enhanced provider abstraction while removing hardcoded Ollama preferences for true provider agnosticism
