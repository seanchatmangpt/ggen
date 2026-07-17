//! Weaver fixture and telemetry capture utilities.
//!
//! These components provide Chicago TDD aligned helpers for running Weaver
//! live-check automatically inside tests. When the `weaver` feature is enabled
//! they expose fixtures, telemetry capture helpers, and assertion adapters that
//! integrate live-check results directly into the assertion pipeline.

#[cfg(all(feature = "weaver", feature = "otel"))]
mod assertions;
#[cfg(all(feature = "weaver", feature = "otel"))]
mod telemetry_capture;
#[cfg(all(feature = "weaver", feature = "otel"))]
mod validation_results;
#[cfg(all(feature = "weaver", feature = "otel"))]
mod weaver_fixture;

#[cfg(all(feature = "weaver", feature = "otel"))]
pub use assertions::*;
#[cfg(all(feature = "weaver", feature = "otel"))]
pub use telemetry_capture::*;
#[cfg(all(feature = "weaver", feature = "otel"))]
pub use validation_results::*;
#[cfg(all(feature = "weaver", feature = "otel"))]
pub use weaver_fixture::*;
