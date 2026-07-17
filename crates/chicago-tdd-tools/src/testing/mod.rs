//! Advanced Testing Techniques
//!
//! Specialized testing methodologies that extend core capabilities:
//! property-based testing, mutation testing, snapshot testing, concurrency
//! testing, CLI testing, and test code generation.

#[cfg(feature = "cli-testing")]
pub mod cli;
#[cfg(feature = "concurrency-testing")]
pub mod concurrency;
pub mod continuous_learning;
pub mod effects;
pub mod generator;
pub mod mutation;
pub mod property;
#[cfg(feature = "snapshot-testing")]
pub mod snapshot;
pub mod state_machine;

// Re-export commonly used items
#[cfg(feature = "cli-testing")]
pub use cli::*;
#[cfg(feature = "concurrency-testing")]
pub use concurrency::*;
pub use continuous_learning::*;
pub use effects::*;
pub use generator::*;
#[cfg(feature = "mutation-testing")]
pub use mutation::*;
#[cfg(feature = "property-testing")]
pub use property::*;
#[cfg(feature = "snapshot-testing")]
pub use snapshot::*;
pub use state_machine::*;
