//! Observability and Chaos Testing Infrastructure
//!
//! Comprehensive chaos engineering framework for testing failure scenarios,
//! recovery mechanisms, and system resilience.
//!
//! # Architecture
//!
//! - **Event Sourcing**: Durable event store for failure replay
//! - **Chaos Injection**: Random failures, delays, clock skew, task killing
//! - **Correlation IDs**: End-to-end trace tracking
//! - **Observability**: Metrics, logs, spans with OpenTelemetry
//!
//! # Example Usage
//!
//! ```ignore
//! let chaos = ChaosEngine::new();
//! chaos.inject_panic("component-name").await;
//! chaos.inject_network_delay(Duration::from_secs(2)).await;
//! ```

pub mod correlation;
pub mod event_store;
pub mod injection;
pub mod scenarios;

pub use correlation::{CorrelationId, CorrelationContext};
pub use event_store::{FailureEvent, EventStore, InMemoryEventStore};
pub use injection::{ChaosEngine, ChaosConfig, PanicInjection};
pub use scenarios::{
    ChaosScenario, PanicInjectionScenario, NetworkChaosScenario,
    ClockSkewScenario, CascadingFailureScenario, RecoveryScenario,
};
