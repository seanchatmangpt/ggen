//! Multi-Region Replication Module
//!
//! Implements active/passive replication with vector clocks for multi-region failover.
//! Phase 1 foundation: core types for 3-region cluster (US-East primary, US-West/EU replicas).
//! Phase 2B: Byzantine evidence correlation for region health scoring and isolation.
//! Phase 2B: Health-triggered failover with quorum-based promotion.

// IMPORTANT: Module order matters due to dependencies
pub mod manager;
pub mod vector_clock;
// TODO: Fix compilation errors in evidence_tracker
// pub mod evidence_tracker;
pub mod event_bus;
pub mod idempotency;
// TODO: Fix compilation errors in these modules
// pub mod conflict;
// pub mod failover;

pub use manager::{
    CausalityResult, MultiRegionConfig, MultiRegionManager, RegionHealth, RegionNode,
    ReplicationLag,
};
pub use vector_clock::VectorClock;
// TODO: Uncomment when evidence_tracker is fixed
// pub use evidence_tracker::{
//     EvidenceLedgerEntry, EvidenceStats, EvidenceTracker, EvidenceTrackerConfig, HealthScore,
//     ReplicationEvent,
// };
pub use event_bus::{
    FilteredReceiver, HealthStatusEvent, RegionUpdateEvent, ReplicationEventBus,
    ReplicationEventType, StreamingEvent, VectorClockChangeEvent,
};
pub use idempotency::{
    EventId, EventRecord, IdempotencyStats, ReplicationEventIdempotency,
};
// TODO: Uncomment when conflict module is fixed
// pub use conflict::{
//     ConflictResolution, ConflictResolver, ConflictType, LWWStrategy, ResolutionPolicy,
//     ResolutionResult, StrategyRegistry,
// };
// TODO: Uncomment when failover module is fixed
// pub use failover::{
//     FailoverCoordinator, FailoverConfig, FailoverDecision, FailoverEvent, FailoverState,
//     HealthCheckResult,
// };
