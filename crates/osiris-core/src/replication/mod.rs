//! Multi-Region Replication Module
//!
//! Implements active/passive replication with vector clocks for multi-region failover.
//! Phase 1 foundation: core types for 3-region cluster (US-East primary, US-West/EU replicas).

pub mod manager;
pub mod vector_clock;

pub use manager::{
    CausalityResult, MultiRegionConfig, MultiRegionManager, RegionHealth, RegionNode,
    ReplicationLag,
};
pub use vector_clock::VectorClock;
