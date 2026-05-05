#![doc = "Binary data structures for pictl process mining platform."]
#![doc = ""]
#![doc = "This crate defines the canonical types that all functions pass around:"]
#![doc = "- Event log formats (EventLog, OCEL)"]
#![doc = "- Process models (DFG, PetriNet, DeclareModel)"]
#![doc = "- Conformance results (ConformanceResult, TokenReplayResult)"]
#![doc = "- Provenance and hashing (ProvenanceChain, BLAKE3 operations)"]

pub mod conformance;
pub mod error;
pub mod event_log;
pub mod hash;
pub mod models;
pub mod ocel;
pub mod provenance;

// Re-exports for convenience
pub use conformance::{ConformanceResult, TokenReplayResult};
pub use error::{Error, Result};
pub use event_log::{AttributeValue, Attributes, Event, EventLog, Trace};
pub use hash::Blake3Hash;
pub use models::{
    DFGEdge, DFGNode, DeclareConstraint, DeclareModel, PetriNet, PetriNetArc, PetriNetPlace,
    PetriNetTransition, DFG,
};
pub use ocel::{OCELEvent, OCELObject, OCEL};
pub use provenance::ProvenanceChain;

pub const VERSION: &str = "26.5.4";
