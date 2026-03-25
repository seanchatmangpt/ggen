//! Distributed Consensus Library - PBFT Implementation
//!
//! This library implements Practical Byzantine Fault Tolerance (PBFT),
//! a state machine replication protocol that tolerates up to f Byzantine faults
//! in a system of n=3f+1 nodes.
//!
//! # Algorithm Overview
//!
//! PBFT proceeds through four phases per consensus round:
//! 1. **Pre-Prepare**: Primary proposes a value to all replicas
//! 2. **Prepare**: Replicas exchange prepare messages, building consensus
//! 3. **Commit**: Upon collecting 2f+1 prepares, replicas commit the value
//! 4. **Decision**: Consensus is finalized with cryptographic receipts
//!
//! # Byzantine Resilience
//!
//! The protocol guarantees consensus despite up to f Byzantine faults where:
//! - n = total number of nodes
//! - f = max Byzantine nodes
//! - Quorum = 2f + 1 votes (to outnumber any Byzantine coalition)
//! - Requirement: n >= 3f + 1
//!
//! # Safety Properties
//!
//! - **Agreement**: All non-faulty nodes agree on the same value
//! - **Validity**: If all non-faulty nodes propose the same value, it will be decided
//! - **Termination**: Non-faulty nodes eventually reach a decision
//!
//! # Modules
pub mod messages;
pub mod node;
pub mod pbft;
pub mod receipts;
pub mod state;

pub use messages::{Message, MessageType};
pub use node::Node;
pub use pbft::{PbftConfig, PbftConsensus};
pub use receipts::{Receipt, ReceiptSignature};
pub use state::{ConsensusState, Phase};
