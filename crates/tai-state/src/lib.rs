#![doc = include_str!("../README.md")]

//! Cloud-Native State Machine Persistence with Firestore
//!
//! This crate provides production-grade state machine persistence using Google Cloud Firestore
//! with support for:
//!
//! - **ACID Transactions**: Multi-document transactions with optimistic locking
//! - **Eventual Consistency**: Conflict resolution strategies and causality tracking
//! - **Crash Recovery**: Event sourcing, snapshots, and replay capabilities
//! - **Distributed Locking**: Pessimistic locking alternatives for high-contention scenarios
//! - **Audit Trails**: Complete change history with cryptographic integrity
//!
//! # Architecture
//!
//! The system is organized hierarchically in Firestore:
//!
//! ```text
//! projects/{project_id}/
//!   governors/{governor_id}/
//!     states/{state_id}
//!       - state: StateSnapshot
//!       - version: u64 (for optimistic locking)
//!       - timestamp: SystemTime
//!       - metadata: ChangeMetadata
//!     events/{event_id}
//!       - action: String
//!       - from_state: StateSnapshot
//!       - to_state: StateSnapshot
//!       - timestamp: SystemTime
//!       - causality: VectorClock
//! ```
//!
//! # Example
//!
//! ```rust,no_run
//! use tai_state::{FirestoreStore, StateSnapshot, ChangeMetadata};
//! use tokio;
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     // Initialize store with Firestore
//!     let store = FirestoreStore::new("my-gcp-project").await?;
//!
//!     // Save state with metadata
//!     let metadata = ChangeMetadata::new("system", "initialization", None);
//!     store.save_state("governor1", "state1", state_snapshot, metadata).await?;
//!
//!     // Retrieve with version for optimistic locking
//!     let (state, version) = store.get_state("governor1", "state1").await?;
//!
//!     // Update with conflict detection
//!     let new_metadata = ChangeMetadata::new("system", "transition", Some(version));
//!     store.update_state("governor1", "state1", new_state, version, new_metadata).await?;
//!
//!     Ok(())
//! }
//! ```

pub mod error;
pub mod eventual_consistency;
pub mod firestore_store;
pub mod state_machine_persister;
pub mod transactions;
pub mod types;

// Re-export main types
pub use error::{Error, Result};
pub use eventual_consistency::{ConflictResolver, EventualConsistency, VectorClock};
pub use firestore_store::{FirestoreStore, StateSnapshot};
pub use state_machine_persister::StateMachinePersister;
pub use transactions::TransactionManager;
pub use types::{ChangeMetadata, StateId, StateSnapshot as TypesSnapshot};
