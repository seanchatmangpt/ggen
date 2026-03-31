//! `ggen-a2a-registry` -- Multi-agent A2A orchestration registry.
//!
//! This crate provides a central registry for managing the lifecycle of
//! A2A (Agent-to-Agent) participants: registration, discovery, health
//! monitoring, and deregistration.
//!
//! # Quick start
//!
//! ```no_run
//! use ggen_a2a_registry::{AgentRegistry, MemoryStore, AgentEntry, HealthStatus};
//! use std::sync::Arc;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let store = Arc::new(MemoryStore::new());
//!     let registry = AgentRegistry::new(store);
//!
//!     let entry = AgentEntry {
//!         id: "agent-1".into(),
//!         name: "Code Generator".into(),
//!         agent_type: "code-gen".into(),
//!         endpoint_url: "http://localhost:8080".into(),
//!         capabilities: vec!["sparql".into(), "code-gen".into()],
//!         health: HealthStatus::Unknown,
//!         registered_at: chrono::Utc::now(),
//!         last_heartbeat: chrono::Utc::now(),
//!     };
//!
//!     let reg = registry.register(entry).await?;
//!     println!("Registered: {}", reg.agent_id);
//!
//!     let agents = registry.list().await?;
//!     println!("Total agents: {}", agents.len());
//!
//!     registry.shutdown().await?;
//!     Ok(())
//! }
//! ```

// Allow significant_drop_tightening: the suggested "merge temporary with usage"
// produces unreadable code for async RwLock guard patterns where the guard
// must be held across multiple operations in the same scope.
#![allow(clippy::significant_drop_tightening)]

pub mod error;
pub mod health;
pub mod query;
pub mod registry;
pub mod store;
pub mod types;

// Re-exports for ergonomic use.
pub use error::{RegistryError, RegistryResult};
pub use health::{ping_agent, HealthConfig, HealthMonitor};
pub use query::{matches_query, AgentQuery};
pub use registry::AgentRegistry;
pub use store::{AgentStore, MemoryStore};
pub use types::{AgentEntry, HealthStatus, Registration};
