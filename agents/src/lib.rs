//! # ggen-agents - Ultrathink Multi-Agent System
//!
//! This crate implements a sophisticated multi-agent architecture for ggen development,
//! featuring 12 hyper-advanced agents that collaborate to ensure code quality,
//! security, performance, and maintainability.
//!
//! ## Agent Architecture
//!
//! The system follows the 80/20 principle, focusing on the 20% of agent interactions
//! that deliver 80% of the value in software development workflows.
//!
//! ### Core Agents (Phase 1 - Foundation)
//!
//! - **london-bdd**: London School TDD patterns and BDD implementation
//! - **byzantene**: Byzantine fault tolerance and distributed system patterns
//! - **test-oracle**: Test data generation and oracle implementation
//! - **mock-master**: Advanced mocking and dependency injection
//!
//! ### Quality Agents (Phase 2 - Intelligence)
//!
//! - **security-sentinel**: Security vulnerability detection and hardening
//! - **audit-architect**: Code quality auditing and compliance
//! - **performance-profiler**: Performance optimization and SLO monitoring
//! - **pattern-philosopher**: Design pattern identification and application
//!
//! ### Knowledge Agents (Phase 3 - Sophistication)
//!
//! - **docs-dynamo**: Documentation generation and maintenance
//! - **cookbook-compiler**: Recipe and pattern documentation
//! - **api-artisan**: API design and interface evolution
//! - **knowledge-weaver**: Cross-domain pattern synthesis

pub mod agents;
pub mod coordination;
pub mod core;
pub mod protocols;
pub mod swarm;
pub mod ultrathink_core;

pub use coordination::{AgentCoordinator, ExecutionPlan, Task};
pub use core::{Agent, AgentContext, AgentError, AgentResult, ExecutionContext};
pub use protocols::{Message, MessageType, Protocol};
pub use swarm::{UltrathinkSwarm, SwarmStatus, SwarmPerformanceMetrics, demonstrate_autonomous_workflows, run_ultrathink_swarm};
pub use ultrathink_core::{UltrathinkCore, UltrathinkStatus, UltrathinkMetrics, demonstrate_focused_autonomous_workflows, run_ultrathink_core};

// Re-export commonly used types
pub use async_trait::async_trait;
pub use serde::{Deserialize, Serialize};
pub use tokio::sync::{mpsc, RwLock};
pub use tokio;
pub use uuid::Uuid;
