//! # ggen-a2a-mcp - Agent-to-Agent Protocol and MCP Server
//!
//! A2A protocol implementation for agent-to-agent communication with
//! Model Context Protocol (MCP) server support.
//!
//! ## Features
//!
//! - Task state machine protocol
//! - Artifact and artifact collection types
//! - In-process and external transport support
//! - Multi-agent orchestration registry
//! - Health monitoring and status tracking

pub mod a2a;
pub mod a2a_generated;
pub mod a2a_registry;

pub use a2a::{
    Artifact, ArtifactCollection, StateTransition, Task, TaskMessage, TaskState, TaskStateMachine,
    Transport,
};
pub use a2a_generated::{Agent, AgentFactory, Message, Port, UnifiedAgent, UnifiedAgentBuilder};
pub use a2a_registry::{AgentEntry, AgentQuery, AgentRegistry, HealthMonitor, HealthStatus};

pub mod mcp_server;
