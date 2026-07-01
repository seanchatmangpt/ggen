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

#![allow(
    clippy::unused_async_trait_impl,
    clippy::for_kv_map,
    clippy::too_long_first_doc_paragraph
)]

pub mod a2a;
pub mod a2a_generated;
pub mod a2a_registry;

pub use a2a::{
    Artifact, ArtifactCollection, StateTransition, Task, TaskMessage, TaskState, TaskStateMachine,
    Transport,
};
pub use a2a_generated::{Agent, AgentFactory, Message, Port, UnifiedAgent, UnifiedAgentBuilder};
pub use a2a_registry::{AgentEntry, AgentQuery, AgentRegistry, HealthMonitor, HealthStatus};

pub mod mcp_packs;
pub mod mcp_server;

pub use mcp_packs::{dispatch_pack_tool, pack_agent_card, PackToolsAdapter, PACK_TOOLS};
