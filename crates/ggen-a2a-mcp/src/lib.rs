//! A2A-MCP Integration for ggen
//!
//! This crate provides integration between the Agent-to-Agent (A2A) protocol
//! and Rusty Model Context Protocol (RMCP), enabling bidirectional
//! communication for ggen's AI agent ecosystem.
//!
//! ## Architecture
//!
//! The crate follows a bridge pattern with adapter layers:
//!
//! ```text
//! ┌─────────────────────────────────────────────┐
//! │              ggen-a2a-mcp Crate             │
//! ├─────────────┬─────────────┬─────────────────┤
//! │  LLM Client │ Translation │  A2A Protocol   │
//! │  (ggen-ai)  │    Layer    │  (a2a-gen)      │
//! ├─────────────┼─────────────┼─────────────────┤
//! │  RMCP Tool  │ Conversion  │  Agent Skills   │
//! │  Interface  │    Layer    │  Interface      │
//! └─────────────┴─────────────┴─────────────────┘
//! ```

pub mod adapter;
pub mod client;
pub mod error;
pub mod ggen_server;
pub mod handlers;
pub mod message;
pub mod server;
pub mod util;
pub mod yawl_bridge;

// Re-export key components
pub use adapter::{AgentToToolAdapter, ToolToAgentAdapter};
pub use error::{A2aMcpError, A2aMcpResult};
pub use handlers::{
    handler::{HandlerContext, HandlerError, HandlerPriority, HandlerStatus},
    BatchProcessor, DataContentHandler, FileContentHandler, HandlerFactory, MessageHandler,
    MessageRouter, MultipartHandler, StreamHandler, TextContentHandler,
};
pub use message::{A2aMessageConverter, LlmRequest, LlmResponse};
pub use yawl_bridge::{
    TaskMapper, YawlEventPublisher, YawlEventType, YawlJoinType, YawlSplitType, YawlStateMapper,
    YawlTask, YawlTaskType,
};

// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
