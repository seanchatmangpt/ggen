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

pub mod error;
pub mod message;
pub mod adapter;
pub mod client;
pub mod server;
pub mod transport;
pub mod util;
pub mod registry;
pub mod handlers;
pub mod mcp;
pub mod yawl_bridge;

// Re-export key components
pub use error::{A2aMcpError, A2aMcpResult};
pub use adapter::{AgentToToolAdapter, ToolToAgentAdapter};
pub use message::{A2aMessageConverter, LlmRequest, LlmResponse};
pub use transport::{McpTransport, McpRequest, McpResponse, McpErrorCode, McpToolDefinition};
pub use registry::{McpTool, McpToolRegistry, McpToolBuilder, ToolExecutionResult, register_core_tools};
pub use handlers::{
    MessageHandler, MessageRouter, HandlerFactory, BatchProcessor,
    TextContentHandler, FileContentHandler, DataContentHandler,
    MultipartHandler, StreamHandler,
    handler::{HandlerContext, HandlerError, HandlerPriority, HandlerStatus},
};
pub use yawl_bridge::{
    YawlStateMapper, TaskMapper, YawlEventPublisher,
    YawlTask, YawlTaskType, YawlSplitType, YawlJoinType, YawlEventType,
};

// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
