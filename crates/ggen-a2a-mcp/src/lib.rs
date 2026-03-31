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
pub mod correlation;
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

/// OTEL semantic convention attribute names used across ggen-a2a-mcp.
///
/// All tracing spans and structured log fields should reference these
/// constants rather than inline string literals, ensuring consistency
/// with the semconv schema and simplifying future schema migrations.
pub mod otel_attrs {
    // --- Service ---
    pub const SERVICE_NAME: &str = "service.name";
    pub const SERVICE_VERSION: &str = "service.version";

    // --- Operation ---
    pub const OPERATION_NAME: &str = "operation.name";
    pub const OPERATION_TYPE: &str = "operation.type";

    // --- A2A protocol ---
    pub const CORRELATION_ID: &str = "a2a.correlation_id";
    pub const CAUSATION_CHAIN: &str = "a2a.causation_chain";
    pub const MESSAGE_ID: &str = "a2a.message_id";
    pub const MESSAGE_TYPE: &str = "a2a.message_type";
    pub const SOURCE_AGENT: &str = "a2a.source";
    pub const TARGET_AGENT: &str = "a2a.target";
    pub const A2A_OPERATION_NAME: &str = "a2a.operation_name";

    // --- YAWL workflow ---
    pub const WORKFLOW_ID: &str = "yawl.workflow_id";
    pub const TASK_ID: &str = "yawl.task_id";
    pub const TASK_NAME: &str = "yawl.task_name";
    pub const TASK_TYPE: &str = "yawl.task_type";
    pub const YAWL_STATE_FROM: &str = "yawl.state.from";
    pub const YAWL_STATE_TO: &str = "yawl.state.to";
    pub const YAWL_OLD_STATE: &str = "yawl.old_state";
    pub const YAWL_NEW_STATE: &str = "yawl.new_state";
    pub const YAWL_GATEWAY_ID: &str = "yawl.gateway_id";
    pub const YAWL_GATEWAY_TYPE: &str = "yawl.gateway_type";

    // --- LLM ---
    pub const LLM_MODEL: &str = "llm.model";
    pub const LLM_PROMPT_TOKENS: &str = "llm.prompt_tokens";
    pub const LLM_COMPLETION_TOKENS: &str = "llm.completion_tokens";
    pub const LLM_TOTAL_TOKENS: &str = "llm.total_tokens";
    pub const LLM_PROMPT_LENGTH: &str = "llm.prompt_length";
    pub const LLM_OUTPUT_LENGTH: &str = "llm.output_length";

    // --- MCP ---
    pub const MCP_TOOL_NAME: &str = "mcp.tool_name";
    pub const MCP_ONTOLOGY_PATH: &str = "mcp.ontology_path";
    pub const MCP_SPARQL_QUERY_LENGTH: &str = "mcp.sparql_query_length";
    pub const MCP_TTL_LENGTH: &str = "mcp.ttl_length";
    pub const MCP_FILES_GENERATED: &str = "mcp.files_generated";
    pub const MCP_RECEIPT: &str = "mcp.receipt";
    pub const MCP_TRIPLE_COUNT: &str = "mcp.triple_count";
    pub const MCP_ERROR_COUNT: &str = "mcp.error_count";
    pub const MCP_PROJECT_PATH: &str = "mcp.project_path";
    pub const MCP_QUERY_PATH: &str = "mcp.query_path";
    pub const MCP_TEMPLATE_PATH: &str = "mcp.template_path";

    // --- Pipeline ---
    pub const PIPELINE_OPERATION: &str = "pipeline.operation";
    pub const PIPELINE_BATCH_SIZE: &str = "pipeline.batch_size";
    pub const PIPELINE_TOTAL: &str = "pipeline.total";
    pub const PIPELINE_SUCCESSFUL: &str = "pipeline.successful";
    pub const PIPELINE_FAILED: &str = "pipeline.failed";
    pub const PIPELINE_DURATION_MS: &str = "pipeline.duration_ms";

    // --- Error ---
    pub const ERROR: &str = "error";
    pub const ERROR_TYPE: &str = "error.type";
    pub const ERROR_MESSAGE: &str = "error.message";
}
