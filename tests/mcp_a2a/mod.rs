//! MCP + A2A Integration Tests
//!
//! Comprehensive test suite for MCP (Model Context Protocol) integration with
//! A2A (Agent-to-Agent) protocol. Tests cover:
//! - Tool discovery and registration
//! - Dynamic tool loading
//! - Schema validation
//! - Error handling
//! - Caching strategies
//! - Multi-server scenarios
//! - Mock A2A server for testing
//! - Mock MCP server for testing
//!
//! ## Test Structure
//!
//! Tests follow Chicago TDD AAA pattern:
//! - **Arrange**: Set up test fixtures and state
//! - **Act**: Execute the function under test
//! - **Assert**: Verify expected outcomes
//!
//! Tests use real collaborators (in-memory mocks) rather than external dependencies,
//! following state-based testing principles.
//!
//! ## Modules
//!
//! - [`mock_a2a_server`] - Mock A2A server implementation for integration testing
//! - [`mock_mcp_server`] - Mock MCP server implementation for integration testing
//! - [`common`] - Shared test utilities, fixtures, and assertions
//! - [`tool_discovery_tests`] - Tool discovery and registration tests
//! - [`transport_tests`] - Transport layer tests

pub mod mock_a2a_server;
pub mod mock_mcp_server;
pub mod common;
mod tool_discovery_tests;
mod transport_tests;

// Re-export key types from mock_a2a_server
pub use mock_a2a_server::{
    MockA2AServer,
    AgentConfig,
    AgentStatus,
    AgentState,
    TaskStatus,
    TaskState as A2ATaskState,
    MockMessage,
    MockEvent,
    MockEventType,
    TransportType,
    JsonRpcRequest as A2AJsonRpcRequest,
    JsonRpcResponse as A2AJsonRpcResponse,
    JsonRpcError as A2AJsonRpcError,
};

// Re-export key types from mock_mcp_server
pub use mock_mcp_server::{
    MockMcpServer,
    ToolHandler,
    ToolHandlerBuilder,
    JsonRpcMessage,
    JsonRpcResponse as McpJsonRpcResponse,
    McpError,
    ErrorMode,
    ServerConfig,
    ServerStats,
    // Test helpers
    echo_tool,
    constant_tool,
    failing_tool,
    create_server_with_standard_tools,
};

// Re-export common test utilities
pub use common::{
    fixtures::*,
    assertions::*,
};

pub use tool_discovery_tests::*;
pub use transport_tests::*;
