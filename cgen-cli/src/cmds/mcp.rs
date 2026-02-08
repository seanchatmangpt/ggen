//! MCP Bridge Commands
//!
//! This module provides MCP (Model Context Protocol) and A2A (Agent-to-Agent) configuration
//! and server management commands using the clap-noun-verb pattern.
//!
//! ## Command Structure
//!
//! ### Configuration Commands
//! - `ggen mcp config init [--file <path>]` - Initialize config file
//! - `ggen mcp config validate [--file <path>]` - Validate configuration
//! - `ggen mcp config show [--effective]` - Show effective configuration
//!
//! ### Server Commands
//! - `ggen mcp server start [--config <file>] [--port <port>]` - Start MCP server
//! - `ggen mcp server stop [--force]` - Stop MCP server
//! - `ggen mcp server status` - Show server status
//!
//! ### Tool Commands
//! - `ggen mcp list [--verbose]` - List all MCP tools
//! - `ggen mcp bridge <agent> [--tool-name <name>]` - Bridge an agent as MCP tool
//! - `ggen mcp status <tool> [--schema]` - Show MCP tool status
//! - `ggen mcp schemas [--detailed]` - List MCP tool schemas
//! - `ggen mcp test <tool> [--args <json>]` - Test MCP tool

// Include the mcp submodule which contains all the MCP functionality
mod mcp_submodule;
pub use mcp_submodule::*;
