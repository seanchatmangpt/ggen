pub mod protocol;
pub mod tools;
pub mod server;

pub use server::McpServer;

// Suppress unused import warning — bp-core and domain are listed as deps
// for future use by MCP tool handlers that read/write entities.
use bp_core as _;
use domain as _;
