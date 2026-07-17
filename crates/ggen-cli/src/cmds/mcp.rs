//! MCP Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements Model Context Protocol commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use ggen_lsp::a2a_mcp::mcp_server::GgenMcpServer;
use serde::Serialize;

#[derive(Serialize)]
struct McpOutput {
    status: String,
    transport: String,
}

/// Start the MCP server using rmcp
#[verb]
fn start_server(transport: Option<String>) -> Result<McpOutput> {
    let transport_val = transport.unwrap_or_else(|| "stdio".to_string());

    if transport_val == "stdio" {
        // Run async code synchronously via runtime block_on
        crate::runtime::block_on(async move {
            GgenMcpServer::start_stdio().await.map_err(|e| {
                crate::utils::error::Error::new(&format!("MCP Server failed: {}", e))
            })
        })
        .map_err(|e: crate::utils::error::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?
        .map_err(|e: crate::utils::error::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?;
    } else {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Only stdio transport is supported currently.".to_string(),
        ));
    }

    Ok(McpOutput {
        status: "Running".to_string(),
        transport: transport_val,
    })
}
