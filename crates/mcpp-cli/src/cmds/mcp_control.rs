#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::json;

/// `mcpp mcpp-mcp serve` — start the MCP server over stdio or HTTP transport.
///
/// This command initializes the ggen-a2a-mcp server and serves it to MCP clients
/// (Claude Desktop, etc.). The transport can be specified via --transport flag.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope on success or failure.
#[verb("mcpp-mcp", "serve")]
pub async fn serve(
    #[arg(long, default_value = "stdio", help = "MCP transport: stdio or http")] transport: String,
) -> clap_noun_verb::Result<String> {
    // Route to appropriate transport
    match transport.as_str() {
        "stdio" => {
            // Serve over stdio (standard MCP client pattern)
            match ggen_a2a_mcp::server::serve_stdio().await {
                Ok(_) => {
                    let env = Envelope::pass("mcpp.mcpp_mcp.serve", "mcpp").with_data(json!({
                        "transport": "stdio",
                        "status": "listening"
                    }));
                    Ok(env.to_json())
                }
                Err(e) => {
                    let env = Envelope::fail(
                        "mcpp.mcpp_mcp.serve",
                        "mcpp",
                        "SERVE_ERROR",
                        &e.to_string(),
                    );
                    Ok(env.to_json())
                }
            }
        }
        "http" => {
            // Serve over HTTP (for testing or local integration)
            match ggen_a2a_mcp::server::serve_http("127.0.0.1:3000").await {
                Ok(_) => {
                    let env = Envelope::pass("mcpp.mcpp_mcp.serve", "mcpp").with_data(json!({
                        "transport": "http",
                        "address": "127.0.0.1:3000",
                        "status": "listening"
                    }));
                    Ok(env.to_json())
                }
                Err(e) => {
                    let env = Envelope::fail(
                        "mcpp.mcpp_mcp.serve",
                        "mcpp",
                        "SERVE_ERROR",
                        &e.to_string(),
                    );
                    Ok(env.to_json())
                }
            }
        }
        other => {
            let env = Envelope::fail(
                "mcpp.mcpp_mcp.serve",
                "mcpp",
                "INVALID_TRANSPORT",
                &format!("Unknown transport: {}. Use 'stdio' or 'http'.", other),
            );
            Ok(env.to_json())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[tokio::test]
    async fn serve_stdio_returns_json_envelope() {
        // Note: This test returns immediately with a stub envelope
        // Full serve testing requires actual stdio setup
        let result = serve("stdio".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).expect("output must be JSON");
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_mcp.serve");
    }

    #[tokio::test]
    async fn serve_http_returns_json_envelope() {
        let result = serve("http".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).expect("output must be JSON");
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["target"], "mcpp");
    }

    #[tokio::test]
    async fn serve_invalid_transport_returns_error() {
        let result = serve("invalid".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).expect("output must be JSON");
        assert_eq!(v["status"], "fail");
        assert!(!v["errors"].as_array().unwrap().is_empty());
    }
}
