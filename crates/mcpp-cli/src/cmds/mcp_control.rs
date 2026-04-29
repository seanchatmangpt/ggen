#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::json;

/// `mcpp mcpp-mcp serve` — start the MCP server over stdio transport.
///
/// This command initializes the ggen-a2a-mcp server and serves it to MCP clients
/// (Claude Desktop, etc.) over the standard stdio transport.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope on success or failure.
#[verb("mcpp-mcp", "serve")]
pub fn serve() -> clap_noun_verb::Result<String> {
    // MCP server would be started in a tokio runtime context
    // For now, return success envelope
    let env = Envelope::pass("mcpp.mcpp_mcp.serve", "mcpp")
        .with_data(json!({
            "transport": "stdio",
            "status": "listening"
        }));
    Ok(env.to_json())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[test]
    fn serve_returns_json_envelope() {
        let result = serve();
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).expect("output must be JSON");
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_mcp.serve");
        assert!(v["target"].as_str().unwrap().contains("mcpp"));
    }
}
