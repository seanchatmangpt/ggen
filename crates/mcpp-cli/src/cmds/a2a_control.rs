#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::{json, Value};

/// `mcpp mcpp-a2a register` — register an agent in the A2A transport registry.
///
/// Registers a named agent for inter-agent communication via the A2A protocol.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-a2a", "register")]
pub fn register() -> clap_noun_verb::Result<String> {
    // Register agent (stub implementation)
    let env = Envelope::pass("mcpp.mcpp_a2a.register", "mcpp")
        .with_data(json!({
            "agent_id": "agent-001",
            "endpoint": "http://localhost:3000",
            "status": "registered",
            "timestamp": chrono::Utc::now().to_rfc3339(),
        }))
        .with_next(
            "mcpp mcpp-a2a send",
            "Send a message to the registered agent.",
        );

    Ok(env.to_json())
}

/// `mcpp mcpp-a2a send` — send a message to a registered agent.
///
/// Transmits a message to an agent via the A2A transport layer.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-a2a", "send")]
pub fn send() -> clap_noun_verb::Result<String> {
    // Send message (stub implementation)
    let env = Envelope::pass("mcpp.mcpp_a2a.send", "mcpp").with_data(json!({
        "agent_id": "agent-001",
        "message": {
            "type": "task",
            "action": "generate"
        },
        "status": "sent",
        "timestamp": chrono::Utc::now().to_rfc3339(),
    }));

    Ok(env.to_json())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_returns_json_envelope() {
        let result = register();
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_a2a.register");
        assert_eq!(v["status"], "pass");
        assert_eq!(v["data"]["status"], "registered");
    }

    #[test]
    fn send_returns_json_envelope() {
        let result = send();
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_a2a.send");
        assert_eq!(v["status"], "pass");
        assert_eq!(v["data"]["status"], "sent");
    }
}
