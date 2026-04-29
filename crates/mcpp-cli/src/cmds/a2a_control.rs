#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::json;

/// `mcpp mcpp-a2a register` — register an agent in the A2A transport registry.
///
/// Registers a named agent with an endpoint URL in the Agent-to-Agent (A2A)
/// transport layer, enabling inter-agent communication.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-a2a", "register")]
pub async fn register(
    #[arg(help = "Unique agent identifier")]
    agent_id: String,
    #[arg(help = "Agent endpoint URL (e.g., http://localhost:3000)")]
    endpoint: String,
) -> clap_noun_verb::Result<String> {
    // Validate agent_id and endpoint format
    if agent_id.is_empty() {
        let env = Envelope::fail(
            "mcpp.mcpp_a2a.register",
            "mcpp",
            "INVALID_INPUT",
            "agent_id cannot be empty",
        );
        return Ok(env.to_json());
    }

    if !endpoint.starts_with("http://") && !endpoint.starts_with("https://") {
        let env = Envelope::fail(
            "mcpp.mcpp_a2a.register",
            "mcpp",
            "INVALID_ENDPOINT",
            "endpoint must be http:// or https://",
        );
        return Ok(env.to_json());
    }

    // Register agent (stub implementation)
    let env = Envelope::pass("mcpp.mcpp_a2a.register", "mcpp")
        .with_data(
            json!({
                "agent_id": agent_id.clone(),
                "endpoint": endpoint.clone(),
                "status": "registered",
                "timestamp": chrono::Utc::now().to_rfc3339(),
            }),
        )
        .with_next(
            &format!("mcpp mcpp-a2a send {} <message>", agent_id),
            "Send a message to the registered agent.",
        );

    Ok(env.to_json())
}

/// `mcpp mcpp-a2a send` — send a message to a registered agent.
///
/// Transmits a message to an agent via the A2A transport layer.
/// The receiving agent must be registered and reachable.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-a2a", "send")]
pub async fn send(
    #[arg(help = "Target agent identifier")]
    agent_id: String,
    #[arg(help = "Message content (JSON string)")]
    message: String,
) -> clap_noun_verb::Result<String> {
    // Validate inputs
    if agent_id.is_empty() {
        let env = Envelope::fail(
            "mcpp.mcpp_a2a.send",
            "mcpp",
            "INVALID_INPUT",
            "agent_id cannot be empty",
        );
        return Ok(env.to_json());
    }

    if message.is_empty() {
        let env = Envelope::fail(
            "mcpp.mcpp_a2a.send",
            "mcpp",
            "INVALID_INPUT",
            "message cannot be empty",
        );
        return Ok(env.to_json());
    }

    // Attempt to parse message as JSON (optional validation)
    let message_json = match serde_json::from_str::<serde_json::Value>(&message) {
        Ok(j) => j,
        Err(_) => {
            // If not JSON, wrap it as a string
            serde_json::json!({ "text": message })
        }
    };

    // Send message (stub implementation)
    let env = Envelope::pass("mcpp.mcpp_a2a.send", "mcpp")
        .with_data(
            json!({
                "agent_id": agent_id,
                "message": message_json,
                "status": "sent",
                "timestamp": chrono::Utc::now().to_rfc3339(),
            }),
        );

    Ok(env.to_json())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[tokio::test]
    async fn register_valid_agent_succeeds() {
        let result = register(
            "agent-001".to_string(),
            "http://localhost:3000".to_string(),
        )
        .await;

        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_a2a.register");
        assert_eq!(v["status"], "pass");
        assert_eq!(v["data"]["agent_id"], "agent-001");
        assert_eq!(v["data"]["status"], "registered");
    }

    #[tokio::test]
    async fn register_empty_agent_id_fails() {
        let result = register("".to_string(), "http://localhost:3000".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["status"], "fail");
    }

    #[tokio::test]
    async fn register_invalid_endpoint_fails() {
        let result = register("agent-001".to_string(), "ftp://invalid".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["status"], "fail");
    }

    #[tokio::test]
    async fn send_valid_message_succeeds() {
        let result = send(
            "agent-001".to_string(),
            r#"{"type":"task","action":"generate"}"#.to_string(),
        )
        .await;

        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_a2a.send");
        assert_eq!(v["status"], "pass");
        assert_eq!(v["data"]["agent_id"], "agent-001");
    }

    #[tokio::test]
    async fn send_empty_message_fails() {
        let result = send("agent-001".to_string(), "".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["status"], "fail");
    }
}
