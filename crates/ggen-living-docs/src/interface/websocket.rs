//! WebSocket handlers for live updates

use actix_ws::Session;
use tracing::{debug, error};

/// WebSocket session handler
pub struct WsSession {
    session: Session,
}

impl WsSession {
    pub fn new(session: Session) -> Self {
        Self { session }
    }

    /// Send a message to the client
    pub async fn send(&mut self, message: &str) -> Result<(), actix_ws::Closed> {
        debug!("Sending WebSocket message: {}", message);
        self.session.text(message).await
    }

    /// Handle incoming message
    pub async fn handle_message(&mut self, message: &str) {
        debug!("Received WebSocket message: {}", message);

        // Parse and handle different message types
        if let Ok(value) = serde_json::from_str::<serde_json::Value>(message) {
            if let Some(msg_type) = value.get("type").and_then(|v| v.as_str()) {
                match msg_type {
                    "query" => self.handle_query(&value).await,
                    "subscribe" => self.handle_subscribe(&value).await,
                    _ => {
                        error!("Unknown message type: {}", msg_type);
                    }
                }
            }
        }
    }

    async fn handle_query(&mut self, _value: &serde_json::Value) {
        // Handle query messages
        let response = serde_json::json!({
            "type": "query_response",
            "results": []
        });

        if let Ok(text) = serde_json::to_string(&response) {
            let _ = self.send(&text).await;
        }
    }

    async fn handle_subscribe(&mut self, _value: &serde_json::Value) {
        // Handle subscription messages
        let response = serde_json::json!({
            "type": "subscribed",
            "status": "ok"
        });

        if let Ok(text) = serde_json::to_string(&response) {
            let _ = self.send(&text).await;
        }
    }
}
