//! Echo Agent — Simplest possible A2A agent using AgentBuilder
//! Ported from EmilLindfors/a2a-rs examples

use a2a_agents::core::AgentBuilder;
use a2a_rs::domain::{A2AError, Message, Part, Role, Task, TaskState};
use a2a_rs::port::AsyncMessageHandler;
use async_trait::async_trait;
use uuid::Uuid;

#[derive(Clone)]
struct EchoHandler;

#[async_trait]
impl AsyncMessageHandler for EchoHandler {
    async fn process_message(
        &self,
        task_id: &str,
        message: &Message,
        _session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        let text = message
            .parts
            .iter()
            .find_map(|part| match part {
                Part::Text { text, .. } => Some(text.clone()),
                _ => None,
            })
            .unwrap_or_else(|| "No text provided".to_string());

        let response = Message::builder()
            .role(Role::Agent)
            .parts(vec![Part::text(format!("Echo: {}", text))])
            .message_id(Uuid::new_v4().to_string())
            .context_id(message.context_id.clone().unwrap_or_default())
            .build();

        Ok(Task::builder()
            .id(task_id.to_string())
            .context_id(message.context_id.clone().unwrap_or_default())
            .status(a2a_rs::domain::TaskStatus {
                state: TaskState::Completed,
                message: Some(response.clone()),
                timestamp: Some(chrono::Utc::now()),
            })
            .history(vec![message.clone(), response])
            .build())
    }

    async fn validate_message(&self, message: &Message) -> Result<(), A2AError> {
        if message.parts.is_empty() {
            return Err(A2AError::ValidationError {
                field: "parts".to_string(),
                message: "Message must contain at least one part".to_string(),
            });
        }
        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    println!("Echo Agent starting on http://127.0.0.1:3030");
    println!("Agent card: http://127.0.0.1:3030/.well-known/agent.json");

    AgentBuilder::from_file("echo-agent.toml")?
        .with_handler(EchoHandler)
        .build_with_auto_storage()
        .await?
        .run()
        .await?;

    Ok(())
}
