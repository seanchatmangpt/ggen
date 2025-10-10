//! Core types for LLM interactions
//!
//! Defines the common data structures used across all LLM providers,
//! including messages, requests, and responses.

use serde::{Deserialize, Serialize};

/// Role of a message participant
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    /// System message for instructions
    System,
    /// User message
    User,
    /// Assistant/AI response
    Assistant,
}

/// A single message in a conversation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    /// Role of the message sender
    pub role: Role,
    /// Content of the message
    pub content: String,
}

impl Message {
    /// Create a new message
    pub fn new(role: Role, content: impl Into<String>) -> Self {
        Self {
            role,
            content: content.into(),
        }
    }

    /// Create a system message
    pub fn system(content: impl Into<String>) -> Self {
        Self::new(Role::System, content)
    }

    /// Create a user message
    pub fn user(content: impl Into<String>) -> Self {
        Self::new(Role::User, content)
    }

    /// Create an assistant message
    pub fn assistant(content: impl Into<String>) -> Self {
        Self::new(Role::Assistant, content)
    }
}

/// Chat completion request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatRequest {
    /// Model identifier (e.g., "gpt-4", "claude-3-opus")
    pub model: String,
    /// Conversation messages
    pub messages: Vec<Message>,
    /// Temperature for response randomness (0.0 - 2.0)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f32>,
    /// Maximum tokens to generate
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u32>,
    /// Enable streaming responses
    #[serde(default)]
    pub stream: bool,
    /// Top-p nucleus sampling
    #[serde(skip_serializing_if = "Option::is_none")]
    pub top_p: Option<f32>,
}

impl ChatRequest {
    /// Create a builder for ChatRequest
    pub fn builder() -> ChatRequestBuilder {
        ChatRequestBuilder::default()
    }
}

/// Builder for ChatRequest
#[derive(Default)]
/// Builder for constructing chat requests with fluent API
pub struct ChatRequestBuilder {
    model: Option<String>,
    messages: Vec<Message>,
    temperature: Option<f32>,
    max_tokens: Option<u32>,
    stream: bool,
    top_p: Option<f32>,
}

impl ChatRequestBuilder {
    /// Set the model
    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    /// Add a message
    pub fn message(mut self, role: impl Into<Role>, content: impl Into<String>) -> Self {
        self.messages.push(Message::new(role.into(), content));
        self
    }

    /// Add multiple messages
    pub fn messages(mut self, messages: Vec<Message>) -> Self {
        self.messages.extend(messages);
        self
    }

    /// Set temperature
    pub fn temperature(mut self, temp: f32) -> Self {
        self.temperature = Some(temp);
        self
    }

    /// Set max tokens
    pub fn max_tokens(mut self, tokens: u32) -> Self {
        self.max_tokens = Some(tokens);
        self
    }

    /// Enable streaming
    pub fn stream(mut self, enable: bool) -> Self {
        self.stream = enable;
        self
    }

    /// Set top-p
    pub fn top_p(mut self, p: f32) -> Self {
        self.top_p = Some(p);
        self
    }

    /// Build the ChatRequest
    pub fn build(self) -> Result<ChatRequest, String> {
        let model = self.model.ok_or("Model is required")?;
        if self.messages.is_empty() {
            return Err("At least one message is required".to_string());
        }

        Ok(ChatRequest {
            model,
            messages: self.messages,
            temperature: self.temperature,
            max_tokens: self.max_tokens,
            stream: self.stream,
            top_p: self.top_p,
        })
    }
}

/// Chat completion response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatResponse {
    /// Generated content
    pub content: String,
    /// Model used
    pub model: String,
    /// Token usage statistics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub usage: Option<TokenUsage>,
    /// Finish reason (e.g., "stop", "length")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
}

/// Token usage statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenUsage {
    /// Tokens in the prompt
    pub prompt_tokens: u32,
    /// Tokens in the completion
    pub completion_tokens: u32,
    /// Total tokens used
    pub total_tokens: u32,
}

impl Role {
    /// Convert string to Role
    pub fn parse_role(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "system" => Some(Role::System),
            "user" => Some(Role::User),
            "assistant" => Some(Role::Assistant),
            _ => None,
        }
    }
}

impl From<&str> for Role {
    fn from(s: &str) -> Self {
        Role::parse_role(s).unwrap_or(Role::User)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let msg = Message::user("Hello");
        assert_eq!(msg.role, Role::User);
        assert_eq!(msg.content, "Hello");
    }

    #[test]
    fn test_chat_request_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Hello")
            .temperature(0.7)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 1);
        assert_eq!(request.temperature, Some(0.7));
    }

    #[test]
    fn test_builder_validation() {
        let result = ChatRequest::builder().build();
        assert!(result.is_err());
    }

    #[test]
    fn test_role_from_str() {
        assert_eq!(Role::parse_role("user"), Some(Role::User));
        assert_eq!(Role::parse_role("SYSTEM"), Some(Role::System));
        assert_eq!(Role::parse_role("invalid"), None);
    }
}
