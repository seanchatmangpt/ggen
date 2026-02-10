//! Message conversion between A2A and LLM formats

use crate::error::{A2aMcpError, A2aMcpResult};
use a2a_generated::converged::message::{ConvergedMessage, UnifiedContent};

/// Converts between A2A ConvergedMessage and LLM prompt/response formats
#[derive(Debug, Default)]
pub struct A2aMessageConverter {
    system_prompt: String,
}

impl A2aMessageConverter {
    /// Create a new message converter
    pub fn new() -> Self {
        Self {
            system_prompt: "You are a helpful AI assistant.".to_string(),
        }
    }

    /// Create a new converter with a custom system prompt
    pub fn with_system_prompt<S: Into<String>>(mut self, prompt: S) -> Self {
        self.system_prompt = prompt.into();
        self
    }

    /// Convert A2A ConvergedMessage to LLM request format
    pub fn a2a_to_llm_request(&self, message: &ConvergedMessage) -> A2aMcpResult<LlmRequest> {
        let content = self.extract_content(message)?;

        Ok(LlmRequest {
            system_prompt: self.system_prompt.clone(),
            user_content: content,
            message_id: message.message_id.clone(),
        })
    }

    /// Convert LLM response to A2A ConvergedMessage
    pub fn llm_response_to_a2a(&self, response: &LlmResponse, original_message: &ConvergedMessage) -> A2aMcpResult<ConvergedMessage> {
        // Create a new ConvergedMessage with the LLM response
        let mut message = ConvergedMessage::text(
            format!("llm-response-{}", uuid::Uuid::new_v4()),
            original_message.target.clone().unwrap_or_else(|| "llm-bridge".to_string()),
            response.content.clone(),
        );

        // Set source as the original target
        message.source = original_message.source.clone();

        Ok(message)
    }

    /// Extract text content from a ConvergedMessage
    fn extract_content(&self, message: &ConvergedMessage) -> A2aMcpResult<String> {
        match &message.payload.content {
            UnifiedContent::Text { content, .. } => Ok(content.clone()),
            UnifiedContent::Data { data, .. } => {
                serde_json::to_string_pretty(data)
                    .map_err(A2aMcpError::from)
            }
            UnifiedContent::File { file, .. } => {
                if let Some(uri) = &file.uri {
                    Ok(format!("File content available at: {}", uri))
                } else if let Some(bytes) = &file.bytes {
                    Ok(format!("Base64 encoded file content: {}", bytes))
                } else {
                    Err(A2aMcpError::Translation("File content has no URI or bytes".to_string()))
                }
            }
            UnifiedContent::Multipart { parts, .. } => {
                let mut content_parts = Vec::new();
                for part in parts {
                    match part {
                        UnifiedContent::Text { content, .. } => {
                            content_parts.push(content.clone());
                        }
                        UnifiedContent::Data { data, .. } => {
                            content_parts.push(serde_json::to_string_pretty(data)?);
                        }
                        _ => content_parts.push("[Unsupported content type]".to_string()),
                    }
                }
                Ok(content_parts.join("\n\n---\n\n"))
            }
            UnifiedContent::Stream { stream_id, .. } => {
                Ok(format!("Streaming content with ID: {}", stream_id))
            }
        }
    }
}

/// LLM request format
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LlmRequest {
    pub system_prompt: String,
    pub user_content: String,
    pub message_id: String,
}

/// LLM response format
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LlmResponse {
    pub content: String,
    pub model: String,
    pub usage: Option<TokenUsage>,
}

/// Token usage information
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TokenUsage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use a2a_generated::converged::message::{
        ConvergedPayload, MessageEnvelope, MessageLifecycle, MessagePriority,
        ConvergedMessageType, MessageState, MessageRouting, QoSRequirements,
        ReliabilityLevel,
    };
    use chrono::Utc;
    use std::collections::HashMap;

    /// Helper to create a default message routing
    fn default_routing() -> MessageRouting {
        MessageRouting {
            path: vec!["test-agent".to_string()],
            metadata: None,
            qos: QoSRequirements {
                reliability: ReliabilityLevel::AtLeastOnce,
                latency: None,
                throughput: None,
            },
        }
    }

    /// Helper to create a default message lifecycle
    fn default_lifecycle() -> MessageLifecycle {
        MessageLifecycle {
            state: MessageState::Created,
            history: Vec::new(),
            timeout: None,
        }
    }

    #[test]
    fn test_text_content_extraction() {
        let converter = A2aMessageConverter::new();
        let message = ConvergedMessage {
            message_id: "test-msg".to_string(),
            source: "test-agent".to_string(),
            target: Some("test-target".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Task,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "text/plain".to_string(),
                correlation_id: None,
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Text {
                    content: "Hello, world!".to_string(),
                    metadata: None,
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let request = converter.a2a_to_llm_request(&message).unwrap();
        assert_eq!(request.user_content, "Hello, world!");
    }

    #[test]
    fn test_llm_response_to_a2a() {
        let converter = A2aMessageConverter::new();
        let original = ConvergedMessage {
            message_id: "original-msg".to_string(),
            source: "user".to_string(),
            target: Some("llm-bridge".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Task,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "text/plain".to_string(),
                correlation_id: None,
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Text {
                    content: "Original message".to_string(),
                    metadata: None,
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let llm_response = LlmResponse {
            content: "LLM response".to_string(),
            model: "gpt-4".to_string(),
            usage: None,
        };

        let result = converter.llm_response_to_a2a(&llm_response, &original).unwrap();
        assert_eq!(result.source, "user");
        assert_eq!(result.target, Some("llm-bridge".to_string()));
    }

    #[test]
    fn test_data_content_extraction() {
        let converter = A2aMessageConverter::new();
        let mut data = HashMap::new();
        data.insert("key".to_string(), serde_json::Value::String("value".to_string()));

        let message = ConvergedMessage {
            message_id: "data-msg".to_string(),
            source: "test-agent".to_string(),
            target: Some("test-target".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Query,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
                correlation_id: None,
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Data {
                    data,
                    schema: None,
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let request = converter.a2a_to_llm_request(&message).unwrap();
        assert!(request.user_content.contains("key"));
        assert!(request.user_content.contains("value"));
    }

    #[test]
    fn test_file_content_extraction() {
        let converter = A2aMessageConverter::new();
        use a2a_generated::converged::message::UnifiedFileContent;

        let message = ConvergedMessage {
            message_id: "file-msg".to_string(),
            source: "test-agent".to_string(),
            target: Some("test-target".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Direct,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/octet-stream".to_string(),
                correlation_id: None,
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::File {
                    file: UnifiedFileContent {
                        name: Some("test.txt".to_string()),
                        mime_type: Some("text/plain".to_string()),
                        bytes: Some("SGVsbG8=".to_string()),
                        uri: None,
                        size: Some(5),
                        hash: None,
                    },
                    metadata: None,
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let request = converter.a2a_to_llm_request(&message).unwrap();
        assert!(request.user_content.contains("Base64 encoded"));
    }

    #[test]
    fn test_multipart_content_extraction() {
        let converter = A2aMessageConverter::new();

        let message = ConvergedMessage {
            message_id: "multipart-msg".to_string(),
            source: "test-agent".to_string(),
            target: Some("test-target".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Direct,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "multipart/mixed".to_string(),
                correlation_id: None,
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Multipart {
                    parts: vec![
                        UnifiedContent::Text {
                            content: "Part 1".to_string(),
                            metadata: None,
                        },
                        UnifiedContent::Text {
                            content: "Part 2".to_string(),
                            metadata: None,
                        },
                    ],
                    boundary: Some("boundary".to_string()),
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let request = converter.a2a_to_llm_request(&message).unwrap();
        assert!(request.user_content.contains("Part 1"));
        assert!(request.user_content.contains("Part 2"));
        assert!(request.user_content.contains("---"));
    }
}
