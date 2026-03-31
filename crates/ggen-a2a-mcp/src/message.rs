//! Message conversion between A2A and LLM formats

use crate::error::{A2aMcpError, A2aMcpResult};
use a2a_generated::converged::message::{ConvergedMessage, UnifiedContent};
use tracing::warn;

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
    pub fn llm_response_to_a2a(
        &self, response: &LlmResponse, original_message: &ConvergedMessage,
    ) -> A2aMcpResult<ConvergedMessage> {
        // Create a new ConvergedMessage with the LLM response
        let mut message = ConvergedMessage::text(
            format!("llm-response-{}", uuid::Uuid::new_v4()),
            original_message
                .target
                .clone()
                .unwrap_or_else(|| "llm-bridge".to_string()),
            response.content.clone(),
        );

        // Set source as the original target (i.e., who processed it)
        message.source = original_message.source.clone();

        // Set target to the original message's target
        message.target = original_message.target.clone();

        Ok(message)
    }

    /// Extract nested multipart content recursively
    fn extract_nested_multipart(
        &self, parts: &[UnifiedContent], depth: usize,
    ) -> A2aMcpResult<String> {
        let indent = "  ".repeat(depth);
        let mut content_parts = Vec::new();

        for part in parts {
            match part {
                UnifiedContent::Text { content, .. } => {
                    content_parts.push(format!("{}{}", indent, content));
                }
                UnifiedContent::Data { data, .. } => {
                    let json = serde_json::to_string_pretty(data)?;
                    for line in json.lines() {
                        content_parts.push(format!("{}{}", indent, line));
                    }
                }
                UnifiedContent::File { file, .. } => {
                    if let Some(name) = &file.name {
                        warn!(
                            "{}Nested file at depth {}: {} (size: {:?})",
                            indent, depth, name, file.size
                        );
                        content_parts.push(format!(
                            "{}[File: {} ({} bytes)]",
                            indent,
                            name,
                            file.size.unwrap_or(0)
                        ));
                    } else if let Some(uri) = &file.uri {
                        warn!("{}Nested file at depth {}: {}", indent, depth, uri);
                        content_parts.push(format!("{}[File at: {}]", indent, uri));
                    }
                }
                UnifiedContent::Stream { stream_id, .. } => {
                    warn!("{}Nested stream at depth {}: {}", indent, depth, stream_id);
                    content_parts.push(format!("{}[Stream ID: {}]", indent, stream_id));
                }
                UnifiedContent::Multipart {
                    parts: nested_parts,
                    ..
                } => {
                    warn!(
                        "{}Deeply nested multipart at depth {} with {} parts",
                        indent,
                        depth,
                        nested_parts.len()
                    );
                    let nested_content = self.extract_nested_multipart(nested_parts, depth + 1)?;
                    content_parts.push(nested_content);
                }
            }
        }

        Ok(content_parts.join("\n\n"))
    }

    /// Extract text content from a ConvergedMessage
    fn extract_content(&self, message: &ConvergedMessage) -> A2aMcpResult<String> {
        match &message.payload.content {
            UnifiedContent::Text { content, .. } => Ok(content.clone()),
            UnifiedContent::Data { data, .. } => {
                serde_json::to_string_pretty(data).map_err(A2aMcpError::from)
            }
            UnifiedContent::File { file, .. } => {
                if let Some(uri) = &file.uri {
                    Ok(format!("File content available at: {}", uri))
                } else if let Some(bytes) = &file.bytes {
                    Ok(format!("Base64 encoded file content: {}", bytes))
                } else {
                    Err(A2aMcpError::Translation(
                        "File content has no URI or bytes".to_string(),
                    ))
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
                        UnifiedContent::File { file, .. } => {
                            // Preserve file metadata with warning
                            if let Some(name) = &file.name {
                                warn!(
                                    "File content in multipart: {} (size: {:?})",
                                    name, file.size
                                );
                                content_parts.push(format!(
                                    "[File: {} ({} bytes)]",
                                    name,
                                    file.size.unwrap_or(0)
                                ));
                            } else if let Some(uri) = &file.uri {
                                warn!("File content in multipart: {}", uri);
                                content_parts.push(format!("[File at: {}]", uri));
                            } else {
                                warn!("File content in multipart: no name or URI");
                                content_parts.push("[File content without identifier]".to_string());
                            }
                        }
                        UnifiedContent::Stream { stream_id, .. } => {
                            // Preserve stream ID with warning
                            warn!("Stream content in multipart: {}", stream_id);
                            content_parts.push(format!("[Stream ID: {}]", stream_id));
                        }
                        UnifiedContent::Multipart {
                            parts: nested_parts,
                            ..
                        } => {
                            // RECURSIVE: Extract nested multipart content
                            warn!(
                                "Nested multipart detected with {} parts - extracting recursively",
                                nested_parts.len()
                            );
                            let nested_content = self.extract_nested_multipart(nested_parts, 1)?;
                            content_parts.push(nested_content);
                        }
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
        ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle, MessagePriority,
        MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    };
    use chrono::Utc;

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

        let result = converter
            .llm_response_to_a2a(&llm_response, &original)
            .unwrap();
        assert_eq!(result.source, "user");
        assert_eq!(result.target, Some("llm-bridge".to_string()));
    }

    #[test]
    fn test_data_content_extraction() {
        let converter = A2aMessageConverter::new();
        let mut data = serde_json::Map::new();
        data.insert(
            "key".to_string(),
            serde_json::Value::String("value".to_string()),
        );

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
                content: UnifiedContent::Data { data, schema: None },
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

    #[test]
    fn test_multipart_with_file_and_stream() {
        let converter = A2aMessageConverter::new();
        use a2a_generated::converged::message::UnifiedFileContent;

        let message = ConvergedMessage {
            message_id: "multipart-mixed-msg".to_string(),
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
                            content: "Text part".to_string(),
                            metadata: None,
                        },
                        UnifiedContent::File {
                            file: UnifiedFileContent {
                                name: Some("document.pdf".to_string()),
                                mime_type: Some("application/pdf".to_string()),
                                bytes: None,
                                uri: Some("file:///path/to/document.pdf".to_string()),
                                size: Some(1024),
                                hash: None,
                            },
                            metadata: None,
                        },
                        UnifiedContent::Stream {
                            stream_id: "stream-abc123".to_string(),
                            chunk_size: 1024,
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
        assert!(request.user_content.contains("Text part"));
        assert!(request.user_content.contains("[File: document.pdf"));
        assert!(request.user_content.contains("1024 bytes"));
        assert!(request.user_content.contains("[Stream ID: stream-abc123]"));
    }

    #[test]
    fn test_nested_multipart_recursive_extraction() {
        let converter = A2aMessageConverter::new();

        // Create nested multipart structure
        let inner_multipart = UnifiedContent::Multipart {
            parts: vec![
                UnifiedContent::Text {
                    content: "Inner text 1".to_string(),
                    metadata: None,
                },
                UnifiedContent::Text {
                    content: "Inner text 2".to_string(),
                    metadata: None,
                },
            ],
            boundary: Some("inner-boundary".to_string()),
        };

        let message = ConvergedMessage {
            message_id: "nested-multipart-msg".to_string(),
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
                            content: "Outer text".to_string(),
                            metadata: None,
                        },
                        inner_multipart,
                    ],
                    boundary: Some("outer-boundary".to_string()),
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
        // Verify all content is extracted
        assert!(request.user_content.contains("Outer text"));
        assert!(request.user_content.contains("Inner text 1"));
        assert!(request.user_content.contains("Inner text 2"));
        // Verify no data loss occurred
        assert!(!request.user_content.contains("[Unsupported content type"));
    }

    #[test]
    fn test_deeply_nested_multipart_with_mixed_content() {
        let converter = A2aMessageConverter::new();
        use a2a_generated::converged::message::UnifiedFileContent;

        // Create deeply nested structure: Multipart -> Multipart -> Text/File/Stream
        let level_2_multipart = UnifiedContent::Multipart {
            parts: vec![
                UnifiedContent::Text {
                    content: "Level 2 text".to_string(),
                    metadata: None,
                },
                UnifiedContent::File {
                    file: UnifiedFileContent {
                        name: Some("deep-file.txt".to_string()),
                        mime_type: Some("text/plain".to_string()),
                        bytes: None,
                        uri: None,
                        size: Some(512),
                        hash: None,
                    },
                    metadata: None,
                },
            ],
            boundary: Some("level-2-boundary".to_string()),
        };

        let level_1_multipart = UnifiedContent::Multipart {
            parts: vec![
                UnifiedContent::Text {
                    content: "Level 1 text".to_string(),
                    metadata: None,
                },
                level_2_multipart,
                UnifiedContent::Stream {
                    stream_id: "level-1-stream".to_string(),
                    chunk_size: 2048,
                    metadata: None,
                },
            ],
            boundary: Some("level-1-boundary".to_string()),
        };

        let message = ConvergedMessage {
            message_id: "deep-nested-msg".to_string(),
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
                content: level_1_multipart,
                context: None,
                hints: None,
                integrity: None,
            },
            routing: default_routing(),
            lifecycle: default_lifecycle(),
            extensions: None,
        };

        let request = converter.a2a_to_llm_request(&message).unwrap();
        // Verify all nested content is extracted
        assert!(request.user_content.contains("Level 1 text"));
        assert!(request.user_content.contains("Level 2 text"));
        assert!(request.user_content.contains("[File: deep-file.txt"));
        assert!(request.user_content.contains("512 bytes"));
        assert!(request.user_content.contains("[Stream ID: level-1-stream]"));
        // Verify no data loss
        assert!(!request.user_content.contains("[Unsupported content type"));
    }

    #[test]
    fn test_multipart_with_data_serialization() {
        let converter = A2aMessageConverter::new();

        let mut data = serde_json::Map::new();
        data.insert(
            "username".to_string(),
            serde_json::Value::String("alice".to_string()),
        );
        data.insert(
            "role".to_string(),
            serde_json::Value::String("admin".to_string()),
        );

        let message = ConvergedMessage {
            message_id: "multipart-data-msg".to_string(),
            source: "test-agent".to_string(),
            target: Some("test-target".to_string()),
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Query,
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
                            content: "User data:".to_string(),
                            metadata: None,
                        },
                        UnifiedContent::Data { data, schema: None },
                    ],
                    boundary: Some("data-boundary".to_string()),
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
        assert!(request.user_content.contains("User data:"));
        assert!(request.user_content.contains("alice"));
        assert!(request.user_content.contains("admin"));
    }

    #[test]
    fn test_no_wildcard_data_loss() {
        let converter = A2aMessageConverter::new();
        use a2a_generated::converged::message::UnifiedFileContent;

        // Test that all content types are handled, nothing falls through to wildcard
        let message = ConvergedMessage {
            message_id: "no-data-loss-msg".to_string(),
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
                            content: "Text".to_string(),
                            metadata: None,
                        },
                        UnifiedContent::Data {
                            data: serde_json::Map::from_iter(
                                vec![(
                                    "key".to_string(),
                                    serde_json::Value::String("value".to_string()),
                                )]
                                .into_iter(),
                            ),
                            schema: None,
                        },
                        UnifiedContent::File {
                            file: UnifiedFileContent {
                                name: Some("test.txt".to_string()),
                                mime_type: None,
                                bytes: None,
                                uri: None,
                                size: Some(100),
                                hash: None,
                            },
                            metadata: None,
                        },
                        UnifiedContent::Stream {
                            stream_id: "stream-123".to_string(),
                            chunk_size: 4096,
                            metadata: None,
                        },
                    ],
                    boundary: Some("test-boundary".to_string()),
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
        // Verify none of the content was discarded
        assert!(!request.user_content.contains("[Unsupported content type]"));
        assert!(request.user_content.contains("Text"));
        assert!(request.user_content.contains("key"));
        assert!(request.user_content.contains("value"));
        assert!(request.user_content.contains("[File: test.txt"));
        assert!(request.user_content.contains("[Stream ID: stream-123]"));
    }
}
