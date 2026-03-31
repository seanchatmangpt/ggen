//! A2A Message Handlers for ggen integration
//!
//! This module provides handlers for processing A2A messages with support
//! for all ConvergedMessage content types and proper error handling.

use crate::error::{A2aMcpError, A2aMcpResult};
use crate::otel_attrs;
use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, UnifiedContent, UnifiedFileContent,
};
use async_trait::async_trait;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tracing::info;

// Re-export handler traits and types
pub use handler::{HandlerContext, HandlerError, HandlerPriority, HandlerResult, HandlerStatus};

pub mod handler {
    use a2a_generated::converged::message::ConvergedMessage;

    use chrono::Utc;

    use std::collections::HashMap;

    /// Result type for message handlers
    pub type HandlerResult<T = ()> = Result<T, HandlerError>;

    /// Error type for message handlers
    #[derive(Debug, thiserror::Error)]
    pub enum HandlerError {
        #[error("Validation error: {0}")]
        Validation(String),

        #[error("Processing error: {0}")]
        Processing(String),

        #[error("IO error: {0}")]
        Io(#[from] std::io::Error),

        #[error("Serialization error: {0}")]
        Serialization(#[from] serde_json::Error),

        #[error("Generic error: {0}")]
        Generic(#[from] anyhow::Error),
    }

    /// Handler priority levels
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum HandlerPriority {
        Lowest = 0,
        Low = 1,
        Normal = 2,
        High = 3,
        Highest = 4,
        Critical = 5,
    }

    /// Handler execution status
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum HandlerStatus {
        Pending,
        Running,
        Completed,
        Failed,
        Skipped,
    }

    /// Context provided to handlers during execution
    #[derive(Debug, Clone)]
    pub struct HandlerContext {
        /// Handler name
        pub handler_name: String,

        /// Message being processed
        pub message: ConvergedMessage,

        /// Start time of processing
        pub start_time: chrono::DateTime<Utc>,

        /// Additional metadata
        pub metadata: HashMap<String, serde_json::Value>,
    }

    impl HandlerContext {
        /// Create a new handler context
        pub fn new(handler_name: String, message: ConvergedMessage) -> Self {
            Self {
                handler_name,
                message,
                start_time: Utc::now(),
                metadata: HashMap::new(),
            }
        }

        /// Add metadata to the context
        pub fn with_metadata(mut self, key: String, value: serde_json::Value) -> Self {
            self.metadata.insert(key, value);
            self
        }
    }
}

/// Trait for handling A2A messages
#[async_trait]
pub trait MessageHandler: Send + Sync + std::fmt::Debug {
    /// Handle the message and return a response
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage>;

    /// Check if this handler can process the given message type
    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool;

    /// Get the handler priority
    fn priority(&self) -> HandlerPriority;

    /// Get the handler name
    fn name(&self) -> &str;

    /// Get the supported message types
    fn supported_types(&self) -> Vec<ConvergedMessageType>;
}

/// Text content handler
#[derive(Debug, Clone)]
pub struct TextContentHandler {
    name: String,
    priority: HandlerPriority,
    supported_types: Vec<ConvergedMessageType>,
}

impl TextContentHandler {
    /// Create a new text content handler
    pub fn new() -> Self {
        Self {
            name: "TextContentHandler".to_string(),
            priority: HandlerPriority::Normal,
            supported_types: vec![
                ConvergedMessageType::Direct,
                ConvergedMessageType::Task,
                ConvergedMessageType::Query,
            ],
        }
    }

    /// Create a new text content handler with custom name
    pub fn with_name<S: Into<String>>(mut self, name: S) -> Self {
        self.name = name.into();
        self
    }

    /// Create a new text content handler with custom priority
    pub fn with_priority(mut self, priority: HandlerPriority) -> Self {
        self.priority = priority;
        self
    }

    /// Process text content
    fn process_text(&self, content: &str) -> A2aMcpResult<String> {
        // Basic text processing - can be extended
        Ok(format!("Processed: {}", content))
    }
}

impl Default for TextContentHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl MessageHandler for TextContentHandler {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let content = match &message.payload.content {
            UnifiedContent::Text { content, .. } => content,
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "validation",
                    error.message = "Expected text content",
                );
                let _guard = error_span.enter();
                return Err(HandlerError::Validation(
                    "Expected text content".to_string(),
                ));
            }
        };

        let processed = self
            .process_text(content)
            .map_err(|e| HandlerError::Processing(format!("Text processing failed: {}", e)))?;

        // Create response message
        let response = ConvergedMessage::text(
            format!("{}-response", message.message_id),
            message.source.clone(),
            processed,
        );

        info!(
            handler = %self.name,
            message_id = %message.message_id,
            "Processed text message"
        );

        Ok(response)
    }

    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool {
        self.supported_types.contains(message_type)
    }

    fn priority(&self) -> HandlerPriority {
        self.priority
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn supported_types(&self) -> Vec<ConvergedMessageType> {
        self.supported_types.clone()
    }
}

/// File content handler
#[derive(Debug, Clone)]
pub struct FileContentHandler {
    name: String,
    priority: HandlerPriority,
    supported_types: Vec<ConvergedMessageType>,
    max_file_size: usize,
}

impl FileContentHandler {
    /// Create a new file content handler
    pub fn new() -> Self {
        Self {
            name: "FileContentHandler".to_string(),
            priority: HandlerPriority::Low,
            supported_types: vec![ConvergedMessageType::Direct, ConvergedMessageType::Task],
            max_file_size: 10 * 1024 * 1024, // 10 MB default
        }
    }

    /// Create a new file content handler with custom max file size
    pub fn with_max_file_size(mut self, size: usize) -> Self {
        self.max_file_size = size;
        self
    }

    /// Validate file content
    fn validate_file(&self, file: &UnifiedFileContent) -> A2aMcpResult<()> {
        // Check file size
        if let Some(size) = file.size {
            if size > self.max_file_size as u64 {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "translation",
                    error.message = format!("File size {} exceeds maximum {}", size, self.max_file_size),
                );
                let _guard = error_span.enter();
                return Err(A2aMcpError::Translation(format!(
                    "File size {} exceeds maximum {}",
                    size, self.max_file_size
                )));
            }
        }

        // Validate that either bytes or URI is present
        match (&file.bytes, &file.uri) {
            (Some(_), None) | (None, Some(_)) => Ok(()),
            (Some(_), Some(_)) => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "translation",
                    error.message = "File cannot have both bytes and URI",
                );
                let _guard = error_span.enter();
                Err(A2aMcpError::Translation(
                    "File cannot have both bytes and URI".to_string(),
                ))
            }
            (None, None) => Err(A2aMcpError::Translation(
                "File must have either bytes or URI".to_string(),
            )),
        }
    }

    /// Process file content
    fn process_file(&self, file: &UnifiedFileContent) -> A2aMcpResult<String> {
        self.validate_file(file)?;

        let file_name = file.name.as_deref().unwrap_or("unknown");

        if let Some(uri) = &file.uri {
            Ok(format!("Processing file from URI: {} ({})", uri, file_name))
        } else {
            let size = file.size.unwrap_or(0);
            Ok(format!(
                "Processing embedded file: {} ({} bytes)",
                file_name, size
            ))
        }
    }
}

impl Default for FileContentHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl MessageHandler for FileContentHandler {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let file = match &message.payload.content {
            UnifiedContent::File { file, .. } => file,
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "validation",
                    error.message = "Expected file content",
                );
                let _guard = error_span.enter();
                return Err(HandlerError::Validation(
                    "Expected file content".to_string(),
                ));
            }
        };

        let result = self
            .process_file(file)
            .map_err(|e| HandlerError::Processing(format!("Failed to process file: {}", e)))?;

        let response = ConvergedMessage::text(
            format!("{}-response", message.message_id),
            message.source.clone(),
            result,
        );

        info!(
            handler = %self.name,
            message_id = %message.message_id,
            file_name = ?file.name,
            "Processed file message"
        );

        Ok(response)
    }

    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool {
        self.supported_types.contains(message_type)
    }

    fn priority(&self) -> HandlerPriority {
        self.priority
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn supported_types(&self) -> Vec<ConvergedMessageType> {
        self.supported_types.clone()
    }
}

/// Data content handler
#[derive(Debug, Clone)]
pub struct DataContentHandler {
    name: String,
    priority: HandlerPriority,
    supported_types: Vec<ConvergedMessageType>,
}

impl DataContentHandler {
    /// Create a new data content handler
    pub fn new() -> Self {
        Self {
            name: "DataContentHandler".to_string(),
            priority: HandlerPriority::Normal,
            supported_types: vec![
                ConvergedMessageType::Query,
                ConvergedMessageType::Command,
                ConvergedMessageType::Sync,
            ],
        }
    }

    /// Process data content
    fn process_data(
        &self, data: &serde_json::Map<String, serde_json::Value>, schema: &Option<String>,
    ) -> A2aMcpResult<String> {
        let mut result = serde_json::Map::new();
        result.insert(
            "status".to_string(),
            serde_json::Value::String("processed".to_string()),
        );
        result.insert(
            "original_keys".to_string(),
            serde_json::Value::Number(data.len().into()),
        );

        if let Some(schema) = schema {
            result.insert(
                "schema".to_string(),
                serde_json::Value::String(schema.clone()),
            );
        }

        serde_json::to_string_pretty(&result).map_err(A2aMcpError::from)
    }
}

impl Default for DataContentHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl MessageHandler for DataContentHandler {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let (data, schema) = match &message.payload.content {
            UnifiedContent::Data { data, schema } => (data, schema),
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "validation",
                    error.message = "Expected data content",
                );
                let _guard = error_span.enter();
                return Err(HandlerError::Validation(
                    "Expected data content".to_string(),
                ));
            }
        };

        let result = self
            .process_data(data, schema)
            .map_err(|e| HandlerError::Processing(format!("Failed to process data: {}", e)))?;

        let response = ConvergedMessage::text(
            format!("{}-response", message.message_id),
            message.source.clone(),
            result,
        );

        info!(
            handler = %self.name,
            message_id = %message.message_id,
            data_keys = data.len(),
            "Processed data message"
        );

        Ok(response)
    }

    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool {
        self.supported_types.contains(message_type)
    }

    fn priority(&self) -> HandlerPriority {
        self.priority
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn supported_types(&self) -> Vec<ConvergedMessageType> {
        self.supported_types.clone()
    }
}

/// Multipart content handler
#[derive(Debug, Clone)]
pub struct MultipartHandler {
    name: String,
    priority: HandlerPriority,
    supported_types: Vec<ConvergedMessageType>,
    max_parts: usize,
}

impl MultipartHandler {
    /// Create a new multipart handler
    pub fn new() -> Self {
        Self {
            name: "MultipartHandler".to_string(),
            priority: HandlerPriority::Low,
            supported_types: vec![
                ConvergedMessageType::Direct,
                ConvergedMessageType::Broadcast,
            ],
            max_parts: 100,
        }
    }

    /// Create a new multipart handler with custom max parts
    pub fn with_max_parts(mut self, max: usize) -> Self {
        self.max_parts = max;
        self
    }

    /// Process multipart content, preserving actual data instead of summaries
    pub fn process_multipart(&self, parts: &[UnifiedContent]) -> A2aMcpResult<String> {
        // Validate max parts limit
        if parts.len() > self.max_parts {
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "translation",
                error.message = format!("Multipart content has {} parts, exceeds maximum {}", parts.len(), self.max_parts),
            );
            let _guard = error_span.enter();
            return Err(A2aMcpError::Translation(format!(
                "Multipart content has {} parts, exceeds maximum {}",
                parts.len(),
                self.max_parts
            )));
        }

        let mut results = Vec::new();
        let mut warnings = Vec::new();

        for (idx, part) in parts.iter().enumerate() {
            let formatted = match part {
                UnifiedContent::Text { content, .. } => {
                    // Preserve actual text content
                    format!("=== Part {}: Text ===\n{}", idx + 1, content)
                }
                UnifiedContent::File { file, .. } => {
                    // Preserve file metadata with clear messaging
                    let name = file.name.as_deref().unwrap_or("unnamed");
                    let size = file.bytes.as_ref().map(|b| b.len()).unwrap_or(0);
                    let mime = file
                        .mime_type
                        .as_deref()
                        .unwrap_or("application/octet-stream");

                    if file.bytes.is_some() {
                        format!(
                            "=== Part {}: File ===\nName: {}\nSize: {} bytes\nMIME: {}\n[Embedded content available - use file:// URI to access]",
                            idx + 1, name, size, mime
                        )
                    } else if let Some(uri) = &file.uri {
                        format!(
                            "=== Part {}: File ===\nName: {}\nURI: {}\nMIME: {}\n[Use file:// URI to access content]",
                            idx + 1, name, uri, mime
                        )
                    } else {
                        format!(
                            "=== Part {}: File ===\nName: {}\nMIME: {}\n[No content available - missing bytes and URI]",
                            idx + 1, name, mime
                        )
                    }
                }
                UnifiedContent::Data { data, .. } => {
                    // Preserve actual structured data as formatted JSON
                    match serde_json::to_string_pretty(&data) {
                        Ok(json) => format!("=== Part {}: Data ===\n{}", idx + 1, json),
                        Err(e) => {
                            warnings.push(format!(
                                "Part {}: Failed to serialize data: {}",
                                idx + 1,
                                e
                            ));
                            format!(
                                "=== Part {}: Data ===\n[Serialization error: {}]",
                                idx + 1,
                                e
                            )
                        }
                    }
                }
                UnifiedContent::Multipart {
                    parts: nested_parts,
                    ..
                } => {
                    // RECURSIVE: Process nested multipart
                    match self.process_multipart(nested_parts) {
                        Ok(nested_content) => {
                            format!(
                                "=== Part {}: Nested Multipart ===\n{}",
                                idx + 1,
                                nested_content
                            )
                        }
                        Err(e) => {
                            warnings.push(format!(
                                "Part {}: Failed to process nested multipart: {}",
                                idx + 1,
                                e
                            ));
                            format!(
                                "=== Part {}: Nested Multipart ===\n[Error processing nested parts: {}]",
                                idx + 1, e
                            )
                        }
                    }
                }
                UnifiedContent::Stream {
                    stream_id,
                    chunk_size,
                    ..
                } => {
                    // Preserve stream metadata with usage guidance
                    format!(
                        "=== Part {}: Stream ===\nStream ID: {}\nChunk Size: {} bytes\n[Use streaming API to consume this stream]",
                        idx + 1, stream_id, chunk_size
                    )
                }
            };
            results.push(formatted);
        }

        // Log warnings if any
        if !warnings.is_empty() {
            for warning in &warnings {
                info!("Multipart processing warning: {}", warning);
            }
        }

        Ok(results.join("\n\n"))
    }
}

impl Default for MultipartHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl MessageHandler for MultipartHandler {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let (parts, boundary) = match &message.payload.content {
            UnifiedContent::Multipart { parts, boundary } => (parts, boundary),
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "validation",
                    error.message = "Expected multipart content",
                );
                let _guard = error_span.enter();
                return Err(HandlerError::Validation(
                    "Expected multipart content".to_string(),
                ));
            }
        };

        let result = self
            .process_multipart(parts)
            .map_err(|e| HandlerError::Processing(format!("Failed to process multipart: {}", e)))?;

        let response = ConvergedMessage::text(
            format!("{}-response", message.message_id),
            message.source.clone(),
            result,
        );

        info!(
            handler = %self.name,
            message_id = %message.message_id,
            parts_count = parts.len(),
            boundary = ?boundary,
            "Processed multipart message"
        );

        Ok(response)
    }

    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool {
        self.supported_types.contains(message_type)
    }

    fn priority(&self) -> HandlerPriority {
        self.priority
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn supported_types(&self) -> Vec<ConvergedMessageType> {
        self.supported_types.clone()
    }
}

/// Stream content handler
#[derive(Debug, Clone)]
pub struct StreamHandler {
    name: String,
    priority: HandlerPriority,
    supported_types: Vec<ConvergedMessageType>,
    max_chunk_size: usize,
}

impl StreamHandler {
    /// Create a new stream handler
    pub fn new() -> Self {
        Self {
            name: "StreamHandler".to_string(),
            priority: HandlerPriority::Low,
            supported_types: vec![ConvergedMessageType::Direct, ConvergedMessageType::Event],
            max_chunk_size: 1024 * 1024, // 1 MB default
        }
    }

    /// Create a new stream handler with custom max chunk size
    pub fn with_max_chunk_size(mut self, size: usize) -> Self {
        self.max_chunk_size = size;
        self
    }

    /// Process stream content
    fn process_stream(&self, stream_id: &str, chunk_size: usize) -> A2aMcpResult<String> {
        if chunk_size > self.max_chunk_size {
            let error_span = tracing::error_span!(
                "ggen.error",
                error.type = "translation",
                error.message = format!("Chunk size {} exceeds maximum {}", chunk_size, self.max_chunk_size),
            );
            let _guard = error_span.enter();
            return Err(A2aMcpError::Translation(format!(
                "Chunk size {} exceeds maximum {}",
                chunk_size, self.max_chunk_size
            )));
        }

        Ok(format!(
            "Stream acknowledged: ID={}, chunk_size={}",
            stream_id, chunk_size
        ))
    }
}

impl Default for StreamHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl MessageHandler for StreamHandler {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let (stream_id, chunk_size) = match &message.payload.content {
            UnifiedContent::Stream {
                stream_id,
                chunk_size,
                ..
            } => (stream_id, *chunk_size),
            _ => {
                let error_span = tracing::error_span!(
                    "ggen.error",
                    error.type = "validation",
                    error.message = "Expected stream content",
                );
                let _guard = error_span.enter();
                return Err(HandlerError::Validation(
                    "Expected stream content".to_string(),
                ));
            }
        };

        let result = self
            .process_stream(stream_id, chunk_size)
            .map_err(|e| HandlerError::Processing(format!("Failed to process stream: {}", e)))?;

        let response = ConvergedMessage::text(
            format!("{}-response", message.message_id),
            message.source.clone(),
            result,
        );

        info!(
            handler = %self.name,
            message_id = %message.message_id,
            stream_id = %stream_id,
            chunk_size = chunk_size,
            "Processed stream message"
        );

        Ok(response)
    }

    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool {
        self.supported_types.contains(message_type)
    }

    fn priority(&self) -> HandlerPriority {
        self.priority
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn supported_types(&self) -> Vec<ConvergedMessageType> {
        self.supported_types.clone()
    }
}

/// Handler factory for creating and managing handlers
#[derive(Clone)]
pub struct HandlerFactory {
    handlers: HashMap<String, Arc<dyn MessageHandler>>,
}

impl Default for HandlerFactory {
    fn default() -> Self {
        Self::new()
    }
}

impl HandlerFactory {
    /// Create a new handler factory
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register a handler
    pub fn register<H>(&mut self, handler: H) -> A2aMcpResult<()>
    where
        H: MessageHandler + 'static,
    {
        let name = handler.name().to_string();
        self.handlers.insert(name, Arc::new(handler));
        Ok(())
    }

    /// Register a boxed handler
    pub fn register_boxed(&mut self, handler: Arc<dyn MessageHandler>) -> A2aMcpResult<()> {
        let name = handler.name();
        self.handlers.insert(name.to_string(), handler);
        Ok(())
    }

    /// Get a handler by name
    pub fn get(&self, name: &str) -> Option<Arc<dyn MessageHandler>> {
        self.handlers.get(name).cloned()
    }

    /// Get all handlers
    pub fn all(&self) -> Vec<Arc<dyn MessageHandler>> {
        self.handlers.values().cloned().collect()
    }

    /// Find handlers that can handle a message type
    pub fn find_for_type(
        &self, message_type: &ConvergedMessageType,
    ) -> Vec<Arc<dyn MessageHandler>> {
        self.all()
            .into_iter()
            .filter(|h| h.can_handle(message_type))
            .collect()
    }

    /// Get the highest priority handler for a message type
    pub fn best_for_type(
        &self, message_type: &ConvergedMessageType,
    ) -> Option<Arc<dyn MessageHandler>> {
        self.find_for_type(message_type)
            .into_iter()
            .max_by_key(|h| h.priority())
    }

    /// Create default handler set
    pub fn with_defaults(mut self) -> Self {
        let _ = self.register(TextContentHandler::new());
        let _ = self.register(FileContentHandler::new());
        let _ = self.register(DataContentHandler::new());
        let _ = self.register(MultipartHandler::new());
        let _ = self.register(StreamHandler::new());
        self
    }
}

/// Message router for routing messages to appropriate handlers
pub struct MessageRouter {
    factory: HandlerFactory,
    default_handler: Option<Arc<dyn MessageHandler>>,
}

impl Default for MessageRouter {
    fn default() -> Self {
        Self::new()
    }
}

impl MessageRouter {
    /// Create a new message router
    pub fn new() -> Self {
        Self {
            factory: HandlerFactory::new(),
            default_handler: None,
        }
    }

    /// Create a router with default handlers
    pub fn with_defaults() -> Self {
        Self {
            factory: HandlerFactory::new().with_defaults(),
            default_handler: None,
        }
    }

    /// Set the default handler
    pub fn with_default_handler(mut self, handler: Arc<dyn MessageHandler>) -> Self {
        self.default_handler = Some(handler);
        self
    }

    /// Register a handler
    pub fn register<H>(&mut self, handler: H) -> A2aMcpResult<()>
    where
        H: MessageHandler + 'static,
    {
        self.factory.register(handler)
    }

    /// Route a message to the appropriate handler
    #[tracing::instrument(skip(self), fields(
        message_id = %message.message_id,
        message_type = ?message.envelope.message_type,
        source = %message.source,
        target = ?message.target,
        correlation_id = ?message.envelope.correlation_id,
        service.name = "ggen-a2a-mcp",
        service.version = env!("CARGO_PKG_VERSION"),
    ))]
    pub async fn route(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage> {
        let start = std::time::Instant::now();
        let message_type = &message.envelope.message_type;

        // Find the best handler for this message type
        let handler = self
            .factory
            .best_for_type(message_type)
            .or_else(|| self.default_handler.clone())
            .ok_or_else(|| {
                let _err_span = tracing::error_span!(
                    "ggen.error",
                    "error.type" = "no_handler_found",
                    message_type = ?message_type,
                )
                .entered();
                HandlerError::Processing(format!(
                    "No handler found for message type: {:?}",
                    message_type
                ))
            })?;

        info!(
            handler = %handler.name(),
            message_id = %message.message_id,
            message_type = ?message_type,
            "Routing message to handler"
        );
        tracing::Span::current().record(otel_attrs::OPERATION_NAME, "route");

        let result = handler.handle(message.clone()).await;
        if let Err(ref e) = result {
            tracing::Span::current().record("error", true);
            tracing::Span::current().record("error.message", e.to_string());
        }
        info!(
            handler = %handler.name(),
            message_id = %message.message_id,
            elapsed_ms = start.elapsed().as_millis() as u64,
            success = result.is_ok(),
            "Message routed"
        );
        result
    }

    /// Get the handler factory
    pub fn factory(&self) -> &HandlerFactory {
        &self.factory
    }
}

/// Batch processing result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchResult {
    /// Total messages processed
    pub total: usize,

    /// Successful messages
    pub successful: usize,

    /// Failed messages
    pub failed: usize,

    /// Processing duration in milliseconds
    pub duration_ms: i64,

    /// Individual results
    pub results: Vec<IndividualResult>,
}

/// Individual message processing result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndividualResult {
    /// Message ID
    pub message_id: String,

    /// Handler name
    pub handler: String,

    /// Success status
    pub success: bool,

    /// Error message (if failed)
    pub error: Option<String>,
}

/// Batch processor for handling multiple messages
pub struct BatchProcessor {
    router: Arc<MessageRouter>,
    max_concurrent: usize,
}

impl BatchProcessor {
    /// Create a new batch processor
    pub fn new(router: Arc<MessageRouter>) -> Self {
        Self {
            router,
            max_concurrent: 10,
        }
    }

    /// Set the maximum concurrent operations
    pub fn with_max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent = max;
        self
    }

    /// Process a batch of messages
    ///
    /// Creates a batch-level tracing span (with `batch.id` and `batch.size`)
    /// and per-message child spans that preserve each message's `correlation_id`,
    /// so distributed traces remain correlated across the batch boundary.
    pub async fn process_batch(&self, messages: Vec<ConvergedMessage>) -> BatchResult {
        let start_time = Utc::now();
        let batch_id = uuid::Uuid::new_v4().to_string();
        let batch_size = messages.len();

        // Batch-level span groups all per-message child spans.
        let batch_span = tracing::info_span!(
            "ggen.pipeline.operation",
            batch.id = %batch_id,
            batch.size = batch_size,
        );
        let _batch_guard = batch_span.enter();

        tracing::Span::current().record(otel_attrs::OPERATION_NAME, "process_batch");
        info!(batch_size, "Batch processing started");
        let semaphore = Arc::new(tokio::sync::Semaphore::new(self.max_concurrent));

        let results = futures::future::join_all(messages.into_iter().map(|message| {
            let semaphore = semaphore.clone();
            let router = self.router.clone();
            let correlation_id = message.envelope.correlation_id.clone();
            let batch_id = batch_id.clone();

            async move {
                let message_id = message.message_id.clone();
                let handler_name = message.envelope.message_type.clone();

                // Per-message child span preserves correlation context.
                let msg_span = tracing::info_span!(
                    "ggen.pipeline.operation",
                    a2a.message_id = %message_id,
                    a2a.correlation_id = ?correlation_id,
                    batch.id = %batch_id,
                );
                let _msg_guard = msg_span.enter();

                let _permit = match semaphore.acquire().await {
                    Ok(permit) => permit,
                    Err(_) => {
                        return IndividualResult {
                            message_id,
                            handler: format!("{:?}", handler_name),
                            success: false,
                            error: Some("Semaphore closed".to_string()),
                        };
                    }
                };

                match router.route(message).await {
                    Ok(_) => IndividualResult {
                        message_id,
                        handler: format!("{:?}", handler_name),
                        success: true,
                        error: None,
                    },
                    Err(e) => IndividualResult {
                        message_id,
                        handler: format!("{:?}", handler_name),
                        success: false,
                        error: Some(e.to_string()),
                    },
                }
            }
        }))
        .await;

        let successful = results.iter().filter(|r| r.success).count();
        let failed = results.len() - successful;
        let duration = Utc::now() - start_time;

        for individual in &results {
            if !individual.success {
                tracing::warn!(
                    message_id = %individual.message_id,
                    handler = %individual.handler,
                    error = ?individual.error,
                    "Batch item failed"
                );
            }
        }

        info!(
            total = results.len(),
            successful,
            failed,
            duration_ms = duration.num_milliseconds(),
            batch.id = %batch_id,
            "Batch processing complete"
        );

        BatchResult {
            total: results.len(),
            successful,
            failed,
            duration_ms: duration.num_milliseconds(),
            results,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_message(_message_type: ConvergedMessageType) -> ConvergedMessage {
        ConvergedMessage::text(
            format!("test-{}", uuid::Uuid::new_v4()),
            "test-agent".to_string(),
            "Test content".to_string(),
        )
    }

    #[tokio::test]
    async fn test_text_handler() {
        let handler = TextContentHandler::new();
        let message = create_test_message(ConvergedMessageType::Direct);

        assert!(handler.can_handle(&ConvergedMessageType::Direct));
        assert_eq!(handler.priority(), HandlerPriority::Normal);

        let result = handler.handle(message).await.unwrap();
        assert!(result
            .payload
            .content
            .as_text()
            .map(|t| t.contains("Processed"))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_data_handler() {
        let handler = DataContentHandler::new();

        let mut data = serde_json::Map::new();
        data.insert(
            "key".to_string(),
            serde_json::Value::String("value".to_string()),
        );

        let mut message = create_test_message(ConvergedMessageType::Query);
        message.payload.content = UnifiedContent::Data {
            data,
            schema: Some("test-schema".to_string()),
        };

        let result = handler.handle(message).await.unwrap();
        assert!(result
            .payload
            .content
            .as_text()
            .map(|t| t.contains("processed"))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_file_handler_validation() {
        let handler = FileContentHandler::new().with_max_file_size(100);

        // Valid file
        let valid_file = UnifiedFileContent {
            name: Some("test.txt".to_string()),
            mime_type: Some("text/plain".to_string()),
            bytes: Some("SGVsbG8=".to_string()),
            uri: None,
            size: Some(5),
            hash: None,
        };
        assert!(handler.validate_file(&valid_file).is_ok());

        // File too large
        let large_file = UnifiedFileContent {
            name: Some("large.txt".to_string()),
            mime_type: Some("text/plain".to_string()),
            bytes: Some("SGVsbG8=".to_string()),
            uri: None,
            size: Some(200), // Exceeds max_file_size
            hash: None,
        };
        assert!(handler.validate_file(&large_file).is_err());
    }

    #[tokio::test]
    async fn test_handler_factory() {
        let factory = HandlerFactory::new().with_defaults();

        // Should find handler for Direct type
        let handler = factory.best_for_type(&ConvergedMessageType::Direct);
        assert!(handler.is_some());
        assert_eq!(handler.unwrap().name(), "TextContentHandler");
    }

    #[tokio::test]
    async fn test_message_router() {
        let router = MessageRouter::with_defaults();
        let message = create_test_message(ConvergedMessageType::Direct);

        let result = router.route(message).await.unwrap();
        assert!(result
            .payload
            .content
            .as_text()
            .map(|t| t.contains("Processed"))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_batch_processor() {
        let router = Arc::new(MessageRouter::with_defaults());
        let processor = BatchProcessor::new(router).with_max_concurrent(2);

        let messages = vec![
            create_test_message(ConvergedMessageType::Direct),
            create_test_message(ConvergedMessageType::Query),
            create_test_message(ConvergedMessageType::Task),
        ];

        let result = processor.process_batch(messages).await;
        assert_eq!(result.total, 3);
        assert_eq!(result.successful, 3);
        assert_eq!(result.failed, 0);
    }

    #[tokio::test]
    async fn test_multipart_handler() {
        let handler = MultipartHandler::new();

        let parts = vec![
            UnifiedContent::Text {
                content: "Part 1".to_string(),
                metadata: None,
            },
            UnifiedContent::Text {
                content: "Part 2".to_string(),
                metadata: None,
            },
        ];

        let mut message = create_test_message(ConvergedMessageType::Direct);
        message.payload.content = UnifiedContent::Multipart {
            parts,
            boundary: Some("boundary".to_string()),
        };

        let result = handler.handle(message).await.unwrap();
        assert!(result
            .payload
            .content
            .as_text()
            .map(|t| t.contains("Part 1"))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_stream_handler() {
        let handler = StreamHandler::new();

        let mut message = create_test_message(ConvergedMessageType::Event);
        message.payload.content = UnifiedContent::Stream {
            stream_id: "test-stream".to_string(),
            chunk_size: 1024,
            metadata: None,
        };

        let result = handler.handle(message).await.unwrap();
        assert!(result
            .payload
            .content
            .as_text()
            .map(|t| t.contains("test-stream"))
            .unwrap_or(false));
    }
}

// Helper extension for UnifiedContent
#[allow(dead_code)]
trait UnifiedContentExt {
    fn as_text(&self) -> Option<&str>;
}

impl UnifiedContentExt for UnifiedContent {
    fn as_text(&self) -> Option<&str> {
        match self {
            Self::Text { content, .. } => Some(content),
            _ => None,
        }
    }
}
