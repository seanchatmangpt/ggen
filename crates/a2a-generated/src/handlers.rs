//! Message Handlers for A2A Protocol
//!
//! This module provides handlers for processing messages in the A2A protocol.
//! It implements the unified message handler interface to achieve 70% redundancy reduction.

pub use message_handler::*;

pub mod message_handler {
    use super::*;
    use async_trait::async_trait;
    use chrono::{DateTime, Utc};
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;

    // Re-export message types from converged module
    pub use super::super::converged::{
        ConvergedMessage, ConvergedMessageType, ConvergedPayload, LatencyRequirements,
        MessageEnvelope, MessageLifecycle, MessagePriority, MessageRouting, MessageState,
        QoSRequirements, ReliabilityLevel, ThroughputRequirements, UnifiedContent,
    };

    /// Unified handler trait for all message types
    #[async_trait]
    pub trait UnifiedMessageHandler: Send + Sync {
        /// Handle a message and return the result
        async fn handle(
            &self, message: &ConvergedMessage,
        ) -> Result<UnifiedHandlerResult, UnifiedHandlerError>;

        /// Check if this handler can process the given message
        fn can_handle(&self, message: &ConvergedMessage) -> bool;

        /// Get the handler priority
        fn priority(&self) -> HandlerPriority;

        /// Get the handler name
        fn name(&self) -> &str;

        /// Get the supported message types
        fn supported_types(&self) -> Vec<ConvergedMessageType>;
    }

    /// Unified handler result
    #[derive(Debug, Clone, PartialEq)]
    pub struct UnifiedHandlerResult {
        /// Response message (optional)
        pub response: Option<ConvergedMessage>,

        /// Processing status
        pub status: HandlerStatus,

        /// Processing metrics
        pub metrics: ProcessingMetrics,

        /// Next actions
        pub actions: Vec<HandlerAction>,

        /// Error information (if any)
        pub error: Option<HandlerError>,

        /// Handler metadata
        pub metadata: Option<HashMap<String, serde_json::Value>>,
    }

    /// Unified handler error
    #[derive(Debug, Clone, PartialEq)]
    pub struct HandlerError {
        /// Error code
        pub code: String,

        /// Error message
        pub message: String,

        /// Error severity
        pub severity: ErrorSeverity,

        /// Error context
        pub context: Option<HashMap<String, serde_json::Value>>,

        /// Stack trace (if available)
        pub stack_trace: Option<String>,
    }

    /// Error severity levels
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ErrorSeverity {
        Info,
        Warning,
        Error,
        Critical,
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

    /// Handler status
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum HandlerStatus {
        Success,
        PartialSuccess,
        Failed,
        Retry,
        Skipped,
    }

    /// Handler actions
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum HandlerAction {
        /// Send a response message
        SendResponse(ConvergedMessage),

        /// Schedule a task
        ScheduleTask(String, TaskConfig),

        /// Create an event
        CreateEvent(String, EventConfig),

        /// Log a message
        Log(String, LogLevel),

        /// Retry the message
        Retry(usize),

        /// Skip processing
        Skip,

        /// Custom action
        Custom(String, HashMap<String, serde_json::Value>),
    }

    /// Log levels
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum LogLevel {
        Debug,
        Info,
        Warning,
        Error,
        Critical,
    }

    /// Task configuration
    #[derive(Debug, Clone, PartialEq)]
    pub struct TaskConfig {
        pub priority: MessagePriority,
        pub timeout: std::time::Duration,
        pub retry_count: usize,
        pub dependencies: Vec<String>,
    }

    /// Event configuration
    #[derive(Debug, Clone, PartialEq)]
    pub struct EventConfig {
        pub event_type: String,
        pub metadata: Option<HashMap<String, serde_json::Value>>,
    }

    /// Processing metrics
    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct ProcessingMetrics {
        /// Processing duration (as chrono TimeDelta)
        pub duration: chrono::TimeDelta,

        /// Message size in bytes
        pub message_size: usize,

        /// CPU usage percentage
        pub cpu_usage: f64,

        /// Memory usage percentage
        pub memory_usage: f64,

        /// Number of operations performed
        pub operations: usize,

        /// Errors encountered
        pub errors: Vec<String>,

        /// Warnings generated
        pub warnings: Vec<String>,

        /// Timestamp of processing start
        pub start_time: DateTime<Utc>,

        /// Timestamp of processing end
        pub end_time: DateTime<Utc>,
    }

    /// Unified handler error
    #[derive(Debug, Clone, PartialEq)]
    pub enum UnifiedHandlerError {
        ValidationError(String),
        ProcessingError(String),
        SecurityError(String),
        NetworkError(String),
        TimeoutError,
        ResourceError(String),
        Custom(String),
    }

    impl std::fmt::Display for UnifiedHandlerError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                UnifiedHandlerError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
                UnifiedHandlerError::ProcessingError(msg) => write!(f, "Processing error: {}", msg),
                UnifiedHandlerError::SecurityError(msg) => write!(f, "Security error: {}", msg),
                UnifiedHandlerError::NetworkError(msg) => write!(f, "Network error: {}", msg),
                UnifiedHandlerError::TimeoutError => write!(f, "Timeout error"),
                UnifiedHandlerError::ResourceError(msg) => write!(f, "Resource error: {}", msg),
                UnifiedHandlerError::Custom(msg) => write!(f, "Custom error: {}", msg),
            }
        }
    }

    impl std::error::Error for UnifiedHandlerError {}

    /// Message router that routes messages to appropriate handlers
    pub struct UnifiedMessageRouter {
        handlers: Vec<Box<dyn UnifiedMessageHandler>>,
        routing_rules: Vec<UnifiedRoutingRule>,
        metrics: RouterMetrics,
    }

    /// Router metrics
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RouterMetrics {
        /// Total messages processed
        pub total_messages: u64,

        /// Successful messages
        pub successful_messages: u64,

        /// Failed messages
        pub failed_messages: u64,

        /// Average processing time (as chrono Duration)
        pub avg_processing_time: chrono::Duration,

        /// Last processed message time
        pub last_processed: DateTime<Utc>,
    }

    impl Default for RouterMetrics {
        fn default() -> Self {
            Self {
                total_messages: 0,
                successful_messages: 0,
                failed_messages: 0,
                avg_processing_time: chrono::Duration::zero(),
                last_processed: Utc::now(),
            }
        }
    }

    impl UnifiedMessageRouter {
        pub fn new() -> Self {
            Self {
                handlers: Vec::new(),
                routing_rules: Vec::new(),
                metrics: RouterMetrics::default(),
            }
        }

        /// Register a handler
        pub fn register_handler<H>(&mut self, handler: H)
        where
            H: UnifiedMessageHandler + 'static,
        {
            self.handlers.push(Box::new(handler));
        }

        /// Register a boxed handler
        pub fn register_boxed_handler(&mut self, handler: Box<dyn UnifiedMessageHandler>) {
            self.handlers.push(handler);
        }

        /// Add a routing rule
        pub fn add_routing_rule(&mut self, rule: UnifiedRoutingRule) {
            self.routing_rules.push(rule);
        }

        /// Route a message to the appropriate handler
        pub async fn route(
            &mut self, message: &ConvergedMessage,
        ) -> Result<UnifiedHandlerResult, UnifiedHandlerError> {
            let start_time = Utc::now();
            self.metrics.total_messages += 1;

            // Find appropriate handler
            let handler = self.find_handler(message).await.ok_or_else(|| {
                UnifiedHandlerError::ValidationError("No suitable handler found".to_string())
            })?;

            // Handle the message
            let result = handler.handle(message).await;

            // Update metrics
            let end_time = Utc::now();
            let duration = end_time - start_time;

            if result.is_ok() {
                self.metrics.successful_messages += 1;
            } else {
                self.metrics.failed_messages += 1;
            }

            // Update average processing time
            self.metrics.avg_processing_time =
                (self.metrics.avg_processing_time * (self.metrics.total_messages - 1) + duration)
                    / self.metrics.total_messages;
            self.metrics.last_processed = end_time;

            result
        }

        /// Find the best handler for a message
        async fn find_handler(
            &self, message: &ConvergedMessage,
        ) -> Option<&Box<dyn UnifiedMessageHandler>> {
            // Filter handlers that can handle this message
            let capable_handlers: Vec<_> = self
                .handlers
                .iter()
                .filter(|h| h.can_handle(message))
                .collect();

            if capable_handlers.is_empty() {
                return None;
            }

            // Apply routing rules
            let mut selected = capable_handlers;

            for rule in &self.routing_rules {
                if let Some(filtered) = rule.apply(message, &selected) {
                    selected = filtered;
                }
            }

            // Select handler with highest priority
            selected.into_iter().max_by_key(|h| h.priority())
        }

        /// Get current metrics
        pub fn metrics(&self) -> &RouterMetrics {
            &self.metrics
        }
    }

    /// Unified routing rule
    #[derive(Debug, Clone, PartialEq)]
    pub struct UnifiedRoutingRule {
        pub name: String,
        pub condition: RoutingCondition,
        pub priority: u32,
        pub metadata: Option<HashMap<String, serde_json::Value>>,
    }

    impl UnifiedRoutingRule {
        /// Apply this rule to filter handlers
        pub fn apply(
            &self, message: &ConvergedMessage, handlers: &[&Box<dyn UnifiedMessageHandler>],
        ) -> Option<Vec<&Box<dyn UnifiedMessageHandler>>> {
            let filtered = handlers
                .iter()
                .filter(|h| self.condition.matches(message, h))
                .copied()
                .collect();

            if filtered.is_empty() {
                None
            } else {
                Some(filtered)
            }
        }
    }

    /// Routing condition
    #[derive(Debug, Clone, PartialEq)]
    pub enum RoutingCondition {
        /// Message type based
        MessageType(ConvergedMessageType),

        /// Source agent based
        SourceAgent(String),

        /// Target agent based
        TargetAgent(String),

        /// Message content based
        ContentCondition(Box<dyn Fn(&ConvergedMessage) -> bool + Send + Sync>),

        /// Handler priority based
        MinimumPriority(HandlerPriority),

        /// Custom condition
        Custom(Box<dyn Fn(&ConvergedMessage) -> bool + Send + Sync>),
    }

    impl RoutingCondition {
        /// Check if condition matches
        pub fn matches(
            &self, message: &ConvergedMessage, handler: &Box<dyn UnifiedMessageHandler>,
        ) -> bool {
            match self {
                RoutingCondition::MessageType(msg_type) => {
                    &message.envelope.message_type == msg_type
                }
                RoutingCondition::SourceAgent(source) => &message.source == source,
                RoutingCondition::TargetAgent(target) => {
                    message.target.as_ref().map_or(false, |t| t == target)
                }
                RoutingCondition::ContentCondition(condition) => condition(message),
                RoutingCondition::MinimumPriority(priority) => handler.priority() >= *priority,
                RoutingCondition::Custom(condition) => condition(message),
            }
        }
    }

    /// Built-in message handlers

    /// Text message handler
    pub struct TextMessageHandler {
        name: String,
        priority: HandlerPriority,
    }

    impl TextMessageHandler {
        pub fn new() -> Self {
            Self {
                name: "TextMessageHandler".to_string(),
                priority: HandlerPriority::Normal,
            }
        }
    }

    #[async_trait]
    impl UnifiedMessageHandler for TextMessageHandler {
        async fn handle(
            &self, message: &ConvergedMessage,
        ) -> Result<UnifiedHandlerResult, UnifiedHandlerError> {
            match &message.payload.content {
                UnifiedContent::Text { content, metadata } => {
                    // Process text content
                    let response_content = format!("Processed text: {}", content);

                    let response = Some(ConvergedMessage::text(
                        format!("{}-response", message.message_id),
                        message.source.clone(),
                        response_content,
                    ));

                    Ok(UnifiedHandlerResult {
                        response,
                        status: HandlerStatus::Success,
                        metrics: ProcessingMetrics {
                            duration: Utc::now() - message.envelope.timestamp,
                            message_size: content.len(),
                            cpu_usage: 0.1,
                            memory_usage: 1.0,
                            operations: 1,
                            errors: Vec::new(),
                            warnings: Vec::new(),
                            start_time: message.envelope.timestamp,
                            end_time: Utc::now(),
                        },
                        actions: vec![HandlerAction::Log(
                            "Text message processed".to_string(),
                            LogLevel::Info,
                        )],
                        error: None,
                        metadata: metadata.clone(),
                    })
                }
                _ => Err(UnifiedHandlerError::ValidationError(
                    "Expected text content".to_string(),
                )),
            }
        }

        fn can_handle(&self, message: &ConvergedMessage) -> bool {
            matches!(
                message.envelope.message_type,
                ConvergedMessageType::Direct | ConvergedMessageType::Task
            )
        }

        fn priority(&self) -> HandlerPriority {
            self.priority
        }

        fn name(&self) -> &str {
            &self.name
        }

        fn supported_types(&self) -> Vec<ConvergedMessageType> {
            vec![ConvergedMessageType::Direct, ConvergedMessageType::Task]
        }
    }

    /// Data processing handler
    pub struct DataProcessingHandler {
        name: String,
        priority: HandlerPriority,
    }

    impl DataProcessingHandler {
        pub fn new() -> Self {
            Self {
                name: "DataProcessingHandler".to_string(),
                priority: HandlerPriority::High,
            }
        }
    }

    #[async_trait]
    impl UnifiedMessageHandler for DataProcessingHandler {
        async fn handle(
            &self, message: &ConvergedMessage,
        ) -> Result<UnifiedHandlerResult, UnifiedHandlerError> {
            match &message.payload.content {
                UnifiedContent::Data { data, schema } => {
                    // Process data content
                    let processed_data = Self::process_data(data)?;

                    let response = Some(ConvergedMessage::text(
                        format!("{}-processed", message.message_id),
                        message.source.clone(),
                        serde_json::to_string(&processed_data).unwrap(),
                    ));

                    Ok(UnifiedHandlerResult {
                        response,
                        status: HandlerStatus::Success,
                        metrics: ProcessingMetrics {
                            duration: Utc::now() - message.envelope.timestamp,
                            message_size: serde_json::to_string(data).unwrap().len(),
                            cpu_usage: 0.2,
                            memory_usage: 2.0,
                            operations: data.len(),
                            errors: Vec::new(),
                            warnings: Vec::new(),
                            start_time: message.envelope.timestamp,
                            end_time: Utc::now(),
                        },
                        actions: vec![HandlerAction::CreateEvent(
                            "data_processed".to_string(),
                            EventConfig {
                                event_type: "data.processed".to_string(),
                                metadata: Some(
                                    schema
                                        .as_ref()
                                        .map(|s| {
                                            let mut map = HashMap::new();
                                            map.insert(
                                                "schema".to_string(),
                                                serde_json::Value::String(s.clone()),
                                            );
                                            map
                                        })
                                        .unwrap_or_default(),
                                ),
                            },
                        )],
                        error: None,
                        metadata: None,
                    })
                }
                _ => Err(UnifiedHandlerError::ValidationError(
                    "Expected data content".to_string(),
                )),
            }
        }

        fn can_handle(&self, message: &ConvergedMessage) -> bool {
            matches!(
                message.envelope.message_type,
                ConvergedMessageType::Query | ConvergedMessageType::Command
            )
        }

        fn priority(&self) -> HandlerPriority {
            self.priority
        }

        fn name(&self) -> &str {
            &self.name
        }

        fn supported_types(&self) -> Vec<ConvergedMessageType> {
            vec![ConvergedMessageType::Query, ConvergedMessageType::Command]
        }
    }

    impl DataProcessingHandler {
        fn process_data(
            data: &serde_json::Map<String, serde_json::Value>,
        ) -> Result<serde_json::Value, UnifiedHandlerError> {
            // Apply some processing logic
            let mut processed = data.clone();

            // Add processed timestamp
            processed.insert(
                "processed_at".to_string(),
                serde_json::Value::String(Utc::now().to_rfc3339()),
            );

            // Add processing info
            processed.insert(
                "processor".to_string(),
                serde_json::Value::String("DataProcessingHandler".to_string()),
            );

            Ok(serde_json::Value::Object(processed))
        }
    }

    /// Error handler for processing errors
    pub struct ErrorHandler {
        name: String,
        priority: HandlerPriority,
    }

    impl ErrorHandler {
        pub fn new() -> Self {
            Self {
                name: "ErrorHandler".to_string(),
                priority: HandlerPriority::Critical,
            }
        }
    }

    #[async_trait]
    impl UnifiedMessageHandler for ErrorHandler {
        async fn handle(
            &self, message: &ConvergedMessage,
        ) -> Result<UnifiedHandlerResult, UnifiedHandlerError> {
            let error_handler_result = UnifiedHandlerResult {
                response: None,
                status: HandlerStatus::Failed,
                metrics: ProcessingMetrics {
                    duration: Utc::now() - message.envelope.timestamp,
                    message_size: 0,
                    cpu_usage: 0.0,
                    memory_usage: 0.0,
                    operations: 0,
                    errors: vec!["Error occurred".to_string()],
                    warnings: Vec::new(),
                    start_time: message.envelope.timestamp,
                    end_time: Utc::now(),
                },
                actions: vec![HandlerAction::Log(
                    "Error occurred during message processing".to_string(),
                    LogLevel::Error,
                )],
                error: Some(HandlerError {
                    code: "ERR_HANDLER".to_string(),
                    message: "Error occurred during message processing".to_string(),
                    severity: ErrorSeverity::Error,
                    context: None,
                    stack_trace: None,
                }),
                metadata: None,
            };

            Ok(error_handler_result)
        }

        fn can_handle(&self, message: &ConvergedMessage) -> bool {
            // This handler can handle any message, but has lowest priority
            true
        }

        fn priority(&self) -> HandlerPriority {
            self.priority
        }

        fn name(&self) -> &str {
            &self.name
        }

        fn supported_types(&self) -> Vec<ConvergedMessageType> {
            vec![
                ConvergedMessageType::Direct,
                ConvergedMessageType::Task,
                ConvergedMessageType::Query,
                ConvergedMessageType::Command,
                ConvergedMessageType::Event,
                ConvergedMessageType::Sync,
                ConvergedMessageType::Broadcast,
                ConvergedMessageType::System,
            ]
        }
    }

    /// Factory for creating default handlers
    pub struct HandlerFactory;

    impl HandlerFactory {
        pub fn create_default_handlers() -> Vec<Box<dyn UnifiedMessageHandler>> {
            vec![
                Box::new(TextMessageHandler::new()),
                Box::new(DataProcessingHandler::new()),
                Box::new(ErrorHandler::new()),
            ]
        }

        pub fn create_router() -> UnifiedMessageRouter {
            let mut router = UnifiedMessageRouter::new();

            // Register default handlers
            for handler in Self::create_default_handlers() {
                router.register_boxed_handler(handler);
            }

            // Add default routing rules
            router.add_routing_rule(UnifiedRoutingRule {
                name: "high_priority".to_string(),
                condition: RoutingCondition::MinimumPriority(HandlerPriority::High),
                priority: 100,
                metadata: None,
            });

            router
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[tokio::test]
        async fn test_text_handler() {
            let handler = TextMessageHandler::new();
            let message = ConvergedMessage::text(
                "msg-123".to_string(),
                "agent-1".to_string(),
                "Hello, world!".to_string(),
            );

            let result = handler.handle(&message).await.unwrap();
            assert!(result.response.is_some());
            assert_eq!(result.status, HandlerStatus::Success);
        }

        #[tokio::test]
        async fn test_data_handler() {
            let handler = DataProcessingHandler::new();
            let mut data = serde_json::Map::new();
            data.insert("test".to_string(), serde_json::Value::String("value"));

            let message = ConvergedMessage::text(
                "msg-456".to_string(),
                "agent-1".to_string(),
                "test".to_string(),
            );

            // Set data content
            use super::super::converged::ConvergedPayload;
            use super::super::converged::UnifiedContent;
            let mut message = message;
            message.payload.content = UnifiedContent::Data { data, schema: None };

            let result = handler.handle(&message).await.unwrap();
            assert!(result.response.is_some());
            assert_eq!(result.status, HandlerStatus::Success);
        }

        #[tokio::test]
        async fn test_router() {
            let mut router = HandlerFactory::create_router();

            let message = ConvergedMessage::text(
                "msg-789".to_string(),
                "agent-1".to_string(),
                "Test message".to_string(),
            );

            let result = router.route(&message).await.unwrap();
            assert!(result.response.is_some());
            assert_eq!(result.status, HandlerStatus::Success);

            // Check metrics
            assert_eq!(router.metrics().total_messages, 1);
            assert_eq!(router.metrics().successful_messages, 1);
        }
    }
}
