//! Adapter implementations
//!
//! This module defines the adapter implementations for integrating
//! different communication protocols and message formats into the
//! A2A system.

use async_trait::async_trait;
use std::collections::HashMap;

/// Adapter trait for protocol integration
#[async_trait]
pub trait Adapter: Send + Sync {
    /// Adapter name
    fn name(&self) -> &str;

    /// Adapter version
    fn version(&self) -> &str;

    /// Initialize the adapter
    async fn initialize(&mut self, config: serde_json::Value) -> Result<(), AdapterError>;

    /// Check if adapter can handle the given message format
    fn can_handle(&self, format: &str) -> bool;

    /// Convert message from adapter format to A2A format
    async fn to_a2a(&self, message: &serde_json::Value) -> Result<serde_json::Value, AdapterError>;

    /// Convert message from A2A format to adapter format
    async fn from_a2a(
        &self, message: &serde_json::Value,
    ) -> Result<serde_json::Value, AdapterError>;

    /// Get adapter capabilities
    fn capabilities(&self) -> AdapterCapabilities;

    /// Cleanup adapter resources
    async fn shutdown(&mut self) -> Result<(), AdapterError>;
}

/// Adapter capabilities
#[derive(Debug, Clone)]
pub struct AdapterCapabilities {
    /// Supported message formats
    pub supported_formats: Vec<String>,
    /// Maximum message size
    pub max_message_size: usize,
    /// Whether adapter supports encryption
    pub supports_encryption: bool,
    /// Whether adapter supports compression
    pub supports_compression: bool,
    /// Additional capabilities
    pub capabilities: HashMap<String, serde_json::Value>,
}

/// Adapter error types
#[derive(Debug, Clone, PartialEq)]
pub struct AdapterError {
    /// Error message
    pub message: String,
    /// Error type
    pub error_type: AdapterErrorType,
    /// Error details
    pub details: Option<serde_json::Value>,
}

impl AdapterError {
    pub fn new(message: String, error_type: AdapterErrorType) -> Self {
        Self {
            message,
            error_type,
            details: None,
        }
    }

    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

impl std::fmt::Display for AdapterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type, self.message)
    }
}

impl std::error::Error for AdapterError {}

/// Types of adapter errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdapterErrorType {
    /// Unsupported message format
    UnsupportedFormat,
    /// Conversion failed
    ConversionFailed,
    /// Adapter not initialized
    NotInitialized,
    /// Adapter timeout
    Timeout,
    /// Invalid configuration
    InvalidConfiguration,
    /// Network error
    NetworkError,
    /// Resource error
    ResourceError,
    /// Unknown error
    Unknown,
}

impl std::fmt::Display for AdapterErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdapterErrorType::UnsupportedFormat => write!(f, "Unsupported message format"),
            AdapterErrorType::ConversionFailed => write!(f, "Conversion failed"),
            AdapterErrorType::NotInitialized => write!(f, "Adapter not initialized"),
            AdapterErrorType::Timeout => write!(f, "Adapter timeout"),
            AdapterErrorType::InvalidConfiguration => write!(f, "Invalid configuration"),
            AdapterErrorType::NetworkError => write!(f, "Network error"),
            AdapterErrorType::ResourceError => write!(f, "Resource error"),
            AdapterErrorType::Unknown => write!(f, "Unknown error"),
        }
    }
}

/// Base adapter implementation
pub struct BaseAdapter {
    name: String,
    version: String,
    config: serde_json::Value,
    initialized: bool,
    capabilities: AdapterCapabilities,
}

impl BaseAdapter {
    pub fn new(name: String, version: String) -> Self {
        Self {
            name,
            version,
            config: serde_json::json!({}),
            initialized: false,
            capabilities: AdapterCapabilities {
                supported_formats: Vec::new(),
                max_message_size: 1024 * 1024,
                supports_encryption: false,
                supports_compression: false,
                capabilities: HashMap::new(),
            },
        }
    }

    pub fn with_supported_formats(mut self, formats: Vec<String>) -> Self {
        self.capabilities.supported_formats = formats;
        self
    }

    pub fn with_max_message_size(mut self, size: usize) -> Self {
        self.capabilities.max_message_size = size;
        self
    }

    pub fn with_encryption_support(mut self, supports: bool) -> Self {
        self.capabilities.supports_encryption = supports;
        self
    }

    pub fn with_compression_support(mut self, supports: bool) -> Self {
        self.capabilities.supports_compression = supports;
        self
    }
}

#[async_trait]
impl Adapter for BaseAdapter {
    fn name(&self) -> &str {
        &self.name
    }

    fn version(&self) -> &str {
        &self.version
    }

    async fn initialize(&mut self, config: serde_json::Value) -> Result<(), AdapterError> {
        self.config = config;
        self.initialized = true;
        Ok(())
    }

    fn can_handle(&self, format: &str) -> bool {
        self.capabilities
            .supported_formats
            .contains(&format.to_string())
    }

    async fn to_a2a(&self, message: &serde_json::Value) -> Result<serde_json::Value, AdapterError> {
        if !self.initialized {
            return Err(AdapterError::new(
                "Adapter not initialized".to_string(),
                AdapterErrorType::NotInitialized,
            ));
        }

        // Default implementation: return message as-is
        Ok(message.clone())
    }

    async fn from_a2a(
        &self, message: &serde_json::Value,
    ) -> Result<serde_json::Value, AdapterError> {
        if !self.initialized {
            return Err(AdapterError::new(
                "Adapter not initialized".to_string(),
                AdapterErrorType::NotInitialized,
            ));
        }

        // Default implementation: return message as-is
        Ok(message.clone())
    }

    fn capabilities(&self) -> AdapterCapabilities {
        self.capabilities.clone()
    }

    async fn shutdown(&mut self) -> Result<(), AdapterError> {
        self.initialized = false;
        Ok(())
    }
}

/// JSON adapter for handling JSON messages
pub struct JsonAdapter {
    base: BaseAdapter,
}

impl JsonAdapter {
    pub fn new() -> Self {
        Self {
            base: BaseAdapter::new("json".to_string(), "1.0.0".to_string())
                .with_supported_formats(vec!["application/json".to_string()])
                .with_max_message_size(10 * 1024 * 1024), // 10MB
        }
    }
}

#[async_trait]
impl Adapter for JsonAdapter {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn version(&self) -> &str {
        self.base.version()
    }

    async fn initialize(&mut self, config: serde_json::Value) -> Result<(), AdapterError> {
        self.base.initialize(config).await
    }

    fn can_handle(&self, format: &str) -> bool {
        format == "application/json" || format.ends_with("+json")
    }

    async fn to_a2a(&self, message: &serde_json::Value) -> Result<serde_json::Value, AdapterError> {
        // For JSON adapter, conversion is straightforward
        Ok(message.clone())
    }

    async fn from_a2a(
        &self, message: &serde_json::Value,
    ) -> Result<serde_json::Value, AdapterError> {
        // For JSON adapter, conversion is straightforward
        Ok(message.clone())
    }

    fn capabilities(&self) -> AdapterCapabilities {
        self.base.capabilities()
    }

    async fn shutdown(&mut self) -> Result<(), AdapterError> {
        self.base.shutdown().await
    }
}

/// XML adapter for handling XML messages
pub struct XmlAdapter {
    base: BaseAdapter,
}

impl XmlAdapter {
    pub fn new() -> Self {
        Self {
            base: BaseAdapter::new("xml".to_string(), "1.0.0".to_string())
                .with_supported_formats(vec!["application/xml".to_string(), "text/xml".to_string()])
                .with_max_message_size(10 * 1024 * 1024), // 10MB
        }
    }

    // Simple XML to JSON conversion (basic implementation)
    fn xml_to_json(&self, xml: &str) -> Result<serde_json::Value, AdapterError> {
        // This is a simplified implementation
        // In a real implementation, you would use proper XML parsing
        let content = format!(
            "{{\"xml_content\": \"{}\"}}",
            xml.escape_default().to_string()
        );
        serde_json::from_str(&content).map_err(|e| {
            AdapterError::new(
                format!("XML to JSON conversion failed: {}", e),
                AdapterErrorType::ConversionFailed,
            )
        })
    }

    // Simple JSON to XML conversion (basic implementation)
    fn json_to_xml(&self, json: &serde_json::Value) -> Result<String, AdapterError> {
        serde_json::to_string(json).map_err(|e| {
            AdapterError::new(
                format!("JSON to XML conversion failed: {}", e),
                AdapterErrorType::ConversionFailed,
            )
        })
    }
}

#[async_trait]
impl Adapter for XmlAdapter {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn version(&self) -> &str {
        self.base.version()
    }

    async fn initialize(&mut self, config: serde_json::Value) -> Result<(), AdapterError> {
        self.base.initialize(config).await
    }

    fn can_handle(&self, format: &str) -> bool {
        format == "application/xml" || format == "text/xml"
    }

    async fn to_a2a(&self, message: &serde_json::Value) -> Result<serde_json::Value, AdapterError> {
        // Extract XML content from message
        let xml_str = message.as_str().ok_or_else(|| {
            AdapterError::new(
                "Message is not a string".to_string(),
                AdapterErrorType::ConversionFailed,
            )
        })?;

        self.xml_to_json(xml_str)
    }

    async fn from_a2a(
        &self, message: &serde_json::Value,
    ) -> Result<serde_json::Value, AdapterError> {
        let xml_str = self.json_to_xml(message)?;
        Ok(serde_json::json!(xml_str))
    }

    fn capabilities(&self) -> AdapterCapabilities {
        self.base.capabilities()
    }

    async fn shutdown(&mut self) -> Result<(), AdapterError> {
        self.base.shutdown().await
    }
}

/// Adapter registry for managing multiple adapters
pub struct AdapterRegistry {
    adapters: HashMap<String, Box<dyn Adapter>>,
}

impl AdapterRegistry {
    pub fn new() -> Self {
        Self {
            adapters: HashMap::new(),
        }
    }

    pub fn register_adapter(&mut self, adapter: Box<dyn Adapter>) {
        self.adapters.insert(adapter.name().to_string(), adapter);
    }

    pub fn get_adapter(&self, name: &str) -> Option<&dyn Adapter> {
        self.adapters.get(name).map(|adapter| adapter.as_ref())
    }

    pub fn find_adapter_for_format(&self, format: &str) -> Option<&dyn Adapter> {
        for adapter in self.adapters.values() {
            if adapter.can_handle(format) {
                return Some(adapter.as_ref());
            }
        }
        None
    }

    pub fn list_adapters(&self) -> Vec<&dyn Adapter> {
        self.adapters
            .values()
            .map(|adapter| adapter.as_ref())
            .collect()
    }
}

/// Message converter for using adapters
pub struct MessageConverter {
    registry: AdapterRegistry,
}

impl MessageConverter {
    pub fn new() -> Self {
        Self {
            registry: AdapterRegistry::new(),
        }
    }

    pub fn with_adapter(mut self, adapter: Box<dyn Adapter>) -> Self {
        self.registry.register_adapter(adapter);
        self
    }

    pub async fn convert_to_a2a(
        &self, message: &serde_json::Value, format: &str,
    ) -> Result<serde_json::Value, AdapterError> {
        let adapter = self
            .registry
            .find_adapter_for_format(format)
            .ok_or_else(|| {
                AdapterError::new(
                    format!("No adapter found for format: {}", format),
                    AdapterErrorType::UnsupportedFormat,
                )
            })?;

        adapter.to_a2a(message).await
    }

    pub async fn convert_from_a2a(
        &self, message: &serde_json::Value, target_format: &str,
    ) -> Result<serde_json::Value, AdapterError> {
        let adapter = self
            .registry
            .find_adapter_for_format(target_format)
            .ok_or_else(|| {
                AdapterError::new(
                    format!("No adapter found for format: {}", target_format),
                    AdapterErrorType::UnsupportedFormat,
                )
            })?;

        adapter.from_a2a(message).await
    }
}

impl Default for MessageConverter {
    fn default() -> Self {
        Self::new()
            .with_adapter(Box::new(JsonAdapter::new()))
            .with_adapter(Box::new(XmlAdapter::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_json_adapter() {
        let mut adapter = JsonAdapter::new();
        adapter.initialize(serde_json::json!({})).await.unwrap();

        let json_msg = serde_json::json!({"test": "message"});
        let converted = adapter.to_a2a(&json_msg).await.unwrap();
        assert_eq!(converted, json_msg);
    }

    #[tokio::test]
    async fn test_xml_adapter() {
        let mut adapter = XmlAdapter::new();
        adapter.initialize(serde_json::json!({})).await.unwrap();

        let xml_msg = serde_json::json!("<test>message</test>");
        let converted = adapter.to_a2a(&xml_msg).await.unwrap();

        // The adapter converts XML to JSON format
        assert!(converted.is_object());
    }

    #[test]
    fn test_adapter_registry() {
        let mut registry = AdapterRegistry::new();
        registry.register_adapter(Box::new(JsonAdapter::new()));

        assert!(registry.get_adapter("json").is_some());
        assert!(registry
            .find_adapter_for_format("application/json")
            .is_some());
    }

    #[tokio::test]
    async fn test_message_converter() {
        let converter = MessageConverter::default();

        let json_msg = serde_json::json!({"test": "message"});
        let converted = converter
            .convert_to_a2a(&json_msg, "application/json")
            .await
            .unwrap();
        assert_eq!(converted, json_msg);
    }
}
