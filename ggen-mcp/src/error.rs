use serde_json::json;
// use std::fmt;
use ggen_utils::error::Error as GgenError;
use anyhow::Error as AnyhowError;

#[derive(Debug, thiserror::Error)]
pub enum GgenMcpError {
    #[error("Missing required parameter: {0}")]
    MissingParameter(String),
    
    #[error("Invalid parameter: {0}")]
    InvalidParameter(String),
    
    #[error("Execution failed: {0}")]
    ExecutionFailed(String),
    
    #[error("Registry error: {0}")]
    RegistryError(String),
    
    #[error("Graph error: {0}")]
    GraphError(String),
    
    #[error("Template error: {0}")]
    TemplateError(String),
    
    #[error("Serialization error: {0}")]
    SerializationError(String),
    
    #[error("Timeout error: {0}")]
    Timeout(String),
    
    #[error("Generation failed: {0}")]
    GenerationFailed(String),
    
    #[error("Core error: {0}")]
    Core(#[from] GgenError),
    
    #[error("Anyhow error: {0}")]
    Anyhow(#[from] AnyhowError),
    
    #[error("MCP protocol error: {0}")]
    Protocol(#[from] rmcp::Error),
}

// Display implementation is now handled by thiserror::Error derive

impl From<GgenMcpError> for rmcp::ErrorData {
    fn from(err: GgenMcpError) -> Self {
        rmcp::ErrorData::internal_error(err.to_string(), None)
    }
}

impl From<serde_json::Error> for GgenMcpError {
    fn from(err: serde_json::Error) -> Self {
        GgenMcpError::SerializationError(err.to_string())
    }
}

pub type Result<T> = std::result::Result<T, GgenMcpError>;

/// Helper to extract string parameter
pub fn get_string_param(params: &serde_json::Value, key: &str) -> Result<String> {
    params
        .get(key)
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .ok_or_else(|| GgenMcpError::MissingParameter(key.to_string()))
}

/// Helper to extract optional string parameter
pub fn get_optional_string_param(params: &serde_json::Value, key: &str) -> Option<String> {
    params.get(key).and_then(|v| v.as_str()).map(|s| s.to_string())
}

/// Helper to extract optional u64 parameter
pub fn get_optional_u64_param(params: &serde_json::Value, key: &str) -> Option<u64> {
    params.get(key).and_then(|v| v.as_u64())
}

/// Helper to extract boolean parameter
pub fn get_bool_param(params: &serde_json::Value, key: &str, default: bool) -> bool {
    params.get(key).and_then(|v| v.as_bool()).unwrap_or(default)
}

/// Helper to extract optional object parameter
pub fn get_optional_object_param(params: &serde_json::Value, key: &str) -> Option<serde_json::Map<String, serde_json::Value>> {
    params.get(key).and_then(|v| v.as_object()).map(|obj| obj.clone())
}

/// Helper to create success response
pub fn success_response(data: serde_json::Value) -> serde_json::Value {
    json!({
        "status": "success",
        "data": data
    })
}

/// Helper to create error response
pub fn error_response(message: &str) -> serde_json::Value {
    json!({
        "status": "error",
        "message": message
    })
}
