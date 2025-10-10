use serde_json::json;
use std::fmt;

#[derive(Debug)]
pub enum GgenMcpError {
    MissingParameter(String),
    InvalidParameter(String),
    ExecutionFailed(String),
    RegistryError(String),
    GraphError(String),
    TemplateError(String),
    SerializationError(String),
}

impl fmt::Display for GgenMcpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingParameter(p) => write!(f, "Missing required parameter: {}", p),
            Self::InvalidParameter(p) => write!(f, "Invalid parameter: {}", p),
            Self::ExecutionFailed(e) => write!(f, "Execution failed: {}", e),
            Self::RegistryError(e) => write!(f, "Registry error: {}", e),
            Self::GraphError(e) => write!(f, "Graph error: {}", e),
            Self::TemplateError(e) => write!(f, "Template error: {}", e),
            Self::SerializationError(e) => write!(f, "Serialization error: {}", e),
        }
    }
}

impl std::error::Error for GgenMcpError {}

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
