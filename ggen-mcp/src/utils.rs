use serde_json::Value;
use crate::error::{GgenMcpError, Result};

// Helper functions for parameter extraction
pub fn get_string_param(params: &Value, key: &str) -> Result<String> {
    params[key].as_str()
        .ok_or_else(|| GgenMcpError::MissingParameter(key.to_string()))
        .map(|s| s.to_string())
}

pub fn get_optional_string_param(params: &Value, key: &str) -> Option<String> {
    params[key].as_str().map(|s| s.to_string())
}

pub fn get_optional_u64_param(params: &Value, key: &str) -> Option<u64> {
    params[key].as_u64()
}

pub fn get_bool_param(params: &Value, key: &str, default: bool) -> bool {
    params[key].as_bool().unwrap_or(default)
}

pub fn get_optional_object_param(params: &Value, key: &str) -> Option<std::collections::HashMap<String, Value>> {
    params[key].as_object().map(|obj| {
        obj.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    })
}

pub fn success_response(data: Value) -> Value {
    serde_json::json!({
        "success": true,
        "data": data
    })
}

