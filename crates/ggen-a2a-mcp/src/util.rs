//! Utility functions for A2A-MCP integration

use crate::error::{A2aMcpError, A2aMcpResult};
use url::Url;

/// Ensures that a URL is properly formatted
pub fn validate_url(url: &str) -> A2aMcpResult<String> {
    let parsed = Url::parse(url)?;

    // Ensure URL has a scheme and host
    if parsed.scheme().is_empty() || parsed.host_str().is_none() {
        return Err(A2aMcpError::Translation(
            "URL must have a scheme and host".to_string(),
        ));
    }

    // Normalize URL
    Ok(parsed.to_string())
}

/// Extracts tool name from a method string
/// For example: "https://example.com/agent:toolName" -> "toolName"
pub fn extract_tool_name(method: &str) -> Option<String> {
    method.split(':').next_back().map(|s| s.to_string())
}

/// Extracts agent URL from a method string
/// For example: "https://example.com/agent:toolName" -> "https://example.com/agent"
pub fn extract_agent_url(method: &str) -> Option<String> {
    method
        .rfind(':')
        .map(|pos| method[..pos].to_string())
        .or_else(|| Some(method.to_string()))
}

/// Normalizes a task ID to ensure it's valid
pub fn normalize_task_id(task_id: &str) -> String {
    // If task_id is not a valid UUID, generate a new one
    if uuid::Uuid::parse_str(task_id).is_err() {
        return uuid::Uuid::new_v4().to_string();
    }
    task_id.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_url() {
        assert!(validate_url("https://example.com").is_ok());
        assert!(validate_url("http://localhost:8080").is_ok());
        assert!(validate_url("not-a-url").is_err());
    }

    #[test]
    fn test_extract_tool_name() {
        assert_eq!(
            extract_tool_name("https://example.com/agent:toolName"),
            Some("toolName".to_string())
        );
        assert_eq!(extract_tool_name("simple"), Some("simple".to_string()));
    }

    #[test]
    fn test_extract_agent_url() {
        assert_eq!(
            extract_agent_url("https://example.com/agent:toolName"),
            Some("https://example.com/agent".to_string())
        );
        assert_eq!(extract_agent_url("simple"), Some("simple".to_string()));
    }

    #[test]
    fn test_normalize_task_id() {
        let valid_uuid = "550e8400-e29b-41d4-a716-446655440000";
        assert_eq!(normalize_task_id(valid_uuid), valid_uuid);

        let invalid = "not-a-uuid";
        let normalized = normalize_task_id(invalid);
        assert_ne!(normalized, invalid);
        assert!(uuid::Uuid::parse_str(&normalized).is_ok());
    }
}
