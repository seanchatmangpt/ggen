//! Request/response transformation pipeline for protocol adapters

use crate::error::{GatewayError, GatewayResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Transformation rule for modifying requests/responses
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformRule {
    /// Rule type (e.g., "add_header", "modify_path", "add_query_param")
    pub rule_type: String,
    /// Source field
    pub source: String,
    /// Target field
    pub target: String,
    /// Transformation value or pattern
    pub value: Option<String>,
}

/// Request transformation context
#[derive(Debug, Clone)]
pub struct RequestTransform {
    /// Original path
    pub path: String,
    /// Original headers
    pub headers: HashMap<String, String>,
    /// Original query parameters
    pub query_params: HashMap<String, String>,
    /// Original body
    pub body: Option<Vec<u8>>,
}

impl RequestTransform {
    /// Create a new request transformation context
    pub fn new(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            headers: HashMap::new(),
            query_params: HashMap::new(),
            body: None,
        }
    }

    /// Add a header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Add a query parameter
    pub fn with_query_param(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.query_params.insert(key.into(), value.into());
        self
    }

    /// Set body
    pub fn with_body(mut self, body: Vec<u8>) -> Self {
        self.body = Some(body);
        self
    }
}

/// Response transformation context
#[derive(Debug, Clone)]
pub struct ResponseTransform {
    /// Response status code
    pub status_code: u16,
    /// Response headers
    pub headers: HashMap<String, String>,
    /// Response body
    pub body: Option<Vec<u8>>,
}

impl ResponseTransform {
    /// Create a new response transformation context
    pub fn new(status_code: u16) -> Self {
        Self {
            status_code,
            headers: HashMap::new(),
            body: None,
        }
    }

    /// Add a header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Set body
    pub fn with_body(mut self, body: Vec<u8>) -> Self {
        self.body = Some(body);
        self
    }
}

/// Transformation pipeline for applying multiple rules
#[derive(Debug, Clone)]
pub struct TransformationPipeline {
    rules: Vec<TransformRule>,
}

impl TransformationPipeline {
    /// Create a new transformation pipeline
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Add a transformation rule
    pub fn add_rule(mut self, rule: TransformRule) -> Self {
        self.rules.push(rule);
        self
    }

    /// Transform a request
    pub fn transform_request(&self, mut request: RequestTransform) -> GatewayResult<RequestTransform> {
        for rule in &self.rules {
            if rule.rule_type == "add_header" {
                if let Some(value) = &rule.value {
                    request = request.with_header(rule.target.clone(), value.clone());
                }
            } else if rule.rule_type == "modify_path" {
                if let Some(value) = &rule.value {
                    request.path = value.clone();
                }
            } else if rule.rule_type == "add_query_param" {
                if let Some(value) = &rule.value {
                    request = request.with_query_param(rule.target.clone(), value.clone());
                }
            } else {
                return Err(GatewayError::TransformationFailed(format!(
                    "Unknown transformation type: {}",
                    rule.rule_type
                )));
            }
        }

        Ok(request)
    }

    /// Transform a response
    pub fn transform_response(&self, mut response: ResponseTransform) -> GatewayResult<ResponseTransform> {
        for rule in &self.rules {
            if rule.rule_type == "add_header" {
                if let Some(value) = &rule.value {
                    response = response.with_header(rule.target.clone(), value.clone());
                }
            } else if rule.rule_type == "modify_status" {
                if let Some(value) = &rule.value {
                    response.status_code = value
                        .parse()
                        .map_err(|_| GatewayError::TransformationFailed("Invalid status code".to_string()))?;
                }
            }
        }

        Ok(response)
    }
}

impl Default for TransformationPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request_transform_creation() {
        let transform = RequestTransform::new("/api/v1/test")
            .with_header("Content-Type", "application/json")
            .with_query_param("page", "1");

        assert_eq!(transform.path, "/api/v1/test");
        assert_eq!(transform.headers.get("Content-Type"), Some(&"application/json".to_string()));
        assert_eq!(transform.query_params.get("page"), Some(&"1".to_string()));
    }

    #[test]
    fn test_response_transform_creation() {
        let transform = ResponseTransform::new(200)
            .with_header("Content-Type", "application/json")
            .with_body(vec![1, 2, 3]);

        assert_eq!(transform.status_code, 200);
        assert_eq!(transform.headers.get("Content-Type"), Some(&"application/json".to_string()));
        assert_eq!(transform.body, Some(vec![1, 2, 3]));
    }

    #[test]
    fn test_transformation_pipeline_add_header() {
        let pipeline = TransformationPipeline::new()
            .add_rule(TransformRule {
                rule_type: "add_header".to_string(),
                source: "".to_string(),
                target: "X-Custom-Header".to_string(),
                value: Some("custom-value".to_string()),
            });

        let request = RequestTransform::new("/api/v1/test");
        let transformed = pipeline.transform_request(request).unwrap();

        assert_eq!(transformed.headers.get("X-Custom-Header"), Some(&"custom-value".to_string()));
    }

    #[test]
    fn test_transformation_pipeline_modify_path() {
        let pipeline = TransformationPipeline::new()
            .add_rule(TransformRule {
                rule_type: "modify_path".to_string(),
                source: "".to_string(),
                target: "".to_string(),
                value: Some("/api/v2/test".to_string()),
            });

        let request = RequestTransform::new("/api/v1/test");
        let transformed = pipeline.transform_request(request).unwrap();

        assert_eq!(transformed.path, "/api/v2/test");
    }

    #[test]
    fn test_transformation_pipeline_response() {
        let pipeline = TransformationPipeline::new()
            .add_rule(TransformRule {
                rule_type: "add_header".to_string(),
                source: "".to_string(),
                target: "X-Gateway-Version".to_string(),
                value: Some("1.0".to_string()),
            });

        let response = ResponseTransform::new(200);
        let transformed = pipeline.transform_response(response).unwrap();

        assert_eq!(transformed.headers.get("X-Gateway-Version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_transformation_pipeline_unknown_rule() {
        let pipeline = TransformationPipeline::new()
            .add_rule(TransformRule {
                rule_type: "unknown_type".to_string(),
                source: "".to_string(),
                target: "".to_string(),
                value: None,
            });

        let request = RequestTransform::new("/api/v1/test");
        let result = pipeline.transform_request(request);

        assert!(result.is_err());
    }
}
