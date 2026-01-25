//! GCP-specific error handling and mapping
//!
//! This module defines GCP error types and provides mapping from Google API errors
//! to TAI error types, with classification for retry eligibility.

use std::fmt;
use thiserror::Error;

/// GCP error kind classification for determining retry eligibility
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcpErrorKind {
    /// Transient error: eligible for retry (service temporarily unavailable)
    Transient,
    /// Permanent error: should not retry (invalid request, authentication failure)
    Permanent,
    /// Quota exceeded: retry with backoff after quota window
    QuotaExceeded,
    /// Rate limited: retry with exponential backoff
    RateLimited,
    /// Authentication error: requires new token or credentials
    AuthenticationFailure,
}

impl fmt::Display for GcpErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Transient => write!(f, "transient"),
            Self::Permanent => write!(f, "permanent"),
            Self::QuotaExceeded => write!(f, "quota_exceeded"),
            Self::RateLimited => write!(f, "rate_limited"),
            Self::AuthenticationFailure => write!(f, "authentication_failure"),
        }
    }
}

impl GcpErrorKind {
    /// Returns true if the error is retryable
    pub fn is_retryable(self) -> bool {
        matches!(
            self,
            Self::Transient | Self::QuotaExceeded | Self::RateLimited
        )
    }
}

/// Context information enriching GCP errors
#[derive(Debug, Clone)]
pub struct GcpErrorContext {
    /// GCP Project ID where error occurred
    pub project_id: Option<String>,
    /// The operation being performed (e.g., "firestore.write", "run.invoke")
    pub operation: Option<String>,
    /// Resource involved in the operation (e.g., "/documents/user-123")
    pub resource: Option<String>,
    /// Attempt number in retry sequence
    pub attempt: Option<u32>,
    /// HTTP status code (if applicable)
    pub http_status: Option<u16>,
}

impl fmt::Display for GcpErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut items = Vec::new();

        if let Some(ref project_id) = self.project_id {
            items.push(format!("project_id={}", project_id));
        }
        if let Some(ref operation) = self.operation {
            items.push(format!("operation={}", operation));
        }
        if let Some(ref resource) = self.resource {
            items.push(format!("resource={}", resource));
        }
        if let Some(attempt) = self.attempt {
            items.push(format!("attempt={}", attempt));
        }
        if let Some(status) = self.http_status {
            items.push(format!("http_status={}", status));
        }

        if items.is_empty() {
            write!(f, "(no context)")
        } else {
            write!(f, "({})", items.join(", "))
        }
    }
}

impl Default for GcpErrorContext {
    fn default() -> Self {
        Self {
            project_id: None,
            operation: None,
            resource: None,
            attempt: None,
            http_status: None,
        }
    }
}

impl GcpErrorContext {
    /// Creates a new context with project ID
    pub fn with_project(project_id: String) -> Self {
        Self {
            project_id: Some(project_id),
            ..Default::default()
        }
    }

    /// Sets the operation being performed
    pub fn with_operation(mut self, operation: String) -> Self {
        self.operation = Some(operation);
        self
    }

    /// Sets the resource involved
    pub fn with_resource(mut self, resource: String) -> Self {
        self.resource = Some(resource);
        self
    }

    /// Sets the attempt number
    pub fn with_attempt(mut self, attempt: u32) -> Self {
        self.attempt = Some(attempt);
        self
    }

    /// Sets the HTTP status code
    pub fn with_http_status(mut self, status: u16) -> Self {
        self.http_status = Some(status);
        self
    }
}

/// GCP error type combining error information with context
#[derive(Debug, Error)]
pub struct GcpError {
    /// Classification of error for retry eligibility
    kind: GcpErrorKind,
    /// Human-readable error message
    message: String,
    /// GCP error code (e.g., "PERMISSION_DENIED", "NOT_FOUND")
    gcp_code: Option<String>,
    /// Contextual information about the error
    context: GcpErrorContext,
    /// Original error (for error chain)
    source_error: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl fmt::Display for GcpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "GCP {} error: {} {} {}",
            self.kind,
            self.message,
            if let Some(ref code) = self.gcp_code {
                format!("[{}]", code)
            } else {
                String::new()
            },
            self.context
        )
    }
}

impl GcpError {
    /// Creates a new GCP error
    pub fn new(kind: GcpErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            gcp_code: None,
            context: GcpErrorContext::default(),
            source_error: None,
        }
    }

    /// Creates a transient error
    pub fn transient(message: impl Into<String>) -> Self {
        Self::new(GcpErrorKind::Transient, message)
    }

    /// Creates a permanent error
    pub fn permanent(message: impl Into<String>) -> Self {
        Self::new(GcpErrorKind::Permanent, message)
    }

    /// Creates a quota exceeded error
    pub fn quota_exceeded(message: impl Into<String>) -> Self {
        Self::new(GcpErrorKind::QuotaExceeded, message)
    }

    /// Creates a rate limit error
    pub fn rate_limited(message: impl Into<String>) -> Self {
        Self::new(GcpErrorKind::RateLimited, message)
    }

    /// Creates an authentication failure error
    pub fn authentication_failure(message: impl Into<String>) -> Self {
        Self::new(GcpErrorKind::AuthenticationFailure, message)
    }

    /// Sets the GCP error code
    pub fn with_gcp_code(mut self, code: String) -> Self {
        self.gcp_code = Some(code);
        self
    }

    /// Sets the context
    pub fn with_context(mut self, context: GcpErrorContext) -> Self {
        self.context = context;
        self
    }

    /// Sets the source error for error chain
    pub fn with_source(mut self, source: Box<dyn std::error::Error + Send + Sync>) -> Self {
        self.source_error = Some(source);
        self
    }

    /// Returns the error kind
    pub fn kind(&self) -> GcpErrorKind {
        self.kind
    }

    /// Returns the GCP error code if available
    pub fn gcp_code(&self) -> Option<&str> {
        self.gcp_code.as_deref()
    }

    /// Returns the context
    pub fn context(&self) -> &GcpErrorContext {
        &self.context
    }

    /// Returns true if the error is retryable
    pub fn is_retryable(&self) -> bool {
        self.kind.is_retryable()
    }
}

/// Maps HTTP status codes to GCP error kinds
pub fn classify_http_status(status: u16) -> GcpErrorKind {
    match status {
        // 4xx Client Errors (mostly permanent)
        400 => GcpErrorKind::Permanent,             // Bad Request
        401 => GcpErrorKind::AuthenticationFailure, // Unauthorized
        403 => GcpErrorKind::Permanent,             // Forbidden (PERMISSION_DENIED)
        404 => GcpErrorKind::Permanent,             // Not Found
        409 => GcpErrorKind::Permanent,             // Conflict
        413 => GcpErrorKind::Permanent,             // Payload Too Large
        429 => GcpErrorKind::RateLimited,           // Too Many Requests
        // 5xx Server Errors (mostly transient)
        500 => GcpErrorKind::Transient, // Internal Server Error
        502 => GcpErrorKind::Transient, // Bad Gateway
        503 => GcpErrorKind::Transient, // Service Unavailable
        504 => GcpErrorKind::Transient, // Gateway Timeout
        _ if status >= 500 => GcpErrorKind::Transient,
        _ => GcpErrorKind::Permanent,
    }
}

/// Maps GCP error codes (from REST API responses) to error kinds and messages
pub fn classify_gcp_error(code: &str, message: &str) -> (GcpErrorKind, String) {
    match code {
        // Authentication errors
        "UNAUTHENTICATED" | "PERMISSION_DENIED" => {
            (GcpErrorKind::AuthenticationFailure, message.to_string())
        }
        // Rate limiting
        "RESOURCE_EXHAUSTED" => (GcpErrorKind::RateLimited, message.to_string()),
        // Quota
        "QUOTA_EXCEEDED" => (GcpErrorKind::QuotaExceeded, message.to_string()),
        // Transient errors
        "UNAVAILABLE" | "DEADLINE_EXCEEDED" | "INTERNAL" | "UNKNOWN" => {
            (GcpErrorKind::Transient, message.to_string())
        }
        // Permanent errors
        "INVALID_ARGUMENT"
        | "NOT_FOUND"
        | "ALREADY_EXISTS"
        | "FAILED_PRECONDITION"
        | "OUT_OF_RANGE"
        | "CANCELLED"
        | "DATA_LOSS" => (GcpErrorKind::Permanent, message.to_string()),
        // Default: treat as permanent to be safe
        _ => (GcpErrorKind::Permanent, message.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_kind_is_retryable() {
        assert!(GcpErrorKind::Transient.is_retryable());
        assert!(GcpErrorKind::QuotaExceeded.is_retryable());
        assert!(GcpErrorKind::RateLimited.is_retryable());
        assert!(!GcpErrorKind::Permanent.is_retryable());
        assert!(!GcpErrorKind::AuthenticationFailure.is_retryable());
    }

    #[test]
    fn test_error_context_display() {
        let context = GcpErrorContext::default()
            .with_project("my-project".to_string())
            .with_operation("firestore.write".to_string())
            .with_http_status(503);

        let displayed = format!("{}", context);
        assert!(displayed.contains("project_id=my-project"));
        assert!(displayed.contains("operation=firestore.write"));
        assert!(displayed.contains("http_status=503"));
    }

    #[test]
    fn test_classify_http_status() {
        assert_eq!(
            classify_http_status(401),
            GcpErrorKind::AuthenticationFailure
        );
        assert_eq!(classify_http_status(429), GcpErrorKind::RateLimited);
        assert_eq!(classify_http_status(503), GcpErrorKind::Transient);
        assert_eq!(classify_http_status(404), GcpErrorKind::Permanent);
    }

    #[test]
    fn test_classify_gcp_error() {
        let (kind, _msg) = classify_gcp_error("UNAUTHENTICATED", "auth failed");
        assert_eq!(kind, GcpErrorKind::AuthenticationFailure);

        let (kind, _msg) = classify_gcp_error("UNAVAILABLE", "service down");
        assert_eq!(kind, GcpErrorKind::Transient);

        let (kind, _msg) = classify_gcp_error("NOT_FOUND", "resource missing");
        assert_eq!(kind, GcpErrorKind::Permanent);
    }

    #[test]
    fn test_gcp_error_creation_and_display() {
        let error = GcpError::transient("service unavailable")
            .with_gcp_code("UNAVAILABLE".to_string())
            .with_context(
                GcpErrorContext::default()
                    .with_project("test-project".to_string())
                    .with_operation("test.operation".to_string()),
            );

        let displayed = format!("{}", error);
        assert!(displayed.contains("transient"));
        assert!(displayed.contains("service unavailable"));
        assert!(displayed.contains("UNAVAILABLE"));
        assert!(displayed.contains("test-project"));
    }
}
