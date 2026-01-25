//! Validation middleware for API endpoints (Week 4 Security Hardening)
//!
//! This middleware provides automatic input validation for HTTP requests using the
//! comprehensive validation framework from ggen-core.
//!
//! ## Features
//!
//! - Request body validation with JSON schemas
//! - Query parameter validation
//! - Path parameter validation
//! - Header validation
//! - Custom validation rules
//!
//! ## Example
//!
//! ```rust,no_run
//! use axum::{Router, routing::post};
//! use ggen_api::middleware::validation::{ValidatedJson, validate_request};
//! use ggen_core::validation::input::StringValidator;
//! use serde::Deserialize;
//!
//! #[derive(Deserialize)]
//! struct CreateUserRequest {
//!     username: String,
//!     email: String,
//! }
//!
//! async fn create_user(
//!     ValidatedJson(req): ValidatedJson<CreateUserRequest>,
//! ) -> &'static str {
//!     // Request is already validated
//!     "User created"
//! }
//!
//! # async fn example() {
//! let app = Router::new()
//!     .route("/users", post(create_user));
//! # }
//! ```

use axum::{
    async_trait,
    extract::{FromRequest, Request},
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use ggen_core::validation::input::{
    InputValidationError, PathValidatorRule, StringValidator, UrlValidator,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::collections::HashMap;

/// Validation error response
#[derive(Debug, Serialize)]
pub struct ValidationErrorResponse {
    pub error: String,
    pub field: Option<String>,
    pub details: HashMap<String, String>,
}

impl IntoResponse for ValidationErrorResponse {
    fn into_response(self) -> Response {
        (StatusCode::BAD_REQUEST, Json(self)).into_response()
    }
}

impl From<InputValidationError> for ValidationErrorResponse {
    fn from(err: InputValidationError) -> Self {
        let (field, details) = match &err {
            InputValidationError::LengthViolation {
                field,
                actual,
                constraint,
            } => (
                Some(field.clone()),
                vec![
                    ("actual".to_string(), actual.to_string()),
                    ("constraint".to_string(), constraint.clone()),
                ]
                .into_iter()
                .collect(),
            ),
            InputValidationError::PatternViolation { field, pattern } => (
                Some(field.clone()),
                vec![("pattern".to_string(), pattern.clone())]
                    .into_iter()
                    .collect(),
            ),
            InputValidationError::RangeViolation {
                field,
                actual,
                constraint,
            } => (
                Some(field.clone()),
                vec![
                    ("actual".to_string(), actual.clone()),
                    ("constraint".to_string(), constraint.clone()),
                ]
                .into_iter()
                .collect(),
            ),
            _ => (None, HashMap::new()),
        };

        Self {
            error: err.to_string(),
            field,
            details,
        }
    }
}

/// Validated JSON extractor with automatic validation
///
/// This extractor validates the JSON body against the type's validation rules.
pub struct ValidatedJson<T>(pub T);

#[async_trait]
impl<T, S> FromRequest<S> for ValidatedJson<T>
where
    T: DeserializeOwned + Validate,
    S: Send + Sync,
{
    type Rejection = ValidationErrorResponse;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        let Json(value) =
            Json::<T>::from_request(req, state)
                .await
                .map_err(|e| ValidationErrorResponse {
                    error: format!("JSON parse error: {}", e),
                    field: None,
                    details: HashMap::new(),
                })?;

        value.validate().map_err(ValidationErrorResponse::from)?;

        Ok(ValidatedJson(value))
    }
}

/// Trait for validatable request types
pub trait Validate {
    fn validate(&self) -> Result<(), InputValidationError>;
}

/// Validated query parameters extractor
pub struct ValidatedQuery<T>(pub T);

#[async_trait]
impl<T, S> FromRequest<S> for ValidatedQuery<T>
where
    T: DeserializeOwned + Validate,
    S: Send + Sync,
{
    type Rejection = ValidationErrorResponse;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        let axum::extract::Query(value) = axum::extract::Query::<T>::from_request(req, state)
            .await
            .map_err(|e| ValidationErrorResponse {
                error: format!("Query parse error: {}", e),
                field: None,
                details: HashMap::new(),
            })?;

        value.validate().map_err(ValidationErrorResponse::from)?;

        Ok(ValidatedQuery(value))
    }
}

/// Common validation rules for API inputs
pub struct ApiValidationRules;

impl ApiValidationRules {
    /// Username validator: 3-32 chars, alphanumeric + underscore/hyphen
    pub fn username() -> StringValidator {
        StringValidator::new()
            .with_length(3, 32)
            .with_pattern(r"^[a-zA-Z0-9_-]+$")
    }

    /// Email validator: standard email format, max 254 chars
    pub fn email() -> StringValidator {
        StringValidator::new()
            .with_length(5, 254)
            .with_pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
    }

    /// Project name validator: 3-64 chars, alphanumeric + underscore/hyphen
    pub fn project_name() -> StringValidator {
        StringValidator::new()
            .with_length(3, 64)
            .with_pattern(r"^[a-zA-Z0-9_-]+$")
    }

    /// Template name validator: alphanumeric + underscore/hyphen/dot/slash
    pub fn template_name() -> StringValidator {
        StringValidator::new()
            .with_length(1, 256)
            .with_pattern(r"^[a-zA-Z0-9_\-./]+$")
    }

    /// API key validator: hex string, 64 chars
    pub fn api_key() -> StringValidator {
        StringValidator::new()
            .with_length(64, 64)
            .with_pattern(r"^[a-fA-F0-9]{64}$")
    }

    /// Version validator: semver format
    pub fn version() -> StringValidator {
        StringValidator::new()
            .with_length(5, 32)
            .with_pattern(r"^\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?$")
    }

    /// Repository URL validator: HTTPS only, GitHub/GitLab
    pub fn repository_url() -> UrlValidator {
        UrlValidator::new()
            .require_https()
            .with_domains(vec!["github.com".to_string(), "gitlab.com".to_string()])
    }

    /// Template path validator: no traversal, .tmpl extension
    pub fn template_path() -> PathValidatorRule {
        PathValidatorRule::new()
            .with_extensions(vec!["tmpl".to_string(), "tera".to_string()])
            .with_max_length(1024)
    }

    /// RDF file path validator: no traversal, .ttl/.rdf/.owl extensions
    pub fn rdf_path() -> PathValidatorRule {
        PathValidatorRule::new()
            .with_extensions(vec![
                "ttl".to_string(),
                "rdf".to_string(),
                "owl".to_string(),
                "n3".to_string(),
            ])
            .with_max_length(1024)
    }
}

/// Validation middleware builder
pub struct ValidationMiddleware {
    rules: HashMap<String, Box<dyn Fn(&str) -> Result<String, InputValidationError> + Send + Sync>>,
}

impl ValidationMiddleware {
    pub fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule<F>(mut self, field: &str, rule: F) -> Self
    where
        F: Fn(&str) -> Result<String, InputValidationError> + Send + Sync + 'static,
    {
        self.rules.insert(field.to_string(), Box::new(rule));
        self
    }

    pub fn validate_field(&self, field: &str, value: &str) -> Result<String, InputValidationError> {
        if let Some(rule) = self.rules.get(field) {
            rule(value)
        } else {
            Ok(value.to_string())
        }
    }
}

impl Default for ValidationMiddleware {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_core::validation::input::CharsetRule;

    #[derive(Deserialize)]
    struct TestRequest {
        username: String,
        email: String,
    }

    impl Validate for TestRequest {
        fn validate(&self) -> Result<(), InputValidationError> {
            ApiValidationRules::username().validate(&self.username)?;
            ApiValidationRules::email().validate(&self.email)?;
            Ok(())
        }
    }

    #[test]
    fn test_username_validator() {
        let validator = ApiValidationRules::username();

        // Valid usernames
        assert!(validator.validate("alice").is_ok());
        assert!(validator.validate("bob_123").is_ok());
        assert!(validator.validate("user-name").is_ok());

        // Invalid usernames
        assert!(validator.validate("ab").is_err()); // too short
        assert!(validator.validate(&"a".repeat(33)).is_err()); // too long
        assert!(validator.validate("user@name").is_err()); // invalid char
        assert!(validator.validate("user name").is_err()); // space
    }

    #[test]
    fn test_email_validator() {
        let validator = ApiValidationRules::email();

        // Valid emails
        assert!(validator.validate("alice@example.com").is_ok());
        assert!(validator.validate("bob+tag@subdomain.example.org").is_ok());

        // Invalid emails
        assert!(validator.validate("not-an-email").is_err());
        assert!(validator.validate("@example.com").is_err());
        assert!(validator.validate("user@").is_err());
    }

    #[test]
    fn test_project_name_validator() {
        let validator = ApiValidationRules::project_name();

        // Valid project names
        assert!(validator.validate("my-project").is_ok());
        assert!(validator.validate("project_123").is_ok());

        // Invalid project names
        assert!(validator.validate("ab").is_err()); // too short
        assert!(validator.validate("project name").is_err()); // space
        assert!(validator.validate("project@123").is_err()); // invalid char
    }

    #[test]
    fn test_api_key_validator() {
        let validator = ApiValidationRules::api_key();

        // Valid API key (64 hex chars)
        let valid_key = "a".repeat(64);
        assert!(validator.validate(&valid_key).is_ok());

        // Invalid API keys
        assert!(validator.validate("short").is_err()); // too short
        assert!(validator.validate(&"z".repeat(64)).is_err()); // invalid hex
    }

    #[test]
    fn test_version_validator() {
        let validator = ApiValidationRules::version();

        // Valid versions
        assert!(validator.validate("1.0.0").is_ok());
        assert!(validator.validate("2.3.4-alpha").is_ok());
        assert!(validator.validate("1.0.0+build.123").is_ok());

        // Invalid versions
        assert!(validator.validate("1.0").is_err());
        assert!(validator.validate("not-a-version").is_err());
    }

    #[test]
    fn test_repository_url_validator() {
        let validator = ApiValidationRules::repository_url();

        // Valid repository URLs
        assert!(validator.validate("https://github.com/user/repo").is_ok());
        assert!(validator
            .validate("https://gitlab.com/user/project")
            .is_ok());

        // Invalid repository URLs
        assert!(validator.validate("http://github.com/user/repo").is_err()); // HTTP
        assert!(validator
            .validate("https://bitbucket.org/user/repo")
            .is_err()); // wrong domain
    }

    #[test]
    fn test_template_path_validator() {
        let validator = ApiValidationRules::template_path();

        // Valid template paths
        assert!(validator
            .validate(std::path::Path::new("templates/rust.tmpl"))
            .is_ok());
        assert!(validator
            .validate(std::path::Path::new("main.tera"))
            .is_ok());

        // Invalid template paths
        assert!(validator
            .validate(std::path::Path::new("../etc/passwd"))
            .is_err()); // traversal
        assert!(validator
            .validate(std::path::Path::new("script.sh"))
            .is_err()); // wrong extension
    }

    #[test]
    fn test_rdf_path_validator() {
        let validator = ApiValidationRules::rdf_path();

        // Valid RDF paths
        assert!(validator
            .validate(std::path::Path::new("ontology.ttl"))
            .is_ok());
        assert!(validator
            .validate(std::path::Path::new("schema.rdf"))
            .is_ok());

        // Invalid RDF paths
        assert!(validator
            .validate(std::path::Path::new("../etc/passwd"))
            .is_err()); // traversal
        assert!(validator
            .validate(std::path::Path::new("data.json"))
            .is_err()); // wrong extension
    }

    #[test]
    fn test_validation_middleware() {
        let middleware = ValidationMiddleware::new().add_rule("username", |value| {
            ApiValidationRules::username().validate(value)
        });

        // Valid field
        assert!(middleware.validate_field("username", "alice").is_ok());

        // Invalid field
        assert!(middleware.validate_field("username", "ab").is_err());

        // Unknown field (no validation)
        assert!(middleware.validate_field("unknown", "anything").is_ok());
    }

    #[test]
    fn test_request_validation() {
        let valid_req = TestRequest {
            username: "alice".to_string(),
            email: "alice@example.com".to_string(),
        };

        assert!(valid_req.validate().is_ok());

        let invalid_req = TestRequest {
            username: "ab".to_string(), // too short
            email: "alice@example.com".to_string(),
        };

        assert!(invalid_req.validate().is_err());
    }
}
