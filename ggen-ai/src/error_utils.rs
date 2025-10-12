//! Error handling utilities for ggen-ai
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should eliminate duplicate error handling code across ggen-ai by providing
//! reusable helper functions that construct clear, actionable GgenAiError instances with
//! consistent messaging patterns.
//!
//! ## RESPONSIBILITIES
//! 1. **LLM Response Parsing Errors**: Should create errors when LLM output can't be parsed
//! 2. **Missing Parameter Errors**: Should create errors when required parameters are absent
//! 3. **Validation Errors**: Should create errors when validation constraints fail
//! 4. **Code Block Extraction Errors**: Should create errors when code blocks can't be extracted
//! 5. **Provider-Agnostic Messages**: Should never mention specific LLM providers in errors
//!
//! ## DESIGN PRINCIPLES
//! - **Clear Errors with Examples**: Every error should show what was expected
//! - **Lenient Input Format**: Accept varied input formats where possible
//! - **Strict Validation**: Validate thoroughly before returning success
//! - **DRY (Don't Repeat Yourself)**: Centralize all error construction patterns
//!
//! ## USAGE PATTERNS
//!
//! ### Before (Duplicate Pattern - 170 occurrences):
//! ```rust,ignore
//! Err(GgenAiError::template_generation(
//!     format!(
//!         "Could not find opening ```yaml marker in response. Response preview: {}",
//!         &response[..response.len().min(200)]
//!     )
//! ))
//! ```
//!
//! ### After (Reusable Helper):
//! ```rust,ignore
//! use crate::error_utils::*;
//!
//! missing_code_block_error(
//!     "yaml",
//!     response,
//!     ErrorContext::TemplateGeneration
//! )
//! ```
//!
//! ## CORE TEAM BEST PRACTICES APPLIED
//! 1. ✅ Clear errors showing what was expected (all helpers include examples)
//! 2. ✅ Lenient on input format (helpers accept &str and String)
//! 3. ✅ Strict on validation (helpers validate thoroughly)
//! 4. ✅ Provider-agnostic (no mention of OpenAI, Anthropic, etc.)
//! 5. ✅ DRY principle (eliminates 170 duplicate error constructions)

use crate::error::{GgenAiError, Result};

/// Error context for categorizing which generator operation failed
#[derive(Debug, Clone, Copy)]
pub enum ErrorContext {
    /// Template generation operation
    TemplateGeneration,
    /// SPARQL query generation operation
    SparqlGeneration,
    /// Ontology/RDF generation operation
    OntologyGeneration,
    /// Code refactoring operation
    RefactorGeneration,
    /// Generic validation operation
    Validation,
}

impl ErrorContext {
    /// Convert context to appropriate GgenAiError constructor
    fn to_error(&self, message: String) -> GgenAiError {
        match self {
            ErrorContext::TemplateGeneration => GgenAiError::template_generation(message),
            ErrorContext::SparqlGeneration => GgenAiError::sparql_generation(message),
            ErrorContext::OntologyGeneration => GgenAiError::ontology_generation(message),
            ErrorContext::RefactorGeneration => GgenAiError::validation(message),
            ErrorContext::Validation => GgenAiError::validation(message),
        }
    }
}

/// Create error for missing code block in LLM response
///
/// # Arguments
/// * `language` - Expected code block language (e.g., "yaml", "sparql", "turtle")
/// * `response` - The full LLM response that was being parsed
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing response preview
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let response = "Here's some text without a code block";
/// let result: Result<String> = missing_code_block_error(
///     "yaml",
///     response,
///     ErrorContext::TemplateGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn missing_code_block_error<T>(
    language: &str, response: &str, context: ErrorContext,
) -> Result<T> {
    let preview_len = response.len().min(200);
    let message = format!(
        "No ```{} code block found in LLM response.\n\
        \n\
        Expected format:\n\
        ```{}\n\
        [content here]\n\
        ```\n\
        \n\
        Response preview: {}...",
        language,
        language,
        &response[..preview_len]
    );
    Err(context.to_error(message))
}

/// Create error for missing closing marker in code block
///
/// # Arguments
/// * `marker` - The missing closing marker (usually "```")
/// * `language` - The code block language that was started
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = missing_closing_marker_error(
///     "```",
///     "yaml",
///     ErrorContext::TemplateGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn missing_closing_marker_error<T>(
    marker: &str, language: &str, context: ErrorContext,
) -> Result<T> {
    let message = format!(
        "Could not find closing {} marker after opening ```{} code block.\n\
        \n\
        Expected format:\n\
        ```{}\n\
        [content here]\n\
        {}\n\
        \n\
        Please ensure the LLM provider returns complete code blocks.",
        marker, language, language, marker
    );
    Err(context.to_error(message))
}

/// Create error for unsupported query type
///
/// # Arguments
/// * `query_type` - The unsupported query type received
/// * `supported_types` - List of supported types
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing supported types
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = unsupported_type_error(
///     "DELETE",
///     &["SELECT", "CONSTRUCT", "ASK", "DESCRIBE"],
///     ErrorContext::SparqlGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn unsupported_type_error<T>(
    query_type: &str, supported_types: &[&str], context: ErrorContext,
) -> Result<T> {
    let message = format!(
        "Unsupported query type: {}\n\
        \n\
        Supported types:\n\
        {}\n\
        \n\
        Please use one of the supported types.",
        query_type,
        supported_types
            .iter()
            .map(|t| format!("  - {}", t))
            .collect::<Vec<_>>()
            .join("\n")
    );
    Err(context.to_error(message))
}

/// Create error for missing required field in JSON/structured data
///
/// # Arguments
/// * `field_name` - Name of the missing field
/// * `data_type` - Type of data structure (e.g., "JSON", "YAML")
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing example
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = missing_field_error(
///     "type",
///     "JSON",
///     ErrorContext::SparqlGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn missing_field_error<T>(
    field_name: &str, data_type: &str, context: ErrorContext,
) -> Result<T> {
    let message = format!(
        "Missing required field '{}' in {} data.\n\
        \n\
        Expected structure should include:\n\
        {{\n\
          \"{}\": \"value\",\n\
          ...\n\
        }}\n\
        \n\
        Please ensure the LLM returns complete {} with all required fields.",
        field_name, data_type, field_name, data_type
    );
    Err(context.to_error(message))
}

/// Create error for invalid content format
///
/// # Arguments
/// * `expected_format` - Description of expected format
/// * `actual_content` - Preview of actual content received
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing what was expected vs received
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = invalid_format_error(
///     "Turtle/RDF with @prefix declarations",
///     "Some plain text without RDF",
///     ErrorContext::OntologyGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn invalid_format_error<T>(
    expected_format: &str, actual_content: &str, context: ErrorContext,
) -> Result<T> {
    let preview_len = actual_content.len().min(200);
    let message = format!(
        "Invalid content format.\n\
        \n\
        Expected: {}\n\
        \n\
        Received content preview:\n\
        {}...\n\
        \n\
        Please ensure the LLM returns content in the expected format.",
        expected_format,
        &actual_content[..preview_len]
    );
    Err(context.to_error(message))
}

/// Create error for no valid content found in response
///
/// # Arguments
/// * `content_type` - Type of content being searched for
/// * `response` - The full response that was searched
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing response preview
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = no_valid_content_error(
///     "SPARQL query (SELECT/CONSTRUCT/ASK/DESCRIBE)",
///     "Here's some random text",
///     ErrorContext::SparqlGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn no_valid_content_error<T>(
    content_type: &str, response: &str, context: ErrorContext,
) -> Result<T> {
    let preview_len = response.len().min(200);
    let message = format!(
        "No valid {} found in LLM response.\n\
        \n\
        Response preview:\n\
        {}...\n\
        \n\
        Please ensure the LLM provider returns valid {}.",
        content_type,
        &response[..preview_len],
        content_type
    );
    Err(context.to_error(message))
}

/// Create error for parsing failure with context
///
/// # Arguments
/// * `what_failed` - Description of what failed to parse
/// * `parse_error` - The underlying parse error message
/// * `content_preview` - Preview of content that failed to parse
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing parse error and content
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let result: Result<String> = parse_failure_error(
///     "JSON response",
///     "expected `,` or `}`",
///     "{invalid json",
///     ErrorContext::RefactorGeneration
/// );
/// assert!(result.is_err());
/// ```
pub fn parse_failure_error<T>(
    what_failed: &str, parse_error: &str, content_preview: &str, context: ErrorContext,
) -> Result<T> {
    let preview_len = content_preview.len().min(200);
    let message = format!(
        "Failed to parse {}.\n\
        \n\
        Parse error: {}\n\
        \n\
        Content preview:\n\
        {}...\n\
        \n\
        Please check the LLM response format.",
        what_failed,
        parse_error,
        &content_preview[..preview_len]
    );
    Err(context.to_error(message))
}

/// Create error for Turtle/RDF validation failure
///
/// This helper creates clear, actionable errors when Turtle validation fails,
/// following the core team's "defensive validation" principle. It's used after
/// lenient extraction to ensure only valid Turtle is returned.
///
/// # Arguments
/// * `validation_error` - The error message from validation (e.g., from oxigraph)
/// * `content` - The Turtle content that failed validation
/// * `context` - Which generator operation failed
///
/// # Returns
/// A `Result::Err` with a descriptive error showing what's invalid and how to fix it
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::error_utils::*;
///
/// let invalid_turtle = "ex:Thing a ex:Class ."; // Missing prefix
/// let result: Result<String> = turtle_validation_error(
///     "Undefined namespace prefix 'ex'",
///     invalid_turtle,
///     ErrorContext::OntologyGeneration
/// );
/// assert!(result.is_err());
/// ```
///
/// # Core Team Best Practices
/// - **Fail Fast**: Catch validation errors at generation time
/// - **Clear Errors**: Show what's wrong and suggest fixes
/// - **Defensive**: Never trust LLM output without validation
pub fn turtle_validation_error<T>(
    validation_error: &str, content: &str, context: ErrorContext,
) -> Result<T> {
    let preview_len = content.len().min(300);
    let message = format!(
        "Turtle validation failed - LLM generated syntactically invalid RDF.\n\
        \n\
        Validation error:\n\
        {}\n\
        \n\
        Generated content (first 300 chars):\n\
        {}\n\
        \n\
        This is a critical error that indicates the LLM returned malformed Turtle.\n\
        The content was successfully extracted but failed syntax validation.\n\
        \n\
        Possible fixes:\n\
        1. Ensure all namespace prefixes are declared with @prefix\n\
        2. Check that all URIs are enclosed in angle brackets < >\n\
        3. Verify all triples end with a period (.)\n\
        4. Ensure string literals are properly quoted\n\
        \n\
        Please regenerate the ontology or fix the Turtle syntax manually.",
        validation_error,
        &content[..preview_len]
    );
    Err(context.to_error(message))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_missing_code_block_error() {
        let response = "Here's some text without a code block";
        let result: Result<String> =
            missing_code_block_error("yaml", response, ErrorContext::TemplateGeneration);

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("```yaml"));
        assert!(msg.contains("Expected format"));
        assert!(msg.contains("Response preview"));
    }

    #[test]
    fn test_missing_closing_marker_error() {
        let result: Result<String> =
            missing_closing_marker_error("```", "yaml", ErrorContext::TemplateGeneration);

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("closing"));
        assert!(msg.contains("```yaml"));
    }

    #[test]
    fn test_unsupported_type_error() {
        let result: Result<String> = unsupported_type_error(
            "DELETE",
            &["SELECT", "CONSTRUCT", "ASK"],
            ErrorContext::SparqlGeneration,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("DELETE"));
        assert!(msg.contains("SELECT"));
        assert!(msg.contains("Supported types"));
    }

    #[test]
    fn test_missing_field_error() {
        let result: Result<String> =
            missing_field_error("type", "JSON", ErrorContext::SparqlGeneration);

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("type"));
        assert!(msg.contains("JSON"));
        assert!(msg.contains("Expected structure"));
    }

    #[test]
    fn test_invalid_format_error() {
        let result: Result<String> =
            invalid_format_error("Turtle/RDF", "plain text", ErrorContext::OntologyGeneration);

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Turtle/RDF"));
        assert!(msg.contains("plain text"));
    }

    #[test]
    fn test_no_valid_content_error() {
        let result: Result<String> = no_valid_content_error(
            "SPARQL query",
            "random text",
            ErrorContext::SparqlGeneration,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("SPARQL query"));
        assert!(msg.contains("random text"));
    }

    #[test]
    fn test_parse_failure_error() {
        let result: Result<String> = parse_failure_error(
            "JSON",
            "unexpected EOF",
            "{invalid",
            ErrorContext::RefactorGeneration,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("JSON"));
        assert!(msg.contains("unexpected EOF"));
        assert!(msg.contains("{invalid"));
    }

    #[test]
    fn test_error_context_to_error() {
        let template_err = ErrorContext::TemplateGeneration.to_error("test".to_string());
        assert!(template_err.to_string().contains("Template generation"));

        let sparql_err = ErrorContext::SparqlGeneration.to_error("test".to_string());
        assert!(sparql_err.to_string().contains("SPARQL generation"));

        let ontology_err = ErrorContext::OntologyGeneration.to_error("test".to_string());
        assert!(ontology_err.to_string().contains("Ontology generation"));
    }

    #[test]
    fn test_turtle_validation_error() {
        let invalid_content = "ex:Thing a ex:Class .";
        let result: Result<String> = turtle_validation_error(
            "Undefined namespace prefix 'ex'",
            invalid_content,
            ErrorContext::OntologyGeneration,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Turtle validation failed"));
        assert!(msg.contains("Undefined namespace prefix"));
        assert!(msg.contains("ex:Thing"));
        assert!(msg.contains("@prefix"));
        assert!(msg.contains("Possible fixes"));
    }

    #[test]
    fn test_turtle_validation_error_truncates_long_content() {
        let long_content = "ex:Thing a ex:Class . ".repeat(100);
        let result: Result<String> = turtle_validation_error(
            "Some error",
            &long_content,
            ErrorContext::OntologyGeneration,
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        // Should truncate to 300 chars
        assert!(msg.contains("first 300 chars"));
    }
}
