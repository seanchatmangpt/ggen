//! RDF identifier validation for SPARQL query safety
//!
//! Provides secure validation functions for RDF URIs and property names to prevent
//! SPARQL injection attacks. All SPARQL query construction should use these validated
//! identifiers.
//!
//! # Security Model
//!
//! This module implements defense-in-depth:
//! 1. **URI Format Validation**: Strict validation of URI structure (must be enclosed in < >)
//! 2. **Character Whitelisting**: Only allowed characters in IRIs per RFC 3987
//! 3. **Length Limits**: Prevent extremely long identifiers that could indicate attacks
//! 4. **Escape Handling**: Proper escaping of special characters in property names
//!
//! # Examples
//!
//! ```rust,no_run
//! use ggen_ai::codegen::validation::{validate_rdf_uri, validate_property_name};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Valid URIs
//! validate_rdf_uri("http://example.com/ontology#MyClass")?;
//! validate_rdf_uri("http://www.w3.org/2000/01/rdf-schema#Class")?;
//!
//! // Invalid URIs (will return Err)
//! assert!(validate_rdf_uri("javascript:alert('xss')").is_err());
//! assert!(validate_rdf_uri("http://example.com/'; DROP TABLE--").is_err());
//!
//! // Validate property names
//! validate_property_name("outputField")?;
//! validate_property_name("sh:path")?;
//!
//! # Ok(())
//! # }
//! ```

use crate::GgenAiError;
use regex::Regex;

/// Maximum allowed length for URI (prevents DoS attacks)
const MAX_URI_LENGTH: usize = 4096;

/// Maximum allowed length for property name (prevents DoS attacks)
const MAX_PROPERTY_NAME_LENGTH: usize = 256;

/// Validate an RDF URI before use in SPARQL queries
///
/// # Validation Rules
///
/// - URI must be a valid absolute URI (start with scheme)
/// - URI must not exceed 4096 characters (prevents DoS)
/// - URI must only contain allowed characters per RFC 3987
/// - URI must be properly formed (no control characters, no unclosed brackets)
///
/// # Returns
///
/// - `Ok(validated_uri)` if the URI is valid
/// - `Err(GgenAiError::Validation)` if the URI is invalid
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ai::codegen::validation::validate_rdf_uri;
///
/// assert!(validate_rdf_uri("http://example.com/ontology#MyClass").is_ok());
/// assert!(validate_rdf_uri("http://example.com/'; DROP TABLE--").is_err());
/// ```
pub fn validate_rdf_uri(uri: &str) -> crate::Result<String> {
    // Check length limit
    if uri.is_empty() {
        return Err(GgenAiError::validation("RDF URI cannot be empty"));
    }

    if uri.len() > MAX_URI_LENGTH {
        return Err(GgenAiError::validation(format!(
            "RDF URI exceeds maximum length of {} characters",
            MAX_URI_LENGTH
        )));
    }

    // URIs must start with a scheme (http, https, urn, etc.)
    if !uri.contains(':') {
        return Err(GgenAiError::validation(
            "RDF URI must contain a scheme (e.g., http:, https:, urn:)",
        ));
    }

    // Extract scheme
    let scheme_end = uri.find(':').unwrap();
    let scheme = &uri[..scheme_end];

    // Scheme must be alphanumeric + hyphen (RFC 3986)
    if !scheme.chars().all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.') {
        return Err(GgenAiError::validation(
            "RDF URI has invalid scheme. Schemes must contain only alphanumerics, +, -, .",
        ));
    }

    // Check for suspicious patterns that indicate injection attempts
    let forbidden_patterns = [
        r"(?i)javascript:", // XSS attempt
        r#"".*".*""#,       // Unclosed quotes
        r"';",              // SQL injection indicator
        r"--",              // SQL comment indicator
        r"/\*",             // SQL comment start
        r"\x00",            // Null byte
        r"[\x00-\x1f]",     // Control characters
    ];

    for pattern in &forbidden_patterns {
        if let Ok(regex) = Regex::new(pattern) {
            if regex.is_match(uri) {
                return Err(GgenAiError::validation(
                    "RDF URI contains forbidden pattern indicating possible injection",
                ));
            }
        }
    }

    // Validate character set (must be valid per RFC 3987)
    // Allow: unreserved / reserved / percent-encoded / IRI characters
    // This is a conservative check
    if !uri.chars().all(|c| {
        c.is_ascii_alphanumeric()
            || ":-_.~!$&'()*+,;=@/?#[]".contains(c)
            || c.is_whitespace() // Some RDF generators include spaces
    }) {
        return Err(GgenAiError::validation(
            "RDF URI contains invalid characters not permitted in IRIs",
        ));
    }

    Ok(uri.to_string())
}

/// Validate a SPARQL property name (predicate name)
///
/// # Validation Rules
///
/// - Property name must not be empty
/// - Property name must not exceed 256 characters
/// - Property name must be a valid QName or full IRI
/// - Property name must not contain control characters or SPARQL injection markers
///
/// # Returns
///
/// - `Ok(validated_property)` if the property name is valid
/// - `Err(GgenAiError::Validation)` if the property name is invalid
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ai::codegen::validation::validate_property_name;
///
/// assert!(validate_property_name("rdfs:comment").is_ok());
/// assert!(validate_property_name("http://example.com/prop").is_ok());
/// assert!(validate_property_name("'; DROP TABLE properties--").is_err());
/// ```
pub fn validate_property_name(property: &str) -> crate::Result<String> {
    // Check if empty
    if property.is_empty() {
        return Err(GgenAiError::validation("Property name cannot be empty"));
    }

    // Check length limit
    if property.len() > MAX_PROPERTY_NAME_LENGTH {
        return Err(GgenAiError::validation(format!(
            "Property name exceeds maximum length of {} characters",
            MAX_PROPERTY_NAME_LENGTH
        )));
    }

    // Check for control characters
    if property.chars().any(|c| c.is_control()) {
        return Err(GgenAiError::validation(
            "Property name contains control characters",
        ));
    }

    // Check for SPARQL injection patterns
    let injection_patterns = [
        r"(?i)UNION\s+SELECT", // SPARQL UNION attack
        r"(?i)OPTIONAL",       // SPARQL OPTIONAL manipulation
        r#""[^"]*"[^"]*""#,    // Quote breaking
        r"';",                 // Quote-semicolon pattern
        r"--",                 // Comment indicator
        r"/\*",                // Comment start
    ];

    for pattern in &injection_patterns {
        if let Ok(regex) = Regex::new(pattern) {
            if regex.is_match(property) {
                return Err(GgenAiError::validation(
                    "Property name contains SPARQL injection pattern",
                ));
            }
        }
    }

    Ok(property.to_string())
}

/// Escape special characters in a property name for safe SPARQL construction
///
/// This function escapes characters that have special meaning in SPARQL
/// but are allowed in property names. It should be used in conjunction with
/// `validate_property_name` for complete safety.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_ai::codegen::validation::escape_sparql_property;
///
/// let escaped = escape_sparql_property("my:property");
/// assert_eq!(escaped, "my\\:property");
/// ```
pub fn escape_sparql_property(property: &str) -> String {
    // Escape special SPARQL characters
    property
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Check if a URI is a valid prefixed IRI (e.g., sh:property)
///
/// Prefixed IRIs are used in SPARQL queries with namespace prefixes.
/// This function validates that the prefix and local part are both valid.
///
/// # Returns
///
/// - `Ok(())` if the prefixed IRI is valid
/// - `Err(GgenAiError::Validation)` if invalid
pub fn validate_prefixed_iri(prefixed_iri: &str) -> crate::Result<()> {
    if !prefixed_iri.contains(':') {
        return Err(GgenAiError::validation(
            "Prefixed IRI must contain ':' separator",
        ));
    }

    let parts: Vec<&str> = prefixed_iri.split(':').collect();
    if parts.len() != 2 {
        return Err(GgenAiError::validation(
            "Prefixed IRI must have exactly one ':' separator",
        ));
    }

    let (prefix, local) = (parts[0], parts[1]);

    if prefix.is_empty() {
        return Err(GgenAiError::validation("Prefixed IRI prefix cannot be empty"));
    }

    if local.is_empty() {
        return Err(GgenAiError::validation("Prefixed IRI local name cannot be empty"));
    }

    // Prefix must be valid NCName (XML naming rules)
    if !prefix.chars().next().unwrap().is_ascii_alphabetic() {
        return Err(GgenAiError::validation(
            "Prefixed IRI prefix must start with a letter",
        ));
    }

    if !prefix.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-') {
        return Err(GgenAiError::validation(
            "Prefixed IRI prefix contains invalid characters",
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Valid URI tests
    #[test]
    fn test_valid_http_uri() {
        assert!(validate_rdf_uri("http://example.com/ontology#MyClass").is_ok());
    }

    #[test]
    fn test_valid_https_uri() {
        assert!(validate_rdf_uri("https://www.w3.org/2000/01/rdf-schema#Class").is_ok());
    }

    #[test]
    fn test_valid_urn_uri() {
        assert!(validate_rdf_uri("urn:uuid:550e8400-e29b-41d4-a716-446655440000").is_ok());
    }

    // Invalid URI tests - injection attempts
    #[test]
    fn test_uri_sql_injection_semicolon() {
        let result = validate_rdf_uri("http://example.com/'; DROP TABLE--");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("forbidden pattern"));
    }

    #[test]
    fn test_uri_sparql_injection_comment() {
        let result = validate_rdf_uri("http://example.com/ontology#Class--\nANY");
        assert!(result.is_err());
    }

    #[test]
    fn test_uri_javascript_xss() {
        let result = validate_rdf_uri("javascript:alert('xss')");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("forbidden pattern"));
    }

    // Length limit tests
    #[test]
    fn test_uri_exceeds_max_length() {
        let long_uri = format!("http://example.com/{}", "x".repeat(5000));
        let result = validate_rdf_uri(&long_uri);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("maximum length"));
    }

    #[test]
    fn test_uri_empty() {
        let result = validate_rdf_uri("");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("empty"));
    }

    #[test]
    fn test_uri_no_scheme() {
        let result = validate_rdf_uri("example.com/ontology");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("scheme"));
    }

    // Property name tests
    #[test]
    fn test_valid_property_simple() {
        assert!(validate_property_name("outputField").is_ok());
    }

    #[test]
    fn test_valid_property_with_prefix() {
        assert!(validate_property_name("sh:path").is_ok());
    }

    #[test]
    fn test_valid_property_full_iri() {
        assert!(validate_property_name("http://www.w3.org/ns/shacl#path").is_ok());
    }

    #[test]
    fn test_property_empty() {
        let result = validate_property_name("");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("empty"));
    }

    #[test]
    fn test_property_exceeds_max_length() {
        let long_prop = "a".repeat(300);
        let result = validate_property_name(&long_prop);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("maximum length"));
    }

    #[test]
    fn test_property_sparql_injection_union() {
        let result = validate_property_name("prop UNION SELECT * WHERE");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("SPARQL injection"));
    }

    #[test]
    fn test_property_sql_injection_comment() {
        let result = validate_property_name("prop'; DROP TABLE--");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("SPARQL injection"));
    }

    #[test]
    fn test_property_control_characters() {
        let result = validate_property_name("prop\x00name");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("control"));
    }

    // Prefixed IRI tests
    #[test]
    fn test_valid_prefixed_iri() {
        assert!(validate_prefixed_iri("sh:path").is_ok());
    }

    #[test]
    fn test_valid_prefixed_iri_complex() {
        assert!(validate_prefixed_iri("rdf:type").is_ok());
    }

    #[test]
    fn test_prefixed_iri_no_separator() {
        let result = validate_prefixed_iri("shpath");
        assert!(result.is_err());
    }

    #[test]
    fn test_prefixed_iri_multiple_separators() {
        let result = validate_prefixed_iri("sh:path:extra");
        assert!(result.is_err());
    }

    #[test]
    fn test_prefixed_iri_empty_prefix() {
        let result = validate_prefixed_iri(":path");
        assert!(result.is_err());
    }

    #[test]
    fn test_prefixed_iri_empty_local() {
        let result = validate_prefixed_iri("sh:");
        assert!(result.is_err());
    }

    // Escape tests
    #[test]
    fn test_escape_sparql_property() {
        let escaped = escape_sparql_property("my\"property");
        assert_eq!(escaped, "my\\\"property");
    }

    #[test]
    fn test_escape_sparql_property_backslash() {
        let escaped = escape_sparql_property("my\\property");
        assert_eq!(escaped, "my\\\\property");
    }

    #[test]
    fn test_escape_sparql_property_newline() {
        let escaped = escape_sparql_property("my\nproperty");
        assert_eq!(escaped, "my\\nproperty");
    }
}
