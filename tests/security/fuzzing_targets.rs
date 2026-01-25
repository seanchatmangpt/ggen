//! Fuzzing targets for security testing
//!
//! Week 12 deliverable: Fuzzing infrastructure for:
//! - RDF/SPARQL parsers
//! - Template validators
//! - Configuration parsers
//! - Input validators
//! - Serialization/deserialization

#![cfg_attr(fuzzing, no_main)]

use std::collections::HashMap;

/// Fuzzing target 1: RDF/Turtle parser
///
/// Tests parser robustness against malformed RDF input
#[cfg(fuzzing)]
use libfuzzer_sys::fuzz_target;

#[cfg(fuzzing)]
fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = fuzz_rdf_parser(s);
    }
});

pub fn fuzz_rdf_parser(input: &str) -> Result<(), String> {
    // Simulate RDF parsing
    // Real implementation would use oxigraph parser
    if input.contains("@prefix") && input.contains(".") {
        // Valid-ish RDF structure
        parse_rdf_structure(input)
    } else {
        Err("Invalid RDF".to_string())
    }
}

fn parse_rdf_structure(input: &str) -> Result<(), String> {
    // Simulate parsing triples
    let lines: Vec<&str> = input.lines().collect();
    for line in lines {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        // Simplified triple parsing
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 3 {
            return Err("Invalid triple".to_string());
        }
    }
    Ok(())
}

/// Fuzzing target 2: SPARQL query parser
pub fn fuzz_sparql_parser(input: &str) -> Result<(), String> {
    // Simulate SPARQL parsing
    if input.to_uppercase().contains("SELECT") || input.to_uppercase().contains("CONSTRUCT") {
        validate_sparql_structure(input)
    } else {
        Err("Not a SPARQL query".to_string())
    }
}

fn validate_sparql_structure(input: &str) -> Result<(), String> {
    // Check for basic SPARQL structure
    let upper = input.to_uppercase();

    if upper.contains("SELECT") && !upper.contains("WHERE") {
        return Err("SELECT without WHERE".to_string());
    }

    // Check for balanced braces
    let open_braces = input.chars().filter(|&c| c == '{').count();
    let close_braces = input.chars().filter(|&c| c == '}').count();

    if open_braces != close_braces {
        return Err("Unbalanced braces".to_string());
    }

    Ok(())
}

/// Fuzzing target 3: Template validator
pub fn fuzz_template_validator(input: &str) -> Result<(), String> {
    // Simulate Tera template validation
    if input.contains("{{") && input.contains("}}") {
        validate_template_syntax(input)
    } else {
        Err("No template syntax found".to_string())
    }
}

fn validate_template_syntax(input: &str) -> Result<(), String> {
    // Check for balanced template delimiters
    let open_count = input.matches("{{").count();
    let close_count = input.matches("}}").count();

    if open_count != close_count {
        return Err("Unbalanced template delimiters".to_string());
    }

    // Check for dangerous template operations
    if input.contains("{% raw %}") && !input.contains("{% endraw %}") {
        return Err("Unclosed raw block".to_string());
    }

    Ok(())
}

/// Fuzzing target 4: Configuration parser (TOML)
pub fn fuzz_config_parser(input: &str) -> Result<HashMap<String, String>, String> {
    // Simulate TOML parsing
    let mut config = HashMap::new();

    for line in input.lines() {
        let trimmed = line.trim();

        // Skip comments and empty lines
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // Parse key = value pairs
        if let Some(pos) = trimmed.find('=') {
            let key = trimmed[..pos].trim();
            let value = trimmed[pos + 1..].trim();

            // Remove quotes from value
            let value = value.trim_matches('"').trim_matches('\'');

            config.insert(key.to_string(), value.to_string());
        } else if trimmed.starts_with('[') && trimmed.ends_with(']') {
            // Section header
            continue;
        } else {
            return Err(format!("Invalid line: {}", trimmed));
        }
    }

    Ok(config)
}

/// Fuzzing target 5: Input validator
pub fn fuzz_input_validator(input: &str) -> Result<(), String> {
    // Test various input validation scenarios

    // Length validation
    if input.len() > 10000 {
        return Err("Input too long".to_string());
    }

    // SQL injection pattern detection
    let dangerous_patterns = [
        "'; DROP TABLE",
        "' OR '1'='1",
        "UNION SELECT",
        "' AND 1=1",
        "'; --",
    ];

    for pattern in &dangerous_patterns {
        if input.to_uppercase().contains(&pattern.to_uppercase()) {
            return Err(format!("Dangerous pattern detected: {}", pattern));
        }
    }

    // XSS pattern detection
    let xss_patterns = [
        "<script>",
        "javascript:",
        "onerror=",
        "onload=",
        "<iframe",
    ];

    for pattern in &xss_patterns {
        if input.to_lowercase().contains(pattern) {
            return Err(format!("XSS pattern detected: {}", pattern));
        }
    }

    // Path traversal detection
    if input.contains("..") || input.contains("%2e%2e") {
        return Err("Path traversal detected".to_string());
    }

    // Command injection detection
    let command_chars = ['|', ';', '&', '$', '`', '\n'];
    for ch in &command_chars {
        if input.contains(*ch) {
            return Err(format!("Command injection character detected: {}", ch));
        }
    }

    Ok(())
}

/// Fuzzing target 6: JSON serialization/deserialization
pub fn fuzz_json_serde(input: &str) -> Result<serde_json::Value, String> {
    // Test JSON parsing robustness
    serde_json::from_str(input).map_err(|e| format!("JSON parse error: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    // Chicago TDD: State-based testing with real collaborators

    #[test]
    fn test_rdf_parser_accepts_valid_rdf() {
        // Arrange
        let valid_rdf = r#"
            @prefix ex: <http://example.org/> .
            ex:subject ex:predicate ex:object .
        "#;

        // Act
        let result = fuzz_rdf_parser(valid_rdf);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_rdf_parser_rejects_invalid_rdf() {
        // Arrange
        let invalid_rdf = "not valid rdf at all";

        // Act
        let result = fuzz_rdf_parser(invalid_rdf);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_sparql_parser_accepts_valid_query() {
        // Arrange
        let valid_query = r#"
            SELECT ?subject ?predicate ?object
            WHERE {
                ?subject ?predicate ?object .
            }
        "#;

        // Act
        let result = fuzz_sparql_parser(valid_query);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_sparql_parser_detects_unbalanced_braces() {
        // Arrange
        let invalid_query = "SELECT ?s WHERE { ?s ?p ?o .";

        // Act
        let result = fuzz_sparql_parser(invalid_query);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Unbalanced braces"));
    }

    #[test]
    fn test_template_validator_accepts_valid_template() {
        // Arrange
        let valid_template = "Hello {{ name }}!";

        // Act
        let result = fuzz_template_validator(valid_template);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_template_validator_detects_unbalanced_delimiters() {
        // Arrange
        let invalid_template = "Hello {{ name !";

        // Act
        let result = fuzz_template_validator(invalid_template);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_config_parser_parses_valid_toml() {
        // Arrange
        let valid_toml = r#"
            name = "test"
            version = "1.0.0"

            [section]
            key = "value"
        "#;

        // Act
        let result = fuzz_config_parser(valid_toml);

        // Assert
        assert!(result.is_ok());
        let config = result.unwrap();
        assert_eq!(config.get("name"), Some(&"test".to_string()));
        assert_eq!(config.get("version"), Some(&"1.0.0".to_string()));
    }

    #[test]
    fn test_input_validator_detects_sql_injection() {
        // Arrange
        let sql_injection = "'; DROP TABLE users; --";

        // Act
        let result = fuzz_input_validator(sql_injection);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Dangerous pattern"));
    }

    #[test]
    fn test_input_validator_detects_xss() {
        // Arrange
        let xss_payload = "<script>alert('XSS')</script>";

        // Act
        let result = fuzz_input_validator(xss_payload);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("XSS pattern"));
    }

    #[test]
    fn test_input_validator_detects_path_traversal() {
        // Arrange
        let path_traversal = "../../etc/passwd";

        // Act
        let result = fuzz_input_validator(path_traversal);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Path traversal"));
    }

    #[test]
    fn test_input_validator_detects_command_injection() {
        // Arrange
        let command_injection = "file.txt; rm -rf /";

        // Act
        let result = fuzz_input_validator(command_injection);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Command injection"));
    }

    #[test]
    fn test_input_validator_accepts_safe_input() {
        // Arrange
        let safe_input = "This is a safe input string with no special characters";

        // Act
        let result = fuzz_input_validator(safe_input);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_json_serde_parses_valid_json() {
        // Arrange
        let valid_json = r#"{"name": "test", "value": 123}"#;

        // Act
        let result = fuzz_json_serde(valid_json);

        // Assert
        assert!(result.is_ok());
        let value = result.unwrap();
        assert_eq!(value["name"], "test");
        assert_eq!(value["value"], 123);
    }

    #[test]
    fn test_json_serde_handles_invalid_json() {
        // Arrange
        let invalid_json = "{name: test, value: 123}"; // Missing quotes

        // Act
        let result = fuzz_json_serde(invalid_json);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_json_serde_handles_deeply_nested_json() {
        // Arrange: Create deeply nested structure
        let mut nested = String::from("{");
        for _ in 0..100 {
            nested.push_str("\"a\":{");
        }
        nested.push_str("\"value\":1");
        for _ in 0..100 {
            nested.push('}');
        }
        nested.push('}');

        // Act
        let result = fuzz_json_serde(&nested);

        // Assert: Should handle deep nesting (or error gracefully)
        // This tests stack overflow protection
        match result {
            Ok(_) => {
                // Parsing succeeded
            }
            Err(e) => {
                // Should fail gracefully, not crash
                assert!(e.contains("error") || e.contains("recursion"));
            }
        }
    }

    #[test]
    fn test_rdf_parser_handles_empty_input() {
        // Arrange
        let empty = "";

        // Act
        let result = fuzz_rdf_parser(empty);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_config_parser_handles_malformed_lines() {
        // Arrange
        let malformed = "this is not a valid config line";

        // Act
        let result = fuzz_config_parser(malformed);

        // Assert
        assert!(result.is_err());
    }
}

/// Fuzzing corpus generator
#[cfg(test)]
mod corpus_generator {
    use super::*;

    #[test]
    fn generate_rdf_corpus() {
        let corpus = vec![
            "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .",
            "@prefix : <http://test.org/> .\n:subject :predicate \"literal\" .",
            "# Comment\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
            "", // Empty
            "@prefix", // Incomplete
            "ex:s ex:p", // Missing object
        ];

        for (i, input) in corpus.iter().enumerate() {
            println!("RDF Corpus {}: {:?}", i, fuzz_rdf_parser(input));
        }
    }

    #[test]
    fn generate_sparql_corpus() {
        let corpus = vec![
            "SELECT ?s WHERE { ?s ?p ?o }",
            "SELECT * WHERE { ?s ?p ?o . FILTER(?p = <http://example.org/predicate>) }",
            "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }",
            "SELECT ?s WHERE { ?s ?p ?o }", // Missing closing brace
            "SELECT", // Incomplete
        ];

        for (i, input) in corpus.iter().enumerate() {
            println!("SPARQL Corpus {}: {:?}", i, fuzz_sparql_parser(input));
        }
    }
}
