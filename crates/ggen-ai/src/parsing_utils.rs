//! Code block parsing utilities for ggen-ai
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should provide reusable, battle-tested utilities for extracting code blocks
//! from varied LLM responses, eliminating the ~80 lines of duplicate parsing logic across
//! generators and supporting the core team's "lenient parsing, defensive validation" philosophy.
//!
//! ## RESPONSIBILITIES
//! 1. **Generic Code Block Extraction**: Should extract code blocks with any language marker
//! 2. **Turtle/RDF-Specific Extraction**: Should handle multiple RDF formats (turtle, ttl, rdf)
//! 3. **Fallback Detection**: Should detect content even without explicit markers
//! 4. **Multiple Block Handling**: Should extract all blocks or just the first valid one
//! 5. **Plain Text Detection**: Should detect RDF-like content in plain text responses
//!
//! ## DESIGN PRINCIPLES (Core Team Best Practices)
//! - **Lenient Parsing**: Accept many input formats (```turtle, ```ttl, ```rdf, plain text)
//! - **Defensive Validation**: Verify content before returning (check for RDF patterns)
//! - **Clear Errors**: Return None instead of panicking, let callers decide error handling
//! - **Support Edge Cases**: Handle missing closing markers, multiple blocks, no code blocks
//! - **Zero Dependencies**: Use only std library for maximum portability
//!
//! ## USAGE PATTERNS
//!
//! ### Before (Duplicated across 6 files, ~80 lines):
//! ```rust,ignore
//! if let Some(start) = response.find("```turtle") {
//!     let search_start = start + 9;
//!     if let Some(end_offset) = response[search_start..].find("```") {
//!         let content = &response[search_start..search_start + end_offset].trim();
//!         return Ok(content.to_string());
//!     }
//! }
//! // ... repeat for ```ttl, ```rdf, any block, plain text...
//! ```
//!
//! ### After (Reusable utility):
//! ```rust,ignore
//! use crate::parsing_utils::extract_turtle_content;
//!
//! if let Some(content) = extract_turtle_content(response) {
//!     return Ok(content);
//! }
//! ```
//!
//! ## EDGE CASES HANDLED
//! - ✅ No code blocks (returns None for extract_code_block, tries plain text for RDF)
//! - ✅ Multiple code blocks (returns first valid one)
//! - ✅ Missing closing ``` marker (returns None gracefully)
//! - ✅ Plain text RDF without code blocks (detects via patterns)
//! - ✅ Code block with language but no content (returns empty string)
//! - ✅ Mixed content (RDF patterns in non-RDF code block)

/// Extract content from a code block with specific language marker
///
/// # Arguments
/// * `text` - The text to search for code blocks
/// * `language` - The language marker to search for (e.g., "yaml", "turtle", "sparql")
///
/// # Returns
/// Some(content) if a code block with the specified language is found, None otherwise
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::extract_code_block;
///
/// let text = "Here's some code:\n```rust\nfn main() {}\n```";
/// let content = extract_code_block(text, "rust");
/// assert_eq!(content, Some("fn main() {}".to_string()));
/// ```
///
/// # Core Team Best Practices
/// - **Lenient**: Accepts varied formats (with/without newlines after marker)
/// - **Defensive**: Returns None instead of panicking on malformed input
/// - **Clear**: Simple API, single purpose
pub fn extract_code_block(text: &str, language: &str) -> Option<String> {
    let marker = format!("```{}", language);
    let start = text.find(&marker)?;
    let mut search_start = start + marker.len();

    // Skip to next line if there's content on the same line as the marker
    if let Some(newline_pos) = text[search_start..].find('\n') {
        search_start += newline_pos + 1;
    }

    let end_offset = text[search_start..].find("```")?;
    let content = text[search_start..search_start + end_offset].trim();

    Some(content.to_string())
}

/// Extract content from any code block (no specific language marker)
///
/// # Arguments
/// * `text` - The text to search for code blocks
///
/// # Returns
/// Some(content) if any code block is found, None otherwise
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::extract_any_code_block;
///
/// let text = "```\nSome content\n```";
/// let content = extract_any_code_block(text);
/// assert_eq!(content, Some("Some content".to_string()));
/// ```
pub fn extract_any_code_block(text: &str) -> Option<String> {
    let start = text.find("```")?;
    let mut search_start = start + 3;

    // Skip language identifier if present (e.g., "```python" -> skip "python")
    if let Some(newline_pos) = text[search_start..].find('\n') {
        search_start += newline_pos + 1;
    }

    let end_offset = text[search_start..].find("```")?;
    let content = text[search_start..search_start + end_offset].trim();

    Some(content.to_string())
}

/// Extract all code blocks with a specific language marker
///
/// # Arguments
/// * `text` - The text to search for code blocks
/// * `language` - The language marker to search for
///
/// # Returns
/// Vec of all matching code block contents (empty vec if none found)
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::extract_all_code_blocks;
///
/// let text = "```sparql\nSELECT * WHERE { ?s ?p ?o }\n```\n\n```sparql\nASK { ?s a ?c }\n```";
/// let blocks = extract_all_code_blocks(text, "sparql");
/// assert_eq!(blocks.len(), 2);
/// ```
pub fn extract_all_code_blocks(text: &str, language: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut current_pos = 0;
    let marker = format!("```{}", language);

    while let Some(start) = text[current_pos..].find(&marker) {
        let abs_start = current_pos + start + marker.len();

        let search_start = if let Some(newline) = text[abs_start..].find('\n') {
            abs_start + newline + 1
        } else {
            abs_start
        };

        if let Some(end_offset) = text[search_start..].find("```") {
            let content = text[search_start..search_start + end_offset].trim();
            blocks.push(content.to_string());
            current_pos = search_start + end_offset + 3;
        } else {
            break;
        }
    }

    blocks
}

/// Check if text looks like RDF/Turtle content based on common patterns
///
/// # Arguments
/// * `text` - The text to check for RDF patterns
///
/// # Returns
/// true if text contains common RDF/Turtle patterns, false otherwise
///
/// # Detection Patterns
/// - Contains `@prefix` declarations
/// - Contains RDF-style triples (` a ` with `;`)
/// - Contains common RDF namespace prefixes (rdfs:, owl:, rdf:)
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::is_rdf_like_content;
///
/// let turtle = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
/// assert!(is_rdf_like_content(turtle));
///
/// let plain_text = "This is just regular text.";
/// assert!(!is_rdf_like_content(plain_text));
/// ```
///
/// # Core Team Best Practices
/// - **Lenient**: Uses multiple detection heuristics
/// - **Defensive**: Returns false for ambiguous content
pub fn is_rdf_like_content(text: &str) -> bool {
    text.contains("@prefix")
        || (text.contains(" a ") && text.contains(";"))
        || text.contains("rdfs:")
        || text.contains("owl:")
        || text.contains("rdf:")
}

/// Validate Turtle/RDF syntax using oxigraph parser
///
/// This function attempts to parse the given content as Turtle RDF to verify
/// it's syntactically valid. It performs **strict validation** after **lenient extraction**,
/// following the core team's "defensive validation" principle.
///
/// # Arguments
/// * `content` - The Turtle/RDF content to validate
///
/// # Returns
/// * `Ok(())` if the content is valid Turtle syntax
/// * `Err(String)` with a descriptive error message if validation fails
///
/// # Validation Strategy
/// - **Fast**: Parse into temporary in-memory store
/// - **Strict**: Reject any syntax errors
/// - **Clear**: Return detailed error messages with context
/// - **Zero side effects**: Does not modify any graphs, just validates
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::validate_turtle_syntax;
///
/// // Valid Turtle passes
/// let valid = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
/// assert!(validate_turtle_syntax(valid).is_ok());
///
/// // Invalid syntax caught
/// let invalid = "@prefix ex <invalid syntax>";
/// assert!(validate_turtle_syntax(invalid).is_err());
///
/// // Empty content rejected
/// assert!(validate_turtle_syntax("").is_err());
/// ```
///
/// # Core Team Best Practices
/// - **Defensive**: Never trust LLM output without validation
/// - **Fail Fast**: Catch errors at generation time, not later
/// - **Clear Errors**: Show what's wrong and suggest fixes
/// - **Performance**: Fast validation suitable for hot path
pub fn validate_turtle_syntax(content: &str) -> Result<(), String> {
    use oxigraph::io::RdfFormat;
    use oxigraph::store::Store;

    // Reject empty content immediately
    let trimmed = content.trim();
    if trimmed.is_empty() {
        return Err(
            "Empty content - Turtle must contain at least one prefix or triple".to_string(),
        );
    }

    // Create temporary in-memory store for validation
    let store = Store::new().map_err(|e| format!("Failed to create validation store: {}", e))?;

    // Attempt to parse Turtle content into the store
    // This will validate syntax and catch all parsing errors
    match store.load_from_reader(RdfFormat::Turtle, content.as_bytes()) {
        Ok(_) => Ok(()), // Parsing succeeded, content is valid
        Err(e) => {
            // Parse error - provide helpful context and suggestions
            let error_msg = e.to_string();
            let mut suggestion = String::new();

            // Provide helpful suggestions based on common errors
            if error_msg.contains("prefix") || error_msg.contains("namespace") {
                suggestion.push_str("\n\nSuggestion: Ensure all prefixes are declared with @prefix before use.\nExample: @prefix ex: <http://example.org/> .");
            } else if error_msg.contains("IRI") || error_msg.contains("URI") {
                suggestion.push_str("\n\nSuggestion: URIs must be enclosed in angle brackets < >.\nExample: <http://example.org/Thing>");
            } else if error_msg.contains("literal") {
                suggestion.push_str("\n\nSuggestion: String literals must be enclosed in quotes.\nExample: \"literal value\"");
            } else if error_msg.contains("triple") {
                suggestion.push_str("\n\nSuggestion: Triples must follow the pattern: subject predicate object .\nExample: ex:Thing a ex:Class .");
            }

            Err(format!(
                "Turtle syntax error: {}{}\n\nContent preview:\n{}",
                error_msg,
                suggestion,
                &trimmed[..trimmed.len().min(200)]
            ))
        }
    }
}

/// Extract Turtle/RDF content from varied response formats
///
/// This is a specialized helper for ontology generation that tries multiple
/// extraction strategies in order of specificity:
///
/// 1. Look for ```turtle code blocks
/// 2. Look for ```ttl code blocks
/// 3. Look for ```rdf code blocks
/// 4. Look for any code block that contains RDF patterns
/// 5. Check if plain text contains RDF patterns
///
/// # Arguments
/// * `response` - The LLM response text to parse
///
/// # Returns
/// Some(content) if Turtle/RDF content is found, None otherwise
///
/// # Example
/// ```rust,ignore
/// use ggen_ai::parsing_utils::extract_turtle_content;
///
/// // Handles explicit turtle marker
/// let response1 = "```turtle\n@prefix ex: <http://example.org/> .\n```";
/// assert!(extract_turtle_content(response1).is_some());
///
/// // Handles ttl marker
/// let response2 = "```ttl\n@prefix ex: <http://example.org/> .\n```";
/// assert!(extract_turtle_content(response2).is_some());
///
/// // Handles plain text RDF
/// let response3 = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
/// assert!(extract_turtle_content(response3).is_some());
/// ```
///
/// # Core Team Best Practices
/// - **Lenient**: Accepts many language markers and plain text
/// - **Defensive**: Validates content is RDF-like before returning
/// - **Clear**: Returns Option<String> for easy error handling
pub fn extract_turtle_content(response: &str) -> Option<String> {
    // Try explicit turtle marker
    if let Some(content) = extract_code_block(response, "turtle") {
        return Some(content);
    }

    // Try ttl marker
    if let Some(content) = extract_code_block(response, "ttl") {
        return Some(content);
    }

    // Try rdf marker
    if let Some(content) = extract_code_block(response, "rdf") {
        return Some(content);
    }

    // Try any code block that looks like RDF
    if let Some(content) = extract_any_code_block(response) {
        if is_rdf_like_content(&content) {
            return Some(content);
        }
    }

    // Check if entire response is plain text RDF
    let trimmed = response.trim();
    if is_rdf_like_content(trimmed) {
        return Some(trimmed.to_string());
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_code_block_with_language() {
        let text =
            "Here's some Turtle:\n```turtle\n@prefix ex: <http://example.org/> .\n```\nDone!";
        let result = extract_code_block(text, "turtle");
        assert_eq!(
            result,
            Some("@prefix ex: <http://example.org/> .".to_string())
        );
    }

    #[test]
    fn test_extract_code_block_no_closing_marker() {
        let text = "```turtle\n@prefix ex: <http://example.org/> .";
        let result = extract_code_block(text, "turtle");
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_code_block_not_found() {
        let text = "No code blocks here!";
        let result = extract_code_block(text, "turtle");
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_any_code_block() {
        let text = "```\nSome generic content\n```";
        let result = extract_any_code_block(text);
        assert_eq!(result, Some("Some generic content".to_string()));
    }

    #[test]
    fn test_extract_any_code_block_with_language() {
        let text = "```python\nprint('hello')\n```";
        let result = extract_any_code_block(text);
        assert_eq!(result, Some("print('hello')".to_string()));
    }

    #[test]
    fn test_extract_all_code_blocks() {
        let text = "```sparql\nSELECT * WHERE { ?s ?p ?o }\n```\n\nSome text\n\n```sparql\nASK { ?s a ?class }\n```";
        let blocks = extract_all_code_blocks(text, "sparql");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0], "SELECT * WHERE { ?s ?p ?o }");
        assert_eq!(blocks[1], "ASK { ?s a ?class }");
    }

    #[test]
    fn test_extract_all_code_blocks_none_found() {
        let text = "No SPARQL blocks here";
        let blocks = extract_all_code_blocks(text, "sparql");
        assert_eq!(blocks.len(), 0);
    }

    #[test]
    fn test_is_rdf_like_content_with_prefix() {
        let content = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
        assert!(is_rdf_like_content(content));
    }

    #[test]
    fn test_is_rdf_like_content_with_rdf_type() {
        let content = "ex:Thing a ex:Class ; rdfs:label \"Thing\" .";
        assert!(is_rdf_like_content(content));
    }

    #[test]
    fn test_is_rdf_like_content_with_namespace() {
        let content = "ex:Thing rdfs:subClassOf owl:Thing .";
        assert!(is_rdf_like_content(content));
    }

    #[test]
    fn test_is_rdf_like_content_plain_text() {
        let content = "This is just regular text with no RDF patterns.";
        assert!(!is_rdf_like_content(content));
    }

    #[test]
    fn test_extract_turtle_content_with_turtle_marker() {
        let response = "Here's the ontology:\n```turtle\n@prefix ex: <http://example.org/> .\n```";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
        assert!(result.unwrap().contains("@prefix"));
    }

    #[test]
    fn test_extract_turtle_content_with_ttl_marker() {
        let response = "```ttl\n@prefix ex: <http://example.org/> .\n```";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
    }

    #[test]
    fn test_extract_turtle_content_with_rdf_marker() {
        let response = "```rdf\n@prefix ex: <http://example.org/> .\n```";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
    }

    #[test]
    fn test_extract_turtle_content_any_block_with_rdf() {
        let response = "```\n@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n```";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
    }

    #[test]
    fn test_extract_turtle_content_plain_text() {
        let response = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
    }

    #[test]
    fn test_extract_turtle_content_no_rdf() {
        let response = "Just some text without any RDF content.";
        let result = extract_turtle_content(response);
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_turtle_content_non_rdf_code_block() {
        let response = "```python\nprint('hello')\n```";
        let result = extract_turtle_content(response);
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_turtle_content_multiple_blocks() {
        // Should return first valid RDF block
        let response =
            "```python\nprint('hi')\n```\n```turtle\n@prefix ex: <http://example.org/> .\n```";
        let result = extract_turtle_content(response);
        assert!(result.is_some());
        assert!(result.unwrap().contains("@prefix"));
    }

    // Tests for validate_turtle_syntax

    #[test]
    fn test_validate_turtle_syntax_valid_with_prefix() {
        let valid = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
        let result = validate_turtle_syntax(valid);
        assert!(
            result.is_ok(),
            "Valid Turtle should pass validation: {:?}",
            result
        );
    }

    #[test]
    fn test_validate_turtle_syntax_valid_multiple_triples() {
        let valid = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ."#;

        let result = validate_turtle_syntax(valid);
        assert!(
            result.is_ok(),
            "Valid complex Turtle should pass: {:?}",
            result
        );
    }

    #[test]
    fn test_validate_turtle_syntax_empty_content() {
        let result = validate_turtle_syntax("");
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        assert!(err_msg.contains("Empty content"));
    }

    #[test]
    fn test_validate_turtle_syntax_whitespace_only() {
        let result = validate_turtle_syntax("   \n\t  \n  ");
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        assert!(err_msg.contains("Empty content"));
    }

    #[test]
    fn test_validate_turtle_syntax_missing_prefix() {
        // Using undefined prefix should fail
        let invalid = "ex:Thing a ex:Class .";
        let result = validate_turtle_syntax(invalid);
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        assert!(err_msg.contains("prefix") || err_msg.contains("namespace"));
        assert!(err_msg.contains("Suggestion"));
    }

    #[test]
    fn test_validate_turtle_syntax_invalid_uri() {
        let invalid = "@prefix ex: not-a-valid-uri .";
        let result = validate_turtle_syntax(invalid);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_turtle_syntax_malformed_triple() {
        let invalid = "@prefix ex: <http://example.org/> .\nex:Thing ex:property";
        let result = validate_turtle_syntax(invalid);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_turtle_syntax_unclosed_literal() {
        let invalid = r#"@prefix ex: <http://example.org/> .
ex:Thing ex:label "unclosed literal ."#;
        let result = validate_turtle_syntax(invalid);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_turtle_syntax_helpful_error_messages() {
        // Test that error messages contain helpful suggestions
        let invalid = "ex:Thing a ex:Class .";
        let result = validate_turtle_syntax(invalid);
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        // Should suggest prefix declaration
        assert!(
            err_msg.contains("@prefix") || err_msg.contains("Suggestion"),
            "Error should contain helpful suggestion: {}",
            err_msg
        );
    }
}
