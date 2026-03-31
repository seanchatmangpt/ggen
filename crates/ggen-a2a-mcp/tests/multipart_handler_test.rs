//! Chicago TDD tests for MultipartHandler
//!
//! Tests cover all content types with actual content preservation.
//! Pattern: Unit tests for multipart processing logic.
//! AAA: Arrange / Act / Assert

use a2a_generated::converged::message::{UnifiedContent, UnifiedFileContent};
use ggen_a2a_mcp::handlers::MultipartHandler;
use serde_json::Map;

// ---------------------------------------------------------------------------
// Helper: Create a default multipart handler for testing
// ---------------------------------------------------------------------------

fn default_handler() -> MultipartHandler {
    MultipartHandler::new().with_max_parts(100)
}

// ---------------------------------------------------------------------------
// Test 1: Text content preservation
// ---------------------------------------------------------------------------

#[test]
fn test_text_content_preserves_actual_content() {
    // Arrange
    let handler = default_handler();
    let text_content = UnifiedContent::Text {
        content: "Hello, world!".to_string(),
        metadata: None,
    };
    let parts = vec![text_content];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Text ==="));
    assert!(output.contains("Hello, world!"));
    assert!(!output.contains("chars")); // Should NOT contain summary metadata
}

// ---------------------------------------------------------------------------
// Test 2: Multiple text parts
// ---------------------------------------------------------------------------

#[test]
fn test_multiple_text_parts_preserve_all_content() {
    // Arrange
    let handler = default_handler();
    let parts = vec![
        UnifiedContent::Text {
            content: "First message".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Second message".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Third message".to_string(),
            metadata: None,
        },
    ];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Text ==="));
    assert!(output.contains("First message"));
    assert!(output.contains("=== Part 2: Text ==="));
    assert!(output.contains("Second message"));
    assert!(output.contains("=== Part 3: Text ==="));
    assert!(output.contains("Third message"));
}

// ---------------------------------------------------------------------------
// Test 3: File content with embedded bytes
// ---------------------------------------------------------------------------

#[test]
fn test_file_content_preserves_metadata_with_embedded_bytes() {
    // Arrange
    let handler = default_handler();
    let file_content = UnifiedFileContent {
        name: Some("test.png".to_string()),
        mime_type: Some("image/png".to_string()),
        bytes: Some("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==".to_string()), // 1x1 red pixel PNG
        uri: None,
        size: Some(68),
        hash: Some("abc123".to_string()),
    };
    let file = UnifiedContent::File {
        file: file_content,
        metadata: None,
    };
    let parts = vec![file];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: File ==="));
    assert!(output.contains("Name: test.png"));
    assert!(output.contains("MIME: image/png"));
    assert!(output.contains("Size: 96 bytes")); // Base64 encoded length
    assert!(output.contains("[Embedded content available"));
    assert!(!output.contains("File (test.png)")); // Should NOT contain old summary format
}

// ---------------------------------------------------------------------------
// Test 4: File content with URI
// ---------------------------------------------------------------------------

#[test]
fn test_file_content_preserves_metadata_with_uri() {
    // Arrange
    let handler = default_handler();
    let file_content = UnifiedFileContent {
        name: Some("document.pdf".to_string()),
        mime_type: Some("application/pdf".to_string()),
        bytes: None,
        uri: Some("file:///path/to/document.pdf".to_string()),
        size: None,
        hash: None,
    };
    let file = UnifiedContent::File {
        file: file_content,
        metadata: None,
    };
    let parts = vec![file];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: File ==="));
    assert!(output.contains("Name: document.pdf"));
    assert!(output.contains("URI: file:///path/to/document.pdf"));
    assert!(output.contains("MIME: application/pdf"));
    assert!(output.contains("[Use file:// URI to access content"));
}

// ---------------------------------------------------------------------------
// Test 5: File content with no bytes or URI
// ---------------------------------------------------------------------------

#[test]
fn test_file_content_handles_missing_bytes_and_uri() {
    // Arrange
    let handler = default_handler();
    let file_content = UnifiedFileContent {
        name: Some("empty.txt".to_string()),
        mime_type: Some("text/plain".to_string()),
        bytes: None,
        uri: None,
        size: None,
        hash: None,
    };
    let file = UnifiedContent::File {
        file: file_content,
        metadata: None,
    };
    let parts = vec![file];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: File ==="));
    assert!(output.contains("[No content available - missing bytes and URI]"));
}

// ---------------------------------------------------------------------------
// Test 6: Data content preservation
// ---------------------------------------------------------------------------

#[test]
fn test_data_content_preserves_structured_data() {
    // Arrange
    let handler = default_handler();
    let mut data_map = Map::new();
    data_map.insert("key1".to_string(), serde_json::json!("value1"));
    data_map.insert("key2".to_string(), serde_json::json!(42));
    data_map.insert("key3".to_string(), serde_json::json!(true));

    let data = UnifiedContent::Data {
        data: data_map.clone(),
        schema: None,
    };
    let parts = vec![data];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Data ==="));
    assert!(output.contains("\"key1\""));
    assert!(output.contains("\"value1\""));
    assert!(output.contains("\"key2\""));
    assert!(output.contains("42"));
    assert!(output.contains("\"key3\""));
    assert!(output.contains("true"));
    assert!(!output.contains("keys")); // Should NOT contain summary metadata
}

// ---------------------------------------------------------------------------
// Test 7: Stream content preservation
// ---------------------------------------------------------------------------

#[test]
fn test_stream_content_preserves_metadata() {
    // Arrange
    let handler = default_handler();
    let stream = UnifiedContent::Stream {
        stream_id: "stream-abc123".to_string(),
        chunk_size: 8192,
        metadata: None,
    };
    let parts = vec![stream];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Stream ==="));
    assert!(output.contains("Stream ID: stream-abc123"));
    assert!(output.contains("Chunk Size: 8192 bytes"));
    assert!(output.contains("[Use streaming API to consume this stream]"));
    assert!(!output.contains("(stream-abc123)")); // Should NOT contain old summary format
}

// ---------------------------------------------------------------------------
// Test 8: Recursive multipart handling
// ---------------------------------------------------------------------------

#[test]
fn test_recursive_multipart_handling() {
    // Arrange
    let handler = default_handler();

    // Create nested multipart
    let inner_parts = vec![
        UnifiedContent::Text {
            content: "Inner text 1".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Inner text 2".to_string(),
            metadata: None,
        },
    ];

    let outer_parts = vec![
        UnifiedContent::Text {
            content: "Outer text".to_string(),
            metadata: None,
        },
        UnifiedContent::Multipart {
            parts: inner_parts,
            boundary: Some("inner-boundary".to_string()),
        },
    ];

    // Act
    let result = handler.process_multipart(&outer_parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Text ==="));
    assert!(output.contains("Outer text"));
    assert!(output.contains("=== Part 2: Nested Multipart ==="));
    assert!(output.contains("Inner text 1"));
    assert!(output.contains("Inner text 2"));
    assert!(!output.contains("Nested multipart")); // Should NOT contain old summary
}

// ---------------------------------------------------------------------------
// Test 9: Deeply nested multipart (3 levels)
// ---------------------------------------------------------------------------

#[test]
fn test_deeply_nested_multipart_handling() {
    // Arrange
    let handler = default_handler();

    // Level 3 (deepest)
    let level3_parts = vec![UnifiedContent::Text {
        content: "Level 3 text".to_string(),
        metadata: None,
    }];

    // Level 2
    let level2_parts = vec![
        UnifiedContent::Text {
            content: "Level 2 text".to_string(),
            metadata: None,
        },
        UnifiedContent::Multipart {
            parts: level3_parts,
            boundary: None,
        },
    ];

    // Level 1 (outermost)
    let level1_parts = vec![
        UnifiedContent::Text {
            content: "Level 1 text".to_string(),
            metadata: None,
        },
        UnifiedContent::Multipart {
            parts: level2_parts,
            boundary: None,
        },
    ];

    // Act
    let result = handler.process_multipart(&level1_parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("Level 1 text"));
    assert!(output.contains("Level 2 text"));
    assert!(output.contains("Level 3 text"));
}

// ---------------------------------------------------------------------------
// Test 10: Max parts validation
// ---------------------------------------------------------------------------

#[test]
fn test_max_parts_validation_enforced() {
    // Arrange
    let handler = MultipartHandler::new().with_max_parts(2);

    // Create 3 parts (exceeds max of 2)
    let parts = vec![
        UnifiedContent::Text {
            content: "Part 1".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Part 2".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Part 3".to_string(),
            metadata: None,
        },
    ];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_err());
    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("exceeds maximum"));
    assert!(error_msg.contains("3 parts"));
    assert!(error_msg.contains("maximum 2"));
}

// ---------------------------------------------------------------------------
// Test 11: Max parts boundary condition (exactly at limit)
// ---------------------------------------------------------------------------

#[test]
fn test_max_parts_boundary_condition_at_limit() {
    // Arrange
    let handler = MultipartHandler::new().with_max_parts(3);

    // Create exactly 3 parts (at the limit)
    let parts = vec![
        UnifiedContent::Text {
            content: "Part 1".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Part 2".to_string(),
            metadata: None,
        },
        UnifiedContent::Text {
            content: "Part 3".to_string(),
            metadata: None,
        },
    ];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("Part 1"));
    assert!(output.contains("Part 2"));
    assert!(output.contains("Part 3"));
}

// ---------------------------------------------------------------------------
// Test 12: Mixed content types
// ---------------------------------------------------------------------------

#[test]
fn test_mixed_content_types_all_preserved() {
    // Arrange
    let handler = default_handler();

    let mut data_map = Map::new();
    data_map.insert("key".to_string(), serde_json::json!("value"));

    let parts = vec![
        UnifiedContent::Text {
            content: "Text content".to_string(),
            metadata: None,
        },
        UnifiedContent::Data {
            data: data_map,
            schema: None,
        },
        UnifiedContent::Stream {
            stream_id: "stream-123".to_string(),
            chunk_size: 4096,
            metadata: None,
        },
    ];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Text ==="));
    assert!(output.contains("Text content"));
    assert!(output.contains("=== Part 2: Data ==="));
    assert!(output.contains("\"key\""));
    assert!(output.contains("=== Part 3: Stream ==="));
    assert!(output.contains("stream-123"));
}

// ---------------------------------------------------------------------------
// Test 13: Empty multipart
// ---------------------------------------------------------------------------

#[test]
fn test_empty_multipart_returns_empty_string() {
    // Arrange
    let handler = default_handler();
    let parts: Vec<UnifiedContent> = vec![];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output, "");
}

// ---------------------------------------------------------------------------
// Test 14: Text with multiline content
// ---------------------------------------------------------------------------

#[test]
fn test_text_with_multiline_content_preserved() {
    // Arrange
    let handler = default_handler();
    let multiline_text = "Line 1\nLine 2\nLine 3";
    let parts = vec![UnifiedContent::Text {
        content: multiline_text.to_string(),
        metadata: None,
    }];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("Line 1"));
    assert!(output.contains("Line 2"));
    assert!(output.contains("Line 3"));
}

// ---------------------------------------------------------------------------
// Test 15: Data with complex nested structure
// ---------------------------------------------------------------------------

#[test]
fn test_data_with_complex_nested_structure() {
    // Arrange
    let handler = default_handler();

    let mut complex_data = Map::new();
    complex_data.insert(
        "nested".to_string(),
        serde_json::json!({
            "array": [1, 2, 3],
            "object": {"inner": "value"},
            "null": null
        }),
    );

    let parts = vec![UnifiedContent::Data {
        data: complex_data,
        schema: None,
    }];

    // Act
    let result = handler.process_multipart(&parts);

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("=== Part 1: Data ==="));
    assert!(output.contains("\"nested\""));
    assert!(output.contains("\"array\""));
    assert!(output.contains("\"object\""));
}
