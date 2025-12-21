//! Merge mode tests (T023) - Chicago School TDD
//!
//! Tests the marker-based merging functionality that preserves manual code
//! while injecting generated code.
//!
//! ## Coverage
//! - Marker preservation in merge operations
//! - Generated section injection
//! - Manual section preservation
//! - Malformed marker handling
//! - End-to-end merge integration

use ggen_core::codegen::merge::{merge_sections, parse_merge_markers};

// ============================================================================
// T023.1: test_marker_preserved_in_merge
// ============================================================================

#[test]
fn test_marker_preserved_in_merge() {
    // Arrange: Existing file with merge markers
    let existing = r#"// Header comment
<<<<<<< GENERATED
fn old_fn() { println!("old"); }
=======
fn my_manual_code() { println!("manual"); }
>>>>>>> MANUAL
// Footer comment
"#;

    let new_generated = r#"fn new_fn() { println!("new"); }"#;

    // Act: Merge new generated code
    let result = merge_sections(new_generated, existing).expect("Merge should succeed");

    // Assert: Markers are preserved
    assert!(
        result.contains("<<<<<<< GENERATED"),
        "Generated marker should be preserved"
    );
    assert!(result.contains("======="), "Separator marker should be preserved");
    assert!(
        result.contains(">>>>>>> MANUAL"),
        "Manual marker should be preserved"
    );

    // Assert: Structure is correct (header, markers, footer)
    assert!(
        result.starts_with("// Header comment\n"),
        "Header comment should be preserved"
    );
    assert!(
        result.contains("// Footer comment\n"),
        "Footer comment should be preserved"
    );
}

// ============================================================================
// T023.2: test_generated_section_injected
// ============================================================================

#[test]
fn test_generated_section_injected() {
    // Arrange: Existing file with old generated code
    let existing = r#"<<<<<<< GENERATED
fn old_generated() { }
=======
fn manual() { }
>>>>>>> MANUAL
"#;

    let new_generated = "fn new_generated() { }";

    // Act: Merge new code
    let result = merge_sections(new_generated, existing).expect("Merge should succeed");

    // Assert: New generated code is present
    assert!(
        result.contains("fn new_generated() { }"),
        "New generated code should be injected"
    );

    // Assert: Old generated code is gone
    assert!(
        !result.contains("fn old_generated() { }"),
        "Old generated code should be replaced"
    );

    // Assert: Generated section is in correct position
    let gen_start = result.find("<<<<<<< GENERATED").expect("Should have start marker");
    let sep = result.find("=======").expect("Should have separator");
    let gen_section = &result[gen_start..sep];
    assert!(
        gen_section.contains("fn new_generated() { }"),
        "Generated code should be between markers"
    );
}

// ============================================================================
// T023.3: test_manual_section_preserved
// ============================================================================

#[test]
fn test_manual_section_preserved() {
    // Arrange: Existing file with manual code
    let manual_code = r#"fn manual_impl() {
    // User's custom implementation
    let x = 42;
    println!("manual code: {}", x);
}"#;

    let existing = format!(
        r#"<<<<<<< GENERATED
fn generated_old() {{ }}
=======
{}
>>>>>>> MANUAL
"#,
        manual_code
    );

    let new_generated = "fn generated_new() { }";

    // Act: Merge new generated code
    let result = merge_sections(new_generated, &existing).expect("Merge should succeed");

    // Assert: Manual code is completely preserved
    assert!(
        result.contains(manual_code),
        "Manual code should be preserved exactly"
    );

    // Assert: Manual code is in correct section (between ======= and >>>>>>>)
    let sep = result.find("=======").expect("Should have separator");
    let end = result.find(">>>>>>> MANUAL").expect("Should have end marker");
    let manual_section = &result[sep..end];
    assert!(
        manual_section.contains("fn manual_impl()"),
        "Manual code should be in manual section"
    );
    assert!(
        manual_section.contains("let x = 42;"),
        "Manual implementation details should be preserved"
    );
}

// ============================================================================
// T023.4: test_malformed_markers_handled
// ============================================================================

#[test]
fn test_malformed_markers_handled() {
    // Arrange: Malformed markers (separator before start marker)
    let malformed1 = r#"=======
fn code() { }
<<<<<<< GENERATED
>>>>>>> MANUAL
"#;

    let new_generated = "fn new() { }";

    // Act: Attempt merge with malformed markers
    let result1 = merge_sections(new_generated, malformed1);

    // Assert: Should return error for invalid marker order
    assert!(
        result1.is_err(),
        "Should error on separator before generated marker"
    );
    let err_msg = result1.unwrap_err().to_string();
    assert!(
        err_msg.contains("must come after"),
        "Error should indicate marker ordering issue"
    );

    // Arrange: Missing manual end marker
    let malformed2 = r#"<<<<<<< GENERATED
fn generated() { }
=======
fn manual() { }
"#;

    // Act: Parse markers (incomplete set)
    let markers = parse_merge_markers(malformed2);

    // Assert: Should return None for incomplete markers
    assert!(
        markers.is_none(),
        "Should return None when markers are incomplete"
    );
}

// ============================================================================
// T023.5: test_merge_mode_integration
// ============================================================================

#[test]
fn test_merge_mode_integration() {
    // Arrange: Simulate a full merge lifecycle
    let initial_content = "";

    // Act 1: First generation (no markers)
    let gen1 = "fn generated_v1() { }";
    let after_gen1 = merge_sections(gen1, initial_content).expect("First merge should succeed");

    // Assert 1: First generation creates markers
    assert!(
        after_gen1.contains("<<<<<<< GENERATED"),
        "First merge should create markers"
    );
    assert!(
        after_gen1.contains("======="),
        "First merge should create separator"
    );
    assert!(
        after_gen1.contains(">>>>>>> MANUAL"),
        "First merge should create end marker"
    );

    // Arrange 2: User adds manual code
    let user_edited = after_gen1.replace(
        "// Add your manual code here",
        "fn user_function() { println!(\"user code\"); }",
    );

    // Act 2: Second generation with user edits
    let gen2 = "fn generated_v2() { }";
    let after_gen2 = merge_sections(gen2, &user_edited).expect("Second merge should succeed");

    // Assert 2: Generated code updated
    assert!(
        after_gen2.contains("fn generated_v2() { }"),
        "Generated code should be updated to v2"
    );
    assert!(
        !after_gen2.contains("fn generated_v1() { }"),
        "Old generated code should be removed"
    );

    // Assert 3: User code preserved
    assert!(
        after_gen2.contains("fn user_function()"),
        "User code should be preserved across regeneration"
    );
    assert!(
        after_gen2.contains("println!(\"user code\")"),
        "User implementation details should be preserved"
    );

    // Arrange 3: Multiple regenerations
    let gen3 = "fn generated_v3() { }";
    let after_gen3 = merge_sections(gen3, &after_gen2).expect("Third merge should succeed");

    // Assert 4: Still preserves user code after multiple regenerations
    assert!(
        after_gen3.contains("fn user_function()"),
        "User code should survive multiple regenerations"
    );
    assert!(
        after_gen3.contains("fn generated_v3() { }"),
        "Latest generated code should be present"
    );
}

// ============================================================================
// Helper Tests - Verify parse_merge_markers robustness
// ============================================================================

#[test]
fn test_parse_merge_markers_with_whitespace() {
    // Arrange: Markers with varying whitespace
    let content = r#"
    <<<<<<< GENERATED
fn gen() { }
    =======
fn manual() { }
    >>>>>>> MANUAL
"#;

    // Act: Parse markers
    let markers = parse_merge_markers(content).expect("Should parse markers with whitespace");

    // Assert: Correct line positions
    assert_eq!(markers.generated_start, 1, "Should find generated marker at line 1");
    assert_eq!(markers.manual_start, 3, "Should find separator at line 3");
    assert_eq!(markers.manual_end, 5, "Should find manual marker at line 5");
}

#[test]
fn test_merge_sections_preserves_file_structure() {
    // Arrange: File with content before and after merge block
    let existing = r#"// File header
use std::collections::HashMap;

<<<<<<< GENERATED
fn old_gen() { }
=======
fn manual() { }
>>>>>>> MANUAL

// File footer
fn other_function() { }
"#;

    let new_generated = "fn new_gen() { }";

    // Act: Merge
    let result = merge_sections(new_generated, existing).expect("Merge should succeed");

    // Assert: File structure preserved
    assert!(
        result.contains("// File header"),
        "Header before merge block should be preserved"
    );
    assert!(
        result.contains("use std::collections::HashMap;"),
        "Imports before merge block should be preserved"
    );
    assert!(
        result.contains("// File footer"),
        "Footer after merge block should be preserved"
    );
    assert!(
        result.contains("fn other_function()"),
        "Code after merge block should be preserved"
    );

    // Assert: Generated code updated in correct location
    assert!(
        result.contains("fn new_gen() { }"),
        "New generated code should be present"
    );
    assert!(
        !result.contains("fn old_gen() { }"),
        "Old generated code should be replaced"
    );
}
