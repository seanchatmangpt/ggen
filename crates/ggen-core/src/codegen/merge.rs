//! Merge mode - Intelligent code merging with marker-based boundaries
//!
//! This module implements marker-based merging that preserves manual code while
//! injecting generated code. It supports the `mode = "Merge"` generation rule setting.
//!
//! ## Marker Format
//!
//! ```text
//! <<<<<<< GENERATED
//! // Generated code goes here
//! =======
//! // Manual code is preserved here
//! >>>>>>> MANUAL
//! ```
//!
//! ## Algorithm
//!
//! 1. Detect merge markers in existing file
//! 2. Extract manual code sections (between ======= and >>>>>>>)
//! 3. Inject generated code sections (between <<<<<<< and =======)
//! 4. Preserve manual sections unchanged
//! 5. Write merged result

use ggen_utils::error::{Error, Result};

/// Merge marker detection result
#[derive(Debug, Clone)]
pub struct MergeMarkers {
    /// Start of generated section (line number)
    pub generated_start: usize,
    /// Start of manual section (line number)
    pub manual_start: usize,
    /// End of merge block (line number)
    pub manual_end: usize,
}

/// Merged code sections
#[derive(Debug, Clone)]
pub struct MergedSections {
    /// Generated code section
    pub generated: String,
    /// Manual code section (preserved)
    pub manual: String,
}

/// Parse merge markers from existing file content
///
/// Returns `None` if no merge markers found (first-time generation).
/// Returns `Some(MergeMarkers)` with line positions if markers detected.
pub fn parse_merge_markers(content: &str) -> Option<MergeMarkers> {
    let lines: Vec<&str> = content.lines().collect();

    let mut generated_start = None;
    let mut manual_start = None;
    let mut manual_end = None;

    for (idx, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("<<<<<<< GENERATED") {
            generated_start = Some(idx);
        } else if trimmed == "=======" {
            manual_start = Some(idx);
        } else if trimmed.starts_with(">>>>>>> MANUAL") {
            manual_end = Some(idx);
        }
    }

    match (generated_start, manual_start, manual_end) {
        (Some(gs), Some(ms), Some(me)) => Some(MergeMarkers {
            generated_start: gs,
            manual_start: ms,
            manual_end: me,
        }),
        _ => None,
    }
}

/// Merge generated code with existing manual sections
///
/// ## Arguments
///
/// * `generated_code` - The newly generated code content
/// * `existing_content` - The existing file content with merge markers
///
/// ## Returns
///
/// Merged content with generated section updated and manual section preserved.
pub fn merge_sections(generated_code: &str, existing_content: &str) -> Result<String> {
    let markers = match parse_merge_markers(existing_content) {
        None => {
            // First-time generation - wrap in markers
            return Ok(format!(
                "<<<<<<< GENERATED\n{}\n=======\n// Add your manual code here\n>>>>>>> MANUAL\n",
                generated_code
            ));
        }
        Some(m) => m,
    };

    let lines: Vec<&str> = existing_content.lines().collect();

    // Validate marker positions
    if markers.manual_start <= markers.generated_start {
        return Err(Error::new(
            "Invalid merge markers: ======= must come after <<<<<<< GENERATED",
        ));
    }
    if markers.manual_end <= markers.manual_start {
        return Err(Error::new(
            "Invalid merge markers: >>>>>>> MANUAL must come after =======",
        ));
    }

    // Extract manual section
    let manual_section: String = lines[(markers.manual_start + 1)..markers.manual_end].join("\n");

    // Build merged content
    let mut merged = String::new();

    // Before merge block
    for line in &lines[..markers.generated_start] {
        merged.push_str(line);
        merged.push('\n');
    }

    // Merge block
    merged.push_str("<<<<<<< GENERATED\n");
    merged.push_str(generated_code);
    merged.push_str("\n=======\n");
    merged.push_str(&manual_section);
    merged.push_str("\n>>>>>>> MANUAL\n");

    // After merge block
    for line in &lines[(markers.manual_end + 1)..] {
        merged.push_str(line);
        merged.push('\n');
    }

    Ok(merged)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_merge_markers_found() {
        let content = r#"
<<<<<<< GENERATED
fn generated() {}
=======
fn manual() {}
>>>>>>> MANUAL
        "#;

        let markers = parse_merge_markers(content).unwrap();
        assert_eq!(markers.generated_start, 1);
        assert_eq!(markers.manual_start, 3);
        assert_eq!(markers.manual_end, 5);
    }

    #[test]
    fn test_parse_merge_markers_not_found() {
        let content = "fn regular_code() {}";
        assert!(parse_merge_markers(content).is_none());
    }

    #[test]
    fn test_merge_sections_first_time() {
        let generated = "fn new_fn() {}";
        let result = merge_sections(generated, "").unwrap();

        assert!(result.contains("<<<<<<< GENERATED"));
        assert!(result.contains("fn new_fn() {}"));
        assert!(result.contains("======="));
        assert!(result.contains(">>>>>>> MANUAL"));
    }

    #[test]
    fn test_merge_sections_preserves_manual() {
        let existing = r#"
<<<<<<< GENERATED
fn old_generated() {}
=======
fn manual_code() {}
>>>>>>> MANUAL
"#;

        let new_generated = "fn new_generated() {}";
        let result = merge_sections(new_generated, existing).unwrap();

        assert!(result.contains("fn new_generated() {}"));
        assert!(result.contains("fn manual_code() {}"));
        assert!(!result.contains("fn old_generated() {}"));
    }
}
