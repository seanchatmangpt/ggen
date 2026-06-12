#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Chicago TDD: Error Code Documentation Completeness Tests
//!
//! This test verifies that all error codes defined in the codebase are documented
//! in AGENT_CONSTRAINTS.md and that the documentation is complete and accurate.
//!
//! Tests use real collaborators: actual source files and documentation, no mocks.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

/// All error codes that are actively emitted from ggen-core
/// Discovered by: grep -rn "error\[E00" crates/ggen-core/src/ --include='*.rs'
const ACTIVE_ERROR_CODES: &[&str] = &[
    "E0001", "E0002", "E0003", "E0004", "E0005", "E0006", "E0007", "E0008", "E0009", "E0010",
    "E0011", "E0012", "E0013", "E0014", "E0020", "E0021", "E0022", "E0023", "E0024", "E0025",
    "E0027", "E0028", "E0029",
];

/// Reserved codes (gaps in the E00XX sequence)
/// These should not appear in error messages, only in documentation
const RESERVED_ERROR_CODES: &[&str] = &["E0015", "E0016", "E0017", "E0018", "E0019", "E0026"];

#[test]
fn test_all_active_error_codes_documented() {
    // Read the documentation file
    // When running from cargo test, we're in the workspace root
    let doc_path = Path::new(".specify/specs/140-agent-diataxis/AGENT_CONSTRAINTS.md");
    let doc_content = fs::read_to_string(doc_path).unwrap_or_else(|_| {
        // Fallback: try relative to this test file
        fs::read_to_string("../../.specify/specs/140-agent-diataxis/AGENT_CONSTRAINTS.md")
            .expect("AGENT_CONSTRAINTS.md not found in expected paths")
    });

    // Extract documented codes from the file (format: ### `E0001`)
    let documented_codes: HashSet<String> = doc_content
        .lines()
        .filter_map(|line| {
            if (line.starts_with("## `E") || line.starts_with("### `E")) && line.ends_with('`') {
                // Extract code from "## `E0001`" or "### `E0001`"
                let start = line.find('`').unwrap_or(0) + 1;
                let end = line.rfind('`').unwrap_or(start);
                Some(line[start..end].to_string())
            } else {
                None
            }
        })
        .collect();

    // Verify all active codes are documented
    let mut missing_codes = Vec::new();
    for code in ACTIVE_ERROR_CODES {
        if !documented_codes.contains(*code) {
            missing_codes.push(*code);
        }
    }

    assert!(
        missing_codes.is_empty(),
        "The following error codes are not documented in AGENT_CONSTRAINTS.md: {:?}\n\
         Please add sections with format: ## `EXXXX`\n\
         Each section must include RULE, REASON, FIX, and CONTEXT fields.",
        missing_codes
    );
}

#[test]
fn test_documented_codes_have_required_sections() {
    // Read the documentation file
    let doc_path = Path::new(".specify/specs/140-agent-diataxis/AGENT_CONSTRAINTS.md");
    let doc_content = fs::read_to_string(doc_path).unwrap_or_else(|_| {
        fs::read_to_string("../../.specify/specs/140-agent-diataxis/AGENT_CONSTRAINTS.md")
            .expect("AGENT_CONSTRAINTS.md not found")
    });

    // For each documented code, verify it has RULE, REASON, FIX sections
    // Look for both "## `E" and "### `E" patterns
    let lines: Vec<&str> = doc_content.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];
        if (line.starts_with("## `E") || line.starts_with("### `E")) && line.ends_with('`') {
            let mut section = String::from(line);
            i += 1;
            // Collect until next heading
            while i < lines.len() && !lines[i].starts_with("## ") && !lines[i].starts_with("### ") {
                section.push('\n');
                section.push_str(lines[i]);
                i += 1;
            }

            let section = section;
            // Extract code from heading
            let first_line = section.lines().next().unwrap_or("");
            let code = if let Some(start) = first_line.find('`') {
                if let Some(end) = first_line.rfind('`') {
                    &first_line[start + 1..end]
                } else {
                    ""
                }
            } else {
                ""
            };

            if code.is_empty() {
                i += 1;
                continue;
            }

            let section_content = section.to_lowercase();
            let has_rule = section_content.contains("**rule:**");
            let has_reason = section_content.contains("**reason:**");
            let has_fix = section_content.contains("**fix:**");

            assert!(
                has_rule && has_reason && has_fix,
                "Error code {} is missing required sections.\n\
                 Required: **RULE:**, **REASON:**, **FIX:**\n\
                 Found: rule={}, reason={}, fix={}",
                code,
                has_rule,
                has_reason,
                has_fix
            );
        } else {
            i += 1;
        }
    }
}

#[test]
fn test_no_reserved_codes_emitted() {
    // Search all source files for reserved error codes in error messages
    // Reserved codes should only appear in documentation, not in code
    let src_path = "crates/ggen-core/src/";
    let src_path_alt = "src/";

    let mut found_reserved = Vec::new();
    for code in RESERVED_ERROR_CODES {
        let pattern = format!("error[{}]", code);

        // Use walkdir to search all .rs files
        use walkdir::WalkDir;

        // Try primary path first
        let search_path = if Path::new(src_path).exists() {
            src_path
        } else {
            src_path_alt
        };

        for entry in WalkDir::new(search_path)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "rs"))
        {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                if content.contains(&pattern) {
                    found_reserved.push((code, entry.path().to_string_lossy().to_string()));
                }
            }
        }
    }

    assert!(
        found_reserved.is_empty(),
        "Reserved error codes found in source code (should only appear in documentation):\n{:?}",
        found_reserved
    );
}

#[test]
fn test_no_orphan_error_codes_in_source() {
    // Extract all error codes actually used in source files
    let src_path = "crates/ggen-core/src/";
    let src_path_alt = "src/";

    let mut source_codes = HashSet::new();
    use walkdir::WalkDir;

    let search_path = if Path::new(src_path).exists() {
        src_path
    } else {
        src_path_alt
    };

    for entry in WalkDir::new(search_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "rs"))
    {
        if let Ok(content) = fs::read_to_string(entry.path()) {
            // Find all patterns like "error[E0001]"
            for line in content.lines() {
                if let Some(pos) = line.find("error[E") {
                    if let Some(end) = line[pos..].find(']') {
                        let code = &line[pos + 6..pos + end];
                        source_codes.insert(code.to_string());
                    }
                }
            }
        }
    }

    // Build expected set: active codes + reserved codes
    let expected_codes: HashSet<String> = ACTIVE_ERROR_CODES
        .iter()
        .chain(RESERVED_ERROR_CODES.iter())
        .map(|s| s.to_string())
        .collect();

    // Find orphans: codes in source but not in expected set
    let orphans: Vec<String> = source_codes
        .iter()
        .filter(|code| !expected_codes.contains(*code))
        .cloned()
        .collect();

    assert!(
        orphans.is_empty(),
        "Found error codes in source that are not in ACTIVE_ERROR_CODES or RESERVED_ERROR_CODES: {:?}\n\
         Either add them to the constant or remove them from the code.",
        orphans
    );
}

#[test]
fn test_error_code_sequence_consistency() {
    // Verify the sequence E0001-E0029 is consistent:
    // - E0001-E0012: Active codes
    // - E0013-E0019: Reserved
    // - E0020-E0029: Active codes (except E0026 which is reserved)

    let mut expected_active = HashSet::new();
    let mut expected_reserved = HashSet::new();

    // E0001-E0014: CODEGEN errors
    for i in 1..=14 {
        expected_active.insert(format!("E{:04}", i));
    }

    // E0015-E0019: Reserved
    for i in 15..=19 {
        expected_reserved.insert(format!("E{:04}", i));
    }

    // E0020-E0025, E0027-E0029: PREFLIGHT errors
    for i in 20..=25 {
        expected_active.insert(format!("E{:04}", i));
    }
    for i in 27..=29 {
        expected_active.insert(format!("E{:04}", i));
    }

    // E0026: Reserved
    expected_reserved.insert("E0026".to_string());

    // Verify active codes match expected
    let actual_active: HashSet<String> = ACTIVE_ERROR_CODES.iter().map(|s| s.to_string()).collect();
    assert_eq!(
        actual_active, expected_active,
        "Active error codes don't match expected sequence"
    );

    // Verify reserved codes match expected
    let actual_reserved: HashSet<String> =
        RESERVED_ERROR_CODES.iter().map(|s| s.to_string()).collect();
    assert_eq!(
        actual_reserved, expected_reserved,
        "Reserved error codes don't match expected sequence"
    );
}

#[test]
fn test_error_codes_from_each_module() {
    // Integration test: verify specific error codes come from expected modules
    // This is a sanity check that the code locations are correct

    let module_code_map = vec![
        ("codegen/merge.rs", "E0001"),
        ("codegen/executor.rs", "E0001"),
        ("manifest/validation.rs", "E0010"),
        ("manifest/validation.rs", "E0011"),
        ("codegen/pipeline.rs", "E0012"),
        ("validation/preflight.rs", "E0020"),
        ("validation/preflight.rs", "E0021"),
    ];

    for (module_path, expected_code) in module_code_map {
        let full_path = format!("crates/ggen-core/src/{}", module_path);
        let fallback_path = format!("src/{}", module_path);

        let content = fs::read_to_string(&full_path)
            .or_else(|_| fs::read_to_string(&fallback_path))
            .unwrap_or_else(|_| format!("File not found: {} or {}", full_path, fallback_path));

        let pattern = format!("error[{}]", expected_code);
        assert!(
            content.contains(&pattern),
            "Expected error code {} not found in {}",
            expected_code,
            module_path
        );
    }
}
