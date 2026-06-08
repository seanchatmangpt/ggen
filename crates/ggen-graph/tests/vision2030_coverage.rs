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
//! Integration test to validate the vision2030.coverage.json file schema and structure.

use ggen_graph::ocel::{generate_coverage_matrix, CoverageMatrix};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

#[test]
fn test_coverage_matrix_schema_and_contents() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Check generated coverage matrix directly
    let generated = generate_coverage_matrix();
    assert_eq!(
        generated.requirements.len(),
        10,
        "Should contain exactly 10 requirements"
    );

    for req in &generated.requirements {
        assert!(!req.id.is_empty(), "Requirement ID should not be empty");
        assert!(
            !req.title.is_empty(),
            "Requirement Title should not be empty"
        );
        assert!(
            !req.description.is_empty(),
            "Requirement Description should not be empty"
        );
        assert!(
            !req.source_files.is_empty(),
            "Requirement {} source files should not be empty",
            req.id
        );
        assert!(
            !req.test_files.is_empty(),
            "Requirement {} test files should not be empty",
            req.id
        );
        assert!(
            !req.commands.is_empty(),
            "Requirement {} commands should not be empty",
            req.id
        );
    }

    // 2. Load the serialized file and check its integrity
    let coverage_path = if Path::new("crates/ggen-graph/audit/vision2030.coverage.json").exists() {
        Path::new("crates/ggen-graph/audit/vision2030.coverage.json").to_path_buf()
    } else {
        Path::new("audit/vision2030.coverage.json").to_path_buf()
    };
    assert!(
        coverage_path.exists(),
        "vision2030.coverage.json file must exist at {:?}. Run emit script first.",
        coverage_path
    );

    let file = File::open(&coverage_path)?;
    let loaded: CoverageMatrix = serde_json::from_reader(BufReader::new(file))?;

    assert_eq!(
        loaded.requirements.len(),
        10,
        "Loaded coverage matrix should have 10 requirements"
    );
    assert_eq!(
        loaded, generated,
        "Loaded coverage matrix does not match the generated default matrix"
    );

    Ok(())
}
