use chicago_tdd_tools::prelude::*;
// Re-pointed from `ggen_core::manifest::{...}` to `ggen_config::manifest::{...}`
// (T052, specs/014-ggen-core-replacement) -- the manifest module's real new
// home (T019-T023). NOTE: `ManifestInputs`/`compute_manifest_key` do not
// exist in `ggen_config::manifest` (confirmed: `grep -rn "ManifestInputs|
// compute_manifest_key" crates/ src/ tests/` matches only this file) -- nor
// did they exist anywhere in `ggen_core` before this migration (same grep
// against `ggen-core/src/` also empty). This file is not part of any
// compiled `cargo test` target today (not `tests/*.rs`-auto-discovered, no
// `[[test]]` entry, not `mod`-included from any entry point that is --
// verified via `cargo metadata`'s test-target list), so it was already
// dead/pre-existing rot, not a regression introduced by re-pointing its
// import path. Left as a flagged gap rather than fabricated, per the
// project's evidence-first rule.
use ggen_config::manifest::{ManifestInputs, compute_manifest_key};
use std::collections::BTreeMap;

test!(test_identical_inputs_yield_identical_hash, {
    // Arrange
    let inputs1 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    let inputs2 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    // Act
    let hash1 = compute_manifest_key(&inputs1);
    let hash2 = compute_manifest_key(&inputs2);
    
    // Assert
    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64);
});

test!(test_different_inputs_yield_different_hash, {
    // Arrange
    let inputs1 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    let inputs2 = ManifestInputs {
        canonical_graph: "different graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    // Act
    let hash1 = compute_manifest_key(&inputs1);
    let hash2 = compute_manifest_key(&inputs2);
    
    // Assert
    assert_ne!(hash1, hash2);
});
