use std::path::PathBuf;
use std::fs;

#[test]
fn test_gc004_pack_domain_lsp_intelligence() {
    let workspace = "/Users/sac/ggen";
    
    // Test 1: Verify the pack LSPs exist and are not monolithic
    let clap_src = PathBuf::from(workspace).join("crates/ggen-pack-clap-noun-verb/src/main.rs");
    let tower_src = PathBuf::from(workspace).join("crates/ggen-pack-tower-lsp-max/src/main.rs");
    
    assert!(clap_src.exists(), "clap-noun-verb-pack-lsp source missing");
    assert!(tower_src.exists(), "tower-lsp-max-pack-lsp source missing");
    
    // Test 2: Prove no-observer-write law (static scan)
    // The language servers must only emit diagnostics and never write to disk directly
    let clap_content = fs::read_to_string(&clap_src).unwrap();
    let tower_content = fs::read_to_string(&tower_src).unwrap();
    
    assert!(!clap_content.contains("std::fs::write"), "clap LSP contains forbidden write path");
    assert!(!clap_content.contains("tokio::fs::write"), "clap LSP contains forbidden write path");
    assert!(!clap_content.contains("fs::write"), "clap LSP contains forbidden write path");
    
    assert!(!tower_content.contains("std::fs::write"), "tower LSP contains forbidden write path");
    assert!(!tower_content.contains("tokio::fs::write"), "tower LSP contains forbidden write path");
    assert!(!tower_content.contains("fs::write"), "tower LSP contains forbidden write path");
    
    // Test 3: Prove ggen-lsp was stripped of pack heuristics
    let ggen_diag_src = PathBuf::from(workspace).join("crates/ggen-lsp/src/handlers/diagnostics.rs");
    let ggen_content = fs::read_to_string(&ggen_diag_src).unwrap();
    
    assert!(!ggen_content.contains("CLAP-CUSTOMIZE-001"), "ggen-lsp must not own CLAP domain law");
    assert!(!ggen_content.contains("CLAP-PROJECT-OPPORTUNITY-001"), "ggen-lsp must not own CLAP domain law");
    assert!(!ggen_content.contains("TOWER-PROJECT-OPPORTUNITY-001"), "ggen-lsp must not own TOWER domain law");
    
    // Ensure it still owns projection state
    assert!(ggen_content.contains("GGEN-PROJECTED"), "ggen-lsp must own projection state");
    assert!(ggen_content.contains("GGEN-DRIFT"), "ggen-lsp must own drift state");
}