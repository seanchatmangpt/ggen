//! Honesty test for the cpmp scanner.
//!
//! After the v26.5.28 closure fix, a scan emits exactly one real output
//! artifact (the JSON scan receipt) and runs no external subprocesses. It must
//! NOT fabricate stub catalog/SHACL TTL files, and must NOT list any `.ttl`
//! artifact that nothing downstream reads.

// Tests use expect() for clear failure messages; panics are intentional in test context.
#![allow(clippy::expect_used)]

use std::path::PathBuf;

use cpmp::models::Receipt;
use cpmp::scanner::scan;

#[test]
fn scan_emits_only_real_receipt_and_no_stub_ttl_or_subprocess() {
    // Arrange: a real temp tree with one .rs file.
    let tmp = std::env::temp_dir().join(format!("cpmp_honesty_{}", uuid::Uuid::new_v4()));
    let src_dir = tmp.join("src");
    std::fs::create_dir_all(&src_dir).expect("create src dir");
    std::fs::write(
        src_dir.join("lib.rs"),
        "pub fn hello() -> &'static str { \"hi\" }\n",
    )
    .expect("write source file");

    let out = tmp.join("out");
    let roots = vec![src_dir.clone()];

    // Act: real scan over the real filesystem.
    scan(&roots, &out).expect("scan succeeds");

    // Assert: the real receipt exists.
    let receipt_path = out.join("scan-receipt.json");
    assert!(
        receipt_path.exists(),
        "scan-receipt.json must exist at {}",
        receipt_path.display()
    );

    // Assert: no stub catalog/SHACL files were fabricated.
    let catalog_ttl = out.join("catalog").join("cpmp-catalog.ttl");
    let shapes_ttl = out.join("catalog").join("cpmp-shapes.ttl");
    assert!(
        !catalog_ttl.exists(),
        "stub catalog TTL must NOT exist: {}",
        catalog_ttl.display()
    );
    assert!(
        !shapes_ttl.exists(),
        "stub shapes TTL must NOT exist: {}",
        shapes_ttl.display()
    );

    // Parse the receipt and assert its claims are honest.
    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt json");
    let receipt: Receipt = serde_json::from_str(&raw).expect("parse receipt json");

    // Every listed output artifact must actually exist, and none may be a .ttl.
    assert!(
        !receipt.output_artifacts.is_empty(),
        "receipt must list at least the real receipt artifact"
    );
    for artifact in &receipt.output_artifacts {
        let p = PathBuf::from(artifact);
        assert!(
            p.exists(),
            "claimed output artifact does not exist: {}",
            artifact
        );
        assert!(
            !artifact.ends_with(".ttl"),
            "receipt must not claim a stub .ttl artifact: {}",
            artifact
        );
    }

    // No external subprocess was run.
    assert!(
        receipt.commands_run.is_empty(),
        "commands_run must be empty (no external subprocess), got: {:?}",
        receipt.commands_run
    );

    // The deterministic scan identity is present.
    assert!(
        !receipt.aggregate_hash.is_empty(),
        "aggregate_hash must be non-empty for a non-empty scan"
    );

    // Cleanup.
    let _ = std::fs::remove_dir_all(&tmp);
}
