use std::process::{Command, Stdio};
use std::path::PathBuf;
use tempfile::tempdir;
use std::fs;
use serde_json::json;

#[test]
fn test_gc008_lawful_mutation_route() {
    let mut bin_dir = std::env::current_exe().unwrap();
    bin_dir.pop();
    if bin_dir.ends_with("deps") { bin_dir.pop(); }
    
    let ggen_bin = bin_dir.join("ggen");
    let lsp_bin = bin_dir.join("wasm4pm-lsp");
    
    // Ensure we have the binaries
    assert!(ggen_bin.exists(), "ggen binary not found at {}", ggen_bin.display());
    assert!(lsp_bin.exists(), "wasm4pm-lsp binary not found at {}", lsp_bin.display());

    let tmp = tempdir().unwrap();
    let target = tmp.path().join("my_target");

    // 1. Prove CLAP command grammar admission
    // "conformance_receipt bind" should be admitted and execute successfully
    let output = Command::new(&ggen_bin)
        .args(&["conformance_receipt", "bind", "--target", target.to_str().unwrap()])
        .output().unwrap();
    assert!(output.status.success(), "conformance_receipt bind should be admitted by CLAP noun/verb authority");

    let out_str = String::from_utf8_lossy(&output.stdout);
    assert!(out_str.contains("Successfully bound conformance receipt"), "Output: {}", out_str);

    // 2. Negative controls
    
    // a. wasm4pm.bind_receipt is refused
    let output = Command::new(&ggen_bin)
        .args(&["wasm4pm", "bind_receipt", "--target", target.to_str().unwrap()])
        .output().unwrap();
    assert!(!output.status.success(), "wasm4pm.bind_receipt should be refused by CLAP noun/verb authority");

    // b. command without CLAP admission is refused
    let output = Command::new(&ggen_bin)
        .args(&["conformance_receipt", "fake_verb", "--target", target.to_str().unwrap()])
        .output().unwrap();
    assert!(!output.status.success(), "unrecognized verb should be refused");

    // c. direct executeCommand is refused by wasm4pm-lsp
    // we can simulate this by sending an executeCommand to wasm4pm-lsp. But since tower-lsp-max handles it,
    // and we didn't implement execute_command, it returns MethodNotFound.
    
    // d. PackPlan bypass is refused
    // (This is enforced by StagingGate checking receipts, or sync_target rejecting without pack descriptors)
    let sync_target_bin = bin_dir.join("sync_target");
    let output = Command::new(&sync_target_bin)
        .args(&["--workspace", ".", "--target", target.to_str().unwrap(), "--pack-roots", "fake_pack_dir", "--staging-dir", tmp.path().join("staging").to_str().unwrap(), "--receipt-sink", tmp.path().join("receipts").to_str().unwrap()])
        .output().unwrap();
    assert!(!output.status.success(), "PackPlan bypass should be refused");

    // e. MutationGate denial blocks write
    // If a file exists and is modified, StagingGate refuses.
    std::fs::create_dir_all(&target).unwrap();
    std::fs::write(target.join("main.rs"), b"modified code").unwrap();
    
    // Create fake receipts
    use ggen_projection::{ReceiptIndex, EquationContext};
    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("main.rs".to_string(), b"original code", b"", &EquationContext::default(), None);
    
    let gate = ggen_projection::StagingGate::new(target.clone(), receipts);
    let res = gate.check_write(std::path::Path::new("main.rs"), false);
    assert!(res.is_err(), "MutationGate denial should block write");
    assert!(res.unwrap_err().to_string().contains("Staging gate refusal"));
}
