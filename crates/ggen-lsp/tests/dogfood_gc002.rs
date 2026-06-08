use std::process::Command;
use std::path::{Path, PathBuf};
use std::fs;
use tempfile::tempdir;

#[test]
fn test_gc002_gall_checkpoint_004_dogfood_proof() {
    let tmp = tempdir().unwrap();
    let root = tmp.path();
    let target_dir = root.join(".tmp_gc004/target");
    let staging_dir = root.join(".tmp_gc004/staging");
    let receipt_sink = root.join(".tmp_gc004/receipts");
    
    let cargo_bin = std::env::current_exe()
        .ok()
        .and_then(|path| path.parent().map(|p| p.join("sync_target")))
        .filter(|p| p.exists())
        .unwrap_or_else(|| {
            let path = PathBuf::from("../../target/debug/sync_target");
            if path.exists() {
                path
            } else {
                PathBuf::from("/Users/sac/ggen/target/debug/sync_target")
            }
        });
    let workspace = "/Users/sac/ggen";
    let cnv_pack = "/Users/sac/ggen/crates/ggen-pack-clap-noun-verb";
    let lsp_pack = "/Users/sac/ggen/crates/ggen-pack-tower-lsp-max";

    let run_projection = || -> std::process::Output {
        Command::new(&cargo_bin)
            .arg("--workspace").arg(workspace)
            .arg("--target").arg(&target_dir)
            .arg("--staging-dir").arg(&staging_dir)
            .arg("--receipt-sink").arg(&receipt_sink)
            .arg("--pack-roots").arg(cnv_pack).arg(lsp_pack)
            .output()
            .expect("Failed to run sync_target")
    };

    // 1. Delete and replay from templates
    let output = run_projection();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "Projection should succeed. stderr: {}", stderr);
    
    assert!(target_dir.join("src/main.rs").exists(), "src/main.rs output must exist");

    // 2. Receipts include template_digest
    let receipts_jsonl = fs::read_to_string(receipt_sink.join("receipts.jsonl")).unwrap();
    assert!(receipts_jsonl.contains("\"template_digest\""));

    // 3. Template mutation changes artifact blake3 and template_digest
    let mutated_pack_dir = root.join("mutated_lsp_pack");
    Command::new("cp").arg("-r").arg(lsp_pack).arg(&mutated_pack_dir).status().unwrap();
    
    let run_mutated_projection = |pack2: &Path, target: &Path, staging: &Path, receipts: &Path| -> std::process::Output {
        Command::new(&cargo_bin)
            .arg("--workspace").arg(workspace)
            .arg("--target").arg(target)
            .arg("--staging-dir").arg(staging)
            .arg("--receipt-sink").arg(receipts)
            .arg("--pack-roots").arg(cnv_pack).arg(pack2)
            .output()
            .expect("Failed to run sync_target")
    };

    let tmpl_path = mutated_pack_dir.join("templates/src/server.rs.tmpl");
    if !tmpl_path.exists() {
        fs::write(&tmpl_path, "modified template content").unwrap();
    } else {
        let mut content = fs::read_to_string(&tmpl_path).unwrap();
        content.push_str("\n// modified");
        fs::write(&tmpl_path, content).unwrap();
    }

    let mutated_target = root.join("mutated_target");
    let mutated_receipts_dir = root.join("mutated_receipts");
    let output_mutated = run_mutated_projection(&mutated_pack_dir, &mutated_target, &root.join("mutated_staging"), &mutated_receipts_dir);
    assert!(output_mutated.status.success());
    let mutated_receipts = fs::read_to_string(mutated_receipts_dir.join("receipts.jsonl")).unwrap();
    
    assert!(mutated_receipts.contains("\"template_digest\""));
    assert!(mutated_receipts != receipts_jsonl, "Receipts should change after template mutation");

    // 4. Required template missing refuses before write
    fs::remove_file(&tmpl_path).unwrap();
    let missing_target = root.join("mutated_target_missing");
    let output_missing = run_mutated_projection(&mutated_pack_dir, &missing_target, &root.join("mutated_staging_missing"), &root.join("mutated_receipts_missing"));
    assert!(!output_missing.status.success(), "Projection should fail when template is missing");
    let stderr_missing = String::from_utf8_lossy(&output_missing.stderr);
    assert!(stderr_missing.contains("Required template missing"));
    assert!(!missing_target.exists(), "Target dir should not be created if projection fails");

    // 5. No reference-copy path remains
    let sync_target_src = fs::read_to_string("/Users/sac/ggen/crates/ggen-projection/src/bin/sync_target.rs").unwrap();
    assert!(!sync_target_src.contains("src_files"), "src_files symbol is forbidden");
    assert!(!sync_target_src.contains("reference implementation"), "reference implementation symbol is forbidden");
}
