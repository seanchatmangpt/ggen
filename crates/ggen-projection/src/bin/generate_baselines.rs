use std::fs;
use std::path::Path;
use std::process::Command;
use std::collections::BTreeMap;
use serde::{Serialize, Deserialize};
use sha2::{Sha256, Digest};

#[derive(Serialize, Deserialize, Clone)]
struct BaselineManifest {
    allowed_ignored_directories: Vec<String>,
    forbidden_generated_paths: Vec<String>,
    ignored_inventory: Vec<String>,
    tracked_status: BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    digest: Option<String>,
    baseline_manifest_digest_algorithm: String,
    gall_receipt_digest_algorithm: String,
}

fn generate_for_repo(
    repo_path: &Path,
    repo_name: &str,
    allowed_ignored_directories: Vec<String>,
    forbidden_generated_paths: Vec<String>,
    ignored_inventory: Vec<String>,
) {
    let output = Command::new("git")
        .args(&["-C", repo_path.to_str().unwrap(), "status", "--porcelain"])
        .output()
        .expect("failed to execute git status");
    
    let stdout = String::from_utf8(output.stdout).expect("invalid utf8");
    
    let mut tracked_status = BTreeMap::new();
    
    for line in stdout.lines() {
        if line.is_empty() { continue; }
        let status = line[0..2].trim().to_string();
        let path = line[3..].trim().to_string();
        // Skip files that are untracked (status "??" or "!!")
        if status != "??" && status != "!!" && !status.is_empty() {
            tracked_status.insert(path, status);
        }
    }
    
    let mut manifest = BaselineManifest {
        allowed_ignored_directories,
        forbidden_generated_paths,
        ignored_inventory,
        tracked_status,
        digest: None,
        baseline_manifest_digest_algorithm: "sha256".to_string(),
        gall_receipt_digest_algorithm: "blake3".to_string(),
    };
    
    let serialized = serde_json::to_string(&manifest).expect("failed to serialize");
    
    let mut hasher = Sha256::new();
    hasher.update(serialized.as_bytes());
    let digest_hex = format!("{:x}", hasher.finalize());
    
    manifest.digest = Some(digest_hex);
    
    let final_json = serde_json::to_string_pretty(&manifest).expect("failed to pretty serialize");
    let baseline_path = repo_path.join(".gc-sealed-baseline");
    fs::write(&baseline_path, final_json).expect("failed to write baseline manifest");
    println!("Generated baseline manifest at {}", baseline_path.display());
}

fn main() {
    let mut ggen_root = std::env::current_dir().unwrap();
    while !ggen_root.join("ggen.toml").exists() {
        if !ggen_root.pop() {
            panic!("Could not find ggen workspace root");
        }
    }
    let parent_dir = ggen_root.parent().unwrap();
    
    let wasm4pm_ignored_dirs = vec![
        ".claude".to_string(),
        ".wasm4pm".to_string(),
        "apps".to_string(),
        "artifacts".to_string(),
        "crates".to_string(),
        "dist".to_string(),
        "docs_quarantine".to_string(),
        "lab".to_string(),
        "node_modules".to_string(),
        "packages".to_string(),
        "playground".to_string(),
        "results".to_string(),
        "scratch".to_string(),
        "target".to_string(),
        "tests/proof/node_modules".to_string(),
        "vendors".to_string(),
        "wasm4pm".to_string(),
    ];
    
    let wasm4pm_forbidden = vec![
        "gc005".to_string(),
        "gc006".to_string(),
    ];
    
    let wasm4pm_inventory = vec![
        ".DS_Store".to_string(),
        ".gc-sealed-baseline".to_string(),
        "PHD_THESIS.log".to_string(),
        "THESIS_DEFENSE_REPORT.log".to_string(),
        "WASM4PM_FOUNDATIONS_SEAN_CHATMAN.log".to_string(),
        "WASM4PM_FOUNDATIONS_VD_AALST.log".to_string(),
        "cv-test-results.log".to_string(),
        "isolate-0xbb9c00000-26140-v8.log".to_string(),
        "performance-audit.log".to_string(),
        "scripts/bench-algorithms.js".to_string(),
        "test-out.log".to_string(),
        "test-output.log".to_string(),
    ];
    
    generate_for_repo(
        &parent_dir.join("wasm4pm"),
        "wasm4pm",
        wasm4pm_ignored_dirs,
        wasm4pm_forbidden,
        wasm4pm_inventory,
    );
    
    let compat_ignored_dirs = vec![
        ".agents".to_string(),
        ".claude".to_string(),
        "docs".to_string(),
        "ggen".to_string(),
        "target".to_string(),
        "target-lsp".to_string(),
        "target_lsp".to_string(),
        "wasm4pm-compat-lsp".to_string(),
        "wip".to_string(),
    ];
    
    let compat_forbidden = vec![
        "gc005".to_string(),
        "gc006".to_string(),
    ];
    
    let compat_inventory = vec![
        ".DS_Store".to_string(),
        ".gc-sealed-baseline".to_string(),
        "conformance_verdict_is_perfect".to_string(),
        "libmod.rlib".to_string(),
        "librust_out.rlib".to_string(),
    ];
    
    generate_for_repo(
        &parent_dir.join("wasm4pm-compat"),
        "wasm4pm-compat",
        compat_ignored_dirs,
        compat_forbidden,
        compat_inventory,
    );
}
