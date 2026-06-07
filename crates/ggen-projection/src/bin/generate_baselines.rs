use std::fs;
use std::path::Path;
use std::process::Command;

fn generate_for_repo(repo_path: &Path, repo_name: &str) {
    let output = Command::new("git")
        .args(&["-C", repo_path.to_str().unwrap(), "status", "--porcelain", "--ignored"])
        .output()
        .expect("failed to execute git status");
    
    let stdout = String::from_utf8(output.stdout).expect("invalid utf8");
    
    let mut tracked_entries = Vec::new();
    let mut ignored_paths = Vec::new();
    
    for line in stdout.lines() {
        if line.is_empty() { continue; }
        if line.starts_with("!! ") {
            let path = &line[3..];
            ignored_paths.push(path.to_string());
        } else {
            // It is a tracked/untracked change
            let status = &line[0..2];
            let path = &line[3..];
            tracked_entries.push((path.to_string(), status.trim().to_string()));
        }
    }
    
    let mut content = String::new();
    content.push_str("# Sealed Workspace Baseline Manifest\n");
    content.push_str("# Generated dynamically by ggen-projection baseline tool\n\n");
    content.push_str("[metadata]\n");
    content.push_str(&format!("repo = \"{}\"\n\n", repo_name));
    
    for (path, status) in tracked_entries {
        content.push_str("[[tracked]]\n");
        content.push_str(&format!("path = \"{}\"\n", path));
        content.push_str(&format!("status = \"{}\"\n\n", status));
    }
    
    content.push_str("[ignored]\n");
    content.push_str("paths = [\n");
    for path in ignored_paths {
        content.push_str(&format!("  \"{}\",\n", path));
    }
    content.push_str("]\n\n");
    
    // Compute BLAKE3 hash
    let hash = blake3::hash(content.as_bytes());
    content.push_str(&format!("digest = \"{}\"\n", hash.to_hex()));
    
    let baseline_path = repo_path.join(".gc-sealed-baseline");
    fs::write(&baseline_path, content).expect("failed to write baseline manifest");
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
    generate_for_repo(&parent_dir.join("wasm4pm"), "wasm4pm");
    generate_for_repo(&parent_dir.join("wasm4pm-compat"), "wasm4pm-compat");
}
