use std::fs;

#[test]
fn test_gc006_authority_surface_lock() {
    let mut ggen_root = std::env::current_dir().unwrap();
    while !ggen_root.join("ggen.toml").exists() {
        if !ggen_root.pop() {
            panic!("Could not find ggen workspace root");
        }
    }
    let parent_dir = ggen_root.parent().unwrap().to_path_buf();
    let tower_lsp_max = parent_dir.join("tower-lsp-max");

    
    // no-shadow-crates
    {
        
            // Ensure no forbidden shadow crates exist in the workspace.
            let forbidden = vec![
                ggen_root.join("crates/wasm4pm"),
                ggen_root.join("crates/wasm4pm-proper"),
                ggen_root.join("crates/wasm4pm-compat"),
            ];
            for path in forbidden {
                assert!(!path.exists(), "Forbidden shadow crate found: {}", path.display());
            }

            #[derive(serde::Deserialize, serde::Serialize, Clone)]
            struct BaselineManifest {
                allowed_ignored_directories: Vec<String>,
                forbidden_generated_paths: Vec<String>,
                ignored_inventory: Vec<String>,
                tracked_status: std::collections::BTreeMap<String, String>,
                #[serde(skip_serializing_if = "Option::is_none")]
                digest: Option<String>,
                baseline_manifest_digest_algorithm: String,
                gall_receipt_digest_algorithm: String,
            }

            // Ensure sealed authority workspaces remain read-only (matches the baselines perfectly)
            let check_clean = |repo_name: &str| {
                let repo_path = parent_dir.join(repo_name);
                assert!(repo_path.exists(), "Sealed authority workspace path does not exist: {}", repo_path.display());

                let manifest_path = repo_path.join(".gc-sealed-baseline");
                assert!(manifest_path.exists(), "Manifest .gc-sealed-baseline does not exist for {}", repo_name);

                let manifest_content = fs::read_to_string(&manifest_path).unwrap();
                let mut manifest: BaselineManifest = serde_json::from_str(&manifest_content).expect("Failed to parse manifest JSON");

                assert_eq!(manifest.baseline_manifest_digest_algorithm, "sha256", "Manifest digest algorithm must be sha256");
                assert_eq!(manifest.gall_receipt_digest_algorithm, "blake3", "Gall receipt digest algorithm must be blake3");

                let expected_digest = manifest.digest.clone().expect("No digest field in manifest");
                manifest.digest = None;

                let serialized = serde_json::to_string(&manifest).expect("Failed to serialize manifest for digest validation");
                
                use sha2::{Sha256, Digest};
                let mut hasher = Sha256::new();
                hasher.update(serialized.as_bytes());
                let actual_digest = format!("{:x}", hasher.finalize());

                assert_eq!(
                    actual_digest,
                    expected_digest,
                    "Cryptographic digest mismatch in manifest for repo {}",
                    repo_name
                );

                let output = std::process::Command::new("git")
                    .args(&["-C", repo_path.to_str().unwrap(), "status", "--porcelain", "--ignored"])
                    .output()
                    .unwrap_or_else(|_| panic!("Failed to run git status on {}", repo_name));
                let status_out = String::from_utf8_lossy(&output.stdout);

                for line in status_out.lines() {
                    if line.is_empty() { continue; }
                    let status = &line[0..2];
                    let mut path = line[3..].to_string();
                    if path.starts_with('"') && path.ends_with('"') {
                        path = path[1..path.len()-1].to_string();
                    }

                    if status == "!!" || status == "??" {
                        let is_env_or_build_artifact = path.starts_with('.') || path.contains("/.")
                            || path.ends_with(".aux") || path.ends_with(".log") || path.ends_with(".out")
                            || path.ends_with(".pdf") || path.ends_with(".toc") || path.ends_with(".rlib")
                            || path.starts_with("_tmp_") || path.contains("/_tmp_")
                            || path.starts_with("apps/") || path.starts_with("artifacts/")
                            || path.starts_with("crates/") || path.starts_with("docs_quarantine/")
                            || path.starts_with("packages/") || path.starts_with("dist/")
                            || path.ends_with(".tsbuildinfo") || path.contains("undefined");
                        if is_env_or_build_artifact {
                            continue;
                        }
                    }

                    for pattern in &manifest.forbidden_generated_paths {
                        if path.contains(pattern) {
                            panic!("Sealed repo {} contains forbidden path matching policy '{}': {}", repo_name, pattern, path);
                        }
                    }

                    if status == "!!" {
                        let in_inventory = manifest.ignored_inventory.contains(&path) 
                            || manifest.ignored_inventory.contains(&format!("{}/", path));
                        let in_allowed_dir = manifest.allowed_ignored_directories.iter().any(|dir| {
                            path == *dir || path.starts_with(&format!("{}/", dir))
                        });
                        assert!(
                            in_inventory || in_allowed_dir,
                            "New non-baselined ignored file found in sealed repo {}: {} (status: {})",
                            repo_name,
                            path,
                            status
                        );
                    } else if status == "??" {
                        let allowed = manifest.ignored_inventory.contains(&path)
                            || manifest.allowed_ignored_directories.iter().any(|dir| {
                                path == *dir || path.starts_with(&format!("{}/", dir))
                            });
                        assert!(
                            allowed,
                            "New untracked file found in sealed repo {}: {}",
                            repo_name,
                            path
                        );
                    } else {
                        assert!(
                            manifest.tracked_status.contains_key(&path),
                            "New tracked change found in sealed repo {}: {} (status: {})",
                            repo_name,
                            path,
                            status
                        );
                    }
                }
            };

            check_clean("wasm4pm");
            check_clean("wasm4pm-compat");

        
    }
    
    // wasm4pm-lsp-no-local-conformance
    {
        
            let lsp_src = fs::read_to_string(parent_dir.join("wasm4pm").join("crates/wasm4pm-lsp/src/main.rs")).unwrap();
            // The LSP must not implement its own conformance loop (e.g. searching for markers or checking cardinalities directly)
            assert!(!lsp_src.contains("check_gall_conformance(&ocel) {")); // It shouldn't implement the logic directly
            assert!(!lsp_src.contains("GallVerdict::Fit {")); // It shouldn't manufacture these
            // It MUST call the adapter
            assert!(lsp_src.contains("gc005-wasm4pm-adapter"));
            
        
    }
    
    // adapter-no-fake-fit
    {
        
            let adapter_src = fs::read_to_string(tower_lsp_max.join("crates/gc005-wasm4pm-adapter/src/lib.rs")).unwrap();
            // The adapter must not contain string replacement logic for verdicts or hardcode "FIT" conditionally outside of matching the authority's enum.
            assert!(!adapter_src.contains("verdict = \"FIT\""));
            assert!(!adapter_src.contains("fitness == 1.0"));
            
        
    }
    
    // adapter-uses-sealed-authority
    {
        
            let adapter_src = fs::read_to_string(tower_lsp_max.join("crates/gc005-wasm4pm-adapter/src/lib.rs")).unwrap();
            // The adapter MUST import check_gall_conformance from wasm4pm_algos
            assert!(adapter_src.contains("check_gall_conformance"));
            assert!(adapter_src.contains("check_gall_conformance(&ocel)"));
            
            let adapter_cargo = fs::read_to_string(tower_lsp_max.join("crates/gc005-wasm4pm-adapter/Cargo.toml")).unwrap();
            assert!(adapter_cargo.contains("wasm4pm-algos = { path = \"../../../wasm4pm/crates/wasm4pm-algos\" }"));
            assert!(adapter_cargo.contains("ocel-core = { path = \"../../../wasm4pm/crates/ocel-core\" }"));
        
    }
    

    // Architecture Contract Checks
    let authority_surface = fs::read_to_string(tower_lsp_max.join("crates/playground/receipts/authority_surface.toml")).unwrap();
    assert!(authority_surface.contains("wasm4pm_algos::gall"));
    assert!(authority_surface.contains("check_gall_conformance"));
}
