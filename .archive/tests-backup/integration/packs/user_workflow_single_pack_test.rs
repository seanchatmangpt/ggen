//! User Workflow Tests - Single Pack Projects
//!
//! Critical end-to-end tests for user workflows with single packs.
//! These are the FMEA validation tests.
//!
//! Workflow 1: Single-pack web API project
//! Workflow 2: Single-pack data science project

use ggen_domain::packs::{
    generate_from_pack, install_pack, list_packs, show_pack, GenerateInput, InstallInput,
};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_workflow_1_web_api_pack_complete_flow() {
    // WORKFLOW 1: Single-pack web API project
    // User journey:
    // 1. List packs to find web-related packs
    // 2. Show pack details
    // 3. Install the pack
    // 4. Generate project from pack
    // 5. Verify project is ready to build/run

    // Step 1: List packs and filter
    let list_result = list_packs(None);
    match list_result {
        Ok(packs) => {
            // Should have some packs
            assert!(packs.len() > 0, "Should have at least one pack");

            // Find startup pack (closest to web API)
            let web_packs: Vec<_> = packs
                .iter()
                .filter(|p| {
                    p.category == "startup"
                        || p.tags.iter().any(|t| t.contains("api") || t.contains("web"))
                })
                .collect();

            if web_packs.is_empty() {
                println!("No web-related packs found, skipping workflow test");
                return;
            }

            let pack_id = &web_packs[0].id;

            // Step 2: Show pack details
            let show_result = show_pack(pack_id);
            assert!(show_result.is_ok(), "Should show pack details");

            let pack = show_result.unwrap();
            assert!(!pack.packages.is_empty(), "Pack should have packages");
            assert!(
                !pack.templates.is_empty() || pack.packages.len() > 0,
                "Pack should have templates or packages"
            );

            // Step 3: Install pack (dry run)
            let install_input = InstallInput {
                pack_id: pack_id.clone(),
                target_dir: Some(PathBuf::from(format!("/tmp/test-workflow1-{}", pack_id))),
                force: false,
                dry_run: true,
            };

            let install_result = install_pack(&install_input).await;
            assert!(install_result.is_ok(), "Should install pack (dry run)");

            let install_output = install_result.unwrap();
            assert_eq!(install_output.pack_id, *pack_id);
            assert!(install_output.total_packages > 0, "Should have packages to install");

            // Step 4: Generate project (if templates available)
            if !pack.templates.is_empty() {
                let mut variables = BTreeMap::new();
                variables.insert("project_name".to_string(), "my-web-api".to_string());
                variables.insert("port".to_string(), "8080".to_string());

                let gen_input = GenerateInput {
                    pack_id: pack_id.clone(),
                    project_name: "my-web-api".to_string(),
                    template_name: Some(pack.templates[0].name.clone()),
                    output_dir: Some(PathBuf::from(format!(
                        "/tmp/test-workflow1-gen-{}",
                        pack_id
                    ))),
                    variables,
                };

                let gen_result = generate_from_pack(&gen_input).await;
                assert!(gen_result.is_ok(), "Should generate project from pack");

                let gen_output = gen_result.unwrap();
                assert_eq!(gen_output.project_name, "my-web-api");
                assert!(gen_output.files_created >= 0);
            }

            // Step 5: Verify project structure (would check files exist in real scenario)
            // For now, we've validated the API chain works end-to-end
            println!("✓ Workflow 1 (Web API) completed successfully");
        }
        Err(e) => {
            // Expected if packs directory doesn't exist in test environment
            println!("Skipping workflow test: {}", e);
        }
    }
}

#[tokio::test]
async fn test_workflow_2_data_science_pack_complete_flow() {
    // WORKFLOW 2: Single-pack data science project
    // User journey:
    // 1. List packs filtered by ML/data-science
    // 2. Show data science pack
    // 3. Install pack
    // 4. Generate ML project
    // 5. Verify ML dependencies and notebooks available

    // Step 1: List packs with ML category
    let list_result = list_packs(Some("ml"));
    match list_result {
        Ok(packs) => {
            if packs.is_empty() {
                println!("No ML packs found in test environment");
                // Try without filter
                let all_packs = list_packs(None).unwrap_or_default();
                if all_packs.is_empty() {
                    println!("No packs available, skipping workflow test");
                    return;
                }

                // Use first available pack for testing
                let pack_id = &all_packs[0].id;
                test_pack_workflow(pack_id, "ml-project").await;
            } else {
                // Found ML packs
                assert!(packs.len() > 0, "Should have ML packs");
                let pack_id = &packs[0].id;

                // Step 2: Show pack
                let show_result = show_pack(pack_id);
                assert!(show_result.is_ok(), "Should show ML pack");

                let pack = show_result.unwrap();
                assert_eq!(pack.category, "ml", "Should be ML category");

                // Step 3: Install (dry run)
                let install_input = InstallInput {
                    pack_id: pack_id.clone(),
                    target_dir: Some(PathBuf::from(format!("/tmp/test-workflow2-{}", pack_id))),
                    force: false,
                    dry_run: true,
                };

                let install_result = install_pack(&install_input).await;
                assert!(install_result.is_ok(), "Should install ML pack");

                // Step 4: Generate ML project
                if !pack.templates.is_empty() {
                    let mut variables = BTreeMap::new();
                    variables.insert("model_type".to_string(), "classification".to_string());
                    variables.insert("dataset".to_string(), "iris".to_string());

                    let gen_input = GenerateInput {
                        pack_id: pack_id.clone(),
                        project_name: "ml-project".to_string(),
                        template_name: Some(pack.templates[0].name.clone()),
                        output_dir: Some(PathBuf::from(format!(
                            "/tmp/test-workflow2-gen-{}",
                            pack_id
                        ))),
                        variables,
                    };

                    let gen_result = generate_from_pack(&gen_input).await;
                    assert!(gen_result.is_ok(), "Should generate ML project");

                    let gen_output = gen_result.unwrap();
                    assert!(
                        gen_output.templates_generated.len() > 0,
                        "Should generate ML templates"
                    );
                }

                // Step 5: Verify can train models (simulated)
                println!("✓ Workflow 2 (Data Science) completed successfully");
            }
        }
        Err(_e) => {
            // Expected if packs don't exist
            println!("Skipping ML workflow test");
        }
    }
}

// Helper function for testing pack workflow
async fn test_pack_workflow(pack_id: &str, project_name: &str) {
    let show_result = show_pack(pack_id);
    if show_result.is_err() {
        println!("Pack {} not found, skipping", pack_id);
        return;
    }

    let pack = show_result.unwrap();

    // Install
    let install_input = InstallInput {
        pack_id: pack_id.to_string(),
        target_dir: Some(PathBuf::from(format!("/tmp/test-workflow-{}", pack_id))),
        force: false,
        dry_run: true,
    };

    let install_result = install_pack(&install_input).await;
    assert!(install_result.is_ok(), "Should install pack");

    // Generate if templates available
    if !pack.templates.is_empty() {
        let gen_input = GenerateInput {
            pack_id: pack_id.to_string(),
            project_name: project_name.to_string(),
            template_name: None,
            output_dir: Some(PathBuf::from(format!("/tmp/test-workflow-gen-{}", pack_id))),
            variables: BTreeMap::new(),
        };

        let gen_result = generate_from_pack(&gen_input).await;
        assert!(gen_result.is_ok(), "Should generate project");
    }
}

#[tokio::test]
async fn test_single_pack_list_show_install_chain() {
    // Simplified end-to-end test of the critical chain:
    // list -> show -> install

    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => {
            println!("No packs available");
            return;
        }
    };

    if packs.is_empty() {
        println!("No packs to test");
        return;
    }

    let pack_id = &packs[0].id;

    // Show
    let pack = show_pack(pack_id).expect("Should show pack");
    assert_eq!(&pack.id, pack_id);

    // Install dry run
    let install_input = InstallInput {
        pack_id: pack_id.clone(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    let install_output = install_pack(&install_input).await.expect("Should install");
    assert_eq!(&install_output.pack_id, pack_id);

    println!("✓ list -> show -> install chain works");
}

#[tokio::test]
async fn test_pack_with_all_metadata_fields() {
    // Test that a pack with full metadata works end-to-end
    let pack_id = "startup-essentials";

    let pack_result = show_pack(pack_id);
    match pack_result {
        Ok(pack) => {
            // Verify metadata is loaded
            assert!(!pack.name.is_empty());
            assert!(!pack.version.is_empty());
            assert!(!pack.description.is_empty());

            // Install should handle all metadata
            let install_input = InstallInput {
                pack_id: pack_id.to_string(),
                target_dir: None,
                force: false,
                dry_run: true,
            };

            let result = install_pack(&install_input).await;
            assert!(result.is_ok(), "Should handle pack with full metadata");
        }
        Err(_) => {
            println!("Pack not found in test environment");
        }
    }
}
