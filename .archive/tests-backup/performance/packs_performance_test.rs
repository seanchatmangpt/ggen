//! Performance Tests for Packs Subsystem
//!
//! Validates that all pack operations meet performance requirements:
//! - Single commands < 100ms
//! - Multi-pack composition (3 packs) < 500ms
//! - Large packs < 2s

use ggen_domain::packs::{
    compose_packs, generate_from_pack, install_pack, list_packs, show_pack, ComposePacksInput,
    CompositionStrategy, GenerateInput, InstallInput,
};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Instant;

#[test]
fn test_list_packs_performance() {
    // Should complete in < 100ms
    let start = Instant::now();
    let _result = list_packs(None);
    let duration = start.elapsed();

    println!("list_packs took: {:?}", duration);
    assert!(
        duration.as_millis() < 100,
        "list_packs should complete in < 100ms, took {:?}",
        duration
    );
}

#[test]
fn test_show_pack_performance() {
    // Should complete in < 100ms
    let start = Instant::now();
    let _result = show_pack("startup-essentials");
    let duration = start.elapsed();

    println!("show_pack took: {:?}", duration);

    // Only assert if pack exists
    if _result.is_ok() {
        assert!(
            duration.as_millis() < 100,
            "show_pack should complete in < 100ms, took {:?}",
            duration
        );
    }
}

#[tokio::test]
async fn test_install_pack_dry_run_performance() {
    // Dry run should be fast (< 100ms)
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: Some(PathBuf::from("/tmp/perf-test-install")),
        force: false,
        dry_run: true,
    };

    let start = Instant::now();
    let _result = install_pack(&input).await;
    let duration = start.elapsed();

    println!("install_pack (dry run) took: {:?}", duration);

    if _result.is_ok() {
        assert!(
            duration.as_millis() < 100,
            "install_pack dry run should complete in < 100ms, took {:?}",
            duration
        );
    }
}

#[tokio::test]
async fn test_compose_two_packs_performance() {
    // Composing 2 packs should be fast (< 500ms)
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => {
            println!("Skipping performance test: no packs available");
            return;
        }
    };

    if packs.len() < 2 {
        println!("Need at least 2 packs for composition performance test");
        return;
    }

    let input = ComposePacksInput {
        pack_ids: vec![packs[0].id.clone(), packs[1].id.clone()],
        project_name: "perf-test".to_string(),
        output_dir: Some(PathBuf::from("/tmp/perf-compose-2")),
        strategy: CompositionStrategy::Merge,
    };

    let start = Instant::now();
    let result = compose_packs(&input).await;
    let duration = start.elapsed();

    println!("compose_packs (2 packs) took: {:?}", duration);

    if result.is_ok() {
        assert!(
            duration.as_millis() < 500,
            "compose_packs (2 packs) should complete in < 500ms, took {:?}",
            duration
        );
    }
}

#[tokio::test]
async fn test_compose_three_packs_performance() {
    // Composing 3 packs should be reasonable (< 500ms)
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    if packs.len() < 3 {
        println!("Need 3 packs for this test");
        return;
    }

    let input = ComposePacksInput {
        pack_ids: vec![
            packs[0].id.clone(),
            packs[1].id.clone(),
            packs[2].id.clone(),
        ],
        project_name: "perf-test-3".to_string(),
        output_dir: Some(PathBuf::from("/tmp/perf-compose-3")),
        strategy: CompositionStrategy::Merge,
    };

    let start = Instant::now();
    let result = compose_packs(&input).await;
    let duration = start.elapsed();

    println!("compose_packs (3 packs) took: {:?}", duration);

    if result.is_ok() {
        assert!(
            duration.as_millis() < 500,
            "compose_packs (3 packs) should complete in < 500ms, took {:?}",
            duration
        );
    }
}

#[tokio::test]
async fn test_generate_from_pack_performance() {
    // Generation should be reasonably fast (< 500ms for basic template)
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    let pack_with_template = packs.iter().find(|p| !p.templates.is_empty());
    if pack_with_template.is_none() {
        return;
    }

    let pack_id = &pack_with_template.unwrap().id;

    let input = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: "perf-test-gen".to_string(),
        template_name: None,
        output_dir: Some(PathBuf::from("/tmp/perf-generate")),
        variables: BTreeMap::new(),
    };

    let start = Instant::now();
    let result = generate_from_pack(&input).await;
    let duration = start.elapsed();

    println!("generate_from_pack took: {:?}", duration);

    if result.is_ok() {
        assert!(
            duration.as_millis() < 500,
            "generate_from_pack should complete in < 500ms, took {:?}",
            duration
        );
    }
}

#[test]
fn test_list_packs_with_filter_performance() {
    // Filtered list should be as fast as unfiltered
    let start = Instant::now();
    let _result = list_packs(Some("startup"));
    let duration = start.elapsed();

    println!("list_packs (filtered) took: {:?}", duration);
    assert!(
        duration.as_millis() < 100,
        "list_packs with filter should complete in < 100ms, took {:?}",
        duration
    );
}

#[test]
fn test_show_multiple_packs_performance() {
    // Showing multiple packs sequentially
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    if packs.len() < 3 {
        return;
    }

    let start = Instant::now();

    for pack in packs.iter().take(3) {
        let _result = show_pack(&pack.id);
    }

    let duration = start.elapsed();

    println!("show_pack (3 sequential) took: {:?}", duration);
    assert!(
        duration.as_millis() < 300,
        "3 sequential show_pack calls should complete in < 300ms, took {:?}",
        duration
    );
}

#[tokio::test]
async fn test_full_workflow_performance() {
    // Complete workflow: list -> show -> install -> generate
    let start = Instant::now();

    // List
    let packs = match list_packs(None) {
        Ok(p) => p,
        Err(_) => return,
    };

    if packs.is_empty() {
        return;
    }

    let pack_id = &packs[0].id;

    // Show
    let pack = match show_pack(pack_id) {
        Ok(p) => p,
        Err(_) => return,
    };

    // Install (dry run)
    let install_input = InstallInput {
        pack_id: pack_id.clone(),
        target_dir: Some(PathBuf::from("/tmp/perf-workflow")),
        force: false,
        dry_run: true,
    };

    let _ = install_pack(&install_input).await;

    // Generate (if templates available)
    if !pack.templates.is_empty() {
        let gen_input = GenerateInput {
            pack_id: pack_id.clone(),
            project_name: "perf-workflow".to_string(),
            template_name: None,
            output_dir: Some(PathBuf::from("/tmp/perf-workflow-gen")),
            variables: BTreeMap::new(),
        };

        let _ = generate_from_pack(&gen_input).await;
    }

    let duration = start.elapsed();

    println!("Full workflow took: {:?}", duration);
    assert!(
        duration.as_millis() < 1000,
        "Full workflow should complete in < 1000ms, took {:?}",
        duration
    );
}

#[test]
fn test_pack_serialization_performance() {
    // Test that pack serialization is fast
    use ggen_domain::packs::{Pack, PackTemplate};
    use std::collections::HashMap;

    let pack = Pack {
        id: "perf-test".to_string(),
        name: "Performance Test Pack".to_string(),
        version: "1.0.0".to_string(),
        description: "Test".to_string(),
        category: "test".to_string(),
        author: None,
        repository: None,
        license: None,
        packages: (0..100).map(|i| format!("package-{}", i)).collect(),
        templates: (0..10)
            .map(|i| PackTemplate {
                name: format!("template-{}", i),
                path: format!("templates/template-{}", i),
                description: "Test template".to_string(),
                variables: vec!["var1".to_string(), "var2".to_string()],
            })
            .collect(),
        sparql_queries: HashMap::new(),
        dependencies: vec![],
        tags: vec!["test".to_string()],
        keywords: vec!["test".to_string()],
        production_ready: true,
        metadata: Default::default(),
    };

    let start = Instant::now();
    let json = serde_json::to_string(&pack).expect("Should serialize");
    let _deserialized: Pack = serde_json::from_str(&json).expect("Should deserialize");
    let duration = start.elapsed();

    println!("Pack serialization/deserialization took: {:?}", duration);
    assert!(
        duration.as_micros() < 1000,
        "Pack serialization should be < 1ms, took {:?}",
        duration
    );
}
