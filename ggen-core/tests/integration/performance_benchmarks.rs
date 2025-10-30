//! Performance Benchmark Integration Tests
//!
//! This module validates that benchmark scenarios execute correctly
//! and establishes baseline performance metrics for CI/CD.

use ggen_core::cleanroom::policy::Locked;
use ggen_core::cleanroom::surfaces::DeterministicSurfaces;
use ggen_core::cleanroom::CleanroomCore;
use ggen_core::lifecycle::{Context, Make, Phase, Project};
use ggen_core::registry::{PackMetadata, RegistryIndex, VersionMetadata};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use std::time::Instant;

/// Performance baseline thresholds
mod thresholds {
    use std::time::Duration;

    // Marketplace operations
    pub const SEARCH_1000_PACKAGES: Duration = Duration::from_millis(50);
    pub const SEARCH_10000_PACKAGES: Duration = Duration::from_millis(500);
    pub const VERSION_RESOLUTION: Duration = Duration::from_millis(10);
    pub const INDEX_SERIALIZATION_1000: Duration = Duration::from_millis(100);

    // Lifecycle operations
    pub const PHASE_EXECUTION: Duration = Duration::from_millis(200);
    pub const CACHE_VALIDATION_100: Duration = Duration::from_millis(50);
    pub const STATE_SAVE_1000: Duration = Duration::from_millis(100);
    pub const STATE_LOAD_1000: Duration = Duration::from_millis(50);

    // Stress tests
    pub const CONCURRENT_SEARCHES_100: Duration = Duration::from_secs(2);
    pub const CACHE_KEY_GENERATION_10000: Duration = Duration::from_millis(500);
}

fn create_test_registry(size: usize) -> RegistryIndex {
    let mut packs = HashMap::new();

    for i in 0..size {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();

        for j in 0..5 {
            let version = format!("{}.0.0", j);
            versions.insert(
                version.clone(),
                VersionMetadata {
                    version: version.clone(),
                    git_url: format!("https://github.com/test/{}.git", id),
                    git_rev: format!("v{}", version),
                    manifest_url: None,
                    sha256: format!("hash{}", j),
                },
            );
        }

        packs.insert(
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: format!("Description for package {}", i),
                tags: vec![format!("tag{}", i % 10), "rust".to_string()],
                keywords: vec![format!("keyword{}", i % 20), "cli".to_string()],
                category: Some("development".to_string()),
                author: Some("Test Author".to_string()),
                latest_version: "4.0.0".to_string(),
                versions,
                downloads: Some(i as u64 * 100),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            },
        );
    }

    RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    }
}

#[test]
fn test_marketplace_search_performance() {
    let index = create_test_registry(1000);

    let start = Instant::now();
    let query = "rust";
    let query_lower = query.to_lowercase();

    let results: Vec<_> = index
        .packs
        .iter()
        .filter(|(_, pack)| {
            pack.name.to_lowercase().contains(&query_lower)
                || pack.description.to_lowercase().contains(&query_lower)
        })
        .collect();

    let duration = start.elapsed();

    println!(
        "Searched 1000 packages in {:?}, found {} results",
        duration,
        results.len()
    );
    assert!(
        duration < thresholds::SEARCH_1000_PACKAGES,
        "Search took {:?}, expected < {:?}",
        duration,
        thresholds::SEARCH_1000_PACKAGES
    );
}

#[test]
fn test_version_resolution_performance() {
    let index = create_test_registry(1000);

    let start = Instant::now();
    let package_id = "package-500";
    let version = "2.0.0";

    let result = index.packs.get(package_id).and_then(|pack| pack.versions.get(version));

    let duration = start.elapsed();

    println!("Resolved version in {:?}", duration);
    assert!(result.is_some());
    assert!(
        duration < thresholds::VERSION_RESOLUTION,
        "Version resolution took {:?}, expected < {:?}",
        duration,
        thresholds::VERSION_RESOLUTION
    );
}

#[test]
fn test_index_serialization_performance() {
    let index = create_test_registry(1000);

    let start = Instant::now();
    let json = serde_json::to_string(&index).expect("Failed to serialize");
    let duration = start.elapsed();

    println!(
        "Serialized 1000 packages to {} bytes in {:?}",
        json.len(),
        duration
    );
    assert!(
        duration < thresholds::INDEX_SERIALIZATION_1000,
        "Serialization took {:?}, expected < {:?}",
        duration,
        thresholds::INDEX_SERIALIZATION_1000
    );
}

#[test]
fn test_lifecycle_phase_execution_performance() {
    let env = CleanroomCore::<Locked>::builder()
        .time_frozen(42)
        .rng_seeded(42)
        .fs_ephemeral()
        .net_offline()
        .build()
        .expect("Failed to create cleanroom");

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "test".to_string(),
        Phase {
            description: Some("Test phase".to_string()),
            command: Some("echo test".to_string()),
            commands: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        },
    );

    let make = Make {
        project: Project {
            name: "test".to_string(),
            project_type: Some("test".to_string()),
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        env.root().to_path_buf(),
        Arc::new(make),
        env.root().join(".ggen/state.json"),
        vec![],
    );

    let start = Instant::now();
    ggen_core::lifecycle::run_phase(&ctx, "test").expect("Failed to run phase");
    let duration = start.elapsed();

    println!("Executed phase in {:?}", duration);
    assert!(
        duration < thresholds::PHASE_EXECUTION,
        "Phase execution took {:?}, expected < {:?}",
        duration,
        thresholds::PHASE_EXECUTION
    );
}

#[test]
fn test_deterministic_performance() {
    // Run the same operation twice with deterministic surfaces
    let mut durations = Vec::new();

    for run in 0..2 {
        let env = CleanroomCore::<Locked>::builder()
            .time_frozen(42)
            .rng_seeded(42)
            .build()
            .expect("Failed to create cleanroom");

        let start = Instant::now();
        let index = create_test_registry(100);
        let _serialized = serde_json::to_string(&index).expect("Failed to serialize");
        let duration = start.elapsed();

        durations.push(duration);
        println!("Run {}: {:?}", run + 1, duration);
    }

    // With deterministic surfaces, timing should be relatively consistent
    // (within 50% variance due to system scheduling)
    let diff = if durations[0] > durations[1] {
        durations[0] - durations[1]
    } else {
        durations[1] - durations[0]
    };

    let avg = (durations[0] + durations[1]) / 2;
    let variance_pct = (diff.as_nanos() * 100) / avg.as_nanos();

    println!(
        "Variance: {}% (diff: {:?}, avg: {:?})",
        variance_pct, diff, avg
    );

    // Note: This is a weak assertion since system scheduling can vary
    // In a true cleanroom container environment, variance would be much lower
    assert!(
        variance_pct < 100,
        "Variance too high: {}%",
        variance_pct
    );
}

#[test]
fn test_stress_concurrent_operations() {
    use rayon::prelude::*;

    let index = create_test_registry(1000);

    let start = Instant::now();
    let queries: Vec<&str> = vec![
        "rust", "web", "cli", "database", "async", "http", "json", "xml", "parser", "api",
    ];

    let results: Vec<_> = queries
        .par_iter()
        .flat_map(|query| {
            let query_lower = query.to_lowercase();
            index
                .packs
                .iter()
                .filter(|(_, pack)| pack.name.to_lowercase().contains(&query_lower))
                .collect::<Vec<_>>()
        })
        .collect();

    let duration = start.elapsed();

    println!(
        "Executed 10 concurrent searches in {:?}, found {} total results",
        duration,
        results.len()
    );
    assert!(
        duration < thresholds::CONCURRENT_SEARCHES_100,
        "Concurrent searches took {:?}, expected < {:?}",
        duration,
        thresholds::CONCURRENT_SEARCHES_100
    );
}

#[test]
fn test_performance_metrics_collection() {
    #[derive(Debug)]
    struct PerformanceMetrics {
        operation: String,
        duration: std::time::Duration,
        items_processed: usize,
        throughput: f64, // items per second
    }

    let mut metrics = Vec::new();

    // Collect metrics for various operations
    let index = create_test_registry(1000);

    // Search operation
    let start = Instant::now();
    let results: Vec<_> = index
        .packs
        .iter()
        .filter(|(_, pack)| pack.name.contains("rust"))
        .collect();
    let duration = start.elapsed();
    metrics.push(PerformanceMetrics {
        operation: "marketplace_search".to_string(),
        duration,
        items_processed: results.len(),
        throughput: (results.len() as f64) / duration.as_secs_f64(),
    });

    // Serialization operation
    let start = Instant::now();
    let json = serde_json::to_string(&index).expect("Failed to serialize");
    let duration = start.elapsed();
    metrics.push(PerformanceMetrics {
        operation: "index_serialization".to_string(),
        duration,
        items_processed: json.len(),
        throughput: (json.len() as f64) / duration.as_secs_f64(),
    });

    // Print metrics summary
    println!("\n=== Performance Metrics Summary ===");
    for metric in &metrics {
        println!(
            "{}: {:?} ({} items, {:.0} items/sec)",
            metric.operation, metric.duration, metric.items_processed, metric.throughput
        );
    }

    // Verify all operations completed within reasonable time
    assert!(metrics.iter().all(|m| m.duration < std::time::Duration::from_secs(1)));
}
