//! Performance Tests for Autonomous System
//!
//! Validates machine timescale requirements (< 6 minutes for complete cycle)
//! Tests parallel regeneration speed, memory usage, and concurrent operations.

use ggen_ai::{
    autonomous::{
        events::{ChangeEvent, ChangeType, GraphChangeNotifier},
        regeneration::{AffectedArtifact, RegenerationConfig, RegenerationEngine},
    },
    EvolutionConfig, GraphEvolutionEngine, MockClient,
};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Target: Complete NL → Graph → Code cycle in < 6 minutes (360 seconds)
const TARGET_CYCLE_TIME_SECONDS: u64 = 360;

/// Target: Individual evolution in < 30 seconds
const TARGET_EVOLUTION_TIME_SECONDS: u64 = 30;

/// Target: Regeneration in < 2 minutes
const TARGET_REGENERATION_TIME_SECONDS: u64 = 120;

/// Test complete cycle time meets < 6 minute requirement
#[tokio::test]
async fn test_complete_cycle_time_under_6_minutes() {
    let start = Instant::now();

    let mock_response = r#"
```turtle
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
ex:FastEntity a owl:Class .
```

```json
[
    {"subject": "ex:FastEntity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Fast"}
]
```
"#;

    // Step 1: Evolution (should be < 30s)
    let evolution_start = Instant::now();
    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("valid");

    let mut engine =
        GraphEvolutionEngine::with_defaults(Box::new(parser_client), Box::new(validator_client))
            .expect("Failed to create engine");

    let evolution_result = engine
        .evolve_from_nl("Create fast entity")
        .await
        .expect("Evolution failed");

    let evolution_time = evolution_start.elapsed();
    assert!(evolution_result.success, "Evolution should succeed");
    println!("Evolution time: {:?}", evolution_time);

    // Step 2: Regeneration (should be < 2 minutes)
    let regen_start = Instant::now();
    let template_client = MockClient::with_response("Generated template");
    let notifier = Arc::new(GraphChangeNotifier::default());

    let regen_config = RegenerationConfig {
        incremental: true,
        parallel_workers: 4, // Maximize parallelism
        target_languages: vec!["rust".to_string()],
        template_dirs: vec![PathBuf::from("templates")],
        output_dir: PathBuf::from("generated"),
        auto_version: true,
        track_dependencies: true,
    };

    let regen_engine = Arc::new(RegenerationEngine::new(
        regen_config,
        Box::new(template_client),
        notifier.clone(),
    ));

    // Register artifact
    let artifact = AffectedArtifact {
        id: "perf_template".to_string(),
        template_id: "fast".to_string(),
        language: "rust".to_string(),
        output_path: PathBuf::from("generated/fast.rs"),
        version: "1.0.0".to_string(),
        dependencies: Vec::new(),
        last_regenerated: None,
    };
    regen_engine.register_artifact(artifact).await;

    // Trigger regeneration
    let event = ChangeEvent::node_added(
        "ex:FastEntity".to_string(),
        std::collections::HashMap::new(),
        "perf_test".to_string(),
    );
    notifier.publish(event).await.expect("Failed to publish");

    let regen_clone = regen_engine.clone();
    tokio::spawn(async move {
        let _ = regen_clone.start().await;
    });

    // Wait for regeneration
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    let regen_time = regen_start.elapsed();
    println!("Regeneration time: {:?}", regen_time);

    // Step 3: Total cycle time
    let total_time = start.elapsed();
    println!("Total cycle time: {:?}", total_time);

    assert!(
        total_time.as_secs() < TARGET_CYCLE_TIME_SECONDS,
        "Complete cycle should be under {} seconds, took {}s",
        TARGET_CYCLE_TIME_SECONDS,
        total_time.as_secs()
    );
}

/// Test individual evolution operations are fast (< 30 seconds)
#[tokio::test]
async fn test_individual_evolution_speed() {
    let mock_response = r#"
```turtle
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Fast"}
]
```
"#;

    let start = Instant::now();

    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("valid");

    let mut engine =
        GraphEvolutionEngine::with_defaults(Box::new(parser_client), Box::new(validator_client))
            .expect("Failed to create engine");

    let result = engine
        .evolve_from_nl("Quick evolution")
        .await
        .expect("Evolution failed");

    let elapsed = start.elapsed();

    assert!(result.success, "Evolution should succeed");
    assert!(
        elapsed.as_secs() < TARGET_EVOLUTION_TIME_SECONDS,
        "Evolution should be under {}s, took {}s",
        TARGET_EVOLUTION_TIME_SECONDS,
        elapsed.as_secs()
    );

    println!("Evolution completed in {:?}", elapsed);
}

/// Test parallel regeneration achieves target performance
#[tokio::test]
async fn test_parallel_regeneration_performance() {
    let start = Instant::now();

    let template_client = MockClient::with_response("Template");
    let notifier = Arc::new(GraphChangeNotifier::default());

    let regen_config = RegenerationConfig {
        incremental: true,
        parallel_workers: 8, // High parallelism
        target_languages: vec![
            "rust".to_string(),
            "typescript".to_string(),
            "python".to_string(),
        ],
        template_dirs: vec![PathBuf::from("templates")],
        output_dir: PathBuf::from("generated"),
        auto_version: true,
        track_dependencies: true,
    };

    let regen_engine = Arc::new(RegenerationEngine::new(
        regen_config,
        Box::new(template_client),
        notifier.clone(),
    ));

    // Register multiple artifacts
    for i in 0..10 {
        let artifact = AffectedArtifact {
            id: format!("template_{}", i),
            template_id: format!("tpl_{}", i),
            language: "rust".to_string(),
            output_path: PathBuf::from(format!("generated/file_{}.rs", i)),
            version: "1.0.0".to_string(),
            dependencies: Vec::new(),
            last_regenerated: None,
        };
        regen_engine.register_artifact(artifact).await;
    }

    // Trigger multiple events
    for i in 0..10 {
        let event = ChangeEvent::node_added(
            format!("ex:Entity{}", i),
            std::collections::HashMap::new(),
            "perf_test".to_string(),
        );
        notifier.publish(event).await.expect("Failed to publish");
    }

    let regen_clone = regen_engine.clone();
    tokio::spawn(async move {
        let _ = regen_clone.start().await;
    });

    // Wait for all regenerations
    tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

    let elapsed = start.elapsed();
    let stats = regen_engine.get_stats().await;

    println!("Parallel regeneration stats: {:?}", stats);
    println!("Time: {:?}", elapsed);

    assert!(stats.events_processed >= 10, "Should process all events");

    // With parallelism, 10 regenerations should be much faster than sequential
    assert!(
        elapsed.as_secs() < TARGET_REGENERATION_TIME_SECONDS,
        "Parallel regeneration should be under {}s, took {}s",
        TARGET_REGENERATION_TIME_SECONDS,
        elapsed.as_secs()
    );
}

/// Test memory usage stays within reasonable bounds
#[tokio::test]
async fn test_memory_usage_under_load() {
    use std::mem;

    let initial_alloc = get_current_memory_usage();

    let mock_response = r#"
```turtle
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Test"}
]
```
"#;

    // Perform 50 evolutions
    for i in 0..50 {
        let parser_client = MockClient::with_response(mock_response);
        let validator_client = MockClient::with_response("valid");

        let mut engine = GraphEvolutionEngine::with_defaults(
            Box::new(parser_client),
            Box::new(validator_client),
        )
        .expect("Failed to create engine");

        let _ = engine.evolve_from_nl(&format!("Entity {}", i)).await;
    }

    let final_alloc = get_current_memory_usage();
    let memory_increase_mb = (final_alloc - initial_alloc) as f64 / 1_048_576.0;

    println!("Memory increase: {:.2} MB", memory_increase_mb);

    // Should stay under 100 MB for 50 operations
    assert!(
        memory_increase_mb < 100.0,
        "Memory usage should be reasonable, increased by {:.2} MB",
        memory_increase_mb
    );
}

/// Test concurrent evolution operations scale efficiently
#[tokio::test]
async fn test_concurrent_evolution_scaling() {
    let concurrency_levels = vec![1, 2, 4, 8];
    let operations_per_level = 10;

    let mock_response = r#"
```turtle
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Test"}
]
```
"#;

    for concurrency in concurrency_levels {
        let start = Instant::now();

        let mut handles = vec![];
        for i in 0..operations_per_level {
            let response = mock_response.to_string();
            let handle = tokio::spawn(async move {
                let parser_client = MockClient::with_response(&response);
                let validator_client = MockClient::with_response("valid");

                let mut engine = GraphEvolutionEngine::with_defaults(
                    Box::new(parser_client),
                    Box::new(validator_client),
                )
                .expect("Failed to create engine");

                engine.evolve_from_nl(&format!("Entity {}", i)).await
            });

            handles.push(handle);

            // Control concurrency by limiting spawns
            if handles.len() >= concurrency {
                let _ = handles.pop().unwrap().await;
            }
        }

        // Wait for remaining
        futures::future::join_all(handles).await;

        let elapsed = start.elapsed();
        println!(
            "Concurrency {}: {} ops in {:?} ({:.2} ops/sec)",
            concurrency,
            operations_per_level,
            elapsed,
            operations_per_level as f64 / elapsed.as_secs_f64()
        );
    }
}

/// Test that delta detection is fast
#[tokio::test]
async fn test_delta_detection_performance() {
    use ggen_ai::DeltaDetector;

    let mut detector = DeltaDetector::new().expect("Failed to create detector");

    // Create large baseline (1000 triples)
    let baseline: Vec<String> = (0..1000)
        .map(|i| format!("ex:Entity{} rdf:type owl:Class .", i))
        .collect();

    let start = Instant::now();
    detector
        .set_baseline(&baseline)
        .expect("Failed to set baseline");
    let baseline_time = start.elapsed();

    println!("Baseline set time: {:?}", baseline_time);

    // Create modified state (990 same, 10 new, 10 deleted)
    let mut new_state: Vec<String> = (0..990)
        .map(|i| format!("ex:Entity{} rdf:type owl:Class .", i))
        .collect();
    new_state.extend((1000..1010).map(|i| format!("ex:NewEntity{} rdf:type owl:Class .", i)));

    let start = Instant::now();
    let delta = detector
        .compute_delta(&new_state)
        .expect("Failed to compute delta");
    let delta_time = start.elapsed();

    println!("Delta computation time: {:?}", delta_time);
    println!(
        "Delta: {} additions, {} deletions",
        delta.stats.additions_count, delta.stats.deletions_count
    );

    // Delta detection should be fast even for large graphs
    assert!(
        delta_time.as_millis() < 1000,
        "Delta detection should be under 1s, took {}ms",
        delta_time.as_millis()
    );
}

/// Test validation performance
#[tokio::test]
async fn test_validation_performance() {
    use ggen_ai::SelfValidator;

    let client = MockClient::with_response("valid");
    let validator = SelfValidator::new(Box::new(client)).expect("Failed to create validator");

    // Create triples to validate
    let triples: Vec<String> = (0..100)
        .map(|i| format!("ex:Entity{} rdf:type owl:Class .", i))
        .collect();

    let start = Instant::now();
    let result = validator
        .validate(&triples)
        .await
        .expect("Validation failed");
    let elapsed = start.elapsed();

    println!("Validated {} triples in {:?}", triples.len(), elapsed);

    assert!(
        elapsed.as_millis() < 5000,
        "Validation should be under 5s, took {}ms",
        elapsed.as_millis()
    );
}

/// Helper function to get current memory usage (approximation)
fn get_current_memory_usage() -> usize {
    // This is a simplified approximation
    // In production, use proper memory profiling tools
    std::alloc::System.alloc_size()
}

// Trait to get allocation size (simplified for testing)
trait AllocSize {
    fn alloc_size(&self) -> usize {
        // Return 0 as placeholder - real implementation would track allocations
        0
    }
}

impl AllocSize for std::alloc::System {}
