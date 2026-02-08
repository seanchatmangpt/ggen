//! Comprehensive Performance Benchmark for WIP Components
//!
//! This benchmark suite evaluates the performance of new WIP components:
//! 1. Validation Components (path_validator.rs, safe_command.rs)
//! 2. Workflow System (ggen-workflow)
//! 4. CLI Commands
//! 5. Connectors (knhk-connectors)
//! 6. Memory Usage Patterns
//! 7. Critical Path Analysis
//!
//! SLO Targets:
//! - First build ≤15s
//! - RDF processing ≤5s/1k triples
//! - Incremental ≤2s

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

// WIP Components imports
use ggen_agent::{agent::AgentManager, bridge::EventBridge};
use ggen_cli::{cmds::agent::*, cmds::mcp::*};
use ggen_utils::path_validator::{PathValidator, SafePath};
use ggen_utils::safe_command::{CommandArg, CommandName, SafeCommand};
use ggen_workflow::{
    patterns::{Parallel, Sequence, WorkflowContext},
    WorkflowContext as WorkflowCtx,
};
use knhk_connectors::{
    ConnectorId, ConnectorRegistry, DataFormat, Delta, SoAArrays, SourceType, Triple,
};

// Test fixtures
const TEST_WORKSPACE_ROOT: &str = "/tmp/ggen-test-workspace";
const TEST_RDF_CONTENT: &str = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class .
ex:name a rdf:Property .
ex:email a rdf:Property .

ex:user1 a ex:User ;
    ex:name "Alice" ;
    ex:email "alice@example.com" .

ex:user2 a ex:User ;
    ex:name "Bob" ;
    ex:email "bob@example.com" .
"#;

const TEST_KAFKA_PAYLOAD: &str = r#"[
  {"user": "alice", "action": "login", "timestamp": "2024-01-01T00:00:00Z"},
  {"user": "bob", "action": "logout", "timestamp": "2024-01-01T00:01:00Z"}
]"#;

fn create_test_environment() {
    std::fs::create_dir_all(TEST_WORKSPACE_ROOT).unwrap();

    // Create test files
    std::fs::write(
        format!("{}/templates/example.tera", TEST_WORKSPACE_ROOT),
        "Hello, {{ name }}!",
    )
    .unwrap();

    std::fs::write(
        format!("{}/specs/test.ttl", TEST_WORKSPACE_ROOT),
        TEST_RDF_CONTENT,
    )
    .unwrap();
}

fn cleanup_test_environment() {
    std::fs::remove_dir_all(TEST_WORKSPACE_ROOT).unwrap();
}

fn bench_path_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("path_validation");

    create_test_environment();

    // Test cases
    let test_cases = vec![
        ("simple_path", "templates/example.tera"),
        ("deep_path", "templates/nested/deep/path/example.tera"),
        ("with_spaces", "templates/my template.tera"),
        ("unicode_path", "templates/ñame.tera"),
    ];

    for (name, path) in test_cases {
        let validator = PathValidator::new(Path::new(TEST_WORKSPACE_ROOT))
            .with_max_depth(10)
            .with_allowed_extensions(vec!["tera", "ttl"]);

        group.bench_with_input(BenchmarkId::new("validate", name), path, |b, path| {
            b.iter(|| black_box(validator.validate(path).unwrap()));
        });
    }

    // Security benchmarks
    let malicious_paths = vec![
        ("path_traversal", "templates/../../../etc/passwd"),
        ("symlink_attack", "/tmp/malicious"),
        ("null_injection", "templates/\0malicious.tera"),
    ];

    for (name, path) in malicious_paths {
        let validator = PathValidator::new(Path::new(TEST_WORKSPACE_ROOT)).with_max_depth(10);

        group.bench_with_input(
            BenchmarkId::new("security_validate", name),
            path,
            |b, path| {
                b.iter(|| {
                    let result = validator.validate(path);
                    black_box(result.is_err())
                });
            },
        );
    }

    group.sample_size(100);
    group.measurement_time(Duration::from_secs(10));

    cleanup_test_environment();
    group.finish();
}

fn bench_safe_command(c: &mut Criterion) {
    let mut group = c.benchmark_group("safe_command");

    // Valid command benchmarks
    let valid_commands = vec![
        ("cargo_build", vec!["cargo", "build", "--release"]),
        ("cargo_check", vec!["cargo", "check", "--all"]),
        ("cargo_test", vec!["cargo", "test"]),
    ];

    for (name, args) in valid_commands {
        group.bench_with_input(BenchmarkId::new("valid_command", name), &args, |b, args| {
            b.iter(|| {
                let mut cmd = SafeCommand::new(args[0]).unwrap();
                for arg in &args[1..] {
                    cmd = cmd.arg(arg).unwrap();
                }
                black_box(cmd.validate().is_ok())
            });
        });
    }

    // Invalid command benchmarks
    let invalid_commands = vec![
        ("command_injection", vec!["cargo", "build; rm -rf /"]),
        ("metacharacter", vec!["ls", "*.txt; echo hacked"]),
        ("long_command", vec!["echo", "a".repeat(5000)]),
    ];

    for (name, args) in invalid_commands {
        group.bench_with_input(
            BenchmarkId::new("invalid_command", name),
            &args,
            |b, args| {
                b.iter(|| {
                    let result = SafeCommand::new(args[0]).and_then(|cmd| cmd.arg(&args[1]));
                    black_box(result.is_err())
                });
            },
        );
    }

    group.sample_size(100);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_agent_system(c: &mut Criterion) {
    let mut group = c.benchmark_group("agent_system");

    create_test_environment();

    // Agent creation and initialization
    group.bench_function("agent_creation", |b| {
        b.iter(|| black_box(AgentManager::new("test-agent", "1.0.0").unwrap()));
    });

    // Event bridge performance
    group.bench_function("event_bridge_dispatch", |b| {
        let bridge = EventBridge::new();
        let test_event = HashMap::from([
            ("type".to_string(), "test".to_string()),
            ("data".to_string(), serde_json::json!({"key": "value"})),
        ]);

        b.iter(|| black_box(bridge.dispatch("test_event", &test_event)));
    });

    // Agent coordination
    group.bench_function("agent_coordination", |b| {
        let mut manager = AgentManager::new("test-coordinator", "1.0.0").unwrap();
        let mut agents = Vec::new();

        // Create test agents
        for i in 0..10 {
            agents.push(AgentManager::new(&format!("agent-{}", i), "1.0.0").unwrap());
        }

        b.iter(|| {
            // Simulate agent coordination
            for agent in &agents {
                black_box(manager.coordinate_with("task", agent));
            }
        });
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(15));

    cleanup_test_environment();
    group.finish();
}

fn bench_workflow_system(c: &mut Criterion) {
    let mut group = c.benchmark_group("workflow_system");

    create_test_environment();

    // Workflow pattern execution
    let workflow_ctx = WorkflowContext {
        name: "test-workflow".to_string(),
        input: serde_json::json!({"data": [1, 2, 3]}),
        metadata: HashMap::new(),
    };

    // Sequential workflow
    group.bench_function("sequential_workflow", |b| {
        let sequence = Sequence::new(vec!["step1", "step2", "step3"], false);

        b.iter(|| black_box(sequence.execute(&workflow_ctx)));
    });

    // Parallel workflow
    group.bench_function("parallel_workflow", |b| {
        let parallel = Parallel::new(
            vec!["task1", "task2", "task3", "task4", "task5"],
            WorkflowContext::default(),
        );

        b.iter(|| black_box(parallel.execute(&workflow_ctx)));
    });

    // Workflow overhead measurement
    group.bench_function("workflow_overhead", |b| {
        b.iter(|| {
            let ctx = WorkflowContext {
                name: "benchmark".to_string(),
                input: serde_json::json!({}),
                metadata: HashMap::new(),
            };

            black_box(ctx.validate_inputs().unwrap())
        });
    });

    // Memory usage for workflow steps
    group.bench_function("workflow_memory_usage", |b| {
        let mut workflow_steps = Vec::new();

        for i in 0..100 {
            workflow_steps.push(format!("step-{}", i));
        }

        b.iter(|| {
            let parallel = Parallel::new(workflow_steps.clone(), WorkflowContext::default());

            black_box(parallel.execute(&workflow_ctx))
        });
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(15));

    cleanup_test_environment();
    group.finish();
}

fn bench_cli_commands(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_commands");

    // CLI startup performance
    group.bench_function("cli_startup", |b| {
        b.iter(|| black_box(ggen_cli::cli::build_cli()));
    });

    // Agent command processing
    group.bench_function("agent_command_processing", |b| {
        let mut args = vec!["ggen", "agent", "start", "test-agent"];

        b.iter(|| black_box(ggen_cli::cmds::agent::process_args(&mut args)));
    });

    // MCP command processing
    group.bench_function("mcp_command_processing", |b| {
        let mut args = vec!["ggen", "mcp", "server", "--host", "localhost"];

        b.iter(|| black_box(ggen_cli::cmds::mcp::process_mcp_args(&mut args)));
    });

    // Command validation overhead
    group.bench_function("command_validation", |b| {
        let test_commands = vec![
            ("valid", vec!["ggen", "sync"]),
            ("invalid", vec!["ggen", "hack"]),
            ("partial", vec!["ggen"]),
        ];

        b.iter(|| {
            for (name, args) in &test_commands {
                black_box(ggen_cli::cmds::validate_command(args).is_ok());
            }
        });
    });

    group.sample_size(100);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_connectors(c: &mut Criterion) {
    let mut group = c.benchmark_group("connectors");

    // Connector registration
    group.bench_function("connector_registration", |b| {
        let mut registry = ConnectorRegistry::new();

        b.iter(|| {
            // Simulate registering multiple connectors
            for i in 0..10 {
                let connector = Box::new(knhk_connectors::kafka::KafkaConnector::new(
                    format!("kafka-{}", i),
                    "test.topic".to_string(),
                    DataFormat::Json,
                ));
                black_box(registry.register(connector));
            }
        });
    });

    // Delta processing performance
    group.bench_function("delta_processing", |b| {
        let mut registry = ConnectorRegistry::new();

        // Register a test connector
        let connector = Box::new(knhk_connectors::kafka::KafkaConnector::new(
            "test-kafka".to_string(),
            "test.topic".to_string(),
            DataFormat::Json,
        ));
        registry.register(connector).unwrap();

        // Create test delta
        let delta = Delta {
            additions: vec![Triple {
                subject: 0xA11CE,
                predicate: 0xC0FFEE,
                object: 0xB0B,
                graph: None,
            }],
            removals: vec![],
            actor: "benchmark".to_string(),
            timestamp_ms: 0,
        };

        b.iter(|| black_box(registry.fetch_delta(&"test-kafka".to_string())));
    });

    // SoA conversion performance
    group.bench_function("soa_conversion", |b| {
        let test_triples: Vec<Triple> = (0..1000)
            .map(|i| Triple {
                subject: i,
                predicate: i + 1000,
                object: i + 2000,
                graph: None,
            })
            .collect();

        b.iter(|| black_box(SoAArrays::from_triples(&test_triples, 8)));
    });

    // Circuit breaker performance
    group.bench_function("circuit_breaker_overhead", |b| {
        let mut registry = ConnectorRegistry::new();

        let connector = Box::new(knhk_connectors::kafka::KafkaConnector::new(
            "cb-test".to_string(),
            "test.topic".to_string(),
            DataFormat::Json,
        ));
        registry.register(connector).unwrap();

        b.iter(|| black_box(registry.fetch_delta(&"cb-test".to_string())));
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(15));
    group.finish();
}

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");

    // Path validator memory
    group.bench_function("path_validator_memory", |b| {
        b.iter(|| {
            let validator = PathValidator::new(Path::new(TEST_WORKSPACE_ROOT))
                .with_max_depth(10)
                .with_allowed_extensions(vec!["tera", "ttl", "md"]);

            black_box(validator)
        });
    });

    // Command builder memory
    group.bench_function("safe_command_memory", |b| {
        b.iter(|| {
            let mut cmd = SafeCommand::new("cargo").unwrap();
            for i in 0..10 {
                cmd = cmd.arg(&format!("--flag{}", i)).unwrap();
            }
            black_box(cmd)
        });
    });

    // Workflow context memory
    group.bench_function("workflow_context_memory", |b| {
        b.iter(|| {
            let mut metadata = HashMap::new();
            for i in 0..100 {
                metadata.insert(format!("key{}", i), format!("value{}", i));
            }

            let ctx = WorkflowContext {
                name: "memory-benchmark".to_string(),
                input: serde_json::json!({"large": "data".repeat(1000)}),
                metadata: metadata.clone(),
            };
            black_box(ctx)
        });
    });

    // Connector registry memory
    group.bench_function("connector_registry_memory", |b| {
        b.iter(|| {
            let mut registry = ConnectorRegistry::new();
            for i in 0..50 {
                let connector = Box::new(knhk_connectors::kafka::KafkaConnector::new(
                    format!("mem-test-{}", i),
                    "test.topic".to_string(),
                    DataFormat::Json,
                ));
                black_box(registry.register(connector));
            }
            black_box(registry)
        });
    });

    group.sample_size(20);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_critical_path_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("critical_path_analysis");

    // Five-stage pipeline benchmarks
    group.bench_function("stage1_normalization", |b| {
        b.iter(|| {
            // Simulate RDF normalization
            let rdf_content = TEST_RDF_CONTENT.repeat(10);
            black_box(ggen_core::rdf::normalize_rdf(&rdf_content))
        });
    });

    group.bench_function("stage2_extraction", |b| {
        b.iter(|| {
            // Simulate SPARQL query execution
            let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
            let store = ggen_core::rdf::create_test_store();
            black_box(store.query(query))
        });
    });

    group.bench_function("stage3_emission", |b| {
        b.iter(|| {
            // Simulate template rendering
            let template = "Hello, {{ name }}!";
            let context = serde_json::json!({"name": "World"});
            black_box(ggen_core::templates::render_template(template, &context))
        });
    });

    group.bench_function("stage4_canonicalization", |b| {
        b.iter(|| {
            // Simulate deterministic formatting
            let content = "  Hello  World  \n  \n  ".to_string();
            black_box(ggen_core::canonicalize::format_deterministic(&content))
        });
    });

    group.bench_function("stage5_receipt", |b| {
        b.iter(|| {
            // Simulate receipt generation
            let content = "test content".repeat(100);
            black_box(ggen_core::receipts::generate_receipt(&content))
        });
    });

    // Pipeline integration
    group.bench_function("pipeline_integration", |b| {
        b.iter(|| {
            // Simulate full pipeline
            let rdf_content = TEST_RDF_CONTENT;
            let pipeline = ggen_core::pipeline::create_pipeline();

            black_box(pipeline.process_rdf(rdf_content))
        });
    });

    group.sample_size(30);
    group.measurement_time(Duration::from_secs(15));
    group.finish();
}

criterion_group!(
    benches,
    bench_path_validation,
    bench_safe_command,
    bench_agent_system,
    bench_workflow_system,
    bench_cli_commands,
    bench_connectors,
    bench_memory_usage,
    bench_critical_path_analysis
);

criterion_main!(benches);
