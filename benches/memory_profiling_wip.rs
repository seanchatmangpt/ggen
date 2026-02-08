//! Memory Profiling for WIP Components
//!
//! This benchmark focuses on memory usage patterns and allocation efficiency
//! for the new WIP components.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::alloc::GlobalAlloc;
use std::sync::Arc;
use std::collections::HashMap;

// Enable memory profiling
#[global_allocator]
static ALLOC: tracing::allocation::TrackingAllocator = tracing::allocation::TrackingAllocator::new();

fn bench_path_validator_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_path_validator");

    group.bench_function("validator_creation", |b| {
        b.iter(|| {
            black_box(
                ggen_utils::path_validator::PathValidator::new(std::path::Path::new("/tmp/test"))
                    .with_max_depth(10)
                    .with_allowed_extensions(vec!["rs", "toml", "md"])
            )
        });
    });

    group.bench_function("validation_calls", |b| {
        let validator = ggen_utils::path_validator::PathValidator::new(std::path::Path::new("/tmp/test"))
            .with_max_depth(10)
            .with_allowed_extensions(vec!["rs", "toml", "md"]);

        let test_paths = vec![
            "src/main.rs",
            "Cargo.toml",
            "README.md",
            "docs/api.md",
            "tests/test.rs",
        ];

        b.iter(|| {
            for path in &test_paths {
                black_box(
                    validator.validate(path).unwrap_or_else(|_| panic!("Failed to validate: {}", path))
                );
            }
        });
    });

    group.finish();
}

fn bench_safe_command_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_safe_command");

    group.bench_function("command_creation", |b| {
        b.iter(|| {
            black_box(
                ggen_utils::safe_command::SafeCommand::new("cargo").unwrap()
            )
        });
    });

    group.bench_function("command_building", |b| {
        let mut cmd = ggen_utils::safe_command::SafeCommand::new("cargo").unwrap();

        b.iter(|| {
            for i in 0..20 {
                cmd = cmd.arg(&format!("--flag{}", i)).unwrap();
            }
            black_box(cmd.validate())
        });
    });

    group.bench_function("whitelist_memory", |b| {
        b.iter(|| {
            let whitelist = vec![
                "cargo", "rustc", "git", "ls", "cat", "head", "tail",
                "cp", "mv", "mkdir", "rm", "rmdir", "chmod", "chown",
            ];

            let allowed_commands: HashMap<String, bool> = whitelist.iter()
                .map(|s| (s.to_string(), true))
                .collect();

            black_box(allowed_commands)
        });
    });

    group.finish();
}

fn bench_agent_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_agent");

    group.bench_function("agent_manager_creation", |b| {
        b.iter(|| {
            black_box(
                ggen_agent::agent::AgentManager::new("test-agent", "1.0.0").unwrap()
            )
        });
    });

    group.bench_function("agent_registry_memory", |b| {
        b.iter(|| {
            let registry = ggen_agent::agent::AgentRegistry::new();

            for i in 0..100 {
                let agent = ggen_agent::agent::AgentManager::new(
                    &format!("agent-{}", i),
                    "1.0.0"
                ).unwrap();

                black_box(
                    registry.register(Arc::new(agent))
                );
            }
        });
    });

    group.bench_function("event_bridge_memory", |b| {
        b.iter(|| {
            let bridge = ggen_agent::bridge::EventBridge::new();
            let mut events = Vec::new();

            for i in 0..1000 {
                events.push(ggen_agent::bridge::Event {
                    id: format!("event-{}", i),
                    typ: "test".to_string(),
                    payload: serde_json::json!({"data": i}),
                    timestamp: std::time::SystemTime::now(),
                });
            }

            black_box(events)
        });
    });

    group.finish();
}

fn bench_workflow_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_workflow");

    group.bench_function("workflow_context_memory", |b| {
        b.iter(|| {
            let mut metadata = HashMap::new();
            for i in 0..1000 {
                metadata.insert(format!("key{}", i), format!("value{}", i));
            }

            let context = ggen_workflow::patterns::WorkflowContext {
                name: "large-workflow".to_string(),
                input: serde_json::json!({
                    "data": vec![1, 2, 3, 4, 5],
                    "config": metadata
                }),
                metadata: metadata,
            };

            black_box(context)
        });
    });

    group.bench_function("workflow_pattern_memory", |b| {
        b.iter(|| {
            let steps: Vec<String> = (0..1000).map(|i| format!("step-{}", i)).collect();
            let workflow = ggen_workflow::patterns::Sequence::new(steps, false);

            black_box(workflow)
        });
    });

    group.bench_function("receipt_memory", |b| {
        b.iter(|| {
            let receipt = ggen_workflow::receipts::WorkflowReceipt {
                workflow_id: "benchmark".to_string(),
                input_hash: "hash123".to_string(),
                output_hash: "hash456".to_string(),
                execution_time_ms: 1000,
                steps: vec![
                    ggen_workflow::receipts::StepReceipt {
                        name: "step1".to_string(),
                        input_size: 1024,
                        output_size: 2048,
                        execution_time_ms: 500,
                        status: "completed".to_string(),
                    }
                ],
                metadata: HashMap::new(),
            };

            black_box(receipt)
        });
    });

    group.finish();
}

fn bench_connector_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_connector");

    group.bench_function("connector_registry_memory", |b| {
        b.iter(|| {
            let registry = knhk_connectors::ConnectorRegistry::new();

            for i in 0..50 {
                let connector = Box::new(
                    knhk_connectors::kafka::KafkaConnector::new(
                        format!("kafka-{}", i),
                        "test.topic".to_string(),
                        knhk_connectors::DataFormat::Json,
                    )
                );

                black_box(
                    registry.register(connector)
                );
            }
        });
    });

    group.bench_function("delta_memory", |b| {
        b.iter(|| {
            let delta = knhk_connectors::Delta {
                additions: (0..1000).map(|i| knhk_connectors::Triple {
                    subject: i,
                    predicate: i + 1000,
                    object: i + 2000,
                    graph: None,
                }).collect(),
                removals: (1000..2000).map(|i| knhk_connectors::Triple {
                    subject: i,
                    predicate: i + 1000,
                    object: i + 2000,
                    graph: None,
                }).collect(),
                actor: "benchmark".to_string(),
                timestamp_ms: 0,
            };

            black_box(delta)
        });
    });

    group.bench_function("soa_arrays_memory", |b| {
        b.iter(|| {
            let triples: Vec<knhk_connectors::Triple> = (0..1000).map(|i| knhk_connectors::Triple {
                subject: i,
                predicate: i + 1000,
                object: i + 2000,
                graph: None,
            }).collect();

            black_box(
                knhk_connectors::SoAArrays::from_triples(&triples, 1000)
            )
        });
    });

    group.bench_function("circuit_breaker_memory", |b| {
        b.iter(|| {
            let mut breakers = Vec::new();

            for i in 0..100 {
                breakers.push(knhk_connectors::CircuitBreaker::new(5, 60000));
            }

            black_box(breakers)
        });
    });

    group.finish();
}

fn bench_cli_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_cli");

    group.bench_function("cli_app_memory", |b| {
        b.iter(|| {
            let app = ggen_cli::cli::build_cli();
            black_box(app)
        });
    });

    group.bench_function("command_definitions_memory", |b| {
        b.iter(|| {
            let commands = vec![
                ggen_cli::cmds::agent::AgentCommand::Start,
                ggen_cli::cmds::agent::AgentCommand::Stop,
                ggen_cli::cmds::agent::AgentCommand::Status,
                ggen_cli::cmds::agent::AgentCommand::List,
                ggen_cli::cmds::mcp::McpCommand::Server,
                ggen_cli::cmds::mcp::McpCommand::Client,
            ];

            let command_configs = commands.into_iter().map(|cmd| {
                ggen_cli::cmds::CommandConfig {
                    name: format!("{:?}", cmd),
                    description: format!("Description for {:?}", cmd),
                    flags: vec![],
                    subcommands: vec![],
                }
            }).collect::<Vec<_>>();

            black_box(command_configs)
        });
    });

    group.finish();
}

fn bench_allocation_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("allocation_patterns");

    group.bench_function("string_allocations", |b| {
        b.iter(|| {
            let mut strings = Vec::new();
            for i in 0..1000 {
                strings.push(format!("benchmark-string-{}", i));
            }
            black_box(strings)
        });
    });

    group.bench_function("json_allocations", |b| {
        b.iter(|| {
            let mut objects = Vec::new();
            for i in 0..1000 {
                objects.push(serde_json::json!({
                    "id": i,
                    "name": format!("object-{}", i),
                    "value": i * 2,
                    "active": i % 2 == 0,
                }));
            }
            black_box(objects)
        });
    });

    group.bench_function("hashmap_allocations", |b| {
        b.iter(|| {
            let mut maps = Vec::new();
            for i in 0..100 {
                let mut map = HashMap::new();
                for j in 0..100 {
                    map.insert(format!("key-{}-{}", i, j), format!("value-{}-{}", i, j));
                }
                maps.push(map);
            }
            black_box(maps)
        });
    });

    group.bench_function("vector_allocations", |b| {
        b.iter(|| {
            let mut vectors = Vec::new();
            for i in 0..100 {
                let mut vec = Vec::new();
                for j in 0..1000 {
                    vec.push(format!("element-{}-{}", i, j));
                }
                vectors.push(vec);
            }
            black_box(vectors)
        });
    });

    group.finish();
}

fn bench_memory_growth(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_growth");

    // Test memory growth over iterations
    group.bench_function("cumulative_allocation_growth", |b| {
        b.iter(|| {
            let mut cumulative = Vec::new();

            // Simulate repeated allocations
            for _ in 0..100 {
                let batch = (0..100).map(|i| format!("item-{}", i)).collect::<Vec<_>>();
                cumulative.extend(batch);
            }

            black_box(cumulative)
        });
    });

    group.bench_function("repeated_operation_memory", |b| {
        let mut data = Vec::new();

        b.iter(|| {
            // Clear and re-allocate
            data.clear();
            data.extend(0..1000);

            // Process data
            let processed: Vec<u64> = data.iter().map(|x| x * 2).collect();

            black_box(processed)
        });
    });

    group.finish();
}

criterion_group!(
    memory_benches,
    bench_path_validator_memory,
    bench_safe_command_memory,
    bench_agent_memory,
    bench_workflow_memory,
    bench_connector_memory,
    bench_cli_memory,
    bench_allocation_patterns,
    bench_memory_growth
);

criterion_main!(memory_benches);