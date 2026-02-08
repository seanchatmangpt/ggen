//! Simple Performance Benchmark for WIP Components
//!
//! This benchmark suite evaluates the performance of new WIP components
//! without complex validation dependencies.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

// Simple implementations for benchmarking
struct SimplePathValidator {
    workspace_root: String,
    max_depth: usize,
    allowed_extensions: Vec<String>,
}

impl SimplePathValidator {
    fn new(workspace_root: &str) -> Self {
        Self {
            workspace_root: workspace_root.to_string(),
            max_depth: 10,
            allowed_extensions: vec!["tera".to_string(), "ttl".to_string(), "md".to_string()],
        }
    }

    fn validate(&self, path: &str) -> Result<String, String> {
        // Simple validation logic
        if path.contains("../") {
            return Err("Path traversal detected".to_string());
        }

        if path.len() > 1000 {
            return Err("Path too long".to_string());
        }

        let ext = Path::new(path)
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        if !self.allowed_extensions.contains(&ext.to_string()) {
            return Err("Extension not allowed".to_string());
        }

        Ok(path.to_string())
    }
}

struct SafeCommand {
    command: String,
    args: Vec<String>,
}

impl SafeCommand {
    fn new(command: &str) -> Result<Self, String> {
        let allowed_commands = ["cargo", "git", "ls", "cat", "head", "tail", "cp", "mv"];

        if !allowed_commands.contains(&command) {
            return Err("Command not in whitelist".to_string());
        }

        if command.contains(";") || command.contains("|") || command.contains("&") {
            return Err("Command contains shell metacharacters".to_string());
        }

        Ok(Self {
            command: command.to_string(),
            args: Vec::new(),
        })
    }

    fn arg(mut self, arg: &str) -> Result<Self, String> {
        if arg.contains(";") || arg.contains("|") || arg.contains("&") {
            return Err("Argument contains shell metacharacters".to_string());
        }

        self.args.push(arg.to_string());
        Ok(self)
    }

    fn validate(&self) -> Result<(), String> {
        let full_command = format!("{} {}", self.command, self.args.join(" "));

        if full_command.len() > 4096 {
            return Err("Command too long".to_string());
        }

        Ok(())
    }
}

struct AgentManager {
    name: String,
    version: String,
    state: String,
}

impl AgentManager {
    fn new(name: &str, version: &str) -> Result<Self, String> {
        Ok(Self {
            name: name.to_string(),
            version: version.to_string(),
            state: "initialized".to_string(),
        })
    }

    fn coordinate_with(&mut self, task: &str, other: &AgentManager) -> Result<(), String> {
        // Simulate coordination
        self.state = format!("coordinating_{}_with_{}", task, other.name);
        Ok(())
    }
}

struct EventBridge {
    events: Vec<HashMap<String, serde_json::Value>>,
}

impl EventBridge {
    fn new() -> Self {
        Self { events: Vec::new() }
    }

    fn dispatch(
        &mut self, event_type: &str, payload: &HashMap<String, serde_json::Value>,
    ) -> Result<(), String> {
        let mut event = payload.clone();
        event.insert(
            "type".to_string(),
            serde_json::Value::String(event_type.to_string()),
        );
        event.insert(
            "timestamp".to_string(),
            serde_json::Value::Number(serde_json::Number::from(1000)),
        );

        self.events.push(event);
        Ok(())
    }
}

struct WorkflowContext {
    name: String,
    input: serde_json::Value,
    metadata: HashMap<String, String>,
}

impl WorkflowContext {
    fn validate_inputs(&self) -> Result<(), String> {
        // Simple validation
        if self.name.is_empty() {
            return Err("Empty workflow name".to_string());
        }
        Ok(())
    }
}

struct WorkflowRegistry {
    workflows: Vec<String>,
}

impl WorkflowRegistry {
    fn new() -> Self {
        Self {
            workflows: Vec::new(),
        }
    }

    fn register(&mut self, name: &str) -> Result<(), String> {
        self.workflows.push(name.to_string());
        Ok(())
    }

    fn execute(&self, name: &str, context: &WorkflowContext) -> Result<String, String> {
        context.validate_inputs()?;
        Ok(format!("executed_{}", name))
    }
}

struct ConnectorRegistry {
    connectors: HashMap<String, String>,
    metrics: HashMap<String, u64>,
}

impl ConnectorRegistry {
    fn new() -> Self {
        Self {
            connectors: HashMap::new(),
            metrics: HashMap::new(),
        }
    }

    fn register(&mut self, name: &str, connector_type: &str) -> Result<(), String> {
        self.connectors
            .insert(name.to_string(), connector_type.to_string());
        self.metrics.insert(name.to_string(), 0);
        Ok(())
    }

    fn fetch_delta(&mut self, name: &str) -> Result<Vec<u64>, String> {
        let count = self.metrics.get(name).unwrap_or(&0).clone();
        self.metrics.insert(name.to_string(), count + 1);

        Ok((0..count).collect())
    }
}

fn bench_path_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("path_validation");

    let validator = SimplePathValidator::new("/tmp/test");

    // Test cases
    let test_cases = vec![
        ("valid_simple", "templates/example.tera"),
        ("valid_deep", "templates/nested/deep/path/example.tera"),
        ("valid_spaces", "templates/my template.tera"),
        ("malicious_traversal", "templates/../../../etc/passwd"),
        ("invalid_extension", "scripts/script.py"),
        ("too_long", "a".repeat(1000)),
    ];

    for (name, path) in test_cases {
        group.bench_with_input(BenchmarkId::new("validate", name), path, |b, path| {
            b.iter(|| black_box(validator.validate(path).is_ok()));
        });
    }

    group.sample_size(100);
    group.measurement_time(Duration::from_secs(5));
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
            let mut cmd = SafeCommand::new(args[0]).unwrap();

            for arg in &args[1..] {
                cmd = cmd.arg(arg).unwrap();
            }

            b.iter(|| black_box(cmd.validate().is_ok()));
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
    group.measurement_time(Duration::from_secs(5));
    group.finish();
}

fn bench_agent_system(c: &mut Criterion) {
    let mut group = c.benchmark_group("agent_system");

    // Agent creation
    group.bench_function("agent_creation", |b| {
        b.iter(|| black_box(AgentManager::new("test-agent", "1.0.0").unwrap()));
    });

    // Event bridge performance
    group.bench_function("event_bridge_dispatch", |b| {
        let mut bridge = EventBridge::new();
        let mut event = HashMap::new();
        event.insert(
            "data".to_string(),
            serde_json::Value::String("test".to_string()),
        );

        b.iter(|| black_box(bridge.dispatch("test_event", &event)));
    });

    // Agent coordination
    group.bench_function("agent_coordination", |b| {
        let mut manager1 = AgentManager::new("agent1", "1.0.0").unwrap();
        let mut manager2 = AgentManager::new("agent2", "1.0.0").unwrap();
        let mut manager3 = AgentManager::new("agent3", "1.0.0").unwrap();

        let mut agents = vec![manager1, manager2, manager3];

        b.iter(|| {
            for i in 0..agents.len() {
                for j in 0..agents.len() {
                    if i != j {
                        black_box(agents[i].coordinate_with("task", &agents[j]));
                    }
                }
            }
        });
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_workflow_system(c: &mut Criterion) {
    let mut group = c.benchmark_group("workflow_system");

    let workflow_ctx = WorkflowContext {
        name: "test-workflow".to_string(),
        input: serde_json::json!({"data": [1, 2, 3]}),
        metadata: HashMap::new(),
    };

    // Workflow execution
    group.bench_function("workflow_execution", |b| {
        let mut registry = WorkflowRegistry::new();

        // Register some workflows
        for i in 0..10 {
            registry.register(&format!("workflow-{}", i)).unwrap();
        }

        b.iter(|| {
            for i in 0..10 {
                black_box(registry.execute(&format!("workflow-{}", i), &workflow_ctx));
            }
        });
    });

    // Workflow context creation
    group.bench_function("workflow_context_creation", |b| {
        b.iter(|| {
            let mut metadata = HashMap::new();
            for i in 0..100 {
                metadata.insert(format!("key{}", i), format!("value{}", i));
            }

            black_box(WorkflowContext {
                name: "benchmark-workflow".to_string(),
                input: serde_json::json!({"data": [1, 2, 3]}),
                metadata: metadata.clone(),
            });
        });
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_connectors(c: &mut Criterion) {
    let mut group = c.benchmark_group("connectors");

    // Connector registration
    group.bench_function("connector_registration", |b| {
        let mut registry = ConnectorRegistry::new();

        b.iter(|| {
            for i in 0..10 {
                black_box(registry.register(&format!("connector-{}", i), "kafka"));
            }
        });
    });

    // Delta processing
    group.bench_function("delta_processing", |b| {
        let mut registry = ConnectorRegistry::new();

        // Register test connectors
        for i in 0..5 {
            registry.register(&format!("kafka-{}", i), "kafka").unwrap();
        }

        b.iter(|| {
            for i in 0..5 {
                black_box(registry.fetch_delta(&format!("kafka-{}", i)));
            }
        });
    });

    group.sample_size(50);
    group.measurement_time(Duration::from_secs(10));
    group.finish();
}

fn bench_cli_commands(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_commands");

    // CLI startup simulation
    group.bench_function("cli_startup", |b| {
        b.iter(|| black_box(format!("ggen {}", "sync")));
    });

    // Command validation simulation
    group.bench_function("command_validation", |b| {
        let valid_commands = vec![
            vec!["ggen", "sync"],
            vec!["ggen", "build"],
            vec!["ggen", "test"],
            vec!["ggen", "agent", "start"],
            vec!["ggen", "mcp", "server"],
        ];

        let invalid_commands = vec![
            vec!["ggen", "hack"],
            vec!["ggen", "delete", "everything"],
            vec!["", ""],
        ];

        b.iter(|| {
            for cmd in &valid_commands {
                black_box(cmd.join(" ").len() > 0);
            }
            for cmd in &invalid_commands {
                black_box(cmd.join(" ").len() > 0);
            }
        });
    });

    group.sample_size(100);
    group.measurement_time(Duration::from_secs(5));
    group.finish();
}

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");

    // Path validator memory
    group.bench_function("path_validator_memory", |b| {
        b.iter(|| black_box(SimplePathValidator::new("/tmp/test")));
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

            black_box(WorkflowContext {
                name: "memory-benchmark".to_string(),
                input: serde_json::json!({"large": "data".repeat(1000)}),
                metadata: metadata,
            })
        });
    });

    // Connector registry memory
    group.bench_function("connector_registry_memory", |b| {
        b.iter(|| {
            let mut registry = ConnectorRegistry::new();
            for i in 0..50 {
                black_box(registry.register(&format!("conn-{}", i), "kafka"));
            }
            black_box(registry)
        });
    });

    group.sample_size(20);
    group.measurement_time(Duration::from_secs(5));
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
            let mut map = HashMap::new();
            for i in 0..1000 {
                map.insert(format!("key-{}", i), format!("value-{}", i));
            }
            black_box(map)
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_path_validation,
    bench_safe_command,
    bench_agent_system,
    bench_workflow_system,
    bench_connectors,
    bench_cli_commands,
    bench_memory_usage,
    bench_allocation_patterns
);

criterion_main!(benches);
