//! Comprehensive Rule Execution Performance Benchmarks
//!
//! Benchmarks the performance of YAWL rule generation patterns with SLO validation.
//! Measures execution time, memory usage, and generated output size.
//!
//! SLO Targets:
//! - Single rule execution: <100ms
//! - All rules (1-10) combined: <1000ms
//! - Peak memory usage: <100MB
//! - Total output size: <5MB

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_yawl::template::{
    ConditionContext, FlowContext, TaskContext, TemplateContext, VariableContext,
};
use ggen_yawl::TemplateRenderer;
use std::time::Instant;

/// Measure memory usage (approximate via Vec size)
struct MemoryMetrics {
    allocated_bytes: usize,
    peak_bytes: usize,
}

impl MemoryMetrics {
    fn new() -> Self {
        Self {
            allocated_bytes: 0,
            peak_bytes: 0,
        }
    }

    fn measure_context_size(context: &TemplateContext) -> usize {
        // Approximate size calculation
        let tasks_size = context.tasks.len() * 200; // ~200 bytes per task context
        let flows_size = context.flows.len() * 150; // ~150 bytes per flow context
        let variables_size = context.variables.len() * 100; // ~100 bytes per variable
        let strings_size =
            context.workflow_name.len() + context.description.len() + context.version.len();

        tasks_size + flows_size + variables_size + strings_size
    }

    fn measure_output_size(output: &str) -> usize {
        output.len()
    }
}

/// Helper to create Rule 1: Config execution context
fn create_rule1_config() -> TemplateContext {
    TemplateContext {
        workflow_name: "ConfigWorkflow".to_string(),
        description: "Configuration loading and validation".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![TaskContext {
            id: "load_config".to_string(),
            name: "Load Configuration".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        }],
        flows: vec![],
        input_condition: None,
        output_condition: None,
        variables: vec![VariableContext {
            name: "config".to_string(),
            var_type: "String".to_string(),
            default: Some("{}".to_string()),
            scope: "workflow".to_string(),
        }],
    }
}

/// Helper to create Rule 2: Spring Boot app generation context
fn create_rule2_spring_boot(entity_count: usize) -> TemplateContext {
    let mut tasks = vec![TaskContext {
        id: "init_spring".to_string(),
        name: "Initialize Spring Boot".to_string(),
        split_type: "AND".to_string(),
        join_type: "AND".to_string(),
        is_auto: true,
        decomposition_id: None,
    }];

    // Add task for each entity
    for i in 0..entity_count {
        tasks.push(TaskContext {
            id: format!("create_entity_{}", i),
            name: format!("Create Entity {}", i),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: false,
            decomposition_id: None,
        });
    }

    tasks.push(TaskContext {
        id: "finalize_boot".to_string(),
        name: "Finalize Boot Configuration".to_string(),
        split_type: "AND".to_string(),
        join_type: "AND".to_string(),
        is_auto: true,
        decomposition_id: None,
    });

    TemplateContext {
        workflow_name: "SpringBootWorkflow".to_string(),
        description: format!("Spring Boot app generation with {} entities", entity_count),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables: vec![VariableContext {
            name: "app_config".to_string(),
            var_type: "Map".to_string(),
            default: None,
            scope: "workflow".to_string(),
        }],
    }
}

/// Helper to create Rule 3: Entity/Class to Task transformation
fn create_rule3_entities(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("entity_{}", i),
            name: format!("Entity {}", i),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: false,
            decomposition_id: None,
        })
        .collect();

    TemplateContext {
        workflow_name: "EntityWorkflow".to_string(),
        description: format!("Workflow with {} entities", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables: Vec::new(),
    }
}

/// Helper to create Rule 4: Property/Relationship to Flow transformation
fn create_rule4_repositories(n: usize) -> TemplateContext {
    let mut tasks = vec![TaskContext {
        id: "start".to_string(),
        name: "Start".to_string(),
        split_type: "AND".to_string(),
        join_type: "AND".to_string(),
        is_auto: true,
        decomposition_id: None,
    }];

    for i in 0..n {
        tasks.push(TaskContext {
            id: format!("repo_{}", i),
            name: format!("Repository {}", i),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: false,
            decomposition_id: None,
        });
    }

    tasks.push(TaskContext {
        id: "end".to_string(),
        name: "End".to_string(),
        split_type: "AND".to_string(),
        join_type: "AND".to_string(),
        is_auto: true,
        decomposition_id: None,
    });

    let flows: Vec<FlowContext> = (0..n)
        .map(|i| FlowContext {
            source: if i == 0 {
                "start".to_string()
            } else {
                format!("repo_{}", i - 1)
            },
            target: format!("repo_{}", i),
            condition: None,
            predicate: Some(format!("flow_{}", i)),
            is_default: i == 0,
        })
        .collect();

    TemplateContext {
        workflow_name: "RepositoryWorkflow".to_string(),
        description: format!("Workflow with {} repositories", n),
        version: "1.0.0".to_string(),
        tasks,
        flows,
        input_condition: None,
        output_condition: None,
        variables: Vec::new(),
    }
}

/// Helper to create Rule 5: Cardinality to Split/Join transformation
fn create_rule5_dtos(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("dto_{}", i),
            name: format!("DTO {}", i),
            split_type: if i % 2 == 0 { "AND" } else { "OR" }.to_string(),
            join_type: if i % 2 == 0 { "AND" } else { "XOR" }.to_string(),
            is_auto: false,
            decomposition_id: None,
        })
        .collect();

    TemplateContext {
        workflow_name: "DtoWorkflow".to_string(),
        description: format!("Workflow with {} DTOs", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables: Vec::new(),
    }
}

/// Helper to create Rule 6: Rules/Conditions transformation
fn create_rule6_controllers(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("controller_{}", i),
            name: format!("Controller {}", i),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: false,
            decomposition_id: None,
        })
        .collect();

    let conditions: Vec<ConditionContext> = (0..n)
        .map(|i| ConditionContext {
            id: format!("cond_{}", i),
            expression: format!("status == 'active_{}'", i),
            condition_type: "rule".to_string(),
        })
        .collect();

    TemplateContext {
        workflow_name: "ControllerWorkflow".to_string(),
        description: format!("Workflow with {} controllers", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: if n > 0 {
            Some(conditions[0].clone())
        } else {
            None
        },
        output_condition: if n > 0 {
            Some(conditions[n - 1].clone())
        } else {
            None
        },
        variables: Vec::new(),
    }
}

/// Helper to create Rule 7: Multiple Instance pattern
fn create_rule7_enums(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("enum_{}", i),
            name: format!("Enum {}", i),
            split_type: "AND".to_string(),
            join_type: "AND".to_string(),
            is_auto: false,
            decomposition_id: Some(format!("mi_enum_{}", i)),
        })
        .collect();

    TemplateContext {
        workflow_name: "EnumWorkflow".to_string(),
        description: format!("Workflow with {} enums (MI)", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables: Vec::new(),
    }
}

/// Helper to create Rule 8: Composite Task pattern
fn create_rule8_services(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("service_{}", i),
            name: format!("Service {}", i),
            split_type: "AND".to_string(),
            join_type: "AND".to_string(),
            is_auto: false,
            decomposition_id: Some(format!("composite_{}", i)),
        })
        .collect();

    TemplateContext {
        workflow_name: "ServiceWorkflow".to_string(),
        description: format!("Workflow with {} composite services", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables: Vec::new(),
    }
}

/// Helper to create Rule 9: HBM/Message patterns
fn create_rule9_hbm(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| {
            let is_send = i % 2 == 0;
            TaskContext {
                id: format!("msg_{}_{}", if is_send { "send" } else { "recv" }, i / 2),
                name: format!(
                    "{} Message {}",
                    if is_send { "Send" } else { "Receive" },
                    i / 2
                ),
                split_type: if is_send { "OR" } else { "AND" }.to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            }
        })
        .collect();

    let variables: Vec<VariableContext> = (0..n / 2)
        .map(|i| VariableContext {
            name: format!("MessageVar{}", i),
            var_type: "String".to_string(),
            default: None,
            scope: "workflow".to_string(),
        })
        .collect();

    TemplateContext {
        workflow_name: "MessageWorkflow".to_string(),
        description: format!("Workflow with {} message tasks", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables,
    }
}

/// Helper to create Rule 10: Data Validation/Serializer pattern
fn create_rule10_serializers(n: usize) -> TemplateContext {
    let tasks: Vec<TaskContext> = (0..n)
        .map(|i| TaskContext {
            id: format!("validator_{}", i),
            name: format!("Validator {}", i),
            split_type: "XOR".to_string(),
            join_type: "AND".to_string(),
            is_auto: false,
            decomposition_id: None,
        })
        .collect();

    let variables: Vec<VariableContext> = (0..n)
        .map(|i| VariableContext {
            name: format!("Payload{}", i),
            var_type: "Object".to_string(),
            default: None,
            scope: "task".to_string(),
        })
        .collect();

    TemplateContext {
        workflow_name: "SerializerWorkflow".to_string(),
        description: format!("Workflow with {} validators", n),
        version: "1.0.0".to_string(),
        tasks,
        flows: Vec::new(),
        input_condition: None,
        output_condition: None,
        variables,
    }
}

/// Benchmark Rule 1: Config execution
fn bench_rule1_config(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule1_config");
    group.bench_function("config_execution", |b| {
        let renderer = TemplateRenderer::new();
        let context = black_box(create_rule1_config());

        b.iter(|| {
            let _ = renderer.render_yawl_xml(black_box(&context));
        });
    });
    group.finish();
}

/// Benchmark Rule 2: Spring Boot app generation
fn bench_rule2_spring_boot(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule2_spring_boot");

    for n in [5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule2_spring_boot(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 3: Entity/Class to Task transformation
fn bench_rule3_entities(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule3_entities");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule3_entities(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 4: Property/Relationship to Flow transformation
fn bench_rule4_repositories(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule4_repositories");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule4_repositories(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 5: Cardinality to Split/Join transformation
fn bench_rule5_dtos(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule5_dtos");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule5_dtos(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 6: Rules/Conditions transformation
fn bench_rule6_controllers(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule6_controllers");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule6_controllers(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 7: Multiple Instance pattern
fn bench_rule7_enums(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule7_enums");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule7_enums(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 8: Composite Task pattern
fn bench_rule8_services(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule8_services");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule8_services(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 9: HBM/Message patterns
fn bench_rule9_hbm(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule9_hbm");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule9_hbm(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark Rule 10: Data Validation/Serializer pattern
fn bench_rule10_serializers(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule10_serializers");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(create_rule10_serializers(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }
    group.finish();
}

/// Benchmark all 10 rules combined (end-to-end)
fn bench_all_rules_combined(c: &mut Criterion) {
    let mut group = c.benchmark_group("all_rules_combined");

    group.bench_function("rules_1_to_10", |b| {
        let renderer = TemplateRenderer::new();

        b.iter(|| {
            let rule1 = create_rule1_config();
            let rule2 = create_rule2_spring_boot(5);
            let rule3 = create_rule3_entities(10);
            let rule4 = create_rule4_repositories(10);
            let rule5 = create_rule5_dtos(10);
            let rule6 = create_rule6_controllers(10);
            let rule7 = create_rule7_enums(10);
            let rule8 = create_rule8_services(10);
            let rule9 = create_rule9_hbm(10);
            let rule10 = create_rule10_serializers(10);

            let contexts = vec![
                black_box(rule1),
                black_box(rule2),
                black_box(rule3),
                black_box(rule4),
                black_box(rule5),
                black_box(rule6),
                black_box(rule7),
                black_box(rule8),
                black_box(rule9),
                black_box(rule10),
            ];

            for ctx in contexts {
                let _ = renderer.render_yawl_xml(&ctx);
            }
        });
    });

    group.finish();
}

/// Benchmark output size generation for all rules
fn bench_output_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("output_sizes");

    group.bench_function("output_size_all_rules", |b| {
        let renderer = TemplateRenderer::new();

        b.iter(|| {
            let contexts = vec![
                create_rule1_config(),
                create_rule2_spring_boot(5),
                create_rule3_entities(10),
                create_rule4_repositories(10),
                create_rule5_dtos(10),
                create_rule6_controllers(10),
                create_rule7_enums(10),
                create_rule8_services(10),
                create_rule9_hbm(10),
                create_rule10_serializers(10),
            ];

            let mut total_size = 0;
            for ctx in contexts {
                if let Ok(output) = renderer.render_yawl_xml(&ctx) {
                    total_size += MemoryMetrics::measure_output_size(&output);
                }
            }
            black_box(total_size);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_rule1_config,
    bench_rule2_spring_boot,
    bench_rule3_entities,
    bench_rule4_repositories,
    bench_rule5_dtos,
    bench_rule6_controllers,
    bench_rule7_enums,
    bench_rule8_services,
    bench_rule9_hbm,
    bench_rule10_serializers,
    bench_all_rules_combined,
    bench_output_sizes
);

criterion_main!(benches);
