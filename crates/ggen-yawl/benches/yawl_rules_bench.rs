//! YAWL Rule Generation Performance Benchmarks (Rules 3-10)
//!
//! Benchmarks the performance of YAWL rule generation patterns:
//! - Rule 3: Class→Task (entities)
//! - Rule 4: Property→Flow (relationships/repositories)
//! - Rule 5: Cardinality Split/Join (DTOs)
//! - Rule 6: Rules→Conditions (controllers)
//! - Rule 7: Multiple Instance patterns (enums)
//! - Rule 8: Composite Task patterns (services)
//! - Rule 9: Message passing patterns (HBM)
//! - Rule 10: Data validation patterns (serializers)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_yawl::template::{
    ConditionContext, FlowContext, TaskContext, TemplateContext, VariableContext,
};
use ggen_yawl::TemplateRenderer;

/// Generate synthetic template context for Rule 3 (entities/tasks)
/// N = number of entities
fn generate_rule3_entities(n: usize) -> TemplateContext {
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

/// Generate synthetic template context for Rule 4 (repositories/flows)
/// N = number of flows
fn generate_rule4_repositories(n: usize) -> TemplateContext {
    let mut tasks = vec![TaskContext {
        id: "start".to_string(),
        name: "Start".to_string(),
        split_type: "AND".to_string(),
        join_type: "AND".to_string(),
        is_auto: true,
        decomposition_id: None,
    }];

    // Add intermediate tasks
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

    // Create flows between tasks
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

/// Generate synthetic template context for Rule 5 (DTOs with cardinality)
/// N = number of DTOs with split/join patterns
fn generate_rule5_dtos(n: usize) -> TemplateContext {
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

/// Generate synthetic template context for Rule 6 (controllers with conditions)
/// N = number of controllers with conditional routing
fn generate_rule6_controllers(n: usize) -> TemplateContext {
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
            Some(
                conditions
                    .get(0)
                    .cloned()
                    .unwrap_or_else(|| ConditionContext {
                        id: "input".to_string(),
                        expression: "true".to_string(),
                        condition_type: "system".to_string(),
                    }),
            )
        } else {
            None
        },
        output_condition: if n > 0 {
            Some(
                conditions
                    .get(n - 1)
                    .cloned()
                    .unwrap_or_else(|| ConditionContext {
                        id: "output".to_string(),
                        expression: "true".to_string(),
                        condition_type: "system".to_string(),
                    }),
            )
        } else {
            None
        },
        variables: Vec::new(),
    }
}

/// Generate synthetic template context for Rule 7 (enums/multiple instances)
/// N = number of enum variants with MI patterns
fn generate_rule7_enums(n: usize) -> TemplateContext {
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

/// Generate synthetic template context for Rule 8 (services/composite tasks)
/// N = number of composite services
fn generate_rule8_services(n: usize) -> TemplateContext {
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

/// Generate synthetic template context for Rule 9 (HBM/message patterns)
/// N = number of message-passing tasks
fn generate_rule9_hbm(n: usize) -> TemplateContext {
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

/// Generate synthetic template context for Rule 10 (serializers/data validation)
/// N = number of validation tasks
fn generate_rule10_serializers(n: usize) -> TemplateContext {
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

/// Benchmark Rule 3: Entity/Class to Task transformation
fn bench_rule3_entities(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule3_entities");

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule3_entities(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule4_repositories(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule5_dtos(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule6_controllers(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule7_enums(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule8_services(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }

    group.finish();
}

/// Benchmark Rule 9: Message/HBM pattern
fn bench_rule9_hbm(c: &mut Criterion) {
    let mut group = c.benchmark_group("rule9_hbm");

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule9_hbm(n));

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

    for n in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();
            let context = black_box(generate_rule10_serializers(n));

            b.iter(|| {
                let _ = renderer.render_yawl_xml(black_box(&context));
            });
        });
    }

    group.finish();
}

/// Benchmark all rules combined (end-to-end)
fn bench_all_rules_combined(c: &mut Criterion) {
    let mut group = c.benchmark_group("all_rules_combined");

    for n in [10, 50, 100].iter() {
        group.throughput(Throughput::Elements((n * 8) as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            let renderer = TemplateRenderer::new();

            b.iter(|| {
                let contexts = vec![
                    black_box(generate_rule3_entities(n)),
                    black_box(generate_rule4_repositories(n)),
                    black_box(generate_rule5_dtos(n)),
                    black_box(generate_rule6_controllers(n)),
                    black_box(generate_rule7_enums(n)),
                    black_box(generate_rule8_services(n)),
                    black_box(generate_rule9_hbm(n)),
                    black_box(generate_rule10_serializers(n)),
                ];

                for ctx in contexts {
                    let _ = renderer.render_yawl_xml(&ctx);
                }
            });
        });
    }

    group.finish();
}

/// Benchmark memory allocation patterns
fn bench_context_allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("context_allocation");

    for n in [100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*n as u64));
        group.bench_with_input(BenchmarkId::new("rule3_allocation", n), n, |b, &n| {
            b.iter(|| {
                let context = black_box(generate_rule3_entities(n));
                black_box(context);
            });
        });

        group.bench_with_input(BenchmarkId::new("rule4_allocation", n), n, |b, &n| {
            b.iter(|| {
                let context = black_box(generate_rule4_repositories(n));
                black_box(context);
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_rule3_entities,
    bench_rule4_repositories,
    bench_rule5_dtos,
    bench_rule6_controllers,
    bench_rule7_enums,
    bench_rule8_services,
    bench_rule9_hbm,
    bench_rule10_serializers,
    bench_all_rules_combined,
    bench_context_allocation
);
criterion_main!(benches);
