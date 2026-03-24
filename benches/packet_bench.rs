use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_packet::validation::validate_work_order;
use ggen_packet::{
    AcceptanceCriterion, AcceptanceTest, Constraint, ConstraintType, Priority, ReversibilityPolicy,
    TestType, WorkOrder, WorkOrderId, WorkOrderStatus,
};
use std::collections::HashSet;

fn bench_work_order_creation(c: &mut Criterion) {
    c.bench_function("work_order_creation", |b| {
        b.iter(|| {
            black_box(
                WorkOrder::new(
                    "Implement feature X".to_string(),
                    "team@example.com".to_string(),
                )
                .unwrap(),
            );
        });
    });
}

fn bench_work_order_id_generation(c: &mut Criterion) {
    c.bench_function("work_order_id_generation", |b| {
        b.iter(|| {
            black_box(WorkOrderId::new());
        });
    });
}

fn bench_simple_validation(c: &mut Criterion) {
    let work_order = WorkOrder::new(
        "Simple work order".to_string(),
        "owner@example.com".to_string(),
    )
    .unwrap();

    c.bench_function("simple_validation", |b| {
        b.iter(|| {
            black_box(validate_work_order(&work_order).unwrap());
        });
    });
}

fn bench_complex_validation(c: &mut Criterion) {
    let mut work_order = WorkOrder::new(
        "Complex work order with all features".to_string(),
        "owner@example.com".to_string(),
    )
    .unwrap();

    // Add constraints
    for i in 0..10 {
        work_order = work_order
            .with_constraint(Constraint {
                description: format!("Constraint {}", i),
                constraint_type: ConstraintType::Quality,
                enforced: true,
            })
            .unwrap();
    }

    // Add acceptance test with multiple criteria
    let acceptance_test = AcceptanceTest {
        description: "Comprehensive acceptance test".to_string(),
        criteria: (0..10)
            .map(|i| AcceptanceCriterion {
                criterion: format!("Criterion {}", i),
                required: true,
                measurable: true,
            })
            .collect(),
        test_type: TestType::Automated,
    };
    work_order = work_order.with_acceptance_test(acceptance_test).unwrap();

    // Add reversibility policy
    let policy = ReversibilityPolicy {
        reversible: true,
        rollback_steps: vec![
            "Step 1: Backup".to_string(),
            "Step 2: Restore".to_string(),
        ],
        backup_required: true,
        verification_required: true,
    };
    work_order = work_order.with_reversibility(policy).unwrap();

    // Add dependencies
    work_order = work_order
        .with_dependencies(vec![
            "dep1".to_string(),
            "dep2".to_string(),
            "dep3".to_string(),
        ])
        .unwrap();

    // Add tags
    let mut tags = HashSet::new();
    for i in 0..10 {
        tags.insert(format!("tag{}", i));
    }
    work_order = work_order.with_tags(tags).unwrap();

    c.bench_function("complex_validation", |b| {
        b.iter(|| {
            black_box(validate_work_order(&work_order).unwrap());
        });
    });
}

fn bench_validation_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("validation_throughput");

    for count in [10, 100, 1000].iter() {
        let work_orders: Vec<WorkOrder> = (0..*count)
            .map(|i| {
                WorkOrder::new(
                    format!("Work order {}", i),
                    format!("owner{}@example.com", i),
                )
                .unwrap()
            })
            .collect();

        group.throughput(Throughput::Elements(*count));
        group.bench_with_input(
            BenchmarkId::from_parameter(count),
            &work_orders,
            |b, work_orders| {
                b.iter(|| {
                    for wo in work_orders {
                        black_box(validate_work_order(wo).unwrap());
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_state_transitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_transitions");

    let work_order = WorkOrder::new(
        "Test work order".to_string(),
        "owner@example.com".to_string(),
    )
    .unwrap();

    group.bench_function("transition_to_validated", |b| {
        b.iter(|| {
            let mut wo = work_order.clone();
            black_box(wo.transition_to(WorkOrderStatus::Validated).unwrap());
        });
    });

    group.finish();
}

fn bench_work_order_builder_pattern(c: &mut Criterion) {
    c.bench_function("builder_pattern_full", |b| {
        b.iter(|| {
            let work_order = WorkOrder::new(
                "Feature implementation".to_string(),
                "team@example.com".to_string(),
            )
            .unwrap()
            .with_constraint(Constraint {
                description: "Complete in 2 hours".to_string(),
                constraint_type: ConstraintType::Time,
                enforced: true,
            })
            .unwrap()
            .with_priority(Priority::High)
            .unwrap()
            .with_dependencies(vec!["task-1".to_string(), "task-2".to_string()])
            .unwrap();

            black_box(work_order);
        });
    });
}

fn bench_terminal_state_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("terminal_state_checks");

    for status in [
        WorkOrderStatus::Pending,
        WorkOrderStatus::InProgress,
        WorkOrderStatus::Completed,
        WorkOrderStatus::Failed,
    ] {
        let mut work_order = WorkOrder::new(
            "Test".to_string(),
            "owner@example.com".to_string(),
        )
        .unwrap();
        work_order.transition_to(status).ok();

        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{:?}", status)),
            &work_order,
            |b, wo| {
                b.iter(|| {
                    black_box(wo.is_terminal());
                    black_box(wo.is_active());
                });
            },
        );
    }

    group.finish();
}

fn bench_priority_comparison(c: &mut Criterion) {
    c.bench_function("priority_comparison", |b| {
        b.iter(|| {
            black_box(Priority::Critical > Priority::High);
            black_box(Priority::High > Priority::Normal);
            black_box(Priority::Normal > Priority::Low);
        });
    });
}

fn bench_serialization(c: &mut Criterion) {
    let work_order = WorkOrder::new(
        "Serialization test".to_string(),
        "owner@example.com".to_string(),
    )
    .unwrap();

    let mut group = c.benchmark_group("serialization");

    group.bench_function("serialize_json", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&work_order).unwrap());
        });
    });

    let json = serde_json::to_string(&work_order).unwrap();
    group.bench_function("deserialize_json", |b| {
        b.iter(|| {
            black_box(serde_json::from_str::<WorkOrder>(&json).unwrap());
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_work_order_creation,
    bench_work_order_id_generation,
    bench_simple_validation,
    bench_complex_validation,
    bench_validation_throughput,
    bench_state_transitions,
    bench_work_order_builder_pattern,
    bench_terminal_state_checks,
    bench_priority_comparison,
    bench_serialization,
);
criterion_main!(benches);
