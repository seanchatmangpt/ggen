use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Instant;
use std::thread;
use std::time::Duration;

/// Represents a remediation plan
#[derive(Debug, Clone)]
struct Plan {
    id: String,
    actions: Vec<Action>,
}

/// Action represents a single remediation step
#[derive(Debug, Clone)]
enum Action {
    Scale { service: String, replicas: i32 },
    CircuitBreak { service: String },
    RateLimitIncrease { service: String, qps: u32 },
    Rollback { service: String, version: String },
}

/// Actuator executes remediation plans on GCP Cloud Run
struct Actuator {
    executed_plans: Vec<Plan>,
    rollback_history: Vec<(Plan, bool)>,
}

impl Actuator {
    fn new() -> Self {
        Self {
            executed_plans: Vec::new(),
            rollback_history: Vec::new(),
        }
    }

    /// Execute a remediation plan end-to-end
    /// Target SLO: ≤500ms end-to-end (plan + execute + rollback)
    fn execute_plan(&mut self, plan: Plan) -> Result<ExecutionResult, ExecutionError> {
        let start = Instant::now();

        // Validate plan
        if plan.actions.is_empty() {
            return Err(ExecutionError::EmptyPlan);
        }

        let mut executed_actions = Vec::new();

        // Execute each action
        for action in &plan.actions {
            let action_start = Instant::now();

            match self.execute_action(action) {
                Ok(_) => {
                    executed_actions.push(action.clone());
                }
                Err(e) => {
                    // Rollback on error
                    let _ = self.rollback(&executed_actions);
                    return Err(e);
                }
            }

            let action_elapsed = action_start.elapsed();
            assert!(
                action_elapsed.as_millis() <= 150,
                "Single action exceeded time budget: {}ms",
                action_elapsed.as_millis()
            );
        }

        let elapsed = start.elapsed();

        // Verify SLO: ≤500ms end-to-end
        assert!(
            elapsed.as_millis() <= 500,
            "Plan execution exceeded SLO: {}ms",
            elapsed.as_millis()
        );

        self.executed_plans.push(plan.clone());

        Ok(ExecutionResult {
            actions_executed: executed_actions.len(),
            duration_ms: elapsed.as_millis() as u64,
        })
    }

    /// Execute a single action (simulated Cloud Run API call)
    fn execute_action(&self, action: &Action) -> Result<(), ExecutionError> {
        // Simulate API latency
        let latency_ms = match action {
            Action::Scale { .. } => 45,      // Cloud Run autoscaling: ~45ms
            Action::CircuitBreak { .. } => 15, // Service mesh update: ~15ms
            Action::RateLimitIncrease { .. } => 20, // Rate limit service: ~20ms
            Action::Rollback { .. } => 60,   // Deployment rollback: ~60ms
        };

        // Simulate I/O operation
        thread::sleep(Duration::from_millis(latency_ms));

        Ok(())
    }

    /// Rollback executed actions
    fn rollback(&mut self, actions: &[Action]) -> Result<(), ExecutionError> {
        let start = Instant::now();
        let mut success = true;

        for action in actions.iter().rev() {
            match action {
                Action::Scale { .. } => {
                    thread::sleep(Duration::from_millis(45));
                }
                Action::CircuitBreak { .. } => {
                    thread::sleep(Duration::from_millis(15));
                }
                Action::RateLimitIncrease { .. } => {
                    thread::sleep(Duration::from_millis(20));
                }
                Action::Rollback { .. } => {
                    thread::sleep(Duration::from_millis(60));
                }
            }
        }

        let elapsed = start.elapsed();

        // Rollback should be at most 2x the execution time
        assert!(
            elapsed.as_millis() <= 400,
            "Rollback exceeded time budget: {}ms",
            elapsed.as_millis()
        );

        if success {
            Ok(())
        } else {
            Err(ExecutionError::RollbackFailed)
        }
    }

    fn executed_count(&self) -> usize {
        self.executed_plans.len()
    }
}

#[derive(Debug)]
struct ExecutionResult {
    actions_executed: usize,
    duration_ms: u64,
}

#[derive(Debug, Clone)]
enum ExecutionError {
    EmptyPlan,
    ActionFailed(String),
    RollbackFailed,
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionError::EmptyPlan => write!(f, "Empty plan"),
            ExecutionError::ActionFailed(msg) => write!(f, "Action failed: {}", msg),
            ExecutionError::RollbackFailed => write!(f, "Rollback failed"),
        }
    }
}

impl std::error::Error for ExecutionError {}

fn create_scale_plan(service: &str, replicas: i32) -> Plan {
    Plan {
        id: format!("plan-scale-{}", service),
        actions: vec![Action::Scale {
            service: service.to_string(),
            replicas,
        }],
    }
}

fn create_comprehensive_plan() -> Plan {
    Plan {
        id: "plan-comprehensive".to_string(),
        actions: vec![
            Action::CircuitBreak {
                service: "api-gateway".to_string(),
            },
            Action::RateLimitIncrease {
                service: "backend-service".to_string(),
                qps: 5000,
            },
            Action::Scale {
                service: "worker-pool".to_string(),
                replicas: 10,
            },
        ],
    }
}

fn benchmark_single_action_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("actuator_single");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("scale_action_execution", |b| {
        b.iter_batched(
            || {
                let actuator = Actuator::new();
                let plan = create_scale_plan("api-service", 5);
                (actuator, plan)
            },
            |(mut actuator, plan)| {
                let result = actuator.execute_plan(black_box(plan));
                assert!(result.is_ok());
                result.unwrap().actions_executed
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_multi_action_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("actuator_multi");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("comprehensive_plan_execution", |b| {
        b.iter_batched(
            || {
                let actuator = Actuator::new();
                let plan = create_comprehensive_plan();
                (actuator, plan)
            },
            |(mut actuator, plan)| {
                let result = actuator.execute_plan(black_box(plan));
                assert!(result.is_ok());
                let res = result.unwrap();
                assert_eq!(res.actions_executed, 3);
                res.duration_ms
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_rollback_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("actuator_rollback");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("rollback_performance", |b| {
        b.iter_batched(
            || {
                let actions = vec![
                    Action::CircuitBreak {
                        service: "api-gateway".to_string(),
                    },
                    Action::RateLimitIncrease {
                        service: "backend-service".to_string(),
                        qps: 5000,
                    },
                ];
                let mut actuator = Actuator::new();
                (actuator, actions)
            },
            |(mut actuator, actions)| {
                let result = actuator.rollback(&actions);
                assert!(result.is_ok());
                actions.len()
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_plan_execution_volume(c: &mut Criterion) {
    let mut group = c.benchmark_group("actuator_volume");
    group.measurement_time(std::time::Duration::from_secs(15));

    for plan_count in [5, 10].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(plan_count),
            plan_count,
            |b, &plan_count| {
                b.iter_batched(
                    || {
                        let actuator = Actuator::new();
                        let plans: Vec<Plan> = (0..plan_count)
                            .map(|i| create_scale_plan(&format!("service-{}", i), (i % 5 + 1) as i32))
                            .collect();
                        (actuator, plans)
                    },
                    |(mut actuator, plans)| {
                        let start = Instant::now();
                        let mut success_count = 0;

                        for plan in black_box(plans) {
                            if actuator.execute_plan(plan).is_ok() {
                                success_count += 1;
                            }
                        }

                        let elapsed = start.elapsed();
                        let per_plan_ms = elapsed.as_secs_f64() * 1000.0 / plan_count as f64;

                        // SLO: ≤500ms per plan execution
                        assert!(
                            per_plan_ms <= 500.0,
                            "Average plan time exceeded SLO: {:.0}ms",
                            per_plan_ms
                        );

                        success_count
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_single_action_execution,
    benchmark_multi_action_execution,
    benchmark_rollback_execution,
    benchmark_plan_execution_volume
);
criterion_main!(benches);
