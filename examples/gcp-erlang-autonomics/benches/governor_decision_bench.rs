use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Instant;

/// FSM state for governor autonomic system
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GovernorState {
    Normal,
    Degraded,
    Critical,
    Recovery,
}

/// Represents system signal thresholds
#[derive(Debug, Clone)]
struct Thresholds {
    cpu_warning: f64,
    cpu_critical: f64,
    memory_warning: f64,
    memory_critical: f64,
}

/// Governor implements autonomic decision-making FSM
struct Governor {
    state: GovernorState,
    thresholds: Thresholds,
    transition_count: u64,
}

impl Governor {
    fn new() -> Self {
        Self {
            state: GovernorState::Normal,
            thresholds: Thresholds {
                cpu_warning: 70.0,
                cpu_critical: 90.0,
                memory_warning: 75.0,
                memory_critical: 95.0,
            },
            transition_count: 0,
        }
    }

    /// Evaluate system metrics and determine next state
    /// Target SLO: ≤10ms per decision
    fn make_decision(
        &mut self,
        cpu_usage: f64,
        memory_usage: f64,
        error_rate: f64,
    ) -> (GovernorState, Vec<String>) {
        let start = Instant::now();
        let mut actions = Vec::new();

        // Check invariants
        assert!(
            cpu_usage >= 0.0 && cpu_usage <= 100.0,
            "CPU usage out of range"
        );
        assert!(
            memory_usage >= 0.0 && memory_usage <= 100.0,
            "Memory usage out of range"
        );

        let new_state = self.evaluate_metrics(cpu_usage, memory_usage, error_rate);

        // Perform state transition with validation
        if new_state != self.state {
            actions = self.transition_state(new_state);
            self.state = new_state;
            self.transition_count += 1;
        }

        let elapsed = start.elapsed();

        // Verify SLO: ≤10ms per decision
        assert!(
            elapsed.as_millis() <= 10,
            "Decision time exceeded SLO: {}ms",
            elapsed.as_millis()
        );

        (self.state, actions)
    }

    fn evaluate_metrics(&self, cpu: f64, memory: f64, error_rate: f64) -> GovernorState {
        let cpu_critical = cpu > self.thresholds.cpu_critical;
        let memory_critical = memory > self.thresholds.memory_critical;
        let error_high = error_rate > 5.0; // 5% error threshold

        let cpu_warning = cpu > self.thresholds.cpu_warning;
        let memory_warning = memory > self.thresholds.memory_warning;

        match self.state {
            GovernorState::Normal => {
                if cpu_critical || memory_critical || error_high {
                    GovernorState::Critical
                } else if cpu_warning || memory_warning {
                    GovernorState::Degraded
                } else {
                    GovernorState::Normal
                }
            }
            GovernorState::Degraded => {
                if cpu_critical || memory_critical || error_high {
                    GovernorState::Critical
                } else if !cpu_warning && !memory_warning && error_rate <= 2.0 {
                    GovernorState::Normal
                } else {
                    GovernorState::Degraded
                }
            }
            GovernorState::Critical => {
                if !cpu_critical && !memory_critical && error_rate <= 2.0 {
                    GovernorState::Recovery
                } else {
                    GovernorState::Critical
                }
            }
            GovernorState::Recovery => {
                if cpu_critical || memory_critical {
                    GovernorState::Critical
                } else if !cpu_warning && !memory_warning && error_rate <= 1.0 {
                    GovernorState::Normal
                } else {
                    GovernorState::Recovery
                }
            }
        }
    }

    fn transition_state(&self, new_state: GovernorState) -> Vec<String> {
        let mut actions = Vec::new();

        match (self.state, new_state) {
            (GovernorState::Normal, GovernorState::Degraded) => {
                actions.push("enable_monitoring".to_string());
                actions.push("increase_polling_frequency".to_string());
            }
            (GovernorState::Degraded, GovernorState::Critical) => {
                actions.push("trigger_autoscaling".to_string());
                actions.push("enable_circuit_breaker".to_string());
                actions.push("alert_sre_team".to_string());
            }
            (GovernorState::Critical, GovernorState::Recovery) => {
                actions.push("begin_graceful_degradation".to_string());
                actions.push("request_additional_resources".to_string());
            }
            (GovernorState::Recovery, GovernorState::Normal) => {
                actions.push("normalize_settings".to_string());
                actions.push("reduce_polling_frequency".to_string());
            }
            _ => {}
        }

        actions
    }

    fn get_state(&self) -> GovernorState {
        self.state
    }

    fn transition_count(&self) -> u64 {
        self.transition_count
    }
}

fn benchmark_fsm_transitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("governor_fsm");
    group.measurement_time(std::time::Duration::from_secs(10));
    group.sample_size(100);

    group.bench_function("state_transition", |b| {
        b.iter_batched(
            || Governor::new(),
            |mut governor| {
                let start = Instant::now();

                // Simulate state transitions
                let (state, _actions) = governor.make_decision(
                    black_box(75.0), // CPU warning level
                    black_box(60.0), // Memory normal
                    black_box(1.5),  // Error rate normal
                );

                let elapsed = start.elapsed();
                assert!(elapsed.as_millis() <= 10, "Decision exceeded SLO");

                state
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_critical_decision(c: &mut Criterion) {
    let mut group = c.benchmark_group("governor_critical");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("critical_state_detection", |b| {
        b.iter_batched(
            || Governor::new(),
            |mut governor| {
                let start = Instant::now();

                // Trigger critical state
                let (state, actions) = governor.make_decision(
                    black_box(92.0), // CPU critical
                    black_box(88.0), // Memory high
                    black_box(4.5),  // Error rate elevated
                );

                let elapsed = start.elapsed();

                assert_eq!(state, GovernorState::Critical);
                assert!(!actions.is_empty(), "Critical state should trigger actions");
                assert!(elapsed.as_millis() <= 10, "Decision exceeded SLO");

                actions.len()
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_decision_volume(c: &mut Criterion) {
    let mut group = c.benchmark_group("governor_volume");
    group.measurement_time(std::time::Duration::from_secs(15));

    for decision_count in [100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(decision_count),
            decision_count,
            |b, &decision_count| {
                b.iter_batched(
                    || {
                        let governor = Governor::new();
                        let metrics = vec![
                            (50.0, 50.0, 1.0),
                            (75.0, 65.0, 1.5),
                            (92.0, 88.0, 4.5),
                            (45.0, 50.0, 0.5),
                        ];
                        (governor, metrics)
                    },
                    |(mut governor, metrics)| {
                        let start = Instant::now();

                        for i in 0..decision_count {
                            let (cpu, mem, err) = metrics[i % metrics.len()];
                            let _ = governor.make_decision(cpu, mem, err);
                        }

                        let elapsed = start.elapsed();
                        let per_decision_ms = elapsed.as_secs_f64() * 1000.0 / decision_count as f64;

                        // SLO: ≤10ms per decision
                        assert!(
                            per_decision_ms <= 10.0,
                            "Average decision time exceeded SLO: {:.2}ms",
                            per_decision_ms
                        );

                        governor.transition_count()
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

fn benchmark_invariant_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("governor_invariants");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("validate_state_machine", |b| {
        b.iter_batched(
            || {
                vec![
                    (50.0, 50.0, 1.0),
                    (75.0, 65.0, 1.5),
                    (92.0, 88.0, 4.5),
                    (45.0, 50.0, 0.5),
                    (80.0, 80.0, 3.0),
                ]
            },
            |metrics| {
                let mut governor = Governor::new();
                let mut transitions = 0;

                for (cpu, mem, err) in black_box(metrics) {
                    let old_state = governor.get_state();
                    let (new_state, _) = governor.make_decision(cpu, mem, err);

                    // Verify valid state transition
                    let valid = match (old_state, new_state) {
                        (GovernorState::Normal, GovernorState::Degraded) => true,
                        (GovernorState::Normal, GovernorState::Critical) => true,
                        (GovernorState::Normal, GovernorState::Normal) => true,
                        (GovernorState::Degraded, GovernorState::Critical) => true,
                        (GovernorState::Degraded, GovernorState::Normal) => true,
                        (GovernorState::Degraded, GovernorState::Degraded) => true,
                        (GovernorState::Critical, GovernorState::Recovery) => true,
                        (GovernorState::Critical, GovernorState::Critical) => true,
                        (GovernorState::Recovery, GovernorState::Normal) => true,
                        (GovernorState::Recovery, GovernorState::Recovery) => true,
                        (GovernorState::Recovery, GovernorState::Critical) => true,
                        _ => false,
                    };

                    assert!(valid, "Invalid state transition: {:?} -> {:?}", old_state, new_state);
                    transitions += 1;
                }

                transitions
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_fsm_transitions,
    benchmark_critical_decision,
    benchmark_decision_volume,
    benchmark_invariant_checks
);
criterion_main!(benches);
