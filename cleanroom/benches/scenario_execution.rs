//! Benchmark suite for scenario execution
//!
//! This benchmark measures the performance of scenario creation,
//! execution, and various scenario operations.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use cleanroom::scenario::{Scenario, RunResult, StepResult, Step, StepSource};
use cleanroom::backend::{Cmd, Backend};
use cleanroom::policy::Policy;
use cleanroom::error::Result;
use std::time::Duration;
use std::collections::HashMap;

fn bench_scenario_creation(c: &mut Criterion) {
    c.bench_function("scenario_creation", |b| {
        b.iter(|| {
            black_box(Scenario::new("test_scenario".to_string()))
        })
    });
}

fn bench_scenario_add_step(c: &mut Criterion) {
    c.bench_function("scenario_add_step", |b| {
        b.iter(|| {
            let mut scenario = Scenario::new("test_scenario".to_string());
            let step = Step {
                name: "test_step".to_string(),
                command: Cmd {
                    bin: "echo".to_string(),
                    args: vec!["hello".to_string()],
                    workdir: None,
                    env: HashMap::new(),
                    policy: Policy::default(),
                },
                source: StepSource::Manual,
            };
            scenario.add_step(step);
            black_box(scenario)
        })
    });
}

fn bench_scenario_add_multiple_steps(c: &mut Criterion) {
    let mut group = c.benchmark_group("scenario_multiple_steps");
    
    for size in [1, 5, 10, 20, 50].iter() {
        group.bench_with_input(BenchmarkId::new("add_steps", size), size, |b, &size| {
            b.iter(|| {
                let mut scenario = Scenario::new("test_scenario".to_string());
                for i in 0..size {
                    let step = Step {
                        name: format!("step_{}", i),
                        command: Cmd {
                            bin: "echo".to_string(),
                            args: vec![format!("hello_{}", i)],
                            workdir: None,
                            env: HashMap::new(),
                            policy: Policy::default(),
                        },
                        source: StepSource::Manual,
                    };
                    scenario.add_step(step);
                }
                black_box(scenario)
            })
        });
    }
    
    group.finish();
}

fn bench_scenario_execution_simulation(c: &mut Criterion) {
    c.bench_function("scenario_execution_simulation", |b| {
        b.iter(|| {
            let mut scenario = Scenario::new("test_scenario".to_string());
            let step = Step {
                name: "test_step".to_string(),
                command: Cmd {
                    bin: "echo".to_string(),
                    args: vec!["hello".to_string()],
                    workdir: None,
                    env: HashMap::new(),
                    policy: Policy::default(),
                },
                source: StepSource::Manual,
            };
            scenario.add_step(step);
            
            // Simulate execution
            let result = RunResult {
                success: true,
                exit_code: Some(0),
                stdout: "hello".to_string(),
                stderr: String::new(),
                duration: Duration::from_millis(10),
                artifacts: Vec::new(),
                metrics: HashMap::new(),
            };
            
            black_box(result)
        })
    });
}

fn bench_scenario_step_result_creation(c: &mut Criterion) {
    c.bench_function("scenario_step_result_creation", |b| {
        b.iter(|| {
            black_box(StepResult {
                step_name: "test_step".to_string(),
                success: true,
                exit_code: Some(0),
                stdout: "hello".to_string(),
                stderr: String::new(),
                duration: Duration::from_millis(10),
                artifacts: Vec::new(),
                metrics: HashMap::new(),
            })
        })
    });
}

fn bench_scenario_run_result_creation(c: &mut Criterion) {
    c.bench_function("scenario_run_result_creation", |b| {
        b.iter(|| {
            black_box(RunResult {
                success: true,
                exit_code: Some(0),
                stdout: "hello".to_string(),
                stderr: String::new(),
                duration: Duration::from_millis(10),
                artifacts: Vec::new(),
                metrics: HashMap::new(),
            })
        })
    });
}

fn bench_scenario_serialization(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    let step = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario.add_step(step);
    
    c.bench_function("scenario_serialization", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&scenario).unwrap())
        })
    });
}

fn bench_scenario_deserialization(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    let step = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario.add_step(step);
    let serialized = serde_json::to_string(&scenario).unwrap();
    
    c.bench_function("scenario_deserialization", |b| {
        b.iter(|| {
            black_box(serde_json::from_str::<Scenario>(&serialized).unwrap())
        })
    });
}

fn bench_scenario_clone(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    let step = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario.add_step(step);
    
    c.bench_function("scenario_clone", |b| {
        b.iter(|| {
            black_box(scenario.clone())
        })
    });
}

fn bench_scenario_hash(c: &mut Criterion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut scenario = Scenario::new("test_scenario".to_string());
    let step = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario.add_step(step);
    
    c.bench_function("scenario_hash", |b| {
        b.iter(|| {
            let mut hasher = DefaultHasher::new();
            scenario.hash(&mut hasher);
            black_box(hasher.finish())
        })
    });
}

fn bench_scenario_equality(c: &mut Criterion) {
    let mut scenario1 = Scenario::new("test_scenario".to_string());
    let step1 = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario1.add_step(step1);
    
    let mut scenario2 = Scenario::new("test_scenario".to_string());
    let step2 = Step {
        name: "test_step".to_string(),
        command: Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        },
        source: StepSource::Manual,
    };
    scenario2.add_step(step2);
    
    c.bench_function("scenario_equality", |b| {
        b.iter(|| {
            black_box(scenario1 == scenario2)
        })
    });
}

fn bench_scenario_step_creation(c: &mut Criterion) {
    c.bench_function("scenario_step_creation", |b| {
        b.iter(|| {
            black_box(Step {
                name: "test_step".to_string(),
                command: Cmd {
                    bin: "echo".to_string(),
                    args: vec!["hello".to_string()],
                    workdir: None,
                    env: HashMap::new(),
                    policy: Policy::default(),
                },
                source: StepSource::Manual,
            })
        })
    });
}

fn bench_scenario_cmd_creation(c: &mut Criterion) {
    c.bench_function("scenario_cmd_creation", |b| {
        b.iter(|| {
            black_box(Cmd {
                bin: "echo".to_string(),
                args: vec!["hello".to_string()],
                workdir: None,
                env: HashMap::new(),
                policy: Policy::default(),
            })
        })
    });
}

fn bench_scenario_step_source_enum(c: &mut Criterion) {
    let source = StepSource::Manual;
    
    c.bench_function("scenario_step_source_enum", |b| {
        b.iter(|| {
            black_box(matches!(source, StepSource::Manual))
        })
    });
}

fn bench_scenario_memory_usage(c: &mut Criterion) {
    c.bench_function("scenario_memory_usage", |b| {
        b.iter(|| {
            let scenarios: Vec<Scenario> = (0..1000)
                .map(|i| {
                    let mut scenario = Scenario::new(format!("scenario_{}", i));
                    let step = Step {
                        name: format!("step_{}", i),
                        command: Cmd {
                            bin: "echo".to_string(),
                            args: vec![format!("hello_{}", i)],
                            workdir: None,
                            env: HashMap::new(),
                            policy: Policy::default(),
                        },
                        source: StepSource::Manual,
                    };
                    scenario.add_step(step);
                    scenario
                })
                .collect();
            black_box(scenarios.len())
        })
    });
}

fn bench_scenario_large_scenario(c: &mut Criterion) {
    c.bench_function("scenario_large_scenario", |b| {
        b.iter(|| {
            let mut scenario = Scenario::new("large_scenario".to_string());
            for i in 0..100 {
                let step = Step {
                    name: format!("step_{}", i),
                    command: Cmd {
                        bin: "echo".to_string(),
                        args: vec![format!("hello_{}", i)],
                        workdir: None,
                        env: HashMap::new(),
                        policy: Policy::default(),
                    },
                    source: StepSource::Manual,
                };
                scenario.add_step(step);
            }
            black_box(scenario)
        })
    });
}

fn bench_scenario_step_iteration(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    for i in 0..50 {
        let step = Step {
            name: format!("step_{}", i),
            command: Cmd {
                bin: "echo".to_string(),
                args: vec![format!("hello_{}", i)],
                workdir: None,
                env: HashMap::new(),
                policy: Policy::default(),
            },
            source: StepSource::Manual,
        };
        scenario.add_step(step);
    }
    
    c.bench_function("scenario_step_iteration", |b| {
        b.iter(|| {
            let mut count = 0;
            for step in scenario.steps() {
                black_box(step);
                count += 1;
            }
            black_box(count)
        })
    });
}

fn bench_scenario_step_search(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    for i in 0..50 {
        let step = Step {
            name: format!("step_{}", i),
            command: Cmd {
                bin: "echo".to_string(),
                args: vec![format!("hello_{}", i)],
                workdir: None,
                env: HashMap::new(),
                policy: Policy::default(),
            },
            source: StepSource::Manual,
        };
        scenario.add_step(step);
    }
    
    c.bench_function("scenario_step_search", |b| {
        b.iter(|| {
            black_box(scenario.find_step("step_25"))
        })
    });
}

fn bench_scenario_step_count(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    for i in 0..50 {
        let step = Step {
            name: format!("step_{}", i),
            command: Cmd {
                bin: "echo".to_string(),
                args: vec![format!("hello_{}", i)],
                workdir: None,
                env: HashMap::new(),
                policy: Policy::default(),
            },
            source: StepSource::Manual,
        };
        scenario.add_step(step);
    }
    
    c.bench_function("scenario_step_count", |b| {
        b.iter(|| {
            black_box(scenario.step_count())
        })
    });
}

fn bench_scenario_clear(c: &mut Criterion) {
    let mut scenario = Scenario::new("test_scenario".to_string());
    for i in 0..50 {
        let step = Step {
            name: format!("step_{}", i),
            command: Cmd {
                bin: "echo".to_string(),
                args: vec![format!("hello_{}", i)],
                workdir: None,
                env: HashMap::new(),
                policy: Policy::default(),
            },
            source: StepSource::Manual,
        };
        scenario.add_step(step);
    }
    
    c.bench_function("scenario_clear", |b| {
        b.iter(|| {
            scenario.clear();
            black_box(scenario.step_count())
        })
    });
}

criterion_group!(
    benches,
    bench_scenario_creation,
    bench_scenario_add_step,
    bench_scenario_add_multiple_steps,
    bench_scenario_execution_simulation,
    bench_scenario_step_result_creation,
    bench_scenario_run_result_creation,
    bench_scenario_serialization,
    bench_scenario_deserialization,
    bench_scenario_clone,
    bench_scenario_hash,
    bench_scenario_equality,
    bench_scenario_step_creation,
    bench_scenario_cmd_creation,
    bench_scenario_step_source_enum,
    bench_scenario_memory_usage,
    bench_scenario_large_scenario,
    bench_scenario_step_iteration,
    bench_scenario_step_search,
    bench_scenario_step_count,
    bench_scenario_clear
);

criterion_main!(benches);
