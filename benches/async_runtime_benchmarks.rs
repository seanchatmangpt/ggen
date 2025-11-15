use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use lazy_static::lazy_static;
use std::hint::black_box;
use std::time::Duration;
use tokio::runtime::{Builder, Runtime};

// Mock async business logic simulating typical CLI operations
async fn simulate_template_generation() -> String {
    // Simulate I/O operations (file reading, network calls, etc.)
    tokio::time::sleep(Duration::from_micros(100)).await;

    // Simulate CPU-bound work (parsing, validation, etc.)
    let mut result = String::new();
    for i in 0..100 {
        result.push_str(&format!("line_{}\n", i));
    }

    result
}

async fn simulate_graph_execution() -> Vec<String> {
    // Simulate graph node execution
    tokio::time::sleep(Duration::from_micros(50)).await;

    let mut results = Vec::new();
    for i in 0..10 {
        results.push(format!("node_{}_output", i));
    }

    results
}

async fn simulate_ai_inference() -> String {
    // Simulate AI API call
    tokio::time::sleep(Duration::from_millis(5)).await;

    "AI generated response".to_string()
}

// Option A: New runtime per command
fn option_a_new_runtime() -> String {
    let rt = Runtime::new().unwrap();
    rt.block_on(async { simulate_template_generation().await })
}

// Option B: Shared static runtime
lazy_static! {
    static ref SHARED_RUNTIME: Runtime = Runtime::new().unwrap();
}

fn option_b_shared_runtime() -> String {
    SHARED_RUNTIME.block_on(async { simulate_template_generation().await })
}

// Option C: Lazy static runtime
lazy_static! {
    static ref LAZY_RUNTIME: Runtime = Builder::new_multi_thread()
        .worker_threads(4)
        .thread_name("ggen-worker")
        .enable_all()
        .build()
        .unwrap();
}

fn option_c_lazy_static() -> String {
    LAZY_RUNTIME.block_on(async { simulate_template_generation().await })
}

// Benchmark 1: Runtime creation overhead
fn bench_runtime_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime_creation");

    group.bench_function("new_runtime_basic", |b| {
        b.iter(|| {
            black_box(Runtime::new().unwrap());
        });
    });

    group.bench_function("new_runtime_multi_thread", |b| {
        b.iter(|| {
            black_box(
                Builder::new_multi_thread()
                    .worker_threads(4)
                    .enable_all()
                    .build()
                    .unwrap(),
            );
        });
    });

    group.bench_function("new_runtime_current_thread", |b| {
        b.iter(|| {
            black_box(Builder::new_current_thread().enable_all().build().unwrap());
        });
    });

    group.finish();
}

// Benchmark 2: Runtime execution approaches
fn bench_runtime_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime_execution");

    group.bench_function("option_a_new_runtime", |b| {
        b.iter(|| {
            black_box(option_a_new_runtime());
        });
    });

    group.bench_function("option_b_shared_runtime", |b| {
        b.iter(|| {
            black_box(option_b_shared_runtime());
        });
    });

    group.bench_function("option_c_lazy_static", |b| {
        b.iter(|| {
            black_box(option_c_lazy_static());
        });
    });

    group.finish();
}

// Benchmark 3: Different workload types
fn bench_workload_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("workload_types");

    // Template generation (I/O + CPU)
    group.bench_function("template_new_runtime", |b| {
        b.iter(|| {
            let rt = Runtime::new().unwrap();
            black_box(rt.block_on(simulate_template_generation()));
        });
    });

    group.bench_function("template_shared_runtime", |b| {
        b.iter(|| {
            black_box(SHARED_RUNTIME.block_on(simulate_template_generation()));
        });
    });

    // Graph execution (parallel tasks)
    group.bench_function("graph_new_runtime", |b| {
        b.iter(|| {
            let rt = Runtime::new().unwrap();
            black_box(rt.block_on(simulate_graph_execution()));
        });
    });

    group.bench_function("graph_shared_runtime", |b| {
        b.iter(|| {
            black_box(SHARED_RUNTIME.block_on(simulate_graph_execution()));
        });
    });

    // AI inference (network I/O heavy)
    group.bench_function("ai_new_runtime", |b| {
        b.iter(|| {
            let rt = Runtime::new().unwrap();
            black_box(rt.block_on(simulate_ai_inference()));
        });
    });

    group.bench_function("ai_shared_runtime", |b| {
        b.iter(|| {
            black_box(SHARED_RUNTIME.block_on(simulate_ai_inference()));
        });
    });

    group.finish();
}

// Benchmark 4: Concurrent command execution
fn bench_concurrent_commands(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_commands");

    for num_commands in [1, 5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*num_commands as u64));

        group.bench_with_input(
            BenchmarkId::new("new_runtime", num_commands),
            num_commands,
            |b, &n| {
                b.iter(|| {
                    for _ in 0..n {
                        let rt = Runtime::new().unwrap();
                        black_box(rt.block_on(simulate_template_generation()));
                    }
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("shared_runtime", num_commands),
            num_commands,
            |b, &n| {
                b.iter(|| {
                    for _ in 0..n {
                        black_box(SHARED_RUNTIME.block_on(simulate_template_generation()));
                    }
                });
            },
        );
    }

    group.finish();
}

// Benchmark 5: Memory allocation patterns
fn bench_memory_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_patterns");

    // Measure memory overhead of runtime creation
    group.bench_function("runtime_allocation", |b| {
        b.iter(|| {
            let rt = black_box(Runtime::new().unwrap());
            drop(rt);
        });
    });

    // Measure shared runtime memory reuse
    group.bench_function("shared_runtime_reuse", |b| {
        b.iter(|| {
            // Multiple uses of same runtime
            for _ in 0..10 {
                black_box(SHARED_RUNTIME.block_on(async { 42 }));
            }
        });
    });

    group.finish();
}

// Benchmark 6: Startup latency
fn bench_startup_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("startup_latency");

    // Cold start with new runtime
    group.bench_function("cold_start_new_runtime", |b| {
        b.iter(|| {
            let start = std::time::Instant::now();
            let rt = Runtime::new().unwrap();
            let result = rt.block_on(simulate_template_generation());
            let duration = start.elapsed();
            black_box((result, duration));
        });
    });

    // Warm start with shared runtime
    group.bench_function("warm_start_shared_runtime", |b| {
        b.iter(|| {
            let start = std::time::Instant::now();
            let result = SHARED_RUNTIME.block_on(simulate_template_generation());
            let duration = start.elapsed();
            black_box((result, duration));
        });
    });

    group.finish();
}

// Benchmark 7: Real-world CLI simulation
fn bench_cli_simulation(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_simulation");
    group.sample_size(50); // Reduce sample size for longer operations

    // Simulate complete CLI command execution
    async fn complete_cli_command() -> String {
        // Parse args (synchronous)
        let _args = vec!["ggen", "template", "create"];

        // Load template (async I/O)
        tokio::time::sleep(Duration::from_micros(200)).await;

        // Validate template (CPU)
        let mut validation = String::new();
        for i in 0..50 {
            validation.push_str(&format!("validate_{}\n", i));
        }

        // Generate output (async I/O)
        tokio::time::sleep(Duration::from_micros(300)).await;

        // Write files (async I/O)
        tokio::time::sleep(Duration::from_micros(100)).await;

        "Command completed".to_string()
    }

    group.bench_function("cli_new_runtime", |b| {
        b.iter(|| {
            let rt = Runtime::new().unwrap();
            black_box(rt.block_on(complete_cli_command()));
        });
    });

    group.bench_function("cli_shared_runtime", |b| {
        b.iter(|| {
            black_box(SHARED_RUNTIME.block_on(complete_cli_command()));
        });
    });

    group.bench_function("cli_lazy_static", |b| {
        b.iter(|| {
            black_box(LAZY_RUNTIME.block_on(complete_cli_command()));
        });
    });

    group.finish();
}

// Benchmark 8: Thread pool efficiency
fn bench_thread_pool(c: &mut Criterion) {
    let mut group = c.benchmark_group("thread_pool");

    for num_threads in [1, 2, 4, 8].iter() {
        group.bench_with_input(
            BenchmarkId::new("threads", num_threads),
            num_threads,
            |b, &n| {
                b.iter(|| {
                    let rt = Builder::new_multi_thread()
                        .worker_threads(n)
                        .enable_all()
                        .build()
                        .unwrap();

                    black_box(rt.block_on(async {
                        // Spawn multiple concurrent tasks
                        let tasks: Vec<_> = (0..10)
                            .map(|_| tokio::spawn(simulate_template_generation()))
                            .collect();

                        for task in tasks {
                            task.await.unwrap();
                        }
                    }));
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_runtime_creation,
    bench_runtime_execution,
    bench_workload_types,
    bench_concurrent_commands,
    bench_memory_patterns,
    bench_startup_latency,
    bench_cli_simulation,
    bench_thread_pool
);

criterion_main!(benches);
