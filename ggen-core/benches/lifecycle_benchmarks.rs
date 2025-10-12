use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::lifecycle::{cache_key, load_state, save_state, run_phase, run_pipeline, Context, Make, LifecycleState};
use std::sync::Arc;
use std::path::PathBuf;
use tempfile::TempDir;
use std::collections::BTreeMap;

// ============================================================================
// BENCHMARK 1: Sequential vs Parallel Execution
// ============================================================================

fn create_test_workspace_make(workspaces: usize, parallel: bool) -> Make {
    let mut workspace_map = BTreeMap::new();

    for i in 0..workspaces {
        workspace_map.insert(
            format!("workspace_{}", i),
            ggen_core::lifecycle::Workspace {
                path: format!("ws{}", i),
                framework: None,
                runtime: None,
                package_manager: None,
            },
        );
    }

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "test".to_string(),
        ggen_core::lifecycle::Phase {
            description: Some("Test phase".to_string()),
            command: Some("echo test".to_string()),
            commands: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: Some(parallel),
        },
    );

    Make {
        project: ggen_core::lifecycle::Project {
            name: "benchmark".to_string(),
            project_type: Some("test".to_string()),
            version: Some("1.0.0".to_string()),
            description: Some("Benchmark project".to_string()),
        },
        workspace: Some(workspace_map),
        lifecycle,
        hooks: None,
    }
}

fn setup_workspace_dirs(temp: &TempDir, count: usize) -> std::io::Result<()> {
    for i in 0..count {
        let ws_dir = temp.path().join(format!("ws{}", i));
        std::fs::create_dir_all(&ws_dir)?;

        // Create state directory
        std::fs::create_dir_all(ws_dir.join(".ggen"))?;
    }
    Ok(())
}

fn bench_sequential_vs_parallel(c: &mut Criterion) {
    let mut group = c.benchmark_group("workspace_execution");
    group.sample_size(10); // Reduce sample size for faster benchmarks

    for workspace_count in [2, 4, 8].iter() {
        // Sequential execution benchmark
        group.bench_with_input(
            BenchmarkId::new("sequential", workspace_count),
            workspace_count,
            |b, &count| {
                let temp = TempDir::new().unwrap();
                setup_workspace_dirs(&temp, count).unwrap();

                let make = create_test_workspace_make(count, false);
                let ctx = Context::new(
                    temp.path().to_path_buf(),
                    Arc::new(make),
                    temp.path().join(".ggen/state.json"),
                    vec![],
                );

                b.iter(|| {
                    run_pipeline(&ctx, &["test".to_string()]).unwrap();
                });
            },
        );

        // Parallel execution benchmark
        group.bench_with_input(
            BenchmarkId::new("parallel", workspace_count),
            workspace_count,
            |b, &count| {
                let temp = TempDir::new().unwrap();
                setup_workspace_dirs(&temp, count).unwrap();

                let make = create_test_workspace_make(count, true);
                let ctx = Context::new(
                    temp.path().to_path_buf(),
                    Arc::new(make),
                    temp.path().join(".ggen/state.json"),
                    vec![],
                );

                b.iter(|| {
                    run_pipeline(&ctx, &["test".to_string()]).unwrap();
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 2: Cache Performance
// ============================================================================

fn bench_cache_key_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_key_generation");

    let phase_names = vec!["build", "test", "deploy"];
    let commands = vec![
        "cargo build --release",
        "cargo test --all-features",
        "docker build -t app:latest .",
    ];
    let env_vars: Vec<(String, String)> = vec![
        ("PATH".to_string(), "/usr/bin:/bin".to_string()),
        ("HOME".to_string(), "/home/user".to_string()),
        ("CARGO_HOME".to_string(), "/home/user/.cargo".to_string()),
    ];

    // Benchmark cache key generation with varying complexity
    for cmd_count in [1, 5, 10].iter() {
        let cmds: Vec<String> = commands.iter().cycle().take(*cmd_count).map(|s| s.to_string()).collect();

        group.bench_with_input(
            BenchmarkId::new("commands", cmd_count),
            cmd_count,
            |b, _| {
                b.iter(|| {
                    for phase in &phase_names {
                        black_box(cache_key(phase, &cmds, &env_vars, &[]));
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_cache_hit_vs_miss(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_operations");

    let temp = TempDir::new().unwrap();
    let cache_dir = temp.path().join("cache");
    std::fs::create_dir_all(&cache_dir).unwrap();

    let phase = "build";
    let key = "abc123def456";

    // Create cache marker for hit scenario
    let cache_path = cache_dir.join(phase);
    std::fs::create_dir_all(&cache_path).unwrap();
    std::fs::write(cache_path.join(key), "").unwrap();

    group.bench_function("cache_hit", |b| {
        b.iter(|| {
            black_box(ggen_core::lifecycle::cache::is_cache_valid(&cache_dir, phase, key));
        });
    });

    group.bench_function("cache_miss", |b| {
        b.iter(|| {
            black_box(ggen_core::lifecycle::cache::is_cache_valid(&cache_dir, phase, "nonexistent"));
        });
    });

    group.finish();
}

fn bench_cache_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_memory");
    group.throughput(Throughput::Elements(1000));

    let commands: Vec<String> = (0..1000)
        .map(|i| format!("echo 'command {}'", i))
        .collect();

    group.bench_function("1000_cache_keys", |b| {
        b.iter(|| {
            let keys: Vec<String> = commands
                .iter()
                .map(|cmd| cache_key("bench", &[cmd.clone()], &[], &[]))
                .collect();
            black_box(keys);
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 3: Hook Execution
// ============================================================================

fn create_make_with_hooks(hook_count: usize) -> Make {
    let mut lifecycle = BTreeMap::new();

    // Create hook phases
    for i in 0..hook_count {
        lifecycle.insert(
            format!("hook_{}", i),
            ggen_core::lifecycle::Phase {
                description: None,
                command: Some("echo hook".to_string()),
                commands: None,
                watch: None,
                port: None,
                outputs: None,
                cache: None,
                workspaces: None,
                parallel: None,
            },
        );
    }

    // Create main phase
    lifecycle.insert(
        "main".to_string(),
        ggen_core::lifecycle::Phase {
            description: None,
            command: Some("echo main".to_string()),
            commands: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        },
    );

    // Create hooks
    let before_hooks: Vec<String> = (0..hook_count)
        .map(|i| format!("hook_{}", i))
        .collect();

    Make {
        project: ggen_core::lifecycle::Project {
            name: "hook_bench".to_string(),
            project_type: Some("test".to_string()),
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: Some(ggen_core::lifecycle::Hooks {
            before_all: Some(before_hooks),
            after_all: None,
            before_init: None,
            after_init: None,
            before_setup: None,
            after_setup: None,
            before_build: None,
            after_build: None,
            before_test: None,
            after_test: None,
            before_deploy: None,
            after_deploy: None,
        }),
    }
}

fn bench_hook_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("hook_execution");
    group.sample_size(10);

    // Single hook overhead
    group.bench_function("single_hook", |b| {
        let temp = TempDir::new().unwrap();
        let make = create_make_with_hooks(1);
        let ctx = Context::new(
            temp.path().to_path_buf(),
            Arc::new(make),
            temp.path().join(".ggen/state.json"),
            vec![],
        );

        b.iter(|| {
            run_phase(&ctx, "main").unwrap();
        });
    });

    // 10 hooks in sequence
    group.bench_function("10_hooks", |b| {
        let temp = TempDir::new().unwrap();
        let make = create_make_with_hooks(10);
        let ctx = Context::new(
            temp.path().to_path_buf(),
            Arc::new(make),
            temp.path().join(".ggen/state.json"),
            vec![],
        );

        b.iter(|| {
            run_phase(&ctx, "main").unwrap();
        });
    });

    group.finish();
}

fn bench_hook_recursion_detection(c: &mut Criterion) {
    let temp = TempDir::new().unwrap();

    // Create a simple make config for recursion detection
    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "test".to_string(),
        ggen_core::lifecycle::Phase {
            description: None,
            command: Some("echo test".to_string()),
            commands: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        },
    );

    let make = Make {
        project: ggen_core::lifecycle::Project {
            name: "recursion_bench".to_string(),
            project_type: Some("test".to_string()),
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp.path().to_path_buf(),
        Arc::new(make),
        temp.path().join(".ggen/state.json"),
        vec![],
    );

    c.bench_function("hook_recursion_detection", |b| {
        b.iter(|| {
            // The recursion detection happens in enter_phase
            run_phase(&ctx, "test").unwrap();
        });
    });
}

// ============================================================================
// BENCHMARK 4: State Persistence
// ============================================================================

fn bench_state_save(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_persistence");

    let temp = TempDir::new().unwrap();
    let state_path = temp.path().join(".ggen/state.json");

    // Create state with varying sizes
    for record_count in [10, 100, 1000].iter() {
        let mut state = LifecycleState::default();

        for i in 0..*record_count {
            state.record_run(
                format!("phase_{}", i % 5),
                1000000 + i as u128,
                100 + (i % 50) as u128,
                true,
            );
            state.add_cache_key(
                format!("phase_{}", i % 5),
                format!("key_{:x}", i),
            );
        }

        group.bench_with_input(
            BenchmarkId::new("save", record_count),
            record_count,
            |b, _| {
                b.iter(|| {
                    save_state(&state_path, &state).unwrap();
                });
            },
        );
    }

    group.finish();
}

fn bench_state_load(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_load");

    let temp = TempDir::new().unwrap();
    let state_path = temp.path().join(".ggen/state.json");

    // Create and save states with varying sizes
    for record_count in [10, 100, 1000].iter() {
        let mut state = LifecycleState::default();

        for i in 0..*record_count {
            state.record_run(
                format!("phase_{}", i % 5),
                1000000 + i as u128,
                100 + (i % 50) as u128,
                true,
            );
            state.add_cache_key(
                format!("phase_{}", i % 5),
                format!("key_{:x}", i),
            );
        }

        save_state(&state_path, &state).unwrap();

        group.bench_with_input(
            BenchmarkId::new("load", record_count),
            record_count,
            |b, _| {
                b.iter(|| {
                    black_box(load_state(&state_path).unwrap());
                });
            },
        );
    }

    group.finish();
}

fn bench_state_size(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_size");
    group.throughput(Throughput::Elements(1000));

    let temp = TempDir::new().unwrap();
    let state_path = temp.path().join(".ggen/state.json");

    group.bench_function("size_1000_records", |b| {
        let mut state = LifecycleState::default();

        for i in 0..1000 {
            state.record_run(
                format!("phase_{}", i % 5),
                1000000 + i as u128,
                100 + (i % 50) as u128,
                true,
            );
            state.add_cache_key(
                format!("phase_{}", i % 5),
                format!("key_{:x}", i),
            );
        }

        b.iter(|| {
            save_state(&state_path, &state).unwrap();
            let metadata = std::fs::metadata(&state_path).unwrap();
            black_box(metadata.len());
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    sequential_parallel,
    bench_sequential_vs_parallel
);

criterion_group!(
    cache_benches,
    bench_cache_key_generation,
    bench_cache_hit_vs_miss,
    bench_cache_memory_usage
);

criterion_group!(
    hook_benches,
    bench_hook_execution,
    bench_hook_recursion_detection
);

criterion_group!(
    state_benches,
    bench_state_save,
    bench_state_load,
    bench_state_size
);

criterion_main!(
    sequential_parallel,
    cache_benches,
    hook_benches,
    state_benches
);
