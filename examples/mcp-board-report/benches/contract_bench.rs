//! Contract execution benchmarks for MCP+
//!
//! Benchmarks:
//! - Contract registration
//! - Simple operation execution
//! - Execution with receipt generation
//! - Execution with signed receipt
//! - Refusal generation
//! - Envelope checking
//!
//! Target SLOs:
//! - Contract execution: < 1ms

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use mcp_contract::engine::{ContractDefinition, ContractEngine};
use mcp_contract::executor::ContractExecutor;
use mcp_contract::state::ContractState;
use mcp_core::crypto::KeyPair;
use mcp_core::types::{Capability, Envelope};

/// Setup a fresh contract engine with a registered contract
fn setup_engine() -> ContractEngine {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let mut engine = ContractEngine::new(keypair);

    let def = ContractDefinition::new("bench-contract", "bench-family", Envelope::default());
    engine.register(def).expect("Failed to register contract");

    engine
}

/// Setup executor and state for direct benchmarking
fn setup_executor() -> (ContractExecutor, ContractState) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let envelope = Envelope::default()
        .with_capability(Capability::Write)
        .with_capability(Capability::Execute);
    let executor = ContractExecutor::new(keypair, envelope);
    let state = ContractState::new("bench-contract", "bench-family");
    (executor, state)
}

/// Benchmark contract registration
fn bench_contract_register(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("contract_register", |b| {
        b.iter_with_setup(
            || {
                let engine = ContractEngine::new(keypair.clone());
                let def = ContractDefinition::new("new-contract", "family", Envelope::default());
                (engine, def)
            },
            |(mut engine, def)| {
                engine.register(black_box(def)).ok();
            },
        )
    });
}

/// Benchmark simple operation execution (minimal work)
fn bench_execute_simple_operation(c: &mut Criterion) {
    let (executor, mut state) = setup_executor();
    let input = b"simple input data";

    c.bench_function("execute_simple_operation", |b| {
        b.iter(|| {
            executor.execute(
                &mut state,
                black_box("simple.operation"),
                black_box(input),
                Capability::Read,
                || Ok(42),
            )
        })
    });
}

/// Benchmark execution with receipt generation (includes signing)
/// Target SLO: < 1ms
fn bench_execute_with_receipt(c: &mut Criterion) {
    let (executor, mut state) = setup_executor();
    let input = b"input data for receipt";

    c.bench_function("execute_with_receipt", |b| {
        b.iter(|| {
            executor.execute(
                &mut state,
                black_box("receipt.operation"),
                black_box(input),
                Capability::Read,
                || Ok("operation result"),
            )
        })
    });
}

/// Benchmark execution with signed receipt and verification
fn bench_execute_with_signed_receipt(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let envelope = Envelope::default();
    let executor = ContractExecutor::new(keypair.clone(), envelope);
    let mut state = ContractState::new("bench-contract", "bench-family");
    let input = b"input for signed receipt";

    c.bench_function("execute_with_signed_receipt", |b| {
        b.iter(|| {
            let result = executor.execute(
                &mut state,
                black_box("signed.operation"),
                black_box(input),
                Capability::Read,
                || Ok(123),
            );
            // Verify the receipt signature
            if let mcp_contract::executor::ExecutionResult::Success { receipt, .. } = result {
                let _ = receipt.verify(&keypair);
            }
        })
    });
}

/// Benchmark refusal generation (capability violation)
fn bench_refusal_generation(c: &mut Criterion) {
    let (executor, mut state) = setup_executor();
    let input = b"input for refused operation";

    // Remove Admin capability to trigger refusal
    c.bench_function("refusal_generation", |b| {
        b.iter(|| {
            executor.execute(
                &mut state,
                black_box("admin.operation"),
                black_box(input),
                Capability::Admin, // Not in envelope
                || Ok("should not run"),
            )
        })
    });
}

/// Benchmark envelope capability checking
fn bench_envelope_check(c: &mut Criterion) {
    let envelope = Envelope::default()
        .with_capability(Capability::Read)
        .with_capability(Capability::Write)
        .with_capability(Capability::Execute)
        .with_max_operations(10000)
        .with_disallowed_pattern("password")
        .with_disallowed_pattern("secret");

    c.bench_function("envelope_check", |b| {
        b.iter(|| {
            let has_read = envelope.has_capability(black_box(Capability::Read));
            let has_admin = envelope.has_capability(black_box(Capability::Admin));
            (has_read, has_admin)
        })
    });
}

/// Benchmark contract state creation
fn bench_state_creation(c: &mut Criterion) {
    c.bench_function("contract_state_creation", |b| {
        b.iter(|| {
            ContractState::new(black_box("contract-id"), black_box("contract-family"))
        })
    });
}

/// Benchmark kill switch checking
fn bench_kill_switch_check(c: &mut Criterion) {
    let mut state = ContractState::new("contract", "family");
    state.kill_switch.kill_family("other-family");

    c.bench_function("kill_switch_check", |b| {
        b.iter(|| state.can_execute())
    });
}

/// Benchmark epoch rotation
fn bench_epoch_rotation(c: &mut Criterion) {
    c.bench_function("epoch_rotation", |b| {
        b.iter_with_setup(
            || {
                let mut state = ContractState::new("contract", "family");
                // Add some receipts
                for i in 0..10 {
                    let hash =
                        mcp_core::crypto::hash_sha256_bytes(format!("receipt-{}", i).as_bytes());
                    state.add_receipt(&hash);
                }
                state
            },
            |mut state| state.rotate_epoch(),
        )
    });
}

/// Benchmark engine-level execution through full pipeline
fn bench_engine_execute(c: &mut Criterion) {
    c.bench_function("engine_execute", |b| {
        b.iter_with_setup(
            setup_engine,
            |mut engine| {
                engine
                    .execute(
                        "bench-contract",
                        "engine.operation",
                        b"input data",
                        Capability::Read,
                        || Ok("result"),
                    )
                    .ok()
            },
        )
    });
}

/// Benchmark multiple sequential operations
fn bench_sequential_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("sequential_operations");

    for count in [10, 50, 100] {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            b.iter_with_setup(
                setup_executor,
                |(executor, mut state)| {
                    for i in 0..count {
                        let input = format!("input-{}", i);
                        executor.execute(
                            &mut state,
                            "seq.operation",
                            input.as_bytes(),
                            Capability::Read,
                            || Ok(i),
                        );
                    }
                },
            )
        });
    }

    group.finish();
}

/// Benchmark engine metrics collection
fn bench_engine_metrics(c: &mut Criterion) {
    c.bench_function("engine_metrics", |b| {
        b.iter_with_setup(
            || {
                let mut engine = setup_engine();
                // Execute some operations
                for i in 0..10 {
                    let input = format!("input-{}", i);
                    let _ = engine.execute(
                        "bench-contract",
                        "op",
                        input.as_bytes(),
                        Capability::Read,
                        || Ok(i),
                    );
                }
                engine
            },
            |engine| engine.metrics(),
        )
    });
}

/// Benchmark bundle generation
fn bench_bundle_generation(c: &mut Criterion) {
    c.bench_function("bundle_generation", |b| {
        b.iter_with_setup(
            || {
                let mut engine = setup_engine();
                // Execute operations to build receipt chain
                for i in 0..100 {
                    let input = format!("input-{}", i);
                    let _ = engine.execute(
                        "bench-contract",
                        "op",
                        input.as_bytes(),
                        Capability::Read,
                        || Ok(i),
                    );
                }
                engine
            },
            |engine| engine.generate_bundle("bench-contract"),
        )
    });
}

/// Benchmark envelope construction
fn bench_envelope_construction(c: &mut Criterion) {
    c.bench_function("envelope_construction", |b| {
        b.iter(|| {
            Envelope::default()
                .with_capability(Capability::Read)
                .with_capability(Capability::Write)
                .with_capability(Capability::Execute)
                .with_max_operations(1000)
                .with_disallowed_pattern("password")
                .with_disallowed_pattern("secret")
                .with_disallowed_pattern("api_key")
        })
    });
}

criterion_group!(
    benches,
    bench_contract_register,
    bench_execute_simple_operation,
    bench_execute_with_receipt,
    bench_execute_with_signed_receipt,
    bench_refusal_generation,
    bench_envelope_check,
    bench_state_creation,
    bench_kill_switch_check,
    bench_epoch_rotation,
    bench_engine_execute,
    bench_sequential_operations,
    bench_engine_metrics,
    bench_bundle_generation,
    bench_envelope_construction,
);

criterion_main!(benches);
