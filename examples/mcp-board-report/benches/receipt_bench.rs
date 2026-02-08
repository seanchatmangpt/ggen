//! Receipt chain benchmarks for MCP+
//!
//! Benchmarks:
//! - Receipt creation
//! - Receipt signing
//! - Receipt verification
//! - Chain appending (100 and 1000 receipts)
//!
//! Target SLOs:
//! - Receipt creation: < 50us

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use mcp_core::crypto::{hash_sha256, hash_sha256_bytes, KeyPair};
use mcp_core::types::{ExecutionMetrics, Receipt};
use mcp_core::GENESIS_HASH;

/// Create test execution metrics
fn create_metrics() -> ExecutionMetrics {
    ExecutionMetrics {
        duration_us: 1000,
        memory_bytes: 1024,
        operations: 1,
        envelope_utilization: 0.5,
    }
}

/// Benchmark receipt creation
/// Target SLO: < 50us
fn bench_receipt_creation(c: &mut Criterion) {
    let metrics = create_metrics();

    c.bench_function("receipt_creation", |b| {
        b.iter(|| {
            Receipt::new(
                black_box(1),
                black_box(GENESIS_HASH),
                black_box("test.operation"),
                black_box("contract-001"),
                black_box("input-hash-abc123"),
                black_box("output-hash-def456"),
                metrics.clone(),
            )
        })
    });
}

/// Benchmark receipt signing
fn bench_receipt_signing(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let metrics = create_metrics();

    c.bench_function("receipt_signing", |b| {
        b.iter_with_setup(
            || {
                Receipt::new(
                    1,
                    GENESIS_HASH,
                    "test.operation",
                    "contract-001",
                    "input-hash",
                    "output-hash",
                    metrics.clone(),
                )
            },
            |mut receipt| receipt.sign(&keypair),
        )
    });
}

/// Benchmark receipt verification
fn bench_receipt_verification(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let metrics = create_metrics();
    let mut receipt = Receipt::new(
        1,
        GENESIS_HASH,
        "test.operation",
        "contract-001",
        "input-hash",
        "output-hash",
        metrics,
    );
    receipt.sign(&keypair);

    c.bench_function("receipt_verification", |b| {
        b.iter(|| receipt.verify(black_box(&keypair)))
    });
}

/// Benchmark chaining 100 receipts
fn bench_chain_append_100(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("chain_append_100", |b| {
        b.iter(|| {
            let mut prev_hash = GENESIS_HASH.to_string();
            let mut receipts = Vec::with_capacity(100);

            for i in 0..100 {
                let metrics = ExecutionMetrics {
                    duration_us: 1000 + i as u64,
                    memory_bytes: 1024,
                    operations: 1,
                    envelope_utilization: (i as f64) / 100.0,
                };

                let mut receipt = Receipt::new(
                    i + 1,
                    &prev_hash,
                    format!("operation.{}", i),
                    "contract-001",
                    hash_sha256(format!("input-{}", i).as_bytes()),
                    hash_sha256(format!("output-{}", i).as_bytes()),
                    metrics,
                );

                receipt.sign(&keypair);
                prev_hash = receipt.chain_hash().to_string();
                receipts.push(receipt);
            }

            receipts
        })
    });
}

/// Benchmark chaining 1000 receipts
fn bench_chain_append_1000(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("chain_append_1000", |b| {
        b.iter(|| {
            let mut prev_hash = GENESIS_HASH.to_string();
            let mut receipts = Vec::with_capacity(1000);

            for i in 0..1000 {
                let metrics = ExecutionMetrics {
                    duration_us: 1000,
                    memory_bytes: 1024,
                    operations: 1,
                    envelope_utilization: 0.5,
                };

                let mut receipt = Receipt::new(
                    i + 1,
                    &prev_hash,
                    "operation",
                    "contract",
                    format!("in-{}", i),
                    format!("out-{}", i),
                    metrics,
                );

                receipt.sign(&keypair);
                prev_hash = receipt.chain_hash().to_string();
                receipts.push(receipt);
            }

            receipts
        })
    });
}

/// Benchmark receipt hash computation
fn bench_receipt_hash(c: &mut Criterion) {
    c.bench_function("receipt_hash_computation", |b| {
        b.iter(|| {
            let hash_input = format!(
                "{}|{}|{}|{}|{}|{}|{}|{}|{}",
                "rcpt-12345-1",
                1,
                GENESIS_HASH,
                "test.operation",
                "contract-001",
                "input-hash",
                "output-hash",
                1000,
                "2024-01-01T00:00:00Z"
            );
            hash_sha256(black_box(hash_input.as_bytes()))
        })
    });
}

/// Benchmark receipt serialization to JSON
fn bench_receipt_serialization(c: &mut Criterion) {
    let metrics = create_metrics();
    let mut receipt = Receipt::new(
        1,
        GENESIS_HASH,
        "test.operation",
        "contract-001",
        "input-hash",
        "output-hash",
        metrics,
    );
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    receipt.sign(&keypair);

    c.bench_function("receipt_serialization", |b| {
        b.iter(|| serde_json::to_string(black_box(&receipt)))
    });
}

/// Benchmark receipt deserialization from JSON
fn bench_receipt_deserialization(c: &mut Criterion) {
    let metrics = create_metrics();
    let mut receipt = Receipt::new(
        1,
        GENESIS_HASH,
        "test.operation",
        "contract-001",
        "input-hash",
        "output-hash",
        metrics,
    );
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    receipt.sign(&keypair);
    let json = serde_json::to_string(&receipt).expect("Failed to serialize");

    c.bench_function("receipt_deserialization", |b| {
        b.iter(|| serde_json::from_str::<Receipt>(black_box(&json)))
    });
}

/// Benchmark receipt chain scaling
fn bench_chain_scaling(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let sizes = [10, 50, 100, 500];

    let mut group = c.benchmark_group("chain_scaling");

    for size in sizes {
        group.throughput(Throughput::Elements(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let mut prev_hash = GENESIS_HASH.to_string();

                for i in 0..size {
                    let metrics = create_metrics();
                    let mut receipt = Receipt::new(
                        i + 1,
                        &prev_hash,
                        "op",
                        "contract",
                        "in",
                        "out",
                        metrics,
                    );
                    receipt.sign(&keypair);
                    prev_hash = receipt.chain_hash().to_string();
                }

                prev_hash
            })
        });
    }

    group.finish();
}

/// Benchmark building a receipt chain and then verifying all signatures
fn bench_chain_build_and_verify(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("chain_build_and_verify_50", |b| {
        b.iter(|| {
            let mut prev_hash = GENESIS_HASH.to_string();
            let mut receipts = Vec::with_capacity(50);

            // Build chain
            for i in 0..50 {
                let metrics = create_metrics();
                let mut receipt = Receipt::new(
                    i + 1,
                    &prev_hash,
                    "operation",
                    "contract",
                    format!("in-{}", i),
                    format!("out-{}", i),
                    metrics,
                );
                receipt.sign(&keypair);
                prev_hash = receipt.chain_hash().to_string();
                receipts.push(receipt);
            }

            // Verify all
            let all_valid = receipts.iter().all(|r| r.verify(&keypair).is_ok());
            (receipts, all_valid)
        })
    });
}

/// Benchmark adding receipt to Merkle tree (simulating contract state)
fn bench_receipt_to_merkle(c: &mut Criterion) {
    use mcp_merkle::MerkleTree;

    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let metrics = create_metrics();

    c.bench_function("receipt_to_merkle", |b| {
        b.iter_with_setup(
            || {
                let mut tree = MerkleTree::new();
                // Pre-populate with some receipts
                for i in 0..99 {
                    tree.add_leaf(hash_sha256_bytes(format!("receipt-{}", i).as_bytes()));
                }
                tree.rebuild();

                let mut receipt = Receipt::new(
                    100,
                    tree.root_hex().unwrap_or_default(),
                    "operation",
                    "contract",
                    "input",
                    "output",
                    metrics.clone(),
                );
                receipt.sign(&keypair);

                (tree, receipt)
            },
            |(mut tree, receipt)| {
                let receipt_hash = hash_sha256_bytes(receipt.receipt_hash.as_bytes());
                tree.add_leaf(receipt_hash);
                tree.rebuild();
                tree.root_hex()
            },
        )
    });
}

criterion_group!(
    benches,
    bench_receipt_creation,
    bench_receipt_signing,
    bench_receipt_verification,
    bench_chain_append_100,
    bench_chain_append_1000,
    bench_receipt_hash,
    bench_receipt_serialization,
    bench_receipt_deserialization,
    bench_chain_scaling,
    bench_chain_build_and_verify,
    bench_receipt_to_merkle,
);

criterion_main!(benches);
