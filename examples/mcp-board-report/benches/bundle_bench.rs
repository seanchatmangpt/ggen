//! Evidence bundle benchmarks for MCP+
//!
//! Benchmarks:
//! - Bundle creation
//! - Bundle with many receipts
//! - Attestation addition
//! - Bundle serialization/deserialization

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use mcp_core::crypto::{hash_sha256, hash_sha256_bytes, KeyPair};
use mcp_core::types::{EvidenceBundle, ExecutionMetrics, Receipt};
use mcp_core::GENESIS_HASH;
use mcp_merkle::MerkleTree;

/// Create test execution metrics
fn create_metrics() -> ExecutionMetrics {
    ExecutionMetrics {
        duration_us: 1000,
        memory_bytes: 1024,
        operations: 1,
        envelope_utilization: 0.5,
    }
}

/// Create a chain of signed receipts and return the Merkle root
fn create_receipt_chain(count: usize, keypair: &KeyPair) -> (Vec<Receipt>, MerkleTree) {
    let mut tree = MerkleTree::new();
    let mut receipts = Vec::with_capacity(count);
    let mut prev_hash = GENESIS_HASH.to_string();

    for i in 0..count {
        let metrics = create_metrics();
        let mut receipt = Receipt::new(
            (i + 1) as u64,
            &prev_hash,
            format!("operation.{}", i),
            "contract-bundle-test",
            hash_sha256(format!("input-{}", i).as_bytes()),
            hash_sha256(format!("output-{}", i).as_bytes()),
            metrics,
        );

        receipt.sign(keypair);
        prev_hash = receipt.chain_hash().to_string();

        // Add to Merkle tree
        let receipt_hash = hash_sha256_bytes(receipt.receipt_hash.as_bytes());
        tree.add_leaf(receipt_hash);

        receipts.push(receipt);
    }

    tree.rebuild();
    (receipts, tree)
}

/// Benchmark empty bundle creation
fn bench_bundle_creation(c: &mut Criterion) {
    c.bench_function("bundle_creation", |b| {
        b.iter(|| EvidenceBundle::new_empty(black_box("gpt-4")))
    });
}

/// Benchmark bundle creation with 100 receipts
fn bench_bundle_with_100_receipts(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("bundle_with_100_receipts", |b| {
        b.iter(|| {
            let (receipts, tree) = create_receipt_chain(100, &keypair);
            let mut bundle = EvidenceBundle::new_empty("gpt-4");
            bundle.receipt_chain_root = tree.root_hex().unwrap_or_default();
            bundle.receipt_count = receipts.len() as u64;
            bundle
        })
    });
}

/// Benchmark bundle creation with 1000 receipts
fn bench_bundle_with_1000_receipts(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("bundle_with_1000_receipts", |b| {
        b.iter(|| {
            let (receipts, tree) = create_receipt_chain(1000, &keypair);
            let mut bundle = EvidenceBundle::new_empty("gpt-4");
            bundle.receipt_chain_root = tree.root_hex().unwrap_or_default();
            bundle.receipt_count = receipts.len() as u64;
            bundle
        })
    });
}

/// Benchmark attestation addition
fn bench_attestation_addition(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("attestation_addition", |b| {
        b.iter_with_setup(
            || {
                let mut bundle = EvidenceBundle::new_empty("gpt-4");
                // Add some initial attestations
                for i in 0..5 {
                    let sig = keypair.sign(format!("attestation-{}", i).as_bytes());
                    bundle.add_attestation(sig.to_hex());
                }
                bundle
            },
            |mut bundle| {
                let sig = keypair.sign(b"new-attestation");
                bundle.add_attestation(sig.to_hex());
                bundle
            },
        )
    });
}

/// Benchmark bundle serialization
fn bench_bundle_serialization(c: &mut Criterion) {
    let mut bundle = EvidenceBundle::new_empty("gpt-4");
    bundle.receipt_count = 1000;
    bundle.refusal_count = 50;

    // Add some attestations
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    for i in 0..10 {
        let sig = keypair.sign(format!("attestation-{}", i).as_bytes());
        bundle.add_attestation(sig.to_hex());
    }

    c.bench_function("bundle_serialization", |b| {
        b.iter(|| serde_json::to_string(black_box(&bundle)))
    });
}

/// Benchmark bundle deserialization
fn bench_bundle_deserialization(c: &mut Criterion) {
    let mut bundle = EvidenceBundle::new_empty("gpt-4");
    bundle.receipt_count = 1000;
    bundle.refusal_count = 50;

    // Add attestations
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    for i in 0..10 {
        let sig = keypair.sign(format!("attestation-{}", i).as_bytes());
        bundle.add_attestation(sig.to_hex());
    }

    let json = serde_json::to_string(&bundle).expect("Failed to serialize");

    c.bench_function("bundle_deserialization", |b| {
        b.iter(|| serde_json::from_str::<EvidenceBundle>(black_box(&json)))
    });
}

/// Benchmark bundle hash computation
fn bench_bundle_hash(c: &mut Criterion) {
    c.bench_function("bundle_hash_computation", |b| {
        b.iter(|| {
            let hash_input = format!(
                "1.0.0|{}|{}|{}|{}|{}|{}|{}|{}",
                "bundle-abc123",
                "2024-01-01T00:00:00Z",
                "2024-01-01T00:00:00Z",
                "2024-01-02T00:00:00Z",
                "gpt-4",
                GENESIS_HASH,
                1000,
                50
            );
            hash_sha256(black_box(hash_input.as_bytes()))
        })
    });
}

/// Benchmark bundle scaling with different receipt counts
fn bench_bundle_scaling(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");
    let sizes = [10, 50, 100, 500];

    let mut group = c.benchmark_group("bundle_scaling");

    for size in sizes {
        group.throughput(Throughput::Elements(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let (receipts, tree) = create_receipt_chain(size, &keypair);
                let mut bundle = EvidenceBundle::new_empty("gpt-4");
                bundle.receipt_chain_root = tree.root_hex().unwrap_or_default();
                bundle.receipt_count = receipts.len() as u64;
                bundle
            })
        });
    }

    group.finish();
}

/// Benchmark multiple attestation additions
fn bench_multiple_attestations(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    let mut group = c.benchmark_group("multiple_attestations");

    for count in [1, 5, 10, 20] {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            b.iter_with_setup(
                || EvidenceBundle::new_empty("gpt-4"),
                |mut bundle| {
                    for i in 0..count {
                        let sig = keypair.sign(format!("attestation-{}", i).as_bytes());
                        bundle.add_attestation(sig.to_hex());
                    }
                    bundle
                },
            )
        });
    }

    group.finish();
}

/// Benchmark full bundle workflow (create receipts + tree + bundle + attestations)
fn bench_full_bundle_workflow(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    c.bench_function("full_bundle_workflow_100", |b| {
        b.iter(|| {
            // Create receipt chain
            let (receipts, tree) = create_receipt_chain(100, &keypair);

            // Create bundle
            let mut bundle = EvidenceBundle::new_empty("gpt-4");
            bundle.receipt_chain_root = tree.root_hex().unwrap_or_default();
            bundle.receipt_count = receipts.len() as u64;

            // Add attestations from multiple auditors
            for i in 0..3 {
                let auditor_key = KeyPair::generate().expect("Failed to generate key");
                let attestation_data = format!(
                    "ATTEST:{}:{}:{}",
                    bundle.bundle_id, bundle.receipt_chain_root, i
                );
                let sig = auditor_key.sign(attestation_data.as_bytes());
                bundle.add_attestation(sig.to_hex());
            }

            // Serialize for storage/transmission
            serde_json::to_string(&bundle)
        })
    });
}

/// Benchmark bundle verification (all attestation signatures)
fn bench_bundle_verification(c: &mut Criterion) {
    // Create bundle with known attestations
    let mut bundle = EvidenceBundle::new_empty("gpt-4");
    bundle.receipt_count = 100;

    let auditor_keys: Vec<KeyPair> = (0..5)
        .map(|_| KeyPair::generate().expect("Failed to generate key"))
        .collect();

    for keypair in &auditor_keys {
        let attestation_data = format!("ATTEST:{}:{}", bundle.bundle_id, bundle.receipt_chain_root);
        let sig = keypair.sign(attestation_data.as_bytes());
        bundle.add_attestation(sig.to_hex());
    }

    c.bench_function("bundle_attestation_verification", |b| {
        b.iter(|| {
            // Verify each attestation
            let attestation_data = format!("ATTEST:{}:{}", bundle.bundle_id, bundle.receipt_chain_root);

            bundle
                .attestations
                .iter()
                .zip(auditor_keys.iter())
                .all(|(sig_hex, keypair)| {
                    if let Ok(sig_bytes) = hex::decode(sig_hex) {
                        if let Ok(sig) = mcp_core::crypto::Signature::from_bytes(sig_bytes) {
                            return keypair.verify(attestation_data.as_bytes(), &sig).is_ok();
                        }
                    }
                    false
                })
        })
    });
}

/// Benchmark bundle JSON size estimation
fn bench_bundle_json_size(c: &mut Criterion) {
    let keypair = KeyPair::generate().expect("Failed to generate keypair");

    let mut group = c.benchmark_group("bundle_json_size");

    for receipt_count in [100, 500, 1000] {
        group.bench_with_input(
            BenchmarkId::from_parameter(receipt_count),
            &receipt_count,
            |b, &receipt_count| {
                b.iter_with_setup(
                    || {
                        let mut bundle = EvidenceBundle::new_empty("gpt-4");
                        bundle.receipt_count = receipt_count;

                        // Add typical attestations
                        for i in 0..10 {
                            let sig = keypair.sign(format!("attest-{}", i).as_bytes());
                            bundle.add_attestation(sig.to_hex());
                        }

                        bundle
                    },
                    |bundle| serde_json::to_string(&bundle).map(|s| s.len()),
                )
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_bundle_creation,
    bench_bundle_with_100_receipts,
    bench_bundle_with_1000_receipts,
    bench_attestation_addition,
    bench_bundle_serialization,
    bench_bundle_deserialization,
    bench_bundle_hash,
    bench_bundle_scaling,
    bench_multiple_attestations,
    bench_full_bundle_workflow,
    bench_bundle_verification,
    bench_bundle_json_size,
);

criterion_main!(benches);
