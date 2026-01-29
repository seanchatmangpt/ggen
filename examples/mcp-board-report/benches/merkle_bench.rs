//! Merkle tree operation benchmarks for MCP+
//!
//! Benchmarks:
//! - Tree building (various leaf counts)
//! - Proof generation
//! - Proof verification
//! - Batch verification
//!
//! Target SLOs:
//! - Merkle tree (1000 leaves): < 10ms

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use mcp_core::crypto::hash_sha256_bytes;
use mcp_merkle::proof::verify_batch;
use mcp_merkle::MerkleTree;

/// Create test leaves (pre-computed hashes)
fn create_leaves(count: usize) -> Vec<[u8; 32]> {
    (0..count)
        .map(|i| hash_sha256_bytes(format!("leaf-data-{}", i).as_bytes()))
        .collect()
}

/// Benchmark tree building with 10 leaves
fn bench_tree_build_10(c: &mut Criterion) {
    let leaves = create_leaves(10);

    c.bench_function("merkle_tree_build_10", |b| {
        b.iter(|| MerkleTree::from_leaves(black_box(leaves.clone())))
    });
}

/// Benchmark tree building with 100 leaves
fn bench_tree_build_100(c: &mut Criterion) {
    let leaves = create_leaves(100);

    c.bench_function("merkle_tree_build_100", |b| {
        b.iter(|| MerkleTree::from_leaves(black_box(leaves.clone())))
    });
}

/// Benchmark tree building with 1000 leaves
/// Target SLO: < 10ms
fn bench_tree_build_1000(c: &mut Criterion) {
    let leaves = create_leaves(1000);

    c.bench_function("merkle_tree_build_1000", |b| {
        b.iter(|| MerkleTree::from_leaves(black_box(leaves.clone())))
    });
}

/// Benchmark tree building with 10000 leaves
fn bench_tree_build_10000(c: &mut Criterion) {
    let leaves = create_leaves(10000);

    c.bench_function("merkle_tree_build_10000", |b| {
        b.iter(|| MerkleTree::from_leaves(black_box(leaves.clone())))
    });
}

/// Benchmark proof generation
fn bench_proof_generation(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves);

    c.bench_function("merkle_proof_generation", |b| {
        b.iter(|| {
            // Generate proof for leaf at index 500
            tree.proof(black_box(500))
        })
    });
}

/// Benchmark proof verification
fn bench_proof_verification(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves);
    let proof = tree.proof(500).expect("Proof should exist");

    c.bench_function("merkle_proof_verification", |b| {
        b.iter(|| proof.verify())
    });
}

/// Benchmark batch verification
fn bench_batch_verification(c: &mut Criterion) {
    let leaves = create_leaves(100);
    let tree = MerkleTree::from_leaves(leaves);
    let root = tree.root().expect("Root should exist");

    // Generate proofs for all leaves
    let proofs: Vec<_> = (0..100).filter_map(|i| tree.proof(i)).collect();

    c.bench_function("merkle_batch_verification_100", |b| {
        b.iter(|| verify_batch(black_box(&proofs), black_box(&root)))
    });
}

/// Benchmark incremental tree building (add one leaf at a time)
fn bench_incremental_build(c: &mut Criterion) {
    let mut group = c.benchmark_group("merkle_incremental");

    for size in [10, 100, 1000] {
        let leaves = create_leaves(size);

        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &leaves,
            |b, leaves| {
                b.iter(|| {
                    let mut tree = MerkleTree::new();
                    for leaf in leaves {
                        tree.add_leaf(*leaf);
                    }
                    tree.rebuild();
                    tree
                })
            },
        );
    }

    group.finish();
}

/// Benchmark tree scaling (throughput analysis)
fn bench_tree_scaling(c: &mut Criterion) {
    let sizes = [10, 50, 100, 500, 1000, 5000];

    let mut group = c.benchmark_group("merkle_scaling");

    for size in sizes {
        let leaves = create_leaves(size);
        group.throughput(Throughput::Elements(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &leaves, |b, leaves| {
            b.iter(|| MerkleTree::from_leaves(black_box(leaves.clone())))
        });
    }

    group.finish();
}

/// Benchmark proof generation at different tree depths
fn bench_proof_generation_by_depth(c: &mut Criterion) {
    let mut group = c.benchmark_group("merkle_proof_by_depth");

    for size in [8, 64, 512, 4096] {
        let leaves = create_leaves(size);
        let tree = MerkleTree::from_leaves(leaves);
        let mid_index = size / 2;

        group.bench_with_input(BenchmarkId::from_parameter(size), &tree, |b, tree| {
            b.iter(|| tree.proof(black_box(mid_index)))
        });
    }

    group.finish();
}

/// Benchmark root computation
fn bench_root_computation(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves);

    c.bench_function("merkle_root_hex", |b| b.iter(|| tree.root_hex()));
}

/// Benchmark leaf verification (presence check)
fn bench_leaf_verification(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves.clone());
    let target_leaf = leaves[500];
    let unknown_leaf = hash_sha256_bytes(b"unknown-leaf");

    let mut group = c.benchmark_group("merkle_leaf_verify");

    group.bench_function("present", |b| {
        b.iter(|| tree.verify_leaf(black_box(&target_leaf)))
    });

    group.bench_function("absent", |b| {
        b.iter(|| tree.verify_leaf(black_box(&unknown_leaf)))
    });

    group.finish();
}

/// Benchmark proof serialization to hex
fn bench_proof_to_hex(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves);
    let proof = tree.proof(500).expect("Proof should exist");

    c.bench_function("merkle_proof_to_hex", |b| b.iter(|| proof.to_hex()));
}

/// Benchmark proof deserialization from hex
fn bench_proof_from_hex(c: &mut Criterion) {
    let leaves = create_leaves(1000);
    let tree = MerkleTree::from_leaves(leaves);
    let proof = tree.proof(500).expect("Proof should exist");
    let hex_proof = proof.to_hex();

    c.bench_function("merkle_proof_from_hex", |b| {
        b.iter(|| hex_proof.to_binary())
    });
}

criterion_group!(
    benches,
    bench_tree_build_10,
    bench_tree_build_100,
    bench_tree_build_1000,
    bench_tree_build_10000,
    bench_proof_generation,
    bench_proof_verification,
    bench_batch_verification,
    bench_incremental_build,
    bench_tree_scaling,
    bench_proof_generation_by_depth,
    bench_root_computation,
    bench_leaf_verification,
    bench_proof_to_hex,
    bench_proof_from_hex,
);

criterion_main!(benches);
