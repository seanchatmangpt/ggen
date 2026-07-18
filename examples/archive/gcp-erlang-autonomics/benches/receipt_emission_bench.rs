use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use sha2::{Sha256, Digest};
use std::time::Instant;

/// Receipt represents cryptographic proof of autonomic system operation
#[derive(Debug, Clone)]
struct Receipt {
    id: String,
    execution_id: String,
    timestamp: u64,
    content_hash: String,
    parent_hash: String,
    actions_executed: Vec<String>,
}

/// ReceiptChain manages Merkle-linked receipt storage
struct ReceiptChain {
    receipts: Vec<Receipt>,
    last_hash: String,
}

impl ReceiptChain {
    fn new() -> Self {
        Self {
            receipts: Vec::new(),
            last_hash: "0000000000000000000000000000000000000000000000000000000000000000".to_string(),
        }
    }

    /// Emit and store a receipt with cryptographic linking
    /// Target SLO: ≤100ms per receipt (hash + store)
    fn emit_receipt(
        &mut self,
        execution_id: String,
        actions: Vec<String>,
    ) -> Result<Receipt, Box<dyn std::error::Error>> {
        let start = Instant::now();

        // Compute content hash
        let content = format!("{:?}", actions);
        let content_hash = Self::compute_hash(&content);

        // Link to previous receipt (Merkle chain)
        let parent_hash = self.last_hash.clone();

        // Create receipt
        let receipt = Receipt {
            id: format!("receipt-{}", self.receipts.len()),
            execution_id,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_millis() as u64,
            content_hash: content_hash.clone(),
            parent_hash,
            actions_executed: actions,
        };

        // Store in chain
        self.receipts.push(receipt.clone());
        self.last_hash = Self::compute_hash(&format!("{:?}", receipt));

        let elapsed = start.elapsed();

        // Verify SLO: ≤100ms per receipt
        assert!(
            elapsed.as_millis() <= 100,
            "Receipt emission exceeded SLO: {}ms",
            elapsed.as_millis()
        );

        Ok(receipt)
    }

    /// Verify receipt integrity
    fn verify_receipt(&self, receipt: &Receipt) -> Result<bool, Box<dyn std::error::Error>> {
        let start = Instant::now();

        // Find receipt in chain
        let stored = self
            .receipts
            .iter()
            .find(|r| r.id == receipt.id)
            .ok_or("Receipt not found")?;

        // Verify content hash
        let content = format!("{:?}", stored.actions_executed);
        let computed_hash = Self::compute_hash(&content);

        let valid = computed_hash == stored.content_hash;

        let elapsed = start.elapsed();
        assert!(
            elapsed.as_millis() <= 20,
            "Verification exceeded time budget: {}ms",
            elapsed.as_millis()
        );

        Ok(valid)
    }

    /// Compute SHA-256 hash
    fn compute_hash(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content);
        let result = hasher.finalize();
        hex::encode(result)
    }

    /// Get receipt count
    fn receipt_count(&self) -> usize {
        self.receipts.len()
    }

    /// Verify chain integrity (all links valid)
    fn verify_chain(&self) -> Result<bool, Box<dyn std::error::Error>> {
        let start = Instant::now();
        let mut current_hash = "0000000000000000000000000000000000000000000000000000000000000000".to_string();

        for receipt in &self.receipts {
            // Verify parent link
            if receipt.parent_hash != current_hash {
                return Ok(false);
            }

            // Compute this receipt's hash
            current_hash = Self::compute_hash(&format!("{:?}", receipt));
        }

        let elapsed = start.elapsed();
        assert!(
            elapsed.as_millis() <= 500,
            "Chain verification exceeded time budget: {}ms",
            elapsed.as_millis()
        );

        Ok(true)
    }
}

fn benchmark_receipt_emission(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_emit");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("single_receipt_emission", |b| {
        b.iter_batched(
            || {
                let chain = ReceiptChain::new();
                let actions = vec![
                    "scale_service".to_string(),
                    "enable_circuit_breaker".to_string(),
                    "alert_team".to_string(),
                ];
                (chain, actions)
            },
            |(mut chain, actions)| {
                let result = chain.emit_receipt(
                    black_box("exec-001".to_string()),
                    black_box(actions),
                );
                assert!(result.is_ok());
                chain.receipt_count()
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_hash_computation(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_hash");
    group.measurement_time(std::time::Duration::from_secs(10));

    for data_size in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}bytes", data_size)),
            data_size,
            |b, &data_size| {
                b.iter_batched(
                    || {
                        let content = "x".repeat(data_size);
                        content
                    },
                    |content| {
                        let start = Instant::now();
                        let hash = ReceiptChain::compute_hash(black_box(&content));
                        let elapsed = start.elapsed();

                        // Hash computation should be <5ms even for large payloads
                        assert!(
                            elapsed.as_millis() < 5,
                            "Hash computation exceeded budget: {}ms",
                            elapsed.as_millis()
                        );

                        hash
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

fn benchmark_receipt_verification(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_verify");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("verify_single_receipt", |b| {
        b.iter_batched(
            || {
                let mut chain = ReceiptChain::new();
                let actions = vec!["action1".to_string(), "action2".to_string()];
                let receipt = chain.emit_receipt("exec-001".to_string(), actions).unwrap();
                (chain, receipt)
            },
            |(chain, receipt)| {
                let result = chain.verify_receipt(black_box(&receipt));
                assert!(result.is_ok());
                assert!(result.unwrap());
                true
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_chain_verification(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_chain_verify");
    group.measurement_time(std::time::Duration::from_secs(15));

    for chain_length in [10, 100, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(chain_length),
            chain_length,
            |b, &chain_length| {
                b.iter_batched(
                    || {
                        let mut chain = ReceiptChain::new();
                        for i in 0..chain_length {
                            let actions = vec![
                                format!("action-{}-1", i),
                                format!("action-{}-2", i),
                            ];
                            let _ = chain.emit_receipt(format!("exec-{:04}", i), actions);
                        }
                        chain
                    },
                    |chain| {
                        let result = chain.verify_chain();
                        assert!(result.is_ok());
                        assert!(result.unwrap());
                        chain.receipt_count()
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

fn benchmark_receipt_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("receipt_throughput");
    group.measurement_time(std::time::Duration::from_secs(15));

    group.bench_function("1000_receipts_per_second", |b| {
        b.iter_batched(
            || {
                let chain = ReceiptChain::new();
                (0..1000)
                    .map(|i| {
                        (
                            format!("exec-{:04}", i),
                            vec![format!("action-{}", i)],
                        )
                    })
                    .collect::<Vec<_>>()
            },
            |receipts| {
                let mut chain = ReceiptChain::new();
                let start = Instant::now();

                for (exec_id, actions) in black_box(receipts) {
                    let _ = chain.emit_receipt(exec_id, actions);
                }

                let elapsed = start.elapsed();
                let throughput = chain.receipt_count() as f64 / elapsed.as_secs_f64();

                // SLO: At least 1000 receipts/sec
                assert!(
                    throughput >= 1000.0,
                    "Receipt throughput below SLO: {:.0} receipts/sec",
                    throughput
                );

                throughput as u64
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_receipt_emission,
    benchmark_hash_computation,
    benchmark_receipt_verification,
    benchmark_chain_verification,
    benchmark_receipt_throughput
);
criterion_main!(benches);
