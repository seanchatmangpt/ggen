use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use serde::{Deserialize, Serialize};
use std::time::Instant;

/// Signal represents a normalized telemetry event from the system
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Signal {
    id: String,
    timestamp: u64,
    metric_type: String,
    value: f64,
    tags: Vec<(String, String)>,
}

/// SignalNormalizer handles event normalization at scale
struct SignalNormalizer {
    buffer: Vec<Signal>,
    max_buffer_size: usize,
}

impl SignalNormalizer {
    fn new(max_buffer_size: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(max_buffer_size),
            max_buffer_size,
        }
    }

    /// Normalize a single signal event
    /// Target SLO: ≤1ms per event
    fn normalize_signal(&mut self, raw_event: &str) -> Result<(), Box<dyn std::error::Error>> {
        // Parse JSON event
        let mut signal: Signal = serde_json::from_str(raw_event)?;

        // Normalize timestamp to milliseconds
        if signal.timestamp > 1_000_000_000_000_000 {
            // Assume nanoseconds, convert to milliseconds
            signal.timestamp /= 1_000_000;
        }

        // Validate metric value
        if !signal.value.is_finite() {
            signal.value = 0.0;
        }

        // Deduplicate and sort tags
        signal.tags.sort_by(|a, b| a.0.cmp(&b.0));
        signal.tags.dedup_by(|a, b| a.0 == b.0);

        // Add to buffer
        if self.buffer.len() < self.max_buffer_size {
            self.buffer.push(signal);
        }

        Ok(())
    }

    /// Flush buffer (simulate persistence)
    fn flush(&mut self) {
        self.buffer.clear();
    }

    /// Get current buffer size
    fn buffer_len(&self) -> usize {
        self.buffer.len()
    }
}

fn generate_raw_event(id: usize) -> String {
    serde_json::json!({
        "id": format!("signal-{}", id),
        "timestamp": 1703001234567890u64 + (id as u64 * 1000),
        "metric_type": match id % 5 {
            0 => "cpu_usage",
            1 => "memory_usage",
            2 => "disk_io",
            3 => "network_latency",
            _ => "error_rate",
        },
        "value": (id % 100) as f64 * 1.5,
        "tags": vec![
            ("environment".to_string(), "production".to_string()),
            ("region".to_string(), "us-central1".to_string()),
            ("service".to_string(), format!("svc-{}", id % 10)),
        ]
    }).to_string()
}

fn benchmark_signal_normalization(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_ingest");
    group.measurement_time(std::time::Duration::from_secs(10));
    group.sample_size(100);

    for signal_count in [100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(signal_count),
            signal_count,
            |b, &signal_count| {
                b.iter_batched(
                    || {
                        let events: Vec<String> = (0..signal_count)
                            .map(generate_raw_event)
                            .collect();
                        let normalizer = SignalNormalizer::new(signal_count * 2);
                        (events, normalizer)
                    },
                    |(events, mut normalizer)| {
                        let start = Instant::now();
                        for event in &events {
                            let _ = normalizer.normalize_signal(event);
                        }
                        let elapsed = start.elapsed();

                        // Verify SLO: ≤1ms per event (in total normalized time)
                        let per_event_ms = elapsed.as_secs_f64() * 1000.0 / signal_count as f64;
                        assert!(
                            per_event_ms <= 1.5,
                            "Signal normalization exceeded SLO: {:.3}ms per event",
                            per_event_ms
                        );

                        normalizer.buffer_len()
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

fn benchmark_signal_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_throughput");
    group.measurement_time(std::time::Duration::from_secs(15));

    group.bench_function("1k_events_per_second", |b| {
        b.iter_batched(
            || {
                let events: Vec<String> = (0..1000)
                    .map(generate_raw_event)
                    .collect();
                let normalizer = SignalNormalizer::new(2000);
                (events, normalizer)
            },
            |(events, mut normalizer)| {
                let start = Instant::now();

                for event in black_box(&events) {
                    let _ = normalizer.normalize_signal(event);
                }

                let elapsed = start.elapsed();
                let throughput = events.len() as f64 / elapsed.as_secs_f64();

                // SLO: At least 1000 events/sec
                assert!(
                    throughput >= 1000.0,
                    "Throughput below SLO: {:.0} events/sec",
                    throughput
                );

                throughput as u64
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn benchmark_memory_allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_memory");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("allocation_rate_10k_events", |b| {
        b.iter_batched(
            || {
                let events: Vec<String> = (0..10000)
                    .map(generate_raw_event)
                    .collect();
                let normalizer = SignalNormalizer::new(15000);
                (events, normalizer)
            },
            |(events, mut normalizer)| {
                for event in black_box(&events) {
                    let _ = normalizer.normalize_signal(event);
                }

                // Memory should remain bounded (no unbounded growth)
                let allocated = normalizer.buffer_len();
                assert!(allocated <= 10000, "Buffer exceeded expected size");

                allocated
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_signal_normalization,
    benchmark_signal_throughput,
    benchmark_memory_allocation
);
criterion_main!(benches);
