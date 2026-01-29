//! Performance benchmarking infrastructure for ggen.
//!
//! This module provides comprehensive benchmarking and profiling capabilities
//! for evaluating the performance of different backend implementations and
//! identifying performance bottlenecks.
//!
//! ## Features
//!
//! - **Backend Comparison**: Compare performance across ETS, Redis, PostgreSQL backends
//! - **Profiling**: CPU and memory profiling with flamegraph generation
//! - **Metrics Collection**: Throughput, latency (P50, P95, P99), memory usage
//! - **Visualization**: ASCII charts and JSON output for analysis
//! - **Erlang Cluster Benchmarking**: Distributed cluster performance measurement
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::benchmarks::{BackendBenchmark, BenchmarkConfig, BackendType};
//! use std::time::Duration;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let config = BenchmarkConfig {
//!     duration: Duration::from_secs(30),
//!     warmup_duration: Duration::from_secs(5),
//!     job_count: 10000,
//!     concurrency: 10,
//! };
//!
//! let mut benchmark = BackendBenchmark::new(config);
//! let results = benchmark.run_comparison(vec![
//!     BackendType::ETS,
//!     BackendType::Redis,
//! ]).await?;
//!
//! println!("Benchmark Results: {:?}", results);
//! # Ok(())
//! # }
//! ```

pub mod backend_comparison;
pub mod erlang_cluster_benchmark;
pub mod profiling;

pub use backend_comparison::{
    BackendBenchmark, BackendType, BenchmarkConfig, BenchmarkMetrics, BenchmarkResult,
    ComparisonReport,
};
pub use erlang_cluster_benchmark::{
    benchmark_cluster_formation, benchmark_global_registry, benchmark_rpc_throughput,
    BenchmarkReporter, ClusterBenchmarkError, ClusterMetrics, ErlangClusterManager, NetworkMode,
};
pub use profiling::{MemoryProfile, MemoryTracker, ProfileConfig, Profiler};
