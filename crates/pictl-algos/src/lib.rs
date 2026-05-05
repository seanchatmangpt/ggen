#![doc = "High-performance, branchless algorithm implementations for pictl process mining."]
#![doc = ""]
#![doc = "This crate implements discovery, conformance, and analysis algorithms optimized for:"]
#![doc = "- Minimal branch misses (cache-friendly execution)"]
#![doc = "- Predictable latency (sub-millisecond for WASM)"]
#![doc = "- Deterministic behavior (same input → same output every time)"]
#![doc = "- SIMD vectorization where applicable"]

pub mod alpha;
pub mod columnar;
pub mod conformance;
pub mod dfg;
pub mod heuristic;
pub mod streaming;

pub use pictl_types;

pub const VERSION: &str = "26.5.4";

