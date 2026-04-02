//! Counter metrics for operation counts.
//!
//! Tracks monotonically increasing counts of:
//! - Receipts processed
//! - Packets transmitted
//! - Tasks completed
//! - Errors encountered

use prometheus::{IntCounter, Registry, Result as PrometheusResult};

/// Counter metrics for A2A/TPS operations.
///
/// All counters are monotonically increasing and track total operation counts
/// since process start. Use these to calculate throughput (λ) over time windows.
pub struct Counters {
    /// Total receipts processed (cryptographic proofs of work).
    pub receipts: IntCounter,

    /// Total packets transmitted (A2A messages).
    pub packets: IntCounter,

    /// Total tasks completed (agent operations).
    pub tasks: IntCounter,

    /// Total errors encountered across all operations.
    pub errors: IntCounter,

    /// Total bytes processed (for throughput calculations).
    pub bytes_processed: IntCounter,
}

impl Counters {
    /// Creates and registers all counter metrics.
    pub fn new(registry: &Registry) -> PrometheusResult<Self> {
        let receipts = IntCounter::new(
            "ggen_tps_receipts_total",
            "Total number of receipts processed",
        )?;

        let packets = IntCounter::new(
            "ggen_tps_packets_total",
            "Total number of packets transmitted",
        )?;

        let tasks = IntCounter::new("ggen_tps_tasks_total", "Total number of tasks completed")?;

        let errors = IntCounter::new(
            "ggen_tps_errors_total",
            "Total number of errors encountered",
        )?;

        let bytes_processed =
            IntCounter::new("ggen_tps_bytes_processed_total", "Total bytes processed")?;

        registry.register(Box::new(receipts.clone()))?;
        registry.register(Box::new(packets.clone()))?;
        registry.register(Box::new(tasks.clone()))?;
        registry.register(Box::new(errors.clone()))?;
        registry.register(Box::new(bytes_processed.clone()))?;

        Ok(Self {
            receipts,
            packets,
            tasks,
            errors,
            bytes_processed,
        })
    }

    /// Increments receipt counter and returns new value.
    pub fn inc_receipts(&self) -> u64 {
        self.receipts.inc();
        self.receipts.get()
    }

    /// Increments packet counter and returns new value.
    pub fn inc_packets(&self) -> u64 {
        self.packets.inc();
        self.packets.get()
    }

    /// Increments task counter and returns new value.
    pub fn inc_tasks(&self) -> u64 {
        self.tasks.inc();
        self.tasks.get()
    }

    /// Increments error counter and returns new value.
    pub fn inc_errors(&self) -> u64 {
        self.errors.inc();
        self.errors.get()
    }

    /// Adds bytes to total processed and returns new value.
    pub fn add_bytes(&self, bytes: u64) -> u64 {
        self.bytes_processed.inc_by(bytes);
        self.bytes_processed.get()
    }

    /// Returns current receipt count.
    pub fn get_receipts(&self) -> u64 {
        self.receipts.get()
    }

    /// Returns current packet count.
    pub fn get_packets(&self) -> u64 {
        self.packets.get()
    }

    /// Returns current task count.
    pub fn get_tasks(&self) -> u64 {
        self.tasks.get()
    }

    /// Returns current error count.
    pub fn get_errors(&self) -> u64 {
        self.errors.get()
    }

    /// Returns current bytes processed.
    pub fn get_bytes(&self) -> u64 {
        self.bytes_processed.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_counter_creation() {
        let registry = Registry::new();
        let counters = Counters::new(&registry);
        assert!(counters.is_ok());
    }

    #[test]
    fn test_increment_receipts() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        assert_eq!(counters.get_receipts(), 0);
        assert_eq!(counters.inc_receipts(), 1);
        assert_eq!(counters.inc_receipts(), 2);
        assert_eq!(counters.get_receipts(), 2);
    }

    #[test]
    fn test_increment_packets() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        assert_eq!(counters.get_packets(), 0);
        assert_eq!(counters.inc_packets(), 1);
        assert_eq!(counters.get_packets(), 1);
    }

    #[test]
    fn test_increment_tasks() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        assert_eq!(counters.get_tasks(), 0);
        assert_eq!(counters.inc_tasks(), 1);
        assert_eq!(counters.get_tasks(), 1);
    }

    #[test]
    fn test_increment_errors() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        assert_eq!(counters.get_errors(), 0);
        assert_eq!(counters.inc_errors(), 1);
        assert_eq!(counters.get_errors(), 1);
    }

    #[test]
    fn test_add_bytes() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        assert_eq!(counters.get_bytes(), 0);
        assert_eq!(counters.add_bytes(100), 100);
        assert_eq!(counters.add_bytes(50), 150);
        assert_eq!(counters.get_bytes(), 150);
    }

    #[test]
    fn test_multiple_operations() {
        let registry = Registry::new();
        let counters = Counters::new(&registry).unwrap();

        counters.inc_receipts();
        counters.inc_packets();
        counters.inc_tasks();
        counters.add_bytes(1024);

        assert_eq!(counters.get_receipts(), 1);
        assert_eq!(counters.get_packets(), 1);
        assert_eq!(counters.get_tasks(), 1);
        assert_eq!(counters.get_bytes(), 1024);
    }
}
