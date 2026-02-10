//! Gauge metrics for current state.
//!
//! Tracks instantaneous values:
//! - Queue depth (L in queueing theory)
//! - Token count
//! - Utilization (ρ in queueing theory)
//! - Active agents

use prometheus::{Gauge, IntGauge, Registry, Result as PrometheusResult};

/// Gauge metrics for system state.
///
/// Tracks current values that can go up or down:
/// - L (queue depth): items waiting in queue
/// - ρ (utilization): fraction of capacity in use (0.0-1.0)
/// - Token count: current token budget
/// - Active agents: currently executing agents
pub struct Gauges {
    /// Current queue depth (L in queueing theory).
    pub queue_depth: IntGauge,

    /// Current token count.
    pub token_count: IntGauge,

    /// Current utilization (ρ in queueing theory, 0.0-1.0).
    pub utilization: Gauge,

    /// Number of currently active agents.
    pub active_agents: IntGauge,

    /// Current memory usage in bytes.
    pub memory_bytes: IntGauge,

    /// Current connection count.
    pub connections: IntGauge,
}

impl Gauges {
    /// Creates and registers all gauge metrics.
    pub fn new(registry: &Registry) -> PrometheusResult<Self> {
        let queue_depth = IntGauge::new(
            "ggen_tps_queue_depth",
            "Current queue depth (L in queueing theory)",
        )?;

        let token_count = IntGauge::new("ggen_tps_token_count", "Current token count")?;

        let utilization = Gauge::new(
            "ggen_tps_utilization",
            "Current utilization (rho in queueing theory, 0.0-1.0)",
        )?;

        let active_agents = IntGauge::new(
            "ggen_tps_active_agents",
            "Number of currently active agents",
        )?;

        let memory_bytes = IntGauge::new("ggen_tps_memory_bytes", "Current memory usage in bytes")?;

        let connections = IntGauge::new("ggen_tps_connections", "Current number of connections")?;

        registry.register(Box::new(queue_depth.clone()))?;
        registry.register(Box::new(token_count.clone()))?;
        registry.register(Box::new(utilization.clone()))?;
        registry.register(Box::new(active_agents.clone()))?;
        registry.register(Box::new(memory_bytes.clone()))?;
        registry.register(Box::new(connections.clone()))?;

        Ok(Self {
            queue_depth,
            token_count,
            utilization,
            active_agents,
            memory_bytes,
            connections,
        })
    }

    /// Sets queue depth (L).
    pub fn set_queue_depth(&self, depth: i64) {
        self.queue_depth.set(depth);
    }

    /// Increments queue depth by 1.
    pub fn inc_queue_depth(&self) {
        self.queue_depth.inc();
    }

    /// Decrements queue depth by 1.
    pub fn dec_queue_depth(&self) {
        self.queue_depth.dec();
    }

    /// Returns current queue depth.
    pub fn get_queue_depth(&self) -> i64 {
        self.queue_depth.get()
    }

    /// Sets token count.
    pub fn set_token_count(&self, count: i64) {
        self.token_count.set(count);
    }

    /// Adds to token count.
    pub fn add_tokens(&self, delta: i64) {
        self.token_count.add(delta);
    }

    /// Subtracts from token count.
    pub fn sub_tokens(&self, delta: i64) {
        self.token_count.sub(delta);
    }

    /// Returns current token count.
    pub fn get_token_count(&self) -> i64 {
        self.token_count.get()
    }

    /// Sets utilization (ρ, should be 0.0-1.0).
    pub fn set_utilization(&self, rho: f64) {
        self.utilization.set(rho);
    }

    /// Returns current utilization.
    pub fn get_utilization(&self) -> f64 {
        self.utilization.get()
    }

    /// Sets active agent count.
    pub fn set_active_agents(&self, count: i64) {
        self.active_agents.set(count);
    }

    /// Increments active agents by 1.
    pub fn inc_active_agents(&self) {
        self.active_agents.inc();
    }

    /// Decrements active agents by 1.
    pub fn dec_active_agents(&self) {
        self.active_agents.dec();
    }

    /// Returns current active agent count.
    pub fn get_active_agents(&self) -> i64 {
        self.active_agents.get()
    }

    /// Sets memory usage in bytes.
    pub fn set_memory_bytes(&self, bytes: i64) {
        self.memory_bytes.set(bytes);
    }

    /// Returns current memory usage.
    pub fn get_memory_bytes(&self) -> i64 {
        self.memory_bytes.get()
    }

    /// Sets connection count.
    pub fn set_connections(&self, count: i64) {
        self.connections.set(count);
    }

    /// Increments connection count.
    pub fn inc_connections(&self) {
        self.connections.inc();
    }

    /// Decrements connection count.
    pub fn dec_connections(&self) {
        self.connections.dec();
    }

    /// Returns current connection count.
    pub fn get_connections(&self) -> i64 {
        self.connections.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gauge_creation() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry);
        assert!(gauges.is_ok());
    }

    #[test]
    fn test_queue_depth_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_queue_depth(), 0);

        gauges.set_queue_depth(10);
        assert_eq!(gauges.get_queue_depth(), 10);

        gauges.inc_queue_depth();
        assert_eq!(gauges.get_queue_depth(), 11);

        gauges.dec_queue_depth();
        assert_eq!(gauges.get_queue_depth(), 10);
    }

    #[test]
    fn test_token_count_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_token_count(), 0);

        gauges.set_token_count(1000);
        assert_eq!(gauges.get_token_count(), 1000);

        gauges.add_tokens(500);
        assert_eq!(gauges.get_token_count(), 1500);

        gauges.sub_tokens(200);
        assert_eq!(gauges.get_token_count(), 1300);
    }

    #[test]
    fn test_utilization_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_utilization(), 0.0);

        gauges.set_utilization(0.75);
        assert!((gauges.get_utilization() - 0.75).abs() < 1e-10);

        gauges.set_utilization(1.0);
        assert!((gauges.get_utilization() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_active_agents_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_active_agents(), 0);

        gauges.set_active_agents(5);
        assert_eq!(gauges.get_active_agents(), 5);

        gauges.inc_active_agents();
        assert_eq!(gauges.get_active_agents(), 6);

        gauges.dec_active_agents();
        assert_eq!(gauges.get_active_agents(), 5);
    }

    #[test]
    fn test_memory_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_memory_bytes(), 0);

        gauges.set_memory_bytes(104857600); // 100MB
        assert_eq!(gauges.get_memory_bytes(), 104857600);
    }

    #[test]
    fn test_connections_operations() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        assert_eq!(gauges.get_connections(), 0);

        gauges.set_connections(10);
        assert_eq!(gauges.get_connections(), 10);

        gauges.inc_connections();
        assert_eq!(gauges.get_connections(), 11);

        gauges.dec_connections();
        assert_eq!(gauges.get_connections(), 10);
    }

    #[test]
    fn test_multiple_gauge_updates() {
        let registry = Registry::new();
        let gauges = Gauges::new(&registry).unwrap();

        gauges.set_queue_depth(5);
        gauges.set_token_count(1000);
        gauges.set_utilization(0.8);
        gauges.set_active_agents(3);
        gauges.set_memory_bytes(50000000);
        gauges.set_connections(20);

        assert_eq!(gauges.get_queue_depth(), 5);
        assert_eq!(gauges.get_token_count(), 1000);
        assert!((gauges.get_utilization() - 0.8).abs() < 1e-10);
        assert_eq!(gauges.get_active_agents(), 3);
        assert_eq!(gauges.get_memory_bytes(), 50000000);
        assert_eq!(gauges.get_connections(), 20);
    }
}
