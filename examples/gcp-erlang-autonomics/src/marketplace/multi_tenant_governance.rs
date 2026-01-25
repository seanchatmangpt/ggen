//! Multi-Tenant Governance Governor with Noisy Neighbor Detection
//!
//! This module implements the **Analyze** and **Plan** phases of MAPE-K for multi-tenant systems:
//! - FSM-based tenant resource governance (healthy → resource_contention → ... → healthy)
//! - Noisy neighbor detection and isolation
//! - Fair-share resource allocation with priority tiers
//! - Load balancing strategies (least-loaded, weighted round-robin, geographic)
//! - Cascade prevention through circuit breakers and bulkheads
//! - Graceful degradation with customer impact tracking
//! - Comprehensive audit trail for compliance

use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};
use std::collections::HashMap;

/// Multi-tenant governance errors
#[derive(Debug, Error)]
pub enum MTGovernorError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Resource analysis failed: {0}")]
    ResourceAnalysisFailed(String),

    #[error("Noisy neighbor detection failed: {0}")]
    NoisyNeighborDetectionFailed(String),

    #[error("Load balancing failed: {0}")]
    LoadBalancingFailed(String),

    #[error("Cascade prevention failed: {0}")]
    CascadePreventionFailed(String),

    #[error("No action possible in state: {state}")]
    NoActionPossible { state: String },

    #[error("Tenant metrics invalid: {0}")]
    InvalidTenantMetrics(String),

    #[error("Recovery failed: {0}")]
    RecoveryFailed(String),
}

/// Tenant priority tier (determines resource allocation ratios)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum TenantTier {
    /// 40% of available resources
    Enterprise,
    /// 35% of available resources
    Professional,
    /// 25% of available resources
    Starter,
}

impl TenantTier {
    pub fn weight(&self) -> f64 {
        match self {
            TenantTier::Enterprise => 0.40,
            TenantTier::Professional => 0.35,
            TenantTier::Starter => 0.25,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            TenantTier::Enterprise => "Enterprise",
            TenantTier::Professional => "Professional",
            TenantTier::Starter => "Starter",
        }
    }
}

/// Resource metrics for a tenant (normalized 0-100 scale)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TenantMetrics {
    pub tenant_id: String,
    pub tier: TenantTier,
    pub cpu_usage: u32,        // 0-100 (%)
    pub memory_usage: u32,      // 0-100 (%)
    pub network_bandwidth: u32, // 0-100 (% of allocated)
    pub disk_io_utilization: u32, // 0-100 (%)
    pub error_rate: f64,        // 0.0-1.0 (0-100%)
    pub request_rate: u32,      // requests per second
    pub eviction_rate: u32,     // memory pressure signal
    pub timestamp: DateTime<Utc>,
}

impl TenantMetrics {
    /// Check if metrics satisfy invariants
    pub fn validate(&self) -> Result<(), MTGovernorError> {
        if self.tenant_id.is_empty() {
            return Err(MTGovernorError::InvariantViolation(
                "tenant_id cannot be empty".to_string(),
            ));
        }

        if self.cpu_usage > 100
            || self.memory_usage > 100
            || self.network_bandwidth > 100
            || self.disk_io_utilization > 100
        {
            return Err(MTGovernorError::InvariantViolation(
                format!(
                    "metric out of bounds: cpu={}, mem={}, net={}, disk={}",
                    self.cpu_usage, self.memory_usage, self.network_bandwidth, self.disk_io_utilization
                ),
            ));
        }

        if self.error_rate < 0.0 || self.error_rate > 1.0 {
            return Err(MTGovernorError::InvariantViolation(
                format!("error_rate {} out of bounds [0.0-1.0]", self.error_rate),
            ));
        }

        Ok(())
    }

    /// Calculate overall resource pressure (0-100)
    pub fn overall_pressure(&self) -> u32 {
        (self.cpu_usage as f64 * 0.4
            + self.memory_usage as f64 * 0.3
            + self.network_bandwidth as f64 * 0.2
            + self.disk_io_utilization as f64 * 0.1) as u32
    }
}

/// FSM States for multi-tenant governance
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum MTGovernorState {
    /// All tenants operating normally, fair resource distribution
    Healthy,
    /// One or more tenants consuming excessive resources (noisy neighbor detected)
    ResourceContention,
    /// Actively rebalancing workloads
    LoadBalancing,
    /// Detecting signs of cascading failure
    CascadePrevention,
    /// Extreme load, graceful degradation required
    EmergencyShutdown,
    /// System recovering from emergency shutdown
    Recovery,
}

impl MTGovernorState {
    pub fn as_str(&self) -> &str {
        match self {
            MTGovernorState::Healthy => "Healthy",
            MTGovernorState::ResourceContention => "ResourceContention",
            MTGovernorState::LoadBalancing => "LoadBalancing",
            MTGovernorState::CascadePrevention => "CascadePrevention",
            MTGovernorState::EmergencyShutdown => "EmergencyShutdown",
            MTGovernorState::Recovery => "Recovery",
        }
    }
}

/// Cascade indicator flags
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CascadeIndicator {
    pub error_rate_spike: f64,        // threshold 0.5 (50%)
    pub latency_surge: f64,           // threshold 2.0x normal
    pub circuit_breaker_open: bool,   // circuit breaker tripped
    pub customer_id: Option<String>,  // which customer affected
}

/// Load balancing strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoadBalancingStrategy {
    /// Route to node with fewest active connections
    LeastLoadedNode,
    /// Enterprise 40%, Professional 35%, Starter 25%
    WeightedRoundRobin,
    /// Route based on remaining capacity
    ResourceBased,
    /// Route to nearest region (latency optimization)
    Geographic(String),
    /// Customer-defined affinity (e.g., same node for data locality)
    Custom(String),
}

/// Events that drive FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MTGovernorEvent {
    // === Healthy state signals ===
    ResourceUtilizationChecked,
    TenantMetricsCollected(Vec<TenantMetrics>),
    AnomalyDetected(String),

    // === Resource contention signals ===
    NoisyNeighborDetected {
        culprit_tenant: String,
        metric: String,
        value: u32,
    },
    UtilizationExceedsThreshold,
    CascadeRiskDetected,

    // === Load balancing signals ===
    RebalancingInProgress,
    RebalancingComplete,
    RebalancingFailed(String),

    // === Cascade prevention signals ===
    CascadeDetected(CascadeIndicator),
    CircuitBreakerEngaged,
    CustomerAutoThrottled(String),

    // === Emergency shutdown signals ===
    ShutdownComplete,
    RecoveryInitiated,
    ManualOverride,

    // === Recovery signals ===
    RecoveryComplete,
    RecoveryFailed(String),
    RestartCustomerWorkloads,

    // === Generic signals ===
    Timeout,
    Reset,
}

/// Audit event for compliance tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEvent {
    pub timestamp: DateTime<Utc>,
    pub tenant_id: Option<String>,
    pub event_type: String,
    pub action: Option<String>,
    pub affected_customers: usize,
    pub duration_ms: u64,
    pub details: String,
}

/// Multi-tenant governor FSM
#[derive(Debug, Clone)]
pub struct MTGovernor {
    state: MTGovernorState,
    tenant_metrics: HashMap<String, TenantMetrics>,
    last_state_change: DateTime<Utc>,
    time_in_state: Duration,
    consecutive_high_pressure: u32,
    audit_trail: Vec<AuditEvent>,
    throttled_tenants: HashMap<String, u32>, // tenant_id -> throttle percentage
    circuit_breaker_open: bool,
    last_rebalance_time: Option<DateTime<Utc>>,
}

impl MTGovernor {
    /// Create new multi-tenant governor
    pub fn new() -> Self {
        Self {
            state: MTGovernorState::Healthy,
            tenant_metrics: HashMap::new(),
            last_state_change: Utc::now(),
            time_in_state: Duration::seconds(0),
            consecutive_high_pressure: 0,
            audit_trail: Vec::new(),
            throttled_tenants: HashMap::new(),
            circuit_breaker_open: false,
            last_rebalance_time: None,
        }
    }

    /// Process event and compute state transition
    pub async fn transition(
        &mut self,
        event: MTGovernorEvent,
    ) -> Result<(MTGovernorState, Option<String>), MTGovernorError> {
        let (new_state, action) = match (&self.state, &event) {
            // === HEALTHY state ===
            (MTGovernorState::Healthy, MTGovernorEvent::TenantMetricsCollected(metrics)) => {
                for m in metrics {
                    m.validate()?;
                }

                // Check for anomalies
                let (has_contention, culprit) = Self::detect_noisy_neighbor(metrics)?;
                if has_contention {
                    let action = format!("Noisy neighbor detected: {}", culprit);
                    self.record_audit_event("noisy_neighbor_detected", Some(&culprit), Some(&action), 1);
                    (MTGovernorState::ResourceContention, Some(action))
                } else {
                    (MTGovernorState::Healthy, None)
                }
            }

            (MTGovernorState::Healthy, MTGovernorEvent::AnomalyDetected(reason)) => {
                let action = format!("Anomaly: {}", reason);
                self.record_audit_event("anomaly_detected", None, Some(&action), 0);
                (MTGovernorState::ResourceContention, Some(action))
            }

            // === RESOURCE_CONTENTION state ===
            (
                MTGovernorState::ResourceContention,
                MTGovernorEvent::NoisyNeighborDetected {
                    culprit_tenant,
                    metric,
                    value,
                },
            ) => {
                self.consecutive_high_pressure += 1;
                let action = format!(
                    "Throttle {}: {} = {} (consecutive: {})",
                    culprit_tenant, metric, value, self.consecutive_high_pressure
                );
                self.record_audit_event("throttle_applied", Some(&culprit_tenant), Some(&action), 1);
                self.throttled_tenants.insert(culprit_tenant.clone(), 50);
                (MTGovernorState::LoadBalancing, Some(action))
            }

            (MTGovernorState::ResourceContention, MTGovernorEvent::Timeout) => {
                // Timeout forces progression to load balancing
                let action = "Contention timeout: initiating load balancing".to_string();
                self.record_audit_event("contention_timeout", None, Some(&action), 0);
                (MTGovernorState::LoadBalancing, Some(action))
            }

            // === LOAD_BALANCING state ===
            (MTGovernorState::LoadBalancing, MTGovernorEvent::RebalancingInProgress) => {
                self.last_rebalance_time = Some(Utc::now());
                (MTGovernorState::LoadBalancing, None)
            }

            (MTGovernorState::LoadBalancing, MTGovernorEvent::RebalancingComplete) => {
                self.consecutive_high_pressure = 0;
                let action = "Rebalancing complete: returning to normal".to_string();
                self.record_audit_event("rebalancing_complete", None, Some(&action), 0);
                (MTGovernorState::Healthy, Some(action))
            }

            (MTGovernorState::LoadBalancing, MTGovernorEvent::RebalancingFailed(reason)) => {
                let action = format!("Rebalancing failed: {}", reason);
                self.record_audit_event("rebalancing_failed", None, Some(&action), 1);
                (MTGovernorState::CascadePrevention, Some(action))
            }

            (MTGovernorState::LoadBalancing, MTGovernorEvent::Timeout) => {
                let action = "Load balancing timeout: escalating to cascade prevention".to_string();
                self.record_audit_event("lb_timeout", None, Some(&action), 1);
                (MTGovernorState::CascadePrevention, Some(action))
            }

            // === CASCADE_PREVENTION state ===
            (MTGovernorState::CascadePrevention, MTGovernorEvent::CascadeDetected(indicator)) => {
                self.circuit_breaker_open = true;
                let action = format!(
                    "Cascade detected: error_rate={:.2}, circuit_breaker_open=true",
                    indicator.error_rate_spike
                );
                self.record_audit_event(
                    "cascade_detected",
                    indicator.customer_id.as_deref(),
                    Some(&action),
                    1,
                );
                (MTGovernorState::EmergencyShutdown, Some(action))
            }

            (MTGovernorState::CascadePrevention, MTGovernorEvent::CircuitBreakerEngaged) => {
                self.circuit_breaker_open = true;
                let action = "Circuit breaker engaged: preventing cascade".to_string();
                self.record_audit_event("circuit_breaker", None, Some(&action), 0);
                (MTGovernorState::CascadePrevention, Some(action))
            }

            (MTGovernorState::CascadePrevention, MTGovernorEvent::Timeout) => {
                let action = "Cascade prevention timeout: entering emergency shutdown".to_string();
                self.record_audit_event("cascade_timeout", None, Some(&action), 2);
                (MTGovernorState::EmergencyShutdown, Some(action))
            }

            // === EMERGENCY_SHUTDOWN state ===
            (MTGovernorState::EmergencyShutdown, MTGovernorEvent::ShutdownComplete) => {
                let action = "Emergency shutdown complete: non-essential features disabled".to_string();
                self.record_audit_event("shutdown_complete", None, Some(&action), 2);
                (MTGovernorState::Recovery, Some(action))
            }

            (MTGovernorState::EmergencyShutdown, MTGovernorEvent::RecoveryInitiated) => {
                let action = "Recovery initiated: gradual system restoration".to_string();
                self.record_audit_event("recovery_initiated", None, Some(&action), 2);
                (MTGovernorState::Recovery, Some(action))
            }

            (MTGovernorState::EmergencyShutdown, MTGovernorEvent::Timeout) => {
                let action = "Emergency shutdown timeout: auto-initiating recovery".to_string();
                self.record_audit_event("emergency_timeout", None, Some(&action), 2);
                (MTGovernorState::Recovery, Some(action))
            }

            // === RECOVERY state ===
            (MTGovernorState::Recovery, MTGovernorEvent::RecoveryComplete) => {
                self.circuit_breaker_open = false;
                self.throttled_tenants.clear();
                self.consecutive_high_pressure = 0;
                let action = "Recovery complete: system fully restored".to_string();
                self.record_audit_event("recovery_complete", None, Some(&action), 0);
                (MTGovernorState::Healthy, Some(action))
            }

            (MTGovernorState::Recovery, MTGovernorEvent::RecoveryFailed(reason)) => {
                let action = format!("Recovery failed: {}", reason);
                self.record_audit_event("recovery_failed", None, Some(&action), 2);
                // Stay in Recovery and retry
                (MTGovernorState::Recovery, Some(action))
            }

            (MTGovernorState::Recovery, MTGovernorEvent::Timeout) => {
                let action = "Recovery timeout: retrying recovery".to_string();
                self.record_audit_event("recovery_retry", None, Some(&action), 1);
                (MTGovernorState::Recovery, Some(action))
            }

            // === Default: invalid transition ===
            (current, event) => {
                return Err(MTGovernorError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                })
            }
        };

        // Update state
        let old_state = self.state;
        self.state = new_state;
        if old_state != new_state {
            self.last_state_change = Utc::now();
            self.time_in_state = Duration::seconds(0);

            tracing::info!(
                from = %old_state.as_str(),
                to = %new_state.as_str(),
                "Multi-tenant governor state transition"
            );
        } else {
            self.time_in_state = self.time_in_state + Duration::seconds(1);
        }

        Ok((new_state, action))
    }

    /// Detect noisy neighbor (tenant consuming excessive resources)
    ///
    /// ## Thresholds
    /// - CPU: > 80% for one tenant while others < 20%
    /// - Network: > 50% of total for single tenant
    /// - Disk I/O: > 90%
    /// - Memory eviction: spike in eviction_rate
    pub fn detect_noisy_neighbor(metrics: &[TenantMetrics]) -> Result<(bool, String), MTGovernorError> {
        if metrics.is_empty() {
            return Ok((false, String::new()));
        }

        // Check CPU hogging
        for metric in metrics {
            if metric.cpu_usage > 80 {
                let others_low = metrics
                    .iter()
                    .filter(|m| m.tenant_id != metric.tenant_id)
                    .all(|m| m.cpu_usage < 20);

                if others_low {
                    return Ok((true, format!("CPU hog: {} = {}%", metric.tenant_id, metric.cpu_usage)));
                }
            }
        }

        // Check network bandwidth hogging
        let total_bandwidth: u32 = metrics.iter().map(|m| m.network_bandwidth).sum();
        for metric in metrics {
            if total_bandwidth > 0 && metric.network_bandwidth as f64 / total_bandwidth as f64 > 0.5 {
                return Ok((
                    true,
                    format!(
                        "Network hog: {} = {}% (total={}%)",
                        metric.tenant_id, metric.network_bandwidth, total_bandwidth
                    ),
                ));
            }
        }

        // Check disk I/O saturation
        for metric in metrics {
            if metric.disk_io_utilization > 90 {
                return Ok((true, format!("Disk I/O saturation: {} = {}%", metric.tenant_id, metric.disk_io_utilization)));
            }
        }

        // Check memory pressure (eviction rate spike)
        for metric in metrics {
            if metric.eviction_rate > 50 {
                return Ok((true, format!("Memory pressure: {} eviction_rate = {}", metric.tenant_id, metric.eviction_rate)));
            }
        }

        Ok((false, String::new()))
    }

    /// Calculate fair-share resource quotas for all tenants
    pub fn calculate_fair_share_quotas(
        &self,
        total_resources: u32,
    ) -> HashMap<String, u32> {
        let mut quotas = HashMap::new();

        // Group tenants by tier
        let mut enterprise_tenants = Vec::new();
        let mut professional_tenants = Vec::new();
        let mut starter_tenants = Vec::new();

        for (tenant_id, metrics) in &self.tenant_metrics {
            match metrics.tier {
                TenantTier::Enterprise => enterprise_tenants.push(tenant_id.clone()),
                TenantTier::Professional => professional_tenants.push(tenant_id.clone()),
                TenantTier::Starter => starter_tenants.push(tenant_id.clone()),
            }
        }

        // Allocate per tier
        let enterprise_total = (total_resources as f64 * TenantTier::Enterprise.weight()) as u32;
        let professional_total = (total_resources as f64 * TenantTier::Professional.weight()) as u32;
        let starter_total = (total_resources as f64 * TenantTier::Starter.weight()) as u32;

        // Divide equally within tier
        if !enterprise_tenants.is_empty() {
            let per_tenant = enterprise_total / enterprise_tenants.len() as u32;
            for tenant_id in enterprise_tenants {
                quotas.insert(tenant_id, per_tenant);
            }
        }

        if !professional_tenants.is_empty() {
            let per_tenant = professional_total / professional_tenants.len() as u32;
            for tenant_id in professional_tenants {
                quotas.insert(tenant_id, per_tenant);
            }
        }

        if !starter_tenants.is_empty() {
            let per_tenant = starter_total / starter_tenants.len() as u32;
            for tenant_id in starter_tenants {
                quotas.insert(tenant_id, per_tenant);
            }
        }

        quotas
    }

    /// Get load balancing strategy recommendations
    pub fn recommend_load_balancing_strategy(&self) -> Result<LoadBalancingStrategy, MTGovernorError> {
        match self.state {
            MTGovernorState::ResourceContention | MTGovernorState::LoadBalancing => {
                // For noisy neighbor situations, use weighted round-robin
                Ok(LoadBalancingStrategy::WeightedRoundRobin)
            }
            MTGovernorState::CascadePrevention | MTGovernorState::EmergencyShutdown => {
                // For cascade situations, use least-loaded to shed load
                Ok(LoadBalancingStrategy::LeastLoadedNode)
            }
            MTGovernorState::Recovery => {
                // For recovery, use resource-based to spread load
                Ok(LoadBalancingStrategy::ResourceBased)
            }
            MTGovernorState::Healthy => {
                // For normal operation, geographic is optimal
                Ok(LoadBalancingStrategy::Geographic("us-central1".to_string()))
            }
        }
    }

    /// Apply graceful degradation (shed non-essential features)
    pub fn apply_graceful_degradation(&mut self) -> Result<Vec<String>, MTGovernorError> {
        let mut shed_features = Vec::new();

        match self.state {
            MTGovernorState::CascadePrevention => {
                // Shed non-essential features
                shed_features.push("analytics".to_string());
                shed_features.push("recommendations".to_string());
            }
            MTGovernorState::EmergencyShutdown => {
                // Shed more aggressively
                shed_features.extend(vec![
                    "analytics".to_string(),
                    "recommendations".to_string(),
                    "caching".to_string(),
                    "logging".to_string(),
                    "metrics".to_string(),
                ]);
            }
            _ => {}
        }

        if !shed_features.is_empty() {
            let action = format!("Graceful degradation: shedding {:?}", shed_features);
            self.record_audit_event("graceful_degradation", None, Some(&action), 0);
        }

        Ok(shed_features)
    }

    /// Record audit event for compliance
    fn record_audit_event(
        &mut self,
        event_type: &str,
        tenant_id: Option<&str>,
        action: Option<&str>,
        affected_customers: usize,
    ) {
        let event = AuditEvent {
            timestamp: Utc::now(),
            tenant_id: tenant_id.map(|s| s.to_string()),
            event_type: event_type.to_string(),
            action: action.map(|s| s.to_string()),
            affected_customers,
            duration_ms: 0,
            details: format!("State: {}", self.state.as_str()),
        };

        self.audit_trail.push(event);
    }

    // Getters for testing and monitoring
    pub fn current_state(&self) -> MTGovernorState {
        self.state
    }

    pub fn audit_trail(&self) -> &[AuditEvent] {
        &self.audit_trail
    }

    pub fn throttled_tenants(&self) -> &HashMap<String, u32> {
        &self.throttled_tenants
    }

    pub fn circuit_breaker_open(&self) -> bool {
        self.circuit_breaker_open
    }

    pub fn time_in_state(&self) -> Duration {
        self.time_in_state
    }

    pub fn consecutive_high_count(&self) -> u32 {
        self.consecutive_high_pressure
    }

    /// Record audit event (public for testing)
    pub fn record_audit_event_public(
        &mut self,
        event_type: &str,
        tenant_id: Option<&str>,
        action: Option<&str>,
        affected_customers: usize,
    ) {
        self.record_audit_event(event_type, tenant_id, action, affected_customers);
    }

    /// Set state directly (test helper only - external tests)
    pub fn _set_state_for_testing(&mut self, state: MTGovernorState) {
        self.state = state;
    }

    /// Add tenant metrics (test helper - external tests)
    pub fn _add_tenant_metrics_for_testing(&mut self, metrics: TenantMetrics) {
        self.tenant_metrics.insert(metrics.tenant_id.clone(), metrics);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_metrics(tenant_id: &str, tier: TenantTier, cpu: u32) -> TenantMetrics {
        TenantMetrics {
            tenant_id: tenant_id.to_string(),
            tier,
            cpu_usage: cpu,
            memory_usage: 50,
            network_bandwidth: 30,
            disk_io_utilization: 40,
            error_rate: 0.01,
            request_rate: 1000,
            eviction_rate: 0,
            timestamp: Utc::now(),
        }
    }

    #[tokio::test]
    async fn test_healthy_state_normal_metrics() {
        // Arrange
        let mut governor = MTGovernor::new();
        let metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 45),
            make_metrics("tenant-2", TenantTier::Professional, 50),
            make_metrics("tenant-3", TenantTier::Starter, 30),
        ];

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::TenantMetricsCollected(metrics))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::Healthy);
        assert!(action.is_none());
    }

    #[tokio::test]
    async fn test_healthy_to_contention_noisy_neighbor() {
        // Arrange
        let mut governor = MTGovernor::new();
        let metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 85), // CPU hog
            make_metrics("tenant-2", TenantTier::Professional, 15),
            make_metrics("tenant-3", TenantTier::Starter, 10),
        ];

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::TenantMetricsCollected(metrics))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::ResourceContention);
        assert!(action.is_some());
        assert!(action.unwrap().contains("tenant-1"));
    }

    #[tokio::test]
    async fn test_contention_to_load_balancing() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::ResourceContention;

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::NoisyNeighborDetected {
                culprit_tenant: "tenant-1".to_string(),
                metric: "cpu".to_string(),
                value: 90,
            })
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::LoadBalancing);
        assert!(action.is_some());
        assert!(governor.throttled_tenants.contains_key("tenant-1"));
        assert_eq!(governor.throttled_tenants["tenant-1"], 50);
    }

    #[tokio::test]
    async fn test_load_balancing_complete_returns_healthy() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::LoadBalancing;

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::RebalancingComplete)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::Healthy);
        assert!(action.is_some());
        assert_eq!(governor.consecutive_high_pressure, 0);
    }

    #[tokio::test]
    async fn test_load_balancing_failure_escalates_to_cascade() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::LoadBalancing;

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::RebalancingFailed(
                "migration timeout".to_string(),
            ))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::CascadePrevention);
        assert!(action.is_some());
    }

    #[tokio::test]
    async fn test_cascade_detection_opens_circuit_breaker() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::CascadePrevention;

        let cascade = CascadeIndicator {
            error_rate_spike: 0.75,
            latency_surge: 3.5,
            circuit_breaker_open: true,
            customer_id: Some("customer-1".to_string()),
        };

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::CascadeDetected(cascade))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::EmergencyShutdown);
        assert!(action.is_some());
        assert!(governor.circuit_breaker_open());
    }

    #[tokio::test]
    async fn test_emergency_shutdown_to_recovery() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::EmergencyShutdown;

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::ShutdownComplete)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::Recovery);
        assert!(action.is_some());
    }

    #[tokio::test]
    async fn test_recovery_complete_returns_healthy() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::Recovery;
        governor.circuit_breaker_open = true;
        governor.throttled_tenants.insert("tenant-1".to_string(), 50);

        // Act
        let (new_state, action) = governor
            .transition(MTGovernorEvent::RecoveryComplete)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, MTGovernorState::Healthy);
        assert!(action.is_some());
        assert!(!governor.circuit_breaker_open());
        assert!(governor.throttled_tenants().is_empty());
    }

    #[test]
    fn test_detect_noisy_neighbor_cpu_hog() {
        // Arrange
        let metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 82),
            make_metrics("tenant-2", TenantTier::Professional, 15),
            make_metrics("tenant-3", TenantTier::Starter, 10),
        ];

        // Act
        let (has_contention, culprit) = MTGovernor::detect_noisy_neighbor(&metrics).unwrap();

        // Assert
        assert!(has_contention);
        assert!(culprit.contains("tenant-1"));
    }

    #[test]
    fn test_detect_noisy_neighbor_network_hog() {
        // Arrange
        let mut metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 30),
            make_metrics("tenant-2", TenantTier::Professional, 20),
            make_metrics("tenant-3", TenantTier::Starter, 15),
        ];
        metrics[0].network_bandwidth = 80; // Hog 80% of network

        // Act
        let (has_contention, culprit) = MTGovernor::detect_noisy_neighbor(&metrics).unwrap();

        // Assert
        assert!(has_contention);
        assert!(culprit.contains("tenant-1"));
    }

    #[test]
    fn test_detect_noisy_neighbor_disk_io_saturation() {
        // Arrange
        let mut metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 30),
            make_metrics("tenant-2", TenantTier::Professional, 25),
        ];
        metrics[0].disk_io_utilization = 95;

        // Act
        let (has_contention, culprit) = MTGovernor::detect_noisy_neighbor(&metrics).unwrap();

        // Assert
        assert!(has_contention);
        assert!(culprit.contains("disk_io"));
    }

    #[test]
    fn test_detect_noisy_neighbor_memory_pressure() {
        // Arrange
        let mut metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 30),
            make_metrics("tenant-2", TenantTier::Professional, 25),
        ];
        metrics[0].eviction_rate = 60;

        // Act
        let (has_contention, culprit) = MTGovernor::detect_noisy_neighbor(&metrics).unwrap();

        // Assert
        assert!(has_contention);
        assert!(culprit.contains("memory_pressure"));
    }

    #[test]
    fn test_no_noisy_neighbor_all_healthy() {
        // Arrange
        let metrics = vec![
            make_metrics("tenant-1", TenantTier::Enterprise, 45),
            make_metrics("tenant-2", TenantTier::Professional, 50),
            make_metrics("tenant-3", TenantTier::Starter, 40),
        ];

        // Act
        let (has_contention, _) = MTGovernor::detect_noisy_neighbor(&metrics).unwrap();

        // Assert
        assert!(!has_contention);
    }

    #[test]
    fn test_calculate_fair_share_quotas() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.tenant_metrics.insert(
            "enterprise-1".to_string(),
            make_metrics("enterprise-1", TenantTier::Enterprise, 30),
        );
        governor.tenant_metrics.insert(
            "professional-1".to_string(),
            make_metrics("professional-1", TenantTier::Professional, 30),
        );
        governor.tenant_metrics.insert(
            "starter-1".to_string(),
            make_metrics("starter-1", TenantTier::Starter, 30),
        );

        // Act
        let quotas = governor.calculate_fair_share_quotas(1000);

        // Assert
        let enterprise_quota = quotas["enterprise-1"];
        let professional_quota = quotas["professional-1"];
        let starter_quota = quotas["starter-1"];

        assert_eq!(enterprise_quota, 400); // 40%
        assert_eq!(professional_quota, 350); // 35%
        assert_eq!(starter_quota, 250); // 25%
    }

    #[tokio::test]
    async fn test_recommend_load_balancing_strategy_weighted_in_contention() {
        // Arrange
        let governor = MTGovernor {
            state: MTGovernorState::ResourceContention,
            ..Default::default()
        };

        // Act
        let strategy = governor.recommend_load_balancing_strategy().unwrap();

        // Assert
        assert!(matches!(strategy, LoadBalancingStrategy::WeightedRoundRobin));
    }

    #[tokio::test]
    async fn test_recommend_load_balancing_strategy_least_loaded_in_cascade() {
        // Arrange
        let governor = MTGovernor {
            state: MTGovernorState::CascadePrevention,
            ..Default::default()
        };

        // Act
        let strategy = governor.recommend_load_balancing_strategy().unwrap();

        // Assert
        assert!(matches!(strategy, LoadBalancingStrategy::LeastLoadedNode));
    }

    #[tokio::test]
    async fn test_graceful_degradation_in_cascade() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::CascadePrevention;

        // Act
        let shed_features = governor.apply_graceful_degradation().unwrap();

        // Assert
        assert!(shed_features.contains(&"analytics".to_string()));
        assert!(shed_features.contains(&"recommendations".to_string()));
    }

    #[tokio::test]
    async fn test_graceful_degradation_aggressive_in_emergency() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::EmergencyShutdown;

        // Act
        let shed_features = governor.apply_graceful_degradation().unwrap();

        // Assert
        assert!(shed_features.len() >= 5);
        assert!(shed_features.contains(&"analytics".to_string()));
        assert!(shed_features.contains(&"caching".to_string()));
        assert!(shed_features.contains(&"logging".to_string()));
    }

    #[test]
    fn test_tenant_metrics_validation() {
        // Arrange & Act
        let valid = make_metrics("tenant-1", TenantTier::Enterprise, 50).validate();
        assert!(valid.is_ok());

        // Arrange invalid metrics
        let mut invalid = make_metrics("tenant-1", TenantTier::Enterprise, 50);
        invalid.cpu_usage = 150;

        // Act & Assert
        assert!(invalid.validate().is_err());
    }

    #[test]
    fn test_audit_trail_records_events() {
        // Arrange
        let mut governor = MTGovernor::new();

        // Act
        governor.record_audit_event("test_event", Some("tenant-1"), Some("test action"), 1);

        // Assert
        assert_eq!(governor.audit_trail().len(), 1);
        assert_eq!(governor.audit_trail()[0].event_type, "test_event");
        assert_eq!(governor.audit_trail()[0].tenant_id, Some("tenant-1".to_string()));
    }

    #[tokio::test]
    async fn test_invalid_state_transition_error() {
        // Arrange
        let mut governor = MTGovernor::new();
        governor.state = MTGovernorState::Healthy;

        // Act - try invalid transition
        let result = governor
            .transition(MTGovernorEvent::RecoveryComplete)
            .await;

        // Assert
        assert!(matches!(result, Err(MTGovernorError::InvalidTransition { .. })));
    }
}

// Default impl for test convenience
impl Default for MTGovernor {
    fn default() -> Self {
        Self::new()
    }
}
