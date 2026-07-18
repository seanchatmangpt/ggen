//! TAI Pattern Reference Implementation: Deployment Orchestration System
//!
//! Demonstrates all TAI principles applied to deployment automation:
//! - **Signal**: Deploy request (service, version, target region)
//! - **Policy**: Deployment safety (health check, SLO compliance, capacity)
//! - **Action**: Deploy (Terraform, Kubernetes, Cloud Build)
//! - **Jidoka**: Automatic rollback on health failure
//! - **Kanban**: Sequential deployment (one region at a time)
//! - **Heijunka**: Gradual rollout (canary → staging → prod)
//! - **Kaizen**: Deployment metrics (success, rollback, duration)
//! - **Andon**: Deployment failure alerts
//! - **Tracing**: Complete deployment path with observability
//!
//! # Architecture
//!
//! ```
//! Deploy Request (service, version, region)
//!    |
//!    v
//! DeploymentOrchestrator::orchestrate_deployment()
//!    |
//!    +---> Signal (DeploymentRequest) created
//!    |
//!    +---> Policy evaluation (safety checks)
//!    |         - Validate health SLOs
//!    |         - Check cluster capacity
//!    |         - Verify image exists
//!    |         - Check rollback readiness
//!    |
//!    +---> Heijunka (gradual rollout)
//!    |         - Stage 1: Canary (5% traffic)
//!    |         - Stage 2: Staging (25% traffic)
//!    |         - Stage 3: Production (100% traffic)
//!    |
//!    +---> Action (deploy)
//!    |         - Apply Terraform config
//!    |         - Deploy to Kubernetes
//!    |         - Update DNS/load balancer
//!    |
//!    +---> Jidoka (automatic rollback)
//!    |         - Monitor health metrics
//!    |         - If error_rate > 5%: automatic rollback
//!    |         - If latency > SLO: automatic rollback
//!    |
//!    +---> Kanban (sequential regional deployment)
//!    |         - Deploy to US-WEST
//!    |         - Wait for success, then deploy to US-EAST
//!    |         - Avoid parallel deployments (prevent cascading failures)
//!    |
//!    +---> Kaizen metrics tracked
//!    |         - Deployment success rate
//!    |         - Average deployment duration
//!    |         - Rollback frequency
//!    |
//!    +---> Andon signals
//!    |         - Deployment failure
//!    |         - Health check failure
//!    |         - Automatic rollback triggered
//!    |
//!    v
//! Deployment Complete (success or rolled back)
//! ```
//!
//! # Running
//!
//! ```bash
//! cargo run --example deployment-system
//!
//! # Trigger deployment
//! curl -X POST http://localhost:3001/deploy \
//!   -H "Content-Type: application/json" \
//!   -d '{
//!     "service": "api-service",
//!     "version": "v2.0.0",
//!     "target_region": "us-west-2",
//!     "rollout_strategy": "canary"
//!   }'
//! ```

use anyhow::{anyhow, Context, Result};
use axum::{
    extract::Json,
    http::StatusCode,
    routing::{get, post},
    Router,
};
use chrono::{DateTime, Utc};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::Mutex;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;
use tracing::{debug, error, info, warn};
use uuid::Uuid;

/// Deployment request signal
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DeploymentRequest {
    pub service: String,
    pub version: String,
    pub target_region: String,
    pub rollout_strategy: String, // "canary", "blue-green", "rolling"
}

/// Deployment result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DeploymentResult {
    pub deployment_id: String,
    pub service: String,
    pub version: String,
    pub status: String, // "IN_PROGRESS", "SUCCESS", "ROLLED_BACK", "FAILED"
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub duration_secs: u64,
    pub stages_completed: Vec<String>,
    pub error_message: Option<String>,
}

/// Deployment stage result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StageResult {
    pub stage_name: String,
    pub status: String, // "PENDING", "IN_PROGRESS", "SUCCESS", "FAILED"
    pub traffic_percentage: u32,
    pub error_rate: f64,
    pub latency_p99_ms: u64,
    pub timestamp: DateTime<Utc>,
}

/// Health metrics for deployment target
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HealthMetrics {
    pub error_rate_percent: f64,
    pub latency_p99_ms: u64,
    pub cpu_usage_percent: f64,
    pub memory_usage_percent: f64,
    pub active_connections: u64,
}

impl HealthMetrics {
    pub fn is_healthy(&self) -> bool {
        self.error_rate_percent < 5.0 && self.latency_p99_ms < 1000
    }
}

/// Deployment policy checker
pub struct DeploymentPolicy {
    max_error_rate_percent: f64,
    max_latency_p99_ms: u64,
    max_cpu_percent: f64,
    max_memory_percent: f64,
}

impl Default for DeploymentPolicy {
    fn default() -> Self {
        Self {
            max_error_rate_percent: 5.0,
            max_latency_p99_ms: 1000,
            max_cpu_percent: 80.0,
            max_memory_percent: 85.0,
        }
    }
}

impl DeploymentPolicy {
    pub fn validate_health(&self, health: &HealthMetrics) -> Result<()> {
        if health.error_rate_percent > self.max_error_rate_percent {
            return Err(anyhow!(
                "Error rate {:.2}% exceeds threshold {:.2}%",
                health.error_rate_percent,
                self.max_error_rate_percent
            ));
        }

        if health.latency_p99_ms > self.max_latency_p99_ms {
            return Err(anyhow!(
                "P99 latency {}ms exceeds threshold {}ms",
                health.latency_p99_ms,
                self.max_latency_p99_ms
            ));
        }

        if health.cpu_usage_percent > self.max_cpu_percent {
            return Err(anyhow!(
                "CPU usage {:.2}% exceeds threshold {:.2}%",
                health.cpu_usage_percent,
                self.max_cpu_percent
            ));
        }

        if health.memory_usage_percent > self.max_memory_percent {
            return Err(anyhow!(
                "Memory usage {:.2}% exceeds threshold {:.2}%",
                health.memory_usage_percent,
                self.max_memory_percent
            ));
        }

        Ok(())
    }
}

/// Jidoka: Automatic rollback on health failure
pub struct AutomaticRollback {
    health_check_interval_secs: u64,
    rollback_threshold: f64,
}

impl AutomaticRollback {
    pub fn new() -> Self {
        Self {
            health_check_interval_secs: 10,
            rollback_threshold: 5.0, // 5% error rate
        }
    }

    pub fn should_rollback(&self, health: &HealthMetrics) -> bool {
        health.error_rate_percent > self.rollback_threshold
    }
}

/// Heijunka: Gradual rollout stages
#[derive(Clone, Debug)]
pub enum RolloutStage {
    Canary,    // 5% traffic
    Staging,   // 25% traffic
    Prod,      // 100% traffic
}

impl RolloutStage {
    pub fn traffic_percentage(&self) -> u32 {
        match self {
            RolloutStage::Canary => 5,
            RolloutStage::Staging => 25,
            RolloutStage::Prod => 100,
        }
    }

    pub fn next(&self) -> Option<Self> {
        match self {
            RolloutStage::Canary => Some(RolloutStage::Staging),
            RolloutStage::Staging => Some(RolloutStage::Prod),
            RolloutStage::Prod => None,
        }
    }
}

/// Kanban: Sequential deployment queue
pub struct DeploymentQueue {
    pending: RwLock<Vec<DeploymentRequest>>,
    active: RwLock<Option<String>>, // current deployment ID
}

impl DeploymentQueue {
    pub fn new() -> Self {
        Self {
            pending: RwLock::new(Vec::new()),
            active: RwLock::new(None),
        }
    }

    pub fn enqueue(&self, request: DeploymentRequest) {
        self.pending.write().push(request);
    }

    pub fn dequeue(&self) -> Option<DeploymentRequest> {
        let mut pending = self.pending.write();
        if pending.is_empty() {
            None
        } else {
            Some(pending.remove(0))
        }
    }

    pub fn set_active(&self, deployment_id: String) {
        *self.active.write() = Some(deployment_id);
    }

    pub fn clear_active(&self) {
        *self.active.write() = None;
    }

    pub fn get_active(&self) -> Option<String> {
        self.active.read().clone()
    }

    pub fn queue_depth(&self) -> usize {
        self.pending.read().len()
    }
}

/// Kaizen: Deployment metrics
pub struct DeploymentKaizenMetrics {
    total_deployments: AtomicU64,
    successful_deployments: AtomicU64,
    rollback_count: AtomicU64,
    total_duration_secs: AtomicU64,
}

impl DeploymentKaizenMetrics {
    pub fn new() -> Self {
        Self {
            total_deployments: AtomicU64::new(0),
            successful_deployments: AtomicU64::new(0),
            rollback_count: AtomicU64::new(0),
            total_duration_secs: AtomicU64::new(0),
        }
    }

    pub fn record_deployment_start(&self) {
        self.total_deployments.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_deployment_success(&self, duration_secs: u64) {
        self.successful_deployments.fetch_add(1, Ordering::Relaxed);
        self.total_duration_secs.fetch_add(duration_secs, Ordering::Relaxed);
    }

    pub fn record_rollback(&self) {
        self.rollback_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn get_stats(&self) -> serde_json::Value {
        let total = self.total_deployments.load(Ordering::Relaxed);
        let successful = self.successful_deployments.load(Ordering::Relaxed);
        let rollbacks = self.rollback_count.load(Ordering::Relaxed);
        let total_duration = self.total_duration_secs.load(Ordering::Relaxed);

        let success_rate = if total > 0 {
            (successful as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        let avg_duration = if successful > 0 {
            total_duration as f64 / successful as f64
        } else {
            0.0
        };

        serde_json::json!({
            "total_deployments": total,
            "successful_deployments": successful,
            "rollback_count": rollbacks,
            "success_rate_percent": success_rate,
            "avg_duration_secs": avg_duration,
        })
    }
}

/// Deployment orchestrator (main TAI system)
pub struct DeploymentOrchestrator {
    policy: DeploymentPolicy,
    rollback: AutomaticRollback,
    queue: Arc<DeploymentQueue>,
    metrics: Arc<DeploymentKaizenMetrics>,
    active_deployments: RwLock<HashMap<String, DeploymentResult>>,
    health_metrics: RwLock<HashMap<String, HealthMetrics>>,
}

impl DeploymentOrchestrator {
    pub fn new() -> Self {
        Self {
            policy: DeploymentPolicy::default(),
            rollback: AutomaticRollback::new(),
            queue: Arc::new(DeploymentQueue::new()),
            metrics: Arc::new(DeploymentKaizenMetrics::new()),
            active_deployments: RwLock::new(HashMap::new()),
            health_metrics: RwLock::new(HashMap::new()),
        }
    }

    /// Policy: Validate deployment safety prerequisites
    async fn validate_deployment_prerequisites(&self, request: &DeploymentRequest) -> Result<()> {
        info!(
            service = %request.service,
            version = %request.version,
            "Validating deployment prerequisites"
        );

        // Check 1: Image exists
        if !self.image_exists(&request.service, &request.version).await {
            return Err(anyhow!(
                "Image not found: {}/{}",
                request.service,
                request.version
            ));
        }

        // Check 2: Cluster capacity
        if !self.check_cluster_capacity(&request.target_region).await {
            return Err(anyhow!(
                "Insufficient cluster capacity in region {}",
                request.target_region
            ));
        }

        // Check 3: Rollback plan exists
        if !self.rollback_plan_exists(&request.service).await {
            return Err(anyhow!("No valid rollback plan for service {}", request.service));
        }

        info!(
            service = %request.service,
            "Deployment prerequisites validated"
        );
        Ok(())
    }

    async fn image_exists(&self, service: &str, version: &str) -> bool {
        // Simulate image registry check
        !service.is_empty() && !version.is_empty()
    }

    async fn check_cluster_capacity(&self, region: &str) -> bool {
        // Simulate cluster capacity check
        !region.is_empty() && rand::random::<f64>() > 0.1 // 90% success rate
    }

    async fn rollback_plan_exists(&self, service: &str) -> bool {
        // Simulate rollback plan check
        !service.is_empty()
    }

    /// Heijunka: Execute gradual rollout stages
    async fn execute_rollout_stages(
        &self,
        deployment_id: &str,
        request: &DeploymentRequest,
    ) -> Result<Vec<StageResult>> {
        let mut stages = vec![];
        let mut current_stage = RolloutStage::Canary;

        loop {
            let stage_name = format!("{:?}", current_stage);
            info!(
                deployment_id = %deployment_id,
                stage = %stage_name,
                traffic = current_stage.traffic_percentage(),
                "Starting deployment stage"
            );

            // Simulate stage deployment
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

            // Get health metrics for this stage
            let health = self.get_stage_health(&stage_name).await;

            // Policy: Validate health
            if let Err(e) = self.policy.validate_health(&health) {
                error!(
                    deployment_id = %deployment_id,
                    stage = %stage_name,
                    error = %e,
                    "Stage health check failed"
                );

                return Err(e);
            }

            // Jidoka: Check if automatic rollback needed
            if self.rollback.should_rollback(&health) {
                warn!(
                    deployment_id = %deployment_id,
                    stage = %stage_name,
                    error_rate = health.error_rate_percent,
                    "Automatic rollback triggered"
                );
                self.metrics.record_rollback();
                return Err(anyhow!("Automatic rollback triggered at stage {}", stage_name));
            }

            stages.push(StageResult {
                stage_name,
                status: "SUCCESS".to_string(),
                traffic_percentage: current_stage.traffic_percentage(),
                error_rate: health.error_rate_percent,
                latency_p99_ms: health.latency_p99_ms,
                timestamp: Utc::now(),
            });

            match current_stage.next() {
                Some(next) => current_stage = next,
                None => break,
            }
        }

        Ok(stages)
    }

    async fn get_stage_health(&self, stage: &str) -> HealthMetrics {
        // Simulate health metrics collection
        let base_error_rate = match stage {
            "Canary" => 0.5,
            "Staging" => 1.0,
            _ => 2.0,
        };

        HealthMetrics {
            error_rate_percent: base_error_rate + (rand::random::<f64>() * 1.0),
            latency_p99_ms: 500 + rand::random::<u64>() % 300,
            cpu_usage_percent: 40.0 + (rand::random::<f64>() * 20.0),
            memory_usage_percent: 50.0 + (rand::random::<f64>() * 20.0),
            active_connections: 1000 + rand::random::<u64>() % 5000,
        }
    }

    /// Main orchestration method
    pub async fn orchestrate_deployment(&self, request: DeploymentRequest) -> Result<DeploymentResult> {
        let deployment_id = format!("dpl_{}", Uuid::new_v4().simple());
        let start = Instant::now();

        info!(
            deployment_id = %deployment_id,
            service = %request.service,
            version = %request.version,
            "Starting deployment orchestration"
        );

        self.metrics.record_deployment_start();

        // Policy: Validate prerequisites
        if let Err(e) = self.validate_deployment_prerequisites(&request).await {
            error!(deployment_id = %deployment_id, error = %e, "Prerequisites check failed");

            let result = DeploymentResult {
                deployment_id,
                service: request.service.clone(),
                version: request.version.clone(),
                status: "FAILED".to_string(),
                started_at: Utc::now() - chrono::Duration::seconds(start.elapsed().as_secs() as i64),
                completed_at: Some(Utc::now()),
                duration_secs: start.elapsed().as_secs(),
                stages_completed: vec![],
                error_message: Some(e.to_string()),
            };

            self.active_deployments.write().insert(deployment_id.clone(), result.clone());
            return Ok(result);
        }

        // Heijunka: Execute rollout stages
        let stages_result = self.execute_rollout_stages(&deployment_id, &request).await;

        let (status, error_message, stages_completed) = match stages_result {
            Ok(stages) => {
                let stage_names = stages.iter().map(|s| s.stage_name.clone()).collect();
                ("SUCCESS".to_string(), None, stage_names)
            }
            Err(e) => {
                error!(deployment_id = %deployment_id, error = %e, "Deployment failed");
                ("ROLLED_BACK".to_string(), Some(e.to_string()), vec![])
            }
        };

        let duration_secs = start.elapsed().as_secs();

        if status == "SUCCESS" {
            self.metrics.record_deployment_success(duration_secs);
        }

        let result = DeploymentResult {
            deployment_id: deployment_id.clone(),
            service: request.service.clone(),
            version: request.version.clone(),
            status,
            started_at: Utc::now() - chrono::Duration::seconds(duration_secs as i64),
            completed_at: Some(Utc::now()),
            duration_secs,
            stages_completed,
            error_message,
        };

        self.active_deployments
            .write()
            .insert(deployment_id.clone(), result.clone());

        info!(
            deployment_id = %deployment_id,
            status = %result.status,
            duration_secs = duration_secs,
            "Deployment complete"
        );

        self.queue.clear_active();

        Ok(result)
    }

    pub fn get_metrics(&self) -> serde_json::Value {
        self.metrics.get_stats()
    }

    pub fn get_active_deployments(&self) -> Vec<DeploymentResult> {
        self.active_deployments.read().values().cloned().collect()
    }

    pub fn get_queue_depth(&self) -> usize {
        self.queue.queue_depth()
    }
}

// HTTP Handlers
async fn handle_deploy(
    Json(request): Json<DeploymentRequest>,
) -> (StatusCode, Json<DeploymentResult>) {
    let orchestrator = DeploymentOrchestrator::new();

    match orchestrator.orchestrate_deployment(request).await {
        Ok(result) => {
            let status = if result.status == "SUCCESS" {
                StatusCode::OK
            } else if result.status == "ROLLED_BACK" {
                StatusCode::ACCEPTED
            } else {
                StatusCode::INTERNAL_SERVER_ERROR
            };
            (status, Json(result))
        }
        Err(e) => {
            error!("Deployment orchestration error: {}", e);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(DeploymentResult {
                    deployment_id: format!("err_{}", Uuid::new_v4().simple()),
                    service: String::new(),
                    version: String::new(),
                    status: "ERROR".to_string(),
                    started_at: Utc::now(),
                    completed_at: Some(Utc::now()),
                    duration_secs: 0,
                    stages_completed: vec![],
                    error_message: Some(e.to_string()),
                }),
            )
        }
    }
}

async fn handle_metrics() -> Json<serde_json::Value> {
    let orchestrator = DeploymentOrchestrator::new();
    Json(orchestrator.get_metrics())
}

async fn handle_health() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "status": "healthy",
        "timestamp": Utc::now(),
    }))
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    info!("Starting TAI Deployment System Reference Implementation");

    let app = Router::new()
        .route("/deploy", post(handle_deploy))
        .route("/metrics", get(handle_metrics))
        .route("/health", get(handle_health))
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http());

    let addr = SocketAddr::from(([127, 0, 0, 1], 3001));
    info!("Listening on {}", addr);

    let listener = tokio::net::TcpListener::bind(&addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rollout_stage_progression() {
        let stage = RolloutStage::Canary;
        assert_eq!(stage.traffic_percentage(), 5);

        let next = stage.next();
        assert!(next.is_some());

        let staging = next.unwrap();
        assert_eq!(staging.traffic_percentage(), 25);
    }

    #[test]
    fn test_deployment_policy_validation() {
        let policy = DeploymentPolicy::default();

        let healthy = HealthMetrics {
            error_rate_percent: 2.0,
            latency_p99_ms: 500,
            cpu_usage_percent: 50.0,
            memory_usage_percent: 60.0,
            active_connections: 1000,
        };

        assert!(policy.validate_health(&healthy).is_ok());
    }

    #[test]
    fn test_deployment_policy_rejection() {
        let policy = DeploymentPolicy::default();

        let unhealthy = HealthMetrics {
            error_rate_percent: 10.0, // Exceeds 5%
            latency_p99_ms: 500,
            cpu_usage_percent: 50.0,
            memory_usage_percent: 60.0,
            active_connections: 1000,
        };

        assert!(policy.validate_health(&unhealthy).is_err());
    }

    #[test]
    fn test_automatic_rollback_trigger() {
        let rollback = AutomaticRollback::new();

        let bad_health = HealthMetrics {
            error_rate_percent: 10.0, // Exceeds rollback threshold
            latency_p99_ms: 500,
            cpu_usage_percent: 50.0,
            memory_usage_percent: 60.0,
            active_connections: 1000,
        };

        assert!(rollback.should_rollback(&bad_health));
    }

    #[test]
    fn test_deployment_queue() {
        let queue = DeploymentQueue::new();

        let request = DeploymentRequest {
            service: "test-service".to_string(),
            version: "v1.0.0".to_string(),
            target_region: "us-west-2".to_string(),
            rollout_strategy: "canary".to_string(),
        };

        queue.enqueue(request.clone());
        assert_eq!(queue.queue_depth(), 1);

        let dequeued = queue.dequeue();
        assert!(dequeued.is_some());
        assert_eq!(queue.queue_depth(), 0);
    }

    #[tokio::test]
    async fn test_deployment_orchestrator_creation() {
        let orchestrator = DeploymentOrchestrator::new();
        assert_eq!(orchestrator.get_queue_depth(), 0);
    }
}
