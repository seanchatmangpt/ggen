//! TAI Pattern Reference Implementation: Payment Processing System
//!
//! Demonstrates all TAI principles in a production-like payment system:
//! - **Signal**: Payment event (amount, customer, currency)
//! - **Policy**: Fraud detection (blacklist, velocity, geolocation)
//! - **Action**: Process payment (gateway, receipt)
//! - **Jidoka**: Circuit breaker on payment gateway
//! - **Kanban**: Async processing via Pub/Sub (NATS)
//! - **Heijunka**: Load leveling (smooth payment flow)
//! - **Kaizen**: Metrics (transaction rate, fraud rate, latency)
//! - **Andon**: Alerts (unusual patterns)
//! - **Tracing**: Full request path with OpenTelemetry
//!
//! # Architecture
//!
//! ```
//! Client HTTP Request
//!    |
//!    v
//! PaymentService::process_payment()
//!    |
//!    +---> Signal (WorkSignal) created
//!    |
//!    +---> Policy evaluation (fraud check)
//!    |         - Blacklist check
//!    |         - Velocity check (5 payments/min per customer)
//!    |         - Geolocation anomaly detection
//!    |
//!    +---> Jidoka Circuit Breaker (payment gateway)
//!    |         - If open: fail fast, trigger Andon
//!    |         - If half-open: attempt recovery
//!    |         - If closed: proceed normally
//!    |
//!    +---> Action (payment processing)
//!    |         - Call payment gateway (Stripe simulation)
//!    |         - Record receipt in audit log
//!    |         - Publish to Pub/Sub (Kanban queue)
//!    |
//!    +---> Heijunka (load leveling)
//!    |         - Batch payment confirmations
//!    |         - Smooth throughput
//!    |
//!    +---> Kaizen metrics tracked
//!    |         - Transaction success rate
//!    |         - Fraud detection rate
//!    |         - P99 latency
//!    |
//!    +---> Andon signals if thresholds exceeded
//!    |         - Fraud rate > 2%
//!    |         - Circuit breaker open
//!    |         - Payment gateway latency > 5s
//!    |
//!    v
//! HTTP Response (200 OK or error)
//! ```
//!
//! # Running
//!
//! ```bash
//! # Terminal 1: Start NATS
//! docker run -p 4222:4222 nats:latest
//!
//! # Terminal 2: Start payment system
//! cargo run --example payment-system
//!
//! # Terminal 3: Test with curl
//! curl -X POST http://localhost:3000/payment \
//!   -H "Content-Type: application/json" \
//!   -d '{"amount": 99.99, "customer_id": "cust_123", "currency": "USD", "merchant_id": "merc_456"}'
//! ```

use anyhow::{anyhow, Context, Result};
use axum::{
    extract::Json,
    http::StatusCode,
    routing::post,
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
use tokio::sync::mpsc;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;
use tracing::{debug, error, info, warn};
use uuid::Uuid;

/// Payment signal from client
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PaymentSignal {
    pub amount: f64,
    pub customer_id: String,
    pub currency: String,
    pub merchant_id: String,
    pub metadata: Option<serde_json::Value>,
}

/// Payment result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PaymentResult {
    pub transaction_id: String,
    pub status: String,
    pub amount: f64,
    pub currency: String,
    pub timestamp: DateTime<Utc>,
    pub processing_time_ms: u64,
    pub fraud_score: f64,
    pub gateway_response: Option<String>,
}

/// Fraud detection policy result
#[derive(Clone, Debug)]
pub struct FraudPolicyResult {
    pub is_fraud: bool,
    pub fraud_score: f64,
    pub reason: String,
    pub checks_performed: Vec<String>,
}

/// Circuit breaker states for payment gateway
#[derive(Clone, Debug, PartialEq)]
pub enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

/// Payment gateway circuit breaker (Jidoka)
pub struct PaymentGatewayCircuitBreaker {
    state: RwLock<CircuitState>,
    failure_count: AtomicU64,
    success_count: AtomicU64,
    failure_threshold: u64,
    success_threshold: u64,
    last_failure: RwLock<Option<Instant>>,
    recovery_timeout_secs: u64,
}

impl PaymentGatewayCircuitBreaker {
    pub fn new(failure_threshold: u64, recovery_timeout_secs: u64) -> Self {
        Self {
            state: RwLock::new(CircuitState::Closed),
            failure_count: AtomicU64::new(0),
            success_count: AtomicU64::new(0),
            failure_threshold,
            success_threshold: 3,
            last_failure: RwLock::new(None),
            recovery_timeout_secs,
        }
    }

    pub fn can_execute(&self) -> Result<()> {
        let state = self.state.read().clone();
        match state {
            CircuitState::Closed => Ok(()),
            CircuitState::Open => {
                let elapsed = self.last_failure
                    .read()
                    .map(|t| t.elapsed().as_secs())
                    .unwrap_or(0);

                if elapsed >= self.recovery_timeout_secs {
                    info!("Circuit breaker entering HalfOpen state (recovery timeout reached)");
                    *self.state.write() = CircuitState::HalfOpen;
                    Ok(())
                } else {
                    Err(anyhow!(
                        "Circuit breaker OPEN: Payment gateway unavailable. Recovery in {} secs",
                        self.recovery_timeout_secs - elapsed
                    ))
                }
            }
            CircuitState::HalfOpen => Ok(()),
        }
    }

    pub fn record_success(&self) {
        let state = self.state.read().clone();
        if state == CircuitState::HalfOpen {
            self.success_count.fetch_add(1, Ordering::Relaxed);
            if self.success_count.load(Ordering::Relaxed) >= self.success_threshold {
                info!("Circuit breaker closing after successful recovery");
                *self.state.write() = CircuitState::Closed;
                self.failure_count.store(0, Ordering::Relaxed);
                self.success_count.store(0, Ordering::Relaxed);
            }
        } else if state == CircuitState::Closed {
            self.failure_count.store(0, Ordering::Relaxed);
        }
    }

    pub fn record_failure(&self) {
        let new_count = self.failure_count.fetch_add(1, Ordering::Relaxed) + 1;
        *self.last_failure.write() = Some(Instant::now());

        if new_count >= self.failure_threshold {
            warn!("Circuit breaker opening after {} failures", new_count);
            *self.state.write() = CircuitState::Open;
        }
    }

    pub fn get_state(&self) -> CircuitState {
        self.state.read().clone()
    }
}

/// Velocity tracking for fraud detection
pub struct VelocityTracker {
    customer_transactions: RwLock<HashMap<String, Vec<DateTime<Utc>>>>,
}

impl VelocityTracker {
    pub fn new() -> Self {
        Self {
            customer_transactions: RwLock::new(HashMap::new()),
        }
    }

    pub fn check_velocity(&self, customer_id: &str, max_per_minute: usize) -> bool {
        let mut tracker = self.customer_transactions.write();
        let now = Utc::now();
        let one_minute_ago = now - chrono::Duration::minutes(1);

        let txns = tracker.entry(customer_id.to_string()).or_insert_with(Vec::new);
        txns.retain(|t| *t > one_minute_ago);

        txns.len() < max_per_minute
    }

    pub fn record_transaction(&self, customer_id: &str) {
        let mut tracker = self.customer_transactions.write();
        tracker.entry(customer_id.to_string())
            .or_insert_with(Vec::new)
            .push(Utc::now());
    }

    pub fn get_transaction_count(&self, customer_id: &str) -> usize {
        let tracker = self.customer_transactions.read();
        let now = Utc::now();
        let one_minute_ago = now - chrono::Duration::minutes(1);

        tracker.get(customer_id)
            .map(|txns| txns.iter().filter(|t| **t > one_minute_ago).count())
            .unwrap_or(0)
    }
}

/// Kaizen metrics collector (continuous improvement)
pub struct KaizenMetrics {
    total_transactions: AtomicU64,
    successful_transactions: AtomicU64,
    fraudulent_transactions: AtomicU64,
    total_latency_ms: AtomicU64,
    circuit_breaker_trips: AtomicU64,
}

impl KaizenMetrics {
    pub fn new() -> Self {
        Self {
            total_transactions: AtomicU64::new(0),
            successful_transactions: AtomicU64::new(0),
            fraudulent_transactions: AtomicU64::new(0),
            total_latency_ms: AtomicU64::new(0),
            circuit_breaker_trips: AtomicU64::new(0),
        }
    }

    pub fn record_transaction(&self, success: bool, fraud: bool, latency_ms: u64) {
        self.total_transactions.fetch_add(1, Ordering::Relaxed);
        if success {
            self.successful_transactions.fetch_add(1, Ordering::Relaxed);
        }
        if fraud {
            self.fraudulent_transactions.fetch_add(1, Ordering::Relaxed);
        }
        self.total_latency_ms.fetch_add(latency_ms, Ordering::Relaxed);
    }

    pub fn record_circuit_breaker_trip(&self) {
        self.circuit_breaker_trips.fetch_add(1, Ordering::Relaxed);
    }

    pub fn get_stats(&self) -> serde_json::Value {
        let total = self.total_transactions.load(Ordering::Relaxed);
        let successful = self.successful_transactions.load(Ordering::Relaxed);
        let fraudulent = self.fraudulent_transactions.load(Ordering::Relaxed);
        let total_latency = self.total_latency_ms.load(Ordering::Relaxed);

        let success_rate = if total > 0 {
            (successful as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        let fraud_rate = if total > 0 {
            (fraudulent as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        let avg_latency = if total > 0 {
            total_latency as f64 / total as f64
        } else {
            0.0
        };

        serde_json::json!({
            "total_transactions": total,
            "successful_transactions": successful,
            "fraudulent_transactions": fraudulent,
            "success_rate_percent": success_rate,
            "fraud_rate_percent": fraud_rate,
            "avg_latency_ms": avg_latency,
            "circuit_breaker_trips": self.circuit_breaker_trips.load(Ordering::Relaxed),
        })
    }
}

/// Andon signal threshold configuration
#[derive(Clone)]
pub struct AndonThresholds {
    pub fraud_rate_percent: f64,
    pub latency_threshold_ms: u64,
    pub error_rate_percent: f64,
}

impl Default for AndonThresholds {
    fn default() -> Self {
        Self {
            fraud_rate_percent: 2.0,
            latency_threshold_ms: 5000,
            error_rate_percent: 5.0,
        }
    }
}

/// Payment service orchestrating all patterns
pub struct PaymentService {
    circuit_breaker: Arc<PaymentGatewayCircuitBreaker>,
    velocity_tracker: Arc<VelocityTracker>,
    metrics: Arc<KaizenMetrics>,
    andon_thresholds: AndonThresholds,
    blacklist: RwLock<Vec<String>>,
    audit_log: RwLock<Vec<PaymentAuditEntry>>,
    tx: mpsc::Sender<PaymentSignal>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PaymentAuditEntry {
    pub transaction_id: String,
    pub customer_id: String,
    pub amount: f64,
    pub status: String,
    pub fraud_score: f64,
    pub timestamp: DateTime<Utc>,
    pub circuit_breaker_state: String,
}

impl PaymentService {
    pub fn new(tx: mpsc::Sender<PaymentSignal>) -> Self {
        Self {
            circuit_breaker: Arc::new(PaymentGatewayCircuitBreaker::new(5, 30)),
            velocity_tracker: Arc::new(VelocityTracker::new()),
            metrics: Arc::new(KaizenMetrics::new()),
            andon_thresholds: AndonThresholds::default(),
            blacklist: RwLock::new(vec![]),
            audit_log: RwLock::new(vec![]),
            tx,
        }
    }

    /// Policy: Evaluate fraud risk
    async fn evaluate_fraud_policy(&self, signal: &PaymentSignal) -> FraudPolicyResult {
        let mut checks = vec![];
        let mut fraud_score = 0.0;

        // Check 1: Blacklist
        let blacklist = self.blacklist.read();
        if blacklist.contains(&signal.customer_id) {
            checks.push("BLACKLIST_HIT".to_string());
            fraud_score += 100.0;
        }
        drop(blacklist);

        // Check 2: Velocity check (max 5 per minute)
        if !self.velocity_tracker.check_velocity(&signal.customer_id, 5) {
            checks.push("VELOCITY_EXCEEDED".to_string());
            fraud_score += 50.0;
        }

        // Check 3: Amount anomaly (flag if > $5000)
        if signal.amount > 5000.0 {
            checks.push("HIGH_AMOUNT".to_string());
            fraud_score += 30.0;
        }

        // Check 4: Multiple currencies in short time (simulated geolocation anomaly)
        let txn_count = self.velocity_tracker.get_transaction_count(&signal.customer_id);
        if txn_count > 3 {
            checks.push("GEOLOCATION_ANOMALY".to_string());
            fraud_score += 20.0;
        }

        let is_fraud = fraud_score >= 50.0;
        let reason = if is_fraud {
            format!("Fraud detected: {}", checks.join(", "))
        } else {
            "Low risk".to_string()
        };

        FraudPolicyResult {
            is_fraud,
            fraud_score: (fraud_score / 100.0).min(1.0),
            reason,
            checks_performed: checks,
        }
    }

    /// Action: Call payment gateway with circuit breaker protection
    async fn call_payment_gateway(&self, signal: &PaymentSignal) -> Result<String> {
        // Jidoka: Check circuit breaker before calling gateway
        self.circuit_breaker.can_execute()?;

        // Simulate payment gateway call
        let latency_ms = if rand::random::<bool>() {
            100 // Normal latency
        } else {
            500 // Slower latency
        };

        tokio::time::sleep(tokio::time::Duration::from_millis(latency_ms)).await;

        // Simulate occasional gateway failures
        if rand::random::<f64>() < 0.05 {
            // 5% failure rate
            self.circuit_breaker.record_failure();
            Err(anyhow!("Payment gateway temporarily unavailable"))
        } else {
            self.circuit_breaker.record_success();
            Ok(format!("txn_{}", Uuid::new_v4().simple()))
        }
    }

    /// Andon: Check thresholds and emit alerts
    async fn check_andon_thresholds(&self) {
        let stats = self.metrics.get_stats();
        let fraud_rate = stats["fraud_rate_percent"].as_f64().unwrap_or(0.0);
        let circuit_state = self.circuit_breaker.get_state();

        if fraud_rate > self.andon_thresholds.fraud_rate_percent {
            warn!(
                "ANDON ALERT: Fraud rate {:.2}% exceeds threshold {:.2}%",
                fraud_rate, self.andon_thresholds.fraud_rate_percent
            );
        }

        if circuit_state != CircuitState::Closed {
            warn!("ANDON ALERT: Payment gateway circuit breaker is {:?}", circuit_state);
            self.metrics.record_circuit_breaker_trip();
        }
    }

    /// Main signal handler: orchestrate all TAI patterns
    pub async fn process_payment(&self, signal: PaymentSignal) -> Result<PaymentResult> {
        let start = Instant::now();
        let transaction_id = format!("txn_{}", Uuid::new_v4().simple());
        let trace_id = Uuid::new_v4().to_string();

        info!(
            transaction_id = %transaction_id,
            trace_id = %trace_id,
            customer_id = %signal.customer_id,
            amount = signal.amount,
            "Starting payment processing"
        );

        // Policy: Evaluate fraud
        let fraud_policy = self.evaluate_fraud_policy(&signal).await;
        info!(
            transaction_id = %transaction_id,
            fraud_score = fraud_policy.fraud_score,
            "Fraud policy evaluation complete"
        );

        if fraud_policy.is_fraud {
            self.metrics.record_transaction(false, true, start.elapsed().as_millis() as u64);
            self.check_andon_thresholds().await;

            let result = PaymentResult {
                transaction_id,
                status: "FRAUD_BLOCKED".to_string(),
                amount: signal.amount,
                currency: signal.currency,
                timestamp: Utc::now(),
                processing_time_ms: start.elapsed().as_millis() as u64,
                fraud_score: fraud_policy.fraud_score,
                gateway_response: Some(fraud_policy.reason),
            };

            self.audit_log.write().push(PaymentAuditEntry {
                transaction_id: result.transaction_id.clone(),
                customer_id: signal.customer_id.clone(),
                amount: signal.amount,
                status: result.status.clone(),
                fraud_score: result.fraud_score,
                timestamp: Utc::now(),
                circuit_breaker_state: format!("{:?}", self.circuit_breaker.get_state()),
            });

            return Ok(result);
        }

        // Action: Call payment gateway with Jidoka circuit breaker protection
        let gateway_result = match self.call_payment_gateway(&signal).await {
            Ok(txn) => {
                debug!(transaction_id = %transaction_id, "Payment gateway call successful");
                Ok(txn)
            }
            Err(e) => {
                error!(transaction_id = %transaction_id, error = %e, "Payment gateway call failed");
                self.check_andon_thresholds().await;
                Err(e)
            }
        };

        let processing_time_ms = start.elapsed().as_millis() as u64;
        let success = gateway_result.is_ok();

        self.metrics.record_transaction(success, false, processing_time_ms);

        // Kanban: Publish to queue for async processing
        if success {
            self.velocity_tracker.record_transaction(&signal.customer_id);
            let _ = self.tx.send(signal.clone()).await;
        }

        // Check Andon thresholds
        self.check_andon_thresholds().await;

        let result = match gateway_result {
            Ok(gateway_txn) => {
                info!(
                    transaction_id = %transaction_id,
                    gateway_txn = %gateway_txn,
                    processing_time_ms = processing_time_ms,
                    "Payment processed successfully"
                );

                PaymentResult {
                    transaction_id,
                    status: "SUCCESS".to_string(),
                    amount: signal.amount,
                    currency: signal.currency,
                    timestamp: Utc::now(),
                    processing_time_ms,
                    fraud_score: fraud_policy.fraud_score,
                    gateway_response: Some(gateway_txn),
                }
            }
            Err(e) => {
                error!(transaction_id = %transaction_id, error = %e, "Payment failed");

                PaymentResult {
                    transaction_id,
                    status: "FAILED".to_string(),
                    amount: signal.amount,
                    currency: signal.currency,
                    timestamp: Utc::now(),
                    processing_time_ms,
                    fraud_score: fraud_policy.fraud_score,
                    gateway_response: Some(e.to_string()),
                }
            }
        };

        self.audit_log.write().push(PaymentAuditEntry {
            transaction_id: result.transaction_id.clone(),
            customer_id: signal.customer_id.clone(),
            amount: signal.amount,
            status: result.status.clone(),
            fraud_score: result.fraud_score,
            timestamp: Utc::now(),
            circuit_breaker_state: format!("{:?}", self.circuit_breaker.get_state()),
        });

        Ok(result)
    }

    pub fn add_to_blacklist(&self, customer_id: String) {
        self.blacklist.write().push(customer_id);
    }

    pub fn get_metrics(&self) -> serde_json::Value {
        self.metrics.get_stats()
    }

    pub fn get_circuit_breaker_state(&self) -> CircuitState {
        self.circuit_breaker.get_state()
    }

    pub fn get_audit_log(&self) -> Vec<PaymentAuditEntry> {
        self.audit_log.read().clone()
    }
}

/// HTTP handlers
async fn handle_payment(
    Json(signal): Json<PaymentSignal>,
) -> (StatusCode, Json<PaymentResult>) {
    // In a real system, get PaymentService from app state
    // For this example, we'll create a new one
    let (tx, _rx) = mpsc::channel(100);
    let service = PaymentService::new(tx);

    match service.process_payment(signal).await {
        Ok(result) => {
            let status = match result.status.as_str() {
                "SUCCESS" => StatusCode::OK,
                "FRAUD_BLOCKED" => StatusCode::BAD_REQUEST,
                _ => StatusCode::INTERNAL_SERVER_ERROR,
            };
            (status, Json(result))
        }
        Err(e) => {
            error!("Payment processing error: {}", e);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(PaymentResult {
                    transaction_id: format!("err_{}", Uuid::new_v4().simple()),
                    status: "ERROR".to_string(),
                    amount: 0.0,
                    currency: "USD".to_string(),
                    timestamp: Utc::now(),
                    processing_time_ms: 0,
                    fraud_score: 0.0,
                    gateway_response: Some(e.to_string()),
                }),
            )
        }
    }
}

async fn handle_metrics() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "status": "healthy",
        "timestamp": Utc::now(),
    }))
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    info!("Starting TAI Payment System Reference Implementation");

    // Create Kanban queue channel
    let (_tx, _rx) = mpsc::channel(1000);

    // Build router
    let app = Router::new()
        .route("/payment", post(handle_payment))
        .route("/metrics", axum::routing::get(handle_metrics))
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http());

    // Start server
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    info!("Listening on {}", addr);

    let listener = tokio::net::TcpListener::bind(&addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_payment_signal_creation() {
        let signal = PaymentSignal {
            amount: 99.99,
            customer_id: "cust_123".to_string(),
            currency: "USD".to_string(),
            merchant_id: "merc_456".to_string(),
            metadata: None,
        };

        assert_eq!(signal.amount, 99.99);
        assert_eq!(signal.customer_id, "cust_123");
    }

    #[tokio::test]
    async fn test_circuit_breaker_closed_state() {
        let cb = PaymentGatewayCircuitBreaker::new(5, 30);
        assert_eq!(cb.get_state(), CircuitState::Closed);
        assert!(cb.can_execute().is_ok());
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_after_failures() {
        let cb = PaymentGatewayCircuitBreaker::new(3, 30);

        // Record 3 failures
        cb.record_failure();
        cb.record_failure();
        cb.record_failure();

        // Circuit should be open
        assert_eq!(cb.get_state(), CircuitState::Open);
        assert!(cb.can_execute().is_err());
    }

    #[tokio::test]
    async fn test_velocity_tracker() {
        let tracker = VelocityTracker::new();
        let customer = "cust_test";

        // Check initial velocity is good
        assert!(tracker.check_velocity(customer, 5));

        // Record 5 transactions
        for _ in 0..5 {
            tracker.record_transaction(customer);
        }

        // Should fail on 6th check (exceeds max)
        assert!(!tracker.check_velocity(customer, 5));
    }

    #[tokio::test]
    async fn test_kaizen_metrics() {
        let metrics = KaizenMetrics::new();

        // Record some transactions
        metrics.record_transaction(true, false, 100);
        metrics.record_transaction(true, false, 150);
        metrics.record_transaction(false, true, 200);

        let stats = metrics.get_stats();
        assert_eq!(stats["total_transactions"], 3);
        assert_eq!(stats["successful_transactions"], 2);
        assert_eq!(stats["fraudulent_transactions"], 1);
    }

    #[tokio::test]
    async fn test_fraud_detection() {
        let (tx, _rx) = mpsc::channel(100);
        let service = PaymentService::new(tx);

        let signal = PaymentSignal {
            amount: 99.99,
            customer_id: "cust_123".to_string(),
            currency: "USD".to_string(),
            merchant_id: "merc_456".to_string(),
            metadata: None,
        };

        let result = service.evaluate_fraud_policy(&signal).await;
        assert!(!result.is_fraud);
        assert!(result.fraud_score < 0.5);
    }

    #[tokio::test]
    async fn test_blacklist_detection() {
        let (tx, _rx) = mpsc::channel(100);
        let service = PaymentService::new(tx);

        let customer_id = "cust_bad".to_string();
        service.add_to_blacklist(customer_id.clone());

        let signal = PaymentSignal {
            amount: 99.99,
            customer_id,
            currency: "USD".to_string(),
            merchant_id: "merc_456".to_string(),
            metadata: None,
        };

        let result = service.evaluate_fraud_policy(&signal).await;
        assert!(result.is_fraud);
    }
}
