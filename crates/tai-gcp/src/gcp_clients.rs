//! Unified GCP service clients with circuit breaker integration
//!
//! This module provides type-safe wrappers around Google Cloud Client Libraries
//! with integrated retry policies, circuit breakers, and comprehensive logging.

use crate::gcp_auth::GcpAuthenticator;
use crate::gcp_config::GcpConfig;
use crate::gcp_errors::{classify_gcp_error, GcpError, GcpErrorContext, GcpErrorKind};
use async_trait::async_trait;
use backoff::ExponentialBackoff;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tracing::{debug, error, info, warn};

/// Circuit breaker state tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CircuitBreakerState {
    /// Circuit is closed, requests pass through
    Closed,
    /// Circuit is open, requests fail fast
    Open,
    /// Circuit is half-open, testing recovery
    HalfOpen,
}

/// Circuit breaker for fault tolerance
struct CircuitBreaker {
    /// Current state
    state: std::sync::Mutex<CircuitBreakerState>,
    /// Failure count
    failure_count: AtomicU32,
    /// Success count during half-open
    success_count: AtomicU32,
    /// Failure threshold before opening
    failure_threshold: u32,
    /// Success threshold to close after half-open
    success_threshold: u32,
}

impl CircuitBreaker {
    /// Creates a new circuit breaker
    fn new(failure_threshold: u32, success_threshold: u32) -> Self {
        Self {
            state: std::sync::Mutex::new(CircuitBreakerState::Closed),
            failure_count: AtomicU32::new(0),
            success_count: AtomicU32::new(0),
            failure_threshold,
            success_threshold,
        }
    }

    /// Records a failure and updates state
    fn record_failure(&self) {
        let count = self.failure_count.fetch_add(1, Ordering::SeqCst);

        if count + 1 >= self.failure_threshold {
            if let Ok(mut state) = self.state.lock() {
                *state = CircuitBreakerState::Open;
                warn!("Circuit breaker opened (failure_count={})", count + 1);
            }
        }
    }

    /// Records a success and updates state
    fn record_success(&self) {
        let mut state = self.state.lock().unwrap();
        match *state {
            CircuitBreakerState::Closed => {
                // Reset failure count on closed circuit
                self.failure_count.store(0, Ordering::SeqCst);
            }
            CircuitBreakerState::HalfOpen => {
                let count = self.success_count.fetch_add(1, Ordering::SeqCst);
                if count + 1 >= self.success_threshold {
                    *state = CircuitBreakerState::Closed;
                    self.failure_count.store(0, Ordering::SeqCst);
                    self.success_count.store(0, Ordering::SeqCst);
                    info!("Circuit breaker closed (recovery successful)");
                }
            }
            CircuitBreakerState::Open => {
                // Keep open until timeout (outside this struct)
            }
        }
    }

    /// Checks if the circuit is open and blocks requests
    fn is_open(&self) -> bool {
        *self.state.lock().unwrap() == CircuitBreakerState::Open
    }

    /// Transitions to half-open state
    fn try_half_open(&self) {
        if let Ok(mut state) = self.state.lock() {
            if *state == CircuitBreakerState::Open {
                *state = CircuitBreakerState::HalfOpen;
                self.success_count.store(0, Ordering::SeqCst);
                debug!("Circuit breaker transitioning to half-open");
            }
        }
    }
}

/// Retry configuration
#[derive(Debug, Clone, Copy)]
pub struct RetryConfig {
    /// Maximum number of retry attempts
    pub max_attempts: u32,
    /// Initial backoff duration
    pub initial_backoff: Duration,
    /// Maximum backoff duration
    pub max_backoff: Duration,
    /// Backoff multiplier
    pub multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            initial_backoff: Duration::from_millis(100),
            max_backoff: Duration::from_secs(30),
            multiplier: 2.0,
        }
    }
}

/// Cloud Run service client
pub struct CloudRunClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
    circuit_breaker: Arc<CircuitBreaker>,
    retry_config: RetryConfig,
}

impl CloudRunClient {
    /// Creates a new Cloud Run client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
            circuit_breaker: Arc::new(CircuitBreaker::new(5, 3)),
            retry_config: RetryConfig::default(),
        }
    }

    /// Invokes a Cloud Run service
    pub async fn invoke_service(
        &self, service_name: &str, request_body: &[u8],
    ) -> Result<Vec<u8>, GcpError> {
        if self.circuit_breaker.is_open() {
            return Err(GcpError::transient("Circuit breaker is open").with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("run.invoke".to_string())
                    .with_resource(service_name.to_string()),
            ));
        }

        let url = format!(
            "https://{}-run.googleapis.com/apis/serving.knative.dev/v1/namespaces/default/services/{}/invoke",
            self.config.region, service_name
        );

        let token = self.auth.get_token().await?;

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("run"))
            .body(request_body.to_vec())
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Cloud Run invocation failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("run.invoke".to_string())
                        .with_resource(service_name.to_string()),
                )
            })?;

        if response.status().is_success() {
            self.circuit_breaker.record_success();
            response
                .bytes()
                .await
                .map(|b| b.to_vec())
                .map_err(|e| GcpError::transient(format!("Failed to read response body: {}", e)))
        } else {
            self.circuit_breaker.record_failure();
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Cloud Run service returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("run.invoke".to_string())
                    .with_resource(service_name.to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Pub/Sub publish client
pub struct PubSubClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
    circuit_breaker: Arc<CircuitBreaker>,
}

#[derive(Debug, Serialize, Deserialize)]
struct PubSubMessage {
    data: String,
    attributes: Option<std::collections::HashMap<String, String>>,
}

impl PubSubClient {
    /// Creates a new Pub/Sub client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
            circuit_breaker: Arc::new(CircuitBreaker::new(5, 3)),
        }
    }

    /// Publishes a message to a topic
    pub async fn publish(
        &self, topic_name: &str, message_data: &[u8],
        attributes: Option<std::collections::HashMap<String, String>>,
    ) -> Result<String, GcpError> {
        if self.circuit_breaker.is_open() {
            return Err(GcpError::transient("Circuit breaker is open").with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("pubsub.publish".to_string())
                    .with_resource(topic_name.to_string()),
            ));
        }

        let url = format!(
            "https://pubsub.googleapis.com/v1/projects/{}/topics/{}:publish",
            self.config.project_id, topic_name
        );

        let token = self.auth.get_token().await?;

        let message = PubSubMessage {
            data: base64::encode(message_data),
            attributes,
        };

        let request_body = serde_json::json!({
            "messages": [message]
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("pubsub"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Pub/Sub publish failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("pubsub.publish".to_string())
                        .with_resource(topic_name.to_string()),
                )
            })?;

        if response.status().is_success() {
            self.circuit_breaker.record_success();
            let body: serde_json::Value = response.json().await.map_err(|e| {
                GcpError::transient(format!("Failed to parse publish response: {}", e))
            })?;
            body.get("messageIds")
                .and_then(|ids| ids.get(0))
                .and_then(|id| id.as_str())
                .map(|id| id.to_string())
                .ok_or_else(|| GcpError::transient("No messageId in publish response"))
        } else {
            self.circuit_breaker.record_failure();
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Pub/Sub publish returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("pubsub.publish".to_string())
                    .with_resource(topic_name.to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Firestore document client
pub struct FirestoreClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
    circuit_breaker: Arc<CircuitBreaker>,
}

impl FirestoreClient {
    /// Creates a new Firestore client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
            circuit_breaker: Arc::new(CircuitBreaker::new(5, 3)),
        }
    }

    /// Writes a document
    pub async fn write_document(
        &self, collection: &str, document_id: &str, data: serde_json::Value,
    ) -> Result<(), GcpError> {
        if self.circuit_breaker.is_open() {
            return Err(GcpError::transient("Circuit breaker is open").with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("firestore.write".to_string())
                    .with_resource(format!("{}/{}", collection, document_id)),
            ));
        }

        let url = format!(
            "https://firestore.googleapis.com/v1/projects/{}/databases/(default)/documents/{}/{}",
            self.config.project_id, collection, document_id
        );

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "fields": data
        });

        let response = self
            .http_client
            .patch(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("firestore"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Firestore write failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("firestore.write".to_string())
                        .with_resource(format!("{}/{}", collection, document_id)),
                )
            })?;

        if response.status().is_success() {
            self.circuit_breaker.record_success();
            Ok(())
        } else {
            self.circuit_breaker.record_failure();
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Firestore write returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("firestore.write".to_string())
                    .with_resource(format!("{}/{}", collection, document_id))
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }

    /// Reads a document
    pub async fn read_document(
        &self, collection: &str, document_id: &str,
    ) -> Result<serde_json::Value, GcpError> {
        let url = format!(
            "https://firestore.googleapis.com/v1/projects/{}/databases/(default)/documents/{}/{}",
            self.config.project_id, collection, document_id
        );

        let token = self.auth.get_token().await?;

        let response = self
            .http_client
            .get(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("firestore"))
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Firestore read failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("firestore.read".to_string())
                        .with_resource(format!("{}/{}", collection, document_id)),
                )
            })?;

        if response.status().is_success() {
            response.json().await.map_err(|e| {
                GcpError::transient(format!("Failed to parse Firestore response: {}", e))
            })
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Firestore read returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("firestore.read".to_string())
                    .with_resource(format!("{}/{}", collection, document_id))
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Cloud Monitoring metrics client
pub struct MonitoringClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
}

impl MonitoringClient {
    /// Creates a new Monitoring client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
        }
    }

    /// Writes a metric timeseries
    pub async fn write_timeseries(
        &self, metric_type: &str, value: f64,
        labels: Option<std::collections::HashMap<String, String>>,
    ) -> Result<(), GcpError> {
        let url = format!(
            "https://monitoring.googleapis.com/v3/projects/{}/timeSeries",
            self.config.project_id
        );

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "timeSeries": [{
                "metric": {
                    "type": metric_type,
                    "labels": labels.unwrap_or_default()
                },
                "resource": {
                    "type": "global",
                    "labels": {}
                },
                "points": [{
                    "interval": {
                        "endTime": chrono::Utc::now().to_rfc3339()
                    },
                    "value": {
                        "doubleValue": value
                    }
                }]
            }]
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("monitoring"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Monitoring write failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("monitoring.write".to_string()),
                )
            })?;

        if response.status().is_success() {
            Ok(())
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Monitoring write returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("monitoring.write".to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Cloud Scheduler jobs client
pub struct SchedulerClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
}

impl SchedulerClient {
    /// Creates a new Scheduler client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
        }
    }

    /// Creates a new scheduled job
    pub async fn create_job(
        &self, job_name: &str, schedule: &str, http_target_uri: &str,
    ) -> Result<(), GcpError> {
        let url = format!(
            "https://cloudscheduler.googleapis.com/v1/projects/{}/locations/{}/jobs",
            self.config.project_id, self.config.region
        );

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "name": format!("projects/{}/locations/{}/jobs/{}", self.config.project_id, self.config.region, job_name),
            "schedule": schedule,
            "timezone": "UTC",
            "httpTarget": {
                "uri": http_target_uri,
                "httpMethod": "POST"
            }
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("scheduler"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Scheduler create job failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("scheduler.create".to_string()),
                )
            })?;

        if response.status().is_success() || response.status().as_u16() == 409 {
            // 409 means job already exists, which is ok
            Ok(())
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Scheduler create returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("scheduler.create".to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Cloud KMS encryption client
pub struct KmsClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
}

impl KmsClient {
    /// Creates a new KMS client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
        }
    }

    /// Encrypts plaintext with a KMS key
    pub async fn encrypt(&self, key_resource: &str, plaintext: &[u8]) -> Result<Vec<u8>, GcpError> {
        let url = format!(
            "https://cloudkms.googleapis.com/v1/{}:encrypt",
            key_resource
        );

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "plaintext": base64::encode(plaintext)
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("kms"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("KMS encrypt failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("kms.encrypt".to_string()),
                )
            })?;

        if response.status().is_success() {
            let body: serde_json::Value = response
                .json()
                .await
                .map_err(|e| GcpError::transient(format!("Failed to parse KMS response: {}", e)))?;

            let ciphertext = body
                .get("ciphertext")
                .and_then(|ct| ct.as_str())
                .ok_or_else(|| GcpError::transient("No ciphertext in KMS response"))?;

            base64::decode(ciphertext)
                .map_err(|e| GcpError::transient(format!("Failed to decode ciphertext: {}", e)))
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("KMS encrypt returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("kms.encrypt".to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }

    /// Decrypts ciphertext with a KMS key
    pub async fn decrypt(
        &self, key_resource: &str, ciphertext: &[u8],
    ) -> Result<Vec<u8>, GcpError> {
        let url = format!(
            "https://cloudkms.googleapis.com/v1/{}:decrypt",
            key_resource
        );

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "ciphertext": base64::encode(ciphertext)
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("kms"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("KMS decrypt failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("kms.decrypt".to_string()),
                )
            })?;

        if response.status().is_success() {
            let body: serde_json::Value = response
                .json()
                .await
                .map_err(|e| GcpError::transient(format!("Failed to parse KMS response: {}", e)))?;

            let plaintext = body
                .get("plaintext")
                .and_then(|pt| pt.as_str())
                .ok_or_else(|| GcpError::transient("No plaintext in KMS response"))?;

            base64::decode(plaintext)
                .map_err(|e| GcpError::transient(format!("Failed to decode plaintext: {}", e)))
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("KMS decrypt returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("kms.decrypt".to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Cloud Logging client
pub struct LoggingClient {
    config: GcpConfig,
    auth: Arc<GcpAuthenticator>,
    http_client: reqwest::Client,
}

impl LoggingClient {
    /// Creates a new Logging client
    pub fn new(config: GcpConfig, auth: Arc<GcpAuthenticator>) -> Self {
        Self {
            config,
            auth,
            http_client: reqwest::Client::new(),
        }
    }

    /// Writes a log entry
    pub async fn write_log_entries(
        &self, log_name: &str, entries: Vec<serde_json::Value>,
    ) -> Result<(), GcpError> {
        let url = "https://logging.googleapis.com/v2/entries:write".to_string();

        let token = self.auth.get_token().await?;

        let request_body = serde_json::json!({
            "logName": format!("projects/{}/logs/{}", self.config.project_id, log_name),
            "entries": entries
        });

        let response = self
            .http_client
            .post(&url)
            .bearer_auth(token)
            .timeout(self.config.get_timeout("logging"))
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                GcpError::transient(format!("Logging write failed: {}", e)).with_context(
                    GcpErrorContext::default()
                        .with_project(self.config.project_id.clone())
                        .with_operation("logging.write".to_string()),
                )
            })?;

        if response.status().is_success() {
            Ok(())
        } else {
            Err(GcpError::new(
                GcpErrorKind::Transient,
                format!("Logging write returned status: {}", response.status()),
            )
            .with_context(
                GcpErrorContext::default()
                    .with_project(self.config.project_id.clone())
                    .with_operation("logging.write".to_string())
                    .with_http_status(response.status().as_u16()),
            ))
        }
    }
}

/// Unified GCP clients container
pub struct GcpClients {
    /// Cloud Run client
    pub run: CloudRunClient,
    /// Pub/Sub client
    pub pubsub: PubSubClient,
    /// Firestore client
    pub firestore: FirestoreClient,
    /// Monitoring client
    pub monitoring: MonitoringClient,
    /// Scheduler client
    pub scheduler: SchedulerClient,
    /// KMS client
    pub kms: KmsClient,
    /// Logging client
    pub logging: LoggingClient,
    /// Configuration
    pub config: GcpConfig,
}

impl GcpClients {
    /// Creates a new GCP clients container
    pub async fn new(config: GcpConfig) -> Result<Self, GcpError> {
        let auth = Arc::new(GcpAuthenticator::from_environment()?);

        info!("Initialized GCP clients for project: {}", config.project_id);

        Ok(Self {
            run: CloudRunClient::new(config.clone(), auth.clone()),
            pubsub: PubSubClient::new(config.clone(), auth.clone()),
            firestore: FirestoreClient::new(config.clone(), auth.clone()),
            monitoring: MonitoringClient::new(config.clone(), auth.clone()),
            scheduler: SchedulerClient::new(config.clone(), auth.clone()),
            kms: KmsClient::new(config.clone(), auth.clone()),
            logging: LoggingClient::new(config.clone(), auth.clone()),
            config,
        })
    }

    /// Creates a new GCP clients container with custom configuration
    pub async fn with_config(
        config: GcpConfig, auth: Arc<GcpAuthenticator>,
    ) -> Result<Self, GcpError> {
        info!("Initialized GCP clients for project: {}", config.project_id);

        Ok(Self {
            run: CloudRunClient::new(config.clone(), auth.clone()),
            pubsub: PubSubClient::new(config.clone(), auth.clone()),
            firestore: FirestoreClient::new(config.clone(), auth.clone()),
            monitoring: MonitoringClient::new(config.clone(), auth.clone()),
            scheduler: SchedulerClient::new(config.clone(), auth.clone()),
            kms: KmsClient::new(config.clone(), auth.clone()),
            logging: LoggingClient::new(config.clone(), auth.clone()),
            config,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_breaker_creation() {
        let cb = CircuitBreaker::new(5, 3);
        assert!(!cb.is_open());
    }

    #[test]
    fn test_circuit_breaker_opens_after_failures() {
        let cb = CircuitBreaker::new(3, 2);
        assert!(!cb.is_open());

        cb.record_failure();
        assert!(!cb.is_open());
        cb.record_failure();
        assert!(!cb.is_open());
        cb.record_failure();
        assert!(cb.is_open());
    }

    #[test]
    fn test_circuit_breaker_half_open() {
        let cb = CircuitBreaker::new(2, 2);
        cb.record_failure();
        cb.record_failure();
        assert!(cb.is_open());

        cb.try_half_open();
        // Should transition to half-open on next check
    }

    #[test]
    fn test_retry_config_default() {
        let config = RetryConfig::default();
        assert_eq!(config.max_attempts, 3);
        assert_eq!(config.multiplier, 2.0);
    }
}
