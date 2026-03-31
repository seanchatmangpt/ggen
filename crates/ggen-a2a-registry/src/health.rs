//! Background health monitor that periodically pings agent endpoints.
//!
//! The monitor runs a Tokio task that iterates over registered agents,
//! sends an HTTP GET to their `endpoint_url`, and updates the health
//! status in the store based on the response.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Notify;
use tokio::task::JoinHandle;
use tokio::time::{interval, timeout};
use tracing::{debug, info, warn};

use crate::error::RegistryError;
use crate::store::AgentStore;
use crate::types::HealthStatus;

// Re-export so downstream consumers can use `ggen_a2a_registry::HealthConfig`.
#[doc(hidden)]
pub use crate::types::HealthConfig;

/// A background health-check loop.
///
/// Created via `HealthMonitor::new()` and started with `start()`.
/// Call `stop()` or drop the monitor to shut it down.
pub struct HealthMonitor {
    config: HealthConfig,
    store: Arc<dyn AgentStore>,
    running: Arc<AtomicBool>,
    cancel: Arc<Notify>,
    handle: tokio::sync::Mutex<Option<JoinHandle<()>>>,
    /// Per-agent consecutive failure count (in-memory).
    failure_counts: Arc<tokio::sync::RwLock<HashMap<String, u32>>>,
}

impl HealthMonitor {
    /// Create a new health monitor (not yet started).
    pub fn new(store: Arc<dyn AgentStore>, config: HealthConfig) -> Self {
        Self {
            config,
            store,
            running: Arc::new(AtomicBool::new(false)),
            cancel: Arc::new(Notify::new()),
            handle: tokio::sync::Mutex::new(None),
            failure_counts: Arc::new(tokio::sync::RwLock::new(HashMap::new())),
        }
    }

    /// Spawn the background health-check loop.
    pub async fn start(&self) {
        if self.running.load(Ordering::Relaxed) {
            return;
        }
        self.running.store(true, Ordering::Relaxed);

        let store = Arc::clone(&self.store);
        let config = self.config.clone();
        let check_interval_ms =
            u64::try_from(self.config.check_interval.as_millis()).unwrap_or(u64::MAX);
        let running = Arc::clone(&self.running);
        let cancel = Arc::clone(&self.cancel);
        let failure_counts = Arc::clone(&self.failure_counts);

        let handle = tokio::spawn(async move {
            let mut timer = interval(config.check_interval);
            // The first tick fires immediately; skip it so the first check
            // happens after `check_interval`.
            timer.tick().await;

            loop {
                tokio::select! {
                    _ = timer.tick() => {
                        if !running.load(Ordering::Relaxed) {
                            break;
                        }
                        perform_health_checks(&store, &config, &failure_counts).await;
                    }
                    () = cancel.notified() => {
                        break;
                    }
                }
            }
        });

        {
            let mut guard = self.handle.lock().await;
            *guard = Some(handle);
        }
        info!(interval_ms = check_interval_ms, "health monitor started");
    }

    /// Signal the background task to stop and await its completion.
    pub async fn stop(&self) {
        self.running.store(false, Ordering::Relaxed);
        self.cancel.notify_one();

        {
            let mut guard = self.handle.lock().await;
            if let Some(handle) = guard.take() {
                handle.abort();
            }
        }
        info!("health monitor stopped");
    }

    /// Check whether the monitor is currently running.
    pub fn is_running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }
}

impl Drop for HealthMonitor {
    fn drop(&mut self) {
        self.running.store(false, Ordering::Relaxed);
        self.cancel.notify_one();
    }
}

/// Run one pass of health checks over all registered agents.
async fn perform_health_checks(
    store: &Arc<dyn AgentStore>, config: &HealthConfig,
    failure_counts: &Arc<tokio::sync::RwLock<HashMap<String, u32>>>,
) {
    let agents = match store.list().await {
        Ok(a) => a,
        Err(e) => {
            warn!(error = %e, "health check pass: failed to list agents");
            return;
        }
    };

    for agent in &agents {
        let new_status = check_single_agent(agent, config).await;
        let id = &agent.id;

        // Track consecutive failures.
        {
            let mut counts = failure_counts.write().await;
            match &new_status {
                HealthStatus::Healthy | HealthStatus::Degraded => {
                    counts.remove(id);
                }
                HealthStatus::Unhealthy | HealthStatus::Offline => {
                    let count = counts.entry(id.clone()).or_insert(0);
                    *count += 1;
                }
                HealthStatus::Unknown => {}
            }
        }

        // If the failure count exceeds the threshold, promote to Offline.
        let final_status = {
            let counts = failure_counts.read().await;
            let fails = counts.get(id).copied().unwrap_or(0);
            if fails >= config.offline_threshold && new_status == HealthStatus::Unhealthy {
                HealthStatus::Offline
            } else {
                new_status
            }
        };

        if let Err(e) = store.update_health(id, final_status).await {
            warn!(agent_id = %id, error = %e, "failed to update health in store");
        }
    }
}

/// Ping a single agent endpoint and return its derived health status.
async fn check_single_agent(
    agent: &crate::types::AgentEntry, config: &HealthConfig,
) -> HealthStatus {
    let url = &agent.endpoint_url;

    let result = timeout(config.ping_timeout, reqwest::Client::new().get(url).send()).await;

    match result {
        Ok(Ok(response)) => {
            let status_code = response.status().as_u16();
            debug!(agent_id = %agent.id, status_code, "health check response");
            if status_code >= 500 {
                HealthStatus::Unhealthy
            } else if status_code >= 400 {
                HealthStatus::Degraded
            } else {
                HealthStatus::Healthy
            }
        }
        Ok(Err(e)) => {
            debug!(agent_id = %agent.id, error = %e, "health check request error");
            HealthStatus::Unhealthy
        }
        Err(_) => {
            debug!(agent_id = %agent.id, "health check timed out");
            HealthStatus::Unhealthy
        }
    }
}

/// Run a one-shot health check for a single agent (used by `AgentRegistry::health_check`).
///
/// # Errors
///
/// Returns `RegistryError::HttpError` if the HTTP request fails.
/// Returns `RegistryError::Timeout` if the request exceeds `ping_timeout`.
pub async fn ping_agent(
    agent: &crate::types::AgentEntry, ping_timeout: Duration,
) -> Result<HealthStatus, RegistryError> {
    let url = &agent.endpoint_url;

    let result = timeout(ping_timeout, reqwest::Client::new().get(url).send()).await;

    match result {
        Ok(Ok(response)) => {
            let status_code = response.status().as_u16();
            if status_code >= 500 {
                Ok(HealthStatus::Unhealthy)
            } else if status_code >= 400 {
                Ok(HealthStatus::Degraded)
            } else {
                Ok(HealthStatus::Healthy)
            }
        }
        Ok(Err(e)) => Err(RegistryError::HttpError {
            url: url.clone(),
            reason: e.to_string(),
        }),
        Err(_) => Err(RegistryError::Timeout {
            operation: format!("health check for {}", agent.id),
            duration: ping_timeout,
        }),
    }
}
