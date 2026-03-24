use crate::service_mesh::{ServiceStatus, Service};
use std::time::{Duration, SystemTime};
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    pub service_id: String,
    pub failure_threshold: usize,
    pub success_threshold: usize,
    pub timeout: Duration,
    failures: Arc<RwLock<usize>>,
    successes: Arc<RwLock<usize>>,
    last_failure: Arc<RwLock<Option<SystemTime>>>,
}

impl CircuitBreaker {
    pub fn new(
        service_id: impl Into<String>,
        failure_threshold: usize,
        success_threshold: usize,
        timeout: Duration,
    ) -> Self {
        Self {
            service_id: service_id.into(),
            failure_threshold,
            success_threshold,
            timeout,
            failures: Arc::new(RwLock::new(0)),
            successes: Arc::new(RwLock::new(0)),
            last_failure: Arc::new(RwLock::new(None)),
        }
    }

    pub async fn record_failure(&self) {
        let mut failures = self.failures.write().await;
        *failures += 1;

        let mut last = self.last_failure.write().await;
        *last = Some(SystemTime::now());
    }

    pub async fn record_success(&self) {
        let mut successes = self.successes.write().await;
        *successes += 1;

        let mut failures = self.failures.write().await;
        *failures = 0;
    }

    pub async fn is_open(&self) -> bool {
        let failures = *self.failures.read().await;
        if failures < self.failure_threshold {
            return false;
        }

        let last = self.last_failure.read().await;
        if let Some(time) = *last {
            if let Ok(elapsed) = time.elapsed() {
                return elapsed < self.timeout;
            }
        }

        false
    }

    pub async fn reset(&self) {
        let mut failures = self.failures.write().await;
        let mut successes = self.successes.write().await;
        *failures = 0;
        *successes = 0;
    }
}

#[derive(Debug, Clone)]
pub struct ServiceHealthMonitor {
    services: Arc<RwLock<Vec<ServiceHealth>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceHealth {
    pub service_id: String,
    pub status: ServiceStatus,
    pub last_check: SystemTime,
    pub consecutive_failures: usize,
}

impl ServiceHealthMonitor {
    pub fn new() -> Self {
        Self {
            services: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub async fn record_check(&self, service_id: String, status: ServiceStatus) {
        let mut services = self.services.write().await;
        
        if let Some(health) = services.iter_mut().find(|s| s.service_id == service_id) {
            health.status = status.clone();
            health.last_check = SystemTime::now();
            
            if status == ServiceStatus::Unhealthy {
                health.consecutive_failures += 1;
            } else {
                health.consecutive_failures = 0;
            }
        } else {
            services.push(ServiceHealth {
                service_id,
                status,
                last_check: SystemTime::now(),
                consecutive_failures: 0,
            });
        }
    }

    pub async fn get_health(&self, service_id: &str) -> Option<ServiceHealth> {
        let services = self.services.read().await;
        services.iter().find(|s| s.service_id == service_id).cloned()
    }

    pub async fn get_unhealthy_services(&self) -> Vec<ServiceHealth> {
        let services = self.services.read().await;
        services
            .iter()
            .filter(|s| s.status == ServiceStatus::Unhealthy)
            .cloned()
            .collect()
    }

    pub async fn get_all_health(&self) -> Vec<ServiceHealth> {
        self.services.read().await.clone()
    }
}

impl Default for ServiceHealthMonitor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_creation() {
        let cb = CircuitBreaker::new("service1", 3, 2, Duration::from_secs(1));
        assert_eq!(cb.service_id, "service1");
        assert!(!cb.is_open().await);
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_after_failures() {
        let cb = CircuitBreaker::new("service1", 2, 1, Duration::from_secs(10));
        
        cb.record_failure().await;
        assert!(!cb.is_open().await);
        
        cb.record_failure().await;
        assert!(cb.is_open().await);
    }

    #[tokio::test]
    async fn test_circuit_breaker_resets_on_success() {
        let cb = CircuitBreaker::new("service1", 2, 1, Duration::from_secs(10));
        
        cb.record_failure().await;
        cb.record_success().await;
        
        assert!(!cb.is_open().await);
    }

    #[tokio::test]
    async fn test_circuit_breaker_timeout() {
        let cb = CircuitBreaker::new("service1", 1, 1, Duration::from_millis(10));
        
        cb.record_failure().await;
        assert!(cb.is_open().await);
        
        tokio::time::sleep(Duration::from_millis(20)).await;
        assert!(!cb.is_open().await);
    }

    #[test]
    fn test_health_monitor_creation() {
        let monitor = ServiceHealthMonitor::new();
        assert_eq!(
            std::any::type_name_of_val(&monitor),
            std::any::type_name::<ServiceHealthMonitor>()
        );
    }

    #[tokio::test]
    async fn test_health_monitor_record_check() {
        let monitor = ServiceHealthMonitor::new();
        monitor
            .record_check("service1".to_string(), ServiceStatus::Healthy)
            .await;

        let health = monitor.get_health("service1").await;
        assert!(health.is_some());
        assert_eq!(health.unwrap().status, ServiceStatus::Healthy);
    }

    #[tokio::test]
    async fn test_health_monitor_consecutive_failures() {
        let monitor = ServiceHealthMonitor::new();
        
        monitor
            .record_check("service1".to_string(), ServiceStatus::Unhealthy)
            .await;
        monitor
            .record_check("service1".to_string(), ServiceStatus::Unhealthy)
            .await;

        let health = monitor.get_health("service1").await.unwrap();
        assert_eq!(health.consecutive_failures, 2);
    }

    #[tokio::test]
    async fn test_health_monitor_get_unhealthy() {
        let monitor = ServiceHealthMonitor::new();
        
        monitor
            .record_check("service1".to_string(), ServiceStatus::Healthy)
            .await;
        monitor
            .record_check("service2".to_string(), ServiceStatus::Unhealthy)
            .await;

        let unhealthy = monitor.get_unhealthy_services().await;
        assert_eq!(unhealthy.len(), 1);
        assert_eq!(unhealthy[0].service_id, "service2");
    }
}
