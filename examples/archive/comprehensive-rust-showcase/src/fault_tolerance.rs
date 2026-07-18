use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CircuitBreakerState {
    Closed,
    Open,
    HalfOpen,
}

#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    id: Uuid,
    state: Arc<RwLock<CircuitBreakerState>>,
    failure_count: Arc<RwLock<usize>>,
    success_count: Arc<RwLock<usize>>,
    last_failure_time: Arc<RwLock<Option<SystemTime>>>,
    failure_threshold: usize,
    success_threshold: usize,
    timeout: Duration,
}

impl CircuitBreaker {
    pub fn new(
        failure_threshold: usize,
        success_threshold: usize,
        timeout: Duration,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            state: Arc::new(RwLock::new(CircuitBreakerState::Closed)),
            failure_count: Arc::new(RwLock::new(0)),
            success_count: Arc::new(RwLock::new(0)),
            last_failure_time: Arc::new(RwLock::new(None)),
            failure_threshold,
            success_threshold,
            timeout,
        }
    }

    pub async fn call<F, T>(&self, f: F) -> Result<T>
    where
        F: Fn() -> Result<T>,
    {
        let state = *self.state.read().await;

        match state {
            CircuitBreakerState::Closed => self.call_closed(f).await,
            CircuitBreakerState::Open => self.call_open().await,
            CircuitBreakerState::HalfOpen => self.call_half_open(f).await,
        }
    }

    async fn call_closed<F, T>(&self, f: F) -> Result<T>
    where
        F: Fn() -> Result<T>,
    {
        match f() {
            Ok(result) => {
                let mut count = self.failure_count.write().await;
                *count = 0;
                Ok(result)
            }
            Err(e) => {
                let mut count = self.failure_count.write().await;
                *count += 1;

                if *count >= self.failure_threshold {
                    let mut state = self.state.write().await;
                    *state = CircuitBreakerState::Open;

                    let mut last_time = self.last_failure_time.write().await;
                    *last_time = Some(SystemTime::now());
                }

                Err(e)
            }
        }
    }

    async fn call_open<T>(&self) -> Result<T> {
        let last_time = self.last_failure_time.read().await;

        if let Some(time) = *last_time {
            if let Ok(elapsed) = time.elapsed() {
                if elapsed >= self.timeout {
                    drop(last_time);
                    let mut state = self.state.write().await;
                    *state = CircuitBreakerState::HalfOpen;
                    let mut success = self.success_count.write().await;
                    *success = 0;
                    return Err(crate::error::AgentError::CircuitBreakerOpen(
                        "Circuit breaker is half-open, retrying".to_string(),
                    ));
                }
            }
        }

        Err(crate::error::AgentError::CircuitBreakerOpen(
            "Circuit breaker is open".to_string(),
        ))
    }

    async fn call_half_open<F, T>(&self, f: F) -> Result<T>
    where
        F: Fn() -> Result<T>,
    {
        match f() {
            Ok(result) => {
                let mut count = self.success_count.write().await;
                *count += 1;

                if *count >= self.success_threshold {
                    let mut state = self.state.write().await;
                    *state = CircuitBreakerState::Closed;

                    let mut failures = self.failure_count.write().await;
                    *failures = 0;
                }

                Ok(result)
            }
            Err(e) => {
                let mut state = self.state.write().await;
                *state = CircuitBreakerState::Open;

                let mut last_time = self.last_failure_time.write().await;
                *last_time = Some(SystemTime::now());

                Err(e)
            }
        }
    }

    pub async fn get_state(&self) -> CircuitBreakerState {
        *self.state.read().await
    }
}

#[derive(Debug, Clone)]
pub struct SupervisorTree {
    id: Uuid,
    children: Arc<RwLock<Vec<Uuid>>>,
    parent: Option<Uuid>,
}

impl SupervisorTree {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            children: Arc::new(RwLock::new(Vec::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            children: Arc::new(RwLock::new(Vec::new())),
            parent: Some(parent_id),
        }
    }

    pub async fn add_child(&self, child_id: Uuid) {
        let mut children = self.children.write().await;
        children.push(child_id);
    }

    pub async fn remove_child(&self, child_id: &Uuid) -> bool {
        let mut children = self.children.write().await;
        if let Some(pos) = children.iter().position(|id| id == child_id) {
            children.remove(pos);
            true
        } else {
            false
        }
    }

    pub async fn get_children(&self) -> Vec<Uuid> {
        self.children.read().await.clone()
    }

    pub fn get_parent(&self) -> Option<Uuid> {
        self.parent
    }

    pub fn get_id(&self) -> Uuid {
        self.id
    }
}

impl Default for SupervisorTree {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_closed_success() {
        let cb = CircuitBreaker::new(3, 2, Duration::from_secs(1));
        let result: Result<i32> = cb.call(|| Ok(42)).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[tokio::test]
    async fn test_circuit_breaker_closed_failure() {
        let cb = CircuitBreaker::new(3, 2, Duration::from_secs(1));
        let result: Result<i32> = cb
            .call(|| Err(crate::error::AgentError::ExecutionFailed("test".into())))
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_after_threshold() {
        let cb = CircuitBreaker::new(2, 1, Duration::from_millis(100));

        for _ in 0..2 {
            let _ = cb
                .call(|| Err::<i32, _>(crate::error::AgentError::ExecutionFailed("err".into())))
                .await;
        }

        let state = cb.get_state().await;
        assert_eq!(state, CircuitBreakerState::Open);
    }

    #[tokio::test]
    async fn test_circuit_breaker_half_open_timeout() {
        let cb = CircuitBreaker::new(1, 1, Duration::from_millis(10));

        let _ = cb
            .call(|| Err::<i32, _>(crate::error::AgentError::ExecutionFailed("err".into())))
            .await;

        assert_eq!(cb.get_state().await, CircuitBreakerState::Open);

        tokio::time::sleep(Duration::from_millis(20)).await;

        let result: Result<i32> =
            cb.call(|| Err::<i32, _>(crate::error::AgentError::ExecutionFailed("err".into())))
                .await;

        assert!(result.is_err());
    }

    #[test]
    fn test_supervisor_tree_creation() {
        let supervisor = SupervisorTree::new();
        assert!(supervisor.get_parent().is_none());
    }

    #[tokio::test]
    async fn test_supervisor_tree_add_child() {
        let supervisor = SupervisorTree::new();
        let child_id = Uuid::new_v4();

        supervisor.add_child(child_id).await;
        let children = supervisor.get_children().await;

        assert!(children.contains(&child_id));
    }

    #[tokio::test]
    async fn test_supervisor_tree_remove_child() {
        let supervisor = SupervisorTree::new();
        let child_id = Uuid::new_v4();

        supervisor.add_child(child_id).await;
        assert!(supervisor.remove_child(&child_id).await);
        assert!(!supervisor.remove_child(&child_id).await);

        let children = supervisor.get_children().await;
        assert!(!children.contains(&child_id));
    }

    #[test]
    fn test_supervisor_tree_with_parent() {
        let parent_id = Uuid::new_v4();
        let supervisor = SupervisorTree::with_parent(parent_id);
        assert_eq!(supervisor.get_parent(), Some(parent_id));
    }
}
