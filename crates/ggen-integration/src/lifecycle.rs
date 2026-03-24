//! Lifecycle management for system coordination

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Lifecycle states for the system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LifecycleState {
    /// System is stopped
    Stopped,
    /// System is starting up
    Starting,
    /// System is running normally
    Running,
    /// System is stopping
    Stopping,
    /// System is in error state
    Error,
}

impl std::fmt::Display for LifecycleState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stopped => write!(f, "Stopped"),
            Self::Starting => write!(f, "Starting"),
            Self::Running => write!(f, "Running"),
            Self::Stopping => write!(f, "Stopping"),
            Self::Error => write!(f, "Error"),
        }
    }
}

/// Lifecycle transition event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleEvent {
    /// Source state
    pub from: LifecycleState,
    /// Target state
    pub to: LifecycleState,
    /// Timestamp of transition
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Additional metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl LifecycleEvent {
    /// Create a new lifecycle event
    #[must_use]
    pub fn new(from: LifecycleState, to: LifecycleState) -> Self {
        Self {
            from,
            to,
            timestamp: chrono::Utc::now(),
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Add metadata to the event
    #[must_use]
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}

/// Lifecycle manager tracking state transitions
pub struct LifecycleManager {
    current_state: LifecycleState,
    event_history: Vec<LifecycleEvent>,
}

impl LifecycleManager {
    /// Create a new lifecycle manager
    #[must_use]
    pub fn new() -> Self {
        Self {
            current_state: LifecycleState::Stopped,
            event_history: Vec::new(),
        }
    }

    /// Get current state
    #[must_use]
    pub const fn current_state(&self) -> LifecycleState {
        self.current_state
    }

    /// Transition to a new state
    pub fn transition(&mut self, to: LifecycleState) -> Result<(), LifecycleError> {
        // Validate transition
        self.validate_transition(self.current_state, to)?;

        // Record event
        let event = LifecycleEvent::new(self.current_state, to);
        self.event_history.push(event);

        // Update state
        self.current_state = to;

        tracing::info!("Lifecycle transition: {}", to);
        Ok(())
    }

    /// Validate if a transition is allowed
    fn validate_transition(
        &self,
        from: LifecycleState,
        to: LifecycleState,
    ) -> Result<(), LifecycleError> {
        let allowed = match (from, to) {
            // Allowed transitions
            (LifecycleState::Stopped, LifecycleState::Starting) => true,
            (LifecycleState::Starting, LifecycleState::Running) => true,
            (LifecycleState::Starting, LifecycleState::Error) => true,
            (LifecycleState::Running, LifecycleState::Stopping) => true,
            (LifecycleState::Running, LifecycleState::Error) => true,
            (LifecycleState::Stopping, LifecycleState::Stopped) => true,
            (LifecycleState::Stopping, LifecycleState::Error) => true,
            (LifecycleState::Error, LifecycleState::Stopped) => true,
            // Same state is always allowed
            (from, to) if from == to => true,
            // All other transitions are invalid
            _ => false,
        };

        if allowed {
            Ok(())
        } else {
            Err(LifecycleError::InvalidTransition { from, to })
        }
    }

    /// Get event history
    #[must_use]
    pub fn event_history(&self) -> &[LifecycleEvent] {
        &self.event_history
    }

    /// Clear event history
    pub fn clear_history(&mut self) {
        self.event_history.clear();
    }
}

impl Default for LifecycleManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Lifecycle errors
#[derive(Error, Debug)]
pub enum LifecycleError {
    /// Invalid state transition
    #[error("Invalid lifecycle transition from {from} to {to}")]
    InvalidTransition {
        /// Source state
        from: LifecycleState,
        /// Target state
        to: LifecycleState,
    },

    /// Timeout during transition
    #[error("Lifecycle transition timeout: {0}")]
    Timeout(String),

    /// Internal error
    #[error("Internal lifecycle error: {0}")]
    Internal(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lifecycle_manager_creation() {
        // Arrange & Act
        let manager = LifecycleManager::new();

        // Assert
        assert_eq!(manager.current_state(), LifecycleState::Stopped);
        assert_eq!(manager.event_history().len(), 0);
    }

    #[test]
    fn test_lifecycle_valid_transitions() {
        // Arrange
        let mut manager = LifecycleManager::new();

        // Act & Assert
        assert!(manager.transition(LifecycleState::Starting).is_ok());
        assert_eq!(manager.current_state(), LifecycleState::Starting);

        assert!(manager.transition(LifecycleState::Running).is_ok());
        assert_eq!(manager.current_state(), LifecycleState::Running);

        assert!(manager.transition(LifecycleState::Stopping).is_ok());
        assert_eq!(manager.current_state(), LifecycleState::Stopping);

        assert!(manager.transition(LifecycleState::Stopped).is_ok());
        assert_eq!(manager.current_state(), LifecycleState::Stopped);
    }

    #[test]
    fn test_lifecycle_invalid_transition() {
        // Arrange
        let mut manager = LifecycleManager::new();

        // Act
        let result = manager.transition(LifecycleState::Running);

        // Assert
        assert!(result.is_err());
        if let Err(LifecycleError::InvalidTransition { from, to }) = result {
            assert_eq!(from, LifecycleState::Stopped);
            assert_eq!(to, LifecycleState::Running);
        }
    }

    #[test]
    fn test_lifecycle_error_state() {
        // Arrange
        let mut manager = LifecycleManager::new();

        // Act
        assert!(manager.transition(LifecycleState::Starting).is_ok());
        assert!(manager.transition(LifecycleState::Error).is_ok());

        // Assert
        assert_eq!(manager.current_state(), LifecycleState::Error);
    }

    #[test]
    fn test_lifecycle_event_history() {
        // Arrange
        let mut manager = LifecycleManager::new();

        // Act
        assert!(manager.transition(LifecycleState::Starting).is_ok());
        assert!(manager.transition(LifecycleState::Running).is_ok());

        // Assert
        assert_eq!(manager.event_history().len(), 2);
        assert_eq!(manager.event_history()[0].from, LifecycleState::Stopped);
        assert_eq!(manager.event_history()[0].to, LifecycleState::Starting);
        assert_eq!(manager.event_history()[1].from, LifecycleState::Starting);
        assert_eq!(manager.event_history()[1].to, LifecycleState::Running);
    }

    #[test]
    fn test_lifecycle_same_state_transition() {
        // Arrange
        let mut manager = LifecycleManager::new();

        // Act
        let result = manager.transition(LifecycleState::Stopped);

        // Assert
        assert!(result.is_ok());
        assert_eq!(manager.current_state(), LifecycleState::Stopped);
    }

    #[test]
    fn test_lifecycle_event_with_metadata() {
        // Arrange
        let event = LifecycleEvent::new(LifecycleState::Stopped, LifecycleState::Starting);

        // Act
        let event = event.with_metadata("reason".to_string(), "startup".to_string());

        // Assert
        assert_eq!(event.metadata.get("reason"), Some(&"startup".to_string()));
    }

    #[test]
    fn test_lifecycle_clear_history() {
        // Arrange
        let mut manager = LifecycleManager::new();
        assert!(manager.transition(LifecycleState::Starting).is_ok());
        assert!(manager.transition(LifecycleState::Running).is_ok());

        // Act
        manager.clear_history();

        // Assert
        assert_eq!(manager.event_history().len(), 0);
        assert_eq!(manager.current_state(), LifecycleState::Running);
    }
}
