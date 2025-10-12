//! Type-safe identifier types
//!
//! This module provides type-safe ID types that prevent mixing different
//! kinds of identifiers and ensure memory safety through non-zero types.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::ids::{ContainerId, SessionId, TaskId};
//!
//! let container_id = ContainerId::new();
//! let session_id = SessionId::from_uuid(uuid);
//! let task_id = TaskId::new();
//! ```

use std::num::NonZeroU64;
use std::fmt;
use std::hash::{Hash, Hasher};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Base ID type with non-zero guarantee
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Id<T>(NonZeroU64, std::marker::PhantomData<T>);

impl<T> Id<T> {
    /// Create a new ID with a random value
    pub fn new() -> Self {
        Self(
            NonZeroU64::new(fastrand::u64(1..)).unwrap(),
            std::marker::PhantomData,
        )
    }

    /// Create an ID from a specific value
    pub fn from_value(value: u64) -> Option<Self> {
        NonZeroU64::new(value).map(|nz| Self(nz, std::marker::PhantomData))
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.get()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.get()
    }
}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> fmt::Display for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.get())
    }
}

impl<T> From<Id<T>> for u64 {
    fn from(id: Id<T>) -> Self {
        id.value()
    }
}

impl<T> TryFrom<u64> for Id<T> {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("ID value cannot be zero")
    }
}

/// Container ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ContainerId(Id<ContainerId>);

impl ContainerId {
    /// Create a new container ID
    pub fn new() -> Self {
        Self(Id::new())
    }

    /// Create a container ID from a value
    pub fn from_value(value: u64) -> Option<Self> {
        Id::from_value(value).map(Self)
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.value()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.as_value()
    }
}

impl Default for ContainerId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ContainerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "container-{}", self.0)
    }
}

impl From<ContainerId> for u64 {
    fn from(id: ContainerId) -> Self {
        id.value()
    }
}

impl TryFrom<u64> for ContainerId {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("Container ID value cannot be zero")
    }
}

/// Session ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SessionId(Id<SessionId>);

impl SessionId {
    /// Create a new session ID
    pub fn new() -> Self {
        Self(Id::new())
    }

    /// Create a session ID from a UUID
    pub fn from_uuid(uuid: Uuid) -> Self {
        // Convert UUID to u64 by taking the first 8 bytes
        let bytes = uuid.as_bytes();
        let value = u64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3],
            bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        Self(Id::from_value(value).unwrap_or_else(|| Id::new()))
    }

    /// Create a session ID from a value
    pub fn from_value(value: u64) -> Option<Self> {
        Id::from_value(value).map(Self)
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.value()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.as_value()
    }

    /// Convert to UUID
    pub fn to_uuid(self) -> Uuid {
        let bytes = self.value().to_le_bytes();
        let mut uuid_bytes = [0u8; 16];
        uuid_bytes[0..8].copy_from_slice(&bytes);
        Uuid::from_bytes(uuid_bytes)
    }
}

impl Default for SessionId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for SessionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "session-{}", self.0)
    }
}

impl From<SessionId> for u64 {
    fn from(id: SessionId) -> Self {
        id.value()
    }
}

impl TryFrom<u64> for SessionId {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("Session ID value cannot be zero")
    }
}

/// Task ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TaskId(Id<TaskId>);

impl TaskId {
    /// Create a new task ID
    pub fn new() -> Self {
        Self(Id::new())
    }

    /// Create a task ID from a value
    pub fn from_value(value: u64) -> Option<Self> {
        Id::from_value(value).map(Self)
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.value()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.as_value()
    }
}

impl Default for TaskId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for TaskId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "task-{}", self.0)
    }
}

impl From<TaskId> for u64 {
    fn from(id: TaskId) -> Self {
        id.value()
    }
}

impl TryFrom<u64> for TaskId {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("Task ID value cannot be zero")
    }
}

/// Test ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TestId(Id<TestId>);

impl TestId {
    /// Create a new test ID
    pub fn new() -> Self {
        Self(Id::new())
    }

    /// Create a test ID from a value
    pub fn from_value(value: u64) -> Option<Self> {
        Id::from_value(value).map(Self)
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.value()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.as_value()
    }
}

impl Default for TestId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for TestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "test-{}", self.0)
    }
}

impl From<TestId> for u64 {
    fn from(id: TestId) -> Self {
        id.value()
    }
}

impl TryFrom<u64> for TestId {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("Test ID value cannot be zero")
    }
}

/// Scenario ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ScenarioId(Id<ScenarioId>);

impl ScenarioId {
    /// Create a new scenario ID
    pub fn new() -> Self {
        Self(Id::new())
    }

    /// Create a scenario ID from a value
    pub fn from_value(value: u64) -> Option<Self> {
        Id::from_value(value).map(Self)
    }

    /// Get the underlying value
    pub fn value(self) -> u64 {
        self.0.value()
    }

    /// Get a reference to the underlying value
    pub fn as_value(&self) -> u64 {
        self.0.as_value()
    }
}

impl Default for ScenarioId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ScenarioId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scenario-{}", self.0)
    }
}

impl From<ScenarioId> for u64 {
    fn from(id: ScenarioId) -> Self {
        id.value()
    }
}

impl TryFrom<u64> for ScenarioId {
    type Error = &'static str;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Self::from_value(value).ok_or("Scenario ID value cannot be zero")
    }
}

/// ID registry for tracking and managing IDs
#[derive(Debug, Default)]
pub struct IdRegistry {
    containers: std::collections::HashSet<ContainerId>,
    sessions: std::collections::HashSet<SessionId>,
    tasks: std::collections::HashSet<TaskId>,
    tests: std::collections::HashSet<TestId>,
    scenarios: std::collections::HashSet<ScenarioId>,
}

impl IdRegistry {
    /// Create a new ID registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a container ID
    pub fn register_container(&mut self, id: ContainerId) -> bool {
        self.containers.insert(id)
    }

    /// Register a session ID
    pub fn register_session(&mut self, id: SessionId) -> bool {
        self.sessions.insert(id)
    }

    /// Register a task ID
    pub fn register_task(&mut self, id: TaskId) -> bool {
        self.tasks.insert(id)
    }

    /// Register a test ID
    pub fn register_test(&mut self, id: TestId) -> bool {
        self.tests.insert(id)
    }

    /// Register a scenario ID
    pub fn register_scenario(&mut self, id: ScenarioId) -> bool {
        self.scenarios.insert(id)
    }

    /// Check if a container ID is registered
    pub fn is_container_registered(&self, id: &ContainerId) -> bool {
        self.containers.contains(id)
    }

    /// Check if a session ID is registered
    pub fn is_session_registered(&self, id: &SessionId) -> bool {
        self.sessions.contains(id)
    }

    /// Check if a task ID is registered
    pub fn is_task_registered(&self, id: &TaskId) -> bool {
        self.tasks.contains(id)
    }

    /// Check if a test ID is registered
    pub fn is_test_registered(&self, id: &TestId) -> bool {
        self.tests.contains(id)
    }

    /// Check if a scenario ID is registered
    pub fn is_scenario_registered(&self, id: &ScenarioId) -> bool {
        self.scenarios.contains(id)
    }

    /// Unregister a container ID
    pub fn unregister_container(&mut self, id: &ContainerId) -> bool {
        self.containers.remove(id)
    }

    /// Unregister a session ID
    pub fn unregister_session(&mut self, id: &SessionId) -> bool {
        self.sessions.remove(id)
    }

    /// Unregister a task ID
    pub fn unregister_task(&mut self, id: &TaskId) -> bool {
        self.tasks.remove(id)
    }

    /// Unregister a test ID
    pub fn unregister_test(&mut self, id: &TestId) -> bool {
        self.tests.remove(id)
    }

    /// Unregister a scenario ID
    pub fn unregister_scenario(&mut self, id: &ScenarioId) -> bool {
        self.scenarios.remove(id)
    }

    /// Get all registered container IDs
    pub fn container_ids(&self) -> &std::collections::HashSet<ContainerId> {
        &self.containers
    }

    /// Get all registered session IDs
    pub fn session_ids(&self) -> &std::collections::HashSet<SessionId> {
        &self.sessions
    }

    /// Get all registered task IDs
    pub fn task_ids(&self) -> &std::collections::HashSet<TaskId> {
        &self.tasks
    }

    /// Get all registered test IDs
    pub fn test_ids(&self) -> &std::collections::HashSet<TestId> {
        &self.tests
    }

    /// Get all registered scenario IDs
    pub fn scenario_ids(&self) -> &std::collections::HashSet<ScenarioId> {
        &self.scenarios
    }

    /// Get total number of registered IDs
    pub fn total_count(&self) -> usize {
        self.containers.len()
            + self.sessions.len()
            + self.tasks.len()
            + self.tests.len()
            + self.scenarios.len()
    }

    /// Clear all registered IDs
    pub fn clear(&mut self) {
        self.containers.clear();
        self.sessions.clear();
        self.tasks.clear();
        self.tests.clear();
        self.scenarios.clear();
    }
}

/// Convenience functions for creating IDs
pub fn container_id() -> ContainerId {
    ContainerId::new()
}

pub fn session_id() -> SessionId {
    SessionId::new()
}

pub fn task_id() -> TaskId {
    TaskId::new()
}

pub fn test_id() -> TestId {
    TestId::new()
}

pub fn scenario_id() -> ScenarioId {
    ScenarioId::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_id() {
        let id1 = ContainerId::new();
        let id2 = ContainerId::new();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
        
        let id3 = ContainerId::from_value(42).unwrap();
        assert_eq!(id3.value(), 42);
        
        assert!(ContainerId::from_value(0).is_none());
    }

    #[test]
    fn test_session_id() {
        let id1 = SessionId::new();
        let id2 = SessionId::new();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
        
        let uuid = Uuid::new_v4();
        let id3 = SessionId::from_uuid(uuid);
        let uuid_back = id3.to_uuid();
        assert_eq!(uuid, uuid_back);
    }

    #[test]
    fn test_task_id() {
        let id1 = TaskId::new();
        let id2 = TaskId::new();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
    }

    #[test]
    fn test_test_id() {
        let id1 = TestId::new();
        let id2 = TestId::new();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
    }

    #[test]
    fn test_scenario_id() {
        let id1 = ScenarioId::new();
        let id2 = ScenarioId::new();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
    }

    #[test]
    fn test_id_display() {
        let container_id = ContainerId::new();
        let session_id = SessionId::new();
        let task_id = TaskId::new();
        let test_id = TestId::new();
        let scenario_id = ScenarioId::new();
        
        assert!(format!("{}", container_id).starts_with("container-"));
        assert!(format!("{}", session_id).starts_with("session-"));
        assert!(format!("{}", task_id).starts_with("task-"));
        assert!(format!("{}", test_id).starts_with("test-"));
        assert!(format!("{}", scenario_id).starts_with("scenario-"));
    }

    #[test]
    fn test_id_conversion() {
        let container_id = ContainerId::new();
        let value: u64 = container_id.into();
        let back: ContainerId = value.try_into().unwrap();
        assert_eq!(container_id, back);
        
        let session_id = SessionId::new();
        let value: u64 = session_id.into();
        let back: SessionId = value.try_into().unwrap();
        assert_eq!(session_id, back);
    }

    #[test]
    fn test_id_registry() {
        let mut registry = IdRegistry::new();
        
        let container_id = ContainerId::new();
        let session_id = SessionId::new();
        let task_id = TaskId::new();
        let test_id = TestId::new();
        let scenario_id = ScenarioId::new();
        
        assert!(registry.register_container(container_id));
        assert!(registry.register_session(session_id));
        assert!(registry.register_task(task_id));
        assert!(registry.register_test(test_id));
        assert!(registry.register_scenario(scenario_id));
        
        assert!(registry.is_container_registered(&container_id));
        assert!(registry.is_session_registered(&session_id));
        assert!(registry.is_task_registered(&task_id));
        assert!(registry.is_test_registered(&test_id));
        assert!(registry.is_scenario_registered(&scenario_id));
        
        assert_eq!(registry.total_count(), 5);
        
        assert!(registry.unregister_container(&container_id));
        assert!(!registry.is_container_registered(&container_id));
        
        assert_eq!(registry.total_count(), 4);
        
        registry.clear();
        assert_eq!(registry.total_count(), 0);
    }

    #[test]
    fn test_convenience_functions() {
        let _container_id = container_id();
        let _session_id = session_id();
        let _task_id = task_id();
        let _test_id = test_id();
        let _scenario_id = scenario_id();
        
        // Just verify they compile and create valid IDs
        assert!(_container_id.value() > 0);
        assert!(_session_id.value() > 0);
        assert!(_task_id.value() > 0);
        assert!(_test_id.value() > 0);
        assert!(_scenario_id.value() > 0);
    }

    #[test]
    fn test_id_hash() {
        use std::collections::HashMap;
        
        let mut map = HashMap::new();
        let container_id = ContainerId::new();
        let session_id = SessionId::new();
        
        map.insert(container_id, "container");
        map.insert(session_id, "session");
        
        assert_eq!(map.get(&container_id), Some(&"container"));
        assert_eq!(map.get(&session_id), Some(&"session"));
    }

    #[test]
    fn test_id_serialization() {
        let container_id = ContainerId::new();
        let session_id = SessionId::new();
        
        let serialized = serde_json::to_string(&container_id).unwrap();
        let deserialized: ContainerId = serde_json::from_str(&serialized).unwrap();
        assert_eq!(container_id, deserialized);
        
        let serialized = serde_json::to_string(&session_id).unwrap();
        let deserialized: SessionId = serde_json::from_str(&serialized).unwrap();
        assert_eq!(session_id, deserialized);
    }
}
