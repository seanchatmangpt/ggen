//! Task message passing transport between agents

use crate::{Task, TaskState};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{mpsc, RwLock};
use uuid::Uuid;

/// Transport errors
#[derive(Debug, Error)]
pub enum TransportError {
    #[error("Agent not found: {0}")]
    AgentNotFound(String),

    #[error("Channel send error: {0}")]
    SendError(String),

    #[error("Task not found: {0}")]
    TaskNotFound(Uuid),

    #[error("Serialization error: {0}")]
    SerializationError(String),
}

/// Task message types for agent communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TaskMessage {
    /// Create a new task
    Create(Task),

    /// Update an existing task
    Update(Task),

    /// Query task status
    Query(Uuid),

    /// Task status response
    Status(Task),

    /// Assign task to agent
    Assign { task_id: Uuid, agent_id: String },

    /// Task state transition notification
    StateChanged {
        task_id: Uuid,
        old_state: TaskState,
        new_state: TaskState,
    },

    /// Task completion notification
    Completed { task_id: Uuid, result: String },

    /// Task failure notification
    Failed { task_id: Uuid, reason: String },

    /// Acknowledge message receipt
    Ack { message_id: Uuid },
}

/// Message envelope with routing information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Envelope {
    /// Message ID
    pub id: Uuid,
    /// Source agent ID
    pub from: String,
    /// Destination agent ID
    pub to: String,
    /// Message payload
    pub message: TaskMessage,
    /// Message timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl Envelope {
    /// Create a new envelope
    pub fn new(from: String, to: String, message: TaskMessage) -> Self {
        Self {
            id: Uuid::new_v4(),
            from,
            to,
            message,
            timestamp: chrono::Utc::now(),
        }
    }
}

/// Transport layer for task message passing
pub struct Transport {
    /// Agent message channels
    agents: Arc<RwLock<HashMap<String, mpsc::UnboundedSender<Envelope>>>>,
    /// Task registry
    tasks: Arc<RwLock<HashMap<Uuid, Task>>>,
}

impl Transport {
    /// Create a new transport
    pub fn new() -> Self {
        Self {
            agents: Arc::new(RwLock::new(HashMap::new())),
            tasks: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register an agent with the transport
    pub async fn register_agent(
        &self,
        agent_id: String,
    ) -> Result<mpsc::UnboundedReceiver<Envelope>, TransportError> {
        let (tx, rx) = mpsc::unbounded_channel();
        let mut agents = self.agents.write().await;
        agents.insert(agent_id, tx);
        Ok(rx)
    }

    /// Unregister an agent
    pub async fn unregister_agent(&self, agent_id: &str) -> Result<(), TransportError> {
        let mut agents = self.agents.write().await;
        agents.remove(agent_id);
        Ok(())
    }

    /// Send a message to an agent
    pub async fn send(
        &self,
        from: String,
        to: String,
        message: TaskMessage,
    ) -> Result<(), TransportError> {
        let envelope = Envelope::new(from, to.clone(), message);

        let agents = self.agents.read().await;
        let sender = agents
            .get(&to)
            .ok_or_else(|| TransportError::AgentNotFound(to.clone()))?;

        sender
            .send(envelope)
            .map_err(|e| TransportError::SendError(e.to_string()))?;

        Ok(())
    }

    /// Broadcast a message to all agents
    pub async fn broadcast(
        &self,
        from: String,
        message: TaskMessage,
    ) -> Result<(), TransportError> {
        let agents = self.agents.read().await;

        for (agent_id, sender) in agents.iter() {
            if agent_id != &from {
                let envelope = Envelope::new(from.clone(), agent_id.clone(), message.clone());
                let _ = sender.send(envelope);
            }
        }

        Ok(())
    }

    /// Store a task in the registry
    pub async fn store_task(&self, task: Task) -> Result<(), TransportError> {
        let mut tasks = self.tasks.write().await;
        tasks.insert(task.id, task);
        Ok(())
    }

    /// Retrieve a task from the registry
    pub async fn get_task(&self, task_id: Uuid) -> Result<Task, TransportError> {
        let tasks = self.tasks.read().await;
        tasks
            .get(&task_id)
            .cloned()
            .ok_or(TransportError::TaskNotFound(task_id))
    }

    /// Update a task in the registry
    pub async fn update_task(&self, task: Task) -> Result<(), TransportError> {
        let mut tasks = self.tasks.write().await;
        if !tasks.contains_key(&task.id) {
            return Err(TransportError::TaskNotFound(task.id));
        }
        tasks.insert(task.id, task);
        Ok(())
    }

    /// List all tasks
    pub async fn list_tasks(&self) -> Result<Vec<Task>, TransportError> {
        let tasks = self.tasks.read().await;
        Ok(tasks.values().cloned().collect())
    }

    /// List tasks by state
    pub async fn list_tasks_by_state(
        &self,
        state: TaskState,
    ) -> Result<Vec<Task>, TransportError> {
        let tasks = self.tasks.read().await;
        Ok(tasks
            .values()
            .filter(|t| t.state == state)
            .cloned()
            .collect())
    }

    /// Get agent count
    pub async fn agent_count(&self) -> usize {
        let agents = self.agents.read().await;
        agents.len()
    }

    /// Get task count
    pub async fn task_count(&self) -> usize {
        let tasks = self.tasks.read().await;
        tasks.len()
    }
}

impl Default for Transport {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_register_agent() {
        let transport = Transport::new();
        let result = transport.register_agent("agent-1".to_string()).await;
        assert!(result.is_ok());
        assert_eq!(transport.agent_count().await, 1);
    }

    #[tokio::test]
    async fn test_unregister_agent() {
        let transport = Transport::new();
        transport
            .register_agent("agent-1".to_string())
            .await
            .unwrap();
        transport.unregister_agent("agent-1").await.unwrap();
        assert_eq!(transport.agent_count().await, 0);
    }

    #[tokio::test]
    async fn test_send_message() {
        let transport = Transport::new();
        let mut rx = transport
            .register_agent("agent-1".to_string())
            .await
            .unwrap();

        let task = Task::new("Test".to_string(), "agent-0".to_string());
        let message = TaskMessage::Create(task);

        transport
            .send("agent-0".to_string(), "agent-1".to_string(), message)
            .await
            .unwrap();

        let envelope = rx.recv().await.unwrap();
        assert_eq!(envelope.from, "agent-0");
        assert_eq!(envelope.to, "agent-1");
        assert!(matches!(envelope.message, TaskMessage::Create(_)));
    }

    #[tokio::test]
    async fn test_send_to_nonexistent_agent() {
        let transport = Transport::new();
        let task = Task::new("Test".to_string(), "agent-0".to_string());
        let message = TaskMessage::Create(task);

        let result = transport
            .send("agent-0".to_string(), "agent-1".to_string(), message)
            .await;

        assert!(matches!(result, Err(TransportError::AgentNotFound(_))));
    }

    #[tokio::test]
    async fn test_broadcast() {
        let transport = Transport::new();
        let mut rx1 = transport
            .register_agent("agent-1".to_string())
            .await
            .unwrap();
        let mut rx2 = transport
            .register_agent("agent-2".to_string())
            .await
            .unwrap();

        let task = Task::new("Test".to_string(), "agent-0".to_string());
        let message = TaskMessage::Create(task);

        transport
            .broadcast("agent-0".to_string(), message)
            .await
            .unwrap();

        let envelope1 = rx1.recv().await.unwrap();
        let envelope2 = rx2.recv().await.unwrap();

        assert_eq!(envelope1.from, "agent-0");
        assert_eq!(envelope2.from, "agent-0");
    }

    #[tokio::test]
    async fn test_store_and_get_task() {
        let transport = Transport::new();
        let task = Task::new("Test".to_string(), "agent-1".to_string());
        let task_id = task.id;

        transport.store_task(task).await.unwrap();
        let retrieved = transport.get_task(task_id).await.unwrap();

        assert_eq!(retrieved.id, task_id);
        assert_eq!(retrieved.title, "Test");
    }

    #[tokio::test]
    async fn test_update_task() {
        let transport = Transport::new();
        let mut task = Task::new("Test".to_string(), "agent-1".to_string());
        let task_id = task.id;

        transport.store_task(task.clone()).await.unwrap();

        task.state = TaskState::Running;
        transport.update_task(task).await.unwrap();

        let retrieved = transport.get_task(task_id).await.unwrap();
        assert_eq!(retrieved.state, TaskState::Running);
    }

    #[tokio::test]
    async fn test_list_tasks() {
        let transport = Transport::new();
        let task1 = Task::new("Test 1".to_string(), "agent-1".to_string());
        let task2 = Task::new("Test 2".to_string(), "agent-1".to_string());

        transport.store_task(task1).await.unwrap();
        transport.store_task(task2).await.unwrap();

        let tasks = transport.list_tasks().await.unwrap();
        assert_eq!(tasks.len(), 2);
    }

    #[tokio::test]
    async fn test_list_tasks_by_state() {
        let transport = Transport::new();
        let mut task1 = Task::new("Test 1".to_string(), "agent-1".to_string());
        task1.state = TaskState::Running;
        let task2 = Task::new("Test 2".to_string(), "agent-1".to_string());

        transport.store_task(task1).await.unwrap();
        transport.store_task(task2).await.unwrap();

        let running = transport
            .list_tasks_by_state(TaskState::Running)
            .await
            .unwrap();
        let created = transport
            .list_tasks_by_state(TaskState::Created)
            .await
            .unwrap();

        assert_eq!(running.len(), 1);
        assert_eq!(created.len(), 1);
    }

    #[tokio::test]
    async fn test_envelope_creation() {
        let task = Task::new("Test".to_string(), "agent-1".to_string());
        let message = TaskMessage::Create(task);
        let envelope = Envelope::new("agent-1".to_string(), "agent-2".to_string(), message);

        assert_eq!(envelope.from, "agent-1");
        assert_eq!(envelope.to, "agent-2");
        assert!(matches!(envelope.message, TaskMessage::Create(_)));
    }
}
