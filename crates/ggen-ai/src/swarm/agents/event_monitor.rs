//! Event Monitor Agent - Monitors for events and triggers swarm execution

use crate::error::Result;
use super::{BaseAgent, EventSource, EventStream, SystemEvent};
use async_trait::async_trait;

/// Event Monitor Agent implementation
pub struct EventMonitorAgentImpl {
    base: BaseAgent,
}

impl EventMonitorAgentImpl {
    pub fn new(base: BaseAgent) -> Self {
        Self { base }
    }
}

/// Stub EventSource implementation
pub struct StubEventSource;

#[async_trait]
impl EventSource for StubEventSource {
    fn name(&self) -> &str {
        "stub-event-source"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        Ok(vec![])
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        Ok(Box::new(StubEventStream))
    }
}

/// Stub EventStream implementation
pub struct StubEventStream;

#[async_trait]
impl EventStream for StubEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        None
    }
}
