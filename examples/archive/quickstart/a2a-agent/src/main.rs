//! A2A Agent Quickstart Example
//!
//! This example demonstrates basic agent-to-agent communication
//! with state machine lifecycle management.
//!
//! Run: cargo run --bin a2a_agent_example

use anyhow::Result;
use std::collections::VecDeque;
use uuid::Uuid;

/// Agent state machine
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AgentState {
    Initializing,
    Ready,
    Processing,
    Idle,
    Error,
    Terminated,
}

impl AgentState {
    pub fn code(&self) -> &'static str {
        match self {
            AgentState::Initializing => "INIT",
            AgentState::Ready => "READY",
            AgentState::Processing => "BUSY",
            AgentState::Idle => "IDLE",
            AgentState::Error => "ERR",
            AgentState::Terminated => "DONE",
        }
    }
}

/// Simple agent with state machine
pub struct Agent {
    pub id: String,
    pub name: String,
    state: AgentState,
    message_queue: VecDeque<String>,
    error_count: u32,
}

impl Agent {
    pub fn new(name: impl Into<String>) -> Self {
        Agent {
            id: Uuid::new_v4().to_string(),
            name: name.into(),
            state: AgentState::Initializing,
            message_queue: VecDeque::new(),
            error_count: 0,
        }
    }

    pub fn state(&self) -> AgentState {
        self.state
    }

    pub fn state_code(&self) -> &'static str {
        self.state.code()
    }

    /// Transition to new state with validation
    pub fn transition(&mut self, to: AgentState) -> Result<()> {
        let from = self.state;

        // Validate: only certain transitions allowed
        let valid = match (from, to) {
            (AgentState::Initializing, AgentState::Ready) => true,
            (AgentState::Ready, AgentState::Processing) => true,
            (AgentState::Processing, AgentState::Idle) => true,
            (AgentState::Idle, AgentState::Processing) => true,
            (_, AgentState::Error) => true,
            (AgentState::Error, AgentState::Ready) => self.error_count < 3,
            (_, AgentState::Terminated) => true,
            _ => false,
        };

        if !valid {
            return Err(anyhow::anyhow!("Invalid transition: {:?} -> {:?}", from, to));
        }

        println!("  {} {} -> {}", self.name, from.code(), to.code());
        self.state = to;
        Ok(())
    }

    pub fn mark_ready(&mut self) -> Result<()> {
        self.transition(AgentState::Ready)
    }

    pub fn mark_processing(&mut self) -> Result<()> {
        self.transition(AgentState::Processing)
    }

    pub fn mark_idle(&mut self) -> Result<()> {
        self.transition(AgentState::Idle)
    }

    pub fn mark_error(&mut self) -> Result<()> {
        self.error_count += 1;
        self.transition(AgentState::Error)
    }

    pub fn recover(&mut self) -> Result<()> {
        if self.error_count >= 3 {
            return Err(anyhow::anyhow!("Max retries exceeded"));
        }
        self.error_count = 0;
        self.transition(AgentState::Ready)
    }

    pub fn terminate(&mut self) -> Result<()> {
        self.transition(AgentState::Terminated)
    }

    pub fn send_message(&mut self, msg: String) {
        self.message_queue.push_back(msg);
    }

    pub fn receive_message(&mut self) -> Option<String> {
        self.message_queue.pop_front()
    }
}

fn main() -> Result<()> {
    println!("🤖 A2A Agent Quickstart Example");
    println!("===============================\n");

    // Create two agents
    let mut agent_a = Agent::new("Agent-A");
    let mut agent_b = Agent::new("Agent-B");

    println!("✅ Created 2 agents\n");

    // Initialize agents
    println!("🔄 State transitions:");
    agent_a.mark_ready()?;
    agent_b.mark_ready()?;

    // Agent A sends message to Agent B
    agent_a.send_message("Hello from Agent A!".to_string());
    println!("\n📤 Agent A sent: 'Hello from Agent A!'");

    // Agent B receives message
    if let Some(msg) = agent_a.receive_message() {
        println!("📥 Agent B received: '{}'", msg);
    }

    // Simulate task processing
    println!("\n🔄 Task processing:");
    agent_a.mark_processing()?;
    agent_a.mark_idle()?;

    // Simulate error and recovery
    println!("\n⚠️  Error handling:");
    agent_b.mark_processing()?;
    agent_b.mark_error()?;
    agent_b.recover()?;

    println!("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("FINAL STATE");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("Agent A: {} ({})", agent_a.name, agent_a.state_code());
    println!("Agent B: {} ({})", agent_b.name, agent_b.state_code());
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    println!("\n✅ Example complete!");

    println!("\n💡 Key Concepts:");
    println!("  - Agents have state machines with validated transitions");
    println!("  - Agents can send/receive messages via queues");
    println!("  - Errors are tracked with retry limits");
    println!("  - State transitions are logged for observability");

    Ok(())
}
