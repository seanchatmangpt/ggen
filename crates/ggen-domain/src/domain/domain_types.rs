//! Domain types module

pub enum AgentType {
    Workflow,
    MessageRouter,
    TaskExecutor,
}

pub struct Agent {
    pub id: String,
    pub name: String,
    pub agent_type: AgentType,
}
