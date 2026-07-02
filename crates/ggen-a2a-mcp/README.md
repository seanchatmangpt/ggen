# ggen-a2a-mcp

Agent-to-Agent (A2A) protocol implementation and Model Context Protocol (MCP) server for multi-agent coordination.

This crate manages multi-agent coordination within the `ggen` framework. It implements task state machines, message envelopes, agent capabilities registries, and standard I/O MCP servers to bridge external agent hosts to the core code generation pipeline.

## Features

- **A2A Task State Machine**: Defines the lifecycle of task coordination (`Task`, `TaskState`, `TaskStateMachine`, `StateTransition`).
- **Artifact Management**: Types representing intermediate inputs and outputs passed between agents (`Artifact`, `ArtifactCollection`).
- **Agent Registry & Health**: Catalog for mapping agent locations, resolving capability queries, and polling statuses (`AgentRegistry`, `HealthMonitor`).
- **Unified Agent Port Primitives**: Abstracts messaging ports and dispatch handlers (`Agent`, `Message`, `Port`, `UnifiedAgent`).
- **Orchestration MCP Server**: Bridges task execution requests (like `ggen.construct`) over the Model Context Protocol to run pipelines.

## Usage

### 1. Driving an A2A Task State Machine

```rust
use ggen_a2a_mcp::{Task, TaskState, StateTransition, TaskStateMachine};
use uuid::Uuid;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let task_id = Uuid::new_v4().to_string();
    let mut task = Task {
        id: task_id,
        state: TaskState::Pending,
        assigned_agent: Some("agent-paul".to_string()),
        description: "Generate DB schema and verify".to_string(),
        artifacts: vec![],
    };

    // Transition state from Pending to Running
    assert!(TaskStateMachine::can_transition(&task.state, &TaskState::Running));
    task.state = TaskState::Running;

    println!("Task {} status: {:?}", task.id, task.state);
    Ok(())
}
```

### 2. Exposing the MCP Server

The orchestration MCP server exposes tools such as `ggen.construct` (runs the core code generation pipeline using a local `ggen.toml` manifest) and `hello` (greeting test tool).

To serve the MCP server over stdio:
```rust
use ggen_a2a_mcp::mcp_server::GgenMcpServer;
use rmcp::RoleServer;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Standard setup to serve GgenMcpServer tools over stdio
    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let server = GgenMcpServer::new(); // initializes the tool router
    
    // Serve runs asynchronously
    let service = rmcp::ServerHandler::serve(server, (stdin, stdout)).await?;
    service.waiting().await?;

    Ok(())
}
```

## Architecture

- **`src/a2a/`**: Protocols for task transitions, artifact collections, state machines, and message transports.
- **`src/a2a_generated/`**: Direct port-to-port messaging schemas, Unified Agent definitions, and payload serialization adapters.
- **`src/a2a_registry/`**: Registry database to track active coordination entities and monitor node health checks.
- **`src/mcp_server.rs`**: Core MCP server implementation routing tools like `ggen.construct` to the generation pipeline.
- **`src/mcp_packs.rs`**: Tool registry adapters mapping routes to promoted packs.
