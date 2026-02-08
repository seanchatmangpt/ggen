# Autonomous System - Simple Design

**Version**: 1.0.0
**Stack**: Rust + Tokio + Claude Code Task Tool
**Deployment**: Systemd (Linux) / Launchd (macOS)

## Overview

A minimal autonomous execution system using only Claude Code's built-in Task tool. No external frameworks, no MCP servers, no complex dependencies - just a perpetual loop that spawns 20 concurrent agents to process a task queue with automatic state persistence and crash recovery.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Main Supervisor Loop                      │
│                     (perpetual process)                      │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ Load State File │
                    │ (.state.json)   │
                    └─────────────────┘
                              │
                              ▼
        ┌──────────────────────────────────────────┐
        │  Spawn 20 Task Tool Agents (concurrent)  │
        └──────────────────────────────────────────┘
                              │
         ┌────────────────────┼────────────────────┐
         ▼                    ▼                    ▼
    ┌────────┐          ┌────────┐          ┌────────┐
    │ Agent  │          │ Agent  │   ...    │ Agent  │
    │   #1   │          │   #2   │          │  #20   │
    └────────┘          └────────┘          └────────┘
         │                    │                    │
         └────────────────────┼────────────────────┘
                              ▼
                    ┌─────────────────┐
                    │   Task Queue    │
                    │ (shared state)  │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ Save State File │
                    │ (every 5 min)   │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ On SIGTERM:     │
                    │ - Save state    │
                    │ - Graceful exit │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ Systemd/Launchd │
                    │ auto-restarts   │
                    └─────────────────┘
```

## Core Components

### 1. Main Supervisor (`main.rs`)

**Responsibilities:**
- Initialize logging and signal handlers
- Load previous state from `.state.json`
- Spawn task processing loop
- Coordinate graceful shutdown

**Key Functions:**
```rust
async fn main() -> Result<()> {
    // Setup signal handlers (SIGTERM, SIGINT)
    // Load state from .state.json
    // Start supervisor loop
    // On shutdown: save state, wait for agents
}
```

### 2. Supervisor Loop (`supervisor.rs`)

**Responsibilities:**
- Spawn 20 concurrent agent tasks
- Monitor agent health
- Persist state every 5 minutes
- Handle task queue coordination

**Key Functions:**
```rust
async fn run_supervisor(state: Arc<Mutex<State>>) -> Result<()> {
    loop {
        // Spawn 20 agent tasks
        let handles = spawn_agents(state.clone(), 20).await;

        // Auto-save every 5 minutes
        tokio::select! {
            _ = interval(Duration::from_secs(300)) => {
                save_state(&state).await?;
            }
            _ = shutdown_signal() => {
                save_state(&state).await?;
                break;
            }
        }
    }
}
```

### 3. Task Agent (`agent.rs`)

**Responsibilities:**
- Pull next task from queue
- Execute using Claude Code Task tool
- Update task status
- Handle errors and retries

**Key Functions:**
```rust
async fn agent_worker(
    id: usize,
    state: Arc<Mutex<State>>,
) -> Result<()> {
    loop {
        // Get next pending task
        let task = {
            let mut s = state.lock().await;
            s.tasks.iter_mut()
                .find(|t| t.status == "pending")
                .map(|t| {
                    t.status = "in_progress".to_string();
                    t.clone()
                })
        };

        if let Some(task) = task {
            // Execute via Task tool
            match execute_task(&task).await {
                Ok(_) => update_status(state.clone(), task.id, "completed").await,
                Err(e) => handle_error(state.clone(), task.id, e).await,
            }
        } else {
            // No pending tasks - wait
            tokio::time::sleep(Duration::from_secs(5)).await;
        }
    }
}
```

### 4. Task Execution (`executor.rs`)

**Responsibilities:**
- Interface with Claude Code Task tool
- Parse task descriptions
- Handle execution results
- Implement retry logic

**Key Functions:**
```rust
async fn execute_task(task: &Task) -> Result<String> {
    // Use Claude Code Task tool
    // This is a placeholder - actual implementation would use
    // the Task tool API when available

    // For now, demonstrate the pattern:
    // 1. Parse task description
    // 2. Call Task tool with prompt
    // 3. Capture result
    // 4. Return outcome
}
```

### 5. State Management (`state.rs`)

**Responsibilities:**
- Load/save state to `.state.json`
- Provide thread-safe state access
- Implement state schema validation

**State Structure:**
```rust
#[derive(Serialize, Deserialize, Clone)]
pub struct State {
    pub tasks: Vec<Task>,
    pub checkpoint: String,  // ISO 8601 timestamp
    pub progress: Progress,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Task {
    pub id: u64,
    pub status: String,  // "pending" | "in_progress" | "completed" | "failed"
    pub desc: String,
    pub created_at: String,
    pub updated_at: String,
    pub retry_count: u32,
    pub result: Option<String>,
    pub error: Option<String>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Progress {
    pub completed: u64,
    pub total: u64,
    pub failed: u64,
}
```

## State Persistence

### File Location
- **Linux**: `/var/lib/ggen-autonomous/.state.json`
- **macOS**: `~/Library/Application Support/ggen-autonomous/.state.json`
- **Development**: `./.state.json`

### Persistence Strategy

1. **Auto-save**: Every 5 minutes
2. **Manual save**: On SIGTERM/SIGINT
3. **Atomic writes**: Write to `.state.json.tmp`, then rename
4. **Backup**: Keep last 3 states as `.state.json.1`, `.state.json.2`, `.state.json.3`

### State Recovery

```rust
async fn load_state() -> Result<State> {
    // Try primary state file
    if let Ok(state) = load_from_file(".state.json").await {
        return Ok(state);
    }

    // Try backups
    for i in 1..=3 {
        if let Ok(state) = load_from_file(&format!(".state.json.{}", i)).await {
            warn!("Recovered from backup #{}", i);
            return Ok(state);
        }
    }

    // Initialize new state
    Ok(State::default())
}
```

## Error Handling & Recovery

### Crash Recovery
1. Systemd/Launchd detects process exit
2. Automatic restart (with exponential backoff)
3. On restart: load `.state.json`
4. Resume processing from last checkpoint
5. In-progress tasks revert to "pending"

### Task Failures
```rust
async fn handle_error(
    state: Arc<Mutex<State>>,
    task_id: u64,
    error: Error,
) -> Result<()> {
    let mut s = state.lock().await;
    if let Some(task) = s.tasks.iter_mut().find(|t| t.id == task_id) {
        task.retry_count += 1;

        if task.retry_count >= 3 {
            task.status = "failed".to_string();
            task.error = Some(error.to_string());
            s.progress.failed += 1;
        } else {
            task.status = "pending".to_string();  // Retry
        }
    }
    Ok(())
}
```

### Agent Failures
- If agent panics: Tokio runtime spawns new agent
- If all agents fail: Supervisor exits, systemd restarts
- Max restart attempts: 5 in 60 seconds (systemd config)

## Deployment

### Systemd (Linux)

**Service File** (`/etc/systemd/system/ggen-autonomous.service`):
```ini
[Unit]
Description=ggen Autonomous System
After=network.target

[Service]
Type=simple
User=ggen
WorkingDirectory=/opt/ggen
ExecStart=/opt/ggen/target/release/ggen-autonomous
Restart=on-failure
RestartSec=10
StartLimitBurst=5
StartLimitIntervalSec=60

# Resource limits
LimitNOFILE=65536
MemoryMax=2G
CPUQuota=200%

# Security
NoNewPrivileges=true
PrivateTmp=true

[Install]
WantedBy=multi-user.target
```

**Commands:**
```bash
sudo systemctl daemon-reload
sudo systemctl enable ggen-autonomous
sudo systemctl start ggen-autonomous
sudo systemctl status ggen-autonomous
journalctl -u ggen-autonomous -f  # View logs
```

### Launchd (macOS)

**Plist File** (`~/Library/LaunchAgents/io.ggen.autonomous.plist`):
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>io.ggen.autonomous</string>

    <key>ProgramArguments</key>
    <array>
        <string>/usr/local/bin/ggen-autonomous</string>
    </array>

    <key>WorkingDirectory</key>
    <string>/Users/ggen/workspace</string>

    <key>RunAtLoad</key>
    <true/>

    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
        <key>Crashed</key>
        <true/>
    </dict>

    <key>ThrottleInterval</key>
    <integer>10</integer>

    <key>StandardOutPath</key>
    <string>/tmp/ggen-autonomous.log</string>

    <key>StandardErrorPath</key>
    <string>/tmp/ggen-autonomous.err</string>
</dict>
</plist>
```

**Commands:**
```bash
launchctl load ~/Library/LaunchAgents/io.ggen.autonomous.plist
launchctl start io.ggen.autonomous
launchctl list | grep ggen
launchctl stop io.ggen.autonomous
launchctl unload ~/Library/LaunchAgents/io.ggen.autonomous.plist
```

## Implementation Considerations

### 1. Concurrency
- Use `tokio::spawn` for agent tasks
- Share state via `Arc<Mutex<State>>`
- Consider `tokio::sync::RwLock` for read-heavy workloads
- Agent count configurable via env var: `AGENT_COUNT=20`

### 2. Task Distribution
```rust
// Simple round-robin with mutex
async fn get_next_task(state: Arc<Mutex<State>>) -> Option<Task> {
    let mut s = state.lock().await;
    s.tasks.iter_mut()
        .find(|t| t.status == "pending")
        .map(|t| {
            t.status = "in_progress".to_string();
            t.clone()
        })
}

// Or use a channel-based queue for better concurrency
fn create_task_queue(tasks: Vec<Task>) -> Receiver<Task> {
    let (tx, rx) = mpsc::channel(100);
    tokio::spawn(async move {
        for task in tasks {
            tx.send(task).await.ok();
        }
    });
    rx
}
```

### 3. Logging
```rust
use tracing::{info, warn, error};
use tracing_subscriber::fmt;

// Initialize in main
tracing_subscriber::fmt()
    .with_target(false)
    .with_thread_ids(true)
    .with_file(true)
    .with_line_number(true)
    .json()  // Structured JSON for parsing
    .init();

// Log in agents
info!(agent_id = 1, task_id = 42, "Processing task");
warn!(agent_id = 1, task_id = 42, error = %e, "Task failed, retrying");
```

### 4. Monitoring
```rust
// Expose metrics endpoint
async fn metrics_handler() -> impl IntoResponse {
    let state = STATE.lock().await;
    Json(json!({
        "tasks_total": state.progress.total,
        "tasks_completed": state.progress.completed,
        "tasks_failed": state.progress.failed,
        "tasks_pending": state.tasks.iter().filter(|t| t.status == "pending").count(),
        "checkpoint": state.checkpoint,
    }))
}

// Start metrics server (optional)
tokio::spawn(async {
    axum::Server::bind(&"127.0.0.1:9090".parse().unwrap())
        .serve(app.into_make_service())
        .await
});
```

### 5. Configuration
```rust
// Use environment variables
pub struct Config {
    pub agent_count: usize,
    pub save_interval_secs: u64,
    pub max_retries: u32,
    pub state_file: PathBuf,
}

impl Config {
    pub fn from_env() -> Self {
        Self {
            agent_count: env::var("AGENT_COUNT")
                .unwrap_or_else(|_| "20".to_string())
                .parse()
                .unwrap_or(20),
            save_interval_secs: env::var("SAVE_INTERVAL")
                .unwrap_or_else(|_| "300".to_string())
                .parse()
                .unwrap_or(300),
            max_retries: 3,
            state_file: PathBuf::from(
                env::var("STATE_FILE").unwrap_or_else(|_| ".state.json".to_string())
            ),
        }
    }
}
```

## Performance Characteristics

| Metric | Target | Notes |
|--------|--------|-------|
| Startup time | <2s | Load state + spawn agents |
| Task throughput | ~20/min | With 20 agents, 1 task/min each |
| State save time | <100ms | Atomic write, small state |
| Memory usage | <100MB | Lightweight, no heavy deps |
| Crash recovery | <5s | Systemd restart + state load |

## Security Considerations

1. **State file permissions**: `chmod 600 .state.json`
2. **User isolation**: Run as dedicated `ggen` user
3. **Resource limits**: Set in systemd/launchd config
4. **Input validation**: Validate task descriptions before execution
5. **Audit logging**: Log all task executions with timestamps

## Future Enhancements

1. **Task prioritization**: Add `priority` field to tasks
2. **Scheduling**: Add `scheduled_at` for delayed execution
3. **Dependencies**: Add `depends_on` for task chains
4. **Webhooks**: Notify on completion via HTTP POST
5. **Distributed**: Scale to multiple nodes with shared state (Redis/PostgreSQL)

## References

- **Claude Code Task Tool**: [Documentation](https://docs.anthropic.com/claude/docs)
- **Tokio Runtime**: [tokio.rs](https://tokio.rs)
- **Systemd**: [systemd.io](https://systemd.io)
- **Launchd**: [developer.apple.com](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html)

---

**Version**: 1.0.0
**Last Updated**: 2026-02-08
**License**: MIT
