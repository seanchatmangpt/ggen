use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpListener;

/// Health status levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Status {
    Healthy,
    Degraded,
    Critical,
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Status::Healthy => write!(f, "HEALTHY"),
            Status::Degraded => write!(f, "DEGRADED"),
            Status::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// Component health status
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ComponentHealth {
    children: u32,
    restarted: u32,
    status: String,
}

/// Circuit breaker state
#[derive(Debug, Clone, Serialize, Deserialize)]
struct CircuitBreakerHealth {
    state: String,
    failures: u32,
    threshold: u32,
}

/// Consensus state
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConsensusHealth {
    nodes: u32,
    quorum: u32,
    reachable: u32,
}

/// Byzantine Fault Tolerance state
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ByzantineHealth {
    byzantine_nodes: u32,
    quorum_status: String,
    violations: u32,
}

/// Storage state
#[derive(Debug, Clone, Serialize, Deserialize)]
struct StorageHealth {
    events: u64,
    last_gc: String,
}

/// All component statuses
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Components {
    supervisor: ComponentHealth,
    circuit_breaker: CircuitBreakerHealth,
    consensus: ConsensusHealth,
    byzantine: ByzantineHealth,
    storage: StorageHealth,
}

/// Metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Metrics {
    uptime_seconds: u64,
    total_restarts: u32,
    circuit_breaks: u32,
    consensus_failures: u32,
}

/// Complete health dashboard response
#[derive(Debug, Clone, Serialize, Deserialize)]
struct HealthDashboard {
    status: String,
    components: Components,
    metrics: Metrics,
}

/// System state holder
struct SystemState {
    start_time: SystemTime,
    total_restarts: u32,
    circuit_breaks: u32,
    consensus_failures: u32,
}

impl SystemState {
    fn new() -> Self {
        Self {
            start_time: SystemTime::now(),
            total_restarts: 5,
            circuit_breaks: 2,
            consensus_failures: 0,
        }
    }

    fn uptime_seconds(&self) -> u64 {
        self.start_time
            .elapsed()
            .unwrap_or(Duration::ZERO)
            .as_secs()
    }

    fn health_status(&self) -> Status {
        if self.consensus_failures > 0 {
            Status::Critical
        } else if self.circuit_breaks > 1 || self.total_restarts > 3 {
            Status::Degraded
        } else {
            Status::Healthy
        }
    }

    fn to_dashboard(&self) -> HealthDashboard {
        let status = self.health_status();

        HealthDashboard {
            status: status.to_string().to_lowercase(),
            components: Components {
                supervisor: ComponentHealth {
                    children: 5,
                    restarted: 3,
                    status: "running".to_string(),
                },
                circuit_breaker: CircuitBreakerHealth {
                    state: "closed".to_string(),
                    failures: 2,
                    threshold: 5,
                },
                consensus: ConsensusHealth {
                    nodes: 3,
                    quorum: 3,
                    reachable: 3,
                },
                byzantine: ByzantineHealth {
                    byzantine_nodes: 0,
                    quorum_status: if self.consensus_failures > 0 {
                        "degraded".to_string()
                    } else {
                        "healthy".to_string()
                    },
                    violations: self.consensus_failures,
                },
                storage: StorageHealth {
                    events: 1523,
                    last_gc: "2m ago".to_string(),
                },
            },
            metrics: Metrics {
                uptime_seconds: self.uptime_seconds(),
                total_restarts: self.total_restarts,
                circuit_breaks: self.circuit_breaks,
                consensus_failures: self.consensus_failures,
            },
        }
    }
}

/// Format duration as human-readable string
fn format_duration(secs: u64) -> String {
    let hours = secs / 3600;
    let minutes = (secs % 3600) / 60;
    if hours > 0 {
        format!("{}h {}m", hours, minutes)
    } else {
        format!("{}m", minutes)
    }
}

/// Get terminal status indicator
fn status_indicator(status: Status) -> &'static str {
    match status {
        Status::Healthy => "✅",
        Status::Degraded => "⚠️ ",
        Status::Critical => "🔴",
    }
}

/// Render text-based dashboard for terminal
fn render_text_dashboard(dashboard: &HealthDashboard, status: Status) -> String {
    let uptime = format_duration(dashboard.metrics.uptime_seconds);

    format!(
        r#"OSIRIS System Health
═══════════════════════════════════
Status: {}

Components:
  Supervisor........ {} {} children, {} restarts
  Circuit Breaker... {} Closed ({}/{} failures)
  Consensus......... {} {}/{} nodes reachable
  Byzantine......... {} {} quorum, {} violations
  Storage........... {} {} events

Uptime: {} | Restarts: {} | Circuit Breaks: {}
"#,
        status,
        status_indicator(status),
        dashboard.components.supervisor.children,
        dashboard.components.supervisor.restarted,
        status_indicator(status),
        dashboard.components.circuit_breaker.failures,
        dashboard.components.circuit_breaker.threshold,
        status_indicator(status),
        dashboard.components.consensus.reachable,
        dashboard.components.consensus.nodes,
        status_indicator(status),
        dashboard.components.byzantine.quorum_status,
        dashboard.components.byzantine.violations,
        status_indicator(status),
        dashboard.components.storage.events,
        uptime,
        dashboard.metrics.total_restarts,
        dashboard.metrics.circuit_breaks,
    )
}

/// HTTP request handler
async fn handle_http_request(
    socket: tokio::net::TcpStream, state: Arc<Mutex<SystemState>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (reader, mut writer) = socket.into_split();
    let mut buf_reader = BufReader::new(reader);
    let mut line = String::new();

    buf_reader.read_line(&mut line).await?;

    if !line.contains("GET") {
        writer
            .write_all(b"HTTP/1.1 405 Method Not Allowed\r\n\r\n")
            .await?;
        return Ok(());
    }

    let dashboard = {
        let system_state = state.lock().unwrap();
        system_state.to_dashboard()
    };

    let json = serde_json::to_string(&dashboard)?;
    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}",
        json.len(),
        json
    );

    writer.write_all(response.as_bytes()).await?;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let state = Arc::new(Mutex::new(SystemState::new()));

    // Check for --text flag for terminal output
    let text_mode = std::env::args().any(|arg| arg == "--text");

    if text_mode {
        let system_state = state.lock().unwrap();
        let dashboard = system_state.to_dashboard();
        let status = system_state.health_status();
        println!("{}", render_text_dashboard(&dashboard, status));
        return Ok(());
    }

    // Start HTTP server on :8080
    let listener = TcpListener::bind("127.0.0.1:8080").await?;
    println!("Health dashboard listening on http://127.0.0.1:8080");

    loop {
        let (socket, _) = listener.accept().await?;
        let state_clone = Arc::clone(&state);

        tokio::spawn(async move {
            let _ = handle_http_request(socket, state_clone).await;
        });
    }
}
