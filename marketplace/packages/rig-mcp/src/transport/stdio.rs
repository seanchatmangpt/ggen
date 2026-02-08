//! Stdio Transport for MCP
//!
//! Implements the stdio transport layer for communicating with local MCP servers.
//! This is the standard transport used by Claude Desktop and other local MCP integrations.

use crate::transport::{
    error::{TransportError, TransportResult},
    JsonRpcRequest, JsonRpcResponse, McpTransport, RequestId, TransportState,
};
use async_trait::async_trait;
use std::{collections::HashMap, process::Stdio, sync::Arc};
use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    process::{Child as TokioChild, Command as TokioCommand},
    sync::{
        mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender},
        Mutex,
    },
    task::JoinHandle,
};
use tokio_util::sync::CancellationToken;

/// Configuration for stdio transport
#[derive(Debug, Clone)]
pub struct StdioConfig {
    /// Command to execute
    pub command: String,
    /// Arguments for the command
    pub args: Vec<String>,
    /// Environment variables to set
    pub env: HashMap<String, String>,
    /// Timeout for requests in seconds
    pub timeout_secs: u64,
}

impl StdioConfig {
    /// Create a new stdio config
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            command: command.into(),
            args: Vec::new(),
            env: HashMap::new(),
            timeout_secs: 30,
        }
    }

    /// Add an argument
    pub fn arg(mut self, arg: impl Into<String>) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Add arguments
    pub fn args(mut self, args: impl IntoIterator<Item = impl Into<String>>) -> Self {
        for arg in args {
            self.args.push(arg.into());
        }
        self
    }

    /// Add an environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Set the timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }
}

/// Internal state for the stdio transport
struct StdioState {
    /// Child process handle
    child: Option<TokioChild>,
    /// Write handle for stdin
    stdin: Option<tokio::process::ChildStdin>,
    /// Read handle for stdout
    stdout: Option<tokio::process::ChildStdout>,
    /// Read handle for stderr
    stderr: Option<tokio::process::ChildStderr>,
    /// Current transport state
    state: TransportState,
    /// Pending request callbacks
    pending: HashMap<String, UnboundedSender<JsonRpcResponse>>,
    /// Cancellation token for background tasks
    cancellation_token: CancellationToken,
    /// Background task handles
    tasks: Vec<JoinHandle<()>>,
    /// Request counter for generating IDs
    request_counter: u64,
}

impl StdioState {
    /// Create a new state
    fn new() -> Self {
        Self {
            child: None,
            stdin: None,
            stdout: None,
            stderr: None,
            state: TransportState::Disconnected,
            pending: HashMap::new(),
            cancellation_token: CancellationToken::new(),
            tasks: Vec::new(),
            request_counter: 0,
        }
    }

    /// Generate next request ID
    fn next_id(&mut self) -> RequestId {
        self.request_counter = self.request_counter.wrapping_add(1);
        RequestId::new_number(self.request_counter)
    }

    /// Cancel all background tasks
    async fn cancel_tasks(&mut self) {
        self.cancellation_token.cancel();
        for task in self.tasks.drain(..) {
            let _ = task.await;
        }
        self.cancellation_token = CancellationToken::new();
    }
}

/// Stdio transport for MCP servers
///
/// Communicates with local MCP servers via stdin/stdout. This is the primary
/// transport mechanism for Claude Desktop and other local integrations.
pub struct StdioTransport {
    /// Configuration
    config: StdioConfig,
    /// Internal state
    state: Arc<Mutex<StdioState>>,
    /// Notification channel sender
    notification_tx: UnboundedSender<JsonRpcRequest>,
    /// Notification channel receiver
    notification_rx: Arc<Mutex<Option<UnboundedReceiver<JsonRpcRequest>>>>,
}

impl StdioTransport {
    /// Create a new stdio transport
    pub fn new(config: StdioConfig) -> Self {
        let (notification_tx, notification_rx) = unbounded_channel();

        Self {
            config,
            state: Arc::new(Mutex::new(StdioState::new())),
            notification_tx,
            notification_rx: Arc::new(Mutex::new(Some(notification_rx))),
        }
    }

    /// Create from a command string
    ///
    /// Parses the command string and creates a transport.
    /// Supports: `"command arg1 arg2"` format.
    pub fn from_command(command: impl AsRef<str>) -> TransportResult<Self> {
        let parts = shell_words::split(command.as_ref())
            .map_err(|e| TransportError::InvalidEndpoint(format!("Invalid command: {}", e)))?;

        if parts.is_empty() {
            return Err(TransportError::InvalidEndpoint("Empty command".to_string()));
        }

        let mut config = StdioConfig::new(&parts[0]);
        if parts.len() > 1 {
            config = config.args(parts[1..].iter());
        }

        Ok(Self::new(config))
    }

    /// Connect to the MCP server
    pub async fn connect(&self) -> TransportResult<()> {
        let mut state = self.state.lock().await;

        // Disconnect if already connected
        if state.state == TransportState::Connected {
            self.disconnect_internal(&mut state).await?;
        }

        state.state = TransportState::Connecting;

        // Build the command
        let mut cmd = TokioCommand::new(&self.config.command);
        cmd.args(&self.config.args);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        // Set environment variables
        for (key, value) in &self.config.env {
            cmd.env(key, value);
        }

        // Spawn the process
        let mut child = cmd.spawn().map_err(|e| {
            TransportError::ConnectionFailed(format!("Failed to spawn process: {}", e))
        })?;

        // Get the stdin/stdout handles
        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| TransportError::ConnectionFailed("Missing stdin handle".to_string()))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| TransportError::ConnectionFailed("Missing stdout handle".to_string()))?;
        let stderr = child.stderr.take();

        // Setup background tasks
        let cancellation_token = state.cancellation_token.clone();
        let state_arc = self.state.clone();
        let notification_tx = self.notification_tx.clone();

        // Reader task
        let reader_handle = {
            let cancellation_token = cancellation_token.clone();
            // Clone Arc references for the reader task
            let state_arc_clone = Arc::clone(&state_arc);
            tokio::spawn(async move {
                Self::reader_task(
                    stdout,
                    stderr,
                    state_arc_clone,
                    notification_tx,
                    cancellation_token,
                )
                .await;
            })
        };

        state.tasks.push(reader_handle);
        state.child = Some(child);
        state.stdin = Some(stdin);
        // Note: stdout and stderr are moved into the reader task
        state.stdout = None;
        state.stderr = None;

        Ok(())
    }

    /// Disconnect from the server
    async fn disconnect_internal(&self, state: &mut StdioState) -> TransportResult<()> {
        state.state = TransportState::ShuttingDown;
        state.cancel_tasks().await;

        // Close stdin to signal EOF to the process
        if let Some(mut stdin) = state.stdin.take() {
            let _ = stdin.shutdown().await;
        }

        // Drop stdout/stderr
        state.stdout = None;
        state.stderr = None;

        // Kill the child process
        if let Some(mut child) = state.child.take() {
            let _ = child.kill().await;
            let _ = child.wait().await;
        }

        // Clear pending requests
        for (_, tx) in state.pending.drain() {
            let _ = tx.send(JsonRpcResponse::error(
                RequestId::Null,
                crate::transport::JsonRpcError::new(-32103, "Transport disconnected"),
            ));
        }

        state.state = TransportState::Disconnected;
        Ok(())
    }

    /// Background task for reading from stdout
    async fn reader_task(
        stdout: tokio::process::ChildStdout, stderr: Option<tokio::process::ChildStderr>,
        state: Arc<Mutex<StdioState>>, notification_tx: UnboundedSender<JsonRpcRequest>,
        cancellation_token: CancellationToken,
    ) {
        let mut reader = BufReader::new(stdout).lines();
        let mut stderr_reader = stderr.map(BufReader::new);

        loop {
            tokio::select! {
                _ = cancellation_token.cancelled() => {
                    break;
                }
                result = reader.next_line() => {
                    match result {
                        Ok(Some(line)) => {
                            if line.is_empty() {
                                continue;
                            }

                            // Parse the JSON-RPC message
                            match Self::parse_message(&line) {
                                Ok(Message::Response(response)) => {
                                    // Handle response
                                    let mut state = state.lock().await;
                                    if let Some(id) = response.id.as_ref() {
                                        let id_str = match id {
                                            RequestId::String(s) => s.clone(),
                                            RequestId::Number(n) => n.to_string(),
                                            RequestId::Null => continue,
                                        };
                                        if let Some(tx) = state.pending.remove(&id_str) {
                                            let _ = tx.send(response);
                                        }
                                    }
                                }
                                Ok(Message::Request(request)) => {
                                    // Handle notification (server request without ID)
                                    if request.id.is_none() || request.id.as_ref().map(|id| id.is_notification()).unwrap_or(false) {
                                        let _ = notification_tx.send(request);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("Failed to parse MCP message: {}", e);
                                }
                            }
                        }
                        Ok(None) => {
                            // EOF - connection closed
                            break;
                        }
                        Err(e) => {
                            eprintln!("Error reading from MCP server: {}", e);
                            break;
                        }
                    }
                }
                result = async {
                    if let Some(ref mut reader) = stderr_reader {
                        reader.lines().next_line().await
                    } else {
                        std::future::pending::<()>().await;
                        Ok(None)
                    }
                } => {
                    if let Ok(Some(line)) = result {
                        eprintln!("MCP stderr: {}", line);
                    }
                }
            }
        }
    }

    /// Parse a JSON-RPC message
    fn parse_message(line: &str) -> TransportResult<Message> {
        // Try parsing as a response first (has result or error)
        if let Ok(response) = serde_json::from_str::<JsonRpcResponse>(line) {
            return Ok(Message::Response(response));
        }

        // Try parsing as a request/notification
        let request = serde_json::from_str::<JsonRpcRequest>(line)?;
        Ok(Message::Request(request))
    }

    /// Send raw data to stdin
    async fn send_raw(&self, data: &[u8]) -> TransportResult<()> {
        // Check connected state first
        {
            let state = self.state.lock().await;
            if state.state != TransportState::Connected {
                return Err(TransportError::NotConnected);
            }
            if state.stdin.is_none() {
                return Err(TransportError::NotConnected);
            }
        }

        // Get mutable stdin for writing
        let mut state = self.state.lock().await;
        let stdin = state
            .stdin
            .as_mut()
            .ok_or_else(|| TransportError::NotConnected)?;

        stdin
            .write_all(data)
            .await
            .map_err(|e| TransportError::Io(e))?;
        stdin
            .write_all(b"\n")
            .await
            .map_err(|e| TransportError::Io(e))?;
        stdin.flush().await.map_err(|e| TransportError::Io(e))?;

        Ok(())
    }
}

#[async_trait]
impl McpTransport for StdioTransport {
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        // Ensure we're connected
        {
            let state = self.state.lock().await;
            if state.state != TransportState::Connected {
                // Auto-connect
                drop(state);
                self.connect().await?;
            }
        }

        // Get or generate the request ID
        let id = match request.id.clone() {
            Some(id) => id,
            None => {
                let mut state = self.state.lock().await;
                state.next_id()
            }
        };

        let id_str = match &id {
            RequestId::String(s) => s.clone(),
            RequestId::Number(n) => n.to_string(),
            RequestId::Null => {
                return Err(TransportError::InvalidParams(
                    "Request ID cannot be null".to_string(),
                ))
            }
        };

        // Create channel for response
        let (tx, mut rx) = unbounded_channel();

        // Register the pending request
        {
            let mut state = self.state.lock().await;
            state.pending.insert(id_str.clone(), tx);
        }

        // Serialize and send the request
        let request_with_id = JsonRpcRequest::with_id(request.method, request.params, id);
        let json = request_with_id.to_json()?;
        self.send_raw(json.as_bytes()).await?;

        // Wait for response with timeout
        let timeout = tokio::time::Duration::from_secs(self.config.timeout_secs);
        let response = tokio::time::timeout(timeout, rx.recv())
            .await
            .map_err(|_| TransportError::Timeout)?
            .ok_or_else(|| TransportError::Timeout)?;

        // Verify ID matches
        if let Some(response_id) = response.id.as_ref() {
            let response_id_str = match response_id {
                RequestId::String(s) => s.clone(),
                RequestId::Number(n) => n.to_string(),
                RequestId::Null => String::new(),
            };
            if response_id_str != id_str {
                return Err(TransportError::IdMismatch {
                    expected: id_str,
                    actual: response_id_str,
                });
            }
        }

        Ok(response)
    }

    async fn send_notification(&self, notification: JsonRpcRequest) -> TransportResult<()> {
        // Ensure we're connected
        {
            let state = self.state.lock().await;
            if state.state != TransportState::Connected {
                drop(state);
                self.connect().await?;
            }
        }

        // Serialize and send
        let json = notification.to_json()?;
        self.send_raw(json.as_bytes()).await?;
        Ok(())
    }

    fn is_connected(&self) -> bool {
        // Use a blocking check in a sync context
        // For async context, callers should use connect() which checks
        true // Optimistic: assume connected until proven otherwise
    }

    async fn close(&self) -> TransportResult<()> {
        let mut state = self.state.lock().await;
        self.disconnect_internal(&mut state).await
    }
}

impl Drop for StdioTransport {
    fn drop(&mut self) {
        let state = self.state.clone();
        tokio::spawn(async move {
            let mut state = state.lock().await;
            if state.state == TransportState::Connected {
                // Best-effort shutdown
                state.cancellation_token.cancel();
                if let Some(mut child) = state.child.take() {
                    let _ = child.kill().await;
                }
            }
        });
    }
}

/// Message type from server (either response or request/notification)
enum Message {
    Response(JsonRpcResponse),
    Request(JsonRpcRequest),
}

/// Builder for creating stdio transports
pub struct StdioTransportBuilder {
    config: StdioConfig,
}

impl StdioTransportBuilder {
    /// Create a new builder
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            config: StdioConfig::new(command),
        }
    }

    /// Add an argument
    pub fn arg(mut self, arg: impl Into<String>) -> Self {
        self.config = self.config.arg(arg);
        self
    }

    /// Add multiple arguments
    pub fn args(mut self, args: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.config = self.config.args(args);
        self
    }

    /// Add an environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config = self.config.env(key, value);
        self
    }

    /// Set timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config = self.config.timeout(secs);
        self
    }

    /// Build the transport
    pub fn build(self) -> StdioTransport {
        StdioTransport::new(self.config)
    }

    /// Build and connect
    pub async fn connect(self) -> TransportResult<StdioTransport> {
        let transport = self.build();
        transport.connect().await?;
        Ok(transport)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_builder() {
        let config = StdioConfig::new("test")
            .arg("--verbose")
            .args(["--config", "test.json"])
            .env("TEST", "value")
            .timeout(60);

        assert_eq!(config.command, "test");
        assert_eq!(config.args, vec!["--verbose", "--config", "test.json"]);
        assert_eq!(config.env.get("TEST"), Some(&"value".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_transport_builder() {
        let transport = StdioTransportBuilder::new("mcp-server")
            .arg("--port")
            .arg("8080")
            .build();

        assert_eq!(transport.config.command, "mcp-server");
        assert_eq!(transport.config.args, vec!["--port", "8080"]);
    }

    #[test]
    fn test_from_command() {
        let transport = StdioTransport::from_command("mcp-server --arg1 --arg2").unwrap();
        assert_eq!(transport.config.command, "mcp-server");
        assert_eq!(transport.config.args, vec!["--arg1", "--arg2"]);
    }

    #[test]
    fn test_from_command_invalid() {
        let result = StdioTransport::from_command("'");
        assert!(result.is_err());
    }

    #[test]
    fn test_message_parsing() {
        let request_json = r#"{"jsonrpc":"2.0","method":"test","params":{},"id":"test-id"}"#;
        let message = StdioTransport::parse_message(request_json).unwrap();
        assert!(matches!(message, Message::Request(_)));

        let response_json = r#"{"jsonrpc":"2.0","result":{},"id":"test-id"}"#;
        let message = StdioTransport::parse_message(response_json).unwrap();
        assert!(matches!(message, Message::Response(_)));
    }
}
