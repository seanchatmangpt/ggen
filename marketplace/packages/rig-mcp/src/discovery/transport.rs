//! Transport client implementations for MCP server communication
//!
//! Provides multiple transport protocols for communicating with MCP servers:
//! - **StdioTransport**: For local process-based MCP servers
//! - **SseTransport**: For Server-Sent Events (SSE) based MCP servers
//! - **HttpTransport**: For direct HTTP-based MCP servers

use super::{DiscoveryError, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;
use thiserror::Error;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};
use tokio::sync::Mutex;
use tokio::time::timeout;

/// Errors that can occur during transport operations
#[derive(Debug, Error)]
pub enum TransportError {
    /// Connection failed
    #[error("connection failed: {0}")]
    ConnectionFailed(String),

    /// Timeout during operation
    #[error("timeout after {0}s")]
    Timeout(u64),

    /// Invalid JSON-RPC response
    #[error("invalid json-rpc response: {0}")]
    InvalidJsonRpc(String),

    /// Server error response
    #[error("server error: {code} - {message}")]
    ServerError { code: i64, message: String },

    /// Process spawn failed (stdio transport)
    #[error("failed to spawn process: {0}")]
    ProcessSpawnFailed(String),

    /// Process exited unexpectedly
    #[error("process exited: {0}")]
    ProcessExited(String),

    /// HTTP request failed
    #[error("http request failed: {0}")]
    HttpError(String),

    /// IO error
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    /// JSON parsing error
    #[error("json error: {0}")]
    JsonError(#[from] serde_json::Error),
}

/// JSON-RPC 2.0 request identifier
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(untagged)]
pub enum RequestId {
    Num(i64),
    Str(String),
}

/// JSON-RPC 2.0 request
#[derive(Debug, Clone, Serialize)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: RequestId,
    pub method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<JsonValue>,
}

impl JsonRpcRequest {
    /// Create a new JSON-RPC request
    pub fn new(id: RequestId, method: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            method: method.into(),
            params: None,
        }
    }

    /// Add parameters to the request
    pub fn with_params(mut self, params: JsonValue) -> Self {
        self.params = Some(params);
        self
    }
}

/// JSON-RPC 2.0 response
#[derive(Debug, Clone, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub id: RequestId,
    #[serde(default)]
    pub result: Option<JsonValue>,
    #[serde(default)]
    pub error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error
#[derive(Debug, Clone, Deserialize)]
pub struct JsonRpcError {
    pub code: i64,
    pub message: String,
    #[serde(default)]
    pub data: Option<JsonValue>,
}

/// Transport request wrapper
#[derive(Debug, Clone)]
pub struct TransportRequest {
    /// JSON-RPC method to call
    pub method: String,
    /// Parameters for the method
    pub params: Option<JsonValue>,
    /// Request timeout in seconds
    pub timeout_secs: u64,
}

impl TransportRequest {
    /// Create a new transport request
    pub fn new(method: impl Into<String>) -> Self {
        Self {
            method: method.into(),
            params: None,
            timeout_secs: 30,
        }
    }

    /// Add parameters to the request
    pub fn with_params(mut self, params: JsonValue) -> Self {
        self.params = Some(params);
        self
    }

    /// Set the timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }
}

/// Transport response wrapper
#[derive(Debug, Clone)]
pub struct TransportResponse {
    /// Result data from the response
    pub result: JsonValue,
    /// Optional error if the request failed
    pub error: Option<String>,
    /// Response time in milliseconds
    pub duration_ms: u64,
}

/// Supported transport types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransportType {
    /// Standard input/output for local processes
    Stdio,
    /// Server-Sent Events for streaming responses
    Sse,
    /// Direct HTTP requests
    Http,
}

/// Configuration for transport clients
#[derive(Debug, Clone)]
pub struct TransportConfig {
    /// Request timeout in seconds
    pub timeout_secs: u64,
    /// Maximum number of retries
    pub max_retries: usize,
    /// Delay between retries in milliseconds
    pub retry_delay_ms: u64,
    /// Additional headers for HTTP/SSE transports
    pub headers: HashMap<String, String>,
}

impl Default for TransportConfig {
    fn default() -> Self {
        Self {
            timeout_secs: 30,
            max_retries: 3,
            retry_delay_ms: 1000,
            headers: HashMap::default(),
        }
    }
}

/// Trait for MCP transport clients
#[async_trait::async_trait]
pub trait TransportClient: Send + Sync {
    /// Send a request to the MCP server
    async fn send(&self, request: TransportRequest) -> Result<TransportResponse>;

    /// Get the transport type
    fn transport_type(&self) -> TransportType;

    /// Check if the connection is alive
    async fn is_alive(&self) -> bool;

    /// Get the server endpoint
    fn endpoint(&self) -> String;
}

/// Stdio transport for local MCP server processes
pub struct StdioTransport {
    /// Command to spawn the server process
    command: Vec<String>,
    /// Working directory for the process
    working_dir: Option<PathBuf>,
    /// Child process handle
    child: Mutex<Option<ChildProcess>>,
    /// Transport configuration
    config: TransportConfig,
    /// Server endpoint identifier
    endpoint: String,
}

/// Child process wrapper for stdio communication
struct ChildProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    next_request_id: i64,
}

impl ChildProcess {
    /// Create a new child process wrapper
    fn new(mut child: tokio::process::Child) -> std::result::Result<Self, std::io::Error> {
        let stdin = child.stdin.take().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::BrokenPipe, "Failed to get stdin")
        })?;
        let stdout = child.stdout.take().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::BrokenPipe, "Failed to get stdout")
        })?;
        Ok(Self {
            child,
            stdin,
            stdout: BufReader::new(stdout),
            next_request_id: 1,
        })
    }

    /// Send a JSON-RPC request
    async fn send_request(
        &mut self, request: &JsonRpcRequest,
    ) -> std::result::Result<JsonRpcResponse, TransportError> {
        // Serialize the request
        let request_json = serde_json::to_string(request).map_err(TransportError::JsonError)?;

        // Send the request
        self.stdin.write_all(request_json.as_bytes()).await?;
        self.stdin.write_all(b"\n").await?;
        self.stdin.flush().await?;

        // Read the response
        let mut response_line = String::new();
        self.stdout.read_line(&mut response_line).await?;

        if response_line.is_empty() {
            return Err(TransportError::ProcessExited(
                "Empty response from process".to_string(),
            ));
        }

        // Parse the response
        let response: JsonRpcResponse = serde_json::from_str(&response_line)
            .map_err(|e| TransportError::InvalidJsonRpc(format!("Parse error: {}", e)))?;

        Ok(response)
    }

    /// Get the next request ID
    fn next_id(&mut self) -> RequestId {
        let id = self.next_request_id;
        self.next_request_id += 1;
        RequestId::Num(id)
    }
}

impl StdioTransport {
    /// Create a new stdio transport
    pub fn new(command: Vec<String>, endpoint: String) -> Self {
        Self {
            command,
            working_dir: None,
            child: Mutex::new(None),
            config: TransportConfig::default(),
            endpoint,
        }
    }

    /// Set the working directory
    pub fn with_working_dir(mut self, dir: PathBuf) -> Self {
        self.working_dir = Some(dir);
        self
    }

    /// Set the transport configuration
    pub fn with_config(mut self, config: TransportConfig) -> Self {
        self.config = config;
        self
    }

    /// Ensure the child process is running
    async fn ensure_process(&self) -> std::result::Result<(), TransportError> {
        let mut child_guard = self.child.lock().await;

        if child_guard.is_some() {
            // Process exists, assume it's still alive
            return Ok(());
        }

        // Spawn the process
        let (program, args) = self
            .command
            .split_first()
            .ok_or_else(|| TransportError::ProcessSpawnFailed("Empty command".to_string()))?;

        let mut cmd = Command::new(program);
        cmd.args(args);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        if let Some(dir) = &self.working_dir {
            cmd.current_dir(dir);
        }

        let child = cmd.spawn().map_err(|e| {
            TransportError::ProcessSpawnFailed(format!("Failed to spawn {}: {}", program, e))
        })?;

        *child_guard = Some(ChildProcess::new(child)?);

        Ok(())
    }
}

#[async_trait::async_trait]
impl TransportClient for StdioTransport {
    async fn send(&self, request: TransportRequest) -> Result<TransportResponse> {
        self.ensure_process().await?;

        let mut child_guard = self.child.lock().await;
        let process = child_guard.as_mut().ok_or_else(|| {
            TransportError::ConnectionFailed("No active child process".to_string())
        })?;

        let start = std::time::Instant::now();
        let rpc_request = JsonRpcRequest::new(process.next_id(), &request.method)
            .with_params(request.params.unwrap_or_default());

        let response = timeout(
            Duration::from_secs(request.timeout_secs),
            process.send_request(&rpc_request),
        )
        .await
        .map_err(|_| TransportError::Timeout(request.timeout_secs))?
        .map_err(|e| DiscoveryError::Transport(e))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        match (response.result, response.error) {
            (Some(result), None) => Ok(TransportResponse {
                result,
                error: None,
                duration_ms,
            }),
            (None, Some(err)) => Err(DiscoveryError::Transport(TransportError::ServerError {
                code: err.code,
                message: err.message,
            })),
            _ => Err(DiscoveryError::Transport(TransportError::InvalidJsonRpc(
                "Response has neither result nor error".to_string(),
            ))),
        }
    }

    fn transport_type(&self) -> TransportType {
        TransportType::Stdio
    }

    async fn is_alive(&self) -> bool {
        let child_guard = self.child.lock().await;
        child_guard.as_ref().is_some()
    }

    fn endpoint(&self) -> String {
        self.endpoint.clone()
    }
}

/// SSE transport for Server-Sent Events
pub struct SseTransport {
    /// SSE endpoint URL
    url: String,
    /// Transport configuration
    config: TransportConfig,
}

impl SseTransport {
    /// Create a new SSE transport
    pub fn new(url: String) -> Self {
        Self {
            url,
            config: TransportConfig::default(),
        }
    }

    /// Set the transport configuration
    pub fn with_config(mut self, config: TransportConfig) -> Self {
        self.config = config;
        self
    }
}

#[async_trait::async_trait]
impl TransportClient for SseTransport {
    async fn send(&self, request: TransportRequest) -> Result<TransportResponse> {
        let start = std::time::Instant::now();

        // Build the HTTP client
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(request.timeout_secs))
            .build()
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        // Prepare the JSON-RPC request
        let rpc_request = JsonRpcRequest::new(
            RequestId::Str(uuid::Uuid::new_v4().to_string()),
            &request.method,
        )
        .with_params(request.params.unwrap_or_default());

        // Send the request
        let response = client
            .post(&self.url)
            .header("Content-Type", "application/json")
            .header("Accept", "text/event-stream")
            .json(&rpc_request)
            .send()
            .await
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        if !response.status().is_success() {
            return Err(DiscoveryError::Transport(TransportError::HttpError(
                format!("HTTP {}", response.status()),
            )));
        }

        // Parse SSE response
        let text = response
            .text()
            .await
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        // Parse SSE data (format: "data: <json>\n\n")
        let json_str = text
            .strip_prefix("data: ")
            .and_then(|s| s.strip_suffix("\n\n"))
            .or_else(|| text.strip_prefix("data:"))
            .ok_or_else(|| {
                DiscoveryError::Transport(TransportError::InvalidJsonRpc(
                    "Invalid SSE format".to_string(),
                ))
            })?;

        let rpc_response: JsonRpcResponse =
            serde_json::from_str(json_str).map_err(TransportError::JsonError)?;

        let duration_ms = start.elapsed().as_millis() as u64;

        match (rpc_response.result, rpc_response.error) {
            (Some(result), None) => Ok(TransportResponse {
                result,
                error: None,
                duration_ms,
            }),
            (None, Some(err)) => Err(DiscoveryError::Transport(TransportError::ServerError {
                code: err.code,
                message: err.message,
            })),
            _ => Err(DiscoveryError::Transport(TransportError::InvalidJsonRpc(
                "Response has neither result nor error".to_string(),
            ))),
        }
    }

    fn transport_type(&self) -> TransportType {
        TransportType::Sse
    }

    async fn is_alive(&self) -> bool {
        let client = match reqwest::Client::builder()
            .timeout(Duration::from_secs(5))
            .build()
        {
            Ok(c) => c,
            Err(_) => return false,
        };

        client
            .head(&self.url)
            .send()
            .await
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }

    fn endpoint(&self) -> String {
        self.url.clone()
    }
}

/// HTTP transport for direct HTTP requests
pub struct HttpTransport {
    /// HTTP endpoint URL
    url: String,
    /// Transport configuration
    config: TransportConfig,
}

impl HttpTransport {
    /// Create a new HTTP transport
    pub fn new(url: String) -> Self {
        Self {
            url,
            config: TransportConfig::default(),
        }
    }

    /// Set the transport configuration
    pub fn with_config(mut self, config: TransportConfig) -> Self {
        self.config = config;
        self
    }
}

#[async_trait::async_trait]
impl TransportClient for HttpTransport {
    async fn send(&self, request: TransportRequest) -> Result<TransportResponse> {
        let start = std::time::Instant::now();

        // Build the HTTP client
        let mut client_builder =
            reqwest::Client::builder().timeout(Duration::from_secs(request.timeout_secs));

        // Add retries if configured
        if self.config.max_retries > 0 {
            // Note: reqwest doesn't have built-in retry, would need retry-async
        }

        let client = client_builder
            .build()
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        // Prepare the JSON-RPC request
        let rpc_request = JsonRpcRequest::new(
            RequestId::Str(uuid::Uuid::new_v4().to_string()),
            &request.method,
        )
        .with_params(request.params.unwrap_or_default());

        // Build the request with headers
        let mut http_request = client
            .post(&self.url)
            .header("Content-Type", "application/json");

        for (key, value) in &self.config.headers {
            http_request = http_request.header(key, value);
        }

        // Send the request
        let response = http_request
            .json(&rpc_request)
            .send()
            .await
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        if !response.status().is_success() {
            return Err(DiscoveryError::Transport(TransportError::HttpError(
                format!("HTTP {}", response.status()),
            )));
        }

        let rpc_response: JsonRpcResponse = response
            .json()
            .await
            .map_err(|e| DiscoveryError::Transport(TransportError::HttpError(e.to_string())))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        match (rpc_response.result, rpc_response.error) {
            (Some(result), None) => Ok(TransportResponse {
                result,
                error: None,
                duration_ms,
            }),
            (None, Some(err)) => Err(DiscoveryError::Transport(TransportError::ServerError {
                code: err.code,
                message: err.message,
            })),
            _ => Err(DiscoveryError::Transport(TransportError::InvalidJsonRpc(
                "Response has neither result nor error".to_string(),
            ))),
        }
    }

    fn transport_type(&self) -> TransportType {
        TransportType::Http
    }

    async fn is_alive(&self) -> bool {
        let client = match reqwest::Client::builder()
            .timeout(Duration::from_secs(5))
            .build()
        {
            Ok(c) => c,
            Err(_) => return false,
        };

        client
            .head(&self.url)
            .send()
            .await
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }

    fn endpoint(&self) -> String {
        self.url.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_rpc_request_serialization() {
        let request = JsonRpcRequest::new(RequestId::Num(1), "test_method")
            .with_params(serde_json::json!({"key": "value"}));

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains(r#""method":"test_method""#));
        assert!(json.contains(r#""params""#));
        assert!(json.contains(r#""id":1"#));
    }

    #[test]
    fn test_transport_config_default() {
        let config = TransportConfig::default();
        assert_eq!(config.timeout_secs, 30);
        assert_eq!(config.max_retries, 3);
        assert_eq!(config.retry_delay_ms, 1000);
    }

    #[test]
    fn test_transport_request_builder() {
        let request = TransportRequest::new("test")
            .with_params(serde_json::json!({"key": "value"}))
            .with_timeout(60);

        assert_eq!(request.method, "test");
        assert!(request.params.is_some());
        assert_eq!(request.timeout_secs, 60);
    }

    #[test]
    fn test_request_id_serialization() {
        let num_id = RequestId::Num(42);
        let json = serde_json::to_string(&num_id).unwrap();
        assert_eq!(json, "42");

        let str_id = RequestId::Str("test-id".to_string());
        let json = serde_json::to_string(&str_id).unwrap();
        assert_eq!(json, r#""test-id""#);
    }
}
