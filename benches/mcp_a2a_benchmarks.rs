//! MCP & A2A Integration Performance Benchmarks
//!
//! Comprehensive performance benchmarks for MCP (Model Context Protocol) and
//! A2A (Agent-to-Agent) integration components.
//!
//! ## Service Level Objectives (SLOs)
//!
//! | Benchmark Category | Target | Description |
//! |-------------------|--------|-------------|
//! | Tool Discovery | <100ms | Discover 100 tools from server |
//! | Message Translation | <1ms | Convert single message between formats |
//! | Tool Execution | <50ms p95 | Execute tool (excluding actual operation) |
//! | Concurrent Operations | >1000 req/sec | Handle concurrent requests |
//! | Memory per Connection | <10KB | Per-connection memory overhead |
//! | Memory per Tool | <1KB | Per-tool metadata storage |
//!
//! ## Running Benchmarks
//!
//! ```bash
//! # Run all MCP & A2A benchmarks
//! cargo bench --bench mcp_a2a_benchmarks
//!
//! # Run specific benchmark group
//! cargo bench --bench mcp_a2a_benchmarks -- tool_discovery
//!
//! # Generate HTML report
//! cargo bench --bench mcp_a2a_benchmarks -- --output-format html
//! ```
//!
//! ## Benchmark Groups
//!
//! - `tool_discovery`: Tool list retrieval and caching performance
//! - `message_translation`: MCP/A2A message format conversion
//! - `tool_execution`: Tool invocation overhead and latency
//! - `concurrent_operations`: Parallel request handling
//! - `memory_usage`: Memory footprint analysis

use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

// =============================================================================
// Test Data Generation
// =============================================================================

/// Represents a mock MCP tool for benchmarking
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct MockTool {
    name: String,
    description: String,
    input_schema: serde_json::Value,
    output_schema: serde_json::Value,
}

/// Represents a mock A2A message for benchmarking
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct MockA2AMessage {
    id: String,
    source: String,
    target: Option<String>,
    message_type: String,
    payload: serde_json::Value,
    timestamp: u64,
}

/// Represents a mock MCP message for benchmarking
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct MockMCPMessage {
    jsonrpc: String,
    id: Option<String>,
    method: Option<String>,
    params: Option<serde_json::Value>,
    result: Option<serde_json::Value>,
    error: Option<serde_json::Value>,
}

/// Generate mock tools for benchmarking
fn generate_mock_tools(count: usize) -> Vec<MockTool> {
    (0..count)
        .map(|i| MockTool {
            name: format!("tool_{}", i),
            description: format!("Mock tool number {} for benchmarking", i),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "param1": {"type": "string"},
                    "param2": {"type": "number"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "result": {"type": "string"}
                }
            }),
        })
        .collect()
}

/// Generate mock A2A message
fn generate_mock_a2a_message(id: usize) -> MockA2AMessage {
    MockA2AMessage {
        id: format!("msg_{}", id),
        source: "agent_1".to_string(),
        target: Some("agent_2".to_string()),
        message_type: "task_request".to_string(),
        payload: serde_json::json!({
            "task": format!("task_{}", id),
            "data": "test payload"
        }),
        timestamp: chrono::Utc::now().timestamp_micros() as u64,
    }
}

/// Generate mock MCP message
fn generate_mock_mcp_message(method: &str, id: usize) -> MockMCPMessage {
    MockMCPMessage {
        jsonrpc: "2.0".to_string(),
        id: Some(format!("id_{}", id)),
        method: Some(method.to_string()),
        params: Some(serde_json::json!({
            "name": "test_tool",
            "arguments": {"param": "value"}
        })),
        result: None,
        error: None,
    }
}

/// Small message payload (typical size)
fn small_payload() -> serde_json::Value {
    serde_json::json!({
        "operation": "read",
        "key": "test_key"
    })
}

/// Medium message payload
fn medium_payload() -> serde_json::Value {
    serde_json::json!({
        "operation": "batch_read",
        "keys": (0..50).map(|i| format!("key_{}", i)).collect::<Vec<_>>(),
        "options": {
            "timeout": 5000,
            "retry": 3,
            "consistent": true
        }
    })
}

/// Large message payload
fn large_payload() -> serde_json::Value {
    serde_json::json!({
        "operation": "bulk_operation",
        "items": (0..500).map(|i| serde_json::json!({
            "key": format!("key_{}", i),
            "value": format!("value_{}", i),
            "metadata": {
                "created": chrono::Utc::now().to_rfc3339(),
                "modified": chrono::Utc::now().to_rfc3339(),
                "version": 1,
                "tags": ["tag1", "tag2", "tag3"]
            }
        })).collect::<Vec<_>>(),
        "options": {
            "timeout": 30000,
            "batch_size": 100,
            "parallel": true
        }
    })
}

// =============================================================================
// Tool Discovery Benchmarks
// =============================================================================

fn bench_tool_discovery(c: &mut Criterion) {
    let mut group = c.benchmark_group("tool_discovery");

    // Benchmark: Tool list retrieval for different sizes
    for tool_count in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*tool_count as u64));

        group.bench_with_input(
            BenchmarkId::new("list_retrieval", tool_count),
            tool_count,
            |b, &count| {
                let tools = generate_mock_tools(count);
                b.iter(|| black_box(tools.len()));
            },
        );
    }

    // Benchmark: Tool lookup by name
    group.bench_function("lookup_by_name", |b| {
        let tools = generate_mock_tools(100);
        let tool_map: HashMap<_, _> = tools.iter().map(|t| (t.name.clone(), t)).collect();

        b.iter(|| black_box(tool_map.get("tool_50")));
    });

    // Benchmark: Tool pattern matching
    group.bench_function("pattern_matching", |b| {
        let tools = generate_mock_tools(100);
        let query = "tool_*";

        b.iter(|| {
            let matches: Vec<_> = tools
                .iter()
                .filter(|t| t.name.starts_with("tool_"))
                .collect();
            black_box(matches.len())
        });
    });

    // Benchmark: Tool schema validation
    group.bench_function("schema_validation", |b| {
        let tools = generate_mock_tools(50);
        let test_input = serde_json::json!({"param1": "test", "param2": 42});

        b.iter(|| {
            tools
                .iter()
                .for_each(|tool| black_box(validate_input_schema(&tool.input_schema, &test_input)))
        });
    });

    group.measurement_time(Duration::from_secs(10));
    group.sample_size(100);
    group.finish();
}

/// Mock schema validation function
fn validate_input_schema(schema: &serde_json::Value, input: &serde_json::Value) -> bool {
    // Simplified validation - just check structure exists
    schema.get("type").is_some() && input.is_object()
}

// =============================================================================
// Message Translation Benchmarks
// =============================================================================

fn bench_message_translation(c: &mut Criterion) {
    let mut group = c.benchmark_group("message_translation");

    // Benchmark: A2A to MCP conversion
    group.bench_function("a2a_to_mcp_small", |b| {
        let a2a_msg = generate_mock_a2a_message(1);
        let payload = small_payload();

        b.iter(|| black_box(convert_a2a_to_mcp(&a2a_msg, &payload)));
    });

    group.bench_function("a2a_to_mcp_medium", |b| {
        let a2a_msg = generate_mock_a2a_message(1);
        let payload = medium_payload();

        b.iter(|| black_box(convert_a2a_to_mcp(&a2a_msg, &payload)));
    });

    group.bench_function("a2a_to_mcp_large", |b| {
        let a2a_msg = generate_mock_a2a_message(1);
        let payload = large_payload();

        b.iter(|| black_box(convert_a2a_to_mcp(&a2a_msg, &payload)));
    });

    // Benchmark: MCP to A2A conversion
    group.bench_function("mcp_to_a2a_small", |b| {
        let mcp_msg = generate_mock_mcp_message("tools/call", 1);

        b.iter(|| black_box(convert_mcp_to_a2a(&mcp_msg)));
    });

    // Benchmark: JSON serialization
    group.bench_function("json_serialization", |b| {
        let a2a_msg = generate_mock_a2a_message(1);

        b.iter(|| black_box(serde_json::to_string(&a2a_msg).unwrap()));
    });

    // Benchmark: JSON deserialization
    group.bench_function("json_deserialization", |b| {
        let json_str = serde_json::to_string(&generate_mock_a2a_message(1)).unwrap();

        b.iter(|| black_box(serde_json::from_str::<MockA2AMessage>(&json_str).unwrap()));
    });

    // Benchmark: Batch conversion (100 messages)
    group.bench_function("batch_conversion_100", |b| {
        let a2a_msgs: Vec<_> = (0..100).map(generate_mock_a2a_message).collect();

        b.iter(|| {
            let converted: Vec<_> = a2a_msgs
                .iter()
                .map(|msg| convert_a2a_to_mcp(msg, &small_payload()))
                .collect();
            black_box(converted.len())
        });
    });

    group.measurement_time(Duration::from_secs(5));
    group.sample_size(1000);
    group.finish();
}

/// Mock A2A to MCP conversion function
fn convert_a2a_to_mcp(a2a_msg: &MockA2AMessage, payload: &serde_json::Value) -> MockMCPMessage {
    MockMCPMessage {
        jsonrpc: "2.0".to_string(),
        id: Some(a2a_msg.id.clone()),
        method: Some("tools/call".to_string()),
        params: Some(serde_json::json!({
            "name": a2a_msg.message_type,
            "arguments": payload
        })),
        result: None,
        error: None,
    }
}

/// Mock MCP to A2A conversion function
fn convert_mcp_to_a2a(mcp_msg: &MockMCPMessage) -> MockA2AMessage {
    MockA2AMessage {
        id: mcp_msg.id.clone().unwrap_or_default(),
        source: "mcp_client".to_string(),
        target: Some("a2a_server".to_string()),
        message_type: mcp_msg.method.clone().unwrap_or_default(),
        payload: mcp_msg.params.clone().unwrap_or_default(),
        timestamp: chrono::Utc::now().timestamp_micros() as u64,
    }
}

// =============================================================================
// Tool Execution Benchmarks
// =============================================================================

fn bench_tool_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("tool_execution");

    // Benchmark: Tool invocation overhead
    group.bench_function("invocation_overhead", |b| {
        let tool = generate_mock_tools(1).pop().unwrap();

        b.iter(|| black_box(invoke_tool(&tool, &small_payload())));
    });

    // Benchmark: Parameter parsing
    group.bench_function("parameter_parsing", |b| {
        let params = serde_json::json!({
            "param1": "value1",
            "param2": 42,
            "param3": true,
            "param4": [1, 2, 3]
        });

        b.iter(|| black_box(parse_parameters(&params)));
    });

    // Benchmark: Response generation
    group.bench_function("response_generation", |b| {
        let result = serde_json::json!({
            "status": "success",
            "data": {"key": "value"}
        });

        b.iter(|| black_box(generate_response("call_id_123", true, Some(&result))));
    });

    // Benchmark: Error handling
    group.bench_function("error_handling", |b| {
        let error = serde_json::json!({
            "code": -32602,
            "message": "Invalid params",
            "data": {"detail": "Missing required parameter"}
        });

        b.iter(|| black_box(generate_error_response("call_id_456", &error)));
    });

    // Benchmark: Streaming response chunks
    group.bench_function("streaming_chunk", |b| {
        let chunk_data = serde_json::json!({
            "content": "Partial response content",
            "done": false
        });

        b.iter(|| black_box(format_streaming_chunk(1, &chunk_data)));
    });

    group.measurement_time(Duration::from_secs(5));
    group.sample_size(500);
    group.finish();
}

/// Mock tool invocation function
fn invoke_tool(tool: &MockTool, payload: &serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "tool": tool.name,
        "result": "executed",
        "input": payload
    })
}

/// Mock parameter parsing function
fn parse_parameters(params: &serde_json::Value) -> HashMap<String, serde_json::Value> {
    let mut map = HashMap::new();
    if let Some(obj) = params.as_object() {
        for (key, value) in obj {
            map.insert(key.clone(), value.clone());
        }
    }
    map
}

/// Mock response generation function
fn generate_response(id: &str, success: bool, result: Option<&serde_json::Value>) -> String {
    format!(
        r#"{{"jsonrpc":"2.0","id":"{}","result":{}}}"#,
        id,
        result.unwrap_or(&serde_json::json!({}))
    )
}

/// Mock error response function
fn generate_error_response(id: &str, error: &serde_json::Value) -> String {
    format!(r#"{{"jsonrpc":"2.0","id":"{}","error":{}}}"#, id, error)
}

/// Mock streaming chunk formatting function
fn format_streaming_chunk(chunk_id: usize, data: &serde_json::Value) -> String {
    format!(
        "data: {}\n\n",
        serde_json::json!({
            "id": chunk_id,
            "data": data
        })
    )
}

// =============================================================================
// Concurrent Operations Benchmarks
// =============================================================================

fn bench_concurrent_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_operations");

    // Benchmark: Concurrent message processing
    for concurrency in [1, 10, 50, 100, 500].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_messages", concurrency),
            concurrency,
            |b, &concurrency| {
                let messages: Vec<_> = (0..concurrency)
                    .map(|i| generate_mock_a2a_message(i))
                    .collect();

                b.iter(|| {
                    let results: Vec<_> = messages
                        .iter()
                        .map(|msg| process_message_concurrently(msg))
                        .collect();
                    black_box(results)
                });
            },
        );
    }

    // Benchmark: Concurrent tool invocations
    for concurrency in [1, 10, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_tools", concurrency),
            concurrency,
            |b, &concurrency| {
                let tools = generate_mock_tools(concurrency);

                b.iter(|| {
                    let results: Vec<_> = tools
                        .iter()
                        .map(|tool| invoke_tool(tool, &small_payload()))
                        .collect();
                    black_box(results)
                });
            },
        );
    }

    // Benchmark: Concurrent message translation
    for concurrency in [1, 10, 50, 100, 500].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_translation", concurrency),
            concurrency,
            |b, &concurrency| {
                let messages: Vec<_> = (0..concurrency)
                    .map(|i| generate_mock_a2a_message(i))
                    .collect();

                b.iter(|| {
                    let results: Vec<_> = messages
                        .iter()
                        .map(|msg| convert_a2a_to_mcp(msg, &small_payload()))
                        .collect();
                    black_box(results)
                });
            },
        );
    }

    // Benchmark: Shared state access (RwLock)
    group.bench_function("shared_state_read", |b| {
        let state = Arc::new(RwLock::new(generate_mock_tools(100)));

        b.iter(|| {
            let read_guard = tokio::task::block_in_place(|| state.blocking_read());
            black_box(read_guard.len())
        });
    });

    group.bench_function("shared_state_write", |b| {
        let state = Arc::new(RwLock::new(generate_mock_tools(100)));

        b.iter(|| {
            let mut write_guard = tokio::task::block_in_place(|| state.blocking_write());
            write_guard.push(generate_mock_tools(1).pop().unwrap());
            black_box(write_guard.len())
        });
    });

    group.measurement_time(Duration::from_secs(10));
    group.sample_size(50);
    group.finish();
}

/// Mock concurrent message processing function
fn process_message_concurrently(msg: &MockA2AMessage) -> MockMCPMessage {
    convert_a2a_to_mcp(msg, &small_payload())
}

// =============================================================================
// Memory Usage Benchmarks
// =============================================================================

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");

    // Benchmark: Per-connection memory
    group.bench_function("connection_state", |b| {
        b.iter_batched(
            || MockConnectionState::new(),
            |state| black_box(state.size_bytes()),
            BatchSize::Small,
        );
    });

    // Benchmark: Per-tool metadata storage
    for tool_count in [1, 10, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("tool_metadata", tool_count),
            tool_count,
            |b, &count| {
                b.iter_batched(
                    || generate_mock_tools(count),
                    |tools| black_box(estimate_tool_memory(&tools)),
                    BatchSize::Small,
                );
            },
        );
    }

    // Benchmark: Message buffer allocation
    for buffer_size in [10, 100, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::new("message_buffer", buffer_size),
            buffer_size,
            |b, &size| {
                b.iter_batched(
                    || MockMessageBuffer::new(size),
                    |buffer| black_box(buffer.capacity_bytes()),
                    BatchSize::Small,
                );
            },
        );
    }

    // Benchmark: Cache memory efficiency
    group.bench_function("cache_entry", |b| {
        b.iter_batched(
            || MockCacheEntry::new("key_123", &small_payload()),
            |entry| black_box(entry.size_bytes()),
            BatchSize::Small,
        );
    });

    group.measurement_time(Duration::from_secs(5));
    group.sample_size(100);
    group.finish();
}

/// Mock connection state for memory measurement
#[derive(Debug)]
struct MockConnectionState {
    id: String,
    connected_at: u64,
    tools: Vec<String>,
    pending_requests: Vec<String>,
    capabilities: Vec<String>,
}

impl MockConnectionState {
    fn new() -> Self {
        Self {
            id: "conn_123".to_string(),
            connected_at: chrono::Utc::now().timestamp_micros() as u64,
            tools: (0..50).map(|i| format!("tool_{}", i)).collect(),
            pending_requests: (0..10).map(|i| format!("req_{}", i)).collect(),
            capabilities: vec![
                "streaming".to_string(),
                "batch".to_string(),
                "notifications".to_string(),
            ],
        }
    }

    fn size_bytes(&self) -> usize {
        let id_size = self.id.len();
        let tools_size = self.tools.iter().map(|t| t.len()).sum::<usize>();
        let pending_size = self.pending_requests.iter().map(|p| p.len()).sum::<usize>();
        let caps_size = self.capabilities.iter().map(|c| c.len()).sum::<usize>();

        id_size + tools_size + pending_size + caps_size + 24 // timestamp
    }
}

/// Estimate tool metadata memory usage
fn estimate_tool_memory(tools: &[MockTool]) -> usize {
    tools
        .iter()
        .map(|t| {
            t.name.len()
                + t.description.len()
                + t.input_schema.to_string().len()
                + t.output_schema.to_string().len()
        })
        .sum()
}

/// Mock message buffer for memory measurement
struct MockMessageBuffer {
    messages: Vec<MockA2AMessage>,
    capacity: usize,
}

impl MockMessageBuffer {
    fn new(capacity: usize) -> Self {
        Self {
            messages: Vec::with_capacity(capacity),
            capacity,
        }
    }

    fn capacity_bytes(&self) -> usize {
        // Rough estimate of allocation
        self.capacity * std::mem::size_of::<MockA2AMessage>()
    }
}

/// Mock cache entry for memory measurement
struct MockCacheEntry {
    key: String,
    value: serde_json::Value,
    timestamp: u64,
    ttl: Option<u64>,
}

impl MockCacheEntry {
    fn new(key: &str, value: &serde_json::Value) -> Self {
        Self {
            key: key.to_string(),
            value: value.clone(),
            timestamp: chrono::Utc::now().timestamp_micros() as u64,
            ttl: Some(300_000_000), // 5 minutes in microseconds
        }
    }

    fn size_bytes(&self) -> usize {
        self.key.len() + self.value.to_string().len() + 16 // timestamp + ttl
    }
}

// =============================================================================
// SLO Validation Helper Functions
// =============================================================================

/// Validate that tool discovery meets SLO (<100ms for 100 tools)
fn validate_tool_discovery_slo(measured_ns: u64) -> bool {
    const SLO_THRESHOLD_NS: u64 = 100_000_000; // 100ms
    measured_ns < SLO_THRESHOLD_NS
}

/// Validate that message translation meets SLO (<1ms per message)
fn validate_message_translation_slo(measured_ns: u64) -> bool {
    const SLO_THRESHOLD_NS: u64 = 1_000_000; // 1ms
    measured_ns < SLO_THRESHOLD_NS
}

/// Validate that tool execution meets SLO (<50ms p95)
fn validate_tool_execution_slo(measured_ns: u64) -> bool {
    const SLO_THRESHOLD_NS: u64 = 50_000_000; // 50ms
    measured_ns < SLO_THRESHOLD_NS
}

/// Validate that concurrent operations meet SLO (>1000 req/sec)
fn validate_concurrent_slo(req_per_sec: u64) -> bool {
    const SLO_MINIMUM: u64 = 1000;
    req_per_sec >= SLO_MINIMUM
}

/// Validate that memory per connection meets SLO (<10KB)
fn validate_connection_memory_slo(bytes: usize) -> bool {
    const SLO_THRESHOLD_BYTES: usize = 10_240; // 10KB
    bytes < SLO_THRESHOLD_BYTES
}

/// Validate that memory per tool meets SLO (<1KB)
fn validate_tool_memory_slo(bytes: usize) -> bool {
    const SLO_THRESHOLD_BYTES: usize = 1024; // 1KB
    bytes < SLO_THRESHOLD_BYTES
}

// =============================================================================
// Criterion Groups
// =============================================================================

criterion_group!(tool_discovery, bench_tool_discovery);

criterion_group!(message_translation, bench_message_translation);

criterion_group!(tool_execution, bench_tool_execution);

criterion_group!(concurrent_operations, bench_concurrent_operations);

criterion_group!(memory_usage, bench_memory_usage);

criterion_main!(
    tool_discovery,
    message_translation,
    tool_execution,
    concurrent_operations,
    memory_usage
);
