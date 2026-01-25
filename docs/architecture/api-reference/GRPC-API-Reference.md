# gRPC API Reference

Complete API documentation for TAI gRPC services.

## Service Summary

| Service | Port | Methods | Purpose |
|---------|------|---------|---------|
| Governor | 50051 | 5 | Policy management and governance |
| Coordinator | 50052 | 5 | Signal and action coordination |
| Scheduler | 50053 | 4 | Task scheduling and execution |

## Governor Service

Manages system-wide governance and policies.

### ProposePolicy
**Request:** Policy
**Response:** Receipt

Propose a new policy for system adoption.

```protobuf
message Policy {
  string id = 1;
  string policy_type = 2;      // "finance", "security", "operational"
  string policy_name = 3;
  int64 version = 4;
  map<string, string> rules = 5;
  bool enabled = 6;
  int64 created_at_ns = 7;
  int64 updated_at_ns = 8;
}

message Receipt {
  string id = 1;
  string request_id = 2;
  string status = 3;           // "success", "failure", "partial"
  int64 timestamp_ns = 4;
  map<string, string> result_data = 5;
  string error_message = 6;
  int64 execution_time_us = 7;
}
```

**Error Codes:**
- `INVALID_ARGUMENT`: Policy validation failed
- `PERMISSION_DENIED`: Insufficient authorization
- `RESOURCE_EXHAUSTED`: Rate limit exceeded
- `INTERNAL`: Internal server error

**Example (Rust):**
```rust
let mut client = GovernorClient::connect("http://governor:50051").await?;

let policy = Policy {
    id: "pol-001".to_string(),
    policy_type: "security".to_string(),
    policy_name: "Enforce TLS 1.3".to_string(),
    version: 1,
    rules: [("tls_version".into(), "1.3".into())].iter().cloned().collect(),
    enabled: false,
    created_at_ns: SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos() as i64,
    updated_at_ns: SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos() as i64,
};

let response = client.propose_policy(tonic::Request::new(policy)).await?;
let receipt = response.into_inner();
println!("Policy proposed: {}", receipt.id);
```

### EnforcePolicy
**Request:** Policy
**Response:** Receipt

Enforce a policy across the system immediately.

**Authorization:** Admin role required

### RevokePolicy
**Request:** Policy
**Response:** Receipt

Revoke an existing policy.

**Authorization:** Admin role required

### GetPolicies
**Request:** GetPoliciesRequest
**Response:** GetPoliciesResponse

Retrieve list of policies with optional filtering.

```protobuf
message GetPoliciesRequest {
  string policy_type = 1;    // Optional filter
  bool enabled_only = 2;
}

message GetPoliciesResponse {
  repeated Policy policies = 1;
}
```

### StreamPolicies
**Request:** StreamPoliciesRequest
**Response:** stream Policy

Stream real-time policy updates (server-push).

```protobuf
message StreamPoliciesRequest {
  string policy_type = 1;    // Optional filter
}
```

Client subscribes and receives Policy messages whenever policies change.

### HealthCheck
**Request:** HealthCheckRequest
**Response:** HealthStatus

Check service health status.

```protobuf
message HealthStatus {
  string service_name = 1;
  string status = 2;        // "healthy", "degraded", "unhealthy"
  int64 timestamp_ns = 3;
  repeated string details = 4;
  int64 uptime_seconds = 5;
}
```

## Coordinator Service

Coordinates actions and signals across services.

### SubmitSignal
**Request:** Signal
**Response:** Receipt

Submit a control signal for coordination.

```protobuf
message Signal {
  string id = 1;
  string signal_type = 2;    // "alert", "event", "command"
  int64 timestamp_ns = 3;
  map<string, string> metadata = 4;
  bytes payload = 5;
  int32 priority = 6;        // 1-10, higher = more urgent
}
```

### RequestAction
**Request:** Action
**Response:** Receipt

Request an action to be performed by a service.

```protobuf
message Action {
  string id = 1;
  string action_type = 2;    // "create", "update", "delete", "scale"
  int64 timestamp_ns = 3;
  map<string, string> parameters = 4;
  bytes payload = 5;
  string source_service = 6;
}
```

### StreamActions
**Request:** StreamActionsRequest
**Response:** stream Action

Stream actions targeted at requesting service (server-push).

```protobuf
message StreamActionsRequest {
  string service_name = 1;
  bool include_history = 2;  // Include unprocessed actions from last 1 hour
}
```

### AcknowledgeAction
**Request:** AcknowledgeActionRequest
**Response:** Receipt

Acknowledge receipt and completion of an action.

```protobuf
message AcknowledgeActionRequest {
  string action_id = 1;
  string status = 2;         // "accepted", "rejected", "failed"
  string message = 3;        // Optional details
}
```

### GetStatus
**Request:** GetStatusRequest
**Response:** CoordinationStatus

Get current coordination status for a service.

```protobuf
message CoordinationStatus {
  string service_name = 1;
  int64 timestamp_ns = 2;
  int32 pending_signals = 3;
  int32 pending_actions = 4;
  int64 last_signal_ns = 5;
}
```

## Scheduler Service

Manages task scheduling and execution.

### SubmitTask
**Request:** ScheduleTaskRequest
**Response:** Receipt

Submit a new task for scheduling.

```protobuf
message ScheduleTaskRequest {
  string task_id = 1;
  string task_type = 2;              // "policy_update", "signal_process"
  map<string, string> parameters = 3;
  int64 scheduled_time_ns = 4;       // Unix timestamp in nanoseconds
  int32 priority = 5;                // 1-10
}
```

### CancelTask
**Request:** CancelTaskRequest
**Response:** Receipt

Cancel a scheduled task.

```protobuf
message CancelTaskRequest {
  string task_id = 1;
  string reason = 2;
}
```

### GetTaskStatus
**Request:** GetTaskStatusRequest
**Response:** TaskStatus

Get current status of a task.

```protobuf
message TaskStatus {
  string task_id = 1;
  string status = 2;         // "pending", "running", "completed", "failed", "cancelled"
  int64 created_at_ns = 3;
  int64 started_at_ns = 4;
  int64 completed_at_ns = 5;
  string error_message = 6;
  map<string, string> result = 7;
}
```

### StreamTaskUpdates
**Request:** StreamTaskUpdatesRequest
**Response:** stream TaskUpdate

Stream real-time updates for a specific task.

```protobuf
message TaskUpdate {
  string task_id = 1;
  string status = 2;
  int64 timestamp_ns = 3;
  string message = 4;        // Status details
}
```

## Common Error Handling

### gRPC Status Codes

| Code | Meaning | Action |
|------|---------|--------|
| `OK` (0) | Success | Process normally |
| `CANCELLED` (1) | Operation cancelled | Retry or abort |
| `UNKNOWN` (2) | Unknown error | Retry with backoff |
| `INVALID_ARGUMENT` (3) | Input validation failed | Fix input and retry |
| `DEADLINE_EXCEEDED` (4) | Operation timed out | Retry (may have executed) |
| `NOT_FOUND` (5) | Resource not found | Resource may be deleted |
| `ALREADY_EXISTS` (6) | Resource already exists | Operation may have succeeded |
| `PERMISSION_DENIED` (7) | Authorization failed | Check credentials/RBAC |
| `RESOURCE_EXHAUSTED` (8) | Rate limit or quota exceeded | Exponential backoff |
| `FAILED_PRECONDITION` (9) | Precondition check failed | Check state dependencies |
| `ABORTED` (10) | Transaction aborted | Retry full operation |
| `INTERNAL` (13) | Server error | Retry with backoff |
| `UNAVAILABLE` (14) | Service unavailable | Retry with longer backoff |
| `UNAUTHENTICATED` (16) | No authentication provided | Add authentication |

### Retry Strategy

```rust
pub async fn call_with_retries<T, F>(
    mut f: F,
    max_retries: u32,
) -> Result<T>
where
    F: std::ops::FnMut() -> BoxFuture<'static, Result<T>>,
{
    for attempt in 0..=max_retries {
        match f().await {
            Ok(result) => return Ok(result),
            Err(status) => {
                // Determine if retryable
                let retryable = matches!(
                    status.code(),
                    Code::Unavailable | Code::ResourceExhausted | Code::Unknown
                );

                if !retryable || attempt == max_retries {
                    return Err(status);
                }

                // Exponential backoff: 2^attempt seconds
                let backoff = Duration::from_secs(2u64.pow(attempt));
                tokio::time::sleep(backoff).await;
            }
        }
    }
    unreachable!()
}
```

## Rate Limiting Headers

All responses include rate limiting metadata:

```
grpc-metadata-x-ratelimit-limit: 100
grpc-metadata-x-ratelimit-remaining: 42
grpc-metadata-x-ratelimit-reset: 1642000000
```

## mTLS Certificate Requirements

Service-to-service calls require mTLS:

```
Certificate CN: service-name.tai-system.svc.cluster.local
Subject Alt Name (SAN): spiffe://cluster.local/ns/tai-system/sa/service-name
```

## Performance Characteristics

| Operation | Latency (p50) | Latency (p99) | Throughput |
|-----------|---------------|---------------|-----------|
| ProposePolicy | 20ms | 100ms | 1k/sec |
| GetPolicies | 15ms | 80ms | 5k/sec |
| StreamPolicies | 5ms | 50ms | N/A |
| SubmitSignal | 10ms | 50ms | 10k/sec |
| RequestAction | 15ms | 80ms | 5k/sec |
| SubmitTask | 12ms | 70ms | 2k/sec |

## Testing

Use `grpcurl` for testing:

```bash
# List services
grpcurl -plaintext localhost:50051 list

# Call method
grpcurl -plaintext \
  -d '{"policy_type":"security","enabled_only":true}' \
  localhost:50051 tai.Governor.GetPolicies

# Stream method
grpcurl -plaintext \
  -d '{"policy_type":"finance"}' \
  localhost:50051 tai.Governor.StreamPolicies
```

## References
- tai.proto: crates/tai-grpc/proto/tai.proto
- Tonic Framework: https://github.com/hyperium/tonic
- gRPC Documentation: https://grpc.io/docs
