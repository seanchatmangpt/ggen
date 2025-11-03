# P2P OpenTelemetry Instrumentation

## Overview

Comprehensive OpenTelemetry (OTEL) instrumentation has been added to all P2P operations in `ggen-marketplace/src/backend/p2p.rs`. This enables production debugging and monitoring of the P2P networking layer.

## Instrumented Operations

### 1. Bootstrap (`bootstrap()`)

**Span Name:** `bootstrap`

**Attributes:**
- `operation` = "bootstrap"
- `peer_id` - Local peer ID
- `bootstrap_node_count` - Number of bootstrap nodes configured
- `connection_time_ms` - Time taken to bootstrap (recorded at completion)
- `connected_peers` - Number of peers connected after bootstrap
- `error` - Error message if bootstrap fails

**Purpose:** Track P2P network initialization and bootstrap node connectivity.

### 2. Gossipsub Publish (`announce_package()`)

**Span Name:** `announce_package`

**Attributes:**
- `operation` = "gossipsub_publish"
- `package_id` - Package being announced
- `package_version` - Version being announced
- `announcement_size_bytes` - Serialized announcement size
- `latency_ms` - Time to publish announcement
- `error` - Error message if publish fails

**Purpose:** Monitor package announcement propagation through gossipsub mesh.

### 3. DHT Storage (`store_in_dht()`)

**Span Name:** `store_in_dht`

**Attributes:**
- `operation` = "dht_put"
- `package_id` - Package ID being stored
- `peer_id` - Local peer ID
- `record_size_bytes` - Size of DHT record
- `latency_ms` - Time to store record
- `error` - Error message if storage fails

**Purpose:** Track DHT write operations and storage latency.

### 4. DHT Query with Fan-Out (`query_dht_parallel()`)

**Span Name:** `query_dht_parallel`

**Attributes:**
- `operation` = "dht_query_parallel"
- `package_id` - Package ID being queried
- `fan_out` - Number of parallel queries
- `peers_queried` - Total peers queried
- `successful_responses` - Number of successful responses
- `latency_ms` - Total query time

**Child Spans:**
- `dht_peer_query` - One per peer queried
  - `peer_id` - Peer being queried
  - `query_latency_ms` - Response time from this peer

**Purpose:** Monitor parallel DHT lookups and peer response times.

### 5. Peer Reputation Updates (`record_peer_success()` / `record_peer_failure()`)

**Span Name:** `record_peer_success` / `record_peer_failure`

**Attributes:**
- `operation` = "peer_reputation_update"
- `peer_id` - Peer whose reputation is being updated
- `response_time_ms` - Response time (for success)
- `result` = "success" | "failure"
- `new_reputation_score` - Updated reputation score (0.0-1.0)

**Purpose:** Track peer reliability and performance metrics.

### 6. Search Operation (`search()`)

**Span Name:** `search` (from Registry trait)

**Attributes:**
- `operation` = "p2p_search"
- `query_text` - Search query text
- `query_limit` - Result limit
- `category_count` - Number of category filters
- `tag_count` - Number of tag filters
- `local_results` - Results from local cache
- `dht_results` - Results from DHT queries
- `total_results` - Total results returned
- `peer_count` - Peers available for queries
- `fan_out` - Fan-out strategy used
- `latency_ms` - Total search time

**Purpose:** Monitor end-to-end search performance across local and distributed sources.

### 7. Publish Operation (`publish()`)

**Span Name:** `publish`

**Attributes:**
- `operation` = "publish"
- `package_id` - Package being published
- `package_version` - Version being published
- `peer_id` - Local peer ID
- `dht_storage_success` - Boolean, DHT storage succeeded
- `gossipsub_success` - Boolean, gossipsub announce succeeded
- `latency_ms` - Total publish time
- `error` - Error message if operation fails

**Purpose:** Track complete package publication workflow.

### 8. Event Processing (`process_events()`)

**Span Name:** `process_events`

**Attributes:**
- `operation` = "process_events"
- `event_type` - Type of event processed
- `peer_id` - Peer involved in event (if applicable)

**Child Spans:**
- `behaviour_event` - For libp2p behavior events
  - `event_category` = "behaviour"

- `connection_established` - For new connections
  - `peer_id` - Connected peer
  - `num_established` - Connection count
  - `is_dialer` - Boolean, initiated by us

- `connection_closed` - For closed connections
  - `peer_id` - Disconnected peer
  - `num_established` - Remaining connections
  - `cause` - Reason for closure

- `incoming_connection_error` - For connection failures
  - `error` - Error description

**Purpose:** Monitor P2P network events and connection health.

## Span Hierarchy

```
publish
├── store_in_dht
│   └── [DHT write operation]
└── announce_package
    └── [Gossipsub publish]

search
├── [Local search - no span]
└── query_dht_parallel
    ├── dht_peer_query (peer 1)
    ├── dht_peer_query (peer 2)
    └── dht_peer_query (peer 3)

process_events
├── behaviour_event
├── connection_established
├── connection_closed
└── incoming_connection_error
```

## Usage Examples

### Viewing Traces in Production

When traces are exported to OTLP collector (e.g., Jaeger, Tempo):

1. **Search Performance Analysis:**
   ```
   Filter: operation="p2p_search"
   View: latency_ms, local_results, dht_results, peer_count
   ```

2. **DHT Query Fan-Out:**
   ```
   Filter: operation="dht_query_parallel"
   View children: dht_peer_query spans with query_latency_ms
   ```

3. **Peer Reliability:**
   ```
   Filter: operation="peer_reputation_update"
   Aggregate: new_reputation_score by peer_id
   ```

4. **Publish Success Rate:**
   ```
   Filter: operation="publish"
   View: dht_storage_success, gossipsub_success
   ```

### Example Trace Output

```json
{
  "trace_id": "abc123",
  "span_id": "def456",
  "name": "publish",
  "attributes": {
    "operation": "publish",
    "package_id": "serde",
    "package_version": "1.0.0",
    "peer_id": "12D3KooW...",
    "dht_storage_success": true,
    "gossipsub_success": true,
    "latency_ms": 245
  },
  "children": [
    {
      "name": "store_in_dht",
      "attributes": {
        "operation": "dht_put",
        "record_size_bytes": 2048,
        "latency_ms": 120
      }
    },
    {
      "name": "announce_package",
      "attributes": {
        "operation": "gossipsub_publish",
        "announcement_size_bytes": 2048,
        "latency_ms": 125
      }
    }
  ]
}
```

## Validation Testing

Comprehensive tests are available in `/tests/p2p_otel_instrumentation_test.rs`:

- `test_bootstrap_instrumentation` - Verifies bootstrap span attributes
- `test_publish_instrumentation` - Validates publish operation tracing
- `test_search_instrumentation` - Checks search span completeness
- `test_process_events_instrumentation` - Tests event processing spans
- `test_peer_reputation_instrumentation` - Validates reputation tracking
- `test_span_hierarchy` - Verifies parent-child span relationships
- `test_latency_tracking` - Confirms latency measurements

## Production Benefits

1. **Performance Debugging:**
   - Identify slow DHT queries
   - Track peer response times
   - Analyze search latency sources

2. **Reliability Monitoring:**
   - Publish success rates
   - Peer connection stability
   - Bootstrap reliability

3. **Network Health:**
   - Peer reputation trends
   - Connection churn rates
   - Event processing patterns

4. **Capacity Planning:**
   - Query fan-out effectiveness
   - DHT storage patterns
   - Gossipsub propagation times

## Integration with Telemetry Stack

The instrumentation uses the `tracing` crate which can export to:

- **Jaeger** - Via OTLP exporter
- **Tempo** - Via OTLP exporter
- **Honeycomb** - Via OTLP exporter
- **AWS X-Ray** - Via AWS distro for OpenTelemetry
- **Google Cloud Trace** - Via OTLP exporter

Configuration example (in application startup):
```rust
use opentelemetry::trace::TracerProvider;
use opentelemetry_otlp::WithExportConfig;

let tracer = opentelemetry_otlp::new_pipeline()
    .tracing()
    .with_exporter(
        opentelemetry_otlp::new_exporter()
            .tonic()
            .with_endpoint("http://localhost:4317")
    )
    .install_batch(opentelemetry::runtime::Tokio)?;

let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);
tracing_subscriber::registry().with(telemetry).init();
```

## Version

This instrumentation was added in version 2.4.0 of ggen-marketplace.
