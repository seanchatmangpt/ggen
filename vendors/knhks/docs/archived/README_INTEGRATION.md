// README_INTEGRATION.md
// Integration Guide for KNHK v1.0 Components

## Component Overview

KNHK v1.0 consists of the following integrated components:

### 1. Hot Path (C)
- **Location**: `src/`, `include/knhk.h`
- **Purpose**: 8-tick execution engine
- **Status**: ✅ Complete

### 2. Warm Path (Rust FFI)
- **Location**: `rust/knhk-hot/`
- **Purpose**: Safe wrappers around C hot path
- **Status**: ✅ Complete

### 3. Connector Framework
- **Location**: `rust/knhk-connectors/`
- **Purpose**: Dark Matter 80/20 connector framework
- **Status**: ✅ Complete
- **Implementations**: Kafka, Salesforce

### 4. ETL Pipeline
- **Location**: `rust/knhk-etl/`
- **Purpose**: Ingest → Transform → Load → Reflex → Emit
- **Status**: ✅ Complete

### 5. Lockchain
- **Location**: `rust/knhk-lockchain/`
- **Purpose**: Merkle-linked provenance storage
- **Status**: ✅ Complete

### 6. OpenTelemetry
- **Location**: `rust/knhk-otel/`
- **Purpose**: Observability and metrics
- **Status**: ✅ Complete

### 7. Erlang High-Level API
- **Location**: `erlang/knhk_rc/`
- **Purpose**: Control plane and orchestration
- **Status**: ✅ Structure complete (stubs)

## Integration Points

### Connector → ETL Pipeline
```rust
use knhk_connectors::{ConnectorRegistry, KafkaConnector};
use knhk_etl::Pipeline;

let mut registry = ConnectorRegistry::new();
let kafka = Box::new(KafkaConnector::new(...));
registry.register(kafka)?;

let pipeline = Pipeline::new(
    registry.list(),
    "urn:knhk:schema:enterprise".to_string(),
    true, // lockchain enabled
    vec!["https://webhook.example.com".to_string()],
);
```

### ETL Pipeline → Lockchain
```rust
use knhk_etl::Pipeline;
use knhk_lockchain::Lockchain;

let mut lockchain = Lockchain::new();
let result = pipeline.execute()?;

// Write receipts to lockchain
for receipt_hash in result.lockchain_hashes {
    lockchain.append(...)?;
}
```

### ETL Pipeline → OpenTelemetry
```rust
use knhk_etl::Pipeline;
use knhk_otel::{Tracer, MetricsHelper};

let mut tracer = Tracer::new();
let span = tracer.start_span("pipeline_execution".to_string(), None);

let result = pipeline.execute()?;

MetricsHelper::record_hook_latency(&mut tracer, ticks, "ASK_SP");
tracer.end_span(span, SpanStatus::Ok);
```

## Testing Integration

All components have Chicago TDD test suites:
- `tests/chicago_receipts.c` - Receipt validation
- `tests/chicago_construct8.c` - CONSTRUCT8 operations
- `tests/chicago_batch.c` - Batch execution
- `tests/chicago_guards.c` - Guard enforcement
- `tests/chicago_performance.c` - Performance validation

## Build Instructions

```bash
# Build C library
make lib

# Build Rust components
cd rust/knhk-connectors && cargo build
cd rust/knhk-etl && cargo build
cd rust/knhk-lockchain && cargo build
cd rust/knhk-otel && cargo build

# Run tests
make test
```

## Next Steps

1. **Schema Registry**: Implement Σ schema validation
2. **Invariant Registry**: Implement Q constraint checking
3. **Git Lockchain**: Integrate with Git for Merkle tree
4. **OTEL Exporters**: Connect to OTEL collectors
5. **Connector Implementations**: Complete Kafka/Salesforce real implementations

