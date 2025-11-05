# KNHKS CLI Guide

**Version**: 0.4.0  
**Principle**: 80/20 - Essential commands that provide 80% of value

## Overview

The KNHKS CLI provides a noun-verb interface based on the CONVO.txt API specification. All commands follow the pattern:

```bash
knhks <noun> <verb> [arguments]
```

## Installation

```bash
cd rust/knhks-cli
cargo build --release
cargo install --path .
```

## Commands

### Boot - System Initialization

**Initialize Σ and Q**
```bash
knhks boot init <sigma.ttl> <q.sparql>
```

Example:
```bash
knhks boot init schema.ttl invariants.sparql
```

### Connect - Connector Management

**Register Connector**
```bash
knhks connect register <name> <schema> <source>
```

**List Connectors**
```bash
knhks connect list
```

Example:
```bash
knhks connect register kafka-prod urn:knhks:schema:default kafka://localhost:9092/triples
knhks connect list
```

### Cover - Cover Definition

**Define Cover**
```bash
knhks cover define <select> <shard>
```

**List Covers**
```bash
knhks cover list
```

Example:
```bash
knhks cover define "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" "max_run_len 8"
```

### Admit - Delta Admission

**Admit Delta**
```bash
knhks admit delta <delta_file>
```

Example:
```bash
knhks admit delta delta.json
```

### Reflex - Reflex Declaration

**Declare Reflex**
```bash
knhks reflex declare <name> <op> <pred> <off> <len>
```

**List Reflexes**
```bash
knhks reflex list
```

Example:
```bash
knhks reflex declare check-count ASK_SP 0xC0FFEE 0 8
```

Valid operations (H_hot set):
- ASK_SP, COUNT_SP_GE, COUNT_SP_LE, COUNT_SP_EQ
- ASK_SPO, ASK_OP, UNIQUE_SP
- COUNT_OP_GE, COUNT_OP_LE, COUNT_OP_EQ
- COMPARE_O_EQ, COMPARE_O_GT, COMPARE_O_LT, COMPARE_O_GE, COMPARE_O_LE
- CONSTRUCT8

### Epoch - Epoch Operations

**Create Epoch**
```bash
knhks epoch create <id> <tau> <lambda>
```

**Run Epoch**
```bash
knhks epoch run <id>
```

**List Epochs**
```bash
knhks epoch list
```

Example:
```bash
knhks epoch create epoch1 8 "reflex1,reflex2"
knhks epoch run epoch1
```

### Route - Action Routing

**Install Route**
```bash
knhks route install <name> <kind> <target>
```

**List Routes**
```bash
knhks route list
```

Route kinds:
- `webhook` - HTTP webhook (http:// or https://)
- `kafka` - Kafka topic (kafka://brokers/topic)
- `grpc` - gRPC endpoint (grpc://host:port/service/method)
- `lockchain` - Git lockchain (file:// or git://)

Example:
```bash
knhks route install webhook1 webhook https://api.example.com/webhook
knhks route install kafka1 kafka kafka://localhost:9092/actions
```

### Receipt - Receipt Operations

**Get Receipt**
```bash
knhks receipt get <id>
```

**Merge Receipts**
```bash
knhks receipt merge <id1,id2,id3>
```

**List Receipts**
```bash
knhks receipt list
```

### Pipeline - ETL Pipeline

**Run Pipeline**
```bash
knhks pipeline run [--connectors <ids>] [--schema <iri>]
```

**Pipeline Status**
```bash
knhks pipeline status
```

Example:
```bash
knhks pipeline run --connectors kafka-prod
knhks pipeline status
```

### Metrics - OTEL Metrics

**Get Metrics**
```bash
knhks metrics get
```

### Coverage - Dark Matter Coverage

**Get Coverage**
```bash
knhks coverage get
```

## Error Handling

All commands return exit codes:
- `0` - Success
- `1` - Error

Errors are displayed to stderr with descriptive messages.

## Configuration

Configuration is stored in:
- Unix: `~/.knhks/`
- Windows: `%APPDATA%/knhks/`

Files:
- `sigma.ttl` - Schema registry
- `q.sparql` - Invariant registry
- `connectors.json` - Connector registry
- `covers.json` - Cover definitions
- `reflexes.json` - Reflex definitions
- `epochs.json` - Epoch definitions
- `routes.json` - Route definitions

## Guard Constraints

All commands enforce guard constraints:
- **max_run_len ≤ 8** - Run length must not exceed 8
- **τ ≤ 8** - Epoch tick budget must not exceed 8
- **Operation validation** - Reflex operations must be in H_hot set

## Examples

### Complete Workflow

```bash
# Initialize system
knhks boot init schema.ttl invariants.sparql

# Register connector
knhks connect register kafka-prod urn:knhks:schema:default kafka://localhost:9092/triples

# Define cover
knhks cover define "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" "max_run_len 8"

# Declare reflex
knhks reflex declare check-count ASK_SP 0xC0FFEE 0 8

# Create epoch
knhks epoch create epoch1 8 "check-count"

# Run pipeline
knhks pipeline run --connectors kafka-prod

# Check status
knhks pipeline status
knhks metrics get
```

## See Also

- [Architecture](architecture.md) - System architecture
- [API Reference](api.md) - API documentation
- [Integration Guide](integration.md) - Integration examples
