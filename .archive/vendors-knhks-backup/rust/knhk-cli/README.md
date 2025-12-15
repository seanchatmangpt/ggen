# KNHK CLI - Multi-File Noun-Verb Implementation

## Structure

Based on CONVO.txt API specifications, the CLI is organized into multiple files:

```
rust/knhk-cli/
├── src/
│   ├── main.rs              # Main entry point with noun/verb definitions
│   └── commands/
│       ├── mod.rs           # Module exports
│       ├── boot.rs          # boot(#{sigma, q}) - Initialize Σ and Q
│       ├── connect.rs       # connect(#{name, schema, source, map, guard})
│       ├── cover.rs         # cover(#{select, shard}) - Define cover over O
│       ├── admit.rs         # admit(Δ) - Admit delta into O
│       ├── reflex.rs        # reflex(#{name, op, run, args, epoch})
│       ├── epoch.rs         # epoch(#{tau, lambda, cover}), run(EpochId)
│       ├── route.rs         # route(#{name, kind, target, encode})
│       ├── receipt.rs       # receipt(Id), merge(Receipts)
│       ├── metrics.rs       # metrics() - OTEL metrics
│       └── coverage.rs      # coverage() - Dark Matter 80/20 coverage
├── Cargo.toml
└── README.md
```

## Nouns (11)

1. **boot** - System initialization (Σ, Q)
2. **connect** - Connector management
3. **cover** - Cover definition over O
4. **admit** - Delta admission
5. **reflex** - Reflex declaration
6. **epoch** - Epoch operations
7. **route** - Action routing
8. **receipt** - Receipt operations
9. **metrics** - Metrics operations
10. **coverage** - Coverage operations

## Verbs (20+)

### Boot
- `init` - Initialize Σ and Q

### Connect
- `register` - Register a connector
- `list` - List connectors

### Cover
- `define` - Define cover over O
- `list` - List covers

### Admit
- `delta` - Admit Δ into O

### Reflex
- `declare` - Declare a reflex
- `list` - List reflexes

### Epoch
- `create` - Create epoch
- `run` - Run epoch
- `list` - List epochs

### Route
- `install` - Install route
- `list` - List routes

### Receipt
- `get` - Get receipt
- `merge` - Merge receipts
- `list` - List receipts

### Metrics
- `get` - Get metrics

### Coverage
- `get` - Get coverage

## Usage Examples

```bash
# Initialize system
knhk boot init --sigma schema.ttl --q invariants.ttl

# Register connector
knhk connect register kafka-prod --schema urn:knhk:schema:kafka --source kafka://localhost:9092

# Define cover
knhk cover define --select predicates.json --shard shard.json

# Admit delta
knhk admit delta --file delta.json

# Declare reflex
knhk reflex declare my-hook --op ASK_SP --pred 0xC0FFEE --off 0 --len 8

# Create epoch
knhk epoch create epoch-1 --tau 8 --lambda "hook1,hook2"

# Run epoch
knhk epoch run epoch-1

# Install route
knhk route install webhook-1 --kind webhook --target https://api.example.com/webhook

# Get receipt
knhk receipt get receipt-123

# Merge receipts
knhk receipt merge receipt-1,receipt-2,receipt-3

# Get metrics
knhk metrics get

# Get coverage
knhk coverage get
```

## Implementation Status

- ✅ Multi-file structure implemented
- ✅ All nouns from CONVO.txt API
- ✅ All verbs from CONVO.txt API
- ⏳ Integration with actual KNHK crates
- ⏳ Error handling and validation
- ⏳ Output formatting (JSON, table)

## Next Steps

1. Integrate with knhk-etl for boot/connect/admit
2. Integrate with knhk-hot for reflex operations
3. Integrate with knhk-lockchain for receipt operations
4. Integrate with knhk-otel for metrics
5. Add parameter parsing and validation
6. Add output formatting
