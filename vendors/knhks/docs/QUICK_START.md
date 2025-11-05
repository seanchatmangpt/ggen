# Quick Start Guide

**80/20 Focus**: This guide covers the essential 20% to get 80% of value.

## 5-Minute Setup

### 1. Build

```bash
make lib
cd rust/knhks-cli && cargo build --release
```

### 2. Initialize

```bash
knhks boot init schema.ttl invariants.sparql
```

### 3. Register Connector

```bash
knhks connect register kafka-prod urn:knhks:schema:default kafka://localhost:9092/triples
```

### 4. Run Pipeline

```bash
knhks pipeline run --connectors kafka-prod
```

## Common Commands

```bash
# List connectors
knhks connect list

# Define cover
knhks cover define "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" "max_run_len 8"

# Declare reflex
knhks reflex declare check-count ASK_SP 0xC0FFEE 0 8

# Run epoch
knhks epoch create epoch1 8 "check-count"
knhks epoch run epoch1

# Check status
knhks pipeline status
knhks metrics get
```

## Next Steps

- [CLI Guide](cli.md) - Complete command reference
- [Architecture](architecture.md) - System architecture
- [Integration Guide](integration.md) - Integration examples

---

**Principle**: Start with these essentials, expand as needed.
