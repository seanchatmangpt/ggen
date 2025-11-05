# KNHKS CLI Documentation

## Overview

The KNHKS CLI provides a command-line interface for managing knowledge hooks, connectors, pipelines, and more.

## Quick Start

```bash
# Initialize system
knhks boot init --sigma schema.ttl --q invariants.ttl

# Register a connector
knhks connect register kafka-prod --schema urn:knhks:schema:kafka --source kafka://localhost:9092

# Run pipeline
knhks pipeline run --connectors kafka-prod
```

## Commands

See `knhks --help` for complete command reference.

