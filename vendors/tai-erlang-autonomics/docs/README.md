# TAI Erlang Autonomics

Production-grade Erlang/OTP runtime for autonomic SKU management on GCP Cloud Run.

## Quick Start

### Build

```bash
cd tai-erlang-autonomics
rebar3 compile
```

### Test

```bash
rebar3 ct
rebar3 proper
```

### Release

```bash
rebar3 release
_build/default/rel/tai_autonomics/bin/tai_autonomics start
```

### Container

```bash
docker build -f container/Containerfile -t tai-autonomics:dev .
docker run -e PORT=8080 -p 8080:8080 tai-autonomics:dev
```

### Health Check

```bash
curl http://localhost:8080/health
```

## Architecture

- **HTTP Server**: Cowboy-based HTTP server with `/health`, `/pubsub`, `/marketplace` endpoints
- **Governors**: gen_statem-based autonomous governors with entitlement gates
- **Receipts**: Cryptographic receipt ledger with hash chain verification
- **Actions**: Bounded executor with poolboy worker pool
- **Observability**: Prometheus metrics, OpenTelemetry tracing, structured JSON logging

## Configuration

See [CONFIG.md](CONFIG.md) for environment variables and configuration options.

## API

See [ENDPOINTS.md](ENDPOINTS.md) for API reference.

## Receipts

See [RECEIPTS.md](RECEIPTS.md) for receipt schema and hash chain explanation.

## Operations

See [RUNBOOK.md](RUNBOOK.md) for operations guide and troubleshooting.
