# TAI Erlang Autonomics

Production-grade Erlang/OTP runtime for autonomic SKU management on GCP Cloud Run.

## Overview

This is a standalone extraction of the Erlang autonomics implementation from the ggen examples directory. It provides:

- **HTTP Server**: Cowboy-based HTTP server with `/health`, `/pubsub`, `/marketplace` endpoints
- **Governors**: gen_statem-based autonomous governors with entitlement gates
- **Receipts**: Cryptographic receipt ledger with hash chain verification
- **Actions**: Bounded executor with poolboy worker pool
- **Observability**: Prometheus metrics, OpenTelemetry tracing, structured JSON logging

## Quick Start

### Prerequisites

- Erlang/OTP 25+ (26 recommended)
- Rebar3 3.20+
- Docker (for containerization)
- Terraform 1.0+ (for GCP deployment)
- gcloud CLI (for GCP deployment)

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

### GCP Deployment

For production deployment to Google Cloud Platform:

```bash
# Verify GCP readiness
./scripts/gcp-ready.sh

# Build and deploy
make gcp-deploy

# Run integration tests
make gcp-test
```

See [GCP Deployment Guide](docs/GCP_DEPLOYMENT.md) for detailed instructions.

## Project Structure

```
tai-erlang-autonomics/
├── apps/
│   └── tai_autonomics/
│       ├── src/          # Erlang source files
│       ├── include/      # Header files
│       ├── test/         # Test suites
│       └── priv/         # Private resources
├── config/               # Configuration files
├── rel/                  # Release configuration
├── container/            # Container files
├── terraform/            # GCP infrastructure as code
├── scripts/              # Utility scripts
├── test/                 # Integration tests
├── .github/workflows/    # CI/CD workflows
└── docs/                # Documentation
```

## Documentation

- [README.md](docs/README.md) - Project overview
- [ENDPOINTS.md](docs/ENDPOINTS.md) - API reference
- [RECEIPTS.md](docs/RECEIPTS.md) - Receipt schema
- [CONFIG.md](docs/CONFIG.md) - Configuration guide
- [RUNBOOK.md](docs/RUNBOOK.md) - Operations runbook
- [GCP_DEPLOYMENT.md](docs/GCP_DEPLOYMENT.md) - GCP deployment guide

## GCP Infrastructure

The project includes Terraform configuration for deploying to Google Cloud Platform:

- **Cloud Run**: Serverless container hosting
- **Pub/Sub**: Event messaging for autonomic signals
- **Firestore**: Receipt ledger storage
- **Artifact Registry**: Container image storage
- **IAM**: Service accounts and permissions
- **Monitoring**: Cloud Monitoring and alerting

See `terraform/` directory for infrastructure definitions and `docs/GCP_DEPLOYMENT.md` for deployment instructions.

## Definition of Done

- ✅ `rebar3 compile` passes cleanly
- ✅ `rebar3 ct` - all Common Test suites pass
- ✅ `rebar3 release` generates release artifact
- ✅ Container builds and runs locally
- ✅ `/health` returns 200 when dependencies ready
- ✅ `/pubsub` and `/marketplace` refuse safely on invalid input
- ✅ Receipts emitted for every request and transition
- ✅ No mocks/fakes in codebase
- ✅ Terraform configuration validates
- ✅ GCP integration tests pass
- ✅ Docker Compose setup works for local development

## License

Apache-2.0
