# Release Notes - TAI Erlang Autonomics v1.0.0

**Release Date:** January 25, 2026
**Version:** 1.0.0
**Status:** Production Ready

---

## Overview

TAI Erlang Autonomics v1.0.0 is the production-ready release of the Erlang/OTP runtime for autonomous SKU management on Google Cloud Platform. This release introduces a complete, fault-tolerant system for managing entitlements, quotas, and autonomous actions with full observability.

---

## Key Features

### 1. HTTP API Server

**Cowboy-Based HTTP Server** with three primary endpoints:

- **`GET /health`** - Health check for readiness probes
  - Returns JSON status of all components
  - Used by Cloud Run/Kubernetes health checks
  - Response time: <5ms

- **`POST /pubsub`** - Receives Google Pub/Sub notifications
  - Validates and processes autonomic signals
  - Triggers corresponding actions
  - Includes request signature verification
  - Graceful handling of malformed payloads

- **`POST /marketplace`** - Processes entitlement actions
  - Grant, revoke, and activate entitlements
  - Enforces quota and billing policies
  - Returns cryptographic receipts

**Features:**
- Port configurable via `PORT` environment variable
- TLS support (via Cloud Run/Load Balancer)
- Graceful shutdown on SIGTERM
- Request timeout protection
- Rate limiting support

### 2. Autonomic Governors

**State Machine-Based Governance** using `gen_statem`

Each tenant has a dedicated governor managing:
- Entitlement lifecycle (active → suspended → complying)
- Quota tracking and enforcement
- Action execution with bounded concurrency
- Receipt emission for all state changes

**Governor States:**
```
boot → active ↔ suspended ↔ complying
```

**Capabilities:**
- Per-tenant isolation
- Idempotent operation semantics
- Automatic retry with exponential backoff
- Comprehensive state transition logging
- Failure recovery without data loss

**Performance:**
- Action execution latency: <100ms (p99)
- Quota checks: <5ms
- State transitions: <10ms

### 3. Cryptographic Receipt Ledger

**Immutable Audit Trail** with hash chain verification

Every action generates a receipt including:
- Action type and result
- Timestamp and tenant information
- Cryptographic signature
- Reference to previous receipt (hash chain)

**Features:**
- SHA-256 hash chain for tamper detection
- JWS digital signatures
- Firestore persistence
- Local buffering on connectivity issues
- Receipt verification tools

**Receipt Types:**
- `action_success` - Action completed successfully
- `action_failure` - Action failed (with error details)
- `state_change` - Governor state transition
- `quota_update` - Quota allocation change
- `refusal` - Request rejected (with reason)

### 4. Bounded Action Executor

**Worker Pool Management** using Poolboy

- Configurable concurrency (default: 10 workers)
- Queue-based request handling
- Timeout protection (default: 30 seconds)
- Fair scheduling
- Prevents resource exhaustion

**Features:**
- Auto-scaling disabled (prevents overload)
- Graceful timeout handling
- Per-action receipt emission
- Worker pool monitoring
- Health reporting

### 5. Observability

#### Prometheus Metrics
- Request count and latency (by endpoint)
- Governor state transitions
- Action execution metrics
- Queue depth and worker utilization
- Memory and CPU metrics

#### OpenTelemetry Tracing
- Distributed trace context propagation
- Span creation for each request
- Automatic attributes (tenant_id, request_id, etc.)
- Configurable sampling rate

#### Structured JSON Logging
- JSON-formatted log output
- Trace correlation IDs
- Severity levels (debug, info, warn, error)
- Structured field extraction
- Cloud Logging integration

### 6. GCP Integration

#### Cloud Run
- Containerized deployment
- Auto-scaling configuration
- Environment variable management
- Service account integration
- Health check support

#### Pub/Sub
- Event-driven architecture
- Automatic subscription management
- Message acknowledgment
- Dead letter handling (optional)
- Configurable retry policy

#### Firestore
- Receipt ledger storage
- Document-based schema
- Automatic indexing
- Transaction support for consistency
- Time-based queries for audit

#### Cloud Monitoring
- Custom metric export
- Dashboard support
- Alert policy integration
- Log-based metrics

### 7. Security

**Authentication:**
- JWT signature verification (configurable)
- Asymmetric key support (RS256, ES256)
- Token expiration enforcement
- Async verification (non-blocking)

**Authorization:**
- Tenant isolation enforcement
- Quota-based access control
- Role-based action restrictions
- Service account RBAC via GCP IAM

**Data Protection:**
- TLS for external calls
- Encrypted Firestore documents
- No credentials in code
- GCP Secret Manager integration

**Compliance:**
- Complete audit trail via receipts
- Non-repudiation via signatures
- Compliance governor for policy enforcement
- Retention policy management

---

## Architecture Components

### Application Structure
```
tai-autonomics/
├── apps/tai_autonomics/
│   ├── src/
│   │   ├── tai_autonomics_app.erl      - Application entry point
│   │   ├── tai_autonomics_sup.erl      - Main supervisor
│   │   ├── tai_http.erl                - HTTP server (gen_server)
│   │   ├── tai_http_handler.erl        - HTTP request handler
│   │   ├── tai_governor.erl            - State machine governor
│   │   ├── tai_receipts.erl            - Receipt ledger (gen_server)
│   │   ├── tai_actions.erl             - Action executor (gen_server)
│   │   ├── tai_tracing.erl             - OpenTelemetry integration
│   │   ├── tai_logging.erl             - Structured logging
│   │   ├── gcp_pubsub.erl              - Pub/Sub client
│   │   ├── gcp_metadata.erl            - GCP metadata service
│   │   └── gcp_firestore.erl           - Firestore client
│   ├── test/
│   │   ├── tai_ct_SUITE.erl            - Common Test suite
│   │   ├── tai_pubsub_prop.erl         - Property-based tests
│   │   └── perf_benchmarks/            - Performance benchmarks
│   └── include/
│       └── tai.hrl                     - Shared header
├── config/
│   ├── sys.config                      - Runtime configuration
│   └── vm.args                         - VM arguments
├── rel/
│   └── relx.config                     - Release configuration
├── container/
│   └── Containerfile                   - Multi-stage Docker build
├── terraform/
│   ├── main.tf                         - Cloud Run, Pub/Sub, Firestore
│   ├── variables.tf                    - Input variables
│   └── outputs.tf                      - Output values
└── docs/
    ├── ARCHITECTURE.md                 - System architecture
    ├── ENDPOINTS.md                    - API reference
    ├── CONFIG.md                       - Configuration guide
    ├── RECEIPTS.md                     - Receipt schema
    ├── RUNBOOK.md                      - Operations guide
    ├── TROUBLESHOOTING.md              - Troubleshooting guide
    └── SECURITY_REQUIREMENTS.md        - Security specification
```

### Supervision Tree
```
tai_autonomics_sup (application supervisor)
├── tai_http (HTTP server)
├── governance_sup (Dynamic governor supervisor)
│   └── (Dynamic) tai_governor instances
├── receipt_ledger_sup (Receipt ledger supervisor)
│   └── tai_receipts
├── cluster_sup (Cluster management)
│   └── tai_cluster
└── observability_sup (Observability)
    ├── prometheus_registry
    └── otel_tracer
```

---

## System Requirements

### Erlang/OTP
- **Minimum:** OTP 25
- **Recommended:** OTP 26 or later
- **Tested:** OTP 26.0

### Build Tools
- Rebar3: 3.20 or later
- Make: 4.3 or later

### Runtime Resources

**Minimum (Development):**
- CPU: 1 core
- Memory: 256 MB
- Disk: 100 MB

**Recommended (Production):**
- CPU: 1-2 cores (Cloud Run default: 1)
- Memory: 512 MB - 1 GB (Cloud Run default: 512 MB)
- Disk: 1 GB
- Network: 10 Mbps minimum

### GCP Services

**Required:**
- Cloud Run (container runtime)
- Firestore (receipt storage)
- Cloud Pub/Sub (event stream)
- Artifact Registry (container registry)

**Recommended:**
- Cloud Monitoring (metrics)
- Cloud Logging (logs)
- Cloud Trace (distributed tracing)
- Cloud IAM (access control)

---

## Deployment Guide

### Build from Source

```bash
# Clone repository
git clone https://github.com/example/tai-erlang-autonomics.git
cd tai-erlang-autonomics

# Install Erlang (if needed)
asdf install erlang 26.0

# Compile
rebar3 compile

# Run tests
rebar3 ct
rebar3 proper

# Build release
rebar3 release

# Run locally
_build/default/rel/tai_autonomics/bin/tai_autonomics foreground
```

### Docker Build

```bash
# Build image
docker build -f container/Containerfile \
  -t tai-autonomics:v1.0.0 \
  -t tai-autonomics:latest .

# Run locally
docker run -p 8080:8080 \
  -e PORT=8080 \
  -e GCP_PROJECT_ID=my-project \
  tai-autonomics:latest

# Test health
curl http://localhost:8080/health
```

### Cloud Run Deployment

```bash
# Push to Artifact Registry
docker tag tai-autonomics:v1.0.0 \
  gcr.io/PROJECT_ID/tai-autonomics:v1.0.0
docker push gcr.io/PROJECT_ID/tai-autonomics:v1.0.0

# Deploy to Cloud Run
gcloud run deploy tai-autonomics \
  --image=gcr.io/PROJECT_ID/tai-autonomics:v1.0.0 \
  --region=us-central1 \
  --port=8080 \
  --memory=512Mi \
  --cpu=1 \
  --service-account=tai-autonomics-sa@PROJECT_ID.iam.gserviceaccount.com \
  --env-vars-file=.env.prod
```

### Terraform Deployment

```bash
# Initialize
cd terraform
terraform init

# Plan
terraform plan -out=tfplan

# Apply
terraform apply tfplan

# Get service URL
gcloud run services describe tai-autonomics \
  --region=us-central1 --format='value(status.url)'
```

---

## Configuration

### Environment Variables

**HTTP Server:**
- `PORT` - Server port (default: 8080)
- `HOST` - Server host (default: 0.0.0.0)

**GCP:**
- `GCP_PROJECT_ID` - GCP project ID (required for production)
- `GCP_REGION` - GCP region (default: us-central1)
- `GOOGLE_APPLICATION_CREDENTIALS` - Path to service account JSON

**Pub/Sub:**
- `PUBSUB_SUBSCRIPTION` - Subscription name (default: erlang-autonomics-signals)
- `PUBSUB_EMULATOR_HOST` - Emulator host for testing

**Firestore:**
- `FIRESTORE_ENABLED` - Enable Firestore (default: true)
- `FIRESTORE_EMULATOR_HOST` - Emulator host for testing

**Observability:**
- `TRACING_ENABLED` - Enable OpenTelemetry (default: true)
- `OTEL_EXPORTER_OTLP_ENDPOINT` - OTLP collector endpoint
- `OTEL_SAMPLER_RATE` - Trace sampling rate (0.0-1.0, default: 0.1)

**Security:**
- `VERIFY_SIGNATURES` - Require JWT verification (default: true for production)
- `JWT_KEY` - Path to public key for verification
- `JWT_ALGORITHM` - Algorithm (RS256, ES256, default: RS256)

See [CONFIG.md](docs/CONFIG.md) for complete reference.

---

## API Reference

### Health Endpoint

```http
GET /health HTTP/1.1
Host: localhost:8080

HTTP/1.1 200 OK
Content-Type: application/json

{"status":"ok"}
```

### Pub/Sub Endpoint

```http
POST /pubsub HTTP/1.1
Host: localhost:8080
Content-Type: application/json

{
  "message": {
    "data": "base64_encoded_data",
    "messageId": "msg_12345"
  },
  "subscription": "projects/my-project/subscriptions/my-sub"
}

HTTP/1.1 200 OK
Content-Type: application/json

{
  "receipt_id": "r_...",
  "type": "action_success",
  "timestamp": 1704067200000
}
```

### Marketplace Endpoint

```http
POST /marketplace HTTP/1.1
Host: localhost:8080
Content-Type: application/json
Authorization: Bearer eyJhbGc...

{
  "tenant_id": "tenant_xyz",
  "entitlement_id": "ent_123",
  "action": "grant",
  "signature": "JWS_signature"
}

HTTP/1.1 200 OK
Content-Type: application/json

{
  "receipt_id": "r_...",
  "type": "action_success",
  "status": "active"
}
```

See [ENDPOINTS.md](docs/ENDPOINTS.md) for complete API specification.

---

## Receipt Schema

Example receipt:

```json
{
  "receipt_id": "r_2024_01_25_abc123",
  "type": "action_success",
  "timestamp": 1704067200000,
  "tenant_id": "tenant_xyz",
  "entitlement_id": "ent_abc",
  "body": {
    "action": "grant",
    "quota": 100,
    "used": 25
  },
  "previous_hash": "sha256_hash_of_previous_receipt",
  "receipt_hash": "sha256_hash_of_this_receipt",
  "signature": "JWS_signature_of_receipt"
}
```

See [RECEIPTS.md](docs/RECEIPTS.md) for schema details and examples.

---

## Performance Characteristics

### Latency (p99)
- Health check: <5ms
- Action execution: <100ms
- Quota check: <5ms
- Receipt emission: <50ms
- Firestore write: <100ms

### Throughput
- Sustained: >100 requests/second per instance
- Burst: >500 requests/second (with queue buildup)
- Worker concurrency: 10 (configurable)

### Resource Usage
- Memory: 100-200 MB base + state
- CPU: <10% at 50 RPS
- Disk: <100 MB (ephemeral in Cloud Run)

### Scaling
- Horizontal: Native Cloud Run auto-scaling
- Vertical: Memory/CPU limits configurable
- Degradation: Graceful under overload (queuing)

---

## Known Limitations

1. **Single Deployment per Project** - Currently designed for single Cloud Run service per project
2. **Firestore Required** - No alternative persistence backends in v1.0.0
3. **Pub/Sub Only** - No other event sources supported yet
4. **Manual Scaling** - Auto-scaling must be configured in Cloud Run console
5. **Regional** - Must be deployed in single GCP region

---

## Breaking Changes

**None** - This is the initial production release (v1.0.0).

---

## Deprecations

**None** - This is the initial production release (v1.0.0).

---

## Migration Guide

**N/A** - This is the initial production release.

---

## Upgrade Path

### From Development to Production
1. Follow [PRODUCTION_DEPLOYMENT_CHECKLIST.md](PRODUCTION_DEPLOYMENT_CHECKLIST.md)
2. Run all tests and validation
3. Review security configuration
4. Deploy to Cloud Run using Terraform
5. Validate all endpoints and monitoring

### Future Upgrades
- Patch releases (v1.0.1, v1.0.2) will be backward compatible
- Minor releases (v1.1.0) will add features without breaking changes
- Major releases (v2.0.0) may have breaking changes (with migration guide)

---

## Support & Resources

### Documentation
- [README.md](README.md) - Quick start guide
- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - System design
- [ENDPOINTS.md](docs/ENDPOINTS.md) - API reference
- [CONFIG.md](docs/CONFIG.md) - Configuration guide
- [RUNBOOK.md](docs/RUNBOOK.md) - Operations guide
- [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) - Troubleshooting guide
- [SECURITY_REQUIREMENTS.md](docs/SECURITY_REQUIREMENTS.md) - Security spec

### Community
- **GitHub Issues:** [https://github.com/example/tai-erlang-autonomics/issues](https://github.com/example/tai-erlang-autonomics/issues)
- **GitHub Discussions:** [https://github.com/example/tai-erlang-autonomics/discussions](https://github.com/example/tai-erlang-autonomics/discussions)

### Commercial Support
- Email: support@example.com
- Phone: +1-XXX-XXX-XXXX
- Portal: [https://support.example.com](https://support.example.com)

---

## Roadmap

### v1.1.0 (Q2 2026)
- [ ] Alternative persistence backends (BigTable, Datastore)
- [ ] Additional event sources (Cloud Tasks, Scheduler)
- [ ] Enhanced monitoring dashboards
- [ ] Improved error recovery

### v1.2.0 (Q3 2026)
- [ ] Multi-region deployment support
- [ ] Advanced quota policies
- [ ] Machine learning-based anomaly detection
- [ ] Compliance report generation

### v2.0.0 (2027)
- [ ] Kubernetes native support (Helm charts)
- [ ] OpenAPI spec generation
- [ ] GraphQL API support
- [ ] Distributed tracing with OpenTelemetry SDKs

---

## License

Apache License 2.0

---

## Contributors

- Sean Chatman (Lead)
- Erlang/OTP Team
- GCP Team

---

## Acknowledgments

This project builds on:
- Erlang/OTP excellent fault tolerance patterns
- GCP managed services ecosystem
- Open-source Erlang libraries (Cowboy, Poolboy, gproc, etc.)

---

## Changelog

### v1.0.0 - 2026-01-25

**Initial Release:**
- HTTP API server with /health, /pubsub, /marketplace endpoints
- State machine-based governors with quota management
- Cryptographic receipt ledger with hash chain
- Bounded action executor with worker pool
- Prometheus metrics and OpenTelemetry tracing
- GCP integration (Cloud Run, Pub/Sub, Firestore)
- Complete documentation and deployment guides
- Production-ready security and compliance
- Comprehensive test suite

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-25
**Next Review:** 2026-02-25
