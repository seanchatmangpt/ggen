# TAI Erlang Autonomics

**Status**: Phase 1 (Eval-only)
**Version**: 1.0.0
**License**: Apache-2.0

---

## Overview

TAI Erlang Autonomics is a production-grade Erlang/OTP runtime implementing **autonomous entitlement gating and bounded action control** via the Model Context Protocol (MCP). It provides specification-driven autonomic SKU management on Google Cloud Platform (GCP).

**Core Mission**: Transform RDF ontology specifications into deterministic, cryptographically verifiable action sequences with immutable audit trails.

---

## Key Features

- **HTTP-First API**: RESTful endpoints for marketplace and event ingestion
- **MCP Integration**: Tool discovery and invocation via Model Context Protocol
- **Autonomic Governors**: gen_statem-based state machines for entitlement control
- **Gate Checking**: Three-stage sequential validation (quota, tier, compliance)
- **Bounded Execution**: Poolboy worker pool prevents resource exhaustion
- **Cryptographic Receipts**: SHA-256 hash chain for audit trail verification
- **Production-Ready**: Fault-tolerant, observable, scalable
- **Eval-Only**: Phase 1 simplifications (no auth, single instance)

---

## Quick Start

### Prerequisites

- **Erlang/OTP 24+** (26 recommended)
- **Rebar3 3.20+**
- **Docker** (optional, for containerization)
- **GCP Project** (for cloud deployment)

### Local Development

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/ggen.git
cd tai-erlang-autonomics

# Install dependencies
rebar3 compile

# Run tests
rebar3 ct

# Start development server
rebar3 shell

# In Erlang shell:
(tai@localhost)> application:start(tai_autonomics).
ok

# In another terminal, test health endpoint
curl http://localhost:8080/health
```

**Expected Output**:
```json
{
  "status": "ok",
  "node": "tai@localhost",
  "uptime_ms": 1234,
  "timestamp": "2026-01-26T14:30:45.123Z",
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "ok"
  }
}
```

### Docker Deployment

```bash
# Build container image
docker build -f container/Containerfile \
  -t tai-autonomics:latest .

# Run container locally
docker run \
  -e PORT=8080 \
  -p 8080:8080 \
  tai-autonomics:latest

# Test in another terminal
curl http://localhost:8080/health
```

### GCP Cloud Run Deployment

```bash
# Set environment
export PROJECT_ID=your-gcp-project
export REGION=us-central1

# Build and push to Artifact Registry
docker build -t gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.0 .
docker push gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.0

# Deploy to Cloud Run
gcloud run deploy tai-autonomics \
  --image=gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.0 \
  --region=${REGION} \
  --no-allow-unauthenticated \
  --cpu=2 \
  --memory=2Gi \
  --timeout=300

# Test deployment
curl https://tai-autonomics.example.com/health
```

---

## Project Structure

```
tai-erlang-autonomics/
├── apps/
│   ├── taiea_core/              # Core runtime
│   │   ├── src/
│   │   │   ├── taiea_http.erl          # HTTP server
│   │   │   ├── taiea_http_handler.erl  # Request handling
│   │   │   └── ...
│   │   ├── test/
│   │   │   └── *.erl                   # Unit tests
│   │   └── priv/
│   │
│   └── tai_autonomics/          # Autonomics app
│       ├── src/
│       │   ├── tai_governor.erl         # State machine
│       │   ├── taiea_gates.erl          # Gate checking
│       │   ├── taiea_receipts.erl       # Receipt ledger
│       │   ├── tai_action_executor.erl  # Action execution
│       │   └── ...
│       └── test/
│           └── *.erl
│
├── config/
│   ├── sys.config               # Runtime configuration
│   └── vm.args                  # VM arguments
│
├── rel/
│   └── reltool.config           # Release configuration
│
├── container/
│   └── Containerfile            # Docker build
│
├── terraform/                   # GCP infrastructure as code
├── scripts/                     # Utility scripts
├── tests/                       # Integration tests
├── .github/
│   └── workflows/               # CI/CD pipelines
│
├── docs/
│   ├── README.md                # This file
│   ├── ARCHITECTURE.md          # System design
│   ├── DEVELOPER_GUIDE.md       # Development setup
│   ├── OPERATIONAL_GUIDE.md     # Operations & deployment
│   ├── API.md                   # API reference
│   └── ...
│
├── rebar.config                 # Build configuration
├── Makefile                     # Make targets
└── docker-compose.yml           # Local development stack
```

---

## Documentation

- **[ARCHITECTURE.md](docs/ARCHITECTURE.md)**: System design, components, data flow
- **[DEVELOPER_GUIDE.md](docs/DEVELOPER_GUIDE.md)**: Building, testing, development workflow
- **[OPERATIONAL_GUIDE.md](docs/OPERATIONAL_GUIDE.md)**: Deployment, scaling, troubleshooting
- **[API.md](docs/API.md)**: HTTP endpoints and MCP tools
- **[CONFIG.md](docs/CONFIG.md)**: Configuration reference
- **[ENDPOINTS.md](docs/ENDPOINTS.md)**: Detailed endpoint specifications

---

## API Overview

### HTTP Endpoints

#### GET /health
System readiness check.

```bash
curl http://localhost:8080/health

# Response
{
  "status": "ok",
  "node": "tai@container-id",
  "uptime_ms": 45000,
  "checks": {"http_server": "ok", "pubsub_ready": "ok"}
}
```

#### POST /marketplace
Entitlement action request.

```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "event": "sku_activate",
    "sku_id": "autonomic-v1",
    "tenant_id": "acme-corp",
    "data": {"feature": "report-gen", "tier": "professional"}
  }'

# Response
{
  "status": "accepted",
  "event_id": "evt-uuid",
  "receipt": {
    "id": "r_2026_01_26_abc123",
    "timestamp": "2026-01-26T14:30:45.123Z",
    "hash": "sha256..."
  }
}
```

#### POST /pubsub
Pub/Sub event ingestion.

```bash
curl -X POST http://localhost:8080/pubsub \
  -H "Content-Type: application/json" \
  -d '{
    "message": {
      "data": "base64-encoded-data",
      "attributes": {"event_type": "quota_reset"}
    }
  }'
```

#### GET /mcp/tools
MCP tool discovery.

```bash
curl http://localhost:8080/mcp/tools | jq '.tools'
```

See [API.md](docs/API.md) for complete endpoint reference.

---

## Phase 1 Status

### Implemented ✓

- [x] HTTP server with health, marketplace, pubsub endpoints
- [x] Request validation and schema checking
- [x] Governor state machine (boot → active → suspended → complying)
- [x] Three-stage gate checking (quota, tier, compliance)
- [x] Bounded action executor with poolboy worker pool
- [x] Receipt emission with SHA-256 hash chain
- [x] Firestore persistence
- [x] Prometheus metrics
- [x] OpenTelemetry tracing
- [x] Docker containerization
- [x] Comprehensive test suite (unit, integration, property-based)
- [x] Development and operational guides

### Planned for Phase 2 ⏳

- [ ] JWT authentication requirement
- [ ] Multi-instance deployment with shared state
- [ ] Redis for distributed governor state
- [ ] PagerDuty / Slack alerting
- [ ] GraphQL API
- [ ] Advanced compliance governors
- [ ] Custom gate plugins

---

## Building & Testing

### Compile

```bash
# Development
rebar3 compile

# Production (optimized)
rebar3 as prod compile
```

### Test

```bash
# All tests
rebar3 ct

# Specific test suite
rebar3 ct --suite=tai_governor_SUITE

# With coverage report
rebar3 ct --cover
open _build/test/cover/index.html
```

### Create Release

```bash
# Development release
rebar3 release

# Production release
rebar3 as prod release

# Start release
_build/prod/rel/tai_autonomics/bin/tai_autonomics start
```

---

## Configuration

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `PORT` | `8080` | HTTP server port |
| `LOG_LEVEL` | `info` | Logging verbosity |
| `FIRESTORE_PROJECT_ID` | Auto-detected | GCP project ID |
| `PUBSUB_TOPIC` | `erlang-autonomics-events` | Pub/Sub topic |

### Runtime Configuration

Edit `config/sys.config`:

```erlang
{taiea_core, [
    {http_port, 8080},
    {max_connections, 1000}
]},
{tai_autonomics, [
    {action_timeout_ms, 30000},
    {pool_size, 10}
]}.
```

See [CONFIG.md](docs/CONFIG.md) for complete reference.

---

## Development Workflow

### 1. Unit Tests (Chicago School TDD)

```bash
# Write failing test
cat > apps/tai_autonomics/test/my_feature_SUITE.erl << 'EOF'
-module(my_feature_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [my_feature_works].

my_feature_works(_Config) ->
    {ok, result} = my_feature:do_something(input),
    ok.
EOF

# Run test (RED)
rebar3 ct --suite=my_feature_SUITE

# Implement feature (GREEN)
# ...

# Refactor (REFACTOR)
rebar3 format
```

### 2. Integration Tests

```bash
# Run integration test suite
rebar3 ct --suite=integration_SUITE
```

### 3. Performance Benchmarks

```bash
# Run benchmark suite
rebar3 ct --suite=performance_SUITE
```

---

## Deployment Checklist

- [ ] All tests passing: `rebar3 ct`
- [ ] Code formatted: `rebar3 format`
- [ ] No compiler warnings: `rebar3 compile`
- [ ] Docker image builds: `docker build ...`
- [ ] Smoke test passes: `./tools/smoke.sh`
- [ ] Canary deployment successful
- [ ] Monitoring/alerting configured
- [ ] Documentation updated

See [OPERATIONAL_GUIDE.md](docs/OPERATIONAL_GUIDE.md) for detailed checklist.

---

## Support & Troubleshooting

### Common Issues

**Port already in use**:
```bash
lsof -ti:8080 | xargs kill -9
```

**Dependency lock mismatch**:
```bash
rebar3 unlock && rebar3 compile
```

**Build cache corrupted**:
```bash
rebar3 clean && rebar3 compile
```

**Tests failing**:
```bash
rebar3 ct --verbose
# Check test output for details
```

For more help, see [OPERATIONAL_GUIDE.md](docs/OPERATIONAL_GUIDE.md#4-troubleshooting-guide).

---

## Architecture Highlights

### Autonomic State Machine

```
boot → active → suspended ↔ complying
                 ↑
                 └─→ active (on reset)
```

### Receipt Chain

Every action emits a cryptographic receipt with hash chain:
```
Receipt N-1 --hash-→ Receipt N --hash-→ Receipt N+1
           signature           signature
```

### Gate Checking

Three-stage sequential validation:
1. **Quota Gate**: Check quota remaining
2. **Tier Gate**: Check user tier
3. **Compliance Gate**: Check compliance status

### Bounded Execution

Worker pool prevents resource exhaustion:
```
Request → Queue → Poolboy (10 workers) → Action → Result
                  ↑                      ↓
                  └─ Timeout: 30s ──────┘
```

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## License

Apache License 2.0 - See LICENSE file for details.

---

## Support

- **Documentation**: See `/docs` directory
- **Issues**: Open GitHub issue with reproducible example
- **Questions**: Post in discussions or contact team

---

**Built with Erlang/OTP • Deployed on GCP Cloud Run • Spec-Driven Architecture**

Last updated: 2026-01-26
