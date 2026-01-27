# TAI Autonomics Installation Guide

**Status**: Production Ready
**Last Updated**: 2026-01-26

---

## Prerequisites

### Required
- **gcloud CLI**: For GCP deployment and authentication
  - Installation: https://cloud.google.com/sdk/docs/install
  - Verify: `gcloud --version`

### Optional
- **Erlang/OTP 25+** (26 recommended): For local compilation
  - Installation: https://www.erlang.org/downloads
  - Verify: `erl -version`
- **Rebar3 3.20+**: Erlang build tool
  - Installation: https://github.com/erlang/rebar3
  - Verify: `rebar3 version`
- **Docker**: For local containerized testing
  - Installation: https://docs.docker.com/get-docker/
  - Verify: `docker --version`

---

## Quick Start (5 minutes)

### Option 1: Cloud Run Deployment (Recommended for PoC)

```bash
cd tai-erlang-autonomics
./tools/gcp-deploy.sh
```

**What this does**:
1. Creates GCP project (or uses existing)
2. Enables Cloud Run, Pub/Sub, Artifact Registry
3. Builds and pushes container image
4. Deploys to Cloud Run
5. Outputs service URL and health check command

**Result**: Service live at `https://your-service.run.app` within 5 minutes.

### Option 2: Local Docker Testing

```bash
cd tai-erlang-autonomics
docker build -f container/Containerfile -t tai-autonomics:dev .
docker run -e PORT=8080 -p 8080:8080 tai-autonomics:dev
```

**Result**: Service available at `http://localhost:8080`

### Option 3: Local Erlang Build (Development Only)

Prerequisites: Erlang 25+, Rebar3 3.20+

```bash
cd tai-erlang-autonomics
rebar3 compile
rebar3 release
_build/default/rel/tai_autonomics/bin/tai_autonomics start
```

**Result**: Service available at `http://localhost:8080`

---

## Detailed Installation

### 1. Cloud Run Deployment (Recommended)

#### Prerequisites
1. GCP account with billing enabled
2. gcloud CLI installed and authenticated
   ```bash
   gcloud auth login
   gcloud config set project YOUR_PROJECT_ID
   ```

#### Steps

**Step 1: Clone and navigate**
```bash
git clone <repository-url>
cd tai-erlang-autonomics
```

**Step 2: Run deployment script**
```bash
./tools/gcp-deploy.sh
```

**Step 3: Follow interactive prompts**
- Confirm project ID
- Select region (default: us-central1)
- Confirm service name (default: tai-autonomics)

**Step 4: Verify deployment**
```bash
curl https://your-service.run.app/health
# Expected: 200 OK with health status JSON
```

**Step 5: Configure environment** (optional)
Edit `terraform/cloud_run.tf` to customize:
- Memory allocation (default: 512MB)
- CPU allocation (default: 1)
- Timeout (default: 300s)
- Environment variables (see Configuration section)

#### Troubleshooting Cloud Run Deployment

**Issue**: `Permission denied` error
```
gcloud auth application-default login
```

**Issue**: `Service already exists`
```
gcloud run services describe tai-autonomics --region us-central1
# Then delete and redeploy if needed
gcloud run services delete tai-autonomics --region us-central1
```

**Issue**: Container image fails to build
```
# Check build logs
gcloud builds log [BUILD_ID]
# Rebuild locally first
docker build -f container/Containerfile -t tai-autonomics:test .
```

---

### 2. Local Docker Deployment

#### Prerequisites
- Docker installed and running
- No Erlang/Rebar3 required

#### Steps

**Step 1: Build container**
```bash
cd tai-erlang-autonomics
docker build -f container/Containerfile -t tai-autonomics:dev .
```

**Step 2: Run container**
```bash
docker run -d \
  --name tai-autonomics \
  -e PORT=8080 \
  -e LOG_LEVEL=info \
  -p 8080:8080 \
  tai-autonomics:dev
```

**Step 3: Verify health**
```bash
curl http://localhost:8080/health
```

**Step 4: View logs**
```bash
docker logs -f tai-autonomics
```

**Step 5: Stop container**
```bash
docker stop tai-autonomics
docker rm tai-autonomics
```

#### Environment Variables
```bash
-e PORT=8080                          # HTTP server port
-e LOG_LEVEL=info                     # Logging level (debug, info, warn, error)
-e NODE_NAME=tai@localhost            # Erlang node name
-e COOKIE=insecure-dev                # Erlang distribution cookie
-e PUBSUB_SUBSCRIPTION=tai-events     # GCP Pub/Sub subscription (optional)
```

#### Healthcheck

The container includes a Docker healthcheck:
```dockerfile
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1
```

---

### 3. Local Erlang Build (Development Only)

#### Prerequisites
- Erlang 25+ (26 recommended)
- Rebar3 3.20+
- macOS, Linux, or WSL2

#### Steps

**Step 1: Install Erlang** (if needed)
```bash
# macOS (via Homebrew)
brew install erlang

# Linux (Ubuntu/Debian)
apt-get update && apt-get install -y erlang

# Or use asdf
asdf plugin add erlang
asdf install erlang 26.0.2
```

**Step 2: Install Rebar3**
```bash
# macOS (via Homebrew)
brew install rebar3

# Or download directly
wget https://github.com/erlang/rebar3/releases/download/3.22.0/rebar3
chmod +x rebar3
mv rebar3 /usr/local/bin/
```

**Step 3: Clone and navigate**
```bash
git clone <repository-url>
cd tai-erlang-autonomics
```

**Step 4: Compile**
```bash
rebar3 compile
```

**Step 5: Run tests** (optional, but recommended)
```bash
rebar3 ct        # Common Test suite
rebar3 eunit     # EUnit tests
rebar3 proper    # Property-based tests
```

**Step 6: Build release**
```bash
rebar3 release
```

**Step 7: Start service**
```bash
# Foreground (for development)
_build/default/rel/tai_autonomics/bin/tai_autonomics foreground

# Daemon mode (for production-like testing)
_build/default/rel/tai_autonomics/bin/tai_autonomics start
_build/default/rel/tai_autonomics/bin/tai_autonomics stop
```

**Step 8: Verify**
```bash
curl http://localhost:8080/health
```

#### Development Mode

For faster iteration:
```bash
rebar3 shell
```

Then in the Erlang shell:
```erlang
% Recompile on changes
c:make().

% Call functions directly
taiea_http_handler:health_check().
```

---

## Configuration

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `PORT` | 8080 | HTTP server port |
| `LOG_LEVEL` | info | Logging level (debug, info, warn, error) |
| `NODE_NAME` | tai@localhost | Erlang node name (Cloud Run: auto-set) |
| `COOKIE` | insecure-dev | Erlang distribution cookie |
| `PUBSUB_SUBSCRIPTION` | (none) | GCP Pub/Sub subscription for events |
| `PUBSUB_TOPIC` | (none) | GCP Pub/Sub topic for publishing |
| `PROJECT_ID` | (none) | GCP project ID (Cloud Run: auto-set) |
| `MAX_WORKERS` | 10 | Max bounded executor pool size |
| `QUEUE_SIZE` | 100 | Bounded queue size |

### Configuration File

For Cloud Run, edit `terraform/cloud_run.tf`:

```hcl
resource "google_cloud_run_service" "tai_autonomics" {
  service_name = "tai-autonomics"

  template {
    spec {
      containers {
        image = "..."

        env {
          name  = "LOG_LEVEL"
          value = "info"
        }

        env {
          name  = "MAX_WORKERS"
          value = "20"
        }

        resources {
          limits = {
            memory = "1Gi"
            cpu    = "2"
          }
        }
      }

      timeout_seconds = 300
    }
  }
}
```

Then apply:
```bash
terraform -chdir=terraform apply
```

---

## Smoke Tests

### Quick Health Check
```bash
curl http://localhost:8080/health
```

Expected response:
```json
{
  "status": "ok",
  "node": "tai@localhost",
  "uptime_ms": 12345,
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "ok"
  }
}
```

### Test Marketplace Endpoint
```bash
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{"event": "sku_inquiry", "sku_id": "autonomic-v1"}'
```

### Test Entitlement Gate
```bash
# Via MCP tool (requires MCP server running)
# See API.md for MCP tool invocation examples
```

---

## Troubleshooting

### Container Won't Start
```bash
# Check image exists
docker images | grep tai-autonomics

# Check logs
docker logs tai-autonomics

# Rebuild with verbose output
docker build --progress=plain -f container/Containerfile -t tai-autonomics:dev .
```

### Health Check Fails
```bash
# Check service is listening
curl -v http://localhost:8080/health

# Check logs for errors
docker logs -f tai-autonomics | grep ERROR

# Verify dependencies (Pub/Sub, etc.)
gcloud pubsub topics list
gcloud pubsub subscriptions list
```

### Compilation Errors (Local Build)
```bash
# Clean and rebuild
rebar3 clean
rebar3 compile

# Check Erlang version
erl -version
# Must be 25+ (26 recommended)

# Update rebar3
rebar3 local install
```

### Port Already in Use
```bash
# Find process using port 8080
lsof -i :8080

# Kill it
kill -9 <PID>

# Or use different port
docker run -p 8081:8080 tai-autonomics:dev
```

### Pub/Sub Connection Issues
```bash
# Verify GCP authentication
gcloud auth application-default login

# Test Pub/Sub access
gcloud pubsub topics list

# Check Cloud Run service account permissions
gcloud iam service-accounts list
```

---

## Upgrade & Patching

### Docker Image Update
```bash
# Rebuild container
docker build -f container/Containerfile \
  -t tai-autonomics:latest .

# Stop old container
docker stop tai-autonomics

# Run new container
docker run -d \
  --name tai-autonomics \
  -p 8080:8080 \
  tai-autonomics:latest
```

### Cloud Run Deployment Update
```bash
# Rebuild and redeploy
./tools/gcp-deploy.sh
```

### Local Release Update
```bash
rebar3 clean
rebar3 release
_build/default/rel/tai_autonomics/bin/tai_autonomics stop
_build/default/rel/tai_autonomics/bin/tai_autonomics start
```

---

## Next Steps

1. **Health Check**: Verify service is running
   ```bash
   curl http://localhost:8080/health
   ```

2. **Read API Documentation**: See `API.md` for endpoint reference

3. **Run MCP Tests**: See `TAIEA_MCP_QUICK_REFERENCE.md` for MCP tool examples

4. **Review Configuration**: See `CONFIG.md` for advanced configuration

5. **Check Runbook**: See `RUNBOOK.md` for operational procedures

---

## Support

For issues during installation:
1. Check Troubleshooting section above
2. Review service logs: `docker logs tai-autonomics`
3. Verify prerequisites are installed correctly
4. Contact support via SUPPORT_MODEL.md process

## Document Control

| Field | Value |
|-------|-------|
| **Version** | 1.0 |
| **Audience** | Customers, integrators, developers |
| **Last Review** | 2026-01-26 |
| **Next Review** | 2026-04-01 |
