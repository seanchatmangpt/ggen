# ggen v5.0.2 - DEB + gVisor Deployment and Operations Runbook

**Version**: 5.0.2
**Last Updated**: 2026-01-05
**Status**: Production Ready
**Audience**: DevOps Engineers, SREs, Platform Teams

---

## Table of Contents

1. [Pre-Deployment Checklist](#pre-deployment-checklist)
2. [Installation & Deployment Guide](#installation--deployment-guide)
3. [Configuration Reference](#configuration-reference)
4. [Operational Procedures](#operational-procedures)
5. [Runbooks](#runbooks)
6. [Monitoring & Alerting Setup](#monitoring--alerting-setup)
7. [FAQ & Troubleshooting](#faq--troubleshooting)

---

## Pre-Deployment Checklist

### System Requirements

**Hardware Requirements:**
- CPU: x86_64 architecture (amd64)
- RAM: Minimum 2GB available
- Disk: 500MB free space for installation
- Network: Internet access for initial setup (optional after installation)

**Operating System Requirements:**
- Debian 11+ (Bookworm recommended)
- Ubuntu 20.04+ LTS
- Any Debian-based distribution with glibc 2.31+

**Software Dependencies:**
| Component | Minimum Version | Required For | Installation |
|-----------|----------------|--------------|--------------|
| glibc | 2.31 | Runtime | `apt-get install libc6` |
| libstdc++ | 10 | Runtime | `apt-get install libstdc++6` |
| gVisor (runsc) | 20230828+ | Sandboxed execution | See [gVisor Setup](#gvisor-installation) |
| containerd | 1.6+ | Container runtime | `apt-get install containerd` |
| Docker | 20.10+ (optional) | Build/development only | `apt-get install docker.io` |

### Security Requirements

**Permissions:**
- Root or sudo access for DEB installation
- User must be in `docker` group (if using Docker deployment)
- File system permissions: read/write to `/usr/bin` and `/usr/share/doc`

**Network:**
- Outbound HTTPS (443) for package downloads (initial setup only)
- No inbound connections required
- No privileged capabilities needed at runtime

**File System:**
- No access to `/proc/sys` required
- No raw device access needed
- No IPC or shared memory usage

### Pre-Deployment Validation

Run this validation script before deployment:

```bash
#!/bin/bash
# Pre-deployment validation script

set -e

echo "=== ggen v5.0.2 Pre-Deployment Validation ==="
echo ""

# Check OS
if [ -f /etc/os-release ]; then
    source /etc/os-release
    echo "✓ OS: $PRETTY_NAME"
else
    echo "✗ Cannot detect OS version"
    exit 1
fi

# Check architecture
ARCH=$(uname -m)
if [ "$ARCH" = "x86_64" ]; then
    echo "✓ Architecture: $ARCH"
else
    echo "✗ Unsupported architecture: $ARCH (requires x86_64)"
    exit 1
fi

# Check glibc version
GLIBC_VERSION=$(ldd --version | head -1 | awk '{print $NF}')
echo "✓ glibc version: $GLIBC_VERSION"

# Check disk space
AVAILABLE=$(df -h /usr | tail -1 | awk '{print $4}')
echo "✓ Available disk space: $AVAILABLE"

# Check for required tools
for cmd in dpkg curl wget; do
    if command -v $cmd &> /dev/null; then
        echo "✓ $cmd: available"
    else
        echo "✗ $cmd: not found (install with: apt-get install $cmd)"
        exit 1
    fi
done

# Check sudo access
if sudo -n true 2>/dev/null; then
    echo "✓ sudo access: available"
else
    echo "⚠ sudo access: requires password (will prompt during installation)"
fi

echo ""
echo "=== Pre-deployment validation complete ==="
echo "System is ready for ggen v5.0.2 deployment"
```

### Dependencies Checklist

- [ ] Operating system meets minimum requirements (Debian 11+/Ubuntu 20.04+)
- [ ] Architecture is x86_64 (amd64)
- [ ] glibc 2.31+ installed
- [ ] libstdc++ 10+ installed
- [ ] At least 500MB disk space available in `/usr`
- [ ] Sudo or root access available
- [ ] Network connectivity for package download (initial setup)
- [ ] Firewall rules allow outbound HTTPS (if applicable)
- [ ] SELinux/AppArmor policies reviewed (if applicable)

### Deployment Decision Matrix

**Choose your deployment method:**

| Method | Use Case | Complexity | Security | Prerequisites |
|--------|----------|------------|----------|---------------|
| **Direct DEB** | Development, testing | Low | Standard | Debian-based OS |
| **Docker + gVisor** | Isolated execution | Medium | High | Docker, gVisor |
| **Kubernetes + gVisor** | Production, scale | High | Very High | K8s cluster, gVisor runtime |
| **OCI Bundle** | Custom runtimes | Medium | High | containerd, runsc |

---

## Installation & Deployment Guide

### Method 1: Direct DEB Installation (Recommended for Development)

**Step 1: Download the DEB Package**

```bash
# Option A: From GitHub releases
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb

# Option B: From local build
# Use pre-built package from: /home/user/ggen/releases/v5.0.2/ggen_5.0.2_amd64.deb
```

**Step 2: Verify Package Integrity**

```bash
# Check SHA256 checksum
sha256sum ggen_5.0.2_amd64.deb
# Compare with published checksum from GitHub releases

# Inspect package contents
dpkg-deb --info ggen_5.0.2_amd64.deb
dpkg-deb --contents ggen_5.0.2_amd64.deb
```

**Step 3: Install the Package**

```bash
sudo dpkg -i ggen_5.0.2_amd64.deb

# If dependencies are missing, fix with:
sudo apt-get install -f
```

**Step 4: Verify Installation**

```bash
# Check binary location
which ggen
# Expected: /usr/bin/ggen

# Verify version
ggen --version
# Expected: ggen 5.0.2

# Test basic functionality
ggen --help
ggen sync --help

# Test dry-run
ggen sync --dry-run
```

**Step 5: Post-Installation Configuration (Optional)**

```bash
# Add shell completion (optional)
ggen completion bash | sudo tee /etc/bash_completion.d/ggen
# Or for zsh:
ggen completion zsh | sudo tee /usr/share/zsh/site-functions/_ggen

# Create default config directory
mkdir -p ~/.config/ggen
```

### Method 2: Docker + gVisor Deployment

**Prerequisites:**
- Docker 20.10+ installed
- gVisor (runsc) configured as Docker runtime

**Step 1: Install gVisor Runtime**

```bash
# Download and install runsc
curl -fsSL https://gvisor.dev/archive.key | sudo gpg --dearmor -o /usr/share/keyrings/gvisor-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/gvisor-archive-keyring.gpg] https://storage.googleapis.com/gvisor/releases release main" | sudo tee /etc/apt/sources.list.d/gvisor.list > /dev/null
sudo apt-get update && sudo apt-get install -y runsc

# Configure Docker to use gVisor
sudo runsc install
sudo systemctl reload docker
```

**Step 2: Build Docker Image**

```bash
cd /path/to/ggen

# Build image with gVisor support
docker build -f Dockerfile.gvisor -t ggen:5.0.2-gvisor .

# Verify image
docker images | grep ggen
```

**Step 3: Run with gVisor Runtime**

```bash
# Run with gVisor runtime
docker run --rm \
  --runtime=runsc \
  -v $(pwd):/workspace \
  ggen:5.0.2-gvisor \
  sync --dry-run

# Run with gVisor runtime using docker-compose
docker-compose -f docker-compose.gvisor.yml up ggen
```

**Step 4: Verify gVisor Isolation**

```bash
# Check that gVisor is active
docker run --runtime=runsc ggen:5.0.2-gvisor sh -c "dmesg | head -5"
# Expected output should include gVisor-specific messages:
#   [   0.000000] Starting gVisor...
#   [   0.445958] Forking spaghetti code...
```

### Method 3: Kubernetes + gVisor Runtime Class

**Prerequisites:**
- Kubernetes cluster 1.20+
- gVisor RuntimeClass configured

**Step 1: Install gVisor RuntimeClass**

```yaml
# gvisor-runtimeclass.yaml
apiVersion: node.k8s.io/v1
kind: RuntimeClass
metadata:
  name: gvisor
handler: runsc
---
apiVersion: v1
kind: Pod
metadata:
  name: ggen-runner
  namespace: default
spec:
  runtimeClassName: gvisor
  containers:
  - name: ggen
    image: debian:bookworm-slim
    command: ["/bin/sh"]
    args:
    - -c
    - |
      # Install ggen
      dpkg -i /tmp/ggen_5.0.2_amd64.deb
      ggen --version
      ggen sync --help
    volumeMounts:
    - name: ggen-package
      mountPath: /tmp
      readOnly: true
    - name: workspace
      mountPath: /workspace
  volumes:
  - name: ggen-package
    hostPath:
      path: /path/to/ggen_5.0.2_amd64.deb
      type: File
  - name: workspace
    emptyDir: {}
```

**Step 2: Deploy to Kubernetes**

```bash
kubectl apply -f gvisor-runtimeclass.yaml
kubectl logs ggen-runner -f
```

**Step 3: Production Deployment with Persistent Storage**

```yaml
# ggen-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ggen-worker
  namespace: production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: ggen-worker
  template:
    metadata:
      labels:
        app: ggen-worker
    spec:
      runtimeClassName: gvisor
      initContainers:
      - name: install-ggen
        image: debian:bookworm-slim
        command: ["dpkg", "-i", "/packages/ggen_5.0.2_amd64.deb"]
        volumeMounts:
        - name: ggen-package
          mountPath: /packages
      containers:
      - name: ggen
        image: debian:bookworm-slim
        command: ["ggen"]
        args: ["sync", "--watch", "--verbose"]
        volumeMounts:
        - name: workspace
          mountPath: /workspace
        resources:
          requests:
            memory: "256Mi"
            cpu: "200m"
          limits:
            memory: "512Mi"
            cpu: "500m"
      volumes:
      - name: ggen-package
        configMap:
          name: ggen-package
      - name: workspace
        persistentVolumeClaim:
          claimName: ggen-workspace
```

### Method 4: Full DEB + gVisor Pipeline (8-Phase Build)

This method runs the complete build-test-publish pipeline with all validation gates.

**Step 1: Prepare Environment**

```bash
cd /home/user/ggen

# Ensure clean state
git status
# Should be clean, or stash changes

# Verify prerequisites
cargo --version
rustc --version
dpkg --version
```

**Step 2: Run Complete Pipeline**

```bash
./scripts/deb-gvisor-pipeline.sh
```

**Pipeline Phases (Automated):**

1. **Phase 1: Compile-Time Validation** (fail-fast)
   - RUSTFLAGS="-D warnings" enforced
   - Zero warnings policy
   - ~80s duration

2. **Phase 2: Build Release Binary**
   - Release optimizations enabled
   - Binary size: ~16MB
   - ~2s duration

3. **Phase 3: Unit Tests** (fail-fast)
   - All workspace tests
   - 100% pass rate required
   - ~2s duration

4. **Phase 4: Create Debian Package**
   - DEB package creation
   - MD5 checksums generated
   - Package size: ~4.1MB
   - <1s duration

5. **Phase 5: Validate Package Structure**
   - Format verification
   - Metadata validation
   - <1s duration

6. **Phase 6: Test Installation**
   - Simulated installation
   - CLI functionality test
   - Subcommand verification
   - <1s duration

7. **Phase 7: gVisor Compliance**
   - Dependency analysis
   - Binary format verification
   - System interface checks
   - <1s duration

8. **Phase 8: Generate Final Report**
   - Comprehensive report
   - Artifact locations
   - Deployment instructions
   - <1s duration

**Step 3: Verify Pipeline Results**

```bash
# Check pipeline log
cat /home/user/ggen/DEB_GVISOR_PIPELINE.log

# Read final report
cat /home/user/ggen/DEB_GVISOR_REPORT.md

# Verify artifacts
ls -lh /home/user/ggen/releases/v5.0.2/
# Expected files:
#   - ggen-5.0.2-x86_64-linux (16MB binary)
#   - ggen-5.0.2-x86_64-linux-gnu.tar.gz (5.4MB tarball)
#   - ggen_5.0.2_amd64.deb (4.1MB DEB package)
#   - SHA256 checksums
```

**Step 4: Deploy Artifacts**

```bash
# Install locally
sudo dpkg -i /home/user/ggen/releases/v5.0.2/ggen_5.0.2_amd64.deb

# Or upload to artifact repository
# (Example: Artifactory, Nexus, S3)
aws s3 cp /home/user/ggen/releases/v5.0.2/ggen_5.0.2_amd64.deb \
  s3://your-bucket/releases/v5.0.2/

# Or publish to APT repository
# (See APT repository setup documentation)
```

---

## Configuration Reference

### Environment Variables

| Variable | Default | Description | Example |
|----------|---------|-------------|---------|
| `GGEN_HOME` | `~/.config/ggen` | Configuration directory | `/etc/ggen` |
| `GGEN_LOG_LEVEL` | `info` | Logging level | `debug`, `info`, `warn`, `error` |
| `GGEN_CACHE_DIR` | `~/.cache/ggen` | Cache directory | `/var/cache/ggen` |
| `GGEN_TIMEOUT` | `120` | Command timeout (seconds) | `300` |
| `RUSTFLAGS` | (none) | Rust compiler flags | `"-D warnings"` |
| `RUST_LOG` | (none) | Rust logging filter | `ggen=debug` |

### ggen.toml Configuration

**Basic Configuration:**

```toml
[project]
name = "my-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"
imports = ["common.ttl"]

[ontology.prefixes]
ex = "http://example.com/ontology#"

[[generation.rules]]
name = "generate-code"
query = { file = "queries/select.rq" }
template = { file = "templates/code.tmpl" }
output_file = "src/generated.rs"
mode = "Overwrite"

[validation]
validate_syntax = true
no_unsafe = true
```

**Advanced Configuration:**

```toml
[project]
name = "advanced-project"
version = "2.0.0"
authors = ["Team <team@example.com>"]
description = "Advanced code generation project"

[ontology]
source = "ontology/main.ttl"
imports = [
  "ontology/common.ttl",
  "ontology/domain.ttl"
]

[ontology.prefixes]
ex = "http://example.com/ontology#"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

# Phase 1: Enrichment via CONSTRUCT queries
[[inference.rules]]
name = "derive-properties"
description = "Enrich graph with derived properties"
construct = """
CONSTRUCT {
  ?entity ex:fullName ?name .
}
WHERE {
  ?entity ex:firstName ?first ;
          ex:lastName ?last .
  BIND(CONCAT(?first, " ", ?last) AS ?name)
}
"""

# Phase 2: Generation via SELECT + Templates
[[generation.rules]]
name = "generate-api"
query = { file = "queries/api.rq" }
template = { file = "templates/api.rs.tmpl" }
output_file = "src/api/generated.rs"
mode = "Overwrite"
when = "always"

[[generation.rules]]
name = "generate-tests"
query = { inline = """
PREFIX ex: <http://example.com/ontology#>
SELECT ?name ?type
WHERE {
  ?entity a ex:TestCase ;
          ex:name ?name ;
          ex:type ?type .
}
""" }
template = { file = "templates/test.rs.tmpl" }
output_file = "tests/generated_tests.rs"
mode = "Append"
when = "if_missing"

[validation]
validate_syntax = true
no_unsafe = true
enforce_types = true

[cache]
enabled = true
directory = ".ggen/cache"
ttl_seconds = 3600

[logging]
level = "info"
format = "json"
output = "logs/ggen.log"
```

### gVisor Runtime Configuration

**containerd config.toml:**

```toml
version = 2

[plugins."io.containerd.grpc.v1.cri"]
  [plugins."io.containerd.grpc.v1.cri".containerd]
    [plugins."io.containerd.grpc.v1.cri".containerd.runtimes]
      [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc]
        runtime_type = "io.containerd.runsc.v1"
        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.runsc.options]
          TypeUrl = "io.containerd.runsc.v1.options"
          ConfigPath = "/etc/containerd/runsc.toml"
```

**runsc.toml:**

```toml
# gVisor runtime configuration
[runsc]
  platform = "ptrace"
  network = "host"
  file-access = "exclusive"
  overlay = true
```

### Docker Daemon Configuration

**/etc/docker/daemon.json:**

```json
{
  "runtimes": {
    "runsc": {
      "path": "/usr/local/bin/runsc",
      "runtimeArgs": [
        "--platform=ptrace",
        "--network=host"
      ]
    }
  },
  "default-runtime": "runc",
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "10m",
    "max-file": "3"
  }
}
```

---

## Operational Procedures

### Standard Operating Procedures (SOPs)

#### SOP-001: Daily Health Check

**Frequency**: Daily
**Duration**: 5 minutes
**Owner**: SRE Team

**Procedure:**

```bash
#!/bin/bash
# Daily health check script

echo "=== ggen v5.0.2 Daily Health Check ==="
date

# 1. Verify ggen binary
echo "1. Binary check..."
ggen --version || { echo "CRITICAL: ggen binary not found"; exit 1; }

# 2. Check disk space
echo "2. Disk space check..."
USAGE=$(df -h /usr | tail -1 | awk '{print $5}' | sed 's/%//')
if [ $USAGE -gt 80 ]; then
    echo "WARNING: Disk usage at ${USAGE}%"
fi

# 3. Test basic functionality
echo "3. Functionality check..."
ggen sync --help > /dev/null || { echo "ERROR: ggen sync command failed"; exit 1; }

# 4. Check logs for errors (last 24h)
echo "4. Log check..."
if [ -f /var/log/ggen.log ]; then
    ERROR_COUNT=$(journalctl -u ggen --since "24 hours ago" | grep -c ERROR)
    echo "Errors in last 24h: $ERROR_COUNT"
fi

# 5. gVisor runtime check (if applicable)
echo "5. gVisor check..."
if command -v runsc &> /dev/null; then
    runsc --version || echo "WARNING: runsc not functioning"
fi

echo ""
echo "=== Health check complete ==="
```

#### SOP-002: Pipeline Execution

**Frequency**: On-demand / CI/CD
**Duration**: 90 seconds
**Owner**: DevOps Team

**Procedure:**

1. **Pre-Execution Checks:**
   ```bash
   # Verify clean git state
   git status
   # Expected: working tree clean

   # Check system resources
   free -h
   df -h
   ```

2. **Execute Pipeline:**
   ```bash
   cd /home/user/ggen
   ./scripts/deb-gvisor-pipeline.sh 2>&1 | tee pipeline-$(date +%Y%m%d-%H%M%S).log
   ```

3. **Monitor Execution:**
   - Watch for Andon signals (RED = stop, YELLOW = investigate, GREEN = proceed)
   - Track phase completion times
   - Monitor resource usage

4. **Post-Execution Validation:**
   ```bash
   # Check pipeline log
   tail -50 DEB_GVISOR_PIPELINE.log

   # Verify artifacts
   ls -lh releases/v5.0.2/

   # Read final report
   cat DEB_GVISOR_REPORT.md
   ```

5. **Artifact Publishing:**
   ```bash
   # Upload to artifact repository
   # (Example commands - adjust for your repository)

   # Option 1: S3
   aws s3 sync releases/v5.0.2/ s3://artifacts/ggen/v5.0.2/

   # Option 2: Artifactory
   curl -u user:token -T releases/v5.0.2/ggen_5.0.2_amd64.deb \
     "https://artifactory.example.com/debian-local/pool/ggen_5.0.2_amd64.deb"
   ```

#### SOP-003: Backup and Recovery

**Frequency**: Weekly
**Duration**: 10 minutes
**Owner**: SRE Team

**Backup Procedure:**

```bash
#!/bin/bash
# Backup script

BACKUP_DIR="/backups/ggen/$(date +%Y%m%d)"
mkdir -p "$BACKUP_DIR"

# 1. Backup configuration
echo "Backing up configuration..."
tar -czf "$BACKUP_DIR/config.tar.gz" \
  /etc/ggen/ \
  ~/.config/ggen/ \
  2>/dev/null || true

# 2. Backup artifacts
echo "Backing up artifacts..."
cp -r /home/user/ggen/releases/v5.0.2/ "$BACKUP_DIR/artifacts/"

# 3. Backup logs
echo "Backing up logs..."
tar -czf "$BACKUP_DIR/logs.tar.gz" \
  /var/log/ggen* \
  /home/user/ggen/*.log \
  2>/dev/null || true

# 4. Create manifest
echo "Creating backup manifest..."
cat > "$BACKUP_DIR/manifest.txt" << EOF
Backup Date: $(date)
ggen Version: $(ggen --version)
Hostname: $(hostname)
Files:
$(ls -lh "$BACKUP_DIR")
EOF

echo "Backup complete: $BACKUP_DIR"
```

**Recovery Procedure:**

```bash
#!/bin/bash
# Recovery script

BACKUP_DIR="/backups/ggen/20260105"  # Adjust date

# 1. Restore configuration
echo "Restoring configuration..."
tar -xzf "$BACKUP_DIR/config.tar.gz" -C /

# 2. Restore artifacts
echo "Restoring artifacts..."
cp -r "$BACKUP_DIR/artifacts/" /home/user/ggen/releases/

# 3. Reinstall from backup
echo "Reinstalling ggen..."
sudo dpkg -i "$BACKUP_DIR/artifacts/ggen_5.0.2_amd64.deb"

# 4. Verify
echo "Verifying installation..."
ggen --version
ggen sync --help

echo "Recovery complete"
```

### Monitoring Procedures

#### Collect Metrics

```bash
#!/bin/bash
# Metrics collection script

# Run every 5 minutes via cron:
# */5 * * * * /usr/local/bin/collect-ggen-metrics.sh

METRICS_FILE="/var/lib/ggen/metrics.json"
mkdir -p "$(dirname "$METRICS_FILE")"

# Collect metrics
cat > "$METRICS_FILE" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "version": "$(ggen --version | awk '{print $2}')",
  "uptime": "$(uptime -p)",
  "disk_usage": {
    "total": "$(df -h /usr | tail -1 | awk '{print $2}')",
    "used": "$(df -h /usr | tail -1 | awk '{print $3}')",
    "available": "$(df -h /usr | tail -1 | awk '{print $4}')",
    "percent": "$(df -h /usr | tail -1 | awk '{print $5}')"
  },
  "memory_usage": {
    "total": "$(free -h | awk '/^Mem:/ {print $2}')",
    "used": "$(free -h | awk '/^Mem:/ {print $3}')",
    "free": "$(free -h | awk '/^Mem:/ {print $4}')"
  },
  "process_count": $(pgrep -c ggen || echo 0),
  "error_count_24h": $(journalctl -u ggen --since "24 hours ago" 2>/dev/null | grep -c ERROR || echo 0)
}
EOF

echo "Metrics collected: $METRICS_FILE"
```

#### Log Collection

```bash
#!/bin/bash
# Centralized log collection

# Ship logs to centralized logging system
# (Example: Elasticsearch, Splunk, CloudWatch)

LOG_FILES=(
  "/var/log/ggen.log"
  "/home/user/ggen/DEB_GVISOR_PIPELINE.log"
  "/var/log/syslog"
)

for log in "${LOG_FILES[@]}"; do
  if [ -f "$log" ]; then
    # Example: Ship to Elasticsearch
    # filebeat -c /etc/filebeat/filebeat.yml

    # Example: Ship to CloudWatch
    # aws logs put-log-events \
    #   --log-group-name /ggen/production \
    #   --log-stream-name $(hostname) \
    #   --log-events file://$log

    echo "Shipped log: $log"
  fi
done
```

---

## Runbooks

### Runbook 1: Partial Pipeline Re-run

**Scenario**: Pipeline failed at Phase 5 (Package Validation), need to re-run from that point.

**Impact**: Medium
**Urgency**: Medium
**Estimated Time**: 10 minutes

**Steps:**

1. **Identify failure point:**
   ```bash
   tail -100 /home/user/ggen/DEB_GVISOR_PIPELINE.log
   # Look for "Phase X: FAILED" message
   ```

2. **Determine safe restart point:**
   - If failed at Phase 1-2: Re-run full pipeline
   - If failed at Phase 3-4: Restart from Phase 2 (build)
   - If failed at Phase 5-8: Restart from Phase 4 (packaging)

3. **Manual phase execution (if pipeline doesn't support partial re-run):**

   **From Phase 4 onwards:**
   ```bash
   cd /home/user/ggen

   # Phase 4: Create Debian Package
   DEB_BUILD_DIR="/tmp/ggen-deb-build"
   RELEASE_DIR="releases/v5.0.2"

   rm -rf "$DEB_BUILD_DIR"
   mkdir -p "$DEB_BUILD_DIR/DEBIAN"
   mkdir -p "$DEB_BUILD_DIR/usr/bin"
   mkdir -p "$DEB_BUILD_DIR/usr/share/doc/ggen"

   cp target/release/ggen "$DEB_BUILD_DIR/usr/bin/ggen"
   chmod +x "$DEB_BUILD_DIR/usr/bin/ggen"

   # Create control file (see pipeline script for full content)
   # ... create DEBIAN/control, postinst, copyright

   # Build package
   dpkg-deb --build "$DEB_BUILD_DIR" "$RELEASE_DIR/ggen_5.0.2_amd64.deb"
   ```

4. **Continue from Phase 5:**
   ```bash
   # Validate package
   dpkg-deb -I "$RELEASE_DIR/ggen_5.0.2_amd64.deb"
   dpkg-deb -c "$RELEASE_DIR/ggen_5.0.2_amd64.deb"

   # Test installation
   TEST_DIR="/tmp/gvisor-test-env"
   dpkg -x "$RELEASE_DIR/ggen_5.0.2_amd64.deb" "$TEST_DIR"
   "$TEST_DIR/usr/bin/ggen" --help
   ```

5. **Verify completion:**
   ```bash
   ls -lh "$RELEASE_DIR/"
   # Should show: ggen_5.0.2_amd64.deb
   ```

**Rollback**: If re-run fails, restore from previous successful build artifacts.

**Prevention**: Ensure all prerequisites are met before running pipeline (see Pre-Deployment Checklist).

---

### Runbook 2: Emergency Rollback

**Scenario**: New deployment is causing production issues, need immediate rollback.

**Impact**: Critical
**Urgency**: High
**Estimated Time**: 5 minutes

**Steps:**

1. **Identify current version:**
   ```bash
   ggen --version
   # Note current version (e.g., 5.0.2)
   ```

2. **Stop current deployment:**
   ```bash
   # Kubernetes
   kubectl scale deployment ggen-worker --replicas=0 -n production

   # Docker
   docker-compose -f docker-compose.gvisor.yml down

   # Systemd
   sudo systemctl stop ggen
   ```

3. **Restore previous version:**
   ```bash
   # Option A: From backup
   sudo dpkg -i /backups/ggen/20260104/artifacts/ggen_5.0.1_amd64.deb

   # Option B: From artifact repository
   wget https://artifacts.example.com/ggen/v5.0.1/ggen_5.0.1_amd64.deb
   sudo dpkg -i ggen_5.0.1_amd64.deb
   ```

4. **Verify rollback:**
   ```bash
   ggen --version
   # Should show: ggen 5.0.1 (previous version)

   ggen sync --help
   # Test basic functionality
   ```

5. **Restart services:**
   ```bash
   # Kubernetes
   kubectl scale deployment ggen-worker --replicas=3 -n production

   # Docker
   docker-compose -f docker-compose.gvisor.yml up -d

   # Systemd
   sudo systemctl start ggen
   ```

6. **Monitor for stability:**
   ```bash
   # Watch logs
   journalctl -u ggen -f

   # Check metrics
   curl http://localhost:9090/metrics
   ```

7. **Post-rollback actions:**
   - Document rollback reason
   - Create incident report
   - Identify root cause of v5.0.2 issues
   - Fix issues before re-attempting deployment

**Rollback**: N/A (this IS the rollback)

**Prevention**:
- Always test in staging environment first
- Use canary deployments (10% → 50% → 100%)
- Implement automated health checks
- Maintain recent backups

---

### Runbook 3: Debugging Pipeline Failures

**Scenario**: Pipeline fails with unclear error message.

**Impact**: Medium
**Urgency**: Low
**Estimated Time**: 30 minutes

**Diagnostic Steps:**

1. **Collect failure information:**
   ```bash
   # Full pipeline log
   cat /home/user/ggen/DEB_GVISOR_PIPELINE.log

   # Last 100 lines
   tail -100 /home/user/ggen/DEB_GVISOR_PIPELINE.log

   # Search for errors
   grep -i "error\|fail\|critical" /home/user/ggen/DEB_GVISOR_PIPELINE.log
   ```

2. **Identify phase and failure mode:**

   **Phase 1 (Compile-Time) Failures:**
   ```bash
   # Check Rust version
   rustc --version
   cargo --version

   # Try compilation manually
   cd /home/user/ggen
   RUSTFLAGS="-D warnings" cargo check 2>&1 | tee check.log

   # Common issues:
   # - Missing Rust toolchain
   # - Compilation warnings treated as errors
   # - Dependency resolution failures
   ```

   **Phase 2 (Build) Failures:**
   ```bash
   # Try release build manually
   cargo build --release -p ggen-cli-lib --bin ggen 2>&1 | tee build.log

   # Check binary
   ls -lh target/release/ggen
   file target/release/ggen

   # Common issues:
   # - Linker errors
   # - Missing system libraries
   # - Out of disk space
   ```

   **Phase 3 (Unit Tests) Failures:**
   ```bash
   # Run tests with verbose output
   cargo test --lib --verbose 2>&1 | tee test.log

   # Run specific failing test
   cargo test --lib test_name -- --nocapture

   # Common issues:
   # - Test logic errors
   # - Environment-specific failures
   # - Resource constraints
   ```

   **Phase 4-8 (Packaging/Validation) Failures:**
   ```bash
   # Check package structure
   dpkg-deb -c releases/v5.0.2/ggen_5.0.2_amd64.deb

   # Validate manually
   dpkg-deb --info releases/v5.0.2/ggen_5.0.2_amd64.deb

   # Common issues:
   # - Invalid control file
   # - Missing files
   # - Permission issues
   ```

3. **Enable debug logging:**
   ```bash
   # Edit pipeline script to add set -x
   sed -i '19a set -x' scripts/deb-gvisor-pipeline.sh

   # Re-run with debug output
   ./scripts/deb-gvisor-pipeline.sh 2>&1 | tee debug.log
   ```

4. **Check system resources:**
   ```bash
   # Disk space
   df -h

   # Memory
   free -h

   # CPU load
   uptime

   # Processes
   ps aux | grep ggen
   ```

5. **Review recent changes:**
   ```bash
   # Git history
   git log --oneline -10

   # Recent file changes
   git diff HEAD~5..HEAD

   # Uncommitted changes
   git status
   git diff
   ```

**Resolution:**

Based on diagnostics, apply appropriate fix:
- **Compilation errors**: Fix code, re-run check
- **Build errors**: Install missing dependencies
- **Test failures**: Fix test logic or environment
- **Packaging errors**: Fix package structure

**Escalation**: If unable to resolve within 1 hour, escalate to development team with full diagnostic output.

---

### Runbook 4: gVisor Runtime Failures

**Scenario**: Container fails to start with gVisor runtime, or exhibits unexpected behavior.

**Impact**: High
**Urgency**: High
**Estimated Time**: 20 minutes

**Diagnostic Steps:**

1. **Verify gVisor installation:**
   ```bash
   # Check runsc binary
   which runsc
   # Expected: /usr/local/bin/runsc

   # Check version
   runsc --version
   # Expected: runsc version 20230828+

   # Check runtime configuration
   cat /etc/docker/daemon.json | jq .runtimes.runsc
   ```

2. **Test gVisor runtime:**
   ```bash
   # Simple test container
   docker run --rm --runtime=runsc alpine echo "gVisor test"

   # Check dmesg for gVisor messages
   docker run --rm --runtime=runsc alpine dmesg | head -5
   # Expected: gVisor-specific boot messages
   ```

3. **Check containerd/Docker logs:**
   ```bash
   # Docker logs
   journalctl -u docker -n 100 --no-pager

   # containerd logs
   journalctl -u containerd -n 100 --no-pager

   # runsc logs
   ls -lh /tmp/runsc-logs/
   cat /tmp/runsc-logs/*.log
   ```

4. **Common gVisor issues and fixes:**

   **Issue: "runtime not found"**
   ```bash
   # Re-install runsc runtime
   sudo runsc install
   sudo systemctl reload docker

   # Verify
   docker info | grep -A5 Runtimes
   ```

   **Issue: "permission denied"**
   ```bash
   # Check runsc permissions
   ls -l /usr/local/bin/runsc
   # Should be: -rwxr-xr-x

   # Fix if needed
   sudo chmod +x /usr/local/bin/runsc
   ```

   **Issue: "platform not supported"**
   ```bash
   # Check available platforms
   runsc platforms

   # Try different platform
   docker run --rm --runtime=runsc \
     --runtime-arg --platform=ptrace \
     alpine echo "test"
   ```

   **Issue: "OCI bundle validation failed"**
   ```bash
   # Verify bundle structure
   tree /path/to/bundle
   # Required:
   # ├── config.json
   # └── rootfs/

   # Validate config.json
   cat /path/to/bundle/config.json | jq .

   # Check for common issues:
   # - Missing process.terminal field
   # - Invalid path references
   # - Missing required fields
   ```

5. **Enable debug logging:**
   ```bash
   # Run with debug logs
   docker run --rm \
     --runtime=runsc \
     --runtime-arg --debug \
     --runtime-arg --debug-log=/tmp/runsc.log \
     alpine echo "debug test"

   # Review logs
   cat /tmp/runsc.log
   ```

**Resolution:**

- **Runtime not found**: Reinstall gVisor, reload Docker
- **Permission errors**: Fix file permissions
- **Platform errors**: Use compatible platform (ptrace, kvm)
- **OCI bundle errors**: Fix bundle structure

**Fallback**: If gVisor continues to fail, fallback to standard runc runtime:
```bash
docker run --rm --runtime=runc ggen:5.0.2 sync --help
```

**Escalation**: If gVisor-specific issues persist, consult [gVisor documentation](https://gvisor.dev/docs/) or file issue.

---

### Runbook 5: Performance Degradation

**Scenario**: ggen operations are slower than expected (SLO violations).

**Impact**: Medium
**Urgency**: Medium
**Estimated Time**: 45 minutes

**Diagnostic Steps:**

1. **Identify SLO violations:**
   ```bash
   # Measure current performance
   time ggen sync --dry-run
   # Expected: <5s

   # Benchmark different operations
   hyperfine 'ggen sync --dry-run' --warmup 3
   ```

2. **Check system resources:**
   ```bash
   # CPU usage
   top -bn1 | head -20

   # Memory usage
   free -h
   vmstat 1 5

   # Disk I/O
   iostat -x 1 5

   # Network (if applicable)
   iftop -i eth0 -t -s 10
   ```

3. **Profile ggen execution:**
   ```bash
   # Enable Rust backtrace
   RUST_BACKTRACE=1 ggen sync --verbose

   # Enable debug logging
   RUST_LOG=debug ggen sync 2>&1 | tee debug.log

   # Use perf (Linux)
   perf record -g ggen sync
   perf report
   ```

4. **Check for bottlenecks:**

   **Disk I/O:**
   ```bash
   # Check cache directory
   du -sh ~/.cache/ggen

   # Clear cache if too large
   rm -rf ~/.cache/ggen/*
   ```

   **Network:**
   ```bash
   # Check external dependency downloads
   tcpdump -i any -nn host example.com

   # Use local mirrors if applicable
   ```

   **CPU:**
   ```bash
   # Check parallel execution
   cargo make test -- --test-threads=1
   # vs
   cargo make test -- --test-threads=4
   ```

5. **Compare with baseline:**
   ```bash
   # Review SLO targets (from Makefile.toml)
   grep "SLO:" /home/user/ggen/Makefile.toml

   # Expected performance:
   # - cargo make check: <5s
   # - cargo make test: <30s
   # - cargo make lint: <60s
   # - Full pipeline: ~90s
   ```

**Optimization Steps:**

1. **Clear caches:**
   ```bash
   rm -rf ~/.cache/ggen
   rm -rf /home/user/ggen/target
   cargo clean
   ```

2. **Optimize build:**
   ```bash
   # Use faster linker
   export RUSTFLAGS="-C link-arg=-fuse-ld=lld"

   # Enable incremental compilation
   export CARGO_INCREMENTAL=1
   ```

3. **Reduce test suite:**
   ```bash
   # Run only fast tests
   cargo test --lib --no-default-features

   # Skip slow integration tests
   cargo test --workspace --exclude ggen-e2e
   ```

4. **Scale resources (if in cloud/container):**
   ```bash
   # Kubernetes: Increase resource limits
   kubectl set resources deployment ggen-worker \
     --limits=cpu=2,memory=2Gi \
     --requests=cpu=1,memory=1Gi

   # Docker: Increase CPU/memory
   docker update --cpus=2 --memory=2g ggen-container
   ```

**Prevention:**
- Monitor performance trends over time
- Set up automated performance regression tests
- Implement caching strategies
- Use performance budgets

**Escalation**: If performance degrades beyond 2x SLO, escalate to development team for profiling.

---

### Runbook 6: Artifact Corruption / Integrity Issues

**Scenario**: Downloaded or deployed artifact fails integrity checks.

**Impact**: Critical
**Urgency**: High
**Estimated Time**: 15 minutes

**Steps:**

1. **Verify artifact integrity:**
   ```bash
   # Check SHA256 checksum
   sha256sum ggen_5.0.2_amd64.deb
   # Compare with published checksum:
   # (Get from GitHub releases or artifact repository)

   # Example expected checksum:
   # 7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b
   ```

2. **Inspect package:**
   ```bash
   # Check file format
   file ggen_5.0.2_amd64.deb
   # Expected: "Debian binary package"

   # Inspect contents
   dpkg-deb --info ggen_5.0.2_amd64.deb
   dpkg-deb --contents ggen_5.0.2_amd64.deb

   # Extract and verify
   dpkg-deb -x ggen_5.0.2_amd64.deb /tmp/ggen-verify
   file /tmp/ggen-verify/usr/bin/ggen
   # Expected: "ELF 64-bit LSB executable, x86-64"
   ```

3. **If corruption detected:**

   **Re-download from trusted source:**
   ```bash
   # Remove corrupted artifact
   rm ggen_5.0.2_amd64.deb

   # Download from GitHub releases (primary source)
   wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb

   # Verify checksum again
   sha256sum ggen_5.0.2_amd64.deb
   ```

   **Or rebuild from source:**
   ```bash
   cd /home/user/ggen
   git checkout v5.0.2
   ./scripts/deb-gvisor-pipeline.sh

   # Use newly built package
   sudo dpkg -i releases/v5.0.2/ggen_5.0.2_amd64.deb
   ```

4. **Verify installation:**
   ```bash
   ggen --version
   ggen sync --help

   # Run basic test
   echo "test: sync" > /tmp/test-ggen.toml
   ggen sync --dry-run
   ```

5. **Post-incident:**
   - Document corruption source (network issue, storage failure, etc.)
   - Review download/storage procedures
   - Implement additional integrity checks
   - Update artifact repository if source was compromised

**Prevention:**
- Always verify checksums before installation
- Use HTTPS for all artifact downloads
- Implement artifact signing (GPG)
- Store multiple copies in different locations
- Automate integrity verification in CI/CD

---

### Runbook 7: Disk Space Exhaustion

**Scenario**: Build or deployment fails due to insufficient disk space.

**Impact**: High
**Urgency**: Medium
**Estimated Time**: 20 minutes

**Diagnostic Steps:**

1. **Check disk usage:**
   ```bash
   df -h
   # Identify full partitions

   # Check specific directories
   du -sh /home/user/ggen/target
   du -sh /var/cache
   du -sh ~/.cache
   du -sh /tmp
   ```

2. **Identify largest directories:**
   ```bash
   # Top 10 largest directories in /home
   du -h /home/user | sort -rh | head -10

   # Find large files (>100MB)
   find /home/user/ggen -type f -size +100M -exec ls -lh {} \;
   ```

**Cleanup Steps:**

1. **Clean build artifacts:**
   ```bash
   cd /home/user/ggen

   # Clean Cargo build cache
   cargo clean

   # Clean node_modules (if applicable)
   find . -name "node_modules" -type d -prune -exec rm -rf {} +

   # Remove old releases (keep last 3)
   cd releases
   ls -t | tail -n +4 | xargs rm -rf
   ```

2. **Clean system caches:**
   ```bash
   # APT cache
   sudo apt-get clean
   sudo apt-get autoclean

   # Journal logs
   sudo journalctl --vacuum-time=7d

   # Temp files
   sudo rm -rf /tmp/*
   sudo rm -rf /var/tmp/*
   ```

3. **Clean Docker/container artifacts (if applicable):**
   ```bash
   # Remove unused Docker images
   docker image prune -a -f

   # Remove unused volumes
   docker volume prune -f

   # Remove build cache
   docker builder prune -a -f
   ```

4. **Archive old logs:**
   ```bash
   # Compress and archive logs older than 30 days
   find /var/log/ggen* -name "*.log" -mtime +30 -exec gzip {} \;

   # Move to archive directory
   mkdir -p /backups/logs
   find /var/log -name "*.gz" -exec mv {} /backups/logs/ \;
   ```

5. **Verify space recovered:**
   ```bash
   df -h
   # Should show increased available space
   ```

**Prevention:**
- Implement automated cleanup scripts (cron jobs)
- Set up disk space monitoring and alerting
- Rotate logs automatically (logrotate)
- Use separate partitions for build artifacts
- Configure retention policies for artifacts

**Emergency Expansion (if cleanup insufficient):**
```bash
# Resize partition (if on cloud/VM)
# AWS EBS example:
aws ec2 modify-volume --volume-id vol-xxxxx --size 100

# Extend filesystem
sudo resize2fs /dev/xvda1

# Or add new volume and mount
sudo mount /dev/xvdb /mnt/build
ln -s /mnt/build /home/user/ggen/target
```

---

### Runbook 8: Handling Security Vulnerabilities

**Scenario**: Security vulnerability discovered in ggen or its dependencies.

**Impact**: Critical
**Urgency**: High
**Estimated Time**: Variable (1-4 hours)

**Steps:**

1. **Assess vulnerability:**
   ```bash
   # Run security audit
   cargo audit

   # Check for known vulnerabilities
   cargo audit --deny warnings

   # Review specific advisory
   cargo audit --advisory-id RUSTSEC-YYYY-NNNN
   ```

2. **Determine severity:**
   - **Critical**: Remote code execution, privilege escalation
   - **High**: Information disclosure, denial of service
   - **Medium**: Local vulnerabilities, configuration issues
   - **Low**: Minor issues, informational

3. **Immediate mitigation (for Critical/High):**

   **Option A: Rollback to previous version**
   ```bash
   # Identify last known good version
   git tag | tail -5

   # Rollback deployment (see Runbook 2)
   sudo dpkg -i /backups/ggen/ggen_5.0.1_amd64.deb
   ```

   **Option B: Apply hotfix**
   ```bash
   # Update vulnerable dependency
   cargo update -p vulnerable-crate --precise 1.2.3

   # Rebuild
   cargo build --release -p ggen-cli-lib --bin ggen

   # Quick validation
   cargo test --lib

   # Deploy hotfix
   sudo cp target/release/ggen /usr/bin/ggen
   ```

4. **Long-term fix:**
   ```bash
   # Create patch branch
   git checkout -b security/fix-RUSTSEC-YYYY-NNNN

   # Update dependencies
   cargo update

   # Update Cargo.toml if needed
   vim Cargo.toml

   # Verify fix
   cargo audit

   # Run full test suite
   cargo test --workspace

   # Build new package
   ./scripts/deb-gvisor-pipeline.sh

   # Create PR for review
   git commit -am "security: fix RUSTSEC-YYYY-NNNN"
   git push origin security/fix-RUSTSEC-YYYY-NNNN
   ```

5. **Deployment:**
   ```bash
   # After PR approval and merge
   git checkout main
   git pull

   # Tag new version
   git tag v5.0.3

   # Deploy updated package
   sudo dpkg -i releases/v5.0.3/ggen_5.0.3_amd64.deb
   ```

6. **Verify fix:**
   ```bash
   # Run security audit
   cargo audit
   # Should show: "Success: No vulnerable packages found"

   # Verify version
   ggen --version
   # Should show: ggen 5.0.3

   # Test functionality
   ggen sync --help
   ```

7. **Communication:**
   - Notify stakeholders of vulnerability and fix
   - Update security advisory (if public)
   - Document in CHANGELOG.md
   - Update SECURITY.md with disclosure policy

**Prevention:**
- Run `cargo audit` in CI/CD pipeline
- Subscribe to security advisories (RustSec)
- Implement automated dependency updates (Dependabot)
- Regular security reviews (weekly/monthly)
- Maintain up-to-date SECURITY.md

**Escalation**:
- **Critical vulnerabilities**: Immediate notification to security team
- **Public disclosure**: Coordinate with security team before announcing
- **Zero-day**: Follow responsible disclosure process

---

### Runbook 9: CI/CD Pipeline Integration

**Scenario**: Integrate DEB + gVisor pipeline into existing CI/CD system.

**Impact**: Medium
**Urgency**: Low
**Estimated Time**: 2-4 hours (one-time setup)

**GitHub Actions Integration:**

```yaml
# .github/workflows/deb-gvisor-pipeline.yml
name: DEB + gVisor Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]
  release:
    types: [created]

env:
  CARGO_TERM_COLOR: always
  RUSTFLAGS: "-D warnings"

jobs:
  build-test-package:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    steps:
    - name: Checkout code
      uses: actions/checkout@v3
      with:
        submodules: recursive

    - name: Install Rust toolchain
      uses: actions-rs/toolchain@v1
      with:
        toolchain: 1.91.1
        profile: minimal
        override: true

    - name: Cache Cargo registry
      uses: actions/cache@v3
      with:
        path: ~/.cargo/registry
        key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

    - name: Cache Cargo build
      uses: actions/cache@v3
      with:
        path: target
        key: ${{ runner.os }}-cargo-build-${{ hashFiles('**/Cargo.lock') }}

    - name: Install system dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y \
          build-essential \
          pkg-config \
          libssl-dev \
          libclang-dev

    - name: Run DEB + gVisor Pipeline
      run: |
        chmod +x scripts/deb-gvisor-pipeline.sh
        ./scripts/deb-gvisor-pipeline.sh

    - name: Upload artifacts
      uses: actions/upload-artifact@v3
      with:
        name: ggen-deb-package
        path: |
          releases/v5.0.2/ggen_5.0.2_amd64.deb
          releases/v5.0.2/*.tar.gz
          DEB_GVISOR_REPORT.md

    - name: Upload to release (on tag)
      if: startsWith(github.ref, 'refs/tags/')
      uses: softprops/action-gh-release@v1
      with:
        files: |
          releases/v5.0.2/ggen_5.0.2_amd64.deb
          releases/v5.0.2/*.tar.gz
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}

  security-audit:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
    - uses: actions-rs/audit-check@v1
      with:
        token: ${{ secrets.GITHUB_TOKEN }}
```

**GitLab CI Integration:**

```yaml
# .gitlab-ci.yml
stages:
  - build
  - test
  - package
  - deploy

variables:
  CARGO_HOME: ${CI_PROJECT_DIR}/.cargo
  RUSTFLAGS: "-D warnings"

cache:
  key: ${CI_COMMIT_REF_SLUG}
  paths:
    - .cargo/
    - target/

build:
  stage: build
  image: rust:1.91.1
  script:
    - cargo build --release --bin ggen
  artifacts:
    paths:
      - target/release/ggen
    expire_in: 1 day

test:
  stage: test
  image: rust:1.91.1
  script:
    - cargo test --workspace --lib

package:
  stage: package
  image: debian:bookworm
  dependencies:
    - build
  script:
    - apt-get update && apt-get install -y dpkg-dev
    - ./scripts/deb-gvisor-pipeline.sh
  artifacts:
    paths:
      - releases/v5.0.2/*.deb
      - DEB_GVISOR_REPORT.md
    expire_in: 7 days

deploy:
  stage: deploy
  image: debian:bookworm
  only:
    - tags
  script:
    - echo "Deploy to artifact repository"
    # Add your deployment commands here
```

**Jenkins Integration:**

```groovy
// Jenkinsfile
pipeline {
    agent any

    environment {
        CARGO_HOME = "${WORKSPACE}/.cargo"
        RUSTFLAGS = "-D warnings"
    }

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Build & Package') {
            steps {
                sh '''
                    chmod +x scripts/deb-gvisor-pipeline.sh
                    ./scripts/deb-gvisor-pipeline.sh
                '''
            }
        }

        stage('Archive Artifacts') {
            steps {
                archiveArtifacts artifacts: 'releases/v5.0.2/*.deb,DEB_GVISOR_REPORT.md', fingerprint: true
            }
        }

        stage('Deploy') {
            when {
                tag pattern: "v\\d+\\.\\d+\\.\\d+", comparator: "REGEXP"
            }
            steps {
                sh '''
                    # Upload to artifact repository
                    # Example: curl -u user:token -T releases/v5.0.2/ggen_5.0.2_amd64.deb https://artifactory.example.com/
                '''
            }
        }
    }

    post {
        always {
            junit 'target/test-results/*.xml'
        }
        success {
            slackSend(color: 'good', message: "Build ${env.BUILD_NUMBER} succeeded")
        }
        failure {
            slackSend(color: 'danger', message: "Build ${env.BUILD_NUMBER} failed")
        }
    }
}
```

---

### Runbook 10: Scaling for High-Volume Workloads

**Scenario**: Need to process large number of code generation tasks concurrently.

**Impact**: High
**Urgency**: Medium
**Estimated Time**: 4-8 hours (initial setup)

**Kubernetes Horizontal Pod Autoscaling:**

```yaml
# ggen-hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: ggen-worker-hpa
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: ggen-worker
  minReplicas: 3
  maxReplicas: 20
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 60
      policies:
      - type: Percent
        value: 100
        periodSeconds: 30
      - type: Pods
        value: 5
        periodSeconds: 30
      selectPolicy: Max
```

**Queue-Based Processing (RabbitMQ example):**

```yaml
# ggen-queue-worker.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ggen-queue-worker
spec:
  replicas: 5
  selector:
    matchLabels:
      app: ggen-queue-worker
  template:
    metadata:
      labels:
        app: ggen-queue-worker
    spec:
      runtimeClassName: gvisor
      containers:
      - name: worker
        image: ggen:5.0.2
        env:
        - name: RABBITMQ_URL
          valueFrom:
            secretKeyRef:
              name: rabbitmq-secret
              key: url
        - name: WORKER_CONCURRENCY
          value: "4"
        command:
        - /bin/sh
        - -c
        - |
          # Worker script (pseudo-code)
          while true; do
            # Consume message from queue
            task=$(rabbitmqadmin get queue=ggen-tasks)

            # Process with ggen
            ggen sync --config "$task"

            # Acknowledge message
            rabbitmqadmin ack "$task"
          done
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
```

**Monitoring Scaling:**

```bash
# Watch pod scaling
kubectl get hpa ggen-worker-hpa -w

# Monitor resource usage
kubectl top pods -l app=ggen-worker

# Check queue depth
kubectl exec -it rabbitmq-0 -- rabbitmqctl list_queues
```

---

## Monitoring & Alerting Setup

### Metrics to Monitor

**System Metrics:**
- CPU usage (target: <70%)
- Memory usage (target: <80%)
- Disk usage (target: <80%)
- Disk I/O (target: <500 IOPS)
- Network throughput

**Application Metrics:**
- ggen process count
- Pipeline execution time (SLO: ~90s)
- Pipeline success rate (target: >95%)
- Error rate (target: <5%)
- Command response times

**Business Metrics:**
- Deployments per day
- Artifact downloads
- Active installations

### Prometheus Configuration

**prometheus.yml:**

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

alerting:
  alertmanagers:
    - static_configs:
        - targets:
            - alertmanager:9093

rule_files:
  - "/etc/prometheus/rules/ggen-alerts.yml"

scrape_configs:
  - job_name: 'ggen'
    static_configs:
      - targets: ['localhost:9090']
        labels:
          service: 'ggen'
          version: '5.0.2'

  - job_name: 'node-exporter'
    static_configs:
      - targets: ['localhost:9100']
```

**Alert Rules (ggen-alerts.yml):**

```yaml
groups:
  - name: ggen-alerts
    interval: 30s
    rules:

    # Pipeline Failure Alert
    - alert: PipelineFailureRate
      expr: |
        rate(ggen_pipeline_failures_total[5m]) > 0.1
      for: 5m
      labels:
        severity: critical
        component: pipeline
      annotations:
        summary: "High pipeline failure rate"
        description: "Pipeline failure rate is {{ $value }} failures/sec (threshold: 0.1)"

    # Slow Pipeline Alert
    - alert: SlowPipelineExecution
      expr: |
        histogram_quantile(0.95, rate(ggen_pipeline_duration_seconds_bucket[5m])) > 120
      for: 10m
      labels:
        severity: warning
        component: pipeline
      annotations:
        summary: "Pipeline execution is slow"
        description: "95th percentile pipeline duration is {{ $value }}s (SLO: 90s)"

    # Disk Space Alert
    - alert: LowDiskSpace
      expr: |
        (node_filesystem_avail_bytes{mountpoint="/usr"} / node_filesystem_size_bytes{mountpoint="/usr"}) < 0.2
      for: 5m
      labels:
        severity: warning
        component: system
      annotations:
        summary: "Low disk space on /usr"
        description: "Disk space is {{ $value | humanizePercentage }} available (threshold: 20%)"

    # Memory Alert
    - alert: HighMemoryUsage
      expr: |
        (node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes) / node_memory_MemTotal_bytes > 0.8
      for: 5m
      labels:
        severity: warning
        component: system
      annotations:
        summary: "High memory usage"
        description: "Memory usage is {{ $value | humanizePercentage }} (threshold: 80%)"

    # gVisor Runtime Alert
    - alert: GVisorRuntimeDown
      expr: |
        up{job="gvisor"} == 0
      for: 2m
      labels:
        severity: critical
        component: gvisor
      annotations:
        summary: "gVisor runtime is down"
        description: "gVisor runtime has been down for more than 2 minutes"

    # Security Vulnerability Alert
    - alert: SecurityVulnerabilityDetected
      expr: |
        ggen_security_vulnerabilities_total > 0
      for: 1m
      labels:
        severity: critical
        component: security
      annotations:
        summary: "Security vulnerabilities detected"
        description: "{{ $value }} security vulnerabilities found in dependencies"
```

### Grafana Dashboard

**Import this JSON for a pre-built dashboard:**

```json
{
  "dashboard": {
    "title": "ggen v5.0.2 - DEB + gVisor Pipeline",
    "panels": [
      {
        "title": "Pipeline Success Rate",
        "targets": [
          {
            "expr": "rate(ggen_pipeline_success_total[5m]) / rate(ggen_pipeline_total[5m]) * 100"
          }
        ],
        "type": "graph"
      },
      {
        "title": "Pipeline Duration (p95)",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(ggen_pipeline_duration_seconds_bucket[5m]))"
          }
        ],
        "type": "graph"
      },
      {
        "title": "System Resource Usage",
        "targets": [
          {
            "expr": "100 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100)",
            "legendFormat": "Memory %"
          },
          {
            "expr": "100 - (avg(rate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100)",
            "legendFormat": "CPU %"
          },
          {
            "expr": "100 - (node_filesystem_avail_bytes{mountpoint=\"/usr\"} / node_filesystem_size_bytes{mountpoint=\"/usr\"} * 100)",
            "legendFormat": "Disk %"
          }
        ],
        "type": "graph"
      }
    ]
  }
}
```

### Logging Setup

**Centralized Logging with Fluentd:**

```yaml
# fluentd-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: fluentd-config
data:
  fluent.conf: |
    <source>
      @type tail
      path /var/log/ggen/*.log
      pos_file /var/log/fluentd-ggen.pos
      tag ggen.*
      <parse>
        @type json
        time_key timestamp
        time_format %Y-%m-%dT%H:%M:%S.%NZ
      </parse>
    </source>

    <filter ggen.**>
      @type record_transformer
      <record>
        service ggen
        version 5.0.2
        environment ${ENV}
      </record>
    </filter>

    <match ggen.**>
      @type elasticsearch
      host elasticsearch.logging.svc.cluster.local
      port 9200
      index_name ggen-${TAG}-%Y%m%d
      type_name _doc
      <buffer>
        @type file
        path /var/log/fluentd-buffers/ggen.buffer
        flush_interval 10s
      </buffer>
    </match>
```

---

## FAQ & Troubleshooting

### Frequently Asked Questions

**Q: What is the minimum system requirement for running ggen v5.0.2?**

A: Minimum requirements:
- OS: Debian 11+ or Ubuntu 20.04+
- CPU: x86_64 (amd64)
- RAM: 2GB available
- Disk: 500MB free space
- Dependencies: glibc 2.31+, libstdc++ 10+

---

**Q: Can I run ggen without gVisor?**

A: Yes! gVisor is optional. You can:
1. Install directly via DEB package (`dpkg -i ggen_5.0.2_amd64.deb`)
2. Run as a standard binary (`/usr/bin/ggen`)
3. Use Docker without gVisor runtime (`docker run --runtime=runc`)

gVisor adds enhanced security isolation but is not required for functionality.

---

**Q: How long does the full pipeline take?**

A: Approximately 90 seconds:
- Phase 1 (Compile): ~80s
- Phase 2 (Build): ~2s
- Phase 3 (Test): ~2s
- Phases 4-8 (Package/Validate): <5s

Times may vary based on system resources and cache state.

---

**Q: Can I skip specific pipeline phases?**

A: The pipeline is designed with fail-fast gates and should not skip phases. However, for development/debugging, you can run individual commands manually:

```bash
# Skip pipeline, just build
cargo build --release -p ggen-cli-lib --bin ggen

# Skip pipeline, just test
cargo test --workspace --lib
```

For production, always run the full pipeline.

---

**Q: How do I upgrade from a previous version?**

A: Simple DEB upgrade:

```bash
# Download new version
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb

# Install (will upgrade existing installation)
sudo dpkg -i ggen_5.0.2_amd64.deb

# Verify
ggen --version
```

Configuration files are preserved during upgrade.

---

**Q: Where are logs stored?**

A: Log locations:
- Pipeline logs: `/home/user/ggen/DEB_GVISOR_PIPELINE.log`
- System logs: `/var/log/syslog` (search for "ggen")
- Application logs: Configured via `GGEN_LOG_FILE` env var
- Container logs: `docker logs <container>` or `kubectl logs <pod>`

---

**Q: How do I enable debug logging?**

A: Set environment variable:

```bash
# For Rust applications
export RUST_LOG=debug
ggen sync --verbose

# For system-wide
export GGEN_LOG_LEVEL=debug
ggen sync
```

---

**Q: Can I use ggen in air-gapped environments?**

A: Yes, after initial setup:
1. Build package on internet-connected machine
2. Transfer DEB package to air-gapped environment
3. Install via `dpkg -i` (no network required)
4. All runtime dependencies are included in package

---

**Q: How do I verify gVisor is actually being used?**

A: Check dmesg output:

```bash
docker run --runtime=runsc alpine dmesg | head -5
# Expected: gVisor-specific boot messages
#   [   0.000000] Starting gVisor...
#   [   0.445958] Forking spaghetti code...
```

Or check runtime:
```bash
docker inspect <container> | grep -i runtime
# Expected: "Runtime": "runsc"
```

---

**Q: What happens if a phase fails?**

A: The pipeline uses fail-fast approach:
- RED Andon signal shown immediately
- Pipeline stops at failed phase
- Error message with recovery instructions displayed
- No subsequent phases executed
- Return code 1 (failure)

See [Runbook 1: Partial Pipeline Re-run](#runbook-1-partial-pipeline-re-run) for recovery.

---

### Common Issues and Solutions

#### Issue: "error: could not compile `ggen` (lib) due to 3 previous errors"

**Cause**: Compilation errors or warnings (RUSTFLAGS="-D warnings" treats warnings as errors)

**Solution**:
```bash
# Check specific errors
RUSTFLAGS="-D warnings" cargo check 2>&1 | grep error

# Fix errors in code, then retry
cargo make check
```

---

#### Issue: "dpkg: error processing package ggen"

**Cause**: Corrupted package or missing dependencies

**Solution**:
```bash
# Verify package integrity
sha256sum ggen_5.0.2_amd64.deb

# Install missing dependencies
sudo apt-get install -f

# Re-download if corrupted
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb
sudo dpkg -i ggen_5.0.2_amd64.deb
```

---

#### Issue: "ggen: command not found" after installation

**Cause**: Binary not in PATH or installation failed

**Solution**:
```bash
# Verify installation
dpkg -l | grep ggen

# Check binary location
which ggen
# Should be: /usr/bin/ggen

# If not found, reinstall
sudo dpkg -i ggen_5.0.2_amd64.deb

# Verify
ggen --version
```

---

#### Issue: "Permission denied" when running ggen

**Cause**: Binary lacks execute permissions

**Solution**:
```bash
# Check permissions
ls -l /usr/bin/ggen
# Should be: -rwxr-xr-x

# Fix permissions if needed
sudo chmod +x /usr/bin/ggen
```

---

#### Issue: Pipeline times out or hangs

**Cause**: Resource constraints, network issues, or lock contention

**Solution**:
```bash
# Check system resources
free -h
df -h
ps aux | grep cargo

# Kill any stuck processes
pkill -9 cargo
rm -rf /home/user/ggen/target/.rustc_info.json

# Clear locks
find /home/user/ggen/target -name "*.rmeta" -delete

# Retry
./scripts/deb-gvisor-pipeline.sh
```

---

#### Issue: "runsc: command not found"

**Cause**: gVisor not installed or not in PATH

**Solution**:
```bash
# Install gVisor
curl -fsSL https://gvisor.dev/archive.key | sudo gpg --dearmor -o /usr/share/keyrings/gvisor-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/gvisor-archive-keyring.gpg] https://storage.googleapis.com/gvisor/releases release main" | sudo tee /etc/apt/sources.list.d/gvisor.list
sudo apt-get update && sudo apt-get install -y runsc

# Verify
runsc --version
```

---

#### Issue: Docker "unknown runtime specified runsc"

**Cause**: Docker not configured for gVisor runtime

**Solution**:
```bash
# Install runsc runtime for Docker
sudo runsc install

# Reload Docker daemon
sudo systemctl reload docker

# Verify
docker info | grep -i runtime
# Should show: runsc
```

---

#### Issue: "No space left on device"

**Cause**: Disk full

**Solution**: See [Runbook 7: Disk Space Exhaustion](#runbook-7-disk-space-exhaustion)

---

### Getting Help

**Documentation:**
- Main README: `/home/user/ggen/README.md`
- gVisor Setup: `/home/user/ggen/docs/GVISOR-SETUP.md`
- Architecture: `/home/user/ggen/docs/architecture/`

**Support Channels:**
- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- GitHub Discussions: https://github.com/seanchatmangpt/ggen/discussions

**Reporting Bugs:**
When reporting issues, include:
1. ggen version (`ggen --version`)
2. OS version (`lsb_release -a`)
3. Full error message or log output
4. Steps to reproduce
5. Expected vs actual behavior

**Template:**
```
### Bug Report

**Environment:**
- ggen version: 5.0.2
- OS: Ubuntu 22.04 LTS
- Architecture: x86_64
- gVisor: Yes (runsc 20230828)

**Steps to Reproduce:**
1. Run `./scripts/deb-gvisor-pipeline.sh`
2. Observe failure at Phase 3

**Expected Behavior:**
Pipeline should complete all 8 phases successfully

**Actual Behavior:**
Pipeline fails at Phase 3 with error: "test result: FAILED"

**Logs:**
```
[attach logs here]
```

**Additional Context:**
First time running pipeline after fresh installation
```

---

## Appendix

### Glossary

- **DEB**: Debian package format (.deb files)
- **gVisor**: Application kernel for containers (enhanced isolation)
- **runsc**: gVisor runtime (OCI-compatible)
- **Poka-Yoke**: Error-proofing mechanisms (from Toyota Production System)
- **FMEA**: Failure Mode and Effects Analysis
- **RPN**: Risk Priority Number (Severity × Occurrence × Detection)
- **Andon Signal**: Visual indicator (RED/YELLOW/GREEN) for status
- **SLO**: Service Level Objective (performance target)
- **OCI**: Open Container Initiative (standardized container format)

### Additional Resources

- [ggen GitHub Repository](https://github.com/seanchatmangpt/ggen)
- [gVisor Documentation](https://gvisor.dev/docs/)
- [OCI Specification](https://github.com/opencontainers/runtime-spec)
- [Debian Packaging Guide](https://www.debian.org/doc/manuals/maint-guide/)

### Version History

| Version | Date | Changes |
|---------|------|---------|
| 5.0.2 | 2026-01-05 | Initial runbook for DEB + gVisor pipeline |

---

**End of Runbook**

For questions or feedback, please open an issue on GitHub.
