# Deployment Guide: ggen v5.0.2

**Target Audience**: DevOps, SRE, Platform Engineering Teams
**Release**: v5.0.2
**Stability**: Production-Ready
**Date**: 2026-01-04

---

## Quick Start

### Fastest Path to Production

```bash
# 1. Download package
cd releases/v5.0.2

# 2. Verify integrity
sha256sum -c ggen_5.0.2_amd64.deb.sha256

# 3. Install
sudo dpkg -i ggen_5.0.2_amd64.deb

# 4. Verify
ggen --version  # Expected: ggen 5.0.2
```

**Time to deployment**: <2 minutes

---

## Deployment Scenarios

### Scenario 1: Developer Workstations (Ubuntu/Debian)

**Objective**: Install ggen on developer machines for local code generation

**Prerequisites**:
- Ubuntu 20.04+ / Debian 11+ (or any Debian-based distro)
- `sudo` access
- Dependencies: `libc6 >= 2.31`, `libstdc++6 >= 10` (auto-installed)

**Steps**:
```bash
# 1. Download package
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb

# 2. Verify checksum
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb.sha256
sha256sum -c ggen_5.0.2_amd64.deb.sha256

# 3. Install
sudo dpkg -i ggen_5.0.2_amd64.deb

# 4. Verify installation
ggen --version
ggen sync --help
```

**Rollback**:
```bash
sudo dpkg -r ggen
```

**Automation** (Ansible):
```yaml
- name: Install ggen
  apt:
    deb: /path/to/ggen_5.0.2_amd64.deb
    state: present
```

---

### Scenario 2: Corporate APT Repository

**Objective**: Distribute ggen via internal APT repository

**Prerequisites**:
- APT repository server (reprepro, aptly, or Artifactory)
- GPG signing key

**Steps**:

#### Using reprepro:
```bash
# 1. Add to repository
reprepro -b /srv/apt includedeb stable ggen_5.0.2_amd64.deb

# 2. Update repository
reprepro -b /srv/apt export stable

# 3. Clients install via apt
# On client machines:
echo "deb https://apt.example.com stable main" | sudo tee /etc/apt/sources.list.d/company.list
sudo apt-get update
sudo apt-get install ggen
```

#### Using Artifactory:
```bash
# 1. Upload package
curl -u admin:password -X PUT \
  "https://artifactory.example.com/artifactory/debian-local/pool/ggen_5.0.2_amd64.deb" \
  -T ggen_5.0.2_amd64.deb

# 2. Clients install
sudo apt-get update
sudo apt-get install ggen
```

**Benefits**:
- Centralized version management
- Automatic dependency resolution
- Standard `apt-get update && apt-get upgrade` workflow

---

### Scenario 3: Docker Containerization

**Objective**: Run ggen in containers for CI/CD or serverless workloads

**Prerequisites**:
- Docker installed
- (Optional) gVisor runtime for security

#### Basic Docker Image:
```dockerfile
# Dockerfile.ggen
FROM debian:bookworm-slim

# Install ggen
COPY releases/v5.0.2/ggen_5.0.2_amd64.deb /tmp/ggen.deb
RUN dpkg -i /tmp/ggen.deb && rm /tmp/ggen.deb

# Verify installation
RUN ggen --version

# Set working directory
WORKDIR /workspace

# Entrypoint
ENTRYPOINT ["ggen"]
CMD ["--help"]
```

**Build and run**:
```bash
# Build image
docker build -f Dockerfile.ggen -t ggen:5.0.2 .

# Run with native runtime
docker run -v $(pwd):/workspace ggen:5.0.2 sync --dry-run

# Run with gVisor runtime (security-hardened)
docker run --runtime=runsc -v $(pwd):/workspace ggen:5.0.2 sync --dry-run
```

#### Multi-Stage Build (Smaller Image):
```dockerfile
FROM debian:bookworm-slim as installer
COPY releases/v5.0.2/ggen_5.0.2_amd64.deb /tmp/ggen.deb
RUN dpkg -i /tmp/ggen.deb

FROM debian:bookworm-slim
COPY --from=installer /usr/bin/ggen /usr/bin/ggen
COPY --from=installer /usr/share/doc/ggen /usr/share/doc/ggen
WORKDIR /workspace
ENTRYPOINT ["ggen"]
```

**CI/CD Integration** (GitLab CI):
```yaml
# .gitlab-ci.yml
generate-code:
  image: ggen:5.0.2
  script:
    - ggen sync
  artifacts:
    paths:
      - generated/
```

---

### Scenario 4: Kubernetes Deployment

**Objective**: Run ggen in Kubernetes with gVisor sandbox for multi-tenant security

**Prerequisites**:
- Kubernetes cluster (1.24+)
- gVisor RuntimeClass configured (optional, for enhanced security)
- Container registry (Docker Hub, GCR, ECR, etc.)

#### Step 1: Push Image to Registry
```bash
# Tag and push
docker tag ggen:5.0.2 registry.example.com/ggen:5.0.2
docker push registry.example.com/ggen:5.0.2
```

#### Step 2: Deploy as Job
```yaml
# ggen-job.yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: ggen-code-generation
spec:
  template:
    metadata:
      labels:
        app: ggen
    spec:
      # Optional: Use gVisor for security isolation
      runtimeClassName: gvisor
      containers:
      - name: ggen
        image: registry.example.com/ggen:5.0.2
        command: ["ggen", "sync"]
        volumeMounts:
        - name: config
          mountPath: /workspace/ggen.toml
          subPath: ggen.toml
        - name: ontology
          mountPath: /workspace/ontology.ttl
          subPath: ontology.ttl
        - name: output
          mountPath: /workspace/output
      volumes:
      - name: config
        configMap:
          name: ggen-config
      - name: ontology
        configMap:
          name: ggen-ontology
      - name: output
        persistentVolumeClaim:
          claimName: ggen-output-pvc
      restartPolicy: Never
  backoffLimit: 3
```

#### Step 3: Deploy as CronJob (Scheduled)
```yaml
# ggen-cronjob.yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: ggen-nightly-generation
spec:
  schedule: "0 2 * * *"  # 2 AM daily
  jobTemplate:
    spec:
      template:
        spec:
          runtimeClassName: gvisor
          containers:
          - name: ggen
            image: registry.example.com/ggen:5.0.2
            command: ["ggen", "sync"]
            volumeMounts:
            - name: config
              mountPath: /workspace
          volumes:
          - name: config
            configMap:
              name: ggen-config
          restartPolicy: OnFailure
```

**Deploy**:
```bash
kubectl apply -f ggen-job.yaml
kubectl logs -f job/ggen-code-generation
```

**Verify gVisor Runtime**:
```bash
kubectl describe pod <pod-name> | grep Runtime
# Expected: runtimeClassName: gvisor
```

---

### Scenario 5: CI/CD Pipeline Integration

**Objective**: Integrate ggen into automated build pipelines

#### GitHub Actions:
```yaml
# .github/workflows/generate.yml
name: Code Generation
on:
  push:
    branches: [main]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Install ggen
      run: |
        wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb
        sudo dpkg -i ggen_5.0.2_amd64.deb

    - name: Generate code
      run: ggen sync

    - name: Commit generated files
      run: |
        git config user.name "GitHub Actions"
        git config user.email "actions@github.com"
        git add generated/
        git commit -m "Auto-generate code" || echo "No changes"
        git push
```

#### GitLab CI:
```yaml
# .gitlab-ci.yml
stages:
  - generate
  - test

generate-code:
  stage: generate
  image: debian:bookworm-slim
  before_script:
    - apt-get update && apt-get install -y wget
    - wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb
    - dpkg -i ggen_5.0.2_amd64.deb
  script:
    - ggen sync
  artifacts:
    paths:
      - generated/
```

#### Jenkins:
```groovy
// Jenkinsfile
pipeline {
    agent any

    stages {
        stage('Install ggen') {
            steps {
                sh '''
                    wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb
                    sudo dpkg -i ggen_5.0.2_amd64.deb
                '''
            }
        }

        stage('Generate Code') {
            steps {
                sh 'ggen sync'
            }
        }

        stage('Archive') {
            steps {
                archiveArtifacts artifacts: 'generated/**'
            }
        }
    }
}
```

---

### Scenario 6: Serverless / Lambda Deployment

**Objective**: Run ggen in AWS Lambda for on-demand code generation

**Prerequisites**:
- AWS account
- Docker (for building Lambda container)

#### Step 1: Build Lambda-Compatible Image
```dockerfile
# Dockerfile.lambda
FROM public.ecr.aws/lambda/provided:al2

# Install ggen (extract from DEB since Lambda uses Amazon Linux)
COPY releases/v5.0.2/ggen-5.0.2-x86_64-linux /usr/local/bin/ggen
RUN chmod +x /usr/local/bin/ggen

# Lambda handler
COPY lambda-handler.sh /var/runtime/bootstrap
RUN chmod +x /var/runtime/bootstrap
```

**Handler script** (`lambda-handler.sh`):
```bash
#!/bin/bash
set -euo pipefail

while true; do
  HEADERS="$(mktemp)"
  EVENT_DATA=$(curl -sS -LD "$HEADERS" -X GET "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/next")
  REQUEST_ID=$(grep -Fi Lambda-Runtime-Aws-Request-Id "$HEADERS" | tr -d '[:space:]' | cut -d: -f2)

  # Run ggen
  cd /tmp
  echo "$EVENT_DATA" | jq -r '.config' > ggen.toml
  echo "$EVENT_DATA" | jq -r '.ontology' > ontology.ttl

  RESPONSE=$(ggen sync 2>&1 | jq -Rs '{"status": "success", "output": .}')

  curl -X POST "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/$REQUEST_ID/response" -d "$RESPONSE"
done
```

**Deploy**:
```bash
# Build and push to ECR
docker build -f Dockerfile.lambda -t ggen-lambda .
docker tag ggen-lambda:latest <account-id>.dkr.ecr.us-east-1.amazonaws.com/ggen-lambda:latest
docker push <account-id>.dkr.ecr.us-east-1.amazonaws.com/ggen-lambda:latest

# Create Lambda function
aws lambda create-function \
  --function-name ggen-code-generator \
  --package-type Image \
  --code ImageUri=<account-id>.dkr.ecr.us-east-1.amazonaws.com/ggen-lambda:latest \
  --role arn:aws:iam::<account-id>:role/lambda-execution-role \
  --timeout 300 \
  --memory-size 512
```

---

## Security Best Practices

### 1. Checksum Verification (MANDATORY)

**Always verify SHA256 checksums before installation**:

```bash
# Download checksum file
wget https://github.com/seanchatmangpt/ggen/releases/download/v5.0.2/ggen_5.0.2_amd64.deb.sha256

# Verify
sha256sum -c ggen_5.0.2_amd64.deb.sha256
# Expected: ggen_5.0.2_amd64.deb: OK
```

**If verification fails**: DO NOT INSTALL. Download fresh copy.

---

### 2. gVisor Sandbox (Recommended for Multi-Tenant)

**Why gVisor**:
- Application-level kernel (not full VM overhead)
- Syscall filtering prevents privilege escalation
- Compatible with existing Docker/Kubernetes infrastructure

**Setup Docker + gVisor**:
```bash
# Install runsc
wget https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc
sudo install -m 755 runsc /usr/local/bin/runsc

# Configure Docker
sudo tee /etc/docker/daemon.json <<EOF
{
  "runtimes": {
    "runsc": {
      "path": "/usr/local/bin/runsc"
    }
  }
}
EOF

sudo systemctl restart docker

# Test
docker run --runtime=runsc ggen:5.0.2 --version
```

**Setup Kubernetes + gVisor**:
```bash
# Install RuntimeClass
kubectl apply -f - <<EOF
apiVersion: node.k8s.io/v1
kind: RuntimeClass
metadata:
  name: gvisor
handler: runsc
EOF

# Use in pods
spec:
  runtimeClassName: gvisor
```

---

### 3. Least Privilege Execution

**Container Security**:
```dockerfile
# Run as non-root user
FROM debian:bookworm-slim
RUN useradd -m -u 1000 ggen
COPY --chown=ggen:ggen ggen_5.0.2_amd64.deb /tmp/ggen.deb
RUN dpkg -i /tmp/ggen.deb && rm /tmp/ggen.deb

USER ggen
WORKDIR /home/ggen
ENTRYPOINT ["ggen"]
```

**Kubernetes Pod Security**:
```yaml
spec:
  securityContext:
    runAsNonRoot: true
    runAsUser: 1000
    fsGroup: 1000
  containers:
  - name: ggen
    securityContext:
      allowPrivilegeEscalation: false
      capabilities:
        drop:
        - ALL
      readOnlyRootFilesystem: true
```

---

### 4. Secrets Management

**DO NOT hardcode secrets in config files**

**Kubernetes Secrets**:
```bash
# Create secret
kubectl create secret generic ggen-secrets \
  --from-literal=api-key=your-api-key

# Use in pod
env:
- name: API_KEY
  valueFrom:
    secretKeyRef:
      name: ggen-secrets
      key: api-key
```

**Docker Secrets**:
```bash
echo "your-api-key" | docker secret create ggen-api-key -

docker service create \
  --secret ggen-api-key \
  --name ggen-service \
  ggen:5.0.2
```

---

## Monitoring & Observability

### Health Checks

**Container Health Check**:
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD ggen --version || exit 1
```

**Kubernetes Liveness Probe**:
```yaml
livenessProbe:
  exec:
    command:
    - ggen
    - --version
  initialDelaySeconds: 5
  periodSeconds: 30
```

---

### Logging

**Structured Logging** (capture ggen output):

**Docker**:
```bash
docker run --log-driver=json-file --log-opt max-size=10m ggen:5.0.2
```

**Kubernetes**:
```yaml
spec:
  containers:
  - name: ggen
    args: ["sync", "--verbose"]
    # Logs automatically collected by cluster logging (EFK, Loki, etc.)
```

**Centralized Logging** (Fluentd):
```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: fluentd-config
data:
  fluent.conf: |
    <match ggen.**>
      @type elasticsearch
      host elasticsearch.default.svc.cluster.local
      port 9200
      logstash_format true
    </match>
```

---

### Metrics

**Pipeline Performance Metrics**:

Monitor these key metrics:
- **Pipeline execution time**: Target <90s
- **Success rate**: Target >99%
- **Error rate**: Target <1%
- **Resource usage**: Memory <512MB, CPU <1 core

**Prometheus Metrics** (custom exporter):
```python
# ggen-exporter.py
from prometheus_client import start_http_server, Counter, Histogram
import subprocess
import time

pipeline_duration = Histogram('ggen_pipeline_duration_seconds', 'Pipeline execution time')
pipeline_success = Counter('ggen_pipeline_success_total', 'Successful pipeline runs')
pipeline_failure = Counter('ggen_pipeline_failure_total', 'Failed pipeline runs')

@pipeline_duration.time()
def run_pipeline():
    result = subprocess.run(['./scripts/deb-gvisor-pipeline.sh'], capture_output=True)
    if result.returncode == 0:
        pipeline_success.inc()
    else:
        pipeline_failure.inc()

if __name__ == '__main__':
    start_http_server(8000)
    while True:
        run_pipeline()
        time.sleep(300)  # Run every 5 minutes
```

---

## Troubleshooting

### Common Issues

#### Issue 1: `dpkg: dependency problems`

**Symptom**:
```
dpkg: dependency problems prevent configuration of ggen:
 ggen depends on libc6 (>= 2.31); however:
  Package libc6 is not installed.
```

**Solution**:
```bash
sudo apt-get update
sudo apt-get install -f  # Fix broken dependencies
sudo dpkg -i ggen_5.0.2_amd64.deb
```

---

#### Issue 2: `ggen: command not found` after installation

**Symptom**: Binary installed but shell can't find it

**Solution**:
```bash
# Refresh shell hash table
hash -r

# Or re-login
exec bash

# Verify
which ggen
```

---

#### Issue 3: `error while loading shared libraries`

**Symptom**:
```
ggen: error while loading shared libraries: libstdc++.so.6: cannot open shared object file
```

**Solution**:
```bash
# Install missing dependencies
sudo apt-get install libstdc++6

# Verify dependencies
ldd /usr/bin/ggen
```

---

#### Issue 4: gVisor runtime not found

**Symptom**:
```
docker: Error response from daemon: unknown or invalid runtime name: runsc.
```

**Solution**:
```bash
# Install gVisor runtime
wget https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc
sudo install -m 755 runsc /usr/local/bin/runsc

# Configure Docker
sudo tee /etc/docker/daemon.json <<EOF
{
  "runtimes": {
    "runsc": {
      "path": "/usr/local/bin/runsc"
    }
  }
}
EOF

sudo systemctl restart docker
```

---

#### Issue 5: Permission denied in Kubernetes

**Symptom**:
```
Error: cannot write to /workspace: permission denied
```

**Solution**:
```yaml
# Add volume mount with correct permissions
volumeMounts:
- name: workspace
  mountPath: /workspace
securityContext:
  fsGroup: 1000  # Match container user UID
```

---

## Rollback Procedures

### Debian Package Rollback

```bash
# Remove current version
sudo dpkg -r ggen

# Install previous version
sudo dpkg -i ggen_5.0.1_amd64.deb
```

---

### Docker Image Rollback

```bash
# Tag previous version as latest
docker tag ggen:5.0.1 ggen:latest

# Kubernetes deployment
kubectl set image deployment/ggen-deployment ggen=ggen:5.0.1
kubectl rollout status deployment/ggen-deployment
```

---

## Performance Tuning

### Build Performance

**Increase parallel compilation**:
```bash
# Set environment variable
export CARGO_BUILD_JOBS=8

# Run pipeline
./scripts/deb-gvisor-pipeline.sh
```

---

### Container Resource Limits

**Docker**:
```bash
docker run --memory=512m --cpus=1.0 ggen:5.0.2 sync
```

**Kubernetes**:
```yaml
resources:
  requests:
    memory: "256Mi"
    cpu: "500m"
  limits:
    memory: "512Mi"
    cpu: "1000m"
```

---

## Compliance & Audit

### Artifact Verification

```bash
# Verify package signature (if GPG-signed)
dpkg-sig --verify ggen_5.0.2_amd64.deb

# Verify checksums
sha256sum -c ggen_5.0.2_amd64.deb.sha256

# Inspect package contents
dpkg-deb -c ggen_5.0.2_amd64.deb

# View package metadata
dpkg-deb -I ggen_5.0.2_amd64.deb
```

---

### Audit Trail

**Pipeline logs** (`DEB_GVISOR_PIPELINE.log`):
- Timestamped execution log
- All phase results with status codes
- Error messages with context

**Build report** (`DEB_GVISOR_REPORT.md`):
- Artifact sizes and checksums
- Validation receipts
- Deployment instructions

---

## Support & Resources

### Documentation

- **Project Configuration**: `/home/user/ggen/CLAUDE.md`
- **Fail-Fast Proof**: `/home/user/ggen/.claude/fail-fast-proof.md`
- **Poka-Yoke Implementation**: `/home/user/ggen/.claude/poka-yoke-implementation.md`
- **Build Report**: `/home/user/ggen/DEB_GVISOR_REPORT.md`

### Community

- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **GitHub Repository**: https://github.com/seanchatmangpt/ggen

### Commercial Support

For enterprise support, contact your account manager or open a support ticket.

---

## Appendix

### Artifact Checksums

**Location**: `releases/v5.0.2/`

```
ggen_5.0.2_amd64.deb.sha256
ggen-5.0.2-x86_64-linux.sha256
ggen-5.0.2-x86_64-linux-gnu.tar.gz.sha256
```

**Verify all artifacts**:
```bash
cd releases/v5.0.2
sha256sum -c *.sha256
```

---

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CARGO_BUILD_JOBS` | Parallel build jobs | CPU count |
| `RUSTFLAGS` | Rust compiler flags | "-D warnings" |
| `GGEN_CONFIG` | Config file path | "./ggen.toml" |

---

### Platform Support Matrix

| Platform | Architecture | Status |
|----------|--------------|--------|
| Debian 11+ | amd64 | ✅ Supported |
| Ubuntu 20.04+ | amd64 | ✅ Supported |
| Ubuntu 22.04+ | amd64 | ✅ Tested |
| Docker | amd64 | ✅ Supported |
| Kubernetes 1.24+ | amd64 | ✅ Supported |
| gVisor | amd64 | ✅ Validated |
| AWS Lambda | amd64 | ⚠️ Experimental |
| ARM64 | arm64 | ❌ Not yet tested |

---

**Document Version**: 1.0
**Last Updated**: 2026-01-05
**Prepared by**: Agent 2 - EPIC 9 Parallel Agent Cycle
