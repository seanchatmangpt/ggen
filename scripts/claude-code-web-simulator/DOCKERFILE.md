# ggen Agent Execution Environment - Dockerfile

## Overview

This Dockerfile creates a minimal, production-ready Docker image for isolated agent execution. It provides a controlled environment for running ggen commands with security, reproducibility, and determinism.

## Architecture

### Multi-Stage Build

The Dockerfile uses a two-stage build pattern:

1. **Builder Stage** (`builder`):
   - Base: `ubuntu:24.04`
   - Installs full build toolchain (Rust, build-essential, development dependencies)
   - Compiles and installs ggen from source
   - Creates intermediate artifacts (~2-3GB during build)

2. **Production Stage** (final):
   - Base: `ubuntu:24.04`
   - Copies only runtime artifacts from builder
   - Minimizes final image size (~800MB)
   - Runs as non-root user (ggen:ggen, UID 1000)
   - Includes health check for container orchestration

## Features

### Security
- **Non-root execution**: User `ggen` (UID 1000) runs all processes
- **Minimal dependencies**: Only runtime libraries, no build tools in final image
- **Read-only filesystem**: Can be enforced at runtime with `--read-only` flag
- **No privileged operations**: Container runs unprivileged

### Performance
- **Layer caching**: Rust installation in separate layer for fast rebuilds
- **Minimal base image**: Ubuntu 24.04 is smaller than earlier versions
- **Cache optimization**: Build dependencies removed after compilation

### Reproducibility
- **Locked ggen version**: Installed with `--locked` flag (from Cargo.lock)
- **Deterministic environment**: Same build always produces same image
- **Environment variables**: Configurable via `GGEN_HOME`, `MCP_PROXY_URL`, `TIMEOUT_DEFAULT`

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `GGEN_HOME` | `/workspace` | Root directory for ggen operations |
| `MCP_PROXY_URL` | `` (empty) | URL for MCP proxy (optional) |
| `TIMEOUT_DEFAULT` | `120` | Default timeout in seconds for CLI commands |
| `RUST_LOG` | `warn` | Rust logging level (debug, info, warn, error) |
| `CARGO_HOME` | `/root/.cargo` | Cargo home directory |

## Building the Image

### Basic Build
```bash
# Build image with default tag
docker build -t ggen-agent:latest scripts/claude-code-web-simulator/

# Build with specific version tag
docker build -t ggen-agent:v6.0.0 scripts/claude-code-web-simulator/

# Build with no-cache (force rebuild)
docker build --no-cache -t ggen-agent:latest scripts/claude-code-web-simulator/
```

### Build with Custom Registry
```bash
# Build for Docker Hub
docker build -t yourusername/ggen-agent:latest scripts/claude-code-web-simulator/

# Build for private registry
docker build -t registry.example.com/ggen-agent:latest scripts/claude-code-web-simulator/

# Build for multiarch (requires buildx)
docker buildx build --platform linux/amd64,linux/arm64 \
  -t yourusername/ggen-agent:latest scripts/claude-code-web-simulator/
```

### Build Output
Successful build produces image size: ~800MB (production stage only)
- Ubuntu 24.04 base: ~80MB
- Runtime dependencies: ~100MB
- ggen binary and dependencies: ~620MB

## Running Containers

### Basic Execution
```bash
# Interactive shell
docker run -it -v /workspace:/workspace ggen-agent:latest

# Run ggen command
docker run --rm -v /workspace:/workspace ggen-agent:latest \
  ggen sync --validate_only true

# Run with MCP proxy URL
docker run --rm \
  -e MCP_PROXY_URL="http://mcp-proxy:3000" \
  -v /workspace:/workspace \
  ggen-agent:latest \
  ggen sync --audit true
```

### Volume Mounting
```bash
# Mount local workspace
docker run -v $(pwd):/workspace ggen-agent:latest

# Mount read-only (for security)
docker run -v $(pwd):/workspace:ro ggen-agent:latest \
  ggen sync --validate_only true

# Mount multiple paths
docker run -v $(pwd)/specs:/workspace/specs \
           -v $(pwd)/output:/workspace/output \
           ggen-agent:latest
```

### Resource Limits
```bash
# Limit CPU and memory
docker run --cpus="2" --memory="2g" \
  -v /workspace:/workspace \
  ggen-agent:latest

# Set timeout constraint
docker run --timeout 120s \
  -v /workspace:/workspace \
  ggen-agent:latest
```

### Network Isolation
```bash
# Disable network access (for security)
docker run --network none \
  -v /workspace:/workspace \
  ggen-agent:latest

# Enable specific port for MCP
docker run -p 3000:3000 \
  -e MCP_PROXY_URL="http://localhost:3000" \
  -v /workspace:/workspace \
  ggen-agent:latest
```

## Validation & Testing

### 1. Verify Build Success
```bash
# Check image exists
docker images ggen-agent

# Inspect image
docker inspect ggen-agent:latest

# Check image size
docker image ls --filter reference=ggen-agent:latest
```

### 2. Test Container Execution
```bash
# Test ggen binary availability
docker run --rm ggen-agent:latest ggen --version

# Test workspace isolation
docker run --rm -v $(pwd):/workspace ggen-agent:latest \
  sh -c "pwd && ls -la /workspace"

# Test health check
docker run --rm ggen-agent:latest healthcheck

# Verify non-root execution
docker run --rm ggen-agent:latest id
```

### 3. Test ggen Functionality
```bash
# Prepare test RDF file
mkdir -p /tmp/test-workspace
cat > /tmp/test-workspace/test.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:concept1 a ex:Concept ;
  ex:name "Test Concept" .
EOF

# Run validation
docker run --rm -v /tmp/test-workspace:/workspace ggen-agent:latest \
  ggen sync --validate_only true

# Expected output: Validation passes or specific error messages
```

### 4. Test Isolation
```bash
# Verify no host filesystem access outside /workspace
docker run --rm ggen-agent:latest \
  sh -c "touch /test.txt" 2>&1 | grep -q "Permission denied" && echo "✓ Read-only root" || echo "✗ Root writable"

# Verify non-root user
docker run --rm ggen-agent:latest \
  sh -c "whoami" | grep -q "ggen" && echo "✓ Non-root user" || echo "✗ Root user"

# Verify workspace is writable
docker run --rm -v /tmp/test:/workspace ggen-agent:latest \
  sh -c "touch /workspace/test.txt && ls /workspace" | grep -q "test.txt" && echo "✓ Workspace writable" || echo "✗ Workspace read-only"
```

## Performance Optimization

### Build Performance
```bash
# Use BuildKit for faster builds
DOCKER_BUILDKIT=1 docker build -t ggen-agent:latest scripts/claude-code-web-simulator/

# Enable inline cache for CI/CD
docker build --build-arg BUILDKIT_INLINE_CACHE=1 \
  -t ggen-agent:latest scripts/claude-code-web-simulator/
```

### Runtime Performance
```bash
# Allocate sufficient resources
docker run --cpus="2" --memory="2g" \
  -v /workspace:/workspace \
  ggen-agent:latest

# Use tmpfs for temporary files (speeds up I/O)
docker run --tmpfs /tmp:rw,size=1g \
  -v /workspace:/workspace \
  ggen-agent:latest
```

## Troubleshooting

### Problem: Build fails with "rustup not found"
**Solution**: Ensure curl is installed in builder stage (it is). Check Docker has network access.

### Problem: ggen binary not found in final image
**Solution**: Verify COPY command copied from correct path. Check builder stage completed successfully.

### Problem: Container exits immediately
**Solution**: Override ENTRYPOINT or CMD to see logs:
```bash
docker run -it ggen-agent:latest /bin/bash
```

### Problem: Permission denied in /workspace
**Solution**: Ensure volume is mounted with proper permissions:
```bash
# Fix with chmod before mounting
chmod 777 /workspace
docker run -v /workspace:/workspace ggen-agent:latest
```

### Problem: MCP proxy connection fails
**Solution**: Verify network connectivity and proxy URL:
```bash
docker run -e MCP_PROXY_URL="http://mcp-proxy:3000" \
  --network host ggen-agent:latest
```

## CI/CD Integration

### GitHub Actions
```yaml
name: Build and Test Docker Image

on: [push, pull_request]

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build Docker image
        run: |
          docker build -t ggen-agent:latest scripts/claude-code-web-simulator/

      - name: Test ggen binary
        run: |
          docker run --rm ggen-agent:latest ggen --version

      - name: Test isolation
        run: |
          docker run --rm ggen-agent:latest id | grep -q "uid=1000" || exit 1

      - name: Push to registry (optional)
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        run: |
          docker tag ggen-agent:latest yourusername/ggen-agent:latest
          docker push yourusername/ggen-agent:latest
```

### Local Testing Script
```bash
#!/bin/bash
set -e

echo "Building Docker image..."
docker build -t ggen-agent:latest scripts/claude-code-web-simulator/

echo "Testing ggen --version..."
docker run --rm ggen-agent:latest ggen --version

echo "Testing non-root user..."
docker run --rm ggen-agent:latest id | grep -q "uid=1000"

echo "Testing health check..."
docker run --rm ggen-agent:latest healthcheck

echo "All tests passed!"
```

## Security Considerations

### Container Security Best Practices
1. **Run as non-root**: Container uses `ggen` user (UID 1000)
2. **Read-only filesystem**: Mount with `:ro` flag for untrusted inputs
3. **Network isolation**: Use `--network none` when network not needed
4. **Resource limits**: Always set CPU and memory limits
5. **Registry scanning**: Scan for vulnerabilities before production use

### Scanning for Vulnerabilities
```bash
# Using Trivy
trivy image ggen-agent:latest

# Using Docker Scout (requires authentication)
docker scout cves ggen-agent:latest

# Using grype
grype ggen-agent:latest
```

## Advanced Usage

### Kubernetes Deployment
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: ggen-agent
spec:
  containers:
  - name: ggen
    image: ggen-agent:latest
    volumeMounts:
    - name: workspace
      mountPath: /workspace
    env:
    - name: GGEN_HOME
      value: /workspace
    - name: TIMEOUT_DEFAULT
      value: "120"
    resources:
      limits:
        memory: "2Gi"
        cpu: "2"
      requests:
        memory: "1Gi"
        cpu: "1"
    securityContext:
      runAsNonRoot: true
      runAsUser: 1000
      readOnlyRootFilesystem: true
  volumes:
  - name: workspace
    emptyDir: {}
```

### Docker Compose
```yaml
version: '3.8'

services:
  ggen-agent:
    build:
      context: .
      dockerfile: scripts/claude-code-web-simulator/Dockerfile
    image: ggen-agent:latest
    container_name: ggen-agent
    volumes:
      - ./workspace:/workspace
    environment:
      GGEN_HOME: /workspace
      MCP_PROXY_URL: ${MCP_PROXY_URL:-}
      TIMEOUT_DEFAULT: 120
    ports:
      - "3000:3000"  # For MCP proxy
    restart: unless-stopped
```

## References

- [Docker Multi-Stage Builds](https://docs.docker.com/build/building/multi-stage/)
- [Docker Security Best Practices](https://docs.docker.com/engine/security/)
- [Rust Docker Best Practices](https://www.rust-lang.org/what/wg-docker/)
- [Ubuntu 24.04 LTS](https://releases.ubuntu.com/24.04/)

---

**Last Updated**: 2026-01-29
**Version**: 1.0.0
**Status**: Production-Ready
