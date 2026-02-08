# Docker Setup for ggen Agent Execution Environment

## Quick Start

### 1. Build the Docker Image

```bash
# Navigate to the claude-code-web-simulator directory
cd scripts/claude-code-web-simulator

# Build the image
./build-and-test-docker.sh

# Or use docker build directly
DOCKER_BUILDKIT=1 docker build -t ggen-agent:latest .
```

### 2. Run a Container

```bash
# Interactive shell
docker run -it -v /workspace:/workspace ggen-agent:latest

# Run ggen command
docker run --rm -v $(pwd):/workspace ggen-agent:latest \
  ggen sync --validate_only true

# Run with resource limits
docker run --rm --cpus="2" --memory="2g" \
  -v $(pwd):/workspace \
  ggen-agent:latest \
  ggen sync --audit true
```

### 3. Use Docker Compose

```bash
# Start services
docker-compose up -d

# Execute command in running container
docker-compose exec ggen ggen --version

# View logs
docker-compose logs -f ggen

# Stop services
docker-compose down
```

## File Structure

```
scripts/claude-code-web-simulator/
├── Dockerfile                    # Multi-stage Docker image
├── .dockerignore                # Optimize build context
├── docker-compose.yml           # Orchestration configuration
├── build-and-test-docker.sh     # Build and test script
├── DOCKERFILE.md                # Detailed Dockerfile documentation
└── DOCKER_README.md             # This file
```

## Building the Image

### Standard Build
```bash
cd scripts/claude-code-web-simulator
docker build -t ggen-agent:latest .
```

### Build with BuildKit (Faster)
```bash
DOCKER_BUILDKIT=1 docker build -t ggen-agent:latest .
```

### Build without Cache
```bash
docker build --no-cache -t ggen-agent:latest .
```

### Build with Custom Registry
```bash
docker build -t my-registry.com/ggen-agent:v6.0.0 .
```

### Using the Build Script
```bash
# Basic build
./build-and-test-docker.sh

# Build without cache
./build-and-test-docker.sh --no-cache

# Build for specific registry
./build-and-test-docker.sh --registry my-registry.com --version v6.0.0

# Build and push
./build-and-test-docker.sh --registry my-registry.com --push
```

## Running Containers

### Interactive Development
```bash
# Start interactive shell
docker run -it \
  -v $(pwd):/workspace \
  ggen-agent:latest

# Inside container
ggen --version
ggen sync --validate_only true
```

### Non-Interactive Commands
```bash
# Run single command
docker run --rm \
  -v $(pwd):/workspace \
  ggen-agent:latest \
  ggen sync --validate_only true

# Run with environment variables
docker run --rm \
  -e GGEN_HOME=/workspace \
  -e MCP_PROXY_URL="http://mcp:3000" \
  -e RUST_LOG=debug \
  -v $(pwd):/workspace \
  ggen-agent:latest \
  ggen sync --audit true
```

### Resource Management
```bash
# Limit CPU
docker run --cpus="2" -v $(pwd):/workspace ggen-agent:latest ggen --version

# Limit memory
docker run --memory="2g" -v $(pwd):/workspace ggen-agent:latest ggen --version

# Both CPU and memory
docker run --cpus="2" --memory="2g" \
  -v $(pwd):/workspace \
  ggen-agent:latest \
  ggen sync --validate_only true
```

### Network Isolation
```bash
# Disable network (for security)
docker run --network none \
  -v $(pwd):/workspace \
  ggen-agent:latest \
  ggen sync --validate_only true

# Enable specific port
docker run -p 3000:3000 \
  -e MCP_PROXY_URL="http://localhost:3000" \
  -v $(pwd):/workspace \
  ggen-agent:latest
```

### Volume Mounting
```bash
# Read-write mount (default)
docker run -v $(pwd):/workspace ggen-agent:latest

# Read-only mount (for security)
docker run -v $(pwd):/workspace:ro ggen-agent:latest

# Multiple mounts
docker run \
  -v $(pwd)/specs:/workspace/specs:ro \
  -v $(pwd)/output:/workspace/output:rw \
  ggen-agent:latest

# Temporary filesystem mount
docker run --tmpfs /tmp:rw,size=1g \
  -v $(pwd):/workspace \
  ggen-agent:latest
```

## Docker Compose Usage

### Start Services
```bash
# Start in background
docker-compose up -d

# Start with output
docker-compose up

# Start specific service
docker-compose up -d ggen
```

### Execute Commands
```bash
# Run command in running container
docker-compose exec ggen ggen --version

# Run with environment
docker-compose exec -e RUST_LOG=debug ggen ggen sync --audit true

# Interactive shell
docker-compose exec -it ggen /bin/bash
```

### View Logs
```bash
# View all logs
docker-compose logs

# Follow logs
docker-compose logs -f ggen

# Last 100 lines
docker-compose logs --tail=100 ggen

# Specific time range
docker-compose logs --since 10m ggen
```

### Stop and Clean
```bash
# Stop services
docker-compose stop

# Stop and remove containers
docker-compose down

# Remove volumes too
docker-compose down -v

# Remove build cache
docker-compose down --remove-orphans
```

### Configuration
```bash
# Set environment variable
export MCP_PROXY_URL="http://mcp-proxy:3000"
docker-compose up -d

# Or in .env file
cat > .env << EOF
MCP_PROXY_URL=http://mcp-proxy:3000
RUST_LOG=debug
TIMEOUT_DEFAULT=120
EOF
docker-compose up -d
```

### Enable Optional Services
```bash
# Enable MCP proxy service
docker-compose --profile with-mcp up -d

# Enable development tools
docker-compose --profile with-devtools up -d

# Enable both
docker-compose --profile with-mcp --profile with-devtools up -d
```

## Testing

### Run Tests
```bash
# Using build script (runs all tests)
./build-and-test-docker.sh

# Test specific functionality
docker run --rm ggen-agent:latest ggen --version
docker run --rm ggen-agent:latest id
docker run --rm -v $(pwd):/workspace ggen-agent:latest ggen sync --validate_only true
```

### Validation Checklist
- [ ] Docker installed and working
- [ ] Build succeeds: `./build-and-test-docker.sh`
- [ ] Image created: `docker images ggen-agent`
- [ ] ggen binary works: `docker run --rm ggen-agent:latest ggen --version`
- [ ] Non-root user: `docker run --rm ggen-agent:latest id | grep uid=1000`
- [ ] Workspace mounted: `docker run --rm -v /tmp:/workspace ggen-agent:latest ls /workspace`
- [ ] Health check passes: `docker run --rm ggen-agent:latest healthcheck`

## Troubleshooting

### Build Issues

**Problem: "rustup not found" during build**
```bash
# Solution: Ensure network access
docker build --network=host -t ggen-agent:latest .
```

**Problem: "ggen binary not found" in final image**
```bash
# Solution: Check builder stage logs
docker build --progress=plain -t ggen-agent:latest . 2>&1 | grep -A5 "COPY.*ggen"
```

**Problem: Build takes too long**
```bash
# Solution: Use BuildKit and cache
DOCKER_BUILDKIT=1 docker build --build-arg BUILDKIT_INLINE_CACHE=1 \
  -t ggen-agent:latest .
```

### Runtime Issues

**Problem: "Permission denied" in /workspace**
```bash
# Solution: Fix directory permissions
chmod 777 /workspace
docker run -v /workspace:/workspace ggen-agent:latest
```

**Problem: Container exits immediately**
```bash
# Solution: Override entrypoint to see errors
docker run -it --entrypoint /bin/bash ggen-agent:latest
```

**Problem: "No such file or directory"**
```bash
# Solution: Verify volume path exists
mkdir -p $(pwd)/workspace
docker run -v $(pwd)/workspace:/workspace ggen-agent:latest
```

**Problem: Resource limit exceeded**
```bash
# Solution: Increase limits
docker run --cpus="4" --memory="4g" \
  -v $(pwd):/workspace \
  ggen-agent:latest
```

### Network Issues

**Problem: Cannot reach MCP proxy**
```bash
# Solution: Verify network and URL
docker run -e MCP_PROXY_URL="http://host.docker.internal:3000" \
  ggen-agent:latest

# Or use docker-compose networks
docker-compose up -d  # MCP proxy will be at http://mcp-proxy:3000
```

**Problem: DNS resolution fails**
```bash
# Solution: Specify DNS servers
docker run --dns 8.8.8.8 --dns 8.8.4.4 \
  -v $(pwd):/workspace \
  ggen-agent:latest
```

## Performance Optimization

### Build Performance
```bash
# Use BuildKit for parallel builds
DOCKER_BUILDKIT=1 docker build -t ggen-agent:latest .

# Cache layers aggressively
docker build --build-arg BUILDKIT_INLINE_CACHE=1 \
  -t ggen-agent:latest .

# Reuse builder cache in CI
docker build --cache-from=ggen-agent:builder \
  -t ggen-agent:latest .
```

### Runtime Performance
```bash
# Use tmpfs for temporary files
docker run --tmpfs /tmp:rw,size=1g \
  -v $(pwd):/workspace \
  ggen-agent:latest

# Allocate sufficient resources
docker run --cpus="2" --memory="2g" \
  -v $(pwd):/workspace \
  ggen-agent:latest

# Use read-only volumes when possible
docker run -v $(pwd)/specs:/workspace/specs:ro \
  -v $(pwd)/output:/workspace/output:rw \
  ggen-agent:latest
```

## Security

### Best Practices
1. **Run as non-root**: Container defaults to user 1000 (ggen)
2. **Read-only filesystem**: Use `:ro` flag for trusted inputs
3. **Network isolation**: Use `--network none` when not needed
4. **Resource limits**: Always set CPU and memory limits
5. **Vulnerability scanning**: Scan image before production use

### Scanning for Vulnerabilities
```bash
# Using Trivy
trivy image ggen-agent:latest

# Using Docker Scout
docker scout cves ggen-agent:latest

# Using Grype
grype ggen-agent:latest
```

### Restrict Image
```bash
# Run with read-only root
docker run --read-only \
  --tmpfs /tmp \
  -v $(pwd):/workspace \
  ggen-agent:latest

# Drop all capabilities
docker run --cap-drop=ALL \
  -v $(pwd):/workspace \
  ggen-agent:latest

# Run with security options
docker run --security-opt no-new-privileges \
  --cap-drop=ALL \
  -v $(pwd):/workspace \
  ggen-agent:latest
```

## Advanced Usage

### Kubernetes Deployment
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ggen-agent
spec:
  replicas: 1
  selector:
    matchLabels:
      app: ggen-agent
  template:
    metadata:
      labels:
        app: ggen-agent
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
          allowPrivilegeEscalation: false
          capabilities:
            drop:
            - ALL
      volumes:
      - name: workspace
        emptyDir: {}
```

### Multi-Architecture Builds
```bash
# Require Docker Buildx
docker buildx create --name ggen-builder
docker buildx use ggen-builder

# Build for multiple platforms
docker buildx build \
  --platform linux/amd64,linux/arm64,linux/arm/v7 \
  -t yourusername/ggen-agent:latest \
  --push .
```

### CI/CD Integration
```yaml
# GitHub Actions example
name: Build Docker Image
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: docker/setup-buildx-action@v2
      - uses: docker/build-push-action@v4
        with:
          context: ./scripts/claude-code-web-simulator
          push: true
          tags: yourusername/ggen-agent:latest
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `GGEN_HOME` | `/workspace` | ggen home directory |
| `MCP_PROXY_URL` | `` | MCP proxy URL (optional) |
| `TIMEOUT_DEFAULT` | `120` | Default timeout in seconds |
| `RUST_LOG` | `warn` | Rust logging level |
| `CARGO_HOME` | `/root/.cargo` | Cargo home directory |

## Image Details

- **Base Image**: ubuntu:24.04 (80MB)
- **Final Size**: ~800MB
- **User**: ggen (UID 1000)
- **Entrypoint**: /bin/bash
- **Working Directory**: /workspace
- **Health Check**: `ggen --version` every 30s

## References

- [Dockerfile Reference](https://docs.docker.com/engine/reference/builder/)
- [Docker Compose Reference](https://docs.docker.com/compose/compose-file/)
- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Multi-Stage Builds](https://docs.docker.com/build/building/multi-stage/)

---

**Last Updated**: 2026-01-29
**Version**: 1.0.0
**Status**: Production-Ready
