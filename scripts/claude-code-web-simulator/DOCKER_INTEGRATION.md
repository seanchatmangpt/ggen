# Docker Integration Guide

## Overview

The Docker integration module (`modules/docker-runner.sh`) replaces sandbox simulation with real Docker containers, providing production-grade isolation and reproducibility for agent execution.

**Key Benefits:**
- Real container isolation (not simulated)
- Deterministic, reproducible execution
- Network isolation via bridge networking
- Resource limits (memory, CPU, PIDs)
- Exit code propagation
- Comprehensive error handling and recovery

## Architecture

```
┌─────────────────────────────────────────────────────┐
│         Agent Bootstrap Process                      │
└─────────────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────┐
│  bootstrap_docker_init()                            │
│  ├─ Check Docker availability                       │
│  ├─ Check socket permissions                        │
│  └─ Initialize image (docker_init_image)            │
└─────────────────────────────────────────────────────┘
                     │
         ┌───────────┴───────────┐
         ▼                       ▼
    Agent 1                  Agent N
         │                       │
    ┌────────────────────────────────────┐
    │ bootstrap_docker_execute()          │
    │ ├─ Spawn container (docker_spawn_*) │
    │ ├─ Mount workspace                  │
    │ ├─ Run command                      │
    │ ├─ Capture output/exit code         │
    │ └─ Generate receipt                 │
    └────────────────────────────────────┘
         │                       │
         ▼                       ▼
    Container 1             Container N
    (ggen-agent)            (ggen-agent)
    [ISOLATED]              [ISOLATED]
         │                       │
    Workspace 1             Workspace N
    [Mounted]               [Mounted]
```

## Installation & Setup

### 1. Install Docker

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install docker.io
sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
newgrp docker  # Activate group membership
```

**macOS:**
```bash
brew install docker
docker run hello-world  # Test installation
```

**Windows:**
- Download [Docker Desktop](https://www.docker.com/products/docker-desktop)
- Install and start Docker Desktop

### 2. Verify Installation

```bash
docker --version
docker ps  # Should work without sudo
docker run hello-world
```

### 3. Initialize ggen Environment

```bash
cd /path/to/ggen/scripts/claude-code-web-simulator
./main.sh start
```

This will:
- Create workspace directories
- Initialize configuration files
- Load Docker runner module
- Check Docker availability

## Core Functions

### Image Management

#### `docker_init_image()`

Initializes the Docker image for agent execution.

**Behavior:**
1. Checks if `ggen-agent:latest` image exists
2. Returns image ID if found
3. Builds image if missing (using Dockerfile or minimal image)
4. Verifies build success
5. Returns image ID on success, exit code 1 on failure

**Usage:**
```bash
source modules/docker-runner.sh
image_id=$(docker_init_image)
echo "Image ID: ${image_id}"
```

**Error Handling:**
- `Docker not installed` → Message with installation instructions
- `Docker daemon not running` → Message with startup instructions
- `Build failure` → Retry with exponential backoff (3 attempts)
- `Image not found after build` → Clear error message

#### `docker_image_exists(image_name)`

Checks if a Docker image exists.

**Usage:**
```bash
if docker_image_exists "ggen-agent:latest"; then
    echo "Image found"
else
    echo "Image not found"
fi
```

### Container Execution

#### `docker_spawn_agent(agent_id, workspace_path, command)`

Spawns a Docker container for agent execution.

**Parameters:**
- `agent_id`: Unique identifier for the agent
- `workspace_path`: Absolute path to workspace (mounted as `/workspace`)
- `command`: Bash command to execute in container

**Behavior:**
1. Validates inputs
2. Verifies Docker image exists
3. Creates isolated network if needed
4. Runs container with resource limits
5. Captures stdout, stderr, exit code
6. Returns output to stdout
7. Stores execution result in `.docker-result-{agent_id}`

**Container Configuration:**
```bash
docker run --rm \
  --name ggen-{agent_id}-{timestamp} \
  --network ggen-isolated \
  --volume ${workspace}:/workspace:rw \
  --workdir=/workspace \
  --memory 512m \
  --cpus 1 \
  --pids-limit 256 \
  --security-opt no-new-privileges:true \
  ggen-agent:latest \
  /bin/bash -c "${command}"
```

**Exit Codes:**
- `0`: Success
- `1`: Docker error (image not found, container creation failed)
- `124`: Timeout (exceeds `DOCKER_TIMEOUT_SECONDS`)
- `N`: Container exit code (propagated directly)

**Usage:**
```bash
source modules/docker-runner.sh

workspace="/tmp/ggen-workspace/agents/test-agent"
mkdir -p "${workspace}"

# Run simple command
docker_spawn_agent "test-agent-1" "${workspace}" "echo 'Hello from container'"

# Run complex command
docker_spawn_agent "test-agent-2" "${workspace}" \
  "cd /workspace && cargo make test && cargo make lint"
```

**Output Files Created:**
- `.docker-result-{agent_id}`: JSON execution result
- `.docker-output-{agent_id}`: Container stdout (temporary)
- `.docker-error-{agent_id}`: Container stderr (temporary)

#### `docker_ensure_network(network_name)`

Ensures an isolated Docker network exists.

**Usage:**
```bash
docker_ensure_network "ggen-isolated"
```

### Bootstrap Integration

#### `bootstrap_docker_init()`

Initializes Docker system during agent bootstrap (called once).

**Behavior:**
1. Checks Docker availability
2. Verifies socket permissions
3. Initializes image
4. Sets up isolated network
5. Returns 0 on success, 1 on failure

**Usage:**
```bash
source modules/docker-runner.sh
bootstrap_docker_init || exit 1
```

#### `bootstrap_docker_execute(agent_id, workspace, command)`

Executes an agent in Docker (called for each agent).

**Usage:**
```bash
bootstrap_docker_execute "validator-1" "/tmp/workspace" \
  "ggen validate --spec ontology.ttl"
```

## Configuration

### Environment Variables

```bash
# Image configuration
export DOCKER_IMAGE_NAME="ggen-agent"
export DOCKER_IMAGE_TAG="latest"

# Network configuration
export DOCKER_NETWORK="ggen-isolated"

# Container limits
export DOCKER_TIMEOUT_SECONDS=120
export DOCKER_MEMORY_LIMIT="512m"
export DOCKER_CPU_LIMIT="1"

# Workspace paths
export WORKSPACE_DIR="/path/to/workspace"
export CONFIG_DIR="/path/to/config"
```

### Resource Limits

Container resource constraints (hard-coded for safety):
```
Memory:    512 MB (max)
CPU:       1 core
PIDs:      256 (max processes)
Timeout:   120 seconds
Root FS:   Read-write (writable workspace only)
```

Modify in `docker-runner.sh`:
```bash
# Around line 30
readonly DOCKER_MEMORY_LIMIT="512m"    # Increase for large workloads
readonly DOCKER_CPU_LIMIT="1"          # Set to "2" for multi-core
readonly DOCKER_TIMEOUT_SECONDS=120    # Increase for long operations
```

### Network Isolation

The `ggen-isolated` Docker network provides:
- Bridge mode networking (default)
- IP masquerading (enabled)
- Isolated from host network
- Container-to-container communication only

## Integration with Main System

### Step 1: Source Module in main.sh

```bash
# In main.sh, after setting up paths
source "${MODULES_DIR}/docker-runner.sh"
```

### Step 2: Initialize During Bootstrap

```bash
init_environment() {
    # ... existing code ...

    # Initialize Docker (called once at startup)
    if ! bootstrap_docker_init; then
        log_warn "Docker not available, falling back to simulation mode"
        export SIMULATION_MODE=true
    else
        log_success "Docker initialized for agent execution"
    fi
}
```

### Step 3: Execute Agents in Docker

```bash
run_validation_agent() {
    local spec_file="$1"
    local agent_id="validator-$(date +%s%N)"
    local sandbox="${WORKSPACE_DIR}/sandboxes/${agent_id}"
    mkdir -p "${sandbox}"

    # Execute in Docker instead of simulating
    if bootstrap_docker_execute "${agent_id}" "${sandbox}" \
        "ggen validate --spec '${spec_file}'"; then

        log_success "Validation passed"
        generate_receipt "${agent_id}" "validation" "passed" "${sandbox}"
    else
        log_error "Validation failed"
        generate_receipt "${agent_id}" "validation" "failed" "${sandbox}"
    fi
}
```

### Step 4: Cleanup on Shutdown

```bash
stop_environment() {
    # ... existing cleanup code ...

    # Cleanup Docker containers and networks
    docker_cleanup

    log_success "Docker cleanup completed"
}
```

## Error Handling

### Common Issues & Solutions

#### Docker Not Installed

**Error:**
```
Docker is not installed
  Please install Docker from https://docs.docker.com/get-docker/
```

**Solution:**
```bash
# Linux
sudo apt-get install docker.io

# macOS
brew install docker

# Windows
# Download Docker Desktop from https://www.docker.com/products/docker-desktop
```

#### Docker Daemon Not Running

**Error:**
```
Docker daemon is not running
  Please start Docker service
```

**Solution:**
```bash
# Linux
sudo systemctl start docker

# macOS
open /Applications/Docker.app

# Windows
# Start Docker Desktop from Application menu
```

#### Permission Denied

**Error:**
```
Got permission denied while trying to connect to Docker daemon
```

**Solution:**
```bash
# Add user to docker group
sudo usermod -aG docker $USER

# Restart Docker
sudo systemctl restart docker

# Activate group membership
newgrp docker
```

#### Container Timeout

**Error:**
```
Container timeout after 120s
Container name: ggen-agent-test-1234567890
```

**Solution:**
1. Increase timeout:
   ```bash
   export DOCKER_TIMEOUT_SECONDS=300
   ```

2. Optimize command to run faster

3. Check container logs:
   ```bash
   docker logs <container_id>
   ```

4. Check Docker resources:
   ```bash
   docker stats
   ```

#### Build Failure

**Error:**
```
Failed to build image: ggen-agent:latest
Build log saved to: /workspace/docker-build.log
```

**Solution:**
1. Check build log:
   ```bash
   cat /workspace/docker-build.log
   ```

2. Verify Dockerfile exists:
   ```bash
   ls -la Dockerfile
   ```

3. Try manual build:
   ```bash
   docker build -t ggen-agent:latest .
   ```

## Testing

### Run Test Suite

```bash
cd /path/to/ggen/scripts/claude-code-web-simulator
bash tests/test-docker-runner.sh
```

**Test Coverage:**
- Availability checks (Docker, socket, health)
- Image lifecycle (init, exists, build)
- Container execution (spawn, output, files, exit codes)
- Network creation
- Error handling
- Bootstrap integration

### Sample Test Output

```
╔════════════════════════════════════════════════════════════════╗
║     Claude Code Web Simulator - Docker Runner Test Suite      ║
╚════════════════════════════════════════════════════════════════╝

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Availability Tests
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
[TEST 1] docker_check_availability: Docker is available
  ✓ PASS docker_check_availability

[TEST 2] docker_check_socket_permissions: Can access Docker socket
  ✓ PASS docker_check_socket_permissions

[TEST 3] docker_health_check: System health check passes
  ✓ PASS docker_health_check

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Test Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total:   15
Passed:  15
Failed:  0
Skipped: 0
✓ All tests passed!
```

## Monitoring & Debugging

### View Active Containers

```bash
source modules/docker-runner.sh
docker_view_containers
```

Output:
```
CONTAINER ID   NAMES                     STATUS         PORTS
a1b2c3d4e5f6   ggen-validator-1234567   Up 5 seconds
g7h8i9j0k1l2   ggen-generator-2345678   Up 2 seconds
```

### View Network

```bash
docker_view_network
```

### Health Check

```bash
docker_health_check
```

### View Container Logs

```bash
docker logs ggen-validator-1234567
```

## Performance Considerations

### Container Startup Time

- First container: ~2-3 seconds (image cache warm)
- Subsequent containers: ~500ms-1s
- Network creation: ~100-200ms

### Resource Usage

- Image size: ~500MB (Debian slim base)
- Memory per container: 50-100MB (at rest)
- Disk space: 500MB + workspace

### Optimization Tips

1. **Reuse containers**: Initialize image once per session
2. **Batch operations**: Run multiple commands in single container
3. **Pre-cache layers**: Build image incrementally
4. **Increase resources**: Adjust `DOCKER_MEMORY_LIMIT` if needed

## Security Considerations

### Default Security Policy

- `no-new-privileges`: Prevents privilege escalation
- Memory limit: Prevents DoS via memory exhaustion
- PID limit: Prevents fork bomb attacks
- Read-only root FS: Container can't modify system files
- Network isolation: Prevents container-to-host communication

### Recommendations

1. Run as non-root user (in container):
   ```dockerfile
   RUN groupadd -r ggen && useradd -r -g ggen ggen
   USER ggen
   ```

2. Limit network access (in container):
   ```bash
   export DOCKER_NETWORK="ggen-isolated"
   ```

3. Regular security updates:
   ```bash
   docker pull ggen-agent:latest
   docker image prune -a
   ```

4. Monitor container activity:
   ```bash
   docker events --filter type=container
   ```

## Production Deployment

### Pre-Flight Checks

```bash
#!/bin/bash
source modules/docker-runner.sh

# 1. Check Docker availability
docker_check_availability || exit 1

# 2. Verify socket permissions
docker_check_socket_permissions || exit 1

# 3. Initialize image
docker_init_image > /dev/null || exit 1

# 4. Test network
docker_ensure_network "ggen-isolated" || exit 1

# 5. Run health check
docker_health_check || exit 1

echo "✓ All pre-flight checks passed"
```

### Graceful Degradation

If Docker is unavailable, fall back to simulation:

```bash
if bootstrap_docker_init; then
    export DOCKER_MODE=true
    log_success "Using Docker for agent execution"
else
    export DOCKER_MODE=false
    log_warn "Docker unavailable, using simulation mode"
fi

# Then in agent execution
if [[ "${DOCKER_MODE}" == "true" ]]; then
    bootstrap_docker_execute "$@"
else
    # Fallback to simulated execution
    run_simulated_agent "$@"
fi
```

### Logging

Container execution is logged to:
- `/workspace/docker-build.log` (build logs)
- `.docker-result-{agent_id}` (execution result JSON)
- Docker daemon logs (system-dependent)

View logs:
```bash
docker logs <container_id>
journalctl -u docker  # Linux systemd
```

## Troubleshooting

### Container won't start

```bash
# Check Docker status
docker ps

# View error logs
docker logs <container_id>

# Check resource availability
docker stats

# Try manual container run
docker run --rm ggen-agent:latest echo "test"
```

### Files not visible in workspace

```bash
# Verify mount
docker run --rm -v /path/to/workspace:/workspace \
  ggen-agent:latest ls -la /workspace

# Check permissions
ls -la /path/to/workspace
chmod 777 /path/to/workspace
```

### Network connectivity issues

```bash
# Verify network exists
docker network ls | grep ggen-isolated

# Check network details
docker network inspect ggen-isolated

# Test connectivity
docker run --rm --network ggen-isolated \
  ggen-agent:latest ping 8.8.8.8
```

### High memory usage

```bash
# Check memory usage
docker stats

# Reduce memory limit
export DOCKER_MEMORY_LIMIT="256m"

# Or increase system memory
# Kill unnecessary containers
docker container prune
```

## Further Reading

- [Docker Documentation](https://docs.docker.com/)
- [Dockerfile Reference](https://docs.docker.com/engine/reference/builder/)
- [Docker Run Reference](https://docs.docker.com/engine/reference/run/)
- [Container Security](https://docs.docker.com/engine/security/)

## Support

For issues:
1. Check Docker installation: `docker --version`
2. Run health check: `docker_health_check`
3. View logs: `cat /workspace/docker-build.log`
4. Check tests: `bash tests/test-docker-runner.sh`
5. Report in GitHub: https://github.com/seanchatmangpt/ggen/issues
