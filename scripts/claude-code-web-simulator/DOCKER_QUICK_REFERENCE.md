# Docker Integration - Quick Reference

One-page reference for Docker runner module integration.

## Files

| File | Purpose | Size |
|------|---------|------|
| `modules/docker-runner.sh` | Main Docker execution module | 664 lines |
| `tests/test-docker-runner.sh` | Test suite (23 tests) | 549 lines |
| `DOCKER_INTEGRATION.md` | Complete documentation | 18 KB |
| `DOCKER_INTEGRATION_CHECKLIST.md` | Implementation guide | 14 KB |
| `DOCKER_DEPLOYMENT_SUMMARY.md` | Deployment overview | 12 KB |

## Core Functions

```bash
# Initialize Docker (call once)
docker_init_image()                              # Returns: image ID or 1

# Execute agents (call for each agent)
docker_spawn_agent(agent_id, workspace, cmd)    # Returns: exit code, output to stdout

# Bootstrap integration
bootstrap_docker_init()                          # One-time setup
bootstrap_docker_execute(agent_id, ws, cmd)     # Per-agent execution

# Utilities
docker_health_check()                            # Verify system health
docker_cleanup()                                 # Cleanup containers
docker_view_containers()                         # List active containers
```

## Quick Start

### 1. Install Docker
```bash
sudo apt-get install docker.io
sudo usermod -aG docker $USER
newgrp docker
```

### 2. Source Module
```bash
source modules/docker-runner.sh
```

### 3. Initialize
```bash
bootstrap_docker_init
```

### 4. Execute Agent
```bash
mkdir -p /tmp/workspace
bootstrap_docker_execute "agent-1" "/tmp/workspace" "echo 'Hello'"
```

### 5. Check Results
```bash
cat /tmp/workspace/.docker-result-agent-1
```

## Integration into main.sh

### Step 1: Load Module (~line 135)
```bash
source "${MODULES_DIR}/docker-runner.sh"
```

### Step 2: Initialize (~line 121)
```bash
init_environment() {
    # ... existing code ...
    bootstrap_docker_init || export DOCKER_ENABLED=false
}
```

### Step 3: Use in Agents
```bash
run_validation_agent() {
    # ... setup code ...
    if [[ "${DOCKER_ENABLED:-false}" == "true" ]]; then
        bootstrap_docker_execute "${agent_id}" "${workspace}" \
            "ggen validate --spec '${spec_file}'"
    else
        # Fallback to simulation
    fi
}
```

### Step 4: Cleanup (~line 520)
```bash
stop_environment() {
    # ... existing code ...
    docker_cleanup
}
```

## Configuration

```bash
# Resource limits
export DOCKER_TIMEOUT_SECONDS=120      # Container timeout
export DOCKER_MEMORY_LIMIT="512m"      # Memory limit
export DOCKER_CPU_LIMIT="1"            # CPU cores

# Image/network
export DOCKER_IMAGE_NAME="ggen-agent"
export DOCKER_IMAGE_TAG="latest"
export DOCKER_NETWORK="ggen-isolated"
```

## Testing

```bash
# Run all tests (23 tests)
bash tests/test-docker-runner.sh

# Manual test
source modules/docker-runner.sh
bootstrap_docker_init
mkdir -p /tmp/test
bootstrap_docker_execute "test-1" "/tmp/test" "echo 'test'"
```

## Container Configuration

```
Memory:     512 MB (limit)
CPU:        1 core (limit)
PID:        256 processes (max)
Timeout:    120 seconds
Network:    Isolated (ggen-isolated bridge)
Filesystem: Workspace mounted at /workspace
Security:   no-new-privileges enabled
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Docker error (image not found, permission denied, etc) |
| 124 | Container timeout |
| N | Container exit code (propagated) |

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Docker not installed | `sudo apt-get install docker.io` |
| Permission denied | `sudo usermod -aG docker $USER && newgrp docker` |
| Image build fails | `cat /workspace/docker-build.log` |
| Container timeout | `export DOCKER_TIMEOUT_SECONDS=300` |
| Files not visible | `docker run --rm -v /path:/workspace ggen-agent:latest ls -la /workspace` |

## Performance

| Operation | Time |
|-----------|------|
| Image init (first) | 2-3 sec |
| Image init (cached) | 0.5 sec |
| Container spawn | 1-2 sec |
| Total per agent | 1-5 sec |

## Error Messages

```
"Docker is not installed"
→ Install Docker, see DOCKER_INTEGRATION.md

"Docker daemon is not running"
→ Start Docker: sudo systemctl start docker

"Permission denied accessing Docker socket"
→ Add to group: sudo usermod -aG docker $USER

"Container timeout after 120s"
→ Increase timeout: export DOCKER_TIMEOUT_SECONDS=300

"Failed to build image"
→ Check log: cat /workspace/docker-build.log
```

## Function Signatures

### Image Management
```bash
docker_init_image()
# Returns: image_id (stdout) or exit code 1

docker_image_exists(image_name)
# Returns: 0 if exists, 1 if not

docker_create_minimal_image()
# Returns: 0 on success, 1 on failure
```

### Container Execution
```bash
docker_spawn_agent(agent_id, workspace_path, command)
# Returns: exit_code (from container), output to stdout
# Creates: .docker-result-{agent_id}, .docker-output-*, .docker-error-*

docker_ensure_network(network_name)
# Returns: 0 if network exists/created, 1 on failure
```

### Bootstrap Integration
```bash
bootstrap_docker_init()
# Returns: 0 on success, 1 on failure
# Sets: $DOCKER_ENABLED environment variable

bootstrap_docker_execute(agent_id, workspace, command)
# Returns: agent exit code
# Creates: .docker-result-{agent_id}
```

### Utilities
```bash
docker_health_check()
# Returns: 0 if all checks pass, 1 on issues

docker_cleanup()
# Returns: none
# Action: Stops all ggen-* containers, removes network

docker_check_availability()
# Returns: 0 if Docker running, 1 if not

docker_check_socket_permissions()
# Returns: 0 if accessible, 1 if not
```

## Monitoring Commands

```bash
# View active containers
docker_view_containers

# View network details
docker_view_network

# View container logs
docker logs <container_id>

# View stats
docker stats ggen-*

# List all containers
docker ps -a | grep ggen

# Inspect network
docker network inspect ggen-isolated
```

## Cleanup Commands

```bash
# Stop all ggen containers
docker container stop $(docker ps -q --filter="name=ggen-*")

# Remove all ggen containers
docker container rm $(docker ps -aq --filter="name=ggen-*")

# Remove isolated network
docker network rm ggen-isolated

# Prune unused resources
docker system prune
```

## Best Practices

✅ DO:
- Initialize image once per session: `bootstrap_docker_init`
- Check Docker availability: `docker_health_check`
- Mount workspace at `/workspace`
- Capture exit codes for error handling
- Use environment variables for configuration
- Clean up on shutdown: `docker_cleanup`

❌ DON'T:
- Create containers without mounting workspace
- Ignore exit codes
- Run containers without timeout
- Skip health checks in production
- Modify resource limits without testing
- Accumulate dead containers

## Documentation

- **Complete Guide:** [DOCKER_INTEGRATION.md](DOCKER_INTEGRATION.md)
- **Implementation:** [DOCKER_INTEGRATION_CHECKLIST.md](DOCKER_INTEGRATION_CHECKLIST.md)
- **Deployment:** [DOCKER_DEPLOYMENT_SUMMARY.md](DOCKER_DEPLOYMENT_SUMMARY.md)
- **Tests:** [tests/test-docker-runner.sh](tests/test-docker-runner.sh)
- **Module:** [modules/docker-runner.sh](modules/docker-runner.sh)

## Support

Issues? Check:
1. [DOCKER_INTEGRATION.md](DOCKER_INTEGRATION.md) - Complete reference
2. `docker_health_check` - Verify system
3. [tests/test-docker-runner.sh](tests/test-docker-runner.sh) - Run tests
4. Docker logs: `docker logs <id>`

---

**Version:** 1.0.0 | **Status:** ✅ Production Ready | **Last Updated:** 2026-01-29
