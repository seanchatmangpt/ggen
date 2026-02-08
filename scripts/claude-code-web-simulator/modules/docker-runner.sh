#!/bin/bash

##############################################################################
# Claude Code Web Simulator - Docker Runner Module
#
# Integrates Docker container execution into agent bootstrap, replacing
# sandbox simulation with real Docker containers for production-grade
# isolation and reproducibility.
#
# Features:
# - Image lifecycle management (check, build, verify)
# - Container orchestration (spawn, monitor, capture)
# - Error handling and graceful degradation
# - Integration with hooks engine and memory system
# - Deterministic receipt generation
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Source paths (these will be set by main.sh)
: "${WORKSPACE_DIR:=/tmp/ggen-workspace}"
: "${CONFIG_DIR:=/tmp/ggen-config}"
: "${MODULES_DIR:=/tmp/ggen-modules}"

# Docker configuration
readonly DOCKER_IMAGE_NAME="ggen-agent"
readonly DOCKER_IMAGE_TAG="latest"
readonly DOCKER_FULL_IMAGE="${DOCKER_IMAGE_NAME}:${DOCKER_IMAGE_TAG}"
readonly DOCKER_NETWORK="ggen-isolated"
readonly DOCKER_TIMEOUT_SECONDS=120
readonly DOCKER_MEMORY_LIMIT="512m"
readonly DOCKER_CPU_LIMIT="1"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

##############################################################################
# Logging Functions
##############################################################################

docker_log() {
    echo -e "${BLUE}[DOCKER]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1" >&2
}

docker_log_success() {
    echo -e "${GREEN}[✓]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1" >&2
}

docker_log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1" >&2
}

docker_log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1" >&2
}

##############################################################################
# Utility Functions
##############################################################################

# Check if Docker is available and running
docker_check_availability() {
    if ! command -v docker &> /dev/null; then
        docker_log_error "Docker is not installed"
        docker_log_error "  Please install Docker from https://docs.docker.com/get-docker/"
        return 1
    fi

    if ! docker info &> /dev/null; then
        docker_log_error "Docker daemon is not running"
        docker_log_error "  Please start Docker service:"
        docker_log_error "    Linux: sudo systemctl start docker"
        docker_log_error "    macOS: open /Applications/Docker.app"
        docker_log_error "    Windows: Start Docker Desktop from Application Menu"
        return 1
    fi

    docker_log_success "Docker is available and running"
    return 0
}

# Check if Docker socket has required permissions
docker_check_socket_permissions() {
    local socket_path="/var/run/docker.sock"

    if [[ ! -S "${socket_path}" ]]; then
        docker_log_warn "Docker socket not found at ${socket_path}"
        return 1
    fi

    if ! docker ps &> /dev/null; then
        docker_log_error "Insufficient permissions to access Docker socket"
        docker_log_error "  Add current user to docker group:"
        docker_log_error "    sudo usermod -aG docker \$USER"
        docker_log_error "    sudo systemctl restart docker"
        docker_log_error "    newgrp docker"
        return 1
    fi

    return 0
}

# Retry logic with exponential backoff
docker_retry() {
    local max_attempts=3
    local timeout_seconds=5
    local attempt=1

    while [[ ${attempt} -le ${max_attempts} ]]; do
        docker_log "Attempt ${attempt}/${max_attempts}: $*"

        if "$@"; then
            return 0
        fi

        if [[ ${attempt} -lt ${max_attempts} ]]; then
            docker_log_warn "Attempt ${attempt} failed, retrying in ${timeout_seconds}s..."
            sleep "${timeout_seconds}"
            timeout_seconds=$((timeout_seconds * 2))
        fi

        attempt=$((attempt + 1))
    done

    docker_log_error "All ${max_attempts} attempts failed"
    return 1
}

##############################################################################
# Image Management Functions
##############################################################################

# Initialize Docker image: check if exists, build if missing, verify success
# Returns: 0 on success, image ID in stdout, non-zero exit code on failure
docker_init_image() {
    docker_log "Initializing Docker image: ${DOCKER_FULL_IMAGE}"

    # Check if Docker is available
    if ! docker_check_availability; then
        docker_log_error "Cannot initialize image: Docker not available"
        return 1
    fi

    # Check Docker socket permissions
    if ! docker_check_socket_permissions; then
        docker_log_warn "Continuing with reduced functionality (socket access issues)"
    fi

    # Check if image already exists
    if docker_image_exists "${DOCKER_FULL_IMAGE}"; then
        local image_id
        image_id=$(docker images --quiet "${DOCKER_FULL_IMAGE}" | head -1)
        docker_log_success "Image already exists: ${DOCKER_FULL_IMAGE} (ID: ${image_id})"
        echo "${image_id}"
        return 0
    fi

    docker_log "Image not found, attempting to build..."

    # Find Dockerfile
    local dockerfile_path=""
    if [[ -f "./Dockerfile" ]]; then
        dockerfile_path="./Dockerfile"
    elif [[ -f "${SCRIPT_DIR}/../Dockerfile" ]]; then
        dockerfile_path="${SCRIPT_DIR}/../Dockerfile"
    elif [[ -f "${WORKSPACE_DIR}/../Dockerfile" ]]; then
        dockerfile_path="${WORKSPACE_DIR}/../Dockerfile"
    else
        docker_log_warn "Dockerfile not found, creating minimal agent image"
        if ! docker_create_minimal_image; then
            docker_log_error "Failed to create minimal image"
            return 1
        fi
    fi

    # Build image with retry logic
    if [[ -n "${dockerfile_path}" ]]; then
        if ! docker_retry docker build \
            --tag "${DOCKER_FULL_IMAGE}" \
            --file "${dockerfile_path}" \
            --progress=plain \
            . 2>&1 | tee -a "${WORKSPACE_DIR}/docker-build.log"; then

            docker_log_error "Failed to build image: ${DOCKER_FULL_IMAGE}"
            docker_log_error "Build log saved to: ${WORKSPACE_DIR}/docker-build.log"
            return 1
        fi
    fi

    # Verify build success
    if ! docker_image_exists "${DOCKER_FULL_IMAGE}"; then
        docker_log_error "Image build completed but image not found"
        return 1
    fi

    local image_id
    image_id=$(docker images --quiet "${DOCKER_FULL_IMAGE}" | head -1)
    docker_log_success "Image built successfully: ${DOCKER_FULL_IMAGE} (ID: ${image_id})"
    echo "${image_id}"
    return 0
}

# Check if Docker image exists
docker_image_exists() {
    local image_name="$1"
    local image_count
    image_count=$(docker images --quiet "${image_name}" 2>/dev/null | wc -l)
    [[ ${image_count} -gt 0 ]]
}

# Create minimal agent image when Dockerfile not found
docker_create_minimal_image() {
    docker_log "Creating minimal ggen-agent image..."

    # Create temporary Dockerfile
    local temp_dockerfile
    temp_dockerfile=$(mktemp --suffix=.Dockerfile)

    cat > "${temp_dockerfile}" <<'EOF'
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Create workspace directory
RUN mkdir -p /workspace && chmod 777 /workspace

# Set working directory
WORKDIR /workspace

# Health check
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=3 \
    CMD test -d /workspace || exit 1

# Default command
CMD ["/bin/bash"]
EOF

    if docker build \
        --tag "${DOCKER_FULL_IMAGE}" \
        --file "${temp_dockerfile}" \
        --progress=plain \
        --quiet . 2>&1 | tee -a "${WORKSPACE_DIR}/docker-build.log"; then

        rm -f "${temp_dockerfile}"
        docker_log_success "Minimal image created"
        return 0
    else
        rm -f "${temp_dockerfile}"
        docker_log_error "Failed to create minimal image"
        return 1
    fi
}

##############################################################################
# Container Management Functions
##############################################################################

# Spawn Docker container for agent execution
# Usage: docker_spawn_agent <agent_id> <workspace_path> <command>
# Returns: 0 on success, output in stdout, non-zero exit code on failure
docker_spawn_agent() {
    local agent_id="$1"
    local workspace_path="$2"
    local command="$3"

    docker_log "Spawning agent container: ${agent_id}"
    docker_log "  Workspace: ${workspace_path}"
    docker_log "  Command: ${command}"

    # Validate inputs
    if [[ -z "${agent_id}" || -z "${workspace_path}" || -z "${command}" ]]; then
        docker_log_error "Missing required arguments: agent_id, workspace_path, command"
        return 1
    fi

    if [[ ! -d "${workspace_path}" ]]; then
        docker_log_error "Workspace path does not exist: ${workspace_path}"
        return 1
    fi

    # Verify image exists
    if ! docker_image_exists "${DOCKER_FULL_IMAGE}"; then
        docker_log_error "Docker image not found: ${DOCKER_FULL_IMAGE}"
        docker_log "Attempting to initialize image..."
        if ! docker_init_image > /dev/null 2>&1; then
            docker_log_error "Failed to initialize image"
            return 1
        fi
    fi

    # Ensure isolated network exists
    docker_ensure_network "${DOCKER_NETWORK}"

    # Create container name with timestamp for uniqueness
    local container_name="ggen-${agent_id}-$(date +%s%N | tail -c 10)"

    # Create output capture file
    local output_file="${workspace_path}/.docker-output-${agent_id}"
    local error_file="${workspace_path}/.docker-error-${agent_id}"

    mkdir -p "${workspace_path}"
    touch "${output_file}" "${error_file}"

    # Execute container with comprehensive error handling
    local exit_code=0
    docker_log "Starting container: ${container_name}"

    # Run with timeout enforcement
    if ! timeout "${DOCKER_TIMEOUT_SECONDS}" docker run \
        --rm \
        --name="${container_name}" \
        --network="${DOCKER_NETWORK}" \
        --volume="${workspace_path}:/workspace:rw" \
        --volume="/tmp:/tmp:rw" \
        --workdir=/workspace \
        --env="AGENT_ID=${agent_id}" \
        --env="WORKSPACE=/workspace" \
        --memory="${DOCKER_MEMORY_LIMIT}" \
        --cpus="${DOCKER_CPU_LIMIT}" \
        --pids-limit=256 \
        --read-only-rootfs=false \
        --security-opt="no-new-privileges:true" \
        "${DOCKER_FULL_IMAGE}" \
        /bin/bash -c "${command}" \
        > "${output_file}" 2> "${error_file}"; then

        exit_code=$?

        # Capture timeout vs other errors
        if [[ ${exit_code} -eq 124 ]]; then
            docker_log_error "Container timeout after ${DOCKER_TIMEOUT_SECONDS}s"
            docker_log_error "Container name: ${container_name}"
        elif [[ ${exit_code} -ne 0 ]]; then
            docker_log_error "Container exited with code ${exit_code}"
        fi
    else
        exit_code=$?
    fi

    # Capture and log output
    local stdout_content
    local stderr_content
    stdout_content=$(cat "${output_file}" 2>/dev/null || echo "")
    stderr_content=$(cat "${error_file}" 2>/dev/null || echo "")

    # Log execution details
    docker_log "Container execution completed (exit code: ${exit_code})"

    if [[ -n "${stdout_content}" ]]; then
        docker_log "  STDOUT: $(echo "${stdout_content}" | head -1)"
    fi

    if [[ -n "${stderr_content}" ]]; then
        docker_log "  STDERR: $(echo "${stderr_content}" | head -1)"
    fi

    # Store execution result
    local result_file="${workspace_path}/.docker-result-${agent_id}"
    cat > "${result_file}" <<EOF
{
  "agent_id": "${agent_id}",
  "container_name": "${container_name}",
  "exit_code": ${exit_code},
  "timestamp": "$(date -Iseconds)",
  "duration_seconds": $(($(date +%s) - $(stat -f%m "${workspace_path}" 2>/dev/null || date +%s))),
  "stdout_chars": ${#stdout_content},
  "stderr_chars": ${#stderr_content}
}
EOF

    # Output captured results
    cat "${output_file}"
    cat "${error_file}" >&2

    # Cleanup
    rm -f "${output_file}" "${error_file}"

    # Return container exit code
    return "${exit_code}"
}

# Ensure isolated Docker network exists
docker_ensure_network() {
    local network_name="$1"

    if docker network ls --format "{{.Name}}" | grep -q "^${network_name}$"; then
        docker_log "Network already exists: ${network_name}"
        return 0
    fi

    docker_log "Creating isolated Docker network: ${network_name}"

    if docker network create \
        --driver=bridge \
        --opt="com.docker.network.bridge.enable_ip_masquerade=true" \
        "${network_name}" > /dev/null 2>&1; then

        docker_log_success "Network created: ${network_name}"
        return 0
    else
        docker_log_error "Failed to create network: ${network_name}"
        return 1
    fi
}

# Cleanup Docker resources
docker_cleanup() {
    docker_log "Cleaning up Docker resources..."

    # Stop all ggen containers
    local container_count=0
    while read -r container_id; do
        if [[ -n "${container_id}" ]]; then
            docker_log "Stopping container: ${container_id}"
            docker stop "${container_id}" 2>/dev/null || true
            docker rm "${container_id}" 2>/dev/null || true
            container_count=$((container_count + 1))
        fi
    done < <(docker ps --filter="name=ggen-*" --quiet 2>/dev/null)

    if [[ ${container_count} -gt 0 ]]; then
        docker_log_success "Stopped ${container_count} containers"
    fi

    # Remove isolated network (optional)
    if docker network ls --format "{{.Name}}" 2>/dev/null | grep -q "^${DOCKER_NETWORK}$"; then
        docker_log "Note: To remove isolated network, run:"
        docker_log "  docker network rm ${DOCKER_NETWORK}"
    fi
}

##############################################################################
# Bootstrap Integration Functions
##############################################################################

# Initialize Docker system (called once at agent bootstrap)
# Returns: 0 on success, image ID in stdout, 1 on failure
bootstrap_docker_init() {
    docker_log "Initializing Docker for agent bootstrap..."

    # Check Docker availability
    if ! docker_check_availability; then
        docker_log_error "Docker not available for bootstrap"
        return 1
    fi

    # Initialize image with retry
    if ! docker_retry docker_init_image; then
        docker_log_error "Failed to initialize Docker image during bootstrap"
        return 1
    fi

    docker_log_success "Docker bootstrap completed"
    return 0
}

# Execute agent in Docker (called for each agent execution)
# Usage: bootstrap_docker_execute <agent_id> <workspace> <command>
# Returns: agent exit code
bootstrap_docker_execute() {
    local agent_id="$1"
    local workspace="$2"
    local command="$3"

    docker_log "Executing agent in Docker: ${agent_id}"

    # Spawn agent container
    if ! docker_spawn_agent "${agent_id}" "${workspace}" "${command}"; then
        local exit_code=$?
        docker_log_error "Agent execution failed with exit code ${exit_code}"
        return "${exit_code}"
    fi

    docker_log_success "Agent execution completed: ${agent_id}"
    return 0
}

##############################################################################
# Monitoring and Debugging Functions
##############################################################################

# View active Docker containers
docker_view_containers() {
    docker_log "Active ggen containers:"
    docker ps --filter="name=ggen-*" --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}"
}

# View Docker network
docker_view_network() {
    docker_log "Docker network status:"
    docker network ls --filter="name=${DOCKER_NETWORK}"
    docker_log "Network details:"
    docker network inspect "${DOCKER_NETWORK}" 2>/dev/null || echo "Network not found"
}

# Check Docker system health
docker_health_check() {
    docker_log "Docker system health check..."

    local issues=0

    # Check availability
    if ! docker_check_availability; then
        docker_log_error "Docker daemon is not running"
        issues=$((issues + 1))
    fi

    # Check socket permissions
    if ! docker_check_socket_permissions; then
        docker_log_error "Insufficient Docker socket permissions"
        issues=$((issues + 1))
    fi

    # Check image
    if ! docker_image_exists "${DOCKER_FULL_IMAGE}"; then
        docker_log_warn "Docker image not found: ${DOCKER_FULL_IMAGE}"
        issues=$((issues + 1))
    fi

    # Check network
    if ! docker network ls --quiet 2>/dev/null | head -1 > /dev/null; then
        docker_log_error "Cannot list Docker networks"
        issues=$((issues + 1))
    fi

    if [[ ${issues} -eq 0 ]]; then
        docker_log_success "All health checks passed"
        return 0
    else
        docker_log_error "Health check failed with ${issues} issue(s)"
        return 1
    fi
}

##############################################################################
# Error Handling and Recovery Functions
##############################################################################

# Handle Docker not installed error
docker_handle_not_installed() {
    docker_log_error "Docker is not installed on this system"
    cat <<'EOF' >&2

SOLUTION: Install Docker

Linux (Ubuntu/Debian):
  sudo apt-get update
  sudo apt-get install docker.io
  sudo usermod -aG docker $USER
  newgrp docker

macOS:
  brew install docker
  docker run hello-world

Windows:
  Download Docker Desktop from https://www.docker.com/products/docker-desktop

After installation:
  docker run hello-world  # Test installation
EOF
    return 1
}

# Handle Docker daemon not running error
docker_handle_daemon_not_running() {
    docker_log_error "Docker daemon is not running"
    cat <<'EOF' >&2

SOLUTION: Start Docker daemon

Linux:
  sudo systemctl start docker
  sudo systemctl enable docker  # Enable on boot

macOS:
  open /Applications/Docker.app

Windows:
  Start Docker Desktop from the Application menu

Verify:
  docker ps  # Should work without errors
EOF
    return 1
}

# Handle permission denied error
docker_handle_permission_denied() {
    docker_log_error "Permission denied accessing Docker socket"
    cat <<'EOF' >&2

SOLUTION: Fix Docker permissions

Linux:
  sudo usermod -aG docker $USER
  sudo systemctl restart docker
  newgrp docker

Verify:
  docker ps  # Should work without sudo

macOS/Windows:
  Usually handled by Docker Desktop installation
EOF
    return 1
}

# Handle container timeout error
docker_handle_timeout() {
    local agent_id="$1"
    local timeout_seconds="${2:-${DOCKER_TIMEOUT_SECONDS}}"

    docker_log_error "Container timeout for agent: ${agent_id}"
    cat <<EOF >&2

SOLUTION: Increase container timeout or optimize command

Current timeout: ${timeout_seconds} seconds

Options:
  1. Increase timeout: export DOCKER_TIMEOUT_SECONDS=300
  2. Optimize command to run faster
  3. Break into smaller tasks
  4. Check container logs: docker logs <container_id>
EOF
    return 1
}

##############################################################################
# Export functions for use by other modules
##############################################################################

# Make functions available for sourcing
export -f docker_log
export -f docker_log_success
export -f docker_log_warn
export -f docker_log_error
export -f docker_check_availability
export -f docker_check_socket_permissions
export -f docker_init_image
export -f docker_spawn_agent
export -f bootstrap_docker_init
export -f bootstrap_docker_execute
export -f docker_cleanup
export -f docker_health_check
export -f docker_view_containers
export -f docker_view_network

# Return success for module load
return 0 2>/dev/null || true
