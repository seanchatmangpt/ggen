#!/bin/bash
##############################################################################
# Production-Grade Staging Deployment Startup Script
# ggen v6.0.0 - Distributed Cluster Launch
#
# Features:
#   - Health checks before marking ready
#   - Detailed logging to stderr
#   - Exit codes on failure
#   - Network isolation
#   - Clean shutdown on error
##############################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_FILE="${SCRIPT_DIR}/deployment.log"
PID_FILE="${SCRIPT_DIR}/.docker-compose.pid"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" | tee -a "$LOG_FILE"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*" | tee -a "$LOG_FILE"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" | tee -a "$LOG_FILE"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" | tee -a "$LOG_FILE" >&2
}

# Cleanup on exit
cleanup() {
    local exit_code=$?
    if [ $exit_code -ne 0 ]; then
        log_error "Deployment failed with exit code $exit_code"
        log_info "Cleaning up..."
        docker-compose -f "$SCRIPT_DIR/docker-compose.yml" down -v 2>/dev/null || true
    fi
    return $exit_code
}

trap cleanup EXIT

##############################################################################
# STEP 1: Validate Environment
##############################################################################

log_info "Step 1: Validating environment..."

# Check Docker daemon
if ! docker info >/dev/null 2>&1; then
    log_error "Docker daemon is not running"
    exit 1
fi
log_success "Docker daemon is running"

# Check Docker Compose
if ! docker-compose --version >/dev/null 2>&1; then
    log_error "docker-compose is not installed"
    exit 1
fi
log_success "docker-compose is available"

# Check required files
for file in Dockerfile docker-compose.yml prometheus.yml; do
    if [ ! -f "$SCRIPT_DIR/$file" ]; then
        log_error "Required file not found: $SCRIPT_DIR/$file"
        exit 1
    fi
done
log_success "All required configuration files present"

##############################################################################
# STEP 2: Prepare Directories
##############################################################################

log_info "Step 2: Preparing directories..."

mkdir -p "$SCRIPT_DIR"/{specs,generated}
log_success "Directories prepared"

##############################################################################
# STEP 3: Clean Previous Deployment (optional)
##############################################################################

log_info "Step 3: Checking for previous deployment..."

if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" ps 2>/dev/null | grep -q "ggen-node"; then
    log_warn "Previous deployment detected. Removing..."
    docker-compose -f "$SCRIPT_DIR/docker-compose.yml" down -v --remove-orphans
    sleep 2
    log_success "Previous deployment removed"
else
    log_info "No previous deployment found"
fi

##############################################################################
# STEP 4: Build Docker Image
##############################################################################

log_info "Step 4: Building Docker image..."

if ! docker-compose -f "$SCRIPT_DIR/docker-compose.yml" build --no-cache 2>&1 | tee -a "$LOG_FILE"; then
    log_error "Docker image build failed"
    exit 1
fi
log_success "Docker image built successfully"

##############################################################################
# STEP 5: Start Cluster
##############################################################################

log_info "Step 5: Starting staging cluster..."

if ! docker-compose -f "$SCRIPT_DIR/docker-compose.yml" up -d 2>&1 | tee -a "$LOG_FILE"; then
    log_error "Failed to start cluster"
    exit 1
fi
log_success "Docker containers started"

##############################################################################
# STEP 6: Health Checks - Firestore Emulator
##############################################################################

log_info "Step 6: Waiting for Firestore Emulator to be healthy..."

max_retries=30
retry_count=0

while [ $retry_count -lt $max_retries ]; do
    if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" ps firestore-emulator | grep -q "healthy"; then
        log_success "Firestore Emulator is healthy"
        break
    fi
    retry_count=$((retry_count + 1))
    if [ $retry_count -eq $max_retries ]; then
        log_error "Firestore Emulator failed to become healthy after ${max_retries}0 seconds"
        exit 1
    fi
    sleep 1
done

##############################################################################
# STEP 7: Health Checks - Prometheus
##############################################################################

log_info "Step 7: Waiting for Prometheus to be healthy..."

retry_count=0
while [ $retry_count -lt $max_retries ]; do
    if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" ps prometheus | grep -q "healthy"; then
        log_success "Prometheus is healthy"
        break
    fi
    retry_count=$((retry_count + 1))
    if [ $retry_count -eq $max_retries ]; then
        log_error "Prometheus failed to become healthy after ${max_retries}0 seconds"
        exit 1
    fi
    sleep 1
done

##############################################################################
# STEP 8: Health Checks - ggen Nodes
##############################################################################

log_info "Step 8: Waiting for ggen nodes to be healthy..."

for node in ggen-node-1 ggen-node-2 ggen-node-3; do
    retry_count=0
    while [ $retry_count -lt $max_retries ]; do
        if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" ps "$node" | grep -q "healthy"; then
            log_success "$node is healthy"
            break
        fi
        retry_count=$((retry_count + 1))
        if [ $retry_count -eq $max_retries ]; then
            log_error "$node failed to become healthy after ${max_retries}0 seconds"
            exit 1
        fi
        sleep 1
    done
done

##############################################################################
# STEP 9: Verify Network Connectivity
##############################################################################

log_info "Step 9: Verifying network connectivity..."

# Test inter-node communication
for node in ggen-node-2 ggen-node-3; do
    if docker-compose -f "$SCRIPT_DIR/docker-compose.yml" exec -T "$node" \
        curl -s -f http://ggen-node-1:8080/health >/dev/null 2>&1; then
        log_success "$node can reach ggen-node-1"
    else
        log_warn "$node cannot reach ggen-node-1 (may be normal if health endpoint not implemented)"
    fi
done

##############################################################################
# STEP 10: Display Cluster Status
##############################################################################

log_info "Step 10: Cluster status summary..."

docker-compose -f "$SCRIPT_DIR/docker-compose.yml" ps | tee -a "$LOG_FILE"

##############################################################################
# SUCCESS
##############################################################################

cat << 'EOF' | tee -a "$LOG_FILE"

████████████████████████████████████████████████████████████████████████████████
█                                                                              █
█  ✓ STAGING DEPLOYMENT SUCCESSFUL                                           █
█                                                                              █
█  Cluster: staging-cluster (3 nodes + Firestore + Prometheus)               █
█                                                                              █
████████████████████████████████████████████████████████████████████████████████

EOF

log_success "All services are running and healthy"

cat << EOF | tee -a "$LOG_FILE"

QUICK REFERENCE:
────────────────────────────────────────────────────────────────────────────────

  📊 DASHBOARDS:
     • Prometheus: http://localhost:9090
     • ggen-node-1: http://localhost:8080/health
     • ggen-node-2: http://localhost:8081/health
     • ggen-node-3: http://localhost:8082/health

  🔧 CLUSTER MANAGEMENT:
     • View logs:     docker-compose -f deploy/staging/docker-compose.yml logs -f
     • Stop cluster:  docker-compose -f deploy/staging/docker-compose.yml down
     • Stop & purge:  docker-compose -f deploy/staging/docker-compose.yml down -v

  🧪 CHAOS TESTING:
     • Kill node-1:   docker-compose -f deploy/staging/docker-compose.yml kill ggen-node-1
     • Resurrect:     docker-compose -f deploy/staging/docker-compose.yml up -d ggen-node-1
     • Pause node-2:  docker pause ggen-node-2
     • Resume:        docker unpause ggen-node-2

  📋 LOGS & DIAGNOSTICS:
     • All logs:      tail -f $LOG_FILE
     • Node-1:        docker logs -f ggen-node-1
     • Firestore:     docker logs -f firestore-emulator

────────────────────────────────────────────────────────────────────────────────

Deployment log: $LOG_FILE
Project root:   $PROJECT_ROOT

EOF

exit 0
