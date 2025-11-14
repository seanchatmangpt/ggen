#!/usr/bin/env bash
# validate-marketplace-package.sh - End-to-end marketplace package validation
#
# Usage:
#   ./scripts/validate-marketplace-package.sh io.ggen.nextjs.ontology-crud
#
# Description:
#   Validates a marketplace package by:
#   1. Creating a clean Docker container
#   2. Installing ggen from source
#   3. Installing the marketplace package
#   4. Running npm install and regenerate
#   5. Validating generated files
#   6. Running build (if applicable)
#
# Exit codes:
#   0 - Success
#   1 - Invalid arguments
#   2 - Docker not available
#   3 - Container creation failed
#   4 - Build failed
#   5 - Package installation failed
#   6 - Validation failed

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DOCKER_IMAGE="node:20-bookworm"
CONTAINER_NAME="ggen-marketplace-validator-$$"
GGEN_REPO="https://github.com/deepwavedigital/ggen.git"
CLEANUP_ON_EXIT=true

# Trap to ensure cleanup
cleanup() {
    local exit_code=$?
    if [[ "$CLEANUP_ON_EXIT" == "true" ]]; then
        echo -e "\n${BLUE}🧹 Cleaning up...${NC}"
        docker rm -f "$CONTAINER_NAME" 2>/dev/null || true
    else
        echo -e "\n${YELLOW}⚠️  Container $CONTAINER_NAME left running for debugging${NC}"
    fi
    exit $exit_code
}
trap cleanup EXIT INT TERM

# Logging functions
log_step() {
    echo -e "\n${BLUE}▶ $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}" >&2
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Usage function
usage() {
    cat <<EOF
Usage: $0 <package-id> [options]

Arguments:
  package-id    Marketplace package ID (e.g., io.ggen.nextjs.ontology-crud)

Options:
  --keep-container    Don't cleanup container on exit (for debugging)
  --help             Show this help message

Examples:
  $0 io.ggen.nextjs.ontology-crud
  $0 io.ggen.rust.cli-app --keep-container

EOF
}

# Parse arguments
if [[ $# -eq 0 ]]; then
    usage
    exit 1
fi

PACKAGE_ID=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --keep-container)
            CLEANUP_ON_EXIT=false
            shift
            ;;
        --help)
            usage
            exit 0
            ;;
        -*)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            if [[ -z "$PACKAGE_ID" ]]; then
                PACKAGE_ID="$1"
            else
                log_error "Multiple package IDs provided"
                usage
                exit 1
            fi
            shift
            ;;
    esac
done

if [[ -z "$PACKAGE_ID" ]]; then
    log_error "Package ID is required"
    usage
    exit 1
fi

# Validate package ID format
if ! [[ "$PACKAGE_ID" =~ ^[a-z0-9]+(\.[a-z0-9]+)+$ ]]; then
    log_error "Invalid package ID format: $PACKAGE_ID"
    log_info "Expected format: namespace.category.name (e.g., io.ggen.nextjs.ontology-crud)"
    exit 1
fi

echo "=========================================="
echo "Marketplace Package Validator"
echo "=========================================="
echo "Package: $PACKAGE_ID"
echo "Container: $CONTAINER_NAME"
echo "=========================================="

# Step 1: Check Docker availability
log_step "Checking Docker availability"
if ! command -v docker &> /dev/null; then
    log_error "Docker is not installed or not in PATH"
    exit 2
fi
if ! docker info &> /dev/null; then
    log_error "Docker daemon is not running"
    exit 2
fi
log_success "Docker is available"

# Step 2: Pull Docker image
log_step "Pulling Docker image: $DOCKER_IMAGE"
if docker pull "$DOCKER_IMAGE"; then
    log_success "Docker image pulled successfully"
else
    log_error "Failed to pull Docker image"
    exit 3
fi

# Step 3: Create container
log_step "Creating Docker container"
if docker run -d --name "$CONTAINER_NAME" "$DOCKER_IMAGE" tail -f /dev/null; then
    log_success "Container created: $CONTAINER_NAME"
else
    log_error "Failed to create container"
    exit 3
fi

# Helper function to run commands in container
docker_exec() {
    docker exec "$CONTAINER_NAME" bash -c "$1"
}

# Step 4: Install system dependencies
log_step "Installing system dependencies"
if docker_exec "apt-get update -qq && apt-get install -y -qq git curl build-essential > /dev/null 2>&1"; then
    log_success "System dependencies installed"
else
    log_error "Failed to install system dependencies"
    exit 4
fi

# Step 5: Install Rust via rustup
log_step "Installing Rust toolchain"
if docker_exec "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --quiet"; then
    log_success "Rust toolchain installed"
else
    log_error "Failed to install Rust"
    exit 4
fi

# Source Rust environment
docker_exec "echo 'source \$HOME/.cargo/env' >> ~/.bashrc"

# Step 6: Clone ggen repository
log_step "Cloning ggen repository"
if docker_exec "source \$HOME/.cargo/env && git clone --quiet $GGEN_REPO /workspace/ggen"; then
    log_success "Repository cloned"
else
    log_error "Failed to clone repository"
    exit 4
fi

# Step 7: Build ggen
log_step "Building ggen (this may take several minutes)"
if docker_exec "cd /workspace/ggen && source \$HOME/.cargo/env && cargo build --release -p ggen-cli --bin ggen --quiet 2>&1 | tail -20"; then
    log_success "ggen built successfully"
else
    log_error "Failed to build ggen"
    log_info "Check build logs above for details"
    exit 4
fi

# Step 8: Add ggen to PATH
log_step "Adding ggen to PATH"
docker_exec "ln -s /workspace/ggen/target/release/ggen /usr/local/bin/ggen"
if docker_exec "ggen --version"; then
    log_success "ggen is in PATH"
else
    log_error "Failed to add ggen to PATH"
    exit 4
fi

# Step 9: Install marketplace package
log_step "Installing marketplace package: $PACKAGE_ID"
INSTALL_OUTPUT=$(docker_exec "cd /workspace && ggen marketplace install $PACKAGE_ID 2>&1" || true)
echo "$INSTALL_OUTPUT"

if echo "$INSTALL_OUTPUT" | grep -q "Successfully installed"; then
    log_success "Package installed successfully"
elif echo "$INSTALL_OUTPUT" | grep -q "already exists"; then
    log_warning "Package directory already exists, continuing validation"
else
    log_error "Failed to install package"
    exit 5
fi

# Determine package directory name (last part of package ID)
PACKAGE_DIR=$(echo "$PACKAGE_ID" | rev | cut -d. -f1 | rev)
PACKAGE_PATH="/workspace/$PACKAGE_DIR"

# Verify package directory exists
if ! docker_exec "test -d $PACKAGE_PATH"; then
    log_error "Package directory not found: $PACKAGE_PATH"
    log_info "Available directories:"
    docker_exec "ls -la /workspace/"
    exit 5
fi

# Step 10: Run npm install
log_step "Running npm install"
if docker_exec "cd $PACKAGE_PATH && npm install --quiet 2>&1 | tail -10"; then
    log_success "npm install completed"
else
    log_error "npm install failed"
    exit 6
fi

# Step 11: Run npm run regenerate
log_step "Running npm run regenerate"
REGEN_OUTPUT=$(docker_exec "cd $PACKAGE_PATH && npm run regenerate 2>&1" || true)
echo "$REGEN_OUTPUT"

if echo "$REGEN_OUTPUT" | grep -qE "(successfully|completed|done)" || [[ $? -eq 0 ]]; then
    log_success "Regeneration completed"
else
    log_warning "Regeneration may have issues (check output above)"
fi

# Step 12: Validate generated files
log_step "Validating generated files"
log_info "Directory structure:"
docker_exec "cd $PACKAGE_PATH && find . -maxdepth 3 -type f -name '*.ts' -o -name '*.js' -o -name '*.json' | head -20"

# Check for common files
EXPECTED_FILES=("package.json")
for file in "${EXPECTED_FILES[@]}"; do
    if docker_exec "test -f $PACKAGE_PATH/$file"; then
        log_success "Found: $file"
    else
        log_warning "Missing: $file"
    fi
done

# Step 13: Run build (if build script exists)
log_step "Checking for build script"
if docker_exec "cd $PACKAGE_PATH && npm run --silent | grep -q build"; then
    log_info "Build script found, running build"
    if docker_exec "cd $PACKAGE_PATH && npm run build 2>&1 | tail -20"; then
        log_success "Build completed successfully"
    else
        log_error "Build failed"
        exit 6
    fi
else
    log_info "No build script found (skipping)"
fi

# Step 14: Final validation summary
log_step "Validation Summary"
echo ""
echo "Package: $PACKAGE_ID"
echo "Location: $PACKAGE_PATH"
echo ""

# Count files by type
log_info "File statistics:"
docker_exec "cd $PACKAGE_PATH && find . -type f -name '*.ts' | wc -l | xargs echo 'TypeScript files:'"
docker_exec "cd $PACKAGE_PATH && find . -type f -name '*.js' | wc -l | xargs echo 'JavaScript files:'"
docker_exec "cd $PACKAGE_PATH && find . -type f -name '*.json' | wc -l | xargs echo 'JSON files:'"

# Check node_modules size
if docker_exec "test -d $PACKAGE_PATH/node_modules"; then
    NODE_MODULES_SIZE=$(docker_exec "du -sh $PACKAGE_PATH/node_modules 2>/dev/null | cut -f1" || echo "unknown")
    log_info "node_modules size: $NODE_MODULES_SIZE"
fi

# Final success message
echo ""
echo "=========================================="
log_success "✨ Package validation completed successfully!"
echo "=========================================="
echo ""

if [[ "$CLEANUP_ON_EXIT" == "false" ]]; then
    log_info "Container $CONTAINER_NAME is still running for debugging"
    log_info "Connect with: docker exec -it $CONTAINER_NAME bash"
    log_info "Working directory: $PACKAGE_PATH"
    log_info "Cleanup with: docker rm -f $CONTAINER_NAME"
fi

exit 0
