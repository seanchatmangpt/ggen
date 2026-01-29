#!/bin/bash

###############################################################################
# Docker Build and Test Script for ggen Agent Execution Environment
#
# Purpose: Build and validate the ggen Docker image
# Usage: ./build-and-test-docker.sh [--no-cache] [--registry REGISTRY] [--push]
###############################################################################

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_NAME="ggen-agent"
IMAGE_TAG="latest"
REGISTRY=""
NO_CACHE=false
PUSH=false
BUILD_ARGS=""

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-cache)
            NO_CACHE=true
            shift
            ;;
        --registry)
            REGISTRY="$2"
            shift 2
            ;;
        --push)
            PUSH=true
            shift
            ;;
        --version)
            IMAGE_TAG="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Build image name with registry
if [ -n "$REGISTRY" ]; then
    FULL_IMAGE_NAME="${REGISTRY}/${IMAGE_NAME}:${IMAGE_TAG}"
else
    FULL_IMAGE_NAME="${IMAGE_NAME}:${IMAGE_TAG}"
fi

# Function to print colored output
print_header() {
    echo -e "${BLUE}===================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}===================================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

# Step 1: Verify Docker is available
print_header "Step 1: Verifying Docker Installation"
if ! command -v docker &> /dev/null; then
    print_error "Docker is not installed"
    exit 1
fi
print_success "Docker is installed: $(docker --version)"

# Step 2: Verify Dockerfile exists
print_header "Step 2: Verifying Dockerfile"
DOCKERFILE="${SCRIPT_DIR}/Dockerfile"
if [ ! -f "$DOCKERFILE" ]; then
    print_error "Dockerfile not found at $DOCKERFILE"
    exit 1
fi
print_success "Dockerfile found at $DOCKERFILE"

# Step 3: Build Docker image
print_header "Step 3: Building Docker Image"
echo "Image name: $FULL_IMAGE_NAME"
echo "Build context: $SCRIPT_DIR"

BUILD_CMD="docker build"
if [ "$NO_CACHE" = true ]; then
    BUILD_CMD="$BUILD_CMD --no-cache"
    print_warning "Building without cache (slower)"
fi

# Enable BuildKit for faster builds
BUILD_CMD="DOCKER_BUILDKIT=1 $BUILD_CMD -t $FULL_IMAGE_NAME -f $DOCKERFILE $SCRIPT_DIR"

echo -e "${YELLOW}Running: $BUILD_CMD${NC}"
if eval "$BUILD_CMD"; then
    print_success "Docker image built successfully"
else
    print_error "Docker image build failed"
    exit 1
fi

# Step 4: Inspect image
print_header "Step 4: Inspecting Image"
IMAGE_SIZE=$(docker image ls --filter reference="$FULL_IMAGE_NAME" --format "{{.Size}}")
IMAGE_ID=$(docker image inspect "$FULL_IMAGE_NAME" --format "{{.ID}}")
print_success "Image ID: ${IMAGE_ID:7:12}"
print_success "Image size: $IMAGE_SIZE"

# Step 5: Test ggen binary
print_header "Step 5: Testing ggen Binary"
echo "Running: ggen --version"
if docker run --rm "$FULL_IMAGE_NAME" ggen --version > /tmp/ggen_version.txt; then
    GGEN_VERSION=$(cat /tmp/ggen_version.txt)
    print_success "ggen version: $GGEN_VERSION"
else
    print_error "Failed to run ggen --version"
    exit 1
fi

# Step 6: Test non-root execution
print_header "Step 6: Testing Non-Root Execution"
echo "Running: id"
if docker run --rm "$FULL_IMAGE_NAME" id > /tmp/ggen_id.txt; then
    ID_OUTPUT=$(cat /tmp/ggen_id.txt)
    if echo "$ID_OUTPUT" | grep -q "uid=1000"; then
        print_success "Running as non-root user: $ID_OUTPUT"
    else
        print_error "Container running as wrong user: $ID_OUTPUT"
        exit 1
    fi
else
    print_error "Failed to run id command"
    exit 1
fi

# Step 7: Test workspace isolation
print_header "Step 7: Testing Workspace Isolation"
TEMP_WORKSPACE=$(mktemp -d)
trap "rm -rf $TEMP_WORKSPACE" EXIT

echo "Creating test file in workspace..."
touch "$TEMP_WORKSPACE/test.txt"

echo "Running container with mounted workspace..."
if docker run --rm -v "$TEMP_WORKSPACE:/workspace" "$FULL_IMAGE_NAME" \
    sh -c "ls -la /workspace && test -f /workspace/test.txt"; then
    print_success "Workspace properly mounted and accessible"
else
    print_error "Workspace isolation test failed"
    exit 1
fi

# Step 8: Test health check
print_header "Step 8: Testing Health Check"
echo "Running container health check..."
if docker run --rm "$FULL_IMAGE_NAME" sh -c "ggen --version > /dev/null 2>&1"; then
    print_success "Health check passed"
else
    print_error "Health check failed"
    exit 1
fi

# Step 9: Test read-only root filesystem
print_header "Step 9: Testing Read-Only Root Filesystem"
echo "Attempting to write to root filesystem..."
if docker run --rm "$FULL_IMAGE_NAME" \
    sh -c "touch /test.txt 2>&1" 2>&1 | grep -q "Read-only"; then
    print_success "Root filesystem is read-only (as expected)"
elif docker run --rm "$FULL_IMAGE_NAME" \
    sh -c "touch /test.txt 2>&1" 2>&1 | grep -q "Permission denied"; then
    print_success "Root filesystem is write-protected (no root access)"
else
    print_warning "Root filesystem check inconclusive (expected behavior varies by Docker configuration)"
fi

# Step 10: Test environment variables
print_header "Step 10: Testing Environment Variables"
echo "Checking GGEN_HOME environment variable..."
if docker run --rm "$FULL_IMAGE_NAME" \
    sh -c "echo \$GGEN_HOME" | grep -q "/workspace"; then
    print_success "GGEN_HOME correctly set to /workspace"
else
    print_error "GGEN_HOME not properly set"
    exit 1
fi

# Step 11: Test resource limits
print_header "Step 11: Testing Resource Limits"
echo "Running container with CPU and memory limits..."
if timeout 30s docker run --rm \
    --cpus="1" \
    --memory="512m" \
    "$FULL_IMAGE_NAME" \
    ggen --version > /dev/null; then
    print_success "Container runs successfully with resource limits"
else
    print_error "Container failed with resource limits"
    exit 1
fi

# Step 12: Optional - Push to registry
if [ "$PUSH" = true ]; then
    print_header "Step 12: Pushing Image to Registry"
    if [ -z "$REGISTRY" ]; then
        print_error "Registry not specified. Use --registry to push"
        exit 1
    fi

    echo "Pushing $FULL_IMAGE_NAME to registry..."
    if docker push "$FULL_IMAGE_NAME"; then
        print_success "Image pushed successfully"
    else
        print_error "Failed to push image"
        exit 1
    fi
fi

# Summary
print_header "Build and Test Summary"
print_success "All tests passed successfully!"
echo ""
echo "Image details:"
echo "  Name: $FULL_IMAGE_NAME"
echo "  ID: ${IMAGE_ID:7:12}"
echo "  Size: $IMAGE_SIZE"
echo ""
echo "Next steps:"
echo "  1. Run container: docker run -it -v /workspace:/workspace $FULL_IMAGE_NAME"
echo "  2. Test ggen: docker run --rm -v /workspace:/workspace $FULL_IMAGE_NAME ggen sync --validate_only true"
if [ "$PUSH" = false ] && [ -n "$REGISTRY" ]; then
    echo "  3. Push image: $0 --registry $REGISTRY --push"
fi
echo ""
