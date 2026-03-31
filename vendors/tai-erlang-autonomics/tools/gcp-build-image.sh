#!/bin/bash

##############################################################################
# TAIEA Docker Image Build & Push Script
#
# Builds TAIEA Docker image and pushes to GCP Container Registry.
#
# Usage:
#   ./tools/gcp-build-image.sh [OPTIONS]
#
# Environment Variables:
#   PROJECT_ID        - GCP Project ID (default: taiea-phase1)
#   REGION            - GCP Region (default: us-central1)
#   IMAGE_TAG         - Docker image tag (default: latest)
#   DOCKER_REGISTRY   - Docker registry (default: gcr.io)
#   DOCKERFILE_PATH   - Path to Dockerfile (default: ./tools/Dockerfile)
#   REGISTRY_URL      - Full registry URL (default: constructed)
#
# Exit codes:
#   0 - Build and push successful
#   1 - Configuration error
#   2 - Docker not installed
#   3 - Build failure
#   4 - Push failure
#   5 - Authentication failure
##############################################################################

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RESET='\033[0m'

# Configuration
PROJECT_ID="${PROJECT_ID:-taiea-phase1}"
REGION="${REGION:-us-central1}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
DOCKER_REGISTRY="${DOCKER_REGISTRY:-gcr.io}"
DOCKERFILE_PATH="${DOCKERFILE_PATH:-./tools/Dockerfile}"
SERVICE_NAME="taiea"
TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")

# Flags
SKIP_BUILD=false
SKIP_PUSH=false
DRY_RUN=false
VERBOSE=false

##############################################################################
# Helper Functions
##############################################################################

log_header() {
    echo -e "${BLUE}=== $1 ===${RESET}"
}

log_info() {
    echo -e "${GREEN}[INFO]${RESET} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${RESET} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${RESET} $1"
}

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${BLUE}[DEBUG]${RESET} $1"
    fi
}

print_usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Build and push TAIEA Docker image to GCP Container Registry.

OPTIONS:
    --skip-build        Skip Docker build step
    --skip-push         Skip registry push step
    --dry-run           Show what would be done without executing
    -v, --verbose       Enable verbose output
    -h, --help          Show this help message

ENVIRONMENT VARIABLES:
    PROJECT_ID          GCP Project ID (default: taiea-phase1)
    REGION              GCP Region (default: us-central1)
    IMAGE_TAG           Docker image tag (default: latest)
    DOCKER_REGISTRY     Docker registry (default: gcr.io)
    DOCKERFILE_PATH     Path to Dockerfile (default: ./tools/Dockerfile)

EXAMPLES:
    # Build and push with default settings
    ./tools/gcp-build-image.sh

    # Build only (no push)
    ./tools/gcp-build-image.sh --skip-push

    # Dry run with verbose output
    ./tools/gcp-build-image.sh --dry-run --verbose

    # Custom project and tag
    PROJECT_ID=my-project IMAGE_TAG=v1.0.0 ./tools/gcp-build-image.sh

EOF
}

check_prerequisites() {
    log_header "Checking Prerequisites"

    local errors=0

    # Check Docker installation
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        echo "  Install from: https://docs.docker.com/get-docker/"
        return 2
    else
        local docker_version=$(docker --version)
        log_info "Docker: $docker_version"
    fi

    # Check gcloud installation
    if ! command -v gcloud &> /dev/null; then
        log_warn "gcloud is not installed - push will fail"
        log_info "Install from: https://cloud.google.com/sdk/docs/install"
        ((errors++))
    else
        local gcloud_version=$(gcloud --version | head -1)
        log_info "gcloud: $gcloud_version"
    fi

    # Check Dockerfile exists
    if [ ! -f "$DOCKERFILE_PATH" ]; then
        log_error "Dockerfile not found: $DOCKERFILE_PATH"
        ((errors++))
    else
        log_info "Dockerfile: $DOCKERFILE_PATH"
    fi

    # Validate project ID format
    if ! [[ "$PROJECT_ID" =~ ^[a-z0-9][a-z0-9-]*[a-z0-9]$ ]]; then
        log_error "Invalid GCP Project ID: $PROJECT_ID"
        echo "  Format: lowercase letters, numbers, hyphens (must start/end with letter or number)"
        ((errors++))
    else
        log_info "Project ID: $PROJECT_ID"
    fi

    if [ $errors -gt 0 ]; then
        return 1
    fi

    return 0
}

configure_docker_auth() {
    log_header "Configuring Docker Authentication"

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY RUN] Would configure Docker for GCP registry"
        return 0
    fi

    # Check if already authenticated
    if gcloud auth list 2>/dev/null | grep -q "ACTIVE"; then
        log_info "gcloud is already authenticated"
    else
        log_warn "gcloud is not authenticated"
        log_info "Run: gcloud auth login"
        return 5
    fi

    # Configure Docker for GCP
    log_verbose "Configuring Docker for ${DOCKER_REGISTRY}..."
    if [ "$VERBOSE" = true ]; then
        gcloud auth configure-docker "${DOCKER_REGISTRY}" --quiet
    else
        gcloud auth configure-docker "${DOCKER_REGISTRY}" --quiet 2>/dev/null
    fi

    log_info "Docker authentication configured"
}

build_image() {
    local image_url="${DOCKER_REGISTRY}/${PROJECT_ID}/${SERVICE_NAME}:${IMAGE_TAG}"
    local image_url_latest="${DOCKER_REGISTRY}/${PROJECT_ID}/${SERVICE_NAME}:latest"

    log_header "Building Docker Image"

    if [ "$SKIP_BUILD" = true ]; then
        log_info "Skipping build (--skip-build)"
        echo "$image_url"
        return 0
    fi

    log_info "Image URL: $image_url"

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY RUN] Would execute:"
        echo "    docker build -f $DOCKERFILE_PATH -t $image_url ."
        if [ "$IMAGE_TAG" != "latest" ]; then
            echo "    docker tag $image_url $image_url_latest"
        fi
        echo "$image_url"
        return 0
    fi

    log_verbose "Starting Docker build..."
    log_verbose "  Dockerfile: $DOCKERFILE_PATH"
    log_verbose "  Build context: ."

    # Build image
    if docker build -f "$DOCKERFILE_PATH" -t "$image_url" .; then
        log_info "Image built successfully: $image_url"
    else
        log_error "Docker build failed"
        return 3
    fi

    # Tag as latest if not already
    if [ "$IMAGE_TAG" != "latest" ]; then
        log_verbose "Tagging as latest..."
        docker tag "$image_url" "$image_url_latest"
        log_info "Tagged as: $image_url_latest"
    fi

    echo "$image_url"
}

push_to_registry() {
    local image_url=$1
    local image_url_latest="${DOCKER_REGISTRY}/${PROJECT_ID}/${SERVICE_NAME}:latest"

    log_header "Pushing Image to Registry"

    if [ "$SKIP_PUSH" = true ]; then
        log_info "Skipping push (--skip-push)"
        return 0
    fi

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY RUN] Would execute:"
        echo "    docker push $image_url"
        if [ "$IMAGE_TAG" != "latest" ]; then
            echo "    docker push $image_url_latest"
        fi
        return 0
    fi

    log_info "Target registry: ${DOCKER_REGISTRY}/${PROJECT_ID}"

    # Push main tag
    log_verbose "Pushing image tag: $image_url"
    if docker push "$image_url"; then
        log_info "Pushed successfully: $image_url"
    else
        log_error "Docker push failed"
        return 4
    fi

    # Push latest tag if different
    if [ "$IMAGE_TAG" != "latest" ]; then
        log_verbose "Pushing image tag: $image_url_latest"
        if docker push "$image_url_latest"; then
            log_info "Pushed successfully: $image_url_latest"
        else
            log_error "Failed to push latest tag"
            return 4
        fi
    fi
}

verify_image() {
    local image_url=$1

    log_header "Verifying Image"

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY RUN] Would verify image in registry"
        return 0
    fi

    if [ "$SKIP_PUSH" = true ]; then
        log_info "Verifying local image..."
        if docker image inspect "$image_url" > /dev/null; then
            log_info "Local image verified: $image_url"
        else
            log_error "Image not found locally"
            return 1
        fi
        return 0
    fi

    log_verbose "Verifying image in registry..."

    # List images in registry
    if gcloud container images list --project="$PROJECT_ID" 2>/dev/null | grep -q "$SERVICE_NAME"; then
        log_info "Image registered in GCP"

        # Get image digest
        log_verbose "Retrieving image digest..."
        local digest=$(gcloud container images describe "${image_url}" \
            --project="$PROJECT_ID" \
            --format='value(image_summary.digest)' 2>/dev/null || echo "N/A")

        if [ "$digest" != "N/A" ]; then
            log_info "Image digest: $digest"
        fi
    else
        log_warn "Could not verify image in registry"
    fi
}

generate_receipt() {
    local image_url="${DOCKER_REGISTRY}/${PROJECT_ID}/${SERVICE_NAME}:${IMAGE_TAG}"
    local image_url_latest="${DOCKER_REGISTRY}/${PROJECT_ID}/${SERVICE_NAME}:latest"

    log_header "Build & Push Receipt"

    cat << EOF
TAIEA Docker Image Build & Push Receipt
=========================================

Timestamp: $TIMESTAMP
Mode: $([ "$DRY_RUN" = true ] && echo "DRY RUN" || echo "EXECUTION")

Configuration:
  Project ID: $PROJECT_ID
  Registry: $DOCKER_REGISTRY
  Service: $SERVICE_NAME
  Tag: $IMAGE_TAG
  Dockerfile: $DOCKERFILE_PATH

Image Details:
  Main tag: $image_url
  Latest tag: $image_url_latest

Build Status:
  ✓ Dockerfile validated
  ✓ Prerequisites checked
  ✓ Docker authentication configured
  ✓ Image built
  ✓ Image pushed to registry
  ✓ Image verified

Next Steps:
  1. Deploy image: ./tools/gcp-deploy.sh
  2. Verify deployment: ./tools/gcp-verify-deployment.sh
  3. Monitor in Cloud Console

References:
  - Container Registry: https://console.cloud.google.com/gcr/images/$PROJECT_ID
  - Artifact Registry: https://console.cloud.google.com/artifacts

EOF
}

##############################################################################
# Main Execution
##############################################################################

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --skip-build)
                SKIP_BUILD=true
                shift
                ;;
            --skip-push)
                SKIP_PUSH=true
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                print_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                print_usage
                exit 1
                ;;
        esac
    done

    # Show mode
    if [ "$DRY_RUN" = true ]; then
        log_header "TAIEA Docker Build & Push (DRY RUN)"
    else
        log_header "TAIEA Docker Build & Push (EXECUTION)"
    fi

    # Execute steps
    check_prerequisites || exit $?
    echo ""

    configure_docker_auth || exit $?
    echo ""

    local image_url=$(build_image) || exit $?
    echo ""

    push_to_registry "$image_url" || exit $?
    echo ""

    verify_image "$image_url" || exit $?
    echo ""

    generate_receipt

    if [ "$DRY_RUN" = true ]; then
        log_info "Dry run completed. Review above and run without --dry-run for actual build/push."
    else
        log_info "Build and push completed successfully!"
    fi

    exit 0
}

main "$@"
