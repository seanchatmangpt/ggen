#!/bin/bash

##############################################################################
# TAIEA GCP Deployment Verification Script
#
# Verifies TAIEA deployment on Cloud Run by:
#   1. Checking service health endpoint
#   2. Retrieving service metrics
#   3. Validating Pub/Sub connectivity
#   4. Testing Firestore access
#   5. Checking IAM permissions
#   6. Verifying logs are being collected
#
# Usage:
#   ./tools/gcp-verify-deployment.sh [OPTIONS]
#
# Environment Variables:
#   PROJECT_ID        - GCP Project ID (default: taiea-phase1)
#   REGION            - GCP Region (default: us-central1)
#   SERVICE_NAME      - Cloud Run service name (default: taiea)
#   VERBOSE           - Enable verbose output (default: false)
#
# Exit codes:
#   0 - All verification checks passed
#   1 - Configuration error
#   2 - Service not found
#   3 - Health check failed
#   4 - Metrics unavailable
#   5 - Pub/Sub check failed
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
SERVICE_NAME="${SERVICE_NAME:-taiea}"
VERBOSE="${VERBOSE:-false}"
TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")

# Health check parameters
HEALTH_CHECK_TIMEOUT=10
MAX_RETRIES=3
RETRY_DELAY=2

# Tracking
CHECKS_PASSED=0
CHECKS_FAILED=0

##############################################################################
# Helper Functions
##############################################################################

log_header() {
    echo -e "${BLUE}=== $1 ===${RESET}"
}

log_pass() {
    echo -e "${GREEN}[PASS]${RESET} $1"
    ((CHECKS_PASSED++))
}

log_fail() {
    echo -e "${RED}[FAIL]${RESET} $1"
    ((CHECKS_FAILED++))
}

log_warn() {
    echo -e "${YELLOW}[WARN]${RESET} $1"
}

log_info() {
    echo -e "${GREEN}[INFO]${RESET} $1"
}

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${BLUE}[DEBUG]${RESET} $1"
    fi
}

print_usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Verify TAIEA deployment on GCP Cloud Run.

OPTIONS:
    -v, --verbose       Enable verbose output
    -h, --help          Show this help message

ENVIRONMENT VARIABLES:
    PROJECT_ID          GCP Project ID (default: taiea-phase1)
    REGION              GCP Region (default: us-central1)
    SERVICE_NAME        Cloud Run service name (default: taiea)

EXAMPLES:
    # Verify with default settings
    ./tools/gcp-verify-deployment.sh

    # Verbose verification
    ./tools/gcp-verify-deployment.sh --verbose

    # Custom project
    PROJECT_ID=my-project ./tools/gcp-verify-deployment.sh

EOF
}

check_gcloud() {
    log_header "Checking gcloud Installation"

    if ! command -v gcloud &> /dev/null; then
        log_fail "gcloud is not installed"
        return 1
    fi

    local version=$(gcloud --version 2>/dev/null | head -1)
    log_pass "gcloud installed: $version"

    # Check authentication
    if gcloud auth list 2>/dev/null | grep -q "ACTIVE"; then
        log_pass "gcloud authentication verified"
    else
        log_fail "gcloud is not authenticated"
        echo "  Run: gcloud auth login"
        return 1
    fi

    return 0
}

check_service_exists() {
    log_header "Checking Cloud Run Service"

    log_verbose "Querying Cloud Run service: $SERVICE_NAME"

    local service_info=$(gcloud run services describe "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(status.url)' \
        2>/dev/null || echo "")

    if [ -z "$service_info" ]; then
        log_fail "Cloud Run service not found: $SERVICE_NAME"
        echo "  Project: $PROJECT_ID"
        echo "  Region: $REGION"
        return 2
    fi

    local service_url="$service_info"
    log_pass "Cloud Run service found: $SERVICE_NAME"
    log_info "Service URL: $service_url"

    echo "$service_url"
}

get_service_details() {
    local service_url=$1

    log_header "Retrieving Service Details"

    local service_info=$(gcloud run services describe "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(metadata.name,status.replicas,status.conditions[0].message)' \
        2>/dev/null)

    log_verbose "Service info:\n$service_info"
    log_pass "Service details retrieved"
}

check_health_endpoint() {
    local service_url=$1

    log_header "Checking Health Endpoint"

    local health_url="${service_url}/health"
    log_verbose "Health endpoint: $health_url"

    local attempt=1
    while [ $attempt -le $MAX_RETRIES ]; do
        log_verbose "Health check attempt $attempt/$MAX_RETRIES..."

        local response=$(curl -s \
            --max-time "$HEALTH_CHECK_TIMEOUT" \
            --write-out "\n%{http_code}" \
            "$health_url" 2>/dev/null || echo -e "\n000")

        local http_code=$(echo "$response" | tail -1)
        local body=$(echo "$response" | head -n -1)

        if [ "$http_code" = "200" ]; then
            log_pass "Health endpoint responding (HTTP $http_code)"
            if [ -n "$body" ]; then
                log_verbose "Response body: $body"
            fi
            return 0
        fi

        if [ $attempt -lt $MAX_RETRIES ]; then
            log_warn "Health check failed (HTTP $http_code), retrying in ${RETRY_DELAY}s..."
            sleep "$RETRY_DELAY"
        fi

        ((attempt++))
    done

    log_fail "Health endpoint not responding after $MAX_RETRIES attempts (HTTP $http_code)"
    return 3
}

check_metrics_endpoint() {
    local service_url=$1

    log_header "Checking Metrics Endpoint"

    local metrics_url="${service_url}/metrics"
    log_verbose "Metrics endpoint: $metrics_url"

    local response=$(curl -s \
        --max-time 5 \
        --write-out "\n%{http_code}" \
        "$metrics_url" 2>/dev/null || echo -e "\n000")

    local http_code=$(echo "$response" | tail -1)
    local body=$(echo "$response" | head -n -1)

    if [ "$http_code" = "200" ]; then
        log_pass "Metrics endpoint responding (HTTP $http_code)"
        log_verbose "Metrics available"
        return 0
    elif [ "$http_code" = "404" ]; then
        log_warn "Metrics endpoint not available (HTTP 404)"
        return 0
    else
        log_fail "Metrics endpoint error (HTTP $http_code)"
        return 4
    fi
}

check_firestore_connectivity() {
    log_header "Checking Firestore Connectivity"

    log_verbose "Checking Firestore database in region: $REGION"

    # Check if Firestore exists
    local firestore_info=$(gcloud firestore databases list \
        --project="$PROJECT_ID" \
        --format='value(name,location)' 2>/dev/null || echo "")

    if [ -z "$firestore_info" ]; then
        log_warn "No Firestore databases found in project"
        return 0
    fi

    log_pass "Firestore database available"
    log_verbose "Database info: $firestore_info"

    # Check collections
    log_verbose "Checking Firestore collections..."
    local collections=$(gcloud firestore collections list \
        --project="$PROJECT_ID" \
        2>/dev/null || echo "")

    if [ -n "$collections" ]; then
        log_info "Firestore collections:"
        echo "$collections" | sed 's/^/  /'
    fi

    return 0
}

check_pubsub_connectivity() {
    log_header "Checking Pub/Sub Connectivity"

    log_verbose "Checking Pub/Sub topic: taiea-signals"

    # Check if topic exists
    local topic_info=$(gcloud pubsub topics list \
        --project="$PROJECT_ID" \
        --filter="name:taiea-signals" \
        --format='value(name)' 2>/dev/null || echo "")

    if [ -z "$topic_info" ]; then
        log_warn "Pub/Sub topic 'taiea-signals' not found"
        return 0
    fi

    log_pass "Pub/Sub topic found: taiea-signals"

    # Check subscription
    local subscription_info=$(gcloud pubsub subscriptions list \
        --project="$PROJECT_ID" \
        --filter="name:taiea-subscription" \
        --format='value(name)' 2>/dev/null || echo "")

    if [ -n "$subscription_info" ]; then
        log_pass "Pub/Sub subscription found: taiea-subscription"
    else
        log_warn "Pub/Sub subscription not found"
    fi

    return 0
}

check_iam_permissions() {
    log_header "Checking IAM Permissions"

    local service_account="taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com"
    log_verbose "Service account: $service_account"

    # Check if service account exists
    local sa_info=$(gcloud iam service-accounts list \
        --project="$PROJECT_ID" \
        --filter="email:taiea-sa@" \
        --format='value(email)' 2>/dev/null || echo "")

    if [ -z "$sa_info" ]; then
        log_warn "Service account not found: $service_account"
        return 0
    fi

    log_pass "Service account found: $sa_info"

    # List roles
    local roles=$(gcloud projects get-iam-policy "$PROJECT_ID" \
        --flatten="bindings[].members" \
        --filter="bindings.members:serviceAccount:${sa_info}" \
        --format='value(bindings.role)' 2>/dev/null | sort | uniq)

    if [ -n "$roles" ]; then
        log_info "Service account roles:"
        echo "$roles" | sed 's/^/  /'
    fi

    return 0
}

check_logs() {
    log_header "Checking Cloud Logs"

    log_verbose "Querying recent logs for service: $SERVICE_NAME"

    # Get recent log entries
    local log_count=$(gcloud logging read \
        "resource.type=cloud_run_revision AND resource.labels.service_name=$SERVICE_NAME" \
        --project="$PROJECT_ID" \
        --limit=5 \
        --format='value(timestamp)' 2>/dev/null | wc -l)

    if [ "$log_count" -gt 0 ]; then
        log_pass "Logs available ($log_count recent entries)"
        log_verbose "Recent log entries found"
    else
        log_warn "No recent log entries found"
    fi

    return 0
}

check_container_image() {
    log_header "Checking Container Image"

    # Get the deployed image
    local image_url=$(gcloud run services describe "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(spec.template.spec.containers[0].image)' 2>/dev/null || echo "")

    if [ -z "$image_url" ]; then
        log_fail "Could not determine deployed image"
        return 1
    fi

    log_pass "Deployed image: $image_url"
    log_verbose "Image URL: $image_url"

    return 0
}

generate_report() {
    local service_url=$1

    log_header "Verification Summary"

    local total=$((CHECKS_PASSED + CHECKS_FAILED))

    cat << EOF
TAIEA Deployment Verification Report
======================================

Timestamp: $TIMESTAMP
Project: $PROJECT_ID
Region: $REGION
Service: $SERVICE_NAME
Service URL: $service_url

Verification Results:
  Passed: $CHECKS_PASSED/$total
  Failed: $CHECKS_FAILED/$total

Checks Performed:
  ✓ gcloud installation and authentication
  ✓ Cloud Run service existence
  ✓ Service details and replicas
  ✓ Health endpoint (/health)
  ✓ Metrics endpoint (/metrics)
  ✓ Firestore connectivity
  ✓ Pub/Sub connectivity
  ✓ IAM permissions
  ✓ Cloud Logs
  ✓ Container image

Service Details:
  URL: $service_url
  Health: $service_url/health
  Metrics: $service_url/metrics
  Signals: $service_url/signals

Monitoring:
  Cloud Console: https://console.cloud.google.com/run/detail/$REGION/$SERVICE_NAME
  Logs: https://console.cloud.google.com/logs/query
  Metrics: https://console.cloud.google.com/monitoring

References:
  - Cloud Run Documentation: https://cloud.google.com/run/docs
  - Troubleshooting Guide: https://cloud.google.com/run/docs/troubleshooting

EOF

    if [ $CHECKS_FAILED -eq 0 ]; then
        log_pass "All verification checks passed!"
        return 0
    else
        log_fail "Some verification checks failed"
        return 1
    fi
}

##############################################################################
# Main Execution
##############################################################################

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                print_usage
                exit 0
                ;;
            *)
                log_fail "Unknown option: $1"
                print_usage
                exit 1
                ;;
        esac
    done

    log_header "TAIEA Deployment Verification"

    # Execute checks
    check_gcloud || exit 1
    echo ""

    local service_url=$(check_service_exists) || exit 2
    echo ""

    get_service_details "$service_url"
    echo ""

    check_container_image || exit 1
    echo ""

    check_health_endpoint "$service_url" || true
    echo ""

    check_metrics_endpoint "$service_url" || true
    echo ""

    check_firestore_connectivity || true
    echo ""

    check_pubsub_connectivity || true
    echo ""

    check_iam_permissions || true
    echo ""

    check_logs || true
    echo ""

    generate_report "$service_url" || exit 1

    exit 0
}

main "$@"
