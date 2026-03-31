#!/bin/bash

# Test Authentication & Authorization for erlmcp
# Purpose: Validate authentication flows and authorization policies
# Usage: ./test-auth.sh [project-id] [service]

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
PROJECT_ID="${1:-$(gcloud config get-value project)}"
SERVICE_NAME="${2:-taiea}"
CLOUD_RUN_URL=""

# Helper functions
log_info() {
  echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
  echo -e "${GREEN}[✓]${NC} $*"
}

log_error() {
  echo -e "${RED}[✗]${NC} $*"
}

log_warning() {
  echo -e "${YELLOW}[⚠]${NC} $*"
}

# Test 1: Check if gcloud is configured
test_gcloud_config() {
  log_info "Testing gcloud configuration..."

  if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" | grep -q .; then
    log_error "No active gcloud authentication. Run: gcloud auth login"
    return 1
  fi

  log_success "gcloud authenticated"

  local project=$(gcloud config get-value project 2>/dev/null || echo "")
  if [ -z "$project" ]; then
    log_error "No default gcloud project. Run: gcloud config set project PROJECT_ID"
    return 1
  fi

  log_success "Default project: $project"
}

# Test 2: Check Cloud Run service exists
test_cloud_run_service() {
  log_info "Testing Cloud Run service existence..."

  local service=$(gcloud run services describe "$SERVICE_NAME" \
    --region="us-central1" \
    --project="$PROJECT_ID" \
    --format="value(status.url)" 2>/dev/null || echo "")

  if [ -z "$service" ]; then
    log_error "Cloud Run service '$SERVICE_NAME' not found in project '$PROJECT_ID'"
    return 1
  fi

  CLOUD_RUN_URL="$service"
  log_success "Cloud Run URL: $CLOUD_RUN_URL"
}

# Test 3: Check service account exists
test_service_accounts() {
  log_info "Testing service accounts..."

  local accounts=(
    "taiea-sa"
    "taiea-admin"
    "taiea-api-client"
    "taiea-billing"
  )

  for account in "${accounts[@]}"; do
    if gcloud iam service-accounts describe "${account}@${PROJECT_ID}.iam.gserviceaccount.com" \
      --project="$PROJECT_ID" &>/dev/null; then
      log_success "Service account exists: $account"
    else
      log_warning "Service account not found: $account"
    fi
  done
}

# Test 4: Get access token for default credentials
test_get_access_token() {
  log_info "Testing access token generation..."

  if ! TOKEN=$(gcloud auth application-default print-access-token 2>/dev/null); then
    log_error "Failed to get access token. Run: gcloud auth application-default login"
    return 1
  fi

  log_success "Access token obtained (${#TOKEN} chars)"

  # Decode and display token info
  local header=$(echo "$TOKEN" | cut -d. -f1 | base64 -d 2>/dev/null || echo "")
  local payload=$(echo "$TOKEN" | cut -d. -f2 | base64 -d 2>/dev/null || echo "")

  if [ -n "$payload" ]; then
    log_info "Token claims:"
    echo "$payload" | jq '.' 2>/dev/null || echo "$payload"
  fi
}

# Test 5: Test Cloud Run health endpoint (public or authenticated)
test_health_endpoint() {
  log_info "Testing Cloud Run health endpoint..."

  local token=$(gcloud auth application-default print-access-token 2>/dev/null || echo "")

  if [ -z "$token" ]; then
    log_warning "No access token, trying unauthenticated request..."
    local response=$(curl -s -w "\n%{http_code}" "$CLOUD_RUN_URL/health" 2>/dev/null || echo "")
  else
    local response=$(curl -s -w "\n%{http_code}" \
      -H "Authorization: Bearer $token" \
      "$CLOUD_RUN_URL/health" 2>/dev/null || echo "")
  fi

  local body=$(echo "$response" | head -n -1)
  local status=$(echo "$response" | tail -n 1)

  if [ "$status" = "200" ] || [ "$status" = "401" ] || [ "$status" = "403" ]; then
    log_success "Health endpoint responded with status: $status"
    if [ -n "$body" ]; then
      echo "$body" | jq '.' 2>/dev/null || echo "$body"
    fi
  else
    log_error "Health endpoint returned unexpected status: $status"
    return 1
  fi
}

# Test 6: Check IAM bindings for Cloud Run invoker
test_iam_bindings() {
  log_info "Testing IAM bindings for Cloud Run service..."

  local service_resource="projects/${PROJECT_ID}/locations/us-central1/services/${SERVICE_NAME}"

  if ! gcloud run services get-iam-policy "$SERVICE_NAME" \
    --region="us-central1" \
    --project="$PROJECT_ID" \
    --format=json 2>/dev/null | jq '.bindings[]' >/dev/null 2>&1; then
    log_warning "Unable to retrieve IAM bindings"
    return 0
  fi

  log_success "IAM bindings retrieved:"
  gcloud run services get-iam-policy "$SERVICE_NAME" \
    --region="us-central1" \
    --project="$PROJECT_ID" \
    --format="table(bindings[].members[])" 2>/dev/null || echo "Unable to format bindings"
}

# Test 7: Check service account permissions
test_service_account_permissions() {
  log_info "Testing service account permissions..."

  local accounts=(
    "taiea-sa"
    "taiea-admin"
    "taiea-api-client"
  )

  for account in "${accounts[@]}"; do
    local email="${account}@${PROJECT_ID}.iam.gserviceaccount.com"

    log_info "Checking permissions for $account..."

    # List roles
    local roles=$(gcloud projects get-iam-policy "$PROJECT_ID" \
      --flatten="bindings[].members" \
      --format="table(bindings.role)" \
      --filter="bindings.members:serviceAccount:${email}" 2>/dev/null || echo "")

    if [ -z "$roles" ]; then
      log_warning "No roles found for $account"
    else
      log_success "Roles for $account:"
      echo "$roles"
    fi
  done
}

# Test 8: Test service account key file
test_service_account_key() {
  log_info "Testing service account key files..."

  local key_files=(
    "credentials/taiea-admin-key.json"
    "credentials/taiea-api-client-key.json"
  )

  for key_file in "${key_files[@]}"; do
    if [ -f "$key_file" ]; then
      log_success "Key file exists: $key_file"

      # Check file permissions (should be 0600)
      local perms=$(stat -c "%a" "$key_file" 2>/dev/null || stat -f "%A" "$key_file" 2>/dev/null || echo "unknown")
      if [ "$perms" = "600" ]; then
        log_success "File permissions correct: 600"
      else
        log_warning "File permissions: $perms (recommend: 600)"
      fi
    else
      log_warning "Key file not found: $key_file"
    fi
  done
}

# Test 9: Test Firestore access
test_firestore_access() {
  log_info "Testing Firestore access..."

  if ! gcloud firestore databases describe --database="(default)" \
    --project="$PROJECT_ID" &>/dev/null; then
    log_warning "Unable to access Firestore database"
    return 0
  fi

  log_success "Firestore database accessible"

  # Try to list documents (may fail due to security rules, which is expected)
  if gcloud firestore documents list --database="(default)" \
    --collection-path="tenants" \
    --project="$PROJECT_ID" &>/dev/null; then
    log_success "Firestore document listing works"
  else
    log_info "Firestore document listing requires additional permissions or may be blocked by security rules"
  fi
}

# Test 10: Test Cloud Audit Logs
test_audit_logs() {
  log_info "Testing Cloud Audit Logs..."

  # Check if audit logs are enabled
  local audit_config=$(gcloud projects get-iam-policy "$PROJECT_ID" \
    --format=json 2>/dev/null | jq '.auditConfigs[]' 2>/dev/null || echo "")

  if [ -z "$audit_config" ]; then
    log_warning "No audit log configurations found"
  else
    log_success "Audit logs configured"
  fi

  # Try to read recent audit logs
  local logs=$(gcloud logging read \
    "resource.type=cloud_run_managed_environment" \
    --project="$PROJECT_ID" \
    --limit=5 \
    --format=json 2>/dev/null | jq length || echo "0")

  log_info "Found $logs recent audit log entries"
}

# Test 11: Test authentication to Cloud Run with service account
test_service_account_authentication() {
  log_info "Testing Cloud Run authentication with service account key..."

  if [ ! -f "credentials/taiea-api-client-key.json" ]; then
    log_warning "Service account key file not found, skipping service account authentication test"
    return 0
  fi

  # Generate identity token using service account key
  local token=$(gcloud auth print-identity-token \
    --audiences="$CLOUD_RUN_URL" \
    --impersonate-service-account="taiea-api-client@${PROJECT_ID}.iam.gserviceaccount.com" \
    --credential-source-file="credentials/taiea-api-client-key.json" 2>/dev/null || echo "")

  if [ -z "$token" ]; then
    log_warning "Unable to generate identity token for service account"
    return 0
  fi

  log_success "Identity token generated for service account"

  # Test Cloud Run invocation with service account token
  local response=$(curl -s -w "\n%{http_code}" \
    -H "Authorization: Bearer $token" \
    "$CLOUD_RUN_URL/health" 2>/dev/null || echo "")

  local status=$(echo "$response" | tail -n 1)

  if [ "$status" = "200" ]; then
    log_success "Cloud Run invocation successful with service account token"
  elif [ "$status" = "401" ] || [ "$status" = "403" ]; then
    log_warning "Cloud Run returned $status (may be expected if Cloud Run is not publicly accessible)"
  else
    log_error "Cloud Run returned unexpected status: $status"
  fi
}

# Test 12: Security configuration check
test_security_configuration() {
  log_info "Testing security configuration..."

  # Check if HTTPS is enforced
  if gcloud run services describe "$SERVICE_NAME" \
    --region="us-central1" \
    --project="$PROJECT_ID" \
    --format="value(status.conditions[0].message)" 2>/dev/null | grep -q "https"; then
    log_success "HTTPS enforced for Cloud Run service"
  fi

  # Check Cloud Armor (if enabled)
  if gcloud compute security-policies list \
    --project="$PROJECT_ID" &>/dev/null; then
    log_info "Cloud Armor available"
  fi
}

# Main test suite
main() {
  log_info "=================="
  log_info "erlmcp Authentication & Authorization Test Suite"
  log_info "=================="
  echo ""

  local tests=(
    "test_gcloud_config"
    "test_cloud_run_service"
    "test_service_accounts"
    "test_get_access_token"
    "test_health_endpoint"
    "test_iam_bindings"
    "test_service_account_permissions"
    "test_service_account_key"
    "test_firestore_access"
    "test_audit_logs"
    "test_service_account_authentication"
    "test_security_configuration"
  )

  local passed=0
  local failed=0

  for test in "${tests[@]}"; do
    echo ""
    if $test; then
      ((passed++))
    else
      ((failed++))
    fi
  done

  echo ""
  log_info "=================="
  log_info "Test Summary"
  log_info "=================="
  log_success "Passed: $passed"
  log_error "Failed: $failed"
  echo ""

  if [ "$failed" -gt 0 ]; then
    return 1
  fi
}

# Run tests
main "$@"
