#!/bin/bash
# validate-gcp-deployment.sh
# Production readiness validation for GCP infrastructure
#
# Usage: ./validate-gcp-deployment.sh [PROJECT_ID]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PROJECT_ID="${1:-$(gcloud config get-value project 2>/dev/null)}"
REGION="${2:-us-central1}"

echo "=================================================="
echo "GCP Infrastructure Validation"
echo "=================================================="
echo "Project: $PROJECT_ID"
echo "Region: $REGION"
echo ""

# Validation counters
PASS=0
FAIL=0
WARN=0

function check_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASS++))
}

function check_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAIL++))
}

function check_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ((WARN++))
}

# ============================================================================
# 1. Prerequisites Check
# ============================================================================

echo "1. Checking Prerequisites..."
echo "----------------------------"

if command -v gcloud &> /dev/null; then
    check_pass "gcloud CLI installed"
else
    check_fail "gcloud CLI not found"
fi

if command -v terraform &> /dev/null; then
    check_pass "Terraform installed"
else
    check_fail "Terraform not found"
fi

if command -v docker &> /dev/null; then
    check_pass "Docker installed"
else
    check_fail "Docker not found"
fi

if [ -z "$PROJECT_ID" ]; then
    check_fail "GCP project ID not set"
    echo "Usage: $0 <PROJECT_ID>"
    exit 1
else
    check_pass "GCP project ID: $PROJECT_ID"
fi

# ============================================================================
# 2. GCP Project Setup
# ============================================================================

echo ""
echo "2. Validating GCP Project..."
echo "----------------------------"

# Check if project exists
if gcloud projects describe "$PROJECT_ID" &> /dev/null; then
    check_pass "Project exists: $PROJECT_ID"
else
    check_fail "Project not found: $PROJECT_ID"
    exit 1
fi

# Check billing
BILLING_ENABLED=$(gcloud beta billing projects describe "$PROJECT_ID" --format="value(billingEnabled)" 2>/dev/null)
if [ "$BILLING_ENABLED" = "True" ]; then
    check_pass "Billing enabled"
else
    check_fail "Billing not enabled"
fi

# ============================================================================
# 3. Required APIs
# ============================================================================

echo ""
echo "3. Checking Required APIs..."
echo "----------------------------"

REQUIRED_APIS=(
    "run.googleapis.com"
    "sqladmin.googleapis.com"
    "storage.googleapis.com"
    "compute.googleapis.com"
    "servicenetworking.googleapis.com"
    "vpcaccess.googleapis.com"
    "secretmanager.googleapis.com"
    "cloudtrace.googleapis.com"
    "monitoring.googleapis.com"
    "logging.googleapis.com"
    "dns.googleapis.com"
    "iam.googleapis.com"
    "artifactregistry.googleapis.com"
    "binaryauthorization.googleapis.com"
)

for api in "${REQUIRED_APIS[@]}"; do
    if gcloud services list --enabled --filter="name:$api" --format="value(name)" 2>/dev/null | grep -q "$api"; then
        check_pass "API enabled: $api"
    else
        check_warn "API not enabled: $api"
    fi
done

# ============================================================================
# 4. Terraform Configuration
# ============================================================================

echo ""
echo "4. Validating Terraform..."
echo "----------------------------"

if [ -d "generated/terraform" ]; then
    check_pass "Terraform directory exists"

    cd generated/terraform || exit 1

    # Check for required files
    for file in gcp_main.tf gcp_variables.tf gcp_outputs.tf; do
        if [ -f "$file" ]; then
            check_pass "File exists: $file"
        else
            check_fail "File missing: $file"
        fi
    done

    # Terraform init
    if terraform init -backend=false &> /dev/null; then
        check_pass "Terraform init successful"
    else
        check_fail "Terraform init failed"
    fi

    # Terraform validate
    if terraform validate &> /dev/null; then
        check_pass "Terraform configuration valid"
    else
        check_fail "Terraform validation failed"
    fi

    cd ../.. || exit 1
else
    check_fail "Terraform directory not found (run 'ggen sync' first)"
fi

# ============================================================================
# 5. Infrastructure Components (if deployed)
# ============================================================================

echo ""
echo "5. Checking Deployed Resources..."
echo "----------------------------"

# Cloud Run service
if gcloud run services describe attribution-service --region="$REGION" --project="$PROJECT_ID" &> /dev/null; then
    check_pass "Cloud Run service deployed"

    SERVICE_URL=$(gcloud run services describe attribution-service --region="$REGION" --project="$PROJECT_ID" --format="value(status.url)" 2>/dev/null)
    echo "   URL: $SERVICE_URL"
else
    check_warn "Cloud Run service not deployed"
fi

# Cloud SQL instance
if gcloud sql instances describe attribution-event-store --project="$PROJECT_ID" &> /dev/null; then
    check_pass "Cloud SQL instance exists"

    SQL_STATE=$(gcloud sql instances describe attribution-event-store --project="$PROJECT_ID" --format="value(state)" 2>/dev/null)
    if [ "$SQL_STATE" = "RUNNABLE" ]; then
        check_pass "Cloud SQL instance running"
    else
        check_warn "Cloud SQL instance state: $SQL_STATE"
    fi
else
    check_warn "Cloud SQL instance not deployed"
fi

# Cloud Storage bucket
BUCKET_NAME="${PROJECT_ID}-attribution-static-content"
if gsutil ls -b "gs://$BUCKET_NAME" &> /dev/null; then
    check_pass "Static content bucket exists"
else
    check_warn "Static content bucket not found"
fi

# VPC network
if gcloud compute networks describe attribution-vpc --project="$PROJECT_ID" &> /dev/null; then
    check_pass "VPC network exists"
else
    check_warn "VPC network not deployed"
fi

# ============================================================================
# 6. Security Checks
# ============================================================================

echo ""
echo "6. Security Validation..."
echo "----------------------------"

# Check Secret Manager secrets
if gcloud secrets describe db-connection-string --project="$PROJECT_ID" &> /dev/null; then
    check_pass "Database secret exists in Secret Manager"
else
    check_warn "Database secret not found"
fi

# Check service accounts
if gcloud iam service-accounts describe "attribution-service-sa@${PROJECT_ID}.iam.gserviceaccount.com" --project="$PROJECT_ID" &> /dev/null; then
    check_pass "Cloud Run service account exists"
else
    check_warn "Service account not found"
fi

# Check Cloud Armor policy
if gcloud compute security-policies describe attribution-security-policy --project="$PROJECT_ID" &> /dev/null; then
    check_pass "Cloud Armor policy configured"
else
    check_warn "Cloud Armor policy not found"
fi

# ============================================================================
# 7. Monitoring Setup
# ============================================================================

echo ""
echo "7. Monitoring & Logging..."
echo "----------------------------"

# Check uptime checks
UPTIME_CHECKS=$(gcloud monitoring uptime list --project="$PROJECT_ID" 2>/dev/null | wc -l)
if [ "$UPTIME_CHECKS" -gt 0 ]; then
    check_pass "Uptime checks configured ($UPTIME_CHECKS found)"
else
    check_warn "No uptime checks found"
fi

# Check alert policies
ALERT_POLICIES=$(gcloud alpha monitoring policies list --project="$PROJECT_ID" 2>/dev/null | wc -l)
if [ "$ALERT_POLICIES" -gt 0 ]; then
    check_pass "Alert policies configured ($ALERT_POLICIES found)"
else
    check_warn "No alert policies found"
fi

# Check log sinks
LOG_SINKS=$(gcloud logging sinks list --project="$PROJECT_ID" 2>/dev/null | wc -l)
if [ "$LOG_SINKS" -gt 0 ]; then
    check_pass "Log sinks configured ($LOG_SINKS found)"
else
    check_warn "No log sinks found"
fi

# ============================================================================
# 8. Health Checks (if deployed)
# ============================================================================

echo ""
echo "8. Service Health Checks..."
echo "----------------------------"

if [ -n "$SERVICE_URL" ]; then
    # Check startup endpoint
    if curl -s -f -m 5 "$SERVICE_URL/health/startup" &> /dev/null; then
        check_pass "Startup health check passed"
    else
        check_fail "Startup health check failed"
    fi

    # Check liveness endpoint
    if curl -s -f -m 5 "$SERVICE_URL/health/live" &> /dev/null; then
        check_pass "Liveness health check passed"
    else
        check_fail "Liveness health check failed"
    fi

    # Check readiness endpoint
    if curl -s -f -m 5 "$SERVICE_URL/health/ready" &> /dev/null; then
        check_pass "Readiness health check passed"
    else
        check_fail "Readiness health check failed"
    fi
else
    check_warn "Service not deployed, skipping health checks"
fi

# ============================================================================
# 9. Cost Controls
# ============================================================================

echo ""
echo "9. Cost Controls..."
echo "----------------------------"

# Check budgets
BUDGETS=$(gcloud billing budgets list --billing-account="$(gcloud beta billing projects describe "$PROJECT_ID" --format='value(billingAccountName)' 2>/dev/null)" 2>/dev/null | wc -l)
if [ "$BUDGETS" -gt 0 ]; then
    check_pass "Budget alerts configured ($BUDGETS found)"
else
    check_warn "No budget alerts configured"
fi

# ============================================================================
# 10. Backup & DR
# ============================================================================

echo ""
echo "10. Backup & Disaster Recovery..."
echo "----------------------------"

# Check Cloud SQL backups
if gcloud sql instances describe attribution-event-store --project="$PROJECT_ID" &> /dev/null; then
    BACKUP_ENABLED=$(gcloud sql instances describe attribution-event-store --project="$PROJECT_ID" --format="value(settings.backupConfiguration.enabled)" 2>/dev/null)
    if [ "$BACKUP_ENABLED" = "True" ]; then
        check_pass "Automated backups enabled"
    else
        check_fail "Automated backups not enabled"
    fi

    PITR_ENABLED=$(gcloud sql instances describe attribution-event-store --project="$PROJECT_ID" --format="value(settings.backupConfiguration.pointInTimeRecoveryEnabled)" 2>/dev/null)
    if [ "$PITR_ENABLED" = "True" ]; then
        check_pass "Point-in-time recovery enabled"
    else
        check_warn "Point-in-time recovery not enabled"
    fi

    # Check for read replica
    if gcloud sql instances describe attribution-event-store-replica --project="$PROJECT_ID" &> /dev/null; then
        check_pass "Read replica configured"
    else
        check_warn "No read replica found"
    fi
fi

# ============================================================================
# Summary
# ============================================================================

echo ""
echo "=================================================="
echo "Validation Summary"
echo "=================================================="
echo -e "${GREEN}Passed:${NC} $PASS"
echo -e "${YELLOW}Warnings:${NC} $WARN"
echo -e "${RED}Failed:${NC} $FAIL"
echo ""

if [ $FAIL -eq 0 ] && [ $WARN -eq 0 ]; then
    echo -e "${GREEN}✓ Infrastructure is production-ready!${NC}"
    exit 0
elif [ $FAIL -eq 0 ]; then
    echo -e "${YELLOW}⚠ Infrastructure has warnings but is deployable${NC}"
    exit 0
else
    echo -e "${RED}✗ Infrastructure has critical issues${NC}"
    exit 1
fi
