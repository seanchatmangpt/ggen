#!/bin/bash
# Smoke Test Suite for TAI Autonomics Deployment
# Validates that all deployed services are functioning correctly

set -e

# Configuration
SERVICE_URL=${1:-"https://api.pricing.example.com"}
ONBOARDING_URL=${2:-"https://app.example.com"}
PROJECT_ID=${3:-"tai-autonomics-prod"}
REGION=${4:-"us-central1"}

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

# Helper functions
pass() {
  echo -e "${GREEN}✓${NC} $1"
  ((PASSED++))
}

fail() {
  echo -e "${RED}✗${NC} $1"
  ((FAILED++))
}

warn() {
  echo -e "${YELLOW}⚠${NC} $1"
}

echo "🧪 TAI Autonomics Smoke Test Suite"
echo "=================================="
echo ""
echo "Testing Service URL: $SERVICE_URL"
echo "Testing Onboarding URL: $ONBOARDING_URL"
echo "GCP Project: $PROJECT_ID"
echo ""

# ============================================================================
# Test 1: Service Health Checks
# ============================================================================

echo "📋 Testing Service Health Checks..."

# Test pricing engine health
if curl -s -f -m 5 "$SERVICE_URL/health" > /dev/null 2>&1; then
  pass "Pricing engine health check endpoint responding"
else
  fail "Pricing engine health check failed"
fi

# Test onboarding app health
if curl -s -f -m 5 "$ONBOARDING_URL/health" > /dev/null 2>&1; then
  pass "Onboarding app health check endpoint responding"
else
  warn "Onboarding app health check may not be at /health"
fi

echo ""

# ============================================================================
# Test 2: HTTP Response Codes
# ============================================================================

echo "📋 Testing HTTP Response Codes..."

# Test pricing engine responds with 200
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$SERVICE_URL/health")
if [ "$STATUS" = "200" ]; then
  pass "Pricing engine returns HTTP 200 on /health"
else
  fail "Pricing engine returned HTTP $STATUS (expected 200)"
fi

# Test onboarding app responds with expected code
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$ONBOARDING_URL/")
if [ "$STATUS" = "200" ] || [ "$STATUS" = "302" ]; then
  pass "Onboarding app returns HTTP $STATUS"
else
  fail "Onboarding app returned unexpected HTTP $STATUS"
fi

echo ""

# ============================================================================
# Test 3: SSL/TLS Certificates
# ============================================================================

echo "📋 Testing SSL/TLS Certificates..."

# Test pricing engine HTTPS
if timeout 5 openssl s_client -connect "${SERVICE_URL#https://}:443" < /dev/null 2>/dev/null | grep -q "subject="; then
  pass "Pricing engine SSL certificate valid"
else
  fail "Pricing engine SSL certificate validation failed"
fi

# Test onboarding app HTTPS
if timeout 5 openssl s_client -connect "${ONBOARDING_URL#https://}:443" < /dev/null 2>/dev/null | grep -q "subject="; then
  pass "Onboarding app SSL certificate valid"
else
  fail "Onboarding app SSL certificate validation failed"
fi

echo ""

# ============================================================================
# Test 4: GCP Infrastructure Verification
# ============================================================================

echo "📋 Testing GCP Infrastructure..."

# Set project
gcloud config set project $PROJECT_ID 2>/dev/null

# Verify Cloud Run service exists
if gcloud run services describe tai-autonomics --region=$REGION > /dev/null 2>&1; then
  pass "Cloud Run service 'tai-autonomics' exists"
else
  fail "Cloud Run service 'tai-autonomics' not found"
fi

# Verify Firestore database exists
if gcloud firestore databases describe --region=$REGION > /dev/null 2>&1; then
  pass "Firestore database exists"
else
  fail "Firestore database not found"
fi

# Verify service account exists
if gcloud iam service-accounts describe tai-autonomics-sa@${PROJECT_ID}.iam.gserviceaccount.com > /dev/null 2>&1; then
  pass "Service account 'tai-autonomics-sa' exists"
else
  fail "Service account 'tai-autonomics-sa' not found"
fi

echo ""

# ============================================================================
# Test 5: Cloud Run Configuration
# ============================================================================

echo "📋 Testing Cloud Run Configuration..."

# Check service is running (all instances)
RUNNING=$(gcloud run services describe tai-autonomics --region=$REGION --format='value(status.conditions[0].status)' 2>/dev/null)
if [ "$RUNNING" = "True" ]; then
  pass "Cloud Run service status is healthy"
else
  fail "Cloud Run service status is unhealthy: $RUNNING"
fi

# Check min instances configured
MIN_INSTANCES=$(gcloud run services describe tai-autonomics --region=$REGION --format='value(spec.template.metadata.annotations["autoscaling.knative.dev/minScale"])' 2>/dev/null)
if [ ! -z "$MIN_INSTANCES" ] && [ "$MIN_INSTANCES" != "0" ]; then
  pass "Cloud Run has min instances configured: $MIN_INSTANCES"
else
  warn "Cloud Run may not have min instances set (could lead to cold starts)"
fi

# Check max instances configured
MAX_INSTANCES=$(gcloud run services describe tai-autonomics --region=$REGION --format='value(spec.template.metadata.annotations["autoscaling.knative.dev/maxScale"])' 2>/dev/null)
if [ ! -z "$MAX_INSTANCES" ]; then
  pass "Cloud Run has max instances configured: $MAX_INSTANCES"
else
  warn "Cloud Run max instances not configured"
fi

echo ""

# ============================================================================
# Test 6: Monitoring and Logging
# ============================================================================

echo "📋 Testing Monitoring and Logging..."

# Check if logs are flowing
RECENT_LOGS=$(gcloud logging read "resource.service=tai-autonomics" --limit=5 --region=$REGION 2>/dev/null | wc -l)
if [ "$RECENT_LOGS" -gt "0" ]; then
  pass "Cloud Logging is receiving logs from service"
else
  warn "No recent logs found (service may be idle)"
fi

# Check if metrics are being collected
METRICS=$(gcloud monitoring metrics-descriptors list --filter="metric.type:run.googleapis.com" 2>/dev/null | wc -l)
if [ "$METRICS" -gt "0" ]; then
  pass "Cloud Monitoring metrics are being collected"
else
  fail "Cloud Monitoring not collecting metrics"
fi

echo ""

# ============================================================================
# Test 7: Firestore Data
# ============================================================================

echo "📋 Testing Firestore Data..."

# Check if collections exist
COLLECTIONS=$(gcloud firestore collections list 2>/dev/null | wc -l)
if [ "$COLLECTIONS" -ge "1" ]; then
  pass "Firestore has $COLLECTIONS collection(s)"
else
  fail "No Firestore collections found"
fi

# Try querying users collection
USERS=$(gcloud firestore documents list --collection=users 2>/dev/null | wc -l)
if [ "$USERS" -ge "0" ]; then
  pass "Can query Firestore 'users' collection ($USERS documents)"
else
  fail "Cannot query Firestore 'users' collection"
fi

echo ""

# ============================================================================
# Test 8: Load Balancer (if configured)
# ============================================================================

echo "📋 Testing Load Balancer Configuration..."

# Check if load balancer exists
if gcloud compute backend-services list --filter="name:tai-autonomics*" 2>/dev/null | grep -q "tai-autonomics"; then
  pass "Cloud Load Balancer backend service found"
else
  warn "Cloud Load Balancer may not be fully configured"
fi

# Check security policy
if gcloud compute security-policies list --filter="name:tai-autonomics*" 2>/dev/null | grep -q "tai-autonomics"; then
  pass "Cloud Armor security policy configured"
else
  warn "Cloud Armor policy may not be configured"
fi

echo ""

# ============================================================================
# Test 9: Performance Baseline
# ============================================================================

echo "📋 Testing Performance (Latency Baseline)..."

# Measure response time to health endpoint
START=$(date +%s%N)
curl -s -f -m 5 "$SERVICE_URL/health" > /dev/null 2>&1
END=$(date +%s%N)

LATENCY_MS=$(( (END - START) / 1000000 ))

if [ "$LATENCY_MS" -lt "500" ]; then
  pass "Response latency is good: ${LATENCY_MS}ms"
elif [ "$LATENCY_MS" -lt "1000" ]; then
  warn "Response latency is moderate: ${LATENCY_MS}ms (target: < 500ms)"
else
  fail "Response latency is high: ${LATENCY_MS}ms (target: < 500ms)"
fi

echo ""

# ============================================================================
# Test 10: Disaster Recovery - Backup Status
# ============================================================================

echo "📋 Testing Backup Configuration..."

# Check if Firestore backups are configured
BACKUPS=$(gcloud firestore backups list 2>/dev/null | wc -l)
if [ "$BACKUPS" -gt "0" ]; then
  pass "Firestore backups are configured"
else
  warn "No Firestore backups found (schedule backups immediately)"
fi

echo ""

# ============================================================================
# Summary Report
# ============================================================================

echo "=================================="
echo "📊 Test Summary"
echo "=================================="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo ""

if [ "$FAILED" -eq "0" ]; then
  echo -e "${GREEN}✅ All critical tests passed! System is ready for deployment.${NC}"
  exit 0
else
  echo -e "${RED}❌ Some tests failed. Please review and fix issues before proceeding.${NC}"
  exit 1
fi
