#!/bin/bash
# GCP Readiness Validation Script
# Verifies that the project is ready for GCP deployment

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 Checking GCP Readiness..."
echo ""

ERRORS=0
WARNINGS=0

# Check required tools
check_tool() {
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}✅${NC} $1 installed"
    else
        echo -e "${RED}❌${NC} $1 not found"
        ERRORS=$((ERRORS + 1))
    fi
}

echo "📦 Checking required tools..."
check_tool "rebar3"
check_tool "docker"
check_tool "terraform"
check_tool "gcloud"
echo ""

# Check GCP authentication
echo "🔐 Checking GCP authentication..."
if gcloud auth list --filter=status:ACTIVE --format="value(account)" | grep -q .; then
    ACTIVE_ACCOUNT=$(gcloud auth list --filter=status:ACTIVE --format="value(account)" | head -n1)
    echo -e "${GREEN}✅${NC} Authenticated as: $ACTIVE_ACCOUNT"
else
    echo -e "${RED}❌${NC} Not authenticated to GCP"
    echo "   Run: gcloud auth login"
    ERRORS=$((ERRORS + 1))
fi

if gcloud auth application-default print-access-token &> /dev/null; then
    echo -e "${GREEN}✅${NC} Application default credentials configured"
else
    echo -e "${YELLOW}⚠️${NC}  Application default credentials not configured"
    echo "   Run: gcloud auth application-default login"
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# Check GCP project
echo "🏗️  Checking GCP project..."
if [ -z "$GCP_PROJECT_ID" ]; then
    CURRENT_PROJECT=$(gcloud config get-value project 2>/dev/null)
    if [ -n "$CURRENT_PROJECT" ]; then
        echo -e "${GREEN}✅${NC} Current project: $CURRENT_PROJECT"
        export GCP_PROJECT_ID=$CURRENT_PROJECT
    else
        echo -e "${RED}❌${NC} GCP_PROJECT_ID not set and no default project"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo -e "${GREEN}✅${NC} GCP_PROJECT_ID: $GCP_PROJECT_ID"
    gcloud config set project "$GCP_PROJECT_ID" &> /dev/null || true
fi
echo ""

# Check Erlang build
echo "🔨 Checking Erlang build..."
if rebar3 compile &> /dev/null; then
    echo -e "${GREEN}✅${NC} Erlang code compiles"
else
    echo -e "${RED}❌${NC} Erlang compilation failed"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Check Docker Compose port conflicts
echo "🐳 Checking Docker Compose configuration..."
if [ -f "docker-compose.yml" ]; then
    # Check for port conflicts in docker-compose.yml
    PORT_8080_COUNT=$(grep -c "8080:8080" docker-compose.yml || echo "0")
    PORT_8081_COUNT=$(grep -c "8081:8081" docker-compose.yml || echo "0")
    
    if [ "$PORT_8080_COUNT" -gt 1 ]; then
        echo -e "${RED}❌${NC} Port 8080 conflict detected in docker-compose.yml"
        echo "   Multiple services are trying to use port 8080"
        ERRORS=$((ERRORS + 1))
    else
        echo -e "${GREEN}✅${NC} No port conflicts detected in docker-compose.yml"
    fi
else
    echo -e "${YELLOW}⚠️${NC}  docker-compose.yml not found"
    WARNINGS=$((WARNINGS + 1))
fi
echo ""

# Check Terraform configuration
echo "🏗️  Checking Terraform configuration..."
if [ -d "terraform" ]; then
    cd terraform
    if terraform init -backend=false &> /dev/null; then
        echo -e "${GREEN}✅${NC} Terraform configuration valid"
    else
        echo -e "${RED}❌${NC} Terraform configuration invalid"
        ERRORS=$((ERRORS + 1))
    fi
    cd ..
else
    echo -e "${RED}❌${NC} terraform/ directory not found"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Check required GCP APIs
echo "🔌 Checking GCP APIs..."
if [ -n "$GCP_PROJECT_ID" ]; then
    REQUIRED_APIS=(
        "run.googleapis.com"
        "pubsub.googleapis.com"
        "firestore.googleapis.com"
        "cloudbuild.googleapis.com"
        "artifactregistry.googleapis.com"
    )
    
    for API in "${REQUIRED_APIS[@]}"; do
        if gcloud services list --enabled --project="$GCP_PROJECT_ID" --filter="name:$API" --format="value(name)" | grep -q "$API"; then
            echo -e "${GREEN}✅${NC} $API enabled"
        else
            echo -e "${YELLOW}⚠️${NC}  $API not enabled"
            WARNINGS=$((WARNINGS + 1))
        fi
    done
fi
echo ""

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ All checks passed! Ready for GCP deployment.${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠️  Ready with $WARNINGS warning(s)${NC}"
    exit 0
else
    echo -e "${RED}❌ Found $ERRORS error(s) and $WARNINGS warning(s)${NC}"
    echo ""
    echo "Please fix the errors before deploying to GCP."
    exit 1
fi
