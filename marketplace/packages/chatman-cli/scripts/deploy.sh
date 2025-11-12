#!/usr/bin/env bash
set -euo pipefail

# chatman-cli Deployment Script
# Target execution time: ≤30 seconds
# Usage: ./deploy.sh [--publish] [--token CRATES_IO_TOKEN]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

START_TIME=$(date +%s)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Flags
PUBLISH=false
CRATES_TOKEN=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --publish)
      PUBLISH=true
      shift
      ;;
    --token)
      CRATES_TOKEN="$2"
      shift 2
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      exit 1
      ;;
  esac
done

log() {
  echo -e "${BLUE}[$(date +%T)]${NC} $1"
}

success() {
  echo -e "${GREEN}✓${NC} $1"
}

error() {
  echo -e "${RED}✗${NC} $1"
}

warn() {
  echo -e "${YELLOW}⚠${NC} $1"
}

# Step 1: Validate ontology and prerequisites (5s target)
log "Step 1/6: Validating ontology and prerequisites..."
if ! bash "$SCRIPT_DIR/validate.sh"; then
  error "Validation failed. Aborting deployment."
  exit 1
fi
success "Validation complete"

# Step 2: Load ontology from RDF (2s target)
log "Step 2/6: Loading ontology from rdf/ontology.ttl..."
if [[ ! -f "rdf/ontology.ttl" ]]; then
  error "Ontology file not found: rdf/ontology.ttl"
  exit 1
fi

# Extract version from ontology
VERSION=$(grep -m1 "owl:versionInfo" rdf/ontology.ttl | sed 's/.*"\(.*\)".*/\1/' || echo "0.1.0")
success "Loaded ontology version: $VERSION"

# Step 3: Generate CLI from ontology using ggen (5s target)
log "Step 3/6: Generating CLI from ontology using ggen..."
if command -v ggen &> /dev/null; then
  # Use ggen to generate Rust CLI from ontology
  ggen generate --input rdf/ontology.ttl --output src/generated --template rust-cli
  success "Generated CLI code from ontology"
else
  warn "ggen not found in PATH, skipping ontology-driven generation"
  warn "Using pre-generated code"
fi

# Step 4: Build release binary (10s target)
log "Step 4/6: Building release binary..."
if ! cargo build --release --quiet 2>/dev/null; then
  error "Build failed"
  exit 1
fi
success "Build complete"

# Step 5: Run tests (5s target)
log "Step 5/6: Running test suite..."
if ! cargo test --release --quiet 2>/dev/null; then
  error "Tests failed"
  exit 1
fi
success "All tests passed"

# Step 6: Publish to crates.io (3s target)
log "Step 6/6: Publishing to crates.io..."

# Always run dry-run first
log "Running dry-run publish..."
if ! cargo publish --dry-run 2>/dev/null; then
  error "Dry-run publish failed"
  exit 1
fi
success "Dry-run publish successful"

# Actual publish if requested
if [[ "$PUBLISH" == true ]]; then
  if [[ -z "$CRATES_TOKEN" ]]; then
    if [[ -z "${CARGO_REGISTRY_TOKEN:-}" ]]; then
      error "No crates.io token provided. Use --token or set CARGO_REGISTRY_TOKEN"
      exit 1
    fi
    CRATES_TOKEN="$CARGO_REGISTRY_TOKEN"
  fi

  log "Publishing to crates.io (version $VERSION)..."
  if ! cargo publish --token "$CRATES_TOKEN" 2>/dev/null; then
    error "Publish failed"
    exit 1
  fi
  success "Published chatman-cli v$VERSION to crates.io"
else
  warn "Skipping actual publish (use --publish flag to publish)"
fi

# Calculate execution time
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
success "Deployment complete in ${DURATION}s"
if [[ $DURATION -le 30 ]]; then
  success "Target ≤30s: ACHIEVED"
else
  warn "Target ≤30s: MISSED (${DURATION}s)"
fi
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

exit 0
