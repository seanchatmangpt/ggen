#!/usr/bin/env bash
set -euo pipefail

# chatman-cli Pre-Deployment Validation Script
# Validates RDF ontology, SPARQL queries, and package integrity

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

ERRORS=0
WARNINGS=0

log() {
  echo -e "${BLUE}[VALIDATE]${NC} $1"
}

success() {
  echo -e "${GREEN}✓${NC} $1"
}

error() {
  echo -e "${RED}✗${NC} $1"
  ((ERRORS++))
}

warn() {
  echo -e "${YELLOW}⚠${NC} $1"
  ((WARNINGS++))
}

# Validation 1: RDF Ontology
log "Validating RDF ontology..."
if [[ ! -f "rdf/ontology.ttl" ]]; then
  error "Missing ontology file: rdf/ontology.ttl"
else
  # Try rapper first (from raptor2-utils)
  if command -v rapper &> /dev/null; then
    if rapper -i turtle -o ntriples rdf/ontology.ttl > /dev/null 2>&1; then
      success "RDF ontology is valid (rapper)"
    else
      error "RDF ontology validation failed (rapper)"
    fi
  # Try riot (from Apache Jena)
  elif command -v riot &> /dev/null; then
    if riot --validate rdf/ontology.ttl > /dev/null 2>&1; then
      success "RDF ontology is valid (riot)"
    else
      error "RDF ontology validation failed (riot)"
    fi
  else
    warn "No RDF validator found (rapper/riot), skipping syntax validation"
    success "Ontology file exists"
  fi

  # Check required ontology elements
  if grep -q "owl:Ontology" rdf/ontology.ttl; then
    success "Ontology declares owl:Ontology"
  else
    error "Missing owl:Ontology declaration"
  fi

  if grep -q "owl:versionInfo" rdf/ontology.ttl; then
    VERSION=$(grep -m1 "owl:versionInfo" rdf/ontology.ttl | sed 's/.*"\(.*\)".*/\1/')
    success "Ontology version: $VERSION"
  else
    warn "Missing owl:versionInfo"
  fi
fi

# Validation 2: SPARQL Queries
log "Validating SPARQL queries..."
if [[ -d "sparql" ]]; then
  SPARQL_COUNT=$(find sparql -name "*.rq" -o -name "*.sparql" 2>/dev/null | wc -l | tr -d ' ')
  if [[ $SPARQL_COUNT -gt 0 ]]; then
    success "Found $SPARQL_COUNT SPARQL queries"

    # Basic SPARQL syntax check
    shopt -s nullglob
    for query in sparql/*.rq sparql/*.sparql; do
      if [[ -f "$query" ]]; then
        if grep -q "SELECT\|CONSTRUCT\|ASK\|DESCRIBE" "$query"; then
          success "  $(basename "$query") - valid SPARQL structure"
        else
          warn "  $(basename "$query") - no SPARQL query type found"
        fi
      fi
    done
    shopt -u nullglob
  else
    warn "No SPARQL queries found in sparql/"
  fi
else
  warn "No sparql/ directory found"
fi

# Validation 3: Cargo.toml Completeness
log "Validating Cargo.toml..."
if [[ ! -f "Cargo.toml" ]]; then
  error "Missing Cargo.toml"
else
  required_fields=("name" "version" "edition" "description" "license" "repository")
  for field in "${required_fields[@]}"; do
    if grep -q "^$field\s*=" Cargo.toml; then
      success "  $field: present"
    else
      error "  $field: missing (required for crates.io)"
    fi
  done

  # Check for README
  if grep -q "^readme\s*=" Cargo.toml || [[ -f "README.md" ]]; then
    success "  README: present"
  else
    warn "  README: missing (recommended for crates.io)"
  fi
fi

# Validation 4: License Files
log "Validating license files..."
if [[ -f "LICENSE" ]] || [[ -f "LICENSE-MIT" ]] || [[ -f "LICENSE-APACHE" ]]; then
  success "License file(s) present"
else
  error "Missing LICENSE file (required for crates.io)"
fi

# Validation 5: Cargo Clippy
log "Running cargo clippy..."
if command -v cargo-clippy &> /dev/null || cargo clippy --version &> /dev/null; then
  if cargo clippy --all-targets --all-features -- -D warnings 2>&1 | grep -q "warning\|error"; then
    warn "Clippy found issues"
  else
    success "Clippy checks passed"
  fi
else
  warn "cargo-clippy not installed, skipping lints"
fi

# Validation 6: Cargo Format Check
log "Checking code formatting..."
if cargo fmt --version &> /dev/null; then
  if cargo fmt --all -- --check > /dev/null 2>&1; then
    success "Code formatting is correct"
  else
    warn "Code needs formatting (run: cargo fmt)"
  fi
else
  warn "rustfmt not installed, skipping format check"
fi

# Validation 7: 43 Workflow Patterns
log "Validating 43 workflow patterns..."
if [[ -f "rdf/ontology.ttl" ]]; then
  PATTERN_COUNT=$(grep -c "chatman:WorkflowPattern" rdf/ontology.ttl || echo 0)
  if [[ $PATTERN_COUNT -ge 43 ]]; then
    success "Found $PATTERN_COUNT workflow patterns (≥43 required)"
  else
    error "Only $PATTERN_COUNT workflow patterns found (43 required)"
  fi
fi

# Validation 8: Lockchain Cryptographic Receipt Schema
log "Validating Lockchain receipt schema..."
if grep -q "lockchain:CryptographicReceipt\|lockchain:Receipt" rdf/ontology.ttl 2>/dev/null; then
  success "Lockchain receipt schema defined"
else
  warn "Lockchain receipt schema not found in ontology"
fi

# Validation 9: Source Code Structure
log "Validating source code structure..."
if [[ -f "src/main.rs" ]] || [[ -f "src/lib.rs" ]]; then
  success "Rust source code present"
else
  error "Missing src/main.rs or src/lib.rs"
fi

# Validation 10: Dependencies Security Check
log "Checking dependencies..."
if command -v cargo-audit &> /dev/null; then
  if cargo audit > /dev/null 2>&1; then
    success "No known security vulnerabilities"
  else
    warn "cargo-audit found potential issues"
  fi
else
  warn "cargo-audit not installed, skipping security check"
fi

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [[ $ERRORS -eq 0 ]]; then
  success "Validation complete: $WARNINGS warnings, $ERRORS errors"
  exit 0
else
  error "Validation failed: $WARNINGS warnings, $ERRORS errors"
  exit 1
fi
