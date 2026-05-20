#!/bin/bash
# validate_release.sh - Comprehensive Release Validation Suite
# This script validates the binary, Homebrew installation, and core CLI functionality.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_step() { echo -e "${YELLOW}${BOLD}--- $1 ---${NC}"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

VERSION=$(grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')
ROOT_DIR=$(pwd)
TEMP_WORK="/tmp/ggen-release-val-$$"
mkdir -p "$TEMP_WORK"

log_info "Starting validation for ggen v${VERSION}"

# 1. Build Release Binary
log_step "Building release binary"
cargo build -p ggen-cli-lib --bin ggen --release
BINARY_PATH="$ROOT_DIR/target/release/ggen"

# 2. Package and Checksum
log_step "Packaging binary"
tar -czf "$TEMP_WORK/ggen.tar.gz" -C "$ROOT_DIR/target/release" ggen
SHA=$(shasum -a 256 "$TEMP_WORK/ggen.tar.gz" | cut -d' ' -f1)
log_info "Package SHA256: $SHA"

# 3. Local Homebrew Tap Setup
log_step "Setting up local Homebrew tap"
brew tap-new local/ggen-release --no-git || true
TAP_DIR=$(brew --repository local/ggen-release)

cat > "$TAP_DIR/Formula/ggen.rb" <<EOF
class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "${VERSION}"
  url "file://$TEMP_WORK/ggen.tar.gz"
  sha256 "${SHA}"
  def install
    bin.install "ggen"
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
EOF

# 4. Install via Homebrew
log_step "Installing via Homebrew"
brew uninstall ggen || true
brew install local/ggen-release/ggen

# Verify path
BREW_BIN="/opt/homebrew/bin/ggen"
if [[ ! -f "$BREW_BIN" ]]; then
    BREW_BIN="/usr/local/bin/ggen"
fi
log_info "Installed binary location: $BREW_BIN"

# 5. Version Verification
log_step "Verifying version"
INSTALLED_VER=$($BREW_BIN --version | tail -n 1)
log_info "Reported version: $INSTALLED_VER"
if [[ "$INSTALLED_VER" != "ggen ${VERSION}" ]]; then
    log_error "Version mismatch! Expected 'ggen ${VERSION}', got '$INSTALLED_VER'"
    exit 1
fi

# 6. 'ggen sync' Verification
log_step "Testing 'ggen sync' functionality"
SYNC_TEST_DIR="$TEMP_WORK/sync-test"
mkdir -p "$SYNC_TEST_DIR"
cd "$SYNC_TEST_DIR"

log_info "Initializing project..."
"$BREW_BIN" init --force=true

log_info "Setting up meaningful ontology and rules..."
# Add data to domain.ttl
cat >> schema/domain.ttl <<EOF

# Sample data for validation
<https://ggen.dev/person/Alice> a schema:Person ;
    rdfs:label "Alice" ;
    schema:age 30 ;
    schema:email "alice@example.com" .

<https://ggen.dev/person/Bob> a schema:Person ;
    rdfs:label "Bob" ;
    schema:age 15 ;
    schema:email "bob@example.com" .
EOF

# Update ggen.toml with a more meaningful inference rule
# This rule identifies VIPs (age > 18)
cat > ggen.toml <<EOF
[project]
name = "release-verification"
version = "${VERSION}"
description = "Meaningful sync verification"
authors = ["Validation Script"]

[ontology]
source = "schema/domain.ttl"

[inference]
rules = [
    { name = "vip-detection", construct = "PREFIX schema: <https://schema.org/> CONSTRUCT { ?s a schema:VipPerson } WHERE { ?s a schema:Person . ?s schema:age ?age . FILTER(?age > 18) }" }
]

[generation]
output_dir = "output"
rules = [
    { 
      name = "gen-vips", 
      query = { inline = "PREFIX schema: <https://schema.org/> SELECT ?label WHERE { ?s a schema:VipPerson . ?s <http://www.w3.org/2000/01/rdf-schema#label> ?label }" },
      template = { inline = "VIP List:\n{% for row in results %}- {{ row.label }}\n{% endfor %}" },
      output_file = "vips.txt"
    }
]
EOF

log_info "Running ggen sync (full execution)..."
"$BREW_BIN" sync

# Verify output
log_info "Verifying generated content..."
if [[ ! -f "output/vips.txt" ]]; then
    log_error "Sync failed to produce output/vips.txt"
    exit 1
fi

CONTENTS=$(cat output/vips.txt)
log_info "Generated VIPs:\n${CONTENTS}"

if [[ "$CONTENTS" == *"Alice"* ]] && [[ "$CONTENTS" != *"Bob"* ]]; then
    log_info "✅ Sync logic verified: Alice is a VIP, Bob is not."
else
    log_error "Sync logic failure! Output content is incorrect."
    exit 1
fi

log_info "Sync verification passed."

# 7. JTBD Smoke Tests
log_step "Running 80/20 JTBD suite using Brew binary"
cd "$ROOT_DIR"
export GGEN_BIN="$BREW_BIN"
bash scripts/jtbd-all-verbs.sh

# 8. Preservation of test files
log_step "Preserving Homebrew test files"
mkdir -p scripts/release-test
cp "$TAP_DIR/Formula/ggen.rb" scripts/release-test/ggen.rb
log_info "Homebrew formula preserved at scripts/release-test/ggen.rb"

# 9. Cleanup
log_step "Cleaning up environment"
brew uninstall ggen
brew untap local/ggen-release
rm -rf "$TEMP_WORK"

echo -e "\n${GREEN}${BOLD}========================================${NC}"
echo -e "${GREEN}${BOLD}   RELEASE VALIDATION PASSED (v${VERSION})   ${NC}"
echo -e "${GREEN}${BOLD}========================================${NC}"
echo -e "You can re-run this validation with: bash scripts/validate_release.sh"
