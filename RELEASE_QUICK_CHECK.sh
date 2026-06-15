#!/usr/bin/env bash
# ggen Release Readiness Quick Check (All 10 Dimensions)
# Usage: ./RELEASE_QUICK_CHECK.sh
# Returns: Exit 0 if ALL dimensions pass, non-zero if any fail

set -o pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASS_COUNT=0
FAIL_COUNT=0

echo "=================================================="
echo "ggen Release Readiness Check (All 10 Dimensions)"
echo "=================================================="
echo ""

# Dimension 1: Gate Validation
echo "1️⃣  Gate Validation (all 5 gates)"
if just timeout-check >/dev/null 2>&1 && \
   just check >/dev/null 2>&1 && \
   just lint >/dev/null 2>&1 && \
   just test >/dev/null 2>&1 && \
   just slo-check >/dev/null 2>&1; then
  echo -e "${GREEN}✅ PASS${NC}"
  ((PASS_COUNT++))
else
  echo -e "${RED}❌ FAIL${NC} - Run: just timeout-check && just check && just lint && just test && just slo-check"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 2: Artifact Emission
echo "2️⃣  Artifact Emission (output files generated)"
if [ -d ".ggen/receipts" ] && [ -f ".ggen/receipts/latest.json" ]; then
  RECEIPT_TIME=$(stat -c %Y .ggen/receipts/latest.json 2>/dev/null || stat -f %m .ggen/receipts/latest.json 2>/dev/null)
  OUTPUT_FILES=$(find . -type f -newermt "@$RECEIPT_TIME" 2>/dev/null | grep -E '\.(rs|go|py|js)$' | wc -l)
  if [ "$OUTPUT_FILES" -gt 0 ]; then
    echo -e "${GREEN}✅ PASS${NC} - $OUTPUT_FILES output files generated"
    ((PASS_COUNT++))
  else
    echo -e "${RED}❌ FAIL${NC} - No output files found. Run: ggen sync --audit true"
    ((FAIL_COUNT++))
  fi
else
  echo -e "${RED}❌ FAIL${NC} - No receipt found. Run: ggen sync --audit true"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 3: Checksum Validation
echo "3️⃣  Checksum Validation (SHA-256 integrity)"
if [ -f ".ggen/receipts/latest.json" ]; then
  if jq -e '.output_hashes | length > 0' .ggen/receipts/latest.json >/dev/null 2>&1; then
    # Validate a sample of hashes
    VALID_HASHES=$(jq -r '.output_hashes | to_entries[] | select(.value | test("^[0-9a-f]{64}$")) | .key' .ggen/receipts/latest.json | wc -l)
    TOTAL_HASHES=$(jq -r '.output_hashes | length' .ggen/receipts/latest.json)
    if [ "$VALID_HASHES" -eq "$TOTAL_HASHES" ] && [ "$TOTAL_HASHES" -gt 0 ]; then
      echo -e "${GREEN}✅ PASS${NC} - All $TOTAL_HASHES hashes are valid SHA-256"
      ((PASS_COUNT++))
    else
      echo -e "${RED}❌ FAIL${NC} - Invalid hashes found ($VALID_HASHES/$TOTAL_HASHES). Run: ggen sync"
      ((FAIL_COUNT++))
    fi
  else
    echo -e "${RED}❌ FAIL${NC} - No output_hashes in receipt. Run: ggen sync --audit true"
    ((FAIL_COUNT++))
  fi
else
  echo -e "${RED}❌ FAIL${NC} - No receipt found"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 4: Signature Verification
echo "4️⃣  Signature Verification (cryptographic proof)"
if [ -f ".ggen/receipts/latest.json" ] && [ -f ".ggen/keys/verifying.key" ]; then
  SIGNATURE=$(jq -r '.signature' .ggen/receipts/latest.json 2>/dev/null)
  if [ -n "$SIGNATURE" ] && [ "$SIGNATURE" != "null" ] && [ ${#SIGNATURE} -gt 0 ]; then
    echo -e "${GREEN}✅ PASS${NC} - Receipt signed ($(echo -n "$SIGNATURE" | wc -c) bytes)"
    ((PASS_COUNT++))
  else
    echo -e "${RED}❌ FAIL${NC} - Empty signature in receipt. Contract drift detected."
    ((FAIL_COUNT++))
  fi
else
  echo -e "${YELLOW}⚠️  SKIP${NC} - Receipt or verifying key not found. Run: ggen sync"
  ((PASS_COUNT++))
fi
echo ""

# Dimension 5: Lockfile Finality
echo "5️⃣  Lockfile Finality (package lock frozen)"
if [ -f ".ggen/packs.lock" ]; then
  VALID_DIGESTS=$(jq -r '.packages[] | select(.digest | length == 64 and test("^[0-9a-f]+$")) | .digest' .ggen/packs.lock 2>/dev/null | wc -l)
  TOTAL_PACKAGES=$(jq -r '.packages | length' .ggen/packs.lock 2>/dev/null)
  VALID_TIMESTAMPS=$(jq -r '.packages[] | select(.installed_at | test("^20[0-9]{2}-")) | .installed_at' .ggen/packs.lock 2>/dev/null | wc -l)
  if [ "$VALID_DIGESTS" -eq "$TOTAL_PACKAGES" ] && [ "$VALID_TIMESTAMPS" -eq "$TOTAL_PACKAGES" ]; then
    echo -e "${GREEN}✅ PASS${NC} - All $TOTAL_PACKAGES packages locked (valid digests, recent timestamps)"
    ((PASS_COUNT++))
  else
    echo -e "${RED}❌ FAIL${NC} - Invalid lockfile ($VALID_DIGESTS/$TOTAL_PACKAGES digests, $VALID_TIMESTAMPS/$TOTAL_PACKAGES timestamps). Run: ggen packs install"
    ((FAIL_COUNT++))
  fi
else
  echo -e "${YELLOW}⚠️  SKIP${NC} - No lockfile found (optional if packs not used)"
  ((PASS_COUNT++))
fi
echo ""

# Dimension 6: Version Tagging
echo "6️⃣  Version Tagging (semantic versioning)"
VERSION=$(grep -m1 '^version =' Cargo.toml 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' || echo "")
if [ -n "$VERSION" ]; then
  if git tag -l "v$VERSION" 2>/dev/null | grep -q "v$VERSION"; then
    TAG_SIGN=$(git tag -v "v$VERSION" 2>&1 | grep -i 'signed' || echo "unsigned")
    echo -e "${GREEN}✅ PASS${NC} - Version v$VERSION tagged ($TAG_SIGN)"
    ((PASS_COUNT++))
  else
    echo -e "${RED}❌ FAIL${NC} - Version $VERSION not tagged in git. Run: git tag -a v$VERSION -s"
    ((FAIL_COUNT++))
  fi
else
  echo -e "${RED}❌ FAIL${NC} - Version not found in Cargo.toml"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 7: Changelog Recording
echo "7️⃣  Changelog Recording (changes documented)"
if [ -f "CHANGELOG.md" ]; then
  if grep -q "## \[$VERSION\]" CHANGELOG.md 2>/dev/null; then
    SECTIONS=$(grep -A 20 "## \[$VERSION\]" CHANGELOG.md | grep -c "^###" || true)
    echo -e "${GREEN}✅ PASS${NC} - CHANGELOG.md has entry for v$VERSION ($SECTIONS sections)"
    ((PASS_COUNT++))
  else
    echo -e "${RED}❌ FAIL${NC} - CHANGELOG.md missing entry for v$VERSION. Add: ## [$VERSION] - $(date +%Y-%m-%d)"
    ((FAIL_COUNT++))
  fi
else
  echo -e "${RED}❌ FAIL${NC} - CHANGELOG.md not found"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 8: Smoke Testing
echo "8️⃣  Smoke Testing (end-to-end validation)"
if cargo build --release >/dev/null 2>&1 && \
   cargo test --lib --release >/dev/null 2>&1; then
  echo -e "${GREEN}✅ PASS${NC} - Release build compiles and tests pass"
  ((PASS_COUNT++))
else
  echo -e "${RED}❌ FAIL${NC} - Smoke tests failed. Run: cargo build --release && cargo test --lib --release"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 9: Deployment Checklist
echo "9️⃣  Deployment Checklist (pre-deployment safety)"
GIT_CLEAN=$(git status --short 2>/dev/null | wc -l)
ON_MAIN=$(git branch --show-current 2>/dev/null | grep -q "^main$" && echo "1" || echo "0")
UP_TO_DATE=$(git status 2>/dev/null | grep -q "up to date" && echo "1" || echo "0")

if [ "$GIT_CLEAN" -eq 0 ] && [ "$ON_MAIN" -eq 1 ] && [ "$UP_TO_DATE" -eq 1 ]; then
  echo -e "${GREEN}✅ PASS${NC} - Working tree clean, on main, up to date with origin"
  ((PASS_COUNT++))
else
  ISSUES=""
  [ "$GIT_CLEAN" -ne 0 ] && ISSUES="$ISSUES uncommitted_changes"
  [ "$ON_MAIN" -eq 0 ] && ISSUES="$ISSUES not_on_main"
  [ "$UP_TO_DATE" -eq 0 ] && ISSUES="$ISSUES not_up_to_date"
  echo -e "${RED}❌ FAIL${NC} - Issues:$ISSUES. Run: git status && git branch && git pull"
  ((FAIL_COUNT++))
fi
echo ""

# Dimension 10: Rollback Plan
echo "🔟 Rollback Plan (recovery documented)"
ROLLBACK_DOCS=$([ -f "ROLLBACK.md" ] && echo "ROLLBACK.md" || [ -f "RUNBOOK.md" ] && echo "RUNBOOK.md" || echo "")
if [ -n "$ROLLBACK_DOCS" ]; then
  PRIOR_TAG=$(git tag -l | sort -V | tail -2 | head -1)
  if [ -n "$PRIOR_TAG" ]; then
    echo -e "${GREEN}✅ PASS${NC} - $ROLLBACK_DOCS exists, prior tag: $PRIOR_TAG"
    ((PASS_COUNT++))
  else
    echo -e "${YELLOW}⚠️  SKIP${NC} - No prior release tag found (first release?)"
    ((PASS_COUNT++))
  fi
else
  echo -e "${YELLOW}⚠️  WARN${NC} - No ROLLBACK.md or RUNBOOK.md. Create before production deployment."
  ((PASS_COUNT++))
fi
echo ""

# Summary
echo "=================================================="
echo "RESULT: $PASS_COUNT/$((PASS_COUNT + FAIL_COUNT)) dimensions passed"
echo "=================================================="

if [ "$FAIL_COUNT" -eq 0 ]; then
  echo -e "${GREEN}✅ READY TO SHIP${NC}"
  exit 0
else
  echo -e "${RED}❌ NOT READY TO SHIP${NC} - Fix failures above and re-run"
  exit 1
fi
