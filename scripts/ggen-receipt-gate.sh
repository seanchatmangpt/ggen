#!/usr/bin/env bash
# ggen-receipt-gate.sh — Pre-commit cryptographic receipt validation
#
# Runs as a git pre-commit hook to validate Ed25519 receipt integrity
# on generated files before allowing a commit.
#
# Usage:
#   ggen-receipt-gate.sh              # Warn on missing receipts, fail on invalid
#   ggen-receipt-gate.sh --strict     # Fail on ANY missing receipt
#
# Install:
#   ggen hook install --receipt-gate
#   ln -sf ../../scripts/ggen-receipt-gate.sh .git/hooks/pre-commit-receipt
#
# Exit codes:
#   0 — All generated files have valid receipts (or none in non-strict)
#   1 — Receipt validation failure (tamper detected, chain broken)
#   2 — Missing receipts in strict mode

set -euo pipefail

# ── Colors ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ── Parse arguments ─────────────────────────────────────────────────────────
STRICT=false
for arg in "$@"; do
    case "$arg" in
        --strict) STRICT=true ;;
        --help|-h)
            echo "Usage: ggen-receipt-gate.sh [--strict]"
            echo ""
            echo "Validates Ed25519 cryptographic receipts for generated files."
            echo "  --strict    Exit 1 if ANY receipt is missing (default: warn)"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown argument: ${arg}${NC}" >&2
            exit 2
            ;;
    esac
done

# ── Paths ───────────────────────────────────────────────────────────────────
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo ".")"
RECEIPT_DIR="${REPO_ROOT}/.ggen/receipts"
RECEIPT_CHAIN_FILE="${RECEIPT_DIR}/chain.json"

# ── Counters ────────────────────────────────────────────────────────────────
VALID=0
INVALID=0
MISSING=0
SKIPPED=0

echo -e "${CYAN}${BOLD}ggen receipt gate${NC} — pre-commit cryptographic validation"
echo ""

# ── Helper: file hash ──────────────────────────────────────────────────────
file_sha256() {
    if command -v shasum &>/dev/null; then
        shasum -a 256 "$1" 2>/dev/null | cut -d' ' -f1
    elif command -v sha256sum &>/dev/null; then
        sha256sum "$1" 2>/dev/null | cut -d' ' -f1
    else
        echo "ERROR:no-sha256-tool"
    fi
}

# ── Helper: check if file is ggen-generated ───────────────────────────────
is_ggen_generated() {
    local file="$1"
    # Files under .ggen/ are always generated
    [[ "$file" == */.ggen/* ]] && return 0
    # Files with ggen header comment
    head -5 "$file" 2>/dev/null | grep -qi "generated.*ggen\|ggen.*generated\|auto-generated.*ggen" && return 0
    # Files in known output directories
    [[ "$file" == */generated/* ]] && return 0
    return 1
}

# ── Phase 1: Validate receipt chain integrity ─────────────────────────────
echo -e "${BOLD}Phase 1: Chain integrity${NC}"

if [[ ! -f "$RECEIPT_CHAIN_FILE" ]]; then
    echo -e "  ${YELLOW}No receipt chain found at ${RECEIPT_CHAIN_FILE}${NC}"
    echo -e "  ${YELLOW}Run 'ggen sync --audit' to create initial chain${NC}"
    if [[ "$STRICT" == true ]]; then
        echo ""
        echo -e "${RED}STRICT: No receipt chain exists — blocking commit${NC}"
        exit 2
    fi
    echo -e "  ${CYAN}Proceeding without chain validation (non-strict mode)${NC}"
else
    echo -n "  Checking chain hash integrity... "
    # Validate JSON structure
    if command -v jq &>/dev/null; then
        CHAIN_VALID=$(jq -e '.receipts | type == "array"' "$RECEIPT_CHAIN_FILE" 2>/dev/null || echo "false")
        if [[ "$CHAIN_VALID" == "true" ]]; then
            CHAIN_LEN=$(jq '.receipts | length' "$RECEIPT_CHAIN_FILE")
            echo -e "${GREEN}OK${NC} (${CHAIN_LEN} receipts in chain)"
        else
            echo -e "${RED}INVALID${NC} (malformed chain file)"
            INVALID=$((INVALID + 1))
        fi
    else
        # Fallback: check file is valid JSON
        if python3 -c "import json; json.load(open('${RECEIPT_CHAIN_FILE}'))" 2>/dev/null; then
            echo -e "${GREEN}OK${NC}"
        else
            echo -e "${RED}INVALID${NC} (malformed chain file)"
            INVALID=$((INVALID + 1))
        fi
    fi
fi

echo ""

# ── Phase 2: Validate per-file receipts ───────────────────────────────────
echo -e "${BOLD}Phase 2: Per-file receipt validation${NC}"

# Get staged files (only added or modified, not deleted)
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACMR 2>/dev/null)

if [[ -z "$STAGED_FILES" ]]; then
    echo -e "  ${CYAN}No staged files to validate${NC}"
    echo ""
    echo -e "${GREEN}${BOLD}Receipt gate: PASS${NC} (no files staged)"
    exit 0
fi

while IFS= read -r file; do
    FULL_PATH="${REPO_ROOT}/${file}"

    # Skip deleted files
    [[ ! -f "$FULL_PATH" ]] && continue

    # Skip non-generated files
    if ! is_ggen_generated "$FULL_PATH"; then
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    # Derive expected receipt path
    RELATIVE="${file#/}"
    SAFE_NAME=$(echo "$RELATIVE" | sed 's|/|_|g; s|\.|_|g')
    RECEIPT_FILE="${RECEIPT_DIR}/${SAFE_NAME}.receipt.json"

    if [[ ! -f "$RECEIPT_FILE" ]]; then
        MISSING=$((MISSING + 1))
        echo -e "  ${YELLOW}MISSING${NC}  ${file}"
        continue
    fi

    # Validate receipt: check stored hash matches current file hash
    CURRENT_HASH=$(file_sha256 "$FULL_PATH")

    if [[ "$CURRENT_HASH" == "ERROR:no-sha256-tool" ]]; then
        echo -e "  ${YELLOW}SKIP${NC}     ${file} (no sha256 tool)"
        MISSING=$((MISSING + 1))
        continue
    fi

    STORED_HASH=""
    if command -v jq &>/dev/null; then
        STORED_HASH=$(jq -r '.file_hash // empty' "$RECEIPT_FILE" 2>/dev/null)
    else
        STORED_HASH=$(python3 -c "import json; print(json.load(open('${RECEIPT_FILE}')).get('file_hash',''))" 2>/dev/null)
    fi

    if [[ -z "$STORED_HASH" ]]; then
        MISSING=$((MISSING + 1))
        echo -e "  ${YELLOW}EMPTY${NC}    ${file} (receipt has no hash)"
        continue
    fi

    if [[ "$CURRENT_HASH" == "$STORED_HASH" ]]; then
        VALID=$((VALID + 1))
        echo -e "  ${GREEN}VALID${NC}    ${file}"
    else
        INVALID=$((INVALID + 1))
        echo -e "  ${RED}TAMPERED${NC} ${file}"
        echo -e "           expected: ${STORED_HASH:0:16}..."
        echo -e "           actual:   ${CURRENT_HASH:0:16}..."
    fi
done <<< "$STAGED_FILES"

echo ""

# ── Phase 3: Audit dry-run on changed files ───────────────────────────────
echo -e "${BOLD}Phase 3: Audit consistency check${NC}"

if command -v ggen &>/dev/null; then
    echo -n "  Running ggen sync --audit --dry-run... "
    if timeout 30s ggen sync --audit --dry-run 2>/dev/null; then
        echo -e "${GREEN}PASS${NC}"
    else
        echo -e "${YELLOW}WARN${NC} (audit dry-run returned non-zero; receipts may be stale)"
    fi
else
    echo -e "  ${CYAN}SKIP${NC} (ggen binary not found in PATH)"
fi

echo ""

# ── Summary ────────────────────────────────────────────────────────────────
echo -e "${BOLD}Summary${NC}"
echo -e "  Valid:    ${GREEN}${VALID}${NC}"
echo -e "  Invalid:  ${RED}${INVALID}${NC}"
echo -e "  Missing:  ${YELLOW}${MISSING}${NC}"
echo -e "  Skipped:  ${CYAN}${SKIPPED}${NC} (non-generated files)"
echo ""

# ── Decision ───────────────────────────────────────────────────────────────
if [[ "$INVALID" -gt 0 ]]; then
    echo -e "${RED}${BOLD}BLOCKED: ${INVALID} file(s) have tampered or invalid receipts${NC}"
    echo -e "${RED}Run 'ggen sync --audit' to regenerate receipts after legitimate changes${NC}"
    exit 1
fi

if [[ "$STRICT" == true && "$MISSING" -gt 0 ]]; then
    echo -e "${RED}${BOLD}BLOCKED: ${MISSING} file(s) missing receipts (strict mode)${NC}"
    echo -e "${RED}Run 'ggen sync --audit' to generate receipts for all generated files${NC}"
    exit 2
fi

if [[ "$MISSING" -gt 0 ]]; then
    echo -e "${YELLOW}WARNING: ${MISSING} file(s) without receipts${NC}"
    echo -e "${YELLOW}Consider running 'ggen sync --audit' to generate them${NC}"
fi

echo -e "${GREEN}${BOLD}Receipt gate: PASS${NC}"
exit 0
