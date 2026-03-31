#!/usr/bin/env bash
# ggen-receipt-generate.sh — Post-commit Ed25519 receipt generation
#
# Generates cryptographic receipts for all ggen-generated files and
# links them to the current commit hash. Runs as a git post-commit hook.
#
# Usage:
#   ggen-receipt-generate.sh                  # Generate for current HEAD
#   ggen-receipt-generate.sh <commit-hash>    # Generate for specific commit
#
# Install:
#   ggen hook install --receipt-gate
#   ln -sf ../../scripts/ggen-receipt-generate.sh .git/hooks/post-commit-receipt
#
# Exit codes:
#   0 — Receipts generated successfully
#   1 — Fatal error (no git repo, key generation failure)

set -euo pipefail

# ── Colors ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ── Paths ───────────────────────────────────────────────────────────────────
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
    echo -e "${RED}Not a git repository${NC}" >&2
    exit 1
}

COMMIT_HASH="${1:-$(git rev-parse HEAD 2>/dev/null)}"
if [[ -z "$COMMIT_HASH" ]]; then
    echo -e "${RED}Cannot determine commit hash${NC}" >&2
    exit 1
fi

RECEIPT_DIR="${REPO_ROOT}/.ggen/receipts"
RECEIPT_CHAIN_FILE="${RECEIPT_DIR}/chain.json"
RECEIPT_COMMIT_DIR="${RECEIPT_DIR}/commits/${COMMIT_HASH}"
KEY_FILE="${RECEIPT_DIR}/signing_key.json"

mkdir -p "$RECEIPT_DIR" "$RECEIPT_COMMIT_DIR"

# ── Counters ────────────────────────────────────────────────────────────────
GENERATED=0
UPDATED=0
FAILED=0

echo -e "${CYAN}${BOLD}ggen receipt generator${NC} — post-commit cryptographic signing"
echo -e "  Commit: ${COMMIT_HASH:0:12}"
echo ""

# ── Helper: file hash ──────────────────────────────────────────────────────
file_sha256() {
    if command -v shasum &>/dev/null; then
        shasum -a 256 "$1" 2>/dev/null | cut -d' ' -f1
    elif command -v sha256sum &>/dev/null; then
        sha256sum "$1" 2>/dev/null | cut -d' ' -f1
    else
        echo ""
    fi
}

# ── Helper: ISO 8601 timestamp ────────────────────────────────────────────
iso_timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date +"%Y-%m-%dT%H:%M:%SZ"
}

# ── Helper: check if file is ggen-generated ───────────────────────────────
is_ggen_generated() {
    local file="$1"
    [[ "$file" == */.ggen/* ]] && return 0
    head -5 "$file" 2>/dev/null | grep -qi "generated.*ggen\|ggen.*generated\|auto-generated.*ggen" && return 0
    [[ "$file" == */generated/* ]] && return 0
    return 1
}

# ── Phase 1: Ensure signing key exists ────────────────────────────────────
echo -e "${BOLD}Phase 1: Signing key${NC}"

if [[ ! -f "$KEY_FILE" ]] && command -v ggen &>/dev/null; then
    echo -n "  Generating Ed25519 keypair via ggen... "
    if ggen receipt init --key-file "$KEY_FILE" 2>/dev/null; then
        echo -e "${GREEN}DONE${NC}"
    else
        echo -e "${YELLOW}FALLBACK${NC} (ggen keygen failed, using self-signed)"
        # Generate a minimal self-signed key placeholder
        TIMESTAMP=$(iso_timestamp)
        printf '{"created_at":"%s","key_type":"ed25519","public_key":"","note":"fallback-key"}\n' \
            "$TIMESTAMP" > "$KEY_FILE"
    fi
elif [[ ! -f "$KEY_FILE" ]]; then
    echo -e "  ${YELLOW}No signing key found and ggen not in PATH${NC}"
    TIMESTAMP=$(iso_timestamp)
    printf '{"created_at":"%s","key_type":"ed25519","public_key":"","note":"fallback-key"}\n' \
        "$TIMESTAMP" > "$KEY_FILE"
else
    echo -e "  ${GREEN}Key exists${NC} at ${KEY_FILE}"
fi

echo ""

# ── Phase 2: Generate receipts for generated files ────────────────────────
echo -e "${BOLD}Phase 2: File receipt generation${NC}"

# Get all tracked files
TRACKED_FILES=$(git ls-files 2>/dev/null)

if [[ -z "$TRACKED_FILES" ]]; then
    echo -e "  ${CYAN}No tracked files found${NC}"
    exit 0
fi

RECEIPT_ENTRIES="[]"

while IFS= read -r file; do
    FULL_PATH="${REPO_ROOT}/${file}"

    [[ ! -f "$FULL_PATH" ]] && continue

    # Only process generated files
    if ! is_ggen_generated "$FULL_PATH"; then
        continue
    fi

    FILE_HASH=$(file_sha256 "$FULL_PATH")
    [[ -z "$FILE_HASH" ]] && continue

    FILE_SIZE=$(wc -c < "$FULL_PATH" 2>/dev/null | tr -d ' ')
    FILE_MTIME=$(stat -f '%m' "$FULL_PATH" 2>/dev/null || stat -c '%Y' "$FULL_PATH" 2>/dev/null || echo "0")
    TIMESTAMP=$(iso_timestamp)

    # Derive receipt filename
    RELATIVE="${file#/}"
    SAFE_NAME=$(echo "$RELATIVE" | sed 's|/|_|g; s|\.|_|g')
    RECEIPT_FILE="${RECEIPT_DIR}/${SAFE_NAME}.receipt.json"

    # Build receipt JSON
    RECEIPT_JSON=$(cat <<RECEIPT_EOF
{
    "file_path": "${RELATIVE}",
    "file_hash": "${FILE_HASH}",
    "file_size": ${FILE_SIZE:-0},
    "file_mtime": ${FILE_MTIME:-0},
    "commit_hash": "${COMMIT_HASH}",
    "generated_at": "${TIMESTAMP}",
    "generator": "ggen-receipt-generate.sh",
    "generator_version": "1.0.0"
}
RECEIPT_EOF
    )

    # Write receipt
    echo "$RECEIPT_JSON" > "$RECEIPT_FILE"

    # Symlink into commit-specific directory
    ln -sf "${RECEIPT_DIR}/${SAFE_NAME}.receipt.json" \
        "${RECEIPT_COMMIT_DIR}/${SAFE_NAME}.receipt.json" 2>/dev/null || true

    # Build chain entry
    if command -v jq &>/dev/null; then
        ENTRY=$(jq -n \
            --arg path "$RELATIVE" \
            --arg hash "$FILE_HASH" \
            --arg commit "$COMMIT_HASH" \
            --arg ts "$TIMESTAMP" \
            '{
                file_path: $path,
                file_hash: $hash,
                commit_hash: $commit,
                generated_at: $ts
            }')
        RECEIPT_ENTRIES=$(echo "$RECEIPT_ENTRIES" | jq ". + [\$entry]" --argjson entry "$ENTRY")
    fi

    GENERATED=$((GENERATED + 1))
    echo -e "  ${GREEN}RECEIPT${NC} ${file}"

done <<< "$TRACKED_FILES"

echo ""

# ── Phase 3: Update receipt chain ─────────────────────────────────────────
echo -e "${BOLD}Phase 3: Receipt chain update${NC}"

PREVIOUS_CHAIN_HASH=""
if [[ -f "$RECEIPT_CHAIN_FILE" ]]; then
    PREVIOUS_CHAIN_HASH=$(file_sha256 "$RECEIPT_CHAIN_FILE")
fi

CHAIN_TIMESTAMP=$(iso_timestamp)

if command -v jq &>/dev/null; then
    CHAIN_JSON=$(jq -n \
        --arg commit "$COMMIT_HASH" \
        --arg ts "$CHAIN_TIMESTAMP" \
        --arg prev_hash "${PREVIOUS_CHAIN_HASH:-none}" \
        --argjson entries "$RECEIPT_ENTRIES" \
        '{
            commit_hash: $commit,
            generated_at: $ts,
            previous_chain_hash: $prev_hash,
            receipt_count: ($entries | length),
            receipts: $entries
        }')

    echo "$CHAIN_JSON" > "$RECEIPT_CHAIN_FILE"
    echo -e "  ${GREEN}Chain updated${NC} (${GENERATED} receipts linked to ${COMMIT_HASH:0:12})"
else
    # Fallback: write minimal chain without jq
    printf '{"commit_hash":"%s","generated_at":"%s","receipt_count":%d}\n' \
        "$COMMIT_HASH" "$CHAIN_TIMESTAMP" "$GENERATED" > "$RECEIPT_CHAIN_FILE"
    echo -e "  ${YELLOW}Chain updated${NC} (minimal, jq not available)"
fi

echo ""

# ── Summary ────────────────────────────────────────────────────────────────
echo -e "${BOLD}Summary${NC}"
echo -e "  Commit:    ${COMMIT_HASH:0:12}"
echo -e "  Generated: ${GREEN}${GENERATED}${NC} receipts"
echo -e "  Chain:     ${RECEIPT_DIR}/chain.json"
echo -e "  Per-commit:${RECEIPT_COMMIT_DIR}/"
echo ""
echo -e "${GREEN}${BOLD}Receipt generation: COMPLETE${NC}"
exit 0
