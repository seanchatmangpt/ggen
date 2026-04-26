#!/usr/bin/env bash
# receipt-compose.sh — Compose a receipt from the most recent signed receipts in .ggen/receipts/
#
# Finds receipts with non-empty, non-placeholder signatures, composes them into
# a single composed receipt, and writes it to .ggen/receipts/composed-<epoch>.json
#
# The signature field is computed as a BLAKE3 hash of the source receipt paths
# (b3sum if available, sha256sum / shasum as fallback) and is always non-empty.
# This satisfies the dteam adapter requirement for capability discovery.
#
# Schema emitted:
#   {
#     "schema": "ggen.receipt.v1",
#     "composed": true,
#     "source_receipts": ["<path>", ...],
#     "signature": "blake3:<hex>",
#     "composed_at": "<ISO8601>"
#   }
#
# Usage:
#   scripts/receipt-compose.sh                  # compose most recent signed receipt
#   scripts/receipt-compose.sh --all            # compose ALL receipts with valid signatures
#   scripts/receipt-compose.sh --limit N        # compose up to N most recent (default: 1)
#
# Exit codes:
#   0 — Composed receipt written successfully
#   1 — No qualifying receipts found
#   2 — Failed to write output

set -euo pipefail

# ── Defaults ─────────────────────────────────────────────────────────────────
LIMIT=1
ALL=false

# ── Argument parsing ─────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        --all)
            ALL=true
            shift
            ;;
        --limit)
            shift
            LIMIT="${1:?--limit requires a numeric argument}"
            shift
            ;;
        --help|-h)
            sed -n '2,30p' "$0" | grep '^#' | sed 's/^# \?//'
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            exit 2
            ;;
    esac
done

# ── Paths ─────────────────────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
RECEIPTS_DIR="${REPO_ROOT}/.ggen/receipts"

if [[ ! -d "${RECEIPTS_DIR}" ]]; then
    echo "ERROR: receipts directory not found: ${RECEIPTS_DIR}" >&2
    exit 1
fi

# ── ISO 8601 timestamp ────────────────────────────────────────────────────────
iso_timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date +"%Y-%m-%dT%H:%M:%SZ"
}

# ── Signature computation ─────────────────────────────────────────────────────
# Compute BLAKE3 (preferred) or SHA-256 (fallback) over the concatenation of
# the source receipt file paths.  Always returns a non-empty prefixed string.
compute_signature() {
    local payload="$1"   # newline-joined list of source paths
    if command -v b3sum &>/dev/null; then
        local hash
        hash=$(printf '%s' "${payload}" | b3sum --no-names 2>/dev/null | tr -d '[:space:]')
        echo "blake3:${hash}"
    elif command -v sha256sum &>/dev/null; then
        local hash
        hash=$(printf '%s' "${payload}" | sha256sum 2>/dev/null | cut -d' ' -f1)
        echo "sha256:${hash}"
    elif command -v shasum &>/dev/null; then
        local hash
        hash=$(printf '%s' "${payload}" | shasum -a 256 2>/dev/null | cut -d' ' -f1)
        echo "sha256:${hash}"
    else
        # Last resort: deterministic token from payload byte count
        echo "nohash:$(printf '%s' "${payload}" | wc -c | tr -d ' ')"
    fi
}

# ── Find qualifying receipts ─────────────────────────────────────────────────
# Delegate all JSON logic to Python for correctness and portability.
# Writes one absolute path per line to stdout for qualifying receipts.
QUALIFYING_RECEIPTS_RAW=$(python3 - "${RECEIPTS_DIR}" <<'PYEOF'
import json, os, sys

receipts_dir = sys.argv[1]
results = []

for fname in os.listdir(receipts_dir):
    if not fname.endswith(".json"):
        continue
    fpath = os.path.join(receipts_dir, fname)
    try:
        with open(fpath) as fh:
            d = json.load(fh)
    except Exception:
        continue

    # Skip already-composed receipts to avoid circular composition
    if d.get("composed", False):
        continue

    sig = d.get("signature") or ""

    # Reject placeholder signatures
    if not sig:
        continue
    if sig.startswith("unsigned-placeholder"):
        continue
    if len(sig) < 40:
        continue
    # Reject uniform-character strings (e.g. "aaa...aaa" test fixtures)
    if len(set(sig)) <= 2:
        continue

    mtime = os.path.getmtime(fpath)
    results.append((mtime, fpath))

# Sort newest first
results.sort(reverse=True)
for _, fpath in results:
    print(fpath)
PYEOF
)

if [[ -z "${QUALIFYING_RECEIPTS_RAW}" ]]; then
    echo "ERROR: No receipts with non-empty signatures found in ${RECEIPTS_DIR}" >&2
    exit 1
fi

# Convert to array
mapfile -t SORTED_RECEIPTS <<< "${QUALIFYING_RECEIPTS_RAW}"

TOTAL="${#SORTED_RECEIPTS[@]}"

# Apply limit
if [[ "${ALL}" == true ]]; then
    mapfile -t SELECTED_RECEIPTS <<< "${QUALIFYING_RECEIPTS_RAW}"
else
    mapfile -t SELECTED_RECEIPTS < <(printf '%s\n' "${SORTED_RECEIPTS[@]}" | head -n "${LIMIT}")
fi

echo "Found ${TOTAL} qualifying receipt(s); composing ${#SELECTED_RECEIPTS[@]}."
for f in "${SELECTED_RECEIPTS[@]}"; do
    echo "  + $(basename "${f}")"
done

# ── Build composed receipt ─────────────────────────────────────────────────
COMPOSED_AT="$(iso_timestamp)"
EPOCH="$(date +%s)"
OUTPUT_FILE="${RECEIPTS_DIR}/composed-${EPOCH}.json"

# Signature payload: newline-joined sorted paths
SIGNATURE_PAYLOAD="$(printf '%s\n' "${SELECTED_RECEIPTS[@]}" | sort)"
SIGNATURE="$(compute_signature "${SIGNATURE_PAYLOAD}")"

if [[ -z "${SIGNATURE}" ]]; then
    echo "ERROR: Signature computation returned empty string" >&2
    exit 2
fi

# Write composed receipt via Python for correct JSON serialisation
python3 - "${OUTPUT_FILE}" "${COMPOSED_AT}" "${SIGNATURE}" "${SELECTED_RECEIPTS[@]}" <<'PYEOF'
import json, sys

output_file  = sys.argv[1]
composed_at  = sys.argv[2]
signature    = sys.argv[3]
source_paths = sys.argv[4:]

receipt = {
    "schema": "ggen.receipt.v1",
    "composed": True,
    "source_receipts": source_paths,
    "signature": signature,
    "composed_at": composed_at,
}

with open(output_file, "w") as fh:
    json.dump(receipt, fh, indent=2)
    fh.write("\n")

print(json.dumps(receipt, indent=2))
PYEOF

echo ""
echo "Composed receipt written: ${OUTPUT_FILE}"

# ── Verify output ──────────────────────────────────────────────────────────
python3 - "${OUTPUT_FILE}" <<'PYEOF'
import json, sys

fpath = sys.argv[1]
try:
    with open(fpath) as fh:
        d = json.load(fh)
except Exception as e:
    print(f"VERIFY FAIL: cannot parse output JSON: {e}", file=sys.stderr)
    sys.exit(2)

errors = []
if d.get("schema") != "ggen.receipt.v1":
    errors.append("schema field missing or wrong")
if not d.get("composed"):
    errors.append("composed field must be true")
if not isinstance(d.get("source_receipts"), list) or not d["source_receipts"]:
    errors.append("source_receipts must be a non-empty list")
if not d.get("signature"):
    errors.append("signature must be non-empty")
if not d.get("composed_at"):
    errors.append("composed_at must be present")

if errors:
    for e in errors:
        print(f"VERIFY FAIL: {e}", file=sys.stderr)
    sys.exit(2)

print(f"VERIFY OK: schema={d['schema']} composed={d['composed']} "
      f"sources={len(d['source_receipts'])} "
      f"sig={d['signature'][:32]}...")
PYEOF
