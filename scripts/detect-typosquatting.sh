#!/usr/bin/env bash
# detect-typosquatting.sh - Detect potential typosquatting attacks in dependencies
#
# Week 8: Supply chain security - Typosquatting detection
# Checks for suspicious dependency names that might be typosquatting popular crates

set -euo pipefail

# Known popular crates to check against
POPULAR_CRATES=(
    "serde"
    "tokio"
    "clap"
    "anyhow"
    "thiserror"
    "reqwest"
    "async-trait"
    "tracing"
    "regex"
    "chrono"
    "uuid"
    "rand"
    "futures"
    "axum"
    "actix-web"
    "diesel"
    "sqlx"
    "hyper"
    "tonic"
    "prost"
)

# Suspicious patterns (common typosquatting techniques)
SUSPICIOUS_PATTERNS=(
    "_rs$"      # Adding _rs suffix
    "^rust_"    # Adding rust_ prefix
    "^r_"       # Adding r_ prefix
    "-rs$"      # Adding -rs suffix
    "^lib"      # Adding lib prefix
)

# Output file for results
OUTPUT_FILE="${1:-typosquat-report.txt}"
SUSPICIOUS_FOUND=0

echo "🔍 Typosquatting Detection Report" > "$OUTPUT_FILE"
echo "Generated: $(date -u '+%Y-%m-%d %H:%M:%S UTC')" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Extract all dependencies from Cargo.lock
if [ ! -f "Cargo.lock" ]; then
    echo "::error::Cargo.lock not found"
    exit 1
fi

# Parse dependencies from Cargo.lock
DEPENDENCIES=$(cargo metadata --format-version 1 --locked | jq -r '.packages[] | .name' | sort -u)

echo "Analyzing $(echo "$DEPENDENCIES" | wc -l) dependencies..." >&2
echo "" >> "$OUTPUT_FILE"

# Function to calculate Levenshtein distance (approximate)
levenshtein_distance() {
    local s1="$1"
    local s2="$2"
    local len1=${#s1}
    local len2=${#s2}

    # Simple heuristic: if length difference > 3, not similar
    local len_diff=$((len1 - len2))
    if [ "${len_diff#-}" -gt 3 ]; then
        echo "999"
        return
    fi

    # Count character differences (simplified)
    local diff=0
    local min_len=$((len1 < len2 ? len1 : len2))

    for ((i=0; i<min_len; i++)); do
        if [ "${s1:i:1}" != "${s2:i:1}" ]; then
            diff=$((diff + 1))
        fi
    done

    diff=$((diff + len_diff))
    echo "${diff#-}"
}

# Check each dependency
while IFS= read -r dep; do
    SUSPICIOUS_REASON=""

    # Skip if dependency is in the popular list (exact match)
    IS_POPULAR=0
    for popular in "${POPULAR_CRATES[@]}"; do
        if [ "$dep" == "$popular" ]; then
            IS_POPULAR=1
            break
        fi
    done

    if [ $IS_POPULAR -eq 1 ]; then
        continue
    fi

    # Check for suspicious patterns
    for pattern in "${SUSPICIOUS_PATTERNS[@]}"; do
        if echo "$dep" | grep -qE "$pattern"; then
            SUSPICIOUS_REASON="Matches suspicious pattern: $pattern"
            break
        fi
    done

    # Check for similarity to popular crates (typosquatting detection)
    if [ -z "$SUSPICIOUS_REASON" ]; then
        for popular in "${POPULAR_CRATES[@]}"; do
            # Calculate similarity (simplified Levenshtein distance)
            distance=$(levenshtein_distance "$dep" "$popular")

            # If very similar (distance <= 2), flag as suspicious
            if [ "$distance" -le 2 ] && [ "$dep" != "$popular" ]; then
                SUSPICIOUS_REASON="Similar to popular crate '$popular' (distance: $distance)"
                break
            fi
        done
    fi

    # Report suspicious dependencies
    if [ -n "$SUSPICIOUS_REASON" ]; then
        echo "⚠️  SUSPICIOUS: $dep" >> "$OUTPUT_FILE"
        echo "    Reason: $SUSPICIOUS_REASON" >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
        SUSPICIOUS_FOUND=$((SUSPICIOUS_FOUND + 1))
    fi
done <<< "$DEPENDENCIES"

# Summary
echo "---" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

if [ $SUSPICIOUS_FOUND -eq 0 ]; then
    echo "✅ No suspicious dependencies detected" >> "$OUTPUT_FILE"
    echo "✅ No suspicious dependencies detected" >&2
    exit 0
else
    echo "⚠️  Found $SUSPICIOUS_FOUND potentially suspicious dependencies" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    echo "**Recommendation**: Manually review these dependencies for legitimacy" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    echo "⚠️  Found $SUSPICIOUS_FOUND potentially suspicious dependencies" >&2
    cat "$OUTPUT_FILE" >&2
    exit 1
fi
