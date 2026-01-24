#!/usr/bin/env bash
# generate-sbom.sh - Generate Software Bill of Materials (SBOM) using CycloneDX
#
# Week 8: Supply chain security - SBOM generation
# Creates standardized SBOM for dependency transparency and compliance

set -euo pipefail

# Configuration
OUTPUT_DIR="${1:-.sbom}"
FORMAT="${2:-json}"  # json, xml, or both

echo "📦 SBOM Generation (CycloneDX)"
echo "=============================="
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Check if cargo-cyclonedx is installed
if ! command -v cargo-cyclonedx &> /dev/null; then
    echo "Installing cargo-cyclonedx..."
    cargo install cargo-cyclonedx
fi

# Generate SBOM in requested formats
echo "Generating SBOM..."

case "$FORMAT" in
    json)
        cargo cyclonedx --format json --output-cdx "$OUTPUT_DIR/sbom.cdx.json"
        echo "✅ Generated: $OUTPUT_DIR/sbom.cdx.json"
        ;;
    xml)
        cargo cyclonedx --format xml --output-cdx "$OUTPUT_DIR/sbom.cdx.xml"
        echo "✅ Generated: $OUTPUT_DIR/sbom.cdx.xml"
        ;;
    both)
        cargo cyclonedx --format json --output-cdx "$OUTPUT_DIR/sbom.cdx.json"
        cargo cyclonedx --format xml --output-cdx "$OUTPUT_DIR/sbom.cdx.xml"
        echo "✅ Generated: $OUTPUT_DIR/sbom.cdx.json"
        echo "✅ Generated: $OUTPUT_DIR/sbom.cdx.xml"
        ;;
    *)
        echo "::error::Invalid format: $FORMAT (must be json, xml, or both)"
        exit 1
        ;;
esac

# Validate SBOM
echo ""
echo "Validating SBOM..."

SBOM_JSON="$OUTPUT_DIR/sbom.cdx.json"
if [ -f "$SBOM_JSON" ]; then
    # Check BOM format
    BOM_FORMAT=$(jq -r '.bomFormat' "$SBOM_JSON")
    if [ "$BOM_FORMAT" != "CycloneDX" ]; then
        echo "::error::Invalid SBOM format: $BOM_FORMAT"
        exit 1
    fi

    # Check spec version
    SPEC_VERSION=$(jq -r '.specVersion' "$SBOM_JSON")
    echo "✅ SBOM Format: $BOM_FORMAT (Spec $SPEC_VERSION)"

    # Count components
    COMPONENT_COUNT=$(jq '.components | length' "$SBOM_JSON")
    echo "✅ Components: $COMPONENT_COUNT"

    # Generate component summary
    echo ""
    echo "📋 Component Summary"
    echo "-------------------"
    jq -r '.components[] | "\(.name)@\(.version)"' "$SBOM_JSON" | head -20

    if [ "$COMPONENT_COUNT" -gt 20 ]; then
        echo "... and $((COMPONENT_COUNT - 20)) more"
    fi
fi

# Generate human-readable summary
echo ""
echo "Generating human-readable summary..."

SUMMARY_FILE="$OUTPUT_DIR/sbom-summary.md"
cat > "$SUMMARY_FILE" << EOF
# Software Bill of Materials (SBOM)

**Generated**: $(date -u '+%Y-%m-%d %H:%M:%S UTC')
**Format**: CycloneDX $(jq -r '.specVersion' "$SBOM_JSON" 2>/dev/null || echo "unknown")
**Components**: $COMPONENT_COUNT

## Dependencies

EOF

jq -r '.components[] | "- **\(.name)** v\(.version) (\(.purl // "no-purl"))"' "$SBOM_JSON" >> "$SUMMARY_FILE"

echo "✅ Generated: $SUMMARY_FILE"

# Generate license summary
echo ""
echo "Generating license summary..."

LICENSE_FILE="$OUTPUT_DIR/licenses.md"
cat > "$LICENSE_FILE" << EOF
# License Summary

**Generated**: $(date -u '+%Y-%m-%d %H:%M:%S UTC')

## License Distribution

EOF

# Count licenses
jq -r '.components[] | .licenses[]?.license.id // .licenses[]?.license.name // "Unknown"' "$SBOM_JSON" | \
    sort | uniq -c | sort -rn >> "$LICENSE_FILE"

echo "✅ Generated: $LICENSE_FILE"

# Check for incompatible licenses
echo ""
echo "Checking for incompatible licenses..."

INCOMPATIBLE_LICENSES=("GPL-3.0" "AGPL-3.0" "SSPL-1.0")
FOUND_INCOMPATIBLE=0

for license in "${INCOMPATIBLE_LICENSES[@]}"; do
    if jq -e ".components[] | select(.licenses[]?.license.id == \"$license\")" "$SBOM_JSON" > /dev/null 2>&1; then
        echo "::warning::Found incompatible license: $license"
        FOUND_INCOMPATIBLE=1
    fi
done

if [ $FOUND_INCOMPATIBLE -eq 0 ]; then
    echo "✅ No incompatible licenses found"
fi

# Generate security summary
echo ""
echo "Generating security summary..."

SECURITY_FILE="$OUTPUT_DIR/security-summary.md"
cat > "$SECURITY_FILE" << EOF
# Security Summary

**Generated**: $(date -u '+%Y-%m-%d %H:%M:%S UTC')

## Dependency Security

EOF

# Run cargo audit and include results
if command -v cargo-audit &> /dev/null; then
    echo "Running cargo audit..." >&2
    if cargo audit --json > "$OUTPUT_DIR/audit.json" 2>/dev/null; then
        echo "✅ No known vulnerabilities" >> "$SECURITY_FILE"
    else
        VULN_COUNT=$(jq '.vulnerabilities.list | length' "$OUTPUT_DIR/audit.json" 2>/dev/null || echo "0")
        echo "⚠️  Found $VULN_COUNT known vulnerabilities" >> "$SECURITY_FILE"
        echo "" >> "$SECURITY_FILE"
        jq -r '.vulnerabilities.list[] | "- **\(.package.name)** v\(.package.version): \(.advisory.title) (Severity: \(.advisory.severity))"' \
            "$OUTPUT_DIR/audit.json" >> "$SECURITY_FILE"
    fi
fi

echo "✅ Generated: $SECURITY_FILE"

# Generate metadata file
echo ""
echo "Generating metadata..."

METADATA_FILE="$OUTPUT_DIR/metadata.json"
cat > "$METADATA_FILE" << EOF
{
  "generated_at": "$(date -u '+%Y-%m-%dT%H:%M:%SZ')",
  "format": "CycloneDX",
  "spec_version": "$(jq -r '.specVersion' "$SBOM_JSON" 2>/dev/null || echo "unknown")",
  "component_count": $COMPONENT_COUNT,
  "cargo_lock_hash": "$(sha256sum Cargo.lock | awk '{print $1}')",
  "files": {
    "sbom_json": "$(basename "$SBOM_JSON")",
    "summary": "$(basename "$SUMMARY_FILE")",
    "licenses": "$(basename "$LICENSE_FILE")",
    "security": "$(basename "$SECURITY_FILE")"
  }
}
EOF

echo "✅ Generated: $METADATA_FILE"

echo ""
echo "✅ SBOM generation complete"
echo ""
echo "Output directory: $OUTPUT_DIR"
echo "Files generated:"
ls -lh "$OUTPUT_DIR"
