#!/bin/bash
# Security Remediation Script for ggen v2.5.0
# Addresses critical vulnerabilities from security audit
# Run from repository root: ./scripts/security-remediation.sh

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== ggen v2.5.0 Security Remediation ===${NC}"
echo "This script fixes critical security issues identified in the audit."
echo ""

# Check we're in the right directory
if [ ! -f "Cargo.toml" ] || [ ! -d "crates" ]; then
    echo -e "${RED}Error: Must run from repository root${NC}"
    exit 1
fi

# Phase 1: Critical Dependency Updates
echo -e "${YELLOW}[1/6] Updating vulnerable dependencies...${NC}"
echo "  - ring 0.16.20 → ≥0.17.12 (RUSTSEC-2025-0009)"
echo "  - wasmtime 28.0.1 → ≥34.0.2 (RUSTSEC-2025-0046)"

# Update specific vulnerable packages
cargo update ring wasmtime

# Verify updates fixed vulnerabilities
echo "  Verifying with cargo audit..."
if ! command -v cargo-audit &> /dev/null; then
    echo "  Installing cargo-audit..."
    cargo install cargo-audit
fi

if cargo audit --deny warnings 2>&1 | grep -q "RUSTSEC-2025-0009\|RUSTSEC-2025-0046"; then
    echo -e "${RED}  Failed: Vulnerabilities still present after update${NC}"
    exit 1
fi
echo -e "${GREEN}  ✓ Vulnerabilities patched${NC}"

# Phase 2: License Headers
echo -e "\n${YELLOW}[2/6] Adding MIT license headers to source files...${NC}"

LICENSE_HEADER="// Copyright (c) $(date +%Y) Sean Chatman
// SPDX-License-Identifier: MIT
"

# Count files needing headers
total_files=$(find crates -name "*.rs" | wc -l | tr -d ' ')
files_with_headers=$(find crates -name "*.rs" -exec grep -l "SPDX-License-Identifier" {} \; | wc -l | tr -d ' ')

echo "  Found $total_files Rust files, $files_with_headers already have headers"

if [ "$files_with_headers" -lt "$total_files" ]; then
    echo "  Adding headers to $((total_files - files_with_headers)) files..."

    # Add headers to files that don't have them
    find crates -name "*.rs" | while read -r file; do
        if ! grep -q "SPDX-License-Identifier" "$file"; then
            # Read existing content
            content=$(cat "$file")
            # Write header + content
            echo "$LICENSE_HEADER" > "$file"
            echo "$content" >> "$file"
        fi
    done
    echo -e "${GREEN}  ✓ License headers added${NC}"
else
    echo -e "${GREEN}  ✓ All files already have license headers${NC}"
fi

# Phase 3: Third-Party Licenses
echo -e "\n${YELLOW}[3/6] Generating third-party license attribution...${NC}"

if ! command -v cargo-license &> /dev/null; then
    echo "  Installing cargo-license..."
    cargo install cargo-license
fi

echo "  Generating THIRD_PARTY_LICENSES.txt..."
cargo license --authors --color=never > THIRD_PARTY_LICENSES.txt
cargo license --json > third-party-licenses.json

echo -e "${GREEN}  ✓ Attribution files created${NC}"

# Phase 4: SBOM Generation
echo -e "\n${YELLOW}[4/6] Generating Software Bill of Materials (SBOM)...${NC}"

if ! command -v cargo-sbom &> /dev/null; then
    echo "  Installing cargo-sbom..."
    cargo install cargo-sbom
fi

echo "  Generating SPDX SBOM..."
cargo sbom --output-format spdx > ggen-sbom.spdx 2>/dev/null || {
    echo "  cargo-sbom not available, using alternative..."
    cargo metadata --format-version=1 | jq -r '.packages[] | "\(.name) \(.version) \(.license // "UNKNOWN")"' > ggen-dependencies.txt
}

echo -e "${GREEN}  ✓ SBOM generated${NC}"

# Phase 5: Security Policy
echo -e "\n${YELLOW}[5/6] Creating SECURITY.md vulnerability disclosure policy...${NC}"

if [ ! -f "SECURITY.md" ]; then
    cat > SECURITY.md << 'EOF'
# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 2.5.x   | :white_check_mark: |
| 2.4.x   | :white_check_mark: |
| < 2.4   | :x:                |

## Reporting a Vulnerability

We take security seriously. If you discover a security vulnerability in ggen, please report it responsibly:

### Reporting Process

1. **DO NOT** open a public GitHub issue for security vulnerabilities
2. Email security findings to: **security@ggen.dev** (or sean@chatmangpt.com until dedicated address is set up)
3. Include in your report:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if available)

### Response Timeline

- **Initial Response:** Within 24 hours for critical issues, 7 days for others
- **Status Updates:** Every 7 days until resolved
- **Disclosure:** Coordinated disclosure after 90 days or when patch is available

### Vulnerability Severity

We use the following severity levels:

- **Critical:** Remote code execution, privilege escalation
- **High:** Authentication bypass, data exposure
- **Medium:** Denial of service, information disclosure
- **Low:** Minor security issues with limited impact

### Recognition

We will credit researchers who report valid vulnerabilities in:
- Release notes / CHANGELOG
- Security advisories
- Hall of Fame (coming soon)

### Security Best Practices

Users should:
- Keep ggen updated to the latest version
- Review `cargo audit` output regularly
- Use environment variables for API keys (never hardcode)
- Validate template sources before execution
- Enable telemetry for security monitoring (opt-in)

## Known Issues

See [docs/SECURITY_COMPLIANCE_AUDIT_V2.5.0.md](docs/SECURITY_COMPLIANCE_AUDIT_V2.5.0.md) for the latest security audit.

## Security Updates

Security updates are released as patch versions (e.g., 2.5.1) and announced via:
- GitHub Security Advisories
- Release notes
- Email notifications (for critical issues)

---

Last Updated: $(date +%Y-%m-%d)
EOF
    echo -e "${GREEN}  ✓ SECURITY.md created${NC}"
else
    echo -e "${GREEN}  ✓ SECURITY.md already exists${NC}"
fi

# Phase 6: CI Security Checks
echo -e "\n${YELLOW}[6/6] Adding CI security checks...${NC}"

mkdir -p .github/workflows

if [ ! -f ".github/workflows/security.yml" ]; then
    cat > .github/workflows/security.yml << 'EOF'
name: Security Audit

on:
  push:
    branches: [master, main]
  pull_request:
    branches: [master, main]
  schedule:
    # Run daily at 6 AM UTC
    - cron: '0 6 * * *'

env:
  CARGO_TERM_COLOR: always

jobs:
  audit:
    name: Security Audit
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Install cargo-audit
        run: cargo install cargo-audit

      - name: Run cargo audit
        run: cargo audit --deny warnings

      - name: Check for yanked dependencies
        run: cargo audit --deny yanked

  clippy-security:
    name: Clippy Security Lints
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable
        with:
          components: clippy

      - name: Run Clippy with security lints
        run: |
          cargo clippy --all-targets --all-features -- \
            -D clippy::unwrap_used \
            -D clippy::expect_used \
            -D clippy::panic \
            -D clippy::todo \
            -D clippy::unimplemented

  sbom:
    name: Generate SBOM
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Generate dependency list
        run: |
          cargo metadata --format-version=1 | \
          jq -r '.packages[] | "\(.name),\(.version),\(.license // "UNKNOWN")"' > sbom.csv

      - name: Upload SBOM artifact
        uses: actions/upload-artifact@v3
        with:
          name: sbom
          path: sbom.csv
          retention-days: 90
EOF
    echo -e "${GREEN}  ✓ Security CI workflow created${NC}"
else
    echo -e "${GREEN}  ✓ Security CI workflow already exists${NC}"
fi

# Final verification
echo -e "\n${GREEN}=== Remediation Complete ===${NC}"
echo ""
echo "Summary of changes:"
echo "  ✓ Updated vulnerable dependencies (ring, wasmtime)"
echo "  ✓ Added license headers to source files"
echo "  ✓ Generated third-party attribution (THIRD_PARTY_LICENSES.txt)"
echo "  ✓ Created SBOM (ggen-sbom.spdx or ggen-dependencies.txt)"
echo "  ✓ Created SECURITY.md vulnerability disclosure policy"
echo "  ✓ Added GitHub Actions security workflow"
echo ""
echo "Next steps:"
echo "  1. Review generated files (SECURITY.md, THIRD_PARTY_LICENSES.txt)"
echo "  2. Commit changes: git add -A && git commit -m 'security: Apply critical remediation from audit'"
echo "  3. Run tests: cargo test --all-features"
echo "  4. Push to trigger CI: git push"
echo ""
echo "For remaining issues, see: docs/SECURITY_COMPLIANCE_AUDIT_V2.5.0.md"
