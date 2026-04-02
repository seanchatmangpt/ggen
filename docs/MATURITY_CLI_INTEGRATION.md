<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace Maturity Matrix - CLI Integration Guide](#marketplace-maturity-matrix---cli-integration-guide)
  - [Overview](#overview)
  - [New CLI Commands](#new-cli-commands)
    - [`ggen marketplace maturity`](#ggen-marketplace-maturity)
    - [`ggen marketplace dashboard`](#ggen-marketplace-dashboard)
    - [`ggen marketplace list` (Enhanced)](#ggen-marketplace-list-enhanced)
    - [`ggen marketplace validate` (Enhanced)](#ggen-marketplace-validate-enhanced)
    - [`ggen marketplace maturity-batch`](#ggen-marketplace-maturity-batch)
  - [Integration Patterns](#integration-patterns)
    - [Pattern 1: CI/CD Gate for Package Publishing](#pattern-1-cicd-gate-for-package-publishing)
    - [Pattern 2: Monthly Maturity Report](#pattern-2-monthly-maturity-report)
    - [Pattern 3: Package Improvement Tracking](#pattern-3-package-improvement-tracking)
    - [Pattern 4: Marketplace Quality Dashboard](#pattern-4-marketplace-quality-dashboard)
    - [Pattern 5: Maturity-Based Recommendations](#pattern-5-maturity-based-recommendations)
  - [Integration with DevOps](#integration-with-devops)
    - [Pre-Release Checklist](#pre-release-checklist)
  - [Integration with RevOps](#integration-with-revops)
    - [Quarterly Business Review (QBR)](#quarterly-business-review-qbr)
  - [Integration with GTM](#integration-with-gtm)
    - [Feature Production Packages](#feature-production-packages)
  - [Dashboard Customization](#dashboard-customization)
    - [HTML Dashboard Template](#html-dashboard-template)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace Maturity Matrix - CLI Integration Guide

## Overview

This guide shows how to integrate the marketplace maturity matrix into the ggen CLI and marketplace operations.

---

## New CLI Commands

### `ggen marketplace maturity`

Assess the maturity of a single package.

```bash
# Basic assessment
ggen marketplace maturity io.ggen.rust.microservice

# With detailed feedback
ggen marketplace maturity io.ggen.rust.microservice --detailed

# Export as JSON
ggen marketplace maturity io.ggen.rust.microservice --format json

# Verify meets production standard
ggen marketplace maturity io.ggen.rust.microservice --verify production
```

**Output**:
```
Package: Rust Microservice Template
ID: io.ggen.rust.microservice
Assessment Date: 2025-11-15T10:30:00Z

Maturity Level: Production (72/100)
Description: Stable and reliable, suitable for production use

Dimension Scores:
  Documentation:  18/20 (90%)  ████████░
  Testing:        16/20 (80%)  ████████░
  Security:       19/20 (95%)  █████████
  Performance:    12/15 (80%)  ████████░
  Adoption:        9/15 (60%)  ██████░░░
  Maintenance:     8/10 (80%)  ████████░

Next Steps:
  1. Increase adoption through documentation and examples
  2. Establish quarterly release schedule
  3. Monitor and respond to community issues
```

---

### `ggen marketplace dashboard`

Generate marketplace-wide maturity dashboard.

```bash
# Generate dashboard
ggen marketplace dashboard

# Export as JSON
ggen marketplace dashboard --format json --output report.json

# Generate HTML report
ggen marketplace dashboard --format html --output index.html

# Show only production packages
ggen marketplace dashboard --min-maturity production
```

**Output**:
```
Marketplace Maturity Dashboard
Generated: 2025-11-15T10:35:00Z

Overall Statistics:
  Total Packages: 47
  Average Score: 68.3
  Packages by Level:
    • Experimental:  8 (17%)
    • Beta:         12 (26%)
    • Production:   22 (47%)
    • Enterprise:    5 (10%)

Dimension Averages:
  Documentation:  17.2/20 (86%)
  Testing:        15.8/20 (79%)
  Security:       17.9/20 (90%)
  Performance:    11.3/15 (75%)
  Adoption:       10.2/15 (68%)
  Maintenance:     8.1/10 (81%)

Top 5 Packages by Maturity:
  1. io.ggen.advanced.rust.api (92)
  2. io.ggen.rust.microservice (72)
  3. io.ggen.typescript.sdk (70)
  4. io.ggen.python.pydantic (68)
  5. io.ggen.healthcare.fhir (65)

Packages Needing Improvement (< 60):
  1. io.experimental.prototype (28)
  2. io.research.bleeding-edge (45)
  3. io.beta.feature-preview (52)
```

---

### `ggen marketplace list` (Enhanced)

Filter packages by maturity level.

```bash
# List all production-ready packages
ggen marketplace list --min-maturity production

# List packages by maturity level
ggen marketplace list --maturity-level beta

# Exclude experimental packages
ggen marketplace list --max-maturity beta

# Sort by maturity score
ggen marketplace list --sort maturity --descending
```

**Output**:
```
Marketplace Packages (Production-Ready)

io.ggen.rust.microservice
  Version: 1.2.0
  Maturity: Production (72/100)
  Downloads: 523
  Rating: 4.3/5.0
  Updated: 2 weeks ago

io.ggen.typescript.sdk
  Version: 2.0.1
  Maturity: Production (70/100)
  Downloads: 412
  Rating: 4.1/5.0
  Updated: 3 weeks ago

io.ggen.python.pydantic
  Version: 1.5.0
  Maturity: Production (68/100)
  Downloads: 389
  Rating: 4.0/5.0
  Updated: 1 month ago
```

---

### `ggen marketplace validate` (Enhanced)

Validate package with maturity scoring.

```bash
# Validate package with maturity check
ggen marketplace validate --package io.new.package

# Require production-level maturity
ggen marketplace validate --package io.new.package --require-level production

# Generate maturity improvement plan
ggen marketplace validate --package io.new.package --improvement-plan
```

**Output**:
```
Validating: io.new.package

Package Validation Results:
  ✓ Structure valid
  ✓ Manifest valid
  ✓ Templates valid
  ✓ RDF ontology valid

Maturity Assessment:
  Current: Beta (55/100)

  To reach Production (61+):
    • Testing: Need 18/20 (currently 14/20)
      - Add integration tests (need 2 more points)
      - Increase coverage to 80%+ (currently 72%)

    • Documentation: Need 1 more point
      - Add more API examples

Improvement Timeline:
  Priority 1: Increase test coverage (1-2 weeks)
  Priority 2: Add integration tests (1 week)
  Priority 3: Expand documentation (3-5 days)

Estimated time to Production: 2-3 weeks
```

---

### `ggen marketplace maturity-batch`

Assess multiple packages in batch.

```bash
# Assess all packages
ggen marketplace maturity-batch --packages-dir marketplace/packages

# Assess and save report
ggen marketplace maturity-batch --packages-dir marketplace/packages \
  --output maturity-report.json

# Generate CSV for spreadsheets
ggen marketplace maturity-batch --packages-dir marketplace/packages \
  --format csv --output maturity-report.csv
```

**Output** (JSON):
```json
{
  "generated_at": "2025-11-15T10:40:00Z",
  "total_packages": 47,
  "average_score": 68.3,
  "assessments": [
    {
      "package_id": "io.ggen.rust.microservice",
      "package_name": "Rust Microservice Template",
      "total_score": 72,
      "maturity_level": "production",
      "scores": {
        "documentation": 18,
        "testing": 16,
        "security": 19,
        "performance": 12,
        "adoption": 9,
        "maintenance": 8
      }
    },
    ...
  ],
  "statistics": {
    "experimental": 8,
    "beta": 12,
    "production": 22,
    "enterprise": 5
  }
}
```

---

## Integration Patterns

### Pattern 1: CI/CD Gate for Package Publishing

```bash
#!/bin/bash
# scripts/ci/validate-maturity.sh

PACKAGE_ID="$1"
MIN_MATURITY="${2:-production}"

echo "Validating maturity for: $PACKAGE_ID"

# Check maturity level
RESULT=$(ggen marketplace maturity "$PACKAGE_ID" \
  --verify "$MIN_MATURITY" \
  --format json)

if [ $? -ne 0 ]; then
  echo "❌ Package does not meet $MIN_MATURITY maturity requirements"
  echo "Run: ggen marketplace maturity $PACKAGE_ID --detailed"
  exit 1
fi

echo "✅ Package meets $MIN_MATURITY maturity requirements"
exit 0
```

**Usage in GitHub Actions**:
```yaml
- name: Validate maturity
  run: |
    bash scripts/ci/validate-maturity.sh io.ggen.rust.microservice production
```

---

### Pattern 2: Monthly Maturity Report

```bash
#!/bin/bash
# scripts/ops/generate-maturity-report.sh

REPORT_DIR="reports/maturity"
TIMESTAMP=$(date +%Y-%m-%d)

mkdir -p "$REPORT_DIR"

echo "Generating maturity dashboard..."
ggen marketplace dashboard \
  --format json \
  --output "$REPORT_DIR/dashboard-$TIMESTAMP.json"

echo "Generating package batch report..."
ggen marketplace maturity-batch \
  --packages-dir marketplace/packages \
  --format csv \
  --output "$REPORT_DIR/packages-$TIMESTAMP.csv"

echo "Reports saved to: $REPORT_DIR"
```

**Automated via cron**:
```bash
# Run monthly on the 1st at 9 AM
0 9 1 * * bash /path/to/generate-maturity-report.sh
```

---

### Pattern 3: Package Improvement Tracking

```bash
#!/bin/bash
# scripts/ops/track-package-improvement.sh

PACKAGE_ID="$1"
HISTORY_FILE="reports/maturity/$PACKAGE_ID.json"

# Get current assessment
CURRENT=$(ggen marketplace maturity "$PACKAGE_ID" --format json)

# Append to history
echo "$CURRENT" >> "$HISTORY_FILE"

# Calculate trend
if [ -f "$HISTORY_FILE" ]; then
  jq -s '
    . as $history |
    {
      package_id: $history[0].package_id,
      current_score: $history[-1].total_score,
      previous_score: $history[-2].total_score,
      score_change: ($history[-1].total_score - $history[-2].total_score),
      trend: (if ($history[-1].total_score > $history[-2].total_score) then "improving" else "declining" end)
    }
  ' "$HISTORY_FILE"
fi
```

---

### Pattern 4: Marketplace Quality Dashboard

```bash
#!/bin/bash
# scripts/ops/publish-dashboard.sh

echo "Generating marketplace quality dashboard..."

ggen marketplace dashboard \
  --format html \
  --output gh-pages/marketplace/maturity-dashboard.html

# Commit and push to gh-pages
cd gh-pages
git add marketplace/maturity-dashboard.html
git commit -m "chore: update marketplace maturity dashboard"
git push origin gh-pages
```

---

### Pattern 5: Maturity-Based Recommendations

```bash
#!/bin/bash
# scripts/marketplace/recommend-packages.sh

CATEGORY="${1:-rust}"
MIN_MATURITY="${2:-production}"

echo "Recommended packages in category: $CATEGORY (maturity: $MIN_MATURITY+)"

ggen marketplace list \
  --category "$CATEGORY" \
  --min-maturity "$MIN_MATURITY" \
  --sort maturity \
  --descending \
  --format json | \
  jq '.[] | {
    name: .name,
    maturity_level: .maturity_level,
    score: .maturity_score,
    downloads: .downloads,
    rating: .rating
  }'
```

---

## Integration with DevOps

### Pre-Release Checklist

```bash
#!/bin/bash
# scripts/devops/pre-release-checks.sh

PACKAGE_ID="$1"
VERSION="$2"

echo "Pre-release validation for: $PACKAGE_ID@$VERSION"

# Check maturity
echo "1. Checking maturity level..."
ggen marketplace maturity "$PACKAGE_ID" --verify production || exit 1

# Check tests pass
echo "2. Running tests..."
cargo test --release || exit 1

# Check security
echo "3. Running security audit..."
cargo audit || exit 1

# Check benchmarks
echo "4. Running benchmarks..."
cargo bench --release || exit 1

# Generate documentation
echo "5. Generating documentation..."
cargo doc --release --no-deps || exit 1

echo "✅ All pre-release checks passed!"
```

---

## Integration with RevOps

### Quarterly Business Review (QBR)

```bash
#!/bin/bash
# scripts/revops/generate-qbr-maturity.sh

QUARTER="$1"  # e.g., "Q4-2025"

echo "Generating maturity QBR for: $QUARTER"

ggen marketplace dashboard \
  --format json \
  --output "reports/qbr/$QUARTER/maturity.json"

# Extract key metrics
jq '{
  quarter: "'$QUARTER'",
  total_packages: .statistics.total_packages,
  average_score: .statistics.average_score,
  level_distribution: .statistics.level_distribution,
  top_packages: .assessments[0:5] | map({id: .package_id, score: .total_score}),
  needs_improvement: .assessments | map(select(.total_score < 60)) | map({id: .package_id, score: .total_score})
}' "reports/qbr/$QUARTER/maturity.json"
```

---

## Integration with GTM

### Feature Production Packages

```bash
#!/bin/bash
# scripts/gtm/promote-production-packages.sh

echo "Finding production-ready packages for promotion..."

PACKAGES=$(ggen marketplace list \
  --min-maturity production \
  --sort downloads \
  --descending \
  --limit 10 \
  --format json)

# Generate promotion materials
echo "$PACKAGES" | jq '.[] | {
  id: .id,
  name: .name,
  description: .description,
  maturity_score: .maturity_score,
  downloads: .downloads,
  promotion_message: "Production-ready: \(.name) (\(.maturity_score)/100 maturity score, \(.downloads) downloads)"
}' > promotion-candidates.json

echo "Generated promotion content for:"
jq -r '.promotion_message' promotion-candidates.json
```

---

## Dashboard Customization

### HTML Dashboard Template

```html
<!DOCTYPE html>
<html>
<head>
  <title>ggen Marketplace Maturity Dashboard</title>
  <style>
    .maturity-score { font-size: 32px; font-weight: bold; }
    .experimental { color: #d32f2f; }
    .beta { color: #f57c00; }
    .production { color: #388e3c; }
    .enterprise { color: #1976d2; }
    .dimension-bar {
      width: 200px;
      height: 20px;
      background: #e0e0e0;
      border-radius: 4px;
      overflow: hidden;
    }
    .dimension-fill {
      height: 100%;
      background: linear-gradient(90deg, #388e3c, #1976d2);
    }
  </style>
</head>
<body>
  <h1>ggen Marketplace Maturity Dashboard</h1>
  <div id="dashboard"></div>

  <script>
    // Load JSON report and render dashboard
    fetch('dashboard.json')
      .then(r => r.json())
      .then(data => {
        const html = `
          <div class="statistics">
            <h2>Marketplace Statistics</h2>
            <p>Total Packages: ${data.statistics.total_packages}</p>
            <p>Average Score: ${data.statistics.average_score.toFixed(1)}/100</p>
          </div>
          <div class="packages">
            <h2>Top Packages by Maturity</h2>
            ${data.assessments.map(p => `
              <div class="package">
                <h3>${p.package_name}</h3>
                <div class="maturity-score ${p.maturity_level}">
                  ${p.total_score}/100
                </div>
              </div>
            `).join('')}
          </div>
        `;
        document.getElementById('dashboard').innerHTML = html;
      });
  </script>
</body>
</html>
```

---

## Best Practices

1. **Automate Assessment**: Run maturity assessment in CI/CD on every package change
2. **Track Trends**: Store historical assessments to track improvement over time
3. **Gate Releases**: Require production-level maturity before publishing
4. **Feature Winners**: Highlight and promote production/enterprise packages
5. **Support Growth**: Help beta packages improve through detailed feedback
6. **Celebrate Milestones**: Acknowledge packages reaching production/enterprise

---

## Troubleshooting

**Q: Command not found: `ggen marketplace maturity`**
A: Update ggen to v2.7.0+: `brew upgrade ggen` or `cargo install ggen --force`

**Q: Package stuck in Beta score**
A: Run `ggen marketplace maturity <id> --detailed` to see specific feedback

**Q: How do I automate assessment?**
A: Add `ggen marketplace maturity-batch` to CI/CD workflows

**Q: Can I see historical trends?**
A: Use batch command with timestamps and store results in JSON files

---

See Also:
- [Marketplace Maturity Matrix Guide](MARKETPLACE_MATURITY_MATRIX.md)
- [Marketplace Operations Workflows](OPERATIONS_WORKFLOWS_GUIDE.md)
