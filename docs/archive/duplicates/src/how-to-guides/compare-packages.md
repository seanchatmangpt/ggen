<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Compare Packages](#how-to-compare-packages)
  - [Basic Comparison](#basic-comparison)
    - [Compare two packages](#compare-two-packages)
  - [Understanding the Comparison](#understanding-the-comparison)
    - [Score comparison](#score-comparison)
    - [Dimension-by-dimension analysis](#dimension-by-dimension-analysis)
  - [Detailed Comparison](#detailed-comparison)
    - [See more information](#see-more-information)
  - [Export Comparison](#export-comparison)
    - [Save for analysis or sharing](#save-for-analysis-or-sharing)
  - [Comparison Workflows](#comparison-workflows)
    - [Scenario 1: Choosing Between Finalists](#scenario-1-choosing-between-finalists)
    - [Scenario 2: Evaluating Upgrade](#scenario-2-evaluating-upgrade)
    - [Scenario 3: Production vs Experimental](#scenario-3-production-vs-experimental)
  - [Multi-Package Analysis](#multi-package-analysis)
    - [Compare multiple packages](#compare-multiple-packages)
    - [Create decision matrix](#create-decision-matrix)
  - [Comparison Tips](#comparison-tips)
    - [1. Know what you're comparing](#1-know-what-youre-comparing)
    - [2. Weight dimensions by importance](#2-weight-dimensions-by-importance)
    - [3. Look for ties](#3-look-for-ties)
    - [4. Check the score gap](#4-check-the-score-gap)
    - [5. Document your decision](#5-document-your-decision)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Compare Packages

This guide shows how to compare two packages side-by-side to make informed decisions.

---

## Basic Comparison

### Compare two packages

```bash
ggen marketplace compare \
  --package-a "io.ggen.compiler-a" \
  --package-b "io.ggen.compiler-b"
```

Output shows:
- **Scores**: Total and per-dimension
- **Winner**: Which package scores higher overall
- **Dimension-by-dimension breakdown**: Where each excels
- **Recommendation**: Which to choose for production

---

## Understanding the Comparison

### Score comparison

```bash
ggen marketplace compare \
  --package-a "io.ggen.research-compiler" \
  --package-b "io.ggen.data-processor"
```

Output:
```json
{
  "package_a": {
    "id": "io.ggen.research-compiler",
    "total_score": 78,
    "maturity_level": "Production"
  },
  "package_b": {
    "id": "io.ggen.data-processor",
    "total_score": 71,
    "maturity_level": "Production"
  },
  "comparison": {
    "winner": "io.ggen.research-compiler",
    "score_difference": 7
  }
}
```

### Dimension-by-dimension analysis

See how they compare on each quality dimension:

```json
{
  "dimension_comparison": {
    "documentation": {
      "package_a": 18,
      "package_b": 15,
      "winner": "package_a"
    },
    "testing": {
      "package_a": 16,
      "package_b": 14,
      "winner": "package_a"
    },
    "security": {
      "package_a": 18,
      "package_b": 16,
      "winner": "package_a"
    },
    "performance": {
      "package_a": 12,
      "package_b": 10,
      "winner": "package_a"
    },
    "adoption": {
      "package_a": 12,
      "package_b": 8,
      "winner": "package_a"
    },
    "maintenance": {
      "package_a": 2,
      "package_b": 2,
      "winner": "tie"
    }
  }
}
```

---

## Detailed Comparison

### See more information

```bash
ggen marketplace compare \
  --package-a "io.ggen.research-compiler" \
  --package-b "io.ggen.data-processor" \
  --detailed
```

Detailed comparison includes:
- Full assessment details
- Specific metrics (coverage %, response time, etc.)
- Improvement recommendations for lower-scoring package
- Use case recommendations

---

## Export Comparison

### Save for analysis or sharing

```bash
ggen marketplace compare \
  --package-a "io.ggen.research-compiler" \
  --package-b "io.ggen.data-processor" \
  --output comparison.json

# Share with team
cat comparison.json | mail -s "Package Comparison" team@company.com

# Import to spreadsheet
cat comparison.json | jq '.dimension_comparison | to_entries | .[] | {dimension: .key, a: .value.package_a, b: .value.package_b}' > comparison.csv
```

---

## Comparison Workflows

### Scenario 1: Choosing Between Finalists

You've narrowed down to 2 packages:

```bash
# Get their full details
ggen marketplace maturity io.ggen.research-compiler --detailed
ggen marketplace maturity io.ggen.data-processor --detailed

# Compare directly
ggen marketplace compare \
  --package-a io.ggen.research-compiler \
  --package-b io.ggen.data-processor \
  --detailed \
  --output decision.json

# Review and document decision
cat decision.json | jq '.recommendation'
# Output: "io.ggen.research-compiler is better suited for production use (score: 78 vs 71)"
```

### Scenario 2: Evaluating Upgrade

New version available. Is it better?

```bash
# Compare old vs new
ggen marketplace compare \
  --package-a "io.ggen.compiler-v1.0" \
  --package-b "io.ggen.compiler-v1.1"

# Check if worth upgrading
ggen marketplace compare \
  --package-a "io.ggen.compiler-v1.0" \
  --package-b "io.ggen.compiler-v1.1" | \
  jq '.comparison.score_difference'
# If > 5: significant improvement
# If 0-5: minor improvement
# If < 0: regression
```

### Scenario 3: Production vs Experimental

Comparing maturity levels:

```bash
# Production package vs newer experimental package
ggen marketplace compare \
  --package-a "io.ggen.compiler-prod" \
  --package-b "io.ggen.compiler-experimental"

# Check specific concerns
ggen marketplace compare \
  --package-a "io.ggen.compiler-prod" \
  --package-b "io.ggen.compiler-experimental" | \
  jq '.dimension_comparison | to_entries[] | select(.value.winner == "package_b") | .key'
# Shows where experimental wins
```

---

## Multi-Package Analysis

### Compare multiple packages

```bash
# Create comparison matrix
for pkg1 in io.ggen.compiler io.ggen.parser io.ggen.processor; do
  for pkg2 in io.ggen.compiler io.ggen.parser io.ggen.processor; do
    if [ "$pkg1" \< "$pkg2" ]; then
      echo "=== $pkg1 vs $pkg2 ==="
      ggen marketplace compare --package-a "$pkg1" --package-b "$pkg2" | \
        jq '.comparison.winner'
    fi
  done
done
```

### Create decision matrix

```bash
#!/bin/bash

# Packages to compare
PACKAGES=(
  "io.ggen.compiler"
  "io.ggen.parser"
  "io.ggen.processor"
)

# Create comparison results file
echo "Package,Documentation,Testing,Security,Performance,Adoption,Maintenance,Total" > results.csv

for pkg in "${PACKAGES[@]}"; do
  ggen marketplace maturity "$pkg" | jq -r \
    "\"$pkg,\(.scores.documentation),\(.scores.testing),\(.scores.security),\(.scores.performance),\(.scores.adoption),\(.scores.maintenance),\(.total_score)\"" \
    >> results.csv
done

# View results
column -t -s ',' results.csv
```

---

## Comparison Tips

### 1. Know what you're comparing

```bash
# Before comparing, understand each package's focus
ggen marketplace maturity io.ggen.research-compiler --detailed
# Read the feedback and next_steps

ggen marketplace maturity io.ggen.data-processor --detailed
# Read the feedback and next_steps

# Then compare
ggen marketplace compare \
  --package-a io.ggen.research-compiler \
  --package-b io.ggen.data-processor
```

### 2. Weight dimensions by importance

Not all dimensions matter equally for your use case:

```
For Production Systems:
  Documentation: 20% (critical)
  Testing:       25% (critical)
  Security:      25% (critical)
  Maintenance:   20% (important)
  Performance:   5% (nice-to-have)
  Adoption:      5% (nice-to-have)

For Research:
  Documentation: 10%
  Testing:       10%
  Security:      5%
  Maintenance:   5%
  Performance:   30% (critical - need speed)
  Adoption:      40% (critical - need novelty)
```

### 3. Look for ties

```bash
ggen marketplace compare --package-a pkg1 --package-b pkg2 | \
  jq '.dimension_comparison | to_entries[] | select(.value.winner == "tie")'
# Ties suggest similar capability in those dimensions
```

### 4. Check the score gap

```bash
ggen marketplace compare --package-a pkg1 --package-b pkg2 | \
  jq '.comparison.score_difference'

# < 3 points: Functionally equivalent, choose by other factors
# 3-10 points: Meaningful difference, winner has clear advantages
# > 10 points: Significant quality gap
```

### 5. Document your decision

```markdown
## Package Comparison Decision

**Date**: 2025-11-15
**Decision**: io.ggen.research-compiler

### Comparison Details
| Dimension | pkg-a | pkg-b | Winner |
|-----------|-------|-------|--------|
| Documentation | 18 | 15 | pkg-a |
| Testing | 16 | 14 | pkg-a |
| Security | 18 | 16 | pkg-a |
| Performance | 12 | 10 | pkg-a |
| Adoption | 12 | 8 | pkg-a |
| Maintenance | 2 | 2 | tie |

### Rationale
- Clear winner across most dimensions (6 of 7)
- Most important for us: Documentation, Security, Testing
- pkg-a superior in all three critical areas
- Score difference (7 points) is meaningful
- Maintenance is weak point for both, acceptable trade-off

### Trade-offs
- Both have maintenance issues (2/10)
- Will plan for own maintenance if needed
- All other dimensions strongly favor pkg-a

### Next Steps
- Verify with team
- Plan adoption timeline
- Monitor for improvements in maintenance
```

---

## Troubleshooting

**Q: Packages have same score but I can still choose one**
A: Check dimension comparison. One may excel in areas critical to you.

**Q: Comparison shows new package is worse but it seemed better**
A: Assessment is based on automated metrics. If actual usage differs, provide feedback to improve scoring.

**Q: How often do comparison results change?**
A: Whenever either package's assessment updates (new release, tests added, vulnerabilities found, etc.).

**Q: Can I compare more than 2 packages?**
A: Run multiple comparisons and create your own matrix (see Multi-Package Analysis section above).
