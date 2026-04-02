<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace Commands - Quick Reference](#marketplace-commands---quick-reference)
  - [Most Common Commands](#most-common-commands)
    - [1. Check Package Maturity](#1-check-package-maturity)
    - [2. Find Production-Ready Packages](#2-find-production-ready-packages)
    - [3. Get Recommendations](#3-get-recommendations)
    - [4. Compare Two Packages](#4-compare-two-packages)
    - [5. Generate Dashboard](#5-generate-dashboard)
    - [6. Export Data](#6-export-data)
  - [Maturity Score Interpretation](#maturity-score-interpretation)
  - [Dimension Scores (out of 20 for most, 15 for perf/adoption, 10 for maintenance)](#dimension-scores-out-of-20-for-most-15-for-perfadoption-10-for-maintenance)
    - [Documentation (0-20)](#documentation-0-20)
    - [Testing (0-20)](#testing-0-20)
    - [Security (0-20)](#security-0-20)
    - [Performance (0-15)](#performance-0-15)
    - [Adoption (0-15)](#adoption-0-15)
    - [Maintenance (0-10)](#maintenance-0-10)
  - [Search Filters](#search-filters)
    - [By Level](#by-level)
    - [By Dimension (0-20 scale)](#by-dimension-0-20-scale)
    - [By Use Case](#by-use-case)
  - [Common Workflows](#common-workflows)
    - [Evaluate New Package](#evaluate-new-package)
    - [Find Best Package for Production](#find-best-package-for-production)
    - [Marketplace Health Check](#marketplace-health-check)
    - [Export for Analysis](#export-for-analysis)
  - [Advanced Usage](#advanced-usage)
    - [Sector Bundles](#sector-bundles)
    - [Batch Operations](#batch-operations)
    - [Package Validation](#package-validation)
  - [Output Formats](#output-formats)
  - [Tips](#tips)
  - [Sample Packages (Test Data)](#sample-packages-test-data)
  - [Quick Checks](#quick-checks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace Commands - Quick Reference

## Most Common Commands

### 1. Check Package Maturity
```bash
ggen marketplace maturity --package_id <PACKAGE>

# Example
ggen marketplace maturity --package_id io.ggen.research-compiler
```

### 2. Find Production-Ready Packages
```bash
ggen marketplace search_maturity --min_level production

# With specific requirements
ggen marketplace search_maturity \
  --min_level production \
  --min_documentation 18 \
  --min_testing 15 \
  --min_security 18
```

### 3. Get Recommendations
```bash
ggen marketplace recommend --use_case <USE_CASE>

# Use cases: production, research, enterprise, startup
ggen marketplace recommend --use_case production
ggen marketplace recommend --use_case production --priority security
```

### 4. Compare Two Packages
```bash
ggen marketplace compare \
  --package_a <PACKAGE_A> \
  --package_b <PACKAGE_B>

# Example
ggen marketplace compare \
  --package_a io.ggen.research-compiler \
  --package_b io.ggen.data-processor
```

### 5. Generate Dashboard
```bash
ggen marketplace dashboard

# With filters
ggen marketplace dashboard --min_maturity production --output dashboard.json
```

### 6. Export Data
```bash
# CSV
ggen marketplace export --format csv --output packages.csv

# JSON
ggen marketplace export --format json --output packages.json

# HTML
ggen marketplace export --format html --output report.html
```

## Maturity Score Interpretation

| Score | Level | Meaning |
|-------|-------|---------|
| 0-40 | Experimental | Early-stage, not production-ready |
| 41-60 | Beta | Suitable for evaluation and testing |
| 61-80 | Production | Ready for production use |
| 81-100 | Enterprise | Mission-critical systems |

## Dimension Scores (out of 20 for most, 15 for perf/adoption, 10 for maintenance)

### Documentation (0-20)
- 18-20: Excellent (README, API docs, examples, changelog)
- 15-17: Good (most documentation present)
- 10-14: Adequate (basic documentation)
- 0-9: Poor (minimal documentation)

### Testing (0-20)
- 18-20: Excellent (90%+ coverage, all test types)
- 15-17: Good (80%+ coverage)
- 10-14: Adequate (60%+ coverage)
- 0-9: Poor (minimal testing)

### Security (0-20)
- 18-20: Excellent (no vulnerabilities, audited)
- 15-17: Good (1 minor vulnerability)
- 10-14: Adequate (2-3 vulnerabilities)
- 0-9: Poor (multiple security issues)

### Performance (0-15)
- 13-15: Excellent (benchmarks, optimized, deterministic)
- 10-12: Good (some optimization)
- 7-9: Adequate (basic performance)
- 0-6: Poor (no optimization)

### Adoption (0-15)
- 13-15: Excellent (5000+ downloads, citations)
- 10-12: Good (1000+ downloads)
- 7-9: Adequate (500+ downloads)
- 0-6: Poor (<500 downloads)

### Maintenance (0-10)
- 9-10: Excellent (active, responsive)
- 7-8: Good (regular updates)
- 5-6: Adequate (occasional updates)
- 0-4: Poor (stale)

## Search Filters

### By Level
```bash
--min_level experimental|beta|production|enterprise
```

### By Dimension (0-20 scale)
```bash
--min_documentation <SCORE>
--min_testing <SCORE>
--min_security <SCORE>
--min_performance <SCORE>
--min_adoption <SCORE>
--min_maintenance <SCORE>
```

### By Use Case
```bash
--use_case production    # Min score: 65
--use_case research      # Min score: 40
--use_case enterprise    # Min score: 85
--use_case startup       # Min score: 50
```

## Common Workflows

### Evaluate New Package
```bash
# 1. Check maturity
ggen marketplace maturity --package_id <PACKAGE>

# 2. Compare with alternatives
ggen marketplace compare --package_a <PACKAGE> --package_b <ALTERNATIVE>

# 3. Get improvement suggestions (if needed)
ggen marketplace improve <PACKAGE>
```

### Find Best Package for Production
```bash
# 1. Get production recommendations
ggen marketplace recommend --use_case production

# 2. Search with specific requirements
ggen marketplace search_maturity \
  --min_level production \
  --min_documentation 17 \
  --min_testing 16 \
  --min_security 18

# 3. Compare top candidates
ggen marketplace compare --package_a <TOP1> --package_b <TOP2>
```

### Marketplace Health Check
```bash
# 1. Generate dashboard
ggen marketplace dashboard --output dashboard.json

# 2. Generate validation report
ggen marketplace report --output health-report.json

# 3. Check production-ready count
ggen marketplace search_maturity --min_level production | jq '.total_matches'
```

### Export for Analysis
```bash
# Export all packages to CSV
ggen marketplace export --format csv --output all-packages.csv

# Export production-ready to JSON
ggen marketplace export \
  --format json \
  --min_maturity production \
  --output production-packages.json
```

## Advanced Usage

### Sector Bundles
```bash
# List available bundles
ggen marketplace bundles

# View bundle details
ggen marketplace bundle_info sector-academic-papers

# Install bundle
ggen marketplace install_bundle sector-academic-papers
```

### Batch Operations
```bash
# Assess multiple packages
ggen marketplace maturity_batch --output batch-results.json

# Emit validation receipts
ggen marketplace emit_receipts --report

# Generate registry artifacts
ggen marketplace generate_artifacts
```

### Package Validation
```bash
# Validate single package
ggen marketplace validate --package <PACKAGE>

# Validate with requirement
ggen marketplace validate \
  --package <PACKAGE> \
  --require_level production

# Validate all packages
ggen marketplace validate --update
```

## Output Formats

All commands output JSON by default. Use `| jq` for pretty printing:

```bash
ggen marketplace maturity --package_id <PACKAGE> | jq .
ggen marketplace recommend --use_case production | jq '.recommendations[]'
ggen marketplace dashboard | jq '.statistics'
```

## Tips

1. **Start with maturity assessment** to understand current state
2. **Use search_maturity** to find packages meeting your criteria
3. **Compare top candidates** before making final decision
4. **Export data** for offline analysis and reporting
5. **Generate dashboards** for team visibility

## Sample Packages (Test Data)

| Package | Score | Level | Use For |
|---------|-------|-------|---------|
| io.ggen.research-compiler | 94 | Enterprise | Production systems |
| io.ggen.data-processor | 87 | Enterprise | Production systems |
| io.ggen.lightweight-parser | 55 | Beta | Evaluation |
| io.ggen.simple-compiler | 48 | Beta | Research/Testing |
| io.ggen.experimental-ai | 30 | Experimental | Prototypes only |

## Quick Checks

```bash
# Is package production-ready?
ggen marketplace maturity --package_id <PACKAGE> | jq '.maturity_level'

# What's the total score?
ggen marketplace maturity --package_id <PACKAGE> | jq '.total_score'

# Which dimension needs work?
ggen marketplace maturity --package_id <PACKAGE> | jq '.scores'

# Get next steps
ggen marketplace maturity --package_id <PACKAGE> | jq '.next_steps[]'
```
