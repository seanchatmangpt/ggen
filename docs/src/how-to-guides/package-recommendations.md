# How to Get Package Recommendations

This guide shows how to use ggen's recommendation system to discover the best packages for your specific use case.

---

## Overview

Instead of manually evaluating 50+ packages, let ggen recommend the right ones for:
- **Production deployments** (high reliability)
- **Research projects** (rapid iteration)
- **Enterprise systems** (comprehensive support)
- **Startup MVPs** (balanced tradeoffs)

---

## Basic Recommendations

### Get recommendations for your use case

```bash
# Production use
ggen marketplace recommend --use-case production

# Research & evaluation
ggen marketplace recommend --use-case research

# Enterprise deployments
ggen marketplace recommend --use-case enterprise

# Startup MVP
ggen marketplace recommend --use-case startup
```

Each recommendation includes:
- **Package ID** - Unique identifier
- **Maturity Level** - Experimental/Beta/Production/Enterprise
- **Total Score** - 0-100 rating
- **Reason** - Why it's recommended
- **Best For** - Ideal use cases

### Customize recommendations

```bash
# Production packages, but willing to accept Beta
ggen marketplace recommend --use-case production --min-score 50

# Research packages with very strong security
ggen marketplace recommend --use-case research --priority security --min-dimension-score 15

# Enterprise, only packages with excellent testing
ggen marketplace recommend --use-case enterprise --min-dimension-score 18
```

---

## Understanding Recommendations

### What "use case" does

| Use Case | Min Score | Best For | Focus Areas |
|----------|-----------|----------|-------------|
| **production** | 65 | Live systems, paying customers | Stability, documentation, testing |
| **research** | 40 | Papers, experiments, prototypes | Novel algorithms, rapid iteration |
| **enterprise** | 85 | Mission-critical, SLAs required | Security, maintenance, adoption |
| **startup** | 50 | MVP, quick validation | Balance of features and stability |

### What "priority" does

```bash
# Focus on documentation quality
ggen marketplace recommend --use-case production --priority documentation

# Focus on security
ggen marketplace recommend --use-case production --priority security

# Focus on testing
ggen marketplace recommend --use-case production --priority testing

# Focus on performance
ggen marketplace recommend --use-case production --priority performance

# Focus on all (default)
ggen marketplace recommend --use-case production --priority all
```

### Example output

```json
{
  "title": "Production-Ready Recommendations",
  "use_case": "production",
  "priority": "all",
  "min_score": 65,
  "recommendations": [
    {
      "rank": 1,
      "package_id": "io.ggen.research-compiler",
      "maturity_level": "Production",
      "total_score": 78,
      "reason": "Excellent documentation and testing with strong security posture",
      "best_for": "Research implementations with production requirements"
    },
    {
      "rank": 2,
      "package_id": "io.ggen.data-processor",
      "maturity_level": "Production",
      "total_score": 71,
      "reason": "Strong testing coverage and active maintenance",
      "best_for": "Data processing pipelines"
    }
  ],
  "guidance": "For production, focus on packages with score >= 65..."
}
```

---

## Advanced Recommendation Workflows

### Scenario: Finding packages for a research paper

```bash
# Get research packages
ggen marketplace recommend --use-case research

# Filter for specific priorities
ggen marketplace recommend --use-case research --priority security

# Compare top two recommendations
ggen marketplace compare \
  --package-a "io.ggen.research-compiler" \
  --package-b "io.ggen.data-processor"
```

### Scenario: Choosing for mission-critical system

```bash
# Enterprise recommendations only
ggen marketplace recommend --use-case enterprise

# Verify they meet minimum standards
ggen marketplace validate \
  --package-id "io.ggen.research-compiler" \
  --require-level "enterprise"
```

### Scenario: MVP with budget constraints

```bash
# Startup recommendations
ggen marketplace recommend --use-case startup

# Check detailed assessment
ggen marketplace maturity io.ggen.research-compiler --detailed

# Compare startup-recommended packages
ggen marketplace compare \
  --package-a "io.ggen.lightweight-parser" \
  --package-b "io.ggen.simple-compiler"
```

---

## Combining with Other Commands

### Recommendation → Validation → Adoption

```bash
# 1. Get recommendations
RECOMMENDED=$(ggen marketplace recommend --use-case production | jq '.recommendations[0].package_id')

# 2. Validate it meets your standards
ggen marketplace validate --package-id "$RECOMMENDED" --require-level "production"

# 3. Assess maturity details
ggen marketplace maturity "$RECOMMENDED" --detailed

# 4. Set it as production-ready in your environment
ggen marketplace validate --package-id "$RECOMMENDED" --update
```

### Recommendation → Comparison → Selection

```bash
# Get top 2 recommendations
ggen marketplace recommend --use-case production | \
  jq '.recommendations[0:2] | .[].package_id' > /tmp/candidates.txt

# Compare them
FIRST=$(sed -n '1p' /tmp/candidates.txt)
SECOND=$(sed -n '2p' /tmp/candidates.txt)

ggen marketplace compare --package-a "$FIRST" --package-b "$SECOND"

# Analyze the winner
WINNER=$(ggen marketplace compare --package-a "$FIRST" --package-b "$SECOND" | \
  jq -r '.recommendation' | grep -oE 'io\.ggen\.[a-z-]+')

echo "Recommended: $WINNER"
```

---

## Tips & Best Practices

### 1. Start with recommendations, then dig deeper

```bash
ggen marketplace recommend --use-case production
# See ranked list ↓
ggen marketplace maturity <top-package> --detailed
# See detailed scores ↓
```

### 2. Compare finalists side-by-side

```bash
# Narrowed down to 2? Compare them:
ggen marketplace compare --package-a pkg1 --package-b pkg2 --detailed
```

### 3. Match priorities to your needs

```
If you need:          Use priority:
-------               --------
Strong tests          testing
Zero vulnerabilities  security
Quick time-to-value   adoption
High performance      performance
Easy onboarding       documentation
Regular updates       maintenance (via search)
```

### 4. Document your selection

```markdown
## Package Selection Decision Log

**Project**: MyProject
**Date**: 2025-11-15
**Use Case**: Production deployment

**Recommendation**: io.ggen.research-compiler
**Score**: 78/100 (Production level)
**Reason**:
- Excellent documentation (18/20)
- Strong testing (16/20)
- Security audit passed (18/20)
- Active maintenance (2/10 is weak point)

**Alternatives Considered**:
- io.ggen.data-processor (71/100) - Good, but lower adoption
- io.ggen.parser (55/100) - Too early stage

**Decision**: Proceed with io.ggen.research-compiler
```

### 5. Re-evaluate periodically

```bash
# Monthly check: has recommendation changed?
ggen marketplace recommend --use-case production

# Track trends
ggen marketplace maturity io.ggen.research-compiler | \
  jq '.total_score' > /tmp/score-$(date +%Y-%m-%d).txt
```

---

## Troubleshooting

**Q: No recommendations appear for my use case**
A: Try lowering the score requirement: `--min-score 40`

**Q: Recommendation seems outdated**
A: Scores update automatically as packages improve. The recommendation reflects current data.

**Q: I want to ignore the recommendation**
A: You can choose any package. Recommendations are guidance, not requirements. Use `ggen marketplace validate --require-level beta` to check your own selection.

**Q: How often do recommendations change?**
A: As soon as a package's assessment changes (new release, test coverage added, vulnerability found, etc.). You can track changes by comparing maturity scores over time.

