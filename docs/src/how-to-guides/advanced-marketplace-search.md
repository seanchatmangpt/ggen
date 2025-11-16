# How to Use Advanced Marketplace Search

This guide shows how to find the perfect packages using advanced filtering and search capabilities.

---

## Basic Search

### Find packages by maturity level

```bash
# Production-ready packages
ggen marketplace search-maturity --min-level production

# Enterprise-grade only
ggen marketplace search-maturity --min-level enterprise

# Beta or better (experimental excluded)
ggen marketplace search-maturity --min-level beta
```

---

## Filtering by Dimension Scores

### Find packages with specific qualities

```bash
# Excellent documentation
ggen marketplace search-maturity --min-documentation 17

# Comprehensive testing
ggen marketplace search-maturity --min-testing 18

# Security-hardened packages
ggen marketplace search-maturity --min-security 18

# High-performance packages
ggen marketplace search-maturity --min-performance 12

# Well-adopted packages
ggen marketplace search-maturity --min-adoption 12

# Recently maintained
ggen marketplace search-maturity --min-maintenance 5
```

---

## Complex Search Queries

### Combine multiple filters

```bash
# Production packages with strong documentation and security
ggen marketplace search-maturity \
  --min-level production \
  --min-documentation 15 \
  --min-security 18

# Well-tested, actively maintained packages
ggen marketplace search-maturity \
  --min-testing 16 \
  --min-maintenance 4

# Research-grade with good documentation
ggen marketplace search-maturity \
  --min-level beta \
  --min-documentation 12 \
  --min-security 15
```

### Exclude problematic packages

```bash
# Production packages, but exclude those with low maintenance
ggen marketplace search-maturity \
  --min-level production \
  --exclude-maintenance-low
```

---

## Real-World Search Scenarios

### Scenario 1: Finding packages for production research

**Requirements:**
- Production maturity level
- Great documentation (need to understand usage)
- Good testing (must be reliable)
- Active maintenance (need support)

```bash
ggen marketplace search-maturity \
  --min-level production \
  --min-documentation 16 \
  --min-testing 14 \
  --min-maintenance 3
```

### Scenario 2: Finding high-security packages

**Requirements:**
- Security score of 18+ (excellent)
- Production or enterprise level
- Any documentation is fine
- Testing doesn't matter as much

```bash
ggen marketplace search-maturity \
  --min-level production \
  --min-security 18
```

### Scenario 3: Finding high-performance packages

**Requirements:**
- Performance score 12+ (solid)
- Good documentation (usage important for perf optimization)
- Any maturity level acceptable

```bash
ggen marketplace search-maturity \
  --min-performance 12 \
  --min-documentation 10
```

### Scenario 4: Finding widely-adopted packages

**Requirements:**
- Adoption score 10+ (proven in real-world)
- Production level (stability)
- Well-maintained (no abandoned projects)

```bash
ggen marketplace search-maturity \
  --min-level production \
  --min-adoption 10 \
  --min-maintenance 3
```

---

## Search Result Analysis

### Understanding search results

```json
{
  "search_criteria": {
    "min_score": 61,
    "min_documentation": 15,
    "min_testing": 14,
    "min_security": 16
  },
  "results": [
    {
      "package_id": "io.ggen.research-compiler",
      "total_score": 78,
      "maturity_level": "Production",
      "matches": {
        "documentation": 18,
        "testing": 16,
        "security": 18,
        "performance": 12,
        "adoption": 12,
        "maintenance": 2
      },
      "all_criteria_met": true
    }
  ],
  "total_matches": 5,
  "note": "Use results to filter marketplace packages for your specific needs"
}
```

### Key fields:
- **package_id** - Unique identifier
- **total_score** - Overall quality (0-100)
- **maturity_level** - Experimental/Beta/Production/Enterprise
- **matches** - Actual scores in each dimension
- **all_criteria_met** - Does package meet all your requirements?
- **total_matches** - How many packages found

---

## Advanced Search Techniques

### 1. Start broad, narrow down

```bash
# Step 1: Get all production packages
ggen marketplace search-maturity --min-level production
# Result: 18 packages

# Step 2: Add documentation requirement
ggen marketplace search-maturity --min-level production --min-documentation 15
# Result: 12 packages

# Step 3: Add security requirement
ggen marketplace search-maturity --min-level production --min-documentation 15 --min-security 16
# Result: 8 packages

# Step 4: Final refinement
ggen marketplace search-maturity --min-level production --min-documentation 15 --min-security 16 --min-testing 15
# Result: 5 packages ready for decision
```

### 2. Tier-based approach

```bash
# High-tier requirements (must have)
MUST_HAVE="--min-level production --min-security 17"

# Medium-tier requirements (should have)
SHOULD_HAVE="--min-documentation 14 --min-testing 14"

# Low-tier requirements (nice to have)
NICE_TO_HAVE="--min-maintenance 3"

# Search with all tiers
ggen marketplace search-maturity $MUST_HAVE $SHOULD_HAVE $NICE_TO_HAVE
```

### 3. Dimension-first approach

Find packages best at what matters most to you:

```bash
# If security is paramount
ggen marketplace search-maturity --min-security 18 --min-level production

# If you need great docs
ggen marketplace search-maturity --min-documentation 17 --min-level beta

# If performance is critical
ggen marketplace search-maturity --min-performance 13 --min-testing 15

# If you need active community
ggen marketplace search-maturity --min-adoption 12 --min-maintenance 4
```

---

## Search + Other Commands

### Search → Compare → Decide

```bash
# Step 1: Find candidates
CANDIDATES=$(ggen marketplace search-maturity --min-level production | \
  jq -r '.results[].package_id' | head -3)

# Step 2: Compare top 2
FIRST=$(echo "$CANDIDATES" | sed -n '1p')
SECOND=$(echo "$CANDIDATES" | sed -n '2p')

ggen marketplace compare --package-a "$FIRST" --package-b "$SECOND" --detailed

# Step 3: Decide and validate
ggen marketplace validate --package-id "$FIRST" --require-level production
```

### Search → Assess → Track

```bash
# Find candidates
ggen marketplace search-maturity --min-level production > candidates.json

# Assess each
cat candidates.json | jq -r '.results[].package_id' | while read pkg; do
  echo "=== $pkg ==="
  ggen marketplace maturity "$pkg" --detailed
done

# Track over time
cp candidates.json candidates-$(date +%Y-%m-%d).json
```

---

## Performance Tips

### 1. Use specific filters first

Good:
```bash
ggen marketplace search-maturity --min-level production --min-security 18
# Narrows down immediately
```

Bad:
```bash
ggen marketplace search-maturity --min-documentation 5 --min-testing 2
# Too broad, returns too many results
```

### 2. Combine related dimensions

```bash
# Good: Safety-focused
ggen marketplace search-maturity --min-security 18 --min-testing 16

# Good: Performance-focused
ggen marketplace search-maturity --min-performance 12 --min-testing 15

# Avoid: Unrelated combinations
ggen marketplace search-maturity --min-adoption 3 --min-performance 20
```

### 3. Exclude first, then require

```bash
# Exclude problematic packages
ggen marketplace search-maturity --exclude-maintenance-low

# Then add requirements
ggen marketplace search-maturity --exclude-maintenance-low --min-testing 15
```

---

## Search Examples by Industry

### SaaS / Web Services

Requirements: Security, Uptime, Scaling

```bash
ggen marketplace search-maturity \
  --min-level production \
  --min-security 17 \
  --min-testing 16 \
  --min-performance 11 \
  --min-maintenance 4
```

### Academic Research

Requirements: Documentation, Novelty, Flexibility

```bash
ggen marketplace search-maturity \
  --min-documentation 14 \
  --min-level beta \
  --min-adoption 5
```

### Fintech / Regulated Systems

Requirements: Security, Testing, Maintenance, Audit Trail

```bash
ggen marketplace search-maturity \
  --min-level enterprise \
  --min-security 18 \
  --min-testing 18 \
  --min-maintenance 5 \
  --exclude-maintenance-low
```

### Startups / MVPs

Requirements: Speed, Balance, Growing Quality

```bash
ggen marketplace search-maturity \
  --min-level beta \
  --min-documentation 10 \
  --min-testing 10 \
  --min-security 12
```

---

## Troubleshooting

**Q: Search returns too many results**
A: Add more specific requirements. Start with `--min-level` and `--min-security`, then add others.

**Q: Search returns no results**
A: Lower your requirements. Try one filter at a time to see where the bottleneck is.

**Q: I want packages that excel in one specific dimension**
A: Use that single filter: `ggen marketplace search-maturity --min-documentation 17`

**Q: Results keep changing**
A: Packages improve as they're developed. Re-search periodically to find improvements.

**Q: How do I save search results?**
A: Redirect to file:
```bash
ggen marketplace search-maturity --min-level production > results.json
cat results.json | jq '.results[] | {id: .package_id, score: .total_score}' > summary.json
```
