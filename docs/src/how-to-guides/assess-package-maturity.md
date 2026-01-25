<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Assess Package Maturity](#how-to-assess-package-maturity)
  - [Quick Assessment](#quick-assessment)
    - [Check a single package](#check-a-single-package)
  - [Understanding the Scores](#understanding-the-scores)
    - [Example breakdown](#example-breakdown)
  - [Assessment for Different Scenarios](#assessment-for-different-scenarios)
    - [For Production Deployments](#for-production-deployments)
    - [For Research Projects](#for-research-projects)
    - [For Internal/Private Use](#for-internalprivate-use)
  - [Comparing Multiple Packages](#comparing-multiple-packages)
    - [Get all packages sorted by score](#get-all-packages-sorted-by-score)
    - [Create a comparison report](#create-a-comparison-report)
    - [Find packages meeting specific criteria](#find-packages-meeting-specific-criteria)
  - [Acting on Assessment Results](#acting-on-assessment-results)
    - [If you see poor documentation (< 10 points)](#if-you-see-poor-documentation--10-points)
    - [If you see poor testing (< 10 points)](#if-you-see-poor-testing--10-points)
    - [If you see security issues (< 10 points)](#if-you-see-security-issues--10-points)
    - [If you see poor maintenance (< 3 points)](#if-you-see-poor-maintenance--3-points)
  - [Integration with Your Workflow](#integration-with-your-workflow)
    - [Automated maturity checks](#automated-maturity-checks)
    - [Documentation](#documentation)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Assess Package Maturity

This guide shows how to evaluate whether a package is ready for your specific use case.

---

## Quick Assessment

### Check a single package

```bash
ggen marketplace maturity --package-id "io.your.package" --detailed
```

This gives you:
- **Total score** (0-100)
- **Maturity level** (Experimental/Beta/Production/Enterprise)
- **Breakdown by dimension** (what's strong, what needs work)
- **Feedback** (specific improvement areas)
- **Next steps** (what to do next)

---

## Understanding the Scores

Each package is scored across 6 dimensions, each worth 0-20 points (except Performance 0-15 and Adoption/Maintenance which are 0-15 and 0-10):

| Dimension | Max Points | What It Measures |
|-----------|-----------|------------------|
| Documentation | 20 | README, API docs, examples, changelog |
| Testing | 20 | Unit tests, integration tests, E2E tests, coverage % |
| Security | 20 | Vulnerabilities, dependency audits, unsafe code |
| Performance | 15 | Benchmarks, optimization, determinism |
| Adoption | 15 | Downloads, citations, active community |
| Maintenance | 10 | Release cadence, responsiveness, active contributors |

### Example breakdown

```
Total Score: 75 (Production-Ready)

Documentation: 18/20 (90%)  ✅ Very Good
Testing:       16/20 (80%)  ✅ Good
Security:      19/20 (95%)  ✅ Excellent
Performance:   12/15 (80%)  ✅ Good
Adoption:      8/15  (53%)  ⚠️  Needs Work
Maintenance:   2/10  (20%)  ❌ Poor
```

This package is Production-ready overall, but the maintainers should:
1. Increase adoption (more users/citations needed)
2. Speed up releases (only 1 release per year)

---

## Assessment for Different Scenarios

### For Production Deployments

**Minimum requirements:**
- Total score ≥ 60 (at least Beta level)
- Documentation ≥ 15/20
- Testing ≥ 15/20
- Security ≥ 15/20
- Maintenance ≥ 5/10

**Check it:**
```bash
ggen marketplace validate --package-id "io.your.package" --require-level "production"
```

### For Research Projects

**Minimum requirements:**
- Total score ≥ 40 (can be Experimental)
- Documentation ≥ 10/20 (enough to understand)
- Testing ≥ 8/20 (basic test coverage okay)
- Adoption not critical

**Best approach:**
```bash
# See all packages, filter manually for your research needs
ggen marketplace list --all

# Check specific one
ggen marketplace maturity --package-id "io.your.package"
```

### For Internal/Private Use

**Minimum requirements:**
- Documentation ≥ 8/20 (you understand it)
- No security vulnerabilities critical to you
- Any maturity level is okay

**Check it:**
```bash
ggen marketplace maturity --package-id "io.your.package" --detailed
```

---

## Comparing Multiple Packages

### Get all packages sorted by score

```bash
ggen marketplace list --sort "score"
```

### Create a comparison report

```bash
# Generate dashboard with all packages and their scores
ggen marketplace dashboard --output comparison.json

# Then view or process it
cat comparison.json | jq '.assessments[] | {name: .package_name, score: .total_score, level: .maturity_level}'
```

Output:
```json
{
  "name": "Research Compiler",
  "score": 78,
  "level": "Production"
}
{
  "name": "Lightweight Parser",
  "score": 55,
  "level": "Beta"
}
```

### Find packages meeting specific criteria

```bash
# Only production packages
ggen marketplace list --min-maturity "production"

# Only packages with excellent documentation
ggen marketplace dashboard | jq '.assessments[] | select(.scores.documentation > 18)'

# Only packages actively maintained (release in last 30 days)
ggen marketplace dashboard | jq '.assessments[] | select(.maintenance.release_cadence > 4)'
```

---

## Acting on Assessment Results

### If you see poor documentation (< 10 points)

✅ **Do this:**
- Contact the maintainers suggesting documentation improvements
- Open an issue requesting examples or API docs
- Contribute documentation yourself

❌ **Don't use it for:**
- Production systems where you need support
- Projects where quick onboarding is critical

### If you see poor testing (< 10 points)

✅ **Do this:**
- Request or contribute tests
- Run it in staging environment first
- Use with caution in production

❌ **Don't use it for:**
- Mission-critical systems
- Large-scale deployments without extensive testing yourself

### If you see security issues (< 10 points)

✅ **Do this:**
- Contact maintainers immediately
- Review the vulnerabilities report
- Wait for a security release

❌ **Don't use it for:**
- Anything involving sensitive data
- Public-facing systems
- Any production deployment until fixed

### If you see poor maintenance (< 3 points)

✅ **Do this:**
- Check how long it's been since last release
- Assess if you can maintain a fork yourself
- Consider if you need active support

❌ **Don't use it for:**
- Products requiring frequent updates
- Systems in rapidly changing domains
- When you need responsive maintainers

---

## Integration with Your Workflow

### Automated maturity checks

**In Git hooks:**
```bash
# Pre-commit: check packages being added
ggen marketplace maturity --package-id "package-id" | jq -e '.total_score >= 60' \
  || echo "Package not production-ready" && exit 1
```

**In CI/CD:**
```bash
# Validate all dependencies meet standard
ggen marketplace validate --package-id "io.your.package" --require-level "production"
```

### Documentation

Document why you chose specific packages:
```
## Package Selection Criteria
- Minimum Production level (60+ points)
- Documentation ≥ 15/20
- Security ≥ 15/20
- No known vulnerabilities
- Maintenance releases ≥ quarterly

## Approved Packages
- io.ggen.research-compiler (78 points, Production)
- io.ggen.data-processor (71 points, Production)
```

---

## Troubleshooting

**Q: A package has a low score but seems good to me**
A: Score reflects automated metrics. If maintainers add more docs/tests/security audits, score improves. Contribute improvements if you believe the package is better than its score suggests.

**Q: Should I always require Enterprise level (80+)?**
A: No. Enterprise is overkill for most projects. Production (61-80) is the sweet spot for stability. Experimental/Beta can be fine for research and internal tools.

**Q: How do I know if a low score is temporary?**
A: Check the "next_steps" feedback. If it shows clear improvement plan and recent activity, it may improve soon. Check maintenance score specifically.

**Q: Can I request a re-assessment?**
A: Assessment is automatic. If a package improves (more tests, docs, releases), its score updates automatically. Submit a PR with improvements if you want to help.
