<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Getting Started with Marketplace Maturity](#tutorial-getting-started-with-marketplace-maturity)
  - [Step 1: Assess Your First Package](#step-1-assess-your-first-package)
    - [Run the command](#run-the-command)
    - [What this means](#what-this-means)
  - [Step 2: Understand the 4 Maturity Levels](#step-2-understand-the-4-maturity-levels)
    - [Run a quick check on multiple packages](#run-a-quick-check-on-multiple-packages)
    - [Try filtering packages](#try-filtering-packages)
  - [Step 3: Generate a Marketplace Dashboard](#step-3-generate-a-marketplace-dashboard)
    - [What this tells you](#what-this-tells-you)
    - [Export the dashboard](#export-the-dashboard)
  - [Step 4: Filter and Find Packages for Your Needs](#step-4-filter-and-find-packages-for-your-needs)
    - [Scenario: You need a package for production research](#scenario-you-need-a-package-for-production-research)
    - [Scenario: You're looking for experimental packages for R&D](#scenario-youre-looking-for-experimental-packages-for-rd)
    - [Scenario: You want the top performers](#scenario-you-want-the-top-performers)
  - [Step 5: Validate Before Using](#step-5-validate-before-using)
  - [What You've Learned](#what-youve-learned)
  - [Next Steps](#next-steps)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: Getting Started with Marketplace Maturity

In this tutorial, you'll learn to assess package quality in the ggen marketplace, understand maturity levels, and make informed decisions about which packages to use.

**Time required**: 20-30 minutes
**Prerequisites**: ggen CLI installed, basic command-line familiarity
**What you'll learn**: Assess package maturity, read maturity scores, filter marketplace, generate dashboards

---

## Step 1: Assess Your First Package

Let's start by checking the maturity of a package. We'll assess a hypothetical package and understand what the scores mean.

### Run the command

```bash
ggen marketplace maturity --package-id "io.ggen.research-compiler" --detailed
```

You'll see output like:

```json
{
  "package_id": "io.ggen.research-compiler",
  "package_name": "Research Compiler",
  "total_score": 78,
  "maturity_level": "Production",
  "description": "Production-ready with solid fundamentals",
  "scores": {
    "documentation": 18,
    "testing": 16,
    "security": 18,
    "performance": 12,
    "adoption": 12,
    "maintenance": 2
  },
  "percentages": {
    "documentation": "90%",
    "testing": "80%",
    "security": "90%",
    "performance": "80%",
    "adoption": "80%",
    "maintenance": "20%"
  },
  "feedback": [...],
  "next_steps": [...]
}
```

### What this means

- **Total Score (78)**: On a scale of 0-100, this package scores 78
- **Maturity Level (Production)**: Safe for production use
- **Scores by dimension**: Each of the 6 quality dimensions has a score
  - Documentation: 18/20 (90%) - Nearly complete docs
  - Testing: 16/20 (80%) - Good test coverage
  - Security: 18/20 (90%) - Very safe, minimal vulnerabilities
  - Performance: 12/15 (80%) - Benchmarks available
  - Adoption: 12/15 (80%) - Good community usage
  - Maintenance: 2/10 (20%) - Could use more active releases

---

## Step 2: Understand the 4 Maturity Levels

Every package falls into one of four maturity categories. Let's compare them:

### Run a quick check on multiple packages

```bash
ggen marketplace list --all
```

This shows all packages. You'll notice each has a maturity level. Here's what each means:

| Level | Score Range | Use Case | Risk Level |
|-------|-------------|----------|-----------|
| 🔴 **Experimental** | 0-40 | Research, prototyping | Very High |
| 🟡 **Beta** | 41-60 | Testing, feedback | High |
| 🟢 **Production** | 61-80 | Live systems | Low |
| 🟦 **Enterprise** | 81-100 | Mission-critical | Very Low |

### Try filtering packages

```bash
# Only production-ready packages
ggen marketplace list --min-maturity "production"

# Only enterprise packages
ggen marketplace list --maturity-level "enterprise"
```

Notice how filtering gives you a smaller, more reliable list. This is useful when you're building something important.

---

## Step 3: Generate a Marketplace Dashboard

Now let's see the health of the entire marketplace at once:

```bash
ggen marketplace dashboard
```

You'll get output like:

```json
{
  "generated_at": "2025-11-15T10:30:00Z",
  "statistics": {
    "total_packages": 45,
    "average_score": 62,
    "level_distribution": {
      "experimental": 8,
      "beta": 12,
      "production": 18,
      "enterprise": 7
    },
    "average_scores_by_dimension": {
      "documentation": 14.2,
      "testing": 13.1,
      "security": 14.8,
      "performance": 11.3,
      "adoption": 10.5,
      "maintenance": 5.2
    }
  }
}
```

### What this tells you

- **Total packages**: 45 available to choose from
- **Average score**: 62 means the marketplace is slightly above "Beta" level overall
- **Distribution**: Most packages are Production-ready (18), with a healthy proportion of Enterprise (7)
- **Weak dimension**: Maintenance scores (5.2/10) suggest packages could use more frequent releases

### Export the dashboard

```bash
# Save to file for sharing or analysis
ggen marketplace dashboard --output marketplace-health.json

# View in your editor
cat marketplace-health.json
```

---

## Step 4: Filter and Find Packages for Your Needs

### Scenario: You need a package for production research

```bash
# Get only production and enterprise packages
ggen marketplace list --min-maturity "production"

# Sort by highest score first
ggen marketplace list --min-maturity "production" --sort "score"
```

### Scenario: You're looking for experimental packages for R&D

```bash
# Show only experimental and beta packages
ggen marketplace list --maturity-level "experimental"
ggen marketplace list --maturity-level "beta"
```

### Scenario: You want the top performers

```bash
# List all packages sorted by score (highest first)
ggen marketplace list --sort "score"

# Take the top 10
ggen marketplace list --sort "score" | head -10
```

---

## Step 5: Validate Before Using

Before adopting a package, validate it meets your requirements:

```bash
# Basic validation
ggen marketplace validate --package-id "io.ggen.research-compiler"

# Require specific maturity level
ggen marketplace validate --package-id "io.ggen.research-compiler" --require-level "production"

# Get improvement suggestions
ggen marketplace validate --package-id "io.ggen.research-compiler" --improvement-plan
```

The improvement plan shows exactly what the package maintainers should focus on next.

---

## What You've Learned

✅ Assessed individual package maturity
✅ Understood the 4 maturity levels and when to use each
✅ Generated marketplace dashboards
✅ Filtered packages by maturity for your use case
✅ Validated packages before adoption

---

## Next Steps

- **Learn more about the scoring system**: Read the [Marketplace Maturity Reference](../reference/maturity-scoring.md)
- **Integrate with your workflow**: See [How to Set Up Maturity Checks in CI/CD](../how-to-guides/maturity-cicd-gates.md)
- **Understand the business model**: Read [Why Marketplace Maturity Matters](../explanations/marketplace-maturity.md)
- **Track workflow metrics**: Learn about [Workflow Analytics](../tutorials/workflow-analytics-basics.md)

---

## Troubleshooting

**Q: I don't see any packages**
A: Run `ggen marketplace list --all` to see the full inventory, including experimental packages.

**Q: The maturity level seems wrong for a package I like**
A: Maturity is calculated automatically from measurable metrics. If you think a score should be higher, the maintainers should add documentation, tests, or releases.

**Q: Can I use Beta packages in production?**
A: Generally not recommended, but depends on your risk tolerance. Use `ggen marketplace validate --require-level "production"` to enforce standards.
