# How to Set Up Maturity Gates in CI/CD

This guide shows how to automatically enforce package maturity standards in your development pipeline.

---

## Why Maturity Gates Matter

**Without gates:**
- Someone accidentally adds an Experimental (low-quality) package
- Production deployment includes untested code
- Security vulnerabilities slip through

**With gates:**
- CI/CD automatically rejects packages below your standard
- Only vetted packages can be deployed
- Clear enforcement of quality policy

---

## GitHub Actions Integration

### Basic gate: Only Production packages allowed

Create `.github/workflows/maturity-gate.yml`:

```yaml
name: Maturity Gate

on: [pull_request]

jobs:
  check-maturity:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Extract packages from PR
        id: packages
        run: |
          # Extract package IDs from your dependencies file
          # Example: ggen.toml, package.json, Cargo.toml, etc.
          PACKAGES=$(grep -o 'io\.[a-z]*\.[a-z]*' ggen.toml | sort -u)
          echo "packages=$PACKAGES" >> $GITHUB_OUTPUT

      - name: Validate maturity
        run: |
          for package in ${{ steps.packages.outputs.packages }}; do
            echo "Checking maturity of $package..."
            ggen marketplace validate \
              --package-id "$package" \
              --require-level "production" \
              || exit 1
          done
```

**Result:** PR fails if any package is below Production level.

### Advanced gate: Custom requirements per project

Create `.github/maturity-policy.json`:

```json
{
  "default_level": "production",
  "critical_packages": [
    {
      "id": "io.ggen.security-audit",
      "require_level": "enterprise",
      "min_score": 90
    },
    {
      "id": "io.ggen.data-processor",
      "require_level": "production",
      "min_dimensions": {
        "testing": 15,
        "security": 18
      }
    }
  ],
  "allowed_exceptions": [
    "io.ggen.experimental-research"
  ]
}
```

Then in workflow:

```yaml
- name: Validate with custom policy
  run: |
    python scripts/validate_maturity.py ggen.toml .github/maturity-policy.json
```

Example `validate_maturity.py`:

```python
#!/usr/bin/env python3
import json
import subprocess
import sys

with open(sys.argv[2]) as f:
    policy = json.load(f)

with open(sys.argv[1]) as f:
    config = f.read()

# Extract packages
packages = re.findall(r'io\.[a-z]*\.[a-z]*', config)

for pkg in packages:
    if pkg in policy['allowed_exceptions']:
        print(f"✓ {pkg} (exception allowed)")
        continue

    # Get maturity info
    result = subprocess.run(
        ['ggen', 'marketplace', 'maturity', '--package-id', pkg],
        capture_output=True, text=True
    )

    maturity = json.loads(result.stdout)

    # Check if meets policy
    if maturity['total_score'] < 60:
        print(f"✗ {pkg} fails: score {maturity['total_score']} < 60")
        sys.exit(1)

print("All packages pass maturity gate ✓")
```

### Gate with improvement plan requirement

```yaml
- name: Check improvement plans
  run: |
    for package in $PACKAGES; do
      ggen marketplace validate \
        --package-id "$package" \
        --improvement-plan > /tmp/plan.json

      # Require documented improvement plan for Beta packages
      if grep -q '"maturity_level": "Beta"' /tmp/plan.json; then
        grep -q '"improvement_plan":' /tmp/plan.json || exit 1
      fi
    done
```

---

## Pre-commit Hook Integration

### Local gate before pushing

Create `.githooks/pre-push`:

```bash
#!/bin/bash

echo "Checking package maturity before push..."

# Get list of changed files
CHANGED=$(git diff --cached --name-only)

# Extract package IDs from changed files
PACKAGES=$(grep -h -o 'io\.[a-z]*\.[a-z]*' $CHANGED | sort -u)

for package in $PACKAGES; do
    echo "  Validating $package..."
    ggen marketplace validate --package-id "$package" --require-level "production"

    if [ $? -ne 0 ]; then
        echo "✗ Package $package does not meet production standard"
        echo "  Push rejected. Fix maturity issues or request exception."
        exit 1
    fi
done

echo "✓ All packages meet maturity standards"
exit 0
```

Make executable:
```bash
chmod +x .githooks/pre-push
```

Configure git:
```bash
git config core.hooksPath .githooks
```

---

## Release Gate: Enforce before Deployment

### Only deploy Production packages

Create `scripts/pre-deploy.sh`:

```bash
#!/bin/bash
set -e

ENVIRONMENT=$1  # "staging" or "production"
MIN_SCORE=60

echo "Pre-deployment maturity check for $ENVIRONMENT..."

# Get list of packages to deploy
PACKAGES=$(ggen marketplace list | jq -r '.packages[].id')

FAILED=0
for package in $PACKAGES; do
    SCORE=$(ggen marketplace maturity --package-id "$package" \
            | jq '.total_score')

    if [ "$ENVIRONMENT" = "production" ] && [ "$SCORE" -lt 70 ]; then
        echo "✗ $package score $SCORE too low for production"
        FAILED=1
    fi
done

if [ $FAILED -eq 1 ]; then
    echo "Deployment blocked by maturity gate"
    exit 1
fi

echo "✓ All packages approved for $ENVIRONMENT deployment"
```

Use in deployment pipeline:

```bash
bash scripts/pre-deploy.sh production && \
  docker build -t myapp:latest . && \
  docker push myapp:latest
```

---

## Reporting & Monitoring

### Weekly maturity report

Create `scripts/maturity-report.sh`:

```bash
#!/bin/bash

echo "📊 Weekly Marketplace Maturity Report"
echo "Generated: $(date)"
echo

# Generate dashboard
ggen marketplace dashboard --output /tmp/dashboard.json

# Statistics
echo "## Summary"
jq '.statistics' /tmp/dashboard.json

# Packages needing improvement
echo ""
echo "## Packages Below Production (Score < 61)"
jq '.assessments[] | select(.total_score < 61) | {name: .package_name, score: .total_score, level: .maturity_level}' /tmp/dashboard.json

# Security issues
echo ""
echo "## Security Concerns"
jq '.assessments[] | select(.scores.security < 15) | {name: .package_name, security_score: .scores.security}' /tmp/dashboard.json

# Maintenance issues
echo ""
echo "## Inactive Packages"
jq '.assessments[] | select(.scores.maintenance < 3) | {name: .package_name, days_since_release: .maintenance.days_since_last_release}' /tmp/dashboard.json
```

Schedule with cron:

```bash
# Add to crontab: every Monday at 9am
0 9 * * 1 /path/to/scripts/maturity-report.sh > /tmp/maturity-report.txt && \
  mail -s "Weekly Maturity Report" team@company.com < /tmp/maturity-report.txt
```

---

## Handling Exceptions

### Approved list for specific packages

Create `APPROVED_EXCEPTIONS.md`:

```markdown
# Approved Maturity Exceptions

## Beta-level packages allowed

- **io.ggen.experimental-features** (score: 52)
  - **Reason**: Prototype for Q4 research initiative
  - **Approved by**: @jane-researcher
  - **Date**: 2025-11-01
  - **Expires**: 2025-12-31
  - **Review**: Will upgrade to Production before release

## Experimental packages (research only)

- **io.ggen.novel-algorithm** (score: 35)
  - **Reason**: New research collaboration, not for production
  - **Scope**: Development/research branch only
  - **Approved by**: @research-lead
  - **Constraints**: Cannot merge to main branch
```

Reference in CI/CD:

```bash
if grep -q "$PACKAGE_ID" APPROVED_EXCEPTIONS.md; then
    echo "✓ $PACKAGE_ID has approved exception"
else
    ggen marketplace validate --package-id "$PACKAGE_ID" --require-level "production"
fi
```

---

## Troubleshooting

**Q: Our internal package isn't in the marketplace**
A: You can assess internal packages too:
```bash
ggen marketplace maturity --package-id "io.company.internal-pkg"
```

**Q: Gate is too strict, blocking legitimate Beta packages**
A: Create an exception list (see above) or lower the requirement:
```bash
ggen marketplace validate --package-id "pkg" --require-level "beta"
```

**Q: How often do maturity scores update?**
A: Automatically whenever package metrics change (new test coverage report, security scan, release, etc.). No manual refresh needed.

**Q: Can I override a gate failure in emergency?**
A: Yes, but document it:
```bash
git commit --allow-empty -m "OVERRIDE: Deploying experimental package due to urgent security fix"
```
Then discuss in code review.
