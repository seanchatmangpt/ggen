# How-to: Implement Andon System

**Set up rapid failure detection with yellow (1-5 failures) and red (6+) alerts**

---

## Prerequisites

- CI/CD system (GitHub Actions, GitLab CI, Jenkins, etc.)
- Existing test suite with `cargo test` integration
- Notification system (Slack, email, PagerDuty, etc.)

---

## Problem Statement

Tests fail silently in CI. By the time you notice:
- 20 commits have passed
- Multiple features are broken
- Root cause is hard to trace

This guide shows you how to implement Andon alerts for immediate failure visibility.

---

## Step 1: Define Alert Thresholds

### Andon Levels

| Level | Threshold | Meaning | Action |
|-------|-----------|---------|--------|
| 🟢 **Green** | 0 failures | Normal | Proceed with feature work |
| 🟡 **Yellow** | 1-5 failures | Warning | Fix before next feature |
| 🔴 **Red** | 6+ failures | Emergency | STOP all work, team meeting |

**Philosophy:** 1-5 failures = isolated issue. 6+ = systemic problem (cascading failures).

---

## Step 2: Implement CI Detection Script

### Create Andon Check Script

```bash
#!/bin/bash
# scripts/andon_check.sh

set -e

echo "🚨 Andon System: Test Failure Detection"
echo "========================================"

# Run tests and capture output
TEST_OUTPUT=$(cargo test --all 2>&1 | tee test_results.log)

# Count failures
FAILURES=$(echo "$TEST_OUTPUT" | grep -c "test.*FAILED" || true)

# Determine Andon level
if [ $FAILURES -eq 0 ]; then
    echo "✅ GREEN ANDON: All tests passing"
    echo "STATUS=green" >> $GITHUB_ENV
    exit 0

elif [ $FAILURES -ge 1 ] && [ $FAILURES -le 5 ]; then
    echo "🟡 YELLOW ANDON: $FAILURES failures detected"
    echo "⚠️  WARNING: Fix these issues before adding new features"
    echo ""
    echo "Failed tests:"
    echo "$TEST_OUTPUT" | grep "test.*FAILED"
    echo ""
    echo "STATUS=yellow" >> $GITHUB_ENV
    echo "FAILURE_COUNT=$FAILURES" >> $GITHUB_ENV
    exit 1

elif [ $FAILURES -ge 6 ]; then
    echo "🔴 RED ANDON: $FAILURES failures detected (CRITICAL)"
    echo "🛑 STOP THE LINE: Cascading failures detected"
    echo ""
    echo "Failed tests:"
    echo "$TEST_OUTPUT" | grep "test.*FAILED"
    echo ""
    echo "ACTION REQUIRED:"
    echo "1. Stop all feature work immediately"
    echo "2. Schedule team meeting within 1 hour"
    echo "3. Investigate root cause (likely systemic issue)"
    echo "4. No merges until green"
    echo ""
    echo "STATUS=red" >> $GITHUB_ENV
    echo "FAILURE_COUNT=$FAILURES" >> $GITHUB_ENV
    exit 1
fi
```

**Make Executable:**

```bash
chmod +x scripts/andon_check.sh
```

---

## Step 3: Integrate with CI/CD

### GitHub Actions

```yaml
# .github/workflows/andon.yml

name: Andon System

on:
  push:
    branches: [main, master, develop]
  pull_request:

jobs:
  andon-check:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run Andon Check
        id: andon
        run: ./scripts/andon_check.sh
        continue-on-error: true

      - name: Notify Yellow Andon (Slack)
        if: env.STATUS == 'yellow'
        uses: slackapi/slack-github-action@v1
        with:
          webhook-url: ${{ secrets.SLACK_WEBHOOK }}
          payload: |
            {
              "text": "🟡 YELLOW ANDON: ${{ env.FAILURE_COUNT }} test failures",
              "blocks": [
                {
                  "type": "section",
                  "text": {
                    "type": "mrkdwn",
                    "text": "*Yellow Andon Alert*\n\n${{ env.FAILURE_COUNT }} tests failing. Fix before next feature work.\n\n<${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}|View Details>"
                  }
                }
              ]
            }

      - name: Notify Red Andon (PagerDuty)
        if: env.STATUS == 'red'
        run: |
          curl -X POST https://events.pagerduty.com/v2/enqueue \
            -H 'Content-Type: application/json' \
            -d '{
              "routing_key": "${{ secrets.PAGERDUTY_KEY }}",
              "event_action": "trigger",
              "payload": {
                "summary": "🔴 RED ANDON: ${{ env.FAILURE_COUNT }} test failures (CRITICAL)",
                "severity": "critical",
                "source": "GitHub Actions",
                "custom_details": {
                  "failure_count": "${{ env.FAILURE_COUNT }}",
                  "branch": "${{ github.ref }}",
                  "commit": "${{ github.sha }}"
                }
              }
            }'

      - name: Block Merge on Red Andon
        if: env.STATUS == 'red'
        run: exit 1
```

### GitLab CI

```yaml
# .gitlab-ci.yml

stages:
  - test
  - notify

andon_check:
  stage: test
  script:
    - ./scripts/andon_check.sh
  artifacts:
    when: always
    paths:
      - test_results.log
    reports:
      junit: test_results.xml

notify_yellow:
  stage: notify
  only:
    variables:
      - $STATUS == "yellow"
  script:
    - |
      curl -X POST $SLACK_WEBHOOK \
        -H 'Content-Type: application/json' \
        -d '{
          "text": "🟡 YELLOW ANDON: '"$FAILURE_COUNT"' test failures"
        }'

notify_red:
  stage: notify
  only:
    variables:
      - $STATUS == "red"
  script:
    - |
      curl -X POST $PAGERDUTY_WEBHOOK \
        -H 'Content-Type: application/json' \
        -d '{
          "event_action": "trigger",
          "payload": {
            "summary": "🔴 RED ANDON: '"$FAILURE_COUNT"' failures (CRITICAL)",
            "severity": "critical"
          }
        }'
  when: on_failure
```

---

## Step 4: Configure Notifications

### Slack Integration

1. **Create Incoming Webhook:**
   - Go to Slack → Apps → Incoming Webhooks
   - Add to channel (e.g., `#build-alerts`)
   - Copy webhook URL

2. **Add to GitHub Secrets:**
   ```bash
   # GitHub Settings → Secrets → New repository secret
   Name: SLACK_WEBHOOK
   Value: https://hooks.slack.com/services/YOUR/WEBHOOK/URL
   ```

3. **Test:**
   ```bash
   curl -X POST "$SLACK_WEBHOOK" \
     -H 'Content-Type: application/json' \
     -d '{"text": "🟡 Test Andon Alert"}'
   ```

### Email Alerts

```yaml
# Add to .github/workflows/andon.yml

- name: Send Email on Red Andon
  if: env.STATUS == 'red'
  uses: dawidd6/action-send-mail@v3
  with:
    server_address: smtp.gmail.com
    server_port: 465
    username: ${{ secrets.EMAIL_USERNAME }}
    password: ${{ secrets.EMAIL_PASSWORD }}
    subject: "🔴 RED ANDON: ${{ env.FAILURE_COUNT }} Critical Test Failures"
    to: team@example.com
    from: ci@example.com
    body: |
      CRITICAL: ${{ env.FAILURE_COUNT }} test failures detected.

      Action Required:
      1. Stop all feature work immediately
      2. Schedule team meeting within 1 hour
      3. Investigate root cause

      Details: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
```

---

## Step 5: Response Playbook

### Yellow Andon Response (1-5 Failures)

**Within 1 Hour:**
1. Review failed test names
2. Assign ownership (who broke what?)
3. Create fix branch
4. Prioritize over new features

**Example:**
```bash
# Failed tests from Andon alert:
# - test_graph_export_rdf ... FAILED
# - test_ontology_validate_circular ... FAILED

# 1. Assign ownership
git log --all --oneline -- crates/ggen-core/src/graph/export.rs | head -5
# Identify recent committer

# 2. Create fix branch
git checkout -b fix/yellow-andon-graph-export

# 3. Fix and verify
cargo test --test graph_tests::test_graph_export_rdf
```

### Red Andon Response (6+ Failures)

**Immediate (< 30 minutes):**
1. Page on-call engineer
2. Stop all merges (freeze `main` branch)
3. Create war room (Zoom/Slack huddle)

**Within 1 Hour:**
4. Identify common failure pattern
5. Determine root cause (cascading dependencies?)
6. Roll back if necessary

**Recovery:**
7. Fix root cause
8. Run full test suite (green required)
9. Post-mortem: what went wrong?

**Example Red Andon Scenario:**

```
🔴 RED ANDON: 23 failures detected

Common pattern: All failures in ontology module
Root cause: Dependency update (rdf-rs 0.5 → 0.6) broke API

Response:
1. Revert dependency update (git revert abc123)
2. Run tests: cargo test --all (✅ green)
3. Create compatibility shim for rdf-rs 0.6
4. Test shim separately before merge
5. Document: "rdf-rs 0.6 requires migration plan"
```

---

## Step 6: Dashboard Visualization

### Create Andon Status Page

```html
<!-- docs/andon-status.html -->
<!DOCTYPE html>
<html>
<head>
    <title>Andon Status - ggen</title>
    <style>
        .green { background: #4caf50; color: white; }
        .yellow { background: #ffeb3b; color: black; }
        .red { background: #f44336; color: white; }
        .status-box {
            padding: 40px;
            font-size: 48px;
            text-align: center;
            font-weight: bold;
            border-radius: 10px;
            margin: 20px;
        }
    </style>
</head>
<body>
    <h1>Andon System Status</h1>
    <div id="status" class="status-box green">
        🟢 GREEN
    </div>
    <div id="details">
        <p>Last updated: <span id="timestamp"></span></p>
        <p>Failed tests: <span id="failures">0</span></p>
    </div>

    <script>
        // Fetch latest status from CI
        async function updateStatus() {
            const response = await fetch('/api/andon-status');
            const data = await response.json();

            const statusBox = document.getElementById('status');
            statusBox.className = `status-box ${data.status}`;
            statusBox.textContent = data.status === 'green' ? '🟢 GREEN' :
                                    data.status === 'yellow' ? '🟡 YELLOW' :
                                    '🔴 RED';

            document.getElementById('failures').textContent = data.failures;
            document.getElementById('timestamp').textContent = new Date().toLocaleString();
        }

        updateStatus();
        setInterval(updateStatus, 60000);  // Update every minute
    </script>
</body>
</html>
```

**Host on GitHub Pages or internal server.**

---

## Verification Checklist

After implementing Andon system:

- [ ] Yellow Andon triggers on 1-5 failures
- [ ] Red Andon triggers on 6+ failures
- [ ] Slack/email notifications arrive within 2 minutes
- [ ] CI blocks merges on Red Andon
- [ ] Team has documented response playbook
- [ ] Dashboard shows real-time status
- [ ] On-call rotation configured (for Red Andon pages)

---

## Common Pitfalls

### Pitfall 1: Alert Fatigue

**Problem:** Yellow Andon fires 50 times/day → team ignores alerts.

**Solution:** Tune thresholds or fix chronic issues.

```bash
# If >10 Yellow Andons in 24 hours, escalate to Red
YELLOW_COUNT=$(grep "YELLOW ANDON" ci.log | grep "$(date +%Y-%m-%d)" | wc -l)
if [ $YELLOW_COUNT -gt 10 ]; then
    echo "🔴 RED ANDON: Chronic Yellow Andon (>10 in 24h)"
    exit 1
fi
```

### Pitfall 2: False Positives from Flaky Tests

**Problem:** Andon fires, but re-run passes (flaky test).

**Solution:** Track flakiness rate, disable flaky tests until fixed.

```bash
# Run test 3 times, only fail if fails all 3
for i in {1..3}; do
    cargo test test_name && break
    [ $i -eq 3 ] && exit 1  # Failed all 3 times
done
```

---

## Related Guides

- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md) - Learn Andon concept
- [Andon Alerts Reference](../reference/andon-alerts-reference.md) - Full alert type catalog
- [How-to: Eliminate Test Warnings](eliminate-test-warnings.md) - Reduce Yellow Andon triggers

---

**Last Updated:** 2025-11-18
