<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: Andon Alerts](#reference-andon-alerts)
  - [Alert Levels](#alert-levels)
  - [Alert Triggers](#alert-triggers)
    - [Green Andon (0 Failures)](#green-andon-0-failures)
    - [Yellow Andon (1-5 Failures)](#yellow-andon-1-5-failures)
    - [Red Andon (6+ Failures)](#red-andon-6-failures)
  - [Alert Configuration](#alert-configuration)
    - [Threshold Tuning](#threshold-tuning)
  - [Remediation Playbooks](#remediation-playbooks)
    - [Yellow Andon Response](#yellow-andon-response)
    - [Red Andon Response](#red-andon-response)
  - [Escalation Matrix](#escalation-matrix)
  - [Notification Templates](#notification-templates)
    - [Slack (Yellow)](#slack-yellow)
    - [PagerDuty (Red)](#pagerduty-red)
  - [Metrics & SLAs](#metrics--slas)
  - [Related Resources](#related-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: Andon Alerts

**Alert types, thresholds, and remediation actions**

---

## Alert Levels

| Level | Symbol | Threshold | Severity | Action Required | Response Time |
|-------|--------|-----------|----------|-----------------|---------------|
| **Green** | 🟢 | 0 failures | Normal | Proceed with feature work | N/A |
| **Yellow** | 🟡 | 1-5 failures | Warning | Fix before next feature | <4 hours |
| **Red** | 🔴 | 6+ failures | Critical | STOP all work, team meeting | <30 minutes |

---

## Alert Triggers

### Green Andon (0 Failures)

**Meaning:** All systems operational.

**Actions:**
- ✅ Proceed with feature development
- ✅ Approve pull requests
- ✅ Deploy to production

**CI Output:**
```
✅ GREEN ANDON: All tests passing (487/487)
Status: HEALTHY
```

---

### Yellow Andon (1-5 Failures)

**Meaning:** Isolated failures detected, not cascading.

**Actions:**
1. Review failed test names
2. Assign ownership (git blame recent changes)
3. Create fix branch
4. Prioritize fix over new features
5. Fix within 4 hours

**CI Output:**
```
🟡 YELLOW ANDON: 3 failures detected
⚠️  WARNING: Fix these issues before adding new features

Failed tests:
- test graph::test_concurrent_export ... FAILED
- test ontology::test_validate_circular ... FAILED
- test lifecycle::test_rollback_state ... FAILED

ACTION: Create fix branches, assign ownership
```

**Notification Channels:**
- Slack: `#build-alerts` channel
- Email: Team distribution list
- CI dashboard: Yellow status badge

---

### Red Andon (6+ Failures)

**Meaning:** Cascading failures, systemic issue.

**Actions (IMMEDIATE):**
1. **Page on-call engineer** (PagerDuty)
2. **Freeze main branch** (block all merges)
3. **Create war room** (Zoom/Slack huddle)
4. **Investigate root cause**:
   - Dependency update broke API?
   - Merge introduced cascading bug?
   - Infrastructure issue (CI runner)?
5. **Roll back if necessary**
6. **Fix and verify**
7. **Post-mortem within 24 hours**

**CI Output:**
```
🔴 RED ANDON: 23 failures detected (CRITICAL)
🛑 STOP THE LINE: Cascading failures detected

Failed tests:
[List of 23 failed tests...]

ACTION REQUIRED:
1. Stop all feature work immediately
2. Schedule team meeting within 30 minutes
3. Investigate root cause (likely systemic issue)
4. No merges until green

Root Cause Candidates:
- Recent dependency update: rdf-rs 0.5 → 0.6
- Last 3 commits on main branch
- CI infrastructure changes
```

**Notification Channels:**
- PagerDuty: Page on-call
- Slack: `@channel` mention in `#incidents`
- Email: Executive summary to leadership
- CI dashboard: Red status badge, block merges

---

## Alert Configuration

### Threshold Tuning

| Team Size | Yellow Threshold | Red Threshold | Reasoning |
|-----------|------------------|---------------|-----------|
| 1-5 devs | 1-3 failures | 4+ failures | Small team, low noise |
| 6-15 devs | 1-5 failures | 6+ failures | Standard threshold |
| 16-30 devs | 1-8 failures | 9+ failures | Higher noise tolerance |
| 31+ devs | 1-10 failures | 11+ failures | Enterprise scale |

**Custom Tuning:**
```bash
# .github/workflows/andon.yml
YELLOW_THRESHOLD=5
RED_THRESHOLD=10  # Tune based on historical data
```

---

## Remediation Playbooks

### Yellow Andon Response

**Timeline:**
- **T+0 min:** Alert triggered
- **T+15 min:** Owner assigned
- **T+60 min:** Fix branch created
- **T+240 min:** Fix merged (4 hour SLA)

**Steps:**
1. **Identify owner:**
   ```bash
   git log --all --oneline -- path/to/failing/test.rs | head -5
   ```

2. **Create fix branch:**
   ```bash
   git checkout -b fix/yellow-andon-test-name
   ```

3. **Reproduce locally:**
   ```bash
   cargo test failing_test_name -- --nocapture
   ```

4. **Fix and verify:**
   ```bash
   cargo test failing_test_name  # Passes locally
   git push origin fix/yellow-andon-test-name
   ```

5. **Merge after CI green:**
   - Automated merge if CI passes
   - Manual review if complex fix

---

### Red Andon Response

**Timeline:**
- **T+0 min:** Alert triggered, page sent
- **T+10 min:** On-call acknowledges
- **T+30 min:** War room assembled
- **T+60 min:** Root cause identified
- **T+120 min:** Fix deployed or rollback completed

**Steps:**
1. **Immediate triage (T+0 to T+10):**
   ```bash
   # Check recent changes
   git log --oneline -20

   # Identify common failure pattern
   cargo test 2>&1 | grep "FAILED" | awk '{print $2}' | sort | uniq -c
   ```

2. **Root cause analysis (T+10 to T+30):**
   ```
   Common patterns:
   - All failures in same module → Dependency update?
   - Failures across modules → Infrastructure issue?
   - Failures after specific commit → Revert candidate?
   ```

3. **Decision point (T+30):**
   - **Revert:** If recent commit caused cascade
   - **Fix forward:** If root cause clear and fixable
   - **Infrastructure:** If CI runner issue, contact ops

4. **Execute fix (T+30 to T+120):**
   ```bash
   # Option A: Revert
   git revert abc123  # Revert breaking commit
   git push origin main

   # Option B: Fix forward
   # (Apply fix, test, merge)

   # Option C: Infrastructure
   # (Contact ops, switch CI runners)
   ```

5. **Verify resolution:**
   ```bash
   cargo test --all  # Must be 100% green
   ```

6. **Post-mortem (T+24 hours):**
   - What went wrong?
   - Why didn't we catch it earlier?
   - What process changes prevent recurrence?

---

## Escalation Matrix

| Scenario | Alert Level | Escalation Path | Example |
|----------|-------------|-----------------|---------|
| 1-2 failures, isolated | Yellow | Assign to contributor | Single test failure |
| 3-5 failures, related | Yellow | Tech lead reviews | Module-level issue |
| 6-10 failures | Red | On-call engineer paged | Dependency breakage |
| 11-20 failures | Red | Team meeting + leadership notification | Major API change |
| 21+ failures | Red | Incident commander assigned | Infrastructure outage |

---

## Notification Templates

### Slack (Yellow)

```json
{
  "text": "🟡 YELLOW ANDON: 3 test failures",
  "blocks": [
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Yellow Andon Alert*\n\n3 tests failing. Fix before next feature work.\n\n*Failed Tests:*\n• test_graph_export\n• test_ontology_validate\n• test_lifecycle_rollback\n\n<https://github.com/org/repo/actions/runs/123|View CI Run>"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": { "type": "plain_text", "text": "Assign to Me" },
          "url": "https://github.com/org/repo/issues/new"
        }
      ]
    }
  ]
}
```

### PagerDuty (Red)

```json
{
  "routing_key": "PAGERDUTY_KEY",
  "event_action": "trigger",
  "payload": {
    "summary": "🔴 RED ANDON: 23 test failures (CRITICAL)",
    "severity": "critical",
    "source": "GitHub Actions CI",
    "custom_details": {
      "failure_count": 23,
      "branch": "main",
      "commit": "abc123def",
      "failed_modules": ["graph", "ontology", "lifecycle"]
    }
  }
}
```

---

## Metrics & SLAs

| Metric | Target | Measurement |
|--------|--------|-------------|
| Yellow Andon MTTR | <4 hours | Time from alert to green |
| Red Andon MTTR | <2 hours | Time from alert to green or rollback |
| False positive rate | <5% | Flaky tests triggering alerts |
| Alert fatigue | <3 Yellow/day | Prevent desensitization |

---

## Related Resources

- [How-to: Implement Andon System](../how-to/implement-andon-system.md) - Setup guide
- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md) - Andon philosophy

---

**Last Updated:** 2025-11-18
