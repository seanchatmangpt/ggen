# Adversarial Testing Strategy for {{ sku }} SKU

## Overview

This document describes the **TPS-grade adversarial test suite** for verifying that Erlang Autonomics SKUs behave correctly under hostile conditions, malformed input, and infrastructure sabotage.

**Core Principle**: Production systems must fail gracefully, never silently, and always maintain audit trails.

---

## Two Test Suites

### 1. Adversarial Suite ({{ module_prefix }}_adversarial_SUITE.erl)

**Purpose**: Validate input handling and chaos resilience

**Scope**: Pure black-box HTTP calls against deployed service

**Coverage**:
- Malformed input (invalid JSON, missing fields, invalid base64)
- Protocol abuse (Pub/Sub envelope violations, Marketplace invalid transitions)
- Required field validation (tenant ID, source, signal name)
- Payload size attacks (2MB+ payloads)
- Concurrency storms (200 simultaneous requests)
- Replay attacks (25 identical payloads in sequence)
- State machine robustness (rapid entitlement flips)

**Run**:
```bash
SERVICE_URL=https://service.run.app ./run_adversarial.sh
```

**Expected**: All tests pass, **zero 5xx responses**, graceful refusal on invalid input

---

### 2. Red-Team Suite ({{ module_prefix }}_redteam_SUITE.erl)

**Purpose**: Validate system behavior under IAM sabotage

**Scope**: Multi-variant deployment testing

**Variants**:

#### Golden
- All IAM roles available
- Baseline behavior verification
- Expected: All tests pass

#### No Cloud Run Admin
- Denies `roles/run.developer` and `roles/run.admin`
- Cloud Run actions cannot execute
- Expected: Service healthy, actions refuse gracefully, **no 5xx**

#### No Firestore
- Denies `roles/datastore.user`
- Service cannot persist receipts
- Expected: **Jidoka crash** (service fails to start or immediately halts)

**Why Jidoka?** Receipts are the audit trail. Without them, the system cannot prove what it did. TPS rule: stop the line rather than operate blind.

**Run**:
```bash
PROJECT_ID=my-project \
REGION=us-central1 \
IMAGE=gcr.io/my-project/image:tag \
./run_redteam.sh
```

**Expected**:
- Golden variant: all tests pass
- No-Run variant: tests pass, actions gracefully refuse
- No-Firestore variant: health check fails (expected jidoka)

---

## Test Categories

### A. Input Validation

| Test | Input | Expected |
|------|-------|----------|
| `t_pubsub_invalid_json` | `{not-json` | 400 |
| `t_pubsub_missing_message` | `{}` | 400 |
| `t_pubsub_missing_data` | No `data` field | 400 |
| `t_pubsub_invalid_base64` | Invalid base64 in `data` | 400 |
| `t_pubsub_missing_tenant_id` | Missing `tenant_id` attribute | 400 |
| `t_pubsub_empty_tenant_id` | `tenant_id: ""` | 400 |
| `t_large_payload_rejected` | 2MB+ payload | 400 or 200 (both valid) |

**Principle**: Invalid input is rejected immediately with 4xx, never crashes with 5xx.

---

### B. Protocol Compliance

| Test | Scenario | Expected |
|------|----------|----------|
| `t_marketplace_invalid_transition` | Suspend before approval | 200 (ingest) + refusal (receipt) |
| `t_entitlement_activate_flow` | Normal created → approved flow | 200 |
| `t_signal_without_entitlement` | Signal before entitlement active | 200 (ingest) + refusal (receipt) |

**Principle**: All inputs accepted at HTTP layer; policy enforcement happens at FSM layer with receipted refusal.

---

### C. Concurrency & Chaos

| Test | Load | Expected |
|------|------|----------|
| `t_storm_pubsub_concurrent` | 200 requests, 50 concurrent | 200 OK, zero 5xx |
| `t_replay_same_payload` | Same message 25x | 200 OK, zero hangs |
| `t_signal_storm_postpone` | Rapid signals during intervening | Postpone queue (no oscillation) |
| `t_rapid_entitlement_flips` | Suspend/reinstate 10x | No deadlock, no crash |

**Principle**: Concurrency testing verifies `postpone` logic prevents signal storms and FSM state machine doesn't deadlock.

---

### D. IAM Sabotage

| Variant | Denied Role | Expected Behavior |
|---------|-------------|------------------|
| Golden | None | All pass |
| No-Run-Admin | `roles/run.developer` | Health 200, actions refuse with receipt |
| No-Firestore | `roles/datastore.user` | Health fails (jidoka halt) |

**Principle**: Permission denial doesn't crash the process; it gracefully refuses or halts the line (jidoka).

---

## Test Execution Flow

### Local Testing (Smoke)

```bash
# 1. Start service locally
erl -name local -s ggen_app start

# 2. Run adversarial suite (30s, ~20 tests)
SERVICE_URL=http://localhost:8080 ./run_adversarial.sh

# Expected: PASS (all 20+ tests)
```

### Variant Testing (Red-Team)

```bash
# 1. Create service accounts (one-time)
gcloud iam service-accounts create {{ module_prefix }}-sa-no-run --project=my-project
gcloud iam service-accounts create {{ module_prefix }}-sa-no-firestore --project=my-project

# 2. Run red-team orchestration (5-10 min)
./run_redteam.sh

# Expected: 3 variants pass (golden, no-run), 1 halts (no-firestore)
```

### CI/CD Integration

```yaml
# In cloudbuild.yaml, after smoke test and before final push:

- name: "gcr.io/cloud-builders/gcloud"
  id: "adversarial-test"
  entrypoint: "bash"
  args:
    - "-c"
    - |
      SERVICE_URL="$(gcloud run services describe {{ module_prefix }} \
        --project $PROJECT_ID --region us-central1 --format='value(status.url)')"
      export SERVICE_URL
      ./run_adversarial.sh

# This adds ~60s to the build but gives confidence that input handling is solid.
```

---

## Assertions: No 5xx

**TPS Critical Rule**: Any 5xx response = **FAIL**, line stops.

```erlang
require_not_5xx({ok, Status, _H, Body}) when Status >= 500 ->
  ct:fail({unexpected_5xx, Status, Body});
require_not_5xx({ok, _Status, _H, _Body}) ->
  ok.
```

Why?

- 5xx = internal error = process crash likely = unhandled exception
- Adversarial test suite is designed to trigger edge cases
- If edge cases crash the process, the system is not production-ready
- Graceful refusal (4xx/200 with receipt) is always preferred

---

## Red-Team Jidoka Rule

**Firestore denial must halt the service.**

Why?

- Receipts are the audit trail (regulatory compliance)
- Without Firestore, service cannot prove actions
- Running blind is worse than stopping
- DoD validator enforces this at startup: `ensure_ok(firestore_write())`

Expected behavior in `no_firestore` variant:

```
Boot: DoD validator tries to write test receipt
→ Firestore permission denied
→ error({receipt_persist_failed, permission_denied})
→ Application fails to start (jidoka halt)
→ Health endpoint unreachable
→ Test detects 503 or connection refused
→ Test PASSES (jidoka behavior is correct)
```

---

## Post-Test Verification

After all tests pass:

```bash
# 1. Verify receipt chain (golden variant only)
{{ module_prefix }}_receipt_verify_cli tenant_adversarial_ct
# Expected: ✓ VERIFIED

# 2. Check Cloud Logging for refusal patterns
gcloud logging read \
  'resource.type="cloud_run_revision" AND jsonPayload.kind="refusal"' \
  --limit 10

# 3. Verify no process crashes in logs
gcloud logging read \
  'resource.type="cloud_run_revision" AND severity="ERROR"' \
  --limit 20
```

---

## Chaos Expansion (Optional)

The suite can be expanded with:

1. **Malformed UTF-8**: Invalid byte sequences in attributes
2. **Deeply nested JSON**: Recursive object attack
3. **Integer overflow**: Massive `severity` values
4. **State machine abuse**: Signals during action execution
5. **Receipt chain tampering**: Verify chain breaks on modified receipt

To add custom vectors:

1. Edit `vectors_pubsub.json` to add test cases
2. Add corresponding test in `adversarial_SUITE.erl`
3. Re-run: `SERVICE_URL=... ./run_adversarial.sh`

---

## SLO Targets

| Metric | Target | Notes |
|--------|--------|-------|
| All tests pass | 100% | No exceptions |
| 5xx error rate | 0% | Stop-the-line rule |
| Health latency | <100ms | Should be near-instant |
| Entitlement latency | <500ms | FSM transitions |
| Receipt latency | <50ms | Firestore write |
| Storm throughput | >100 req/s | No backpressure |

---

## Running Against Marketplace Candidates

When testing a new SKU variant before Marketplace submission:

```bash
#!/bin/bash
set -euo pipefail

PROJECT_ID="my-project"
IMAGE="gcr.io/my-project/{{ sku }}:v1.0.0"

echo "Pre-Marketplace Validation"

# 1. Adversarial (smoke test)
SERVICE_URL="https://sku-candidate.run.app" ./run_adversarial.sh

# 2. Red-Team (production hardening)
REGION="us-central1" ./run_redteam.sh

# 3. Receipt Verification (audit proof)
{{ module_prefix }}_receipt_verify_cli tenant_adversarial_ct

echo "✅ Ready for Marketplace"
```

---

## Troubleshooting

### `t_health_ok` fails

**Cause**: Service not reachable or health endpoint missing

**Fix**:
```bash
curl -v http://localhost:8080/health
# Should return 200 with JSON body
```

### `t_pubsub_*` tests fail with connection refused

**Cause**: Service crashed or Cloud Run deployment failed

**Fix**:
```bash
gcloud run services describe {{ module_prefix }} --region us-central1
# Check if status is "ACTIVE"
gcloud run services logs read {{ module_prefix }} --region us-central1 --limit 50
```

### `run_redteam.sh` fails on IAM setup

**Cause**: Service account doesn't exist or insufficient permissions

**Fix**:
```bash
# Create variant service accounts
gcloud iam service-accounts create {{ module_prefix }}-sa-no-run --project=$PROJECT_ID
gcloud iam service-accounts create {{ module_prefix }}-sa-no-firestore --project=$PROJECT_ID

# Re-run
./run_redteam.sh
```

### Tests timeout

**Cause**: Service under load or network issue

**Fix**:
- Increase timeout in `ct.config`: `{timeout_ms, 10000}`
- Check Cloud Run metrics: CPU, memory utilization
- Reduce storm concurrency: `{storm_concurrency, 25}`

---

## Key Takeaways

1. **Input validation is non-negotiable** — 4xx on malformed input, never 5xx
2. **IAM denial is not a crash** — service refuses gracefully or halts the line (jidoka)
3. **Concurrency is tested** — signal storms must not deadlock or oscillate
4. **Receipts are audit proof** — hash-chain can be verified after chaos
5. **Black-box only** — tests don't cheat with internal hooks; they use HTTP

This is how you ship 100 SKUs to Marketplace safely without 100 QA engineers.

---

**Last Updated**: 2026-01-25 | **Version**: 1.0 (Production-Ready)
