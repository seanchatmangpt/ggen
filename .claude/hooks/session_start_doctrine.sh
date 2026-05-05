#!/usr/bin/env bash
# Session Start Doctrine Injection
# Injects ETHOS constitutional rules into Claude session context

set -euo pipefail

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
EVIDENCE_LOG="${PROJECT_ROOT}/artifacts/ethos-validation/evidence.log"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Ensure evidence log directory exists
mkdir -p "$(dirname "${EVIDENCE_LOG}")"

# Output doctrine rules as JSON
cat <<'JSON_EOF'
[
  {
    "rule": "completion_states",
    "category": "constitutional",
    "text": "MODELED ≠ IMPLEMENTED ≠ INTEGRATED ≠ EVIDENCED ≠ CONFORMANT ≠ ATTESTED ≠ RELEASE_AUTHORIZED. These are distinct, non-skipable states. No state transition shortcuts allowed.",
    "enforcement": "plan_check",
    "non_negotiable": true
  },
  {
    "rule": "stop_not_done",
    "category": "constitutional",
    "text": "Stop ≠ Done. StopAttempt is a request, not a release authorization. Release requires explicit external attestation of conformance.",
    "enforcement": "release_gate",
    "non_negotiable": true
  },
  {
    "rule": "no_self_authored_conformance",
    "category": "constitutional",
    "text": "No self-authored conformance receipt. Conformance must be externally attested by independent verifier (ETHOS validation script, external auditor, or MCP server conformance check).",
    "enforcement": "conformance_gate",
    "non_negotiable": true
  },
  {
    "rule": "external_release_receipt",
    "category": "constitutional",
    "text": "Release requires external receipt from independent verifier. Self-attested release is forbidden. ReleaseGranted only valid when backed by external conformance receipt.",
    "enforcement": "release_gate",
    "non_negotiable": true
  },
  {
    "rule": "forbidden_release_path",
    "category": "constitutional",
    "text": "Forbidden path: StopAttempt → ReleaseGranted (without external conformance). This path violates ETHOS constitutional constraint and must be rejected by release gate.",
    "enforcement": "release_gate",
    "non_negotiable": true
  },
  {
    "rule": "evidence_required",
    "category": "constitutional",
    "text": "Every claim requires externalizable evidence (OTel span, test assertion, schema conformance). Self-attested claims without external proof are invalid.",
    "enforcement": "claim_validation",
    "non_negotiable": true
  },
  {
    "rule": "causal_validation",
    "category": "constitutional",
    "text": "Causality must be validated through process mining. Model ≠ Log is a first-class defect. Event-log truth trumps architectural diagrams.",
    "enforcement": "process_mining",
    "non_negotiable": true
  },
  {
    "rule": "tier_invariance",
    "category": "constitutional",
    "text": "ETHOS tier invariance: equivalence across tiers must be proven. Equivalence is trivially true when all tiers are unverified. Only 1/6 tiers currently verified.",
    "enforcement": "tier_validation",
    "non_negotiable": true
  }
]
JSON_EOF

# Log injection to evidence log for traceability
cat >> "${EVIDENCE_LOG}" <<LOG_EOF
### Doctrine Injection [${TIMESTAMP}]
Source: session_start_doctrine.sh
Rules injected: 8 constitutional constraints

Rules:
  - completion_states: MODELED ≠ IMPLEMENTED ≠ INTEGRATED ≠ EVIDENCED ≠ CONFORMANT ≠ ATTESTED ≠ RELEASE_AUTHORIZED
  - stop_not_done: Stop ≠ Done. StopAttempt is a request, not a release authorization
  - no_self_authored_conformance: No self-authored conformance receipt. Conformance must be externally attested
  - external_release_receipt: Release requires external receipt from independent verifier
  - forbidden_release_path: Forbidden path: StopAttempt → ReleaseGranted (without external conformance)
  - evidence_required: Every claim requires externalizable evidence
  - causal_validation: Causality must be validated through process mining
  - tier_invariance: ETHOS tier invariance: equivalence across tiers must be proven

LOG_EOF

exit 0
