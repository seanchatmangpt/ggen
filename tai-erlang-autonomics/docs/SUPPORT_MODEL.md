# TAI Autonomics Support Model

**Status**: Production Ready
**Phase**: Phase 1 (MCP-only, evaluation mode)
**Last Updated**: 2026-01-26

---

## Overview

TAI Autonomics does **not** offer human-operated support tickets, phone lines, or live chat. Instead, support is delivered exclusively by **MCP agents** following deterministic, stop-the-line principles.

**One-liner**: MCP-only TCPS support with deterministic remediation, explicit refusals, and auditable receipts. No human support channel.

---

## What You Get

### Deterministic Remediation
The same issue produces the same diagnosis and the same fix. No ad-hoc "try this" variations. Every action is governed by versioned runbooks and decision trees.

### Stop-the-Line Safety
If an action is unsafe or preconditions are missing, the system **refuses** and emits:
- Explicit reason for refusal
- Next lawful step to unblock progress
- Reference receipt for your audit trail

Example refusal:
```
REFUSED: Cannot apply feature license; tenant entitlement gate failed.
REASON: Tenant quota exceeded for SKU "autonomic-v1".
NEXT STEP: Upgrade tenant tier or contact sales@taiautonomics.io
RECEIPT: [uuid]
```

### Evidence-First Support
Every decision, action, and refusal produces a **hash-chained receipt** suitable for:
- Audit trails (SOC2, ISO, NIST inquiries)
- Compliance evidence bundles
- Incident investigations
- Cost allocation

### No Back-and-Forth
The system collects all evidence automatically. You describe the issue; the system outputs:
- Diagnosis runbook
- Remediation commands
- Recovery confirmation
- Exported receipt chain

### Observable, Not Human-Operated
Humans can observe outputs via MCP clients; MCP agents execute remediation under entitlement and policy gates. No human interprets tickets or makes ad-hoc decisions.

---

## What You Should NOT Expect

- **Live chat or email support tickets** - Not offered
- **Personalized tuning consultation** - Not offered
- **Emergency hotline** - Not offered (use incident response runbooks instead)
- **Account manager for small issues** - Not offered
- **Custom workarounds or exceptions** - Not offered; all remediations follow standard paths

---

## Support Workflow

### Issue Occurs
You observe a failure, warning, or unexpected behavior.

### Collect Evidence
- System health check via MCP: `taiea.health.check`
- Entitlement status via MCP: `taiea.entitlement.apply_event` (with issue context)
- Receipt chain validation via MCP: `taiea.receipts.verify_chain`
- Support model reference via MCP: `taiea.support.model`

### MCP Agent Responds
The agent processes your evidence and emits:
- **Diagnosis** (root cause, reference documentation)
- **Remediation runbook** (step-by-step commands)
- **Safety gate validation** (preconditions, constraints)
- **Receipt proof** (hash-chained ledger entry)

### Execute Remediation
You execute the runbook. MCP tools track outcomes and emit completion receipts.

### Escalation (If Needed)
If issue persists after Phase 1 remediation, you may upgrade to Phase 2 (insured/contracted support with human engineering available).

---

## Receipt Format

Every action produces a receipt with:

```json
{
  "id": "receipt-uuid-v4",
  "timestamp": "2026-01-26T14:30:45.123Z",
  "event": "action_name",
  "status": "success|error|refused",
  "tool": "taiea.support.action",
  "message": "Human-readable summary",
  "metadata": {
    "tenant_id": "...",
    "receipt_chain_hash": "sha256:...",
    "action_duration_ms": 1234,
    "preconditions_met": true,
    "gates_passed": ["entitlement_gate", "quota_gate", "safety_gate"]
  }
}
```

### Receipt Export
To export your receipt chain for audit:

```bash
MCP Tool: taiea.receipts.verify_chain
Input: {
  "tenant_id": "your-tenant-id",
  "receipt_id": "receipt-uuid" (optional—all if omitted)
}
Output: Exported JSON or CSV suitable for audit bundles
```

### Example Receipt
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "timestamp": "2026-01-26T14:35:22.456Z",
  "event": "feature_activation",
  "status": "success",
  "tool": "taiea.entitlement.apply_event",
  "message": "Feature 'report-gen' activated for tenant 'acme-corp'",
  "metadata": {
    "tenant_id": "acme-corp",
    "feature": "report-gen",
    "previous_state": "inactive",
    "new_state": "active",
    "receipt_chain_hash": "sha256:abc123...",
    "gates_passed": ["entitlement", "quota", "security"],
    "action_duration_ms": 89
  }
}
```

---

## Phase 1 Limitations

### Evaluation Mode
TAI Autonomics is currently in **Phase 1 (evaluation mode)**. This means:

- **No SLA commitment** - Best effort response times
- **Stdout receipts only** - Receipts logged to stdout; no Firestore persistence
- **No multi-tenant billing** - Evaluation tier only; no production billing
- **Limited scale** - Single Cloud Run instance; no auto-scaling yet
- **No insurance** - Support delivery not insured (Phase 2 adds insurance)
- **No 24/7 ops** - Business hours best effort during Phase 1

### Intended For
- Early access partners
- Proof-of-concept evaluations
- Integration testing
- Feature validation
- Recipe and runbook validation

### Not Intended For
- Production customer deployments
- SLA-critical workloads
- Multi-tenant billing scenarios
- Compliance-mandated support

---

## Transition to Phase 2

### When Phase 2 Launches (Q2 2026)
- **Insurance-backed support** with contractual SLA commitments
- **Firestore persistence** for receipt ledgers
- **Multi-tenant billing** with audit trail
- **Dedicated ops team** with on-call rotation
- **24/7 availability** option for premium tiers
- **Root cause analysis** recipes added to standard work

### Upgrade Path
1. You reach out to sales@taiautonomics.io during your Phase 1 evaluation
2. TAI team proposes Phase 2 contract with SLA tier
3. Execution includes Firestore migration and ops handoff
4. Full support doctrine deployed; MCP tools gain new gates and strategies

---

## Support Tiers (When Available in Phase 2)

| Tier | Response Time | Uptime SLA | Features | Cost |
|------|---------------|-----------|----------|------|
| **Evaluation** (Phase 1) | Best effort | None | MCP tools, receipts, stdout logging | Free |
| **Standard** (Phase 2) | 2 hours | 99.5% | Firestore receipts, ops team, runbook gen | TBD |
| **Professional** (Phase 2) | 1 hour | 99.9% | All Standard + priority queue, 12h RCA | TBD |
| **Enterprise** (Phase 2) | 15 min | 99.99% | All Professional + 24/7 on-call, custom recipes | TBD |

---

## MCP Tools Reference

### `taiea.health.check`
Check system health and dependencies.

**Input**: `{}`
**Output**: Health status, all subsystem checks, uptime, node info
**Receipt**: Yes (success or error)

### `taiea.entitlement.apply_event`
Apply entitlement state changes (activate feature, upgrade tier, etc.).

**Input**:
```json
{
  "tenant_id": "your-tenant-id",
  "event_type": "feature_activate|tier_upgrade|quota_increase",
  "event_data": {
    "feature": "report-gen",
    "new_tier": "professional",
    "reason": "User requested"
  }
}
```
**Output**: Decision result, new state, gate status
**Receipt**: Yes (success, refused, or error)

### `taiea.receipts.verify_chain`
Verify receipt chain integrity and export for audit.

**Input**:
```json
{
  "tenant_id": "your-tenant-id",
  "receipt_id": "uuid-optional"
}
```
**Output**: Chain hash status, validity, export bundle
**Receipt**: Yes (verification status)

### `taiea.support.model`
Retrieve support model configuration and SLA details.

**Input**: `{}`
**Output**: Support model definition, tiers, gates, SLA config
**Receipt**: Yes (always success)

---

## Known Limitations & Workarounds

### Issue: Receipt Persistence
**Phase 1**: Receipts logged to stdout only.
**Workaround**: Export receipts to your audit system via scripts.
**Phase 2**: Firestore persistence; automatic retention/export.

### Issue: No Multi-Tenant Billing
**Phase 1**: Single evaluation tenant.
**Workaround**: Separate deployments per customer for PoC.
**Phase 2**: Native multi-tenant billing with per-tenant quotas.

### Issue: No 24/7 Ops
**Phase 1**: Business hours best effort.
**Workaround**: Use incident response runbooks and auto-remediation.
**Phase 2**: On-call ops team with escalation paths.

### Issue: Single Instance (No Auto-Scaling)
**Phase 1**: Manual scaling.
**Workaround**: Monitor load; contact TAI for instance resize.
**Phase 2**: Auto-scaling policies and load-based provisioning.

---

## Compliance & Audit

### Audit-Ready Receipts
All support actions produce receipts suitable for:
- **SOC2 Type II** - Control evidence bundles
- **ISO 27001** - Access control and change logs
- **NIST 800-53** - Security control verification
- **HIPAA** - Audit trail for protected health information (if applicable)
- **GDPR** - Data access and retention logs

### Decommissioning
When you end your evaluation:
1. Request decommission via support MCP tool
2. System generates retention/deletion proof
3. Receipts exported to your audit system
4. Attestation signed with TAI key
5. Completion receipt emitted and exported

---

## Contact & Escalation

### For Sales & Licensing
**Email**: sales@taiautonomics.io
**Purpose**: Upgrade to Phase 2, licensing questions, special contracts
**Response Time (Phase 1)**: Business hours, best effort

### For Technical Issues (Phase 1)
**Method**: MCP tools only
**Tools**: `taiea.health.check`, `taiea.entitlement.apply_event`, `taiea.receipts.verify_chain`
**Response Format**: Runbook, refusal reason, or next step
**No email address**: All technical inquiries processed via MCP

### For Urgent Issues
**Phase 1**: No urgent support offered; use auto-remediation playbooks.
**Phase 2**: Escalation path available with Professional+ tiers.

---

## FAQ

**Q: Can I talk to a human?**
A: Not in Phase 1. Phase 2 (Q2 2026) includes human engineering support for Professional and Enterprise tiers.

**Q: What if my issue doesn't match a known remediation?**
A: The system refuses with explicit reason and next step. You may upgrade to Phase 2 to access custom recipes from the ops team.

**Q: How are receipts verified?**
A: Each receipt is hash-chained to the prior receipt and signed with TAI's key. Use `taiea.receipts.verify_chain` to verify the chain is unbroken.

**Q: Can I get a refund if support doesn't work?**
A: Phase 1 is free evaluation. Phase 2 contracts include SLA-backed refund terms.

**Q: How long will Phase 1 last?**
A: Estimated Q1 2026 end date; Phase 2 launches Q2 2026. You'll be notified 30 days before Phase 1 sunsetting.

---

## Document Control

| Field | Value |
|-------|-------|
| **Version** | 1.0 |
| **Audience** | Customers, prospects, integrators |
| **Distribution** | Public / Marketplace listings |
| **Last Review** | 2026-01-26 |
| **Next Review** | 2026-04-01 (Phase 2 launch prep) |
