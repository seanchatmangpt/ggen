# Security Quick Reference Guide

**TL;DR:** All pricing calculations are cryptographically signed, immutably logged, and verified at every access point. No customer or employee can modify historical data without immediate detection.

---

## Core Principles

1. **Immutability:** Once created, no pricing receipt can be modified or deleted
2. **Authentication:** Every action requires cryptographic proof of identity
3. **Transparency:** Every operation logged with timestamp and actor ID
4. **Verification:** All claims (metrics, calculations, approvals) verified before acceptance
5. **Defense in Depth:** Multiple controls at each layer (application, database, infrastructure)

---

## Quick Facts

| Aspect | Answer |
|--------|--------|
| **Can customer modify their value?** | NO - metrics are validated against system records |
| **Can employee modify historical receipt?** | NO - database prevents UPDATE/DELETE on pricing_receipts |
| **Can someone delete audit logs?** | NO - append-only storage, impossible to delete |
| **Can formula be secretly changed?** | NO - code review enforced, hash verified |
| **Can employee approve fraudulent override?** | NO - requires multi-approval chain + customer confirmation |
| **Can data leak to competitors?** | NO - row-level security isolates customers |
| **Can hacker forge receipt?** | NO - requires system private key (in secure storage) |
| **If tampering detected, can we prove it?** | YES - cryptographic receipts + audit trail |
| **Can we recover from fraud?** | YES - complete audit trail for forensics |
| **Is this insurable?** | YES - E&O + Cyber + Fidelity Bond coverage |

---

## Attack Prevention Summary

### Attack: Customer Disputes Pricing
**Attacker Goal:** Claim calculation was wrong to get refund
**Prevention:** Pricing receipt includes immutable input snapshot + system signature
**Detection:** Recalculate and compare; any mismatch triggers alert
**Resolution:** Show customer cryptographically signed receipt proving calculation

### Attack: Insider Modifies Receipt
**Attacker Goal:** Change $50K receipt to $0 to hide theft
**Prevention:** Database prevents UPDATE/DELETE; hash chain detects tampering
**Detection:** Receipt verification fails; audit log shows gap
**Resolution:** Incident response team investigates; law enforcement if criminal

### Attack: Employee Approves Fraud
**Attacker Goal:** Override pricing for bribed customer
**Prevention:** Multi-approval chain; customer must confirm
**Detection:** Pattern analysis flags unusual overrides
**Resolution:** Employee terminated; customer charged correct amount

### Attack: Hacker Injects Formula
**Attacker Goal:** Underreport all customer values by 50%
**Prevention:** Code review enforced; formula hash verified; staged rollout
**Detection:** Recalculation audit shows drift; monitoring alerts
**Resolution:** Rollback to prior formula; investigate code access logs

### Attack: DBA Deletes Audit Logs
**Attacker Goal:** Hide evidence of theft
**Prevention:** Append-only storage; hash chain makes gap detectable
**Detection:** Automatic verification detects missing entries
**Resolution:** Restore from monthly backup; identify tampering timestamp

---

## Control Summary

### Layer 1: Input Validation
- Metrics validated against system data (Salesforce, Stripe, etc)
- Metrics signed by customer (attestation)
- Oversized/malformed inputs rejected

### Layer 2: Calculation
- Formula version verified (hash check)
- Calculation result signed by system
- Calculation logged immediately

### Layer 3: Output
- Receipt generated with cryptographic proof
- Receipt hash linked to previous receipt (chain)
- Receipt stored in immutable table (no UPDATE/DELETE)

### Layer 4: Storage
- Append-only database table (prevents modification)
- Cryptographic signatures on every entry
- Hourly backup replication (3 geographic locations)

### Layer 5: Access
- Row-level security (customers only see own data)
- mTLS authentication (certificate-based)
- API key signing (HMAC-SHA256)
- Rate limiting (10 submissions/day per customer)

### Layer 6: Audit
- Every action logged immutably
- Audit entries cryptographically signed
- Hash chain detects tampering
- Monthly export to immutable storage

### Layer 7: Monitoring
- Real-time anomaly detection
- Pattern analysis for fraudulent behavior
- Automatic alert escalation
- 24/7 incident response

---

## Most Common Fraud Scenarios & Responses

### Scenario 1: "Your calculation was wrong, refund me $50K"

**Response Protocol:**
1. Retrieve customer's pricing receipt
2. Verify receipt signature (proves authenticity)
3. Recalculate using stored metrics
4. Compare recalculated vs stored value
5. If match: Show customer signed receipt, explain calculation
6. If mismatch: Investigate calculation error, issue credit if warranted

**Outcome:** Customer cannot dispute signed receipt; we have cryptographic proof

---

### Scenario 2: "I dispute the metrics you recorded"

**Response Protocol:**
1. Show customer immutable snapshot from receipt
2. Cross-check against system records (Salesforce, Stripe, etc)
3. If system records match snapshot: Customer is mistaken
4. If system records differ: Investigate data collection error
5. If customer submitted false data: They admitted to fraud

**Outcome:** Either customer is wrong or they submitted false metrics (provable)

---

### Scenario 3: "My competitor got a better rate"

**Response Protocol:**
1. Explain value-indexed pricing model
2. Show competitor's pricing receipt (if authorized)
3. Explain difference in business metrics
4. Offer to recalculate if they dispute their metrics
5. Escalate to Sales if contract negotiation needed

**Outcome:** Customers can verify pricing is fair and transparent

---

### Scenario 4: "We found a $500K calculation error"

**Response Protocol:**
1. IMMEDIATE: Investigate historical receipts
2. Identify which customers affected
3. Determine root cause (code bug, data error, etc)
4. Calculate total financial impact
5. Notify insurance carrier within 24 hours
6. Issue credits if error was our fault
7. Document remediation in audit log

**Outcome:** Contained damage with full audit trail for insurance claim

---

## Key Metrics to Monitor

Monitor these metrics via PagerDuty/Datadog:

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| Receipt generation latency P99 | <500ms | >1s |
| Receipt verification latency | <100ms | >500ms |
| Calculation accuracy (sample audit) | 100% | <99.9% |
| Dispute rate | <1% | >5% |
| Override requests/month | <10/employee | >20 |
| Unauthorized access attempts | 0 | >5/hour |
| Audit log gaps | 0 | >0 |
| Formula changes | <1/month | >5 |

---

## Incident Response Flowchart

```
Fraud Suspected
      ↓
STEP 1: Preserve Evidence
├─ DO NOT shut down systems
├─ Snapshot database state
├─ Preserve logs
└─ Lock affected accounts
      ↓
STEP 2: Alert Leadership (0-15 mins)
├─ Page on-call security engineer
├─ Notify CSO + CFO + General Counsel
└─ Page incident commander
      ↓
STEP 3: Contain (0-60 mins)
├─ Disable fraudulent accounts
├─ Pause affected transactions
├─ Notify customers (if revenue impact > $100K)
└─ Engage external forensics firm
      ↓
STEP 4: Investigate (Days 1-7)
├─ Timeline reconstruction
├─ Actor behavior analysis
├─ Root cause determination
├─ Financial impact calculation
└─ Evidence collection
      ↓
STEP 5: Remediate (Days 1-30)
├─ Issue credits if warranted
├─ File incident report (if required)
├─ Fix root cause (code/process)
├─ Implement prevention
└─ Notify insurance carrier
      ↓
STEP 6: Post-Mortem (Days 7-14)
├─ Document lessons learned
├─ Update security controls
├─ Train team on prevention
└─ Close incident ticket
```

---

## Regulatory Compliance

### Requirements Met

| Requirement | How Met |
|------------|---------|
| **Data Integrity** | Cryptographic signatures + hash chains |
| **Audit Trail** | Append-only log + monthly export |
| **Access Control** | Row-level security + role-based permissions |
| **Authentication** | mTLS + API key signing |
| **Encryption** | SHA-256 hashing + Ed25519 signatures |
| **Retention** | Monthly export to immutable storage |
| **Incident Response** | 6-step protocol + forensics plan |
| **Insurance** | E&O + Cyber + Fidelity Bond |

### External Audits

- **Annual:** SOC 2 Type II (Controls: CC6.1, CC7.2, CC7.5)
- **Bi-Annual:** Penetration testing
- **Monthly:** Audit log integrity verification
- **Quarterly:** Anomaly detection review

---

## Emergency Procedures

### If Fraud is Detected
1. **STOP:** Page on-call engineer immediately
2. **PRESERVE:** Don't shut down systems; snapshot state
3. **ISOLATE:** Disable compromised accounts
4. **NOTIFY:** Alert CSO + CFO within 15 minutes
5. **INVESTIGATE:** Engage forensics team within 4 hours

### If Audit Chain is Broken
1. **ALERT:** PagerDuty critical alert
2. **INVESTIGATE:** Determine what was tampered
3. **RESTORE:** Load backup from previous day
4. **VERIFY:** Re-verify chain integrity
5. **REPORT:** Document in incident report

### If Customer Disputes Charge
1. **RETRIEVE:** Get pricing receipt
2. **VERIFY:** Check receipt signature
3. **RECALCULATE:** Recompute value from metrics
4. **COMPARE:** Check recalculated vs stored
5. **RESPOND:** Provide proof to customer

---

## Password/Key Management

**DO NOT:**
- Hardcode API keys in code
- Share system keys in Slack
- Log sensitive values
- Store keys in Git
- Bypass security controls for "expedience"

**DO:**
- Store keys in AWS KMS (or similar)
- Rotate keys quarterly
- Audit key access monthly
- Use mTLS for server-to-server auth
- Require code review for all security changes

---

## Questions?

**Security Questions:** security@example.com
**Incident Response:** oncall-security@example.com
**Compliance Questions:** compliance@example.com
**Technical Questions:** security-team@example.com

---

**Last Updated:** 2026-01-25
**Classification:** CONFIDENTIAL - FINANCIAL SECURITY
