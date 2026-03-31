# TAI Autonomics Phase 2: Insurance & Contracts
## Quick Reference Guide

**Full Document**: `/Users/sac/ggen/tai-erlang-autonomics/PHASE_2_INSURANCE_AND_CONTRACTS.md` (2,060 lines)

---

## KEY DECISIONS (For Immediate Action)

### 1. Insurance Procurement (Week 1-2)
- **Policy**: Professional Liability (Tech E&O) with Contractual Liability endorsement
- **Limits**: $2M per occurrence / $2M aggregate
- **Deductible**: $2,500
- **Cost**: ~$1,050/year ($88/month)
- **Broker**: Marsh (2-week turnaround) OR Willis (deeper relationships)
- **Effective Date**: Feb 15, 2026 (or as soon as possible)

### 2. Insurance Carriers (Recommended)
| Carrier | Strength | Timeline |
|---------|----------|----------|
| **Hartford** | Tech-native; fast underwriting | 1-2 weeks |
| **AIG** | Proven tech market; deep appetite | 2-3 weeks |
| **Travelers/Chubb** | Integrated cyber + E&O | 2-3 weeks |

**Action**: Contact Marsh or Willis TODAY requesting quotes for $2M coverage.

---

## REVENUE RECOGNITION (ASC 606)

### The Critical Boundary

**Evaluation Agreement** (No Revenue):
- Duration: 30-90 days
- Cost to customer: $0
- Warranty: AS-IS, no warranty
- Performance obligation: Best effort (no SLA)
- Revenue recognized: $0 (NOT a contract)

**Production Agreement (MSA)** (Revenue-Generating):
- Duration: 12 months (auto-renew)
- Cost to customer: $X/month
- Warranty: Merchantability implied; SLA committed
- Performance obligation: Defined SLA (99.5% uptime, etc.)
- Revenue recognized: $X/month (when insurance verified)

### Revenue Recognition Trigger
```
Revenue is NOT recognized until:
1. MSA signed by both parties, AND
2. Insurance certificate verified by customer (phone call to broker)
3. Audit trail entry: insurance_audit_trail.verification_status = VALID
```

---

## MASTER SERVICE AGREEMENT (MSA) — Key Clauses

### Liability Cap (Section 7)
```
LESSER OF:
(a) $2,000,000 (policy limit), OR
(b) Fees paid in preceding 12 months × 2

EXAMPLE:
Customer pays $5,000/month ($60,000/year)
Liability cap = $60,000 × 2 = $120,000

This equals insurance policy limit and is fully covered.
```

### Indemnification (Section 6)
- **TAI Indemnifies Customer**: IP infringement claims
- **Customer Indemnifies TAI**: Misuse, third-party claims from customer's use
- **Both backed by**: $2M E&O insurance

### SLA (Section 5.2)
- Uptime target: 99.5% monthly
- Response time: <500ms median, <2000ms p99
- Support response: <24 hours for P1 issues
- Remedy: Service credits (5%, 15%, or 30% of monthly fee)

---

## DATA PROCESSING ADDENDUM (DPA)

**Mandatory If**: Customer uploads ANY personal data (email, phone, IP address, etc.)

**Covers**:
- GDPR (EU residents): Article 28 data processor obligations
- CCPA (California residents): Service provider restrictions
- Data subject rights: Access, deletion, portability
- Breach notification: 72-hour requirement
- Sub-processors: Google Cloud, Datadog, etc.

**Cost**: Included in MSA (no additional fee)

---

## AUDIT TRAIL SYSTEM (Insurance Verification Proof)

### Why It Matters
Proves to auditors and customers that insurance was verified BEFORE production go-live.

### Three Components

1. **Insurance Certificate (ACORD 25)**
   - PDF with digital signature
   - Contains: Policy number, limits, effective/expiration dates
   - Customer verifies by calling broker directly

2. **Cryptographic Hash**
   - SHA-256 of certificate
   - Stored in audit trail database
   - Prevents tampering/forgery

3. **Audit Trail Database**
   Tables: `insurance_audit_trail`, `revenue_recognition`
   - Immutable logs (append-only)
   - Daily expiration monitoring
   - Monthly compliance reports

### Chain of Custody (Timeline)
```
Day 1: Evaluation agreement signed
       ↓ (90 days later)
Day 90: Customer decides to convert
        ↓
Day 91: MSA signed by both parties (status: "pending insurance")
        ↓
Day 92: Insurance certificate sent to customer
        ↓
Day 93: Customer verifies insurance with broker
        ↓
Day 94: MSA countersigned; insurance verified
        ↓
Day 95: PRODUCTION GO-LIVE (revenue recognition begins)
```

---

## INSURANCE LAPSE PROTOCOL

**If TAI's insurance lapses**:
1. Detection: Automated daily check
2. Customer notification: Email within 24 hours
3. Cure window: 30 days to restore
4. If not restored: Customer can terminate + receive pro-rata refund

**Prevention**:
- Auto-renew 90 days before expiration
- CEO + CFO alerts at 120, 60, 30 days
- Monthly compliance check in audit trail DB

---

## 90-DAY EXECUTION PLAN

| Week | Milestone | Owner |
|------|-----------|-------|
| **W1** | Insurance quotes; broker selection | CFO |
| **W2** | Insurance underwriting; MSA drafting | Legal |
| **W3** | ACORD certificate received; audit trail DB setup | Compliance |
| **W4** | First customer conversion initiated | CSM |
| **W5** | First customer MSA signed; go-live | CSM + Ops |
| **W6-8** | 3-5 customers converting to production | CSM |
| **W9-10** | Series A materials (insurance section) complete | Finance |
| **W11-13** | Monitoring; scaling; D&O insurance (for Series A) | All |

**Critical Dates**:
- Week 1 (Jan 26): Start broker outreach TODAY
- Week 2 (Feb 2): Insurance decision from underwriter
- Week 3 (Feb 9): ACORD certificate received
- Week 4 (Feb 16): First customer conversion
- Week 13 (Apr 20): Series A fundraising materials ready

---

## WHAT GOES WHERE (File Organization)

```
/tai-erlang-autonomics/
├── PHASE_2_INSURANCE_AND_CONTRACTS.md (MAIN DOCUMENT, 2,060 lines)
│   ├── Part 1: Insurance Policy Requirements (policy types, limits, carriers, costs)
│   ├── Part 2: ASC 606 Revenue Recognition (eval vs. production, 5-step model)
│   ├── Part 3: MSA/Contract Language (templates + sample contracts)
│   ├── Part 4: Audit Trail & Insurance Verification (cryptographic proof)
│   ├── Part 5: Regulatory Compliance (SEC, state, HIPAA/HITRUST)
│   ├── Part 6: Operational Handoff (CSM playbook, customer communications)
│   ├── Part 7: Risk Matrix (12 failure modes + mitigation)
│   ├── Part 8: 90-Day Execution Plan (week-by-week timeline)
│   └── Part 9: Critical Success Factors & References
│
├── PHASE_2_INSURANCE_QUICK_REFERENCE.md (THIS FILE)
│
├── (TO CREATE NEXT - Not in scope):
│   ├── Insurance certificate PDF (ACORD 25) - from broker
│   ├── Signed MSA (per customer) - from legal/DocuSign
│   ├── Signed DPA (if applicable) - from legal/customer
│   └── Audit trail database schema.sql - from engineering
```

---

## RISK MITIGATION SUMMARY

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|-----------|
| Insurance lapse mid-contract | Low (10%) | CRITICAL | Auto-renew 90 days early |
| Revenue recognized without insurance | Medium (30%) | HIGH | Audit trail DB blocks recognition |
| Customer data breach (GDPR violation) | Medium (25%) | CRITICAL | DPA mandatory; encryption AES-256 |
| Insurance claim exceeds policy limit | Low (5%) | HIGH | Liability cap = 2x annual fees |
| Customer contract dispute (IP infringement) | Low (8%) | HIGH | Indemnification + E&O insurance |
| Customer sues for defects | Low (10%) | HIGH | SLA + liability cap; E&O insurance |
| Insurance certificate forged | Low (2%) | CRITICAL | Cryptographic hash verification |
| Customer doesn't verify insurance | Medium (40%) | MEDIUM | Automated email reminder system |
| Contractor discloses trade secrets | Low (5%) | CRITICAL | Confidentiality clause (5-year) |
| Unauthorized customer use | Medium (25%) | MEDIUM | License restrictions; audit rights |
| Customer never converts (eval-only) | Medium (35%) | LOW | Expected; manage expectations |
| Healthcare customer without HIPAA BAA | Low (5%) | HIGH | No healthcare customers in Phase 2 |

---

## IMMEDIATE ACTION ITEMS (This Week)

- [ ] **Monday (Jan 27)**: Email Marsh/Willis/Aon for $2M E&O quotes
- [ ] **Tuesday (Jan 28)**: Select broker based on timeline/cost
- [ ] **Wednesday (Jan 29)**: Call broker; provide business information
- [ ] **Thursday (Jan 30)**: Respond to underwriting questions within 24 hours
- [ ] **Friday (Jan 31)**: Confirm effective date (target Feb 15)
- [ ] **Week 2**: Legal begins MSA template customization
- [ ] **Week 3**: ACORD certificate received; audit trail DB operational

---

## KEY CONTACTS & TEMPLATES

### Insurance Brokers (Call Them Today)
- **Willis Towers Watson**: Tech E&O team
- **Aon**: Tech & Specialty Insurance
- **Marsh**: Technology & Professional Services

### Legal/Contract Templates
All in `/PHASE_2_INSURANCE_AND_CONTRACTS.md`:
- Section 3.1: Master Service Agreement (full template)
- Section 3.3: Indemnification clauses
- Section 3.4: Data Processing Addendum (GDPR/CCPA)
- Section 6.1: Eval-to-Production transition clause

### Audit Trail Database
- Section 4.2: SQL schema for `insurance_audit_trail` and `revenue_recognition` tables
- Immutable logs; daily monitoring; monthly reports

---

## SUCCESS METRICS (End of Phase 2, Week 13)

✓ Insurance active ($2M limit, zero claims)
✓ 5-6 customers in production (insured)
✓ Recurring revenue: $[X]/month (only from verified customers)
✓ Zero audit findings on insurance controls
✓ Audit trail database: 100% data integrity
✓ Series A materials complete (insurance section)
✓ Zero revenue recognized without insurance verification
✓ Repeatable process: 2-hour MSA customization, 5-day conversion

---

**Document Status**: Complete and ready for implementation
**Owner**: CFO (insurance), General Counsel (contracts), CSM (customer handoff)
**Review Frequency**: After first 3 conversions (Week 5-6); quarterly thereafter
**Next Step**: Forward to CFO for immediate broker outreach
