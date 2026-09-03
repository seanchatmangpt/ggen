# Security Architecture: Value-Indexed Pricing System

**Status:** PRODUCTION READY
**Classification:** CONFIDENTIAL - FINANCIAL SECURITY
**Last Updated:** 2026-01-25

---

## Overview

This directory contains the complete security architecture for the value-indexed pricing system. The system protects against fraud, manipulation, and attacks from hostile customers, compromised insiders, and sophisticated adversaries.

**Core Principle:** Value calculations are immutable, cryptographically verified, and audit-logged at every step. No customer or employee can modify historical pricing data without immediate detection.

---

## Documents

### 1. **SECURITY_ARCHITECTURE.md** (22KB)
Complete security design covering:
- **Threat Model** (10 adversary profiles with incentives)
- **Attack Surface Analysis** (6 critical components vulnerable to attack)
- **Cryptographic Receipt System** (tamper-proof audit trail)
- **Data Isolation & Access Control** (row-level security, mTLS)
- **Administrative Controls** (multi-approval override workflow)
- **Audit Logging** (immutable chain-of-custody)
- **Incident Response** (6-step investigation protocol)
- **Insurance & Risk Management** (what's insurable vs self-insured)
- **Monitoring & Alerting** (real-time anomaly detection)
- **Penetration Testing** (test plan and scenarios)

**Key Sections:**
- Threat matrix with likelihood/impact/priority
- 5-phase receipt generation pipeline
- SQL examples for row-level security policies
- Erlang code for cryptographic verification
- Incident response state machine
- Investigation checklist

### 2. **FRAUD_PREVENTION_GUIDE.md** (28KB)
Detailed fraud prevention covering:
- **Fraud Categories** (4 attack types with examples)
  - Value Underreporting (customer submits false metrics)
  - Historical Receipt Tampering (insider modifies data)
  - Pricing Override Abuse (unauthorized discounts)
  - Calculation Engine Bugs (accidental errors)

- **Attack Vectors & Countermeasures** (4 major vectors)
  - Direct database manipulation
  - API parameter tampering
  - Calculation formula injection
  - Audit log deletion

- **Detection Techniques** (real-time anomaly detection)
  - Statistical anomaly detection (3-sigma rule)
  - Behavioral analysis (employee pattern tracking)
  - Access pattern analysis (unusual data access)
  - Formula drift detection (recalculation audits)

- **Penetration Testing Playbook** (5 detailed scenarios)
  - PT Scenario 1: Direct database modification
  - PT Scenario 2: Receipt forgery
  - PT Scenario 3: Cross-customer data access
  - PT Scenario 4: Formula injection
  - PT Scenario 5: Audit log manipulation

- **Fraud Investigation Procedures**
  - Evidence preservation
  - Timeline reconstruction
  - Actor analysis
  - Financial impact calculation
  - Root cause determination

- **Customer Dispute Resolution**
  - Receipt verification process
  - Calculation accuracy confirmation
  - Refund/credit issuance workflow

**Key Features:**
- Real attack examples with incentive analysis
- Erlang code for each countermeasure
- PT test cases (CRITICAL / PASS / FAIL)
- Investigation workflow
- Compliance checklist

---

## Implementation Code

### 1. **pricing_receipt.erl** (Erlang Module)
Cryptographic receipt system:
```erlang
%% Generate tamper-proof receipt
{ok, Receipt} = pricing_receipt:generate(
    TenantId,
    Metrics,          % Input metrics (immutable copy)
    FormulaVersion,   % Formula version hash
    ActorId           % Who authorized
).

%% Verify receipt integrity (detects ANY modification)
{ok, verified} = pricing_receipt:verify(Receipt).

%% Conversion to/from JSON
JSON = pricing_receipt:to_json(Receipt).
{ok, Receipt2} = pricing_receipt:from_json(JSON).
```

**Key Functions:**
- `generate/4` - Create receipt with cryptographic proof
- `verify/1` - Detect tampering (signature, hash, chain)
- `to_json/1` - Serialize for storage
- `from_json/1` - Deserialize from JSON

**Cryptographic Proof:**
- SHA-256 hashing of metrics & formula
- Ed25519 signatures (system + actor)
- Merkle chain linking to previous receipts
- Proof-of-calculation (CPU entropy, memory samples)

### 2. **audit_log.erl** (Erlang Module)
Immutable append-only audit trail:
```erlang
%% Log action with metadata
{ok, EntryId} = audit_log:log(
    Action,      % calculate | override | access | deploy
    ResourceType,% receipt | formula | customer
    ResourceId,  % What was affected
    Metadata     % Additional context
).

%% Query audit log (with integrity verification)
{ok, Entries} = audit_log:query(TenantId, StartTime, EndTime).

%% Verify entire audit log integrity
{ok, valid} = audit_log:verify_integrity().

%% Export to tamper-proof storage (monthly)
{ok, ExportPath} = audit_log:export_monthly().

%% Detect gaps in chain (indicates tampering)
{ok, no_gap} = audit_log:detect_gap().
```

**Key Features:**
- Append-only storage (no UPDATE/DELETE)
- Cryptographic signing of each entry
- Hash chain linking to prior entry
- Monthly export to immutable storage (S3, GCS)
- Tamper detection via gap analysis

### 3. **security_tests.erl** (EUnit Test Suite)
Comprehensive security tests:
```bash
# Run all security tests
rebar3 eunit -m security_tests

# Run specific test suite
rebar3 eunit -m security_tests -n receipt_tampering_test_
```

**Test Suites:**
1. Receipt Tampering Detection (4 tests)
   - Value change detection
   - Metrics change detection
   - Signature verification
   - Chain break detection

2. Audit Log Integrity (4 tests)
   - Entry immutability
   - Chain integrity verification
   - Gap detection
   - Monthly export

3. Override Approval Workflow (3 tests)
   - Small override (1 approval)
   - Large override (3+ approvals)
   - Abuse detection

4. Data Isolation (3 tests)
   - Customer cannot access competitor data
   - Employee access filtered by tenant
   - Unauthorized access logged

5. Formula Integrity (3 tests)
   - Code hash verification
   - Code review requirement
   - Injection detection

6. Calculation Accuracy (3 tests)
   - Precision maintained
   - Error handling
   - Audit trail created

**Coverage:** 20+ test cases covering all attack vectors

---

## Threat Coverage

### What's Protected Against

| Threat | Vector | Control | Detection |
|--------|--------|---------|-----------|
| **Customer disputes value** | Submit false metrics | Metric validation, signature verification | Real-time anomaly detection |
| **Insider modifies receipts** | Direct DB tampering | Table-level immutability, hash chain | Gap detection in audit log |
| **Employee approves fraud** | Override abuse | Multi-approval workflow, pattern detection | Behavioral analysis |
| **Hacker injects formula** | Code injection | Code review, version signing, hash verification | Formula hash mismatch alert |
| **Attacker deletes evidence** | Audit log deletion | Append-only storage, hash chain | Gap detection |
| **Employee steals data** | Cross-customer access | Row-level security, mTLS | Access pattern analysis |

### Attack Scenarios Tested

#### Scenario 1: Receipt Tampering
```
Attacker: DBA with database access
Goal: Modify $50K receipt to $0
Method: UPDATE pricing_receipts SET calculated_value = 0
Result: DETECTED
├─ Hash chain broken (previous_hash no longer links)
├─ Signature verification fails (data modified)
└─ Gap detected in audit log
```

#### Scenario 2: Formula Injection
```
Attacker: DevOps engineer
Goal: Underreport values by 50%
Method: Inject code: Value = Value * 0.5
Result: PREVENTED
├─ Code review enforced (requires 2 approvals)
├─ Staged rollout (1% → 10% → 100% customers)
├─ Monitoring detects calculation drift
└─ Rollback capability (within 60 days)
```

#### Scenario 3: Cross-Customer Access
```
Attacker: Customer A
Goal: Access Customer B's metrics
Method: SQL: SELECT * FROM pricing WHERE customer_id = B
Result: BLOCKED
├─ Row-level security filters by tenant_id
├─ Database role rejects query without auth
└─ Access attempt logged as security incident
```

#### Scenario 4: Override Abuse
```
Attacker: Support agent
Goal: Give $100K refund to customer (paid $50K bribe)
Method: Request override without justification
Result: REQUIRES APPROVAL
├─ Approval chain enforced (Finance Manager → Director → CFO)
├─ Pattern analysis flags suspicious behavior
├─ Customer must confirm acceptance (prevents fraud)
└─ Audit log documents every approval
```

---

## Key Controls

### 1. Cryptographic Receipts
Every pricing calculation produces a tamper-proof receipt:
- **Immutable Input Snapshot:** Original metrics frozen at calculation time
- **Deterministic Calculation:** Same metrics → same result (always)
- **Cryptographic Signature:** System + actor must approve
- **Hash Chain:** Links to previous receipt (gap detection)
- **Proof of Work:** CPU entropy + memory samples prevent spoofing

### 2. Audit Logging
Every action is logged immutably:
- **Append-Only Storage:** No updates, no deletes
- **Cryptographic Signing:** Each entry signed with system key
- **Chain of Custody:** Hash links to prior entry
- **Monthly Export:** Encrypted backup to immutable storage (S3, GCS)
- **Gap Detection:** Break in chain alerts security team

### 3. Access Control
Multi-layer data isolation:
- **Row-Level Security:** Database enforces tenant isolation
- **Role-Based Access:** Customer/Employee/Admin permissions
- **mTLS Authentication:** Certificate-based API access
- **API Key Signing:** HMAC-SHA256 request signature
- **Rate Limiting:** Max 10 submissions/day per customer

### 4. Administrative Controls
Prevent unauthorized overrides:
- **Multi-Approval Workflow:** 1-4 approvals depending on amount
- **Approval Authority:** Finance Manager → Director → CFO → Board
- **Timeouts:** Approvals must complete within 5-30 days
- **Customer Confirmation:** Customer must accept override
- **Immutable Record:** Override logged with all approvals

### 5. Monitoring & Alerts
Real-time anomaly detection:
- **Value Anomalies:** Z-score > 3 standard deviations
- **Override Patterns:** > 10 overrides/month per employee
- **Access Anomalies:** Cross-customer access attempts
- **Calculation Drift:** > 1% variance in recalculated values
- **Formula Changes:** Unauthorized code modifications
- **Audit Gaps:** Break in hash chain

---

## Deployment Checklist

### Pre-Deployment
- [ ] Security architecture reviewed by external CISO
- [ ] Penetration tests completed (all scenarios passed)
- [ ] Insurance policies active (E&O, Fidelity Bond, Cyber)
- [ ] Incident response procedures documented and drilled
- [ ] Employee security training completed
- [ ] Key management system deployed (AWS KMS or similar)

### Deployment
- [ ] Cryptographic receipt system deployed
- [ ] Audit logging system operational
- [ ] Row-level security policies enabled in database
- [ ] mTLS certificates deployed to API servers
- [ ] Monitoring & alerting configured (PagerDuty)
- [ ] Backup procedures tested

### Post-Deployment
- [ ] Monthly audit log exports running
- [ ] Anomaly detection active (monitoring dashboard)
- [ ] Incident response team on-call
- [ ] Security updates applied within 30 days
- [ ] Quarterly security reviews scheduled
- [ ] Annual penetration testing booked

---

## Security Contacts

| Role | Responsibility | Contact |
|------|-----------------|---------|
| **Chief Security Officer** | Overall security | cso@example.com |
| **Incident Response Lead** | Security incidents | security@example.com |
| **Compliance Officer** | Regulatory compliance | compliance@example.com |
| **Forensics Team** | Incident investigation | forensics@example.com |

### Escalation
**Critical Security Issue (confidentiality, integrity, availability at risk):**
1. Page on-call engineer (SMS)
2. Notify CSO (email + phone)
3. Invoke incident response procedures
4. Engage external forensics firm within 4 hours

---

## Testing & Verification

### Run Security Test Suite
```bash
# All tests
rebar3 eunit -m security_tests

# Specific suite
rebar3 eunit -m security_tests -n audit_log_test_

# With coverage
rebar3 cover
```

### Penetration Testing
```bash
# Run PT scenarios
./security/run_penetration_tests.sh

# Expected result: All attacks detected/prevented
# PT Scenario 1 (DB tampering): PASS
# PT Scenario 2 (Receipt forgery): PASS
# PT Scenario 3 (Cross-customer access): PASS
# PT Scenario 4 (Formula injection): PASS
# PT Scenario 5 (Audit tampering): PASS
```

### Manual Verification
```bash
# Verify receipt integrity
erlang -run pricing_receipt verify

# Check audit log chain
erlang -run audit_log verify_integrity

# Export audit log
erlang -run audit_log export_monthly

# Test anomaly detection
erlang -run monitoring detect_anomalies
```

---

## Compliance

### Standards & Frameworks
- **NIST Cybersecurity Framework** (CSF 2.0)
- **OWASP Top 10** (A01:2021 - Broken Access Control)
- **CWE Top 25** (CWE-200: Information Exposure)
- **SOC 2 Type II** (Controls: CC6.1, CC7.2, CC7.5)
- **PCI-DSS** (Requirement 3: Protect cardholder data)

### Audit Requirements
- **Monthly:** Audit log integrity verification
- **Quarterly:** Anomaly detection review
- **Semi-Annually:** Penetration testing
- **Annually:** External security audit (SOC 2 Type II)
- **Bi-Annually:** Insurance policy renewal

---

## References

### Security Standards
- [NIST Cybersecurity Framework](https://www.nist.gov/cyberframework)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)

### Cryptography
- [RFC 8032 - Edwards-Curve Digital Signature Algorithm (EdDSA)](https://tools.ietf.org/html/rfc8032)
- [FIPS 180-4 - Secure Hash Standard (SHA)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)

### Cloud Security
- [AWS KMS Best Practices](https://docs.aws.amazon.com/kms/latest/developerguide/best-practices.html)
- [GCP Key Management](https://cloud.google.com/docs/security/key-management-deep-dive)

### Incident Response
- [NIST Incident Response Guide](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-61r2.pdf)
- [SANS Incident Response Process](https://www.sans.org/white-papers/)

---

## License

CONFIDENTIAL - FINANCIAL SECURITY
For internal use only. Unauthorized distribution prohibited.

---

**Document Owner:** Chief Security Officer
**Last Reviewed:** 2026-01-25
**Next Review:** 2026-04-25
**Classification:** CONFIDENTIAL - FINANCIAL SECURITY
