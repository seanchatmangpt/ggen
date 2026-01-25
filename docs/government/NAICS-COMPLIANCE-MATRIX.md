# NAICS Codes, Compliance Frameworks, & SKU Mapping

## Part 1: NAICS Classification

### Primary NAICS Code: 518210
**Data Processing, Hosting, and Related Services**

Industry group: Information Technology Services
Applies to: Organizations providing data processing and hosting services in cloud environments

**Why this code**: TAI 2030 delivers hosted autonomic governance services on GCP (data processing of operational signals → decisions → actions).

---

### Secondary NAICS Code: 541512
**Computer Systems Design Services**

Industry group: Professional Services
Applies to: Custom system design and integration for government IT environments

**Why this code**: TAI 2030 designs and implements autonomic governance systems tailored to agency needs (policy packs, compliance frameworks, organizational structure).

---

## Part 2: Compliance Framework Matrix

### FISMA (Federal Information Security Modernization Act)
**Applies To**: All federal agencies + contractors processing federal data
**Government Buyers**: Defense, NASA, EPA, DoD, IC, VA, all civilian agencies

| SKU | FISMA Contribution | Evidence | SLO |
|-----|-------------------|----------|-----|
| ATO Guard Pack | Evidence collection for FISMA audit | Receipt + audit export | 100% coverage |
| Permission Drift Guard | IA-2: Account Management, IA-5: Authentication | Permission change receipts | <100ms per change |
| Change Governance Guard | CM-3: Change Control | Deploy receipts + approval records | <50ms per change |
| Signal Storm Governor | SI-4: Information System Monitoring | Alert receipts + mitigation | <500ms per alert |
| Zero-Trust Enforcer | AC-2: Account Management | Every action requires receipt | 100% mandatory |

**FISMA Audit Evidence**:
- ✅ Mandatory receipts for every action
- ✅ Automated evidence ledger (Firestore)
- ✅ Sub-minute investigation (Receipt Verifier)
- ✅ Policy-as-code enforcement (Policy Pack Compiler)
- ✅ Audit trail exportable as evidence

**Contract Value**: $500K–$5M/year (depends on agency size)

---

### FedRAMP (Federal Risk & Authorization Management Program)
**Applies To**: Cloud service providers serving federal agencies
**Government Buyers**: GSA, OMB, all agencies using cloud (AWS GovCloud, Azure Government, GCP)

| SKU | FedRAMP Requirement | Evidence | SLO |
|-----|-------------------|----------|-----|
| Provenance Ledger | Audit logs + event logging | Hash-chained receipts | 100% completeness |
| Regression Rollback Guard | Incident response + recovery | Rollback receipts | <500ms action |
| Environment Baseline Guard | Configuration management | Baseline verification receipts | <100ms per check |
| Artifact Attestation Pack | Supply chain integrity (in-toto) | SBOM + attestations | Signed + verifiable |

**FedRAMP Authorization Path**:
- ✅ Prototype → Provisional Authorization (Year 1–2)
- ✅ 18-month continuous monitoring
- ✅ Full Authorization achievable

**Contract Value**: $300K–$2M/year (per cloud service)

---

### SOC 2 Type II (Service Organization Control)
**Applies To**: Service providers handling customer data
**Government Buyers**: Any agency using TAI 2030 as managed service

| SKU | SOC 2 Principle | Evidence | Audit Period |
|-----|-----------------|----------|--------------|
| ATO Guard Pack | CC7: Contingency Planning | Receipt backups + verification | Continuous |
| Receipt Verifier | A1: Controls over access | Receipt ledger access controls | Continuous |
| Audit Readiness Pack | A1: User Access Control | Automated audit trails | 12 months |
| Data Integrity Guard | C1: Data availability | Data change receipts | Continuous |

**SOC 2 Type II Scope**:
- 12-month continuous monitoring
- Annual auditor assessment
- Audit report available to customers
- Typical cost: $50K–$100K (included in contract)

**Contract Value**: $200K–$1M/year (included in base contract)

---

### HIPAA (Health Insurance Portability & Accountability Act)
**Applies To**: Organizations handling Protected Health Information (PHI)
**Government Buyers**: VA, NIH, HHS, Health Plans, Providers

| SKU | HIPAA Requirement | Evidence | Standard |
|-----|------------------|----------|----------|
| Data Integrity Guard | §164.308: Audit Controls | Data change receipts + verification | Audit logs |
| Budget Spike Guard | §164.308: Cost Controls | Spend change receipts + alerts | Financial controls |
| Compliance Monitor | §164.312: Logging & Monitoring | Continuous compliance receipts | Continuous monitoring |

**HIPAA Audit Evidence**:
- ✅ Automatic PHI access logging
- ✅ Change receipts for all data modifications
- ✅ Breach detection (anomaly quarantine)
- ✅ Retention policy enforcement (automatic purge receipts)

**Contract Value**: $400K–$3M/year (larger datasets = higher value)

---

### 21 CFR Part 11 (FDA Electronic Records)
**Applies To**: Pharmaceutical, medical device, biotech organizations
**Government Buyers**: FDA-regulated contractors, NIH grant recipients

| SKU | CFR Requirement | Evidence | SLO |
|-----|-----------------|----------|-----|
| Provenance Ledger | 11.100: Audit Trail | Hash-chained event log | Immutable + verifiable |
| Receipt Verifier | 11.70: Access Controls | Signed receipts + verification | Cryptographically sound |
| Artifact Attestation Pack | 11.50: Validation | Build + deploy attestation | Reproducible artifacts |

**21 CFR Part 11 Compliance**:
- ✅ Immutable audit trails (hash-chained)
- ✅ Digital signatures (receipts are signed)
- ✅ System documentation (generated from ontology)
- ✅ Validation evidence (all actions receipted)

**Contract Value**: $300K–$2M/year (highly regulated = premium)

---

### NIST SP 800-53 (Security Controls Catalog)
**Applies To**: All federal agencies + contractors (referenced in FISMA)
**Government Buyers**: All federal agencies

```
IA-2: Authentication
  → Zero-Trust Enforcer proves every action with receipt

IA-5: Password Management
  → Permission Drift Guard enforces credential rotation

AC-2: Account Management
  → Tenant Isolation Governors enforce per-tenant boundaries

AC-3: Access Control
  → Policy Pack Compiler generates enforceable access rules

AU-2: Audit Events
  → Receipt ledger automatically records every event

CA-2: Security Assessment & Authorization
  → ATO Guard Pack provides automated compliance evidence

CM-3: Change Control
  → Change Governance Guard enforces safe deployments

SI-4: System Monitoring
  → Signal Storm Governor + Data Integrity Guard detect anomalies
```

**NIST 800-53 Coverage**:
- ✅ 15+ security controls directly addressed
- ✅ Automated evidence generation
- ✅ Continuous verification
- ✅ Audit trail completeness

**Contract Value**: Included in FISMA contract

---

### DFARS (Defense Federal Acquisition Regulation Supplement)
**Applies To**: Defense contractors handling controlled unclassified information (CUI)
**Government Buyers**: DoD, Defense contractors

| SKU | DFARS Requirement | Evidence | SLO |
|-----|------------------|----------|-----|
| ATO Guard Pack | 252.204-7012: CUI Control | Receipt ledger for all access | Mandatory logging |
| Permission Drift Guard | 252.204-7012: Access Control | IAM change receipts | <100ms per change |
| Change Governance Guard | 252.204-7012: Deployment Control | Deploy receipts + approvals | <50ms per action |
| Zero-Trust Enforcer | 252.204-7012: Verification | Every action requires proof | 100% coverage |

**DFARS Compliance**:
- ✅ CUI access logging (mandatory)
- ✅ Change control (every deployment receipted)
- ✅ Incident reporting (<72 hours)
- ✅ Supplier security (SCAP scans, vulnerability management)

**Contract Value**: $500K–$5M/year (CUI-sensitive = premium)

---

## Part 3: SKU-to-Compliance Mapping

```
┌─────────────────────────────────────────────────────────────┐
│ ATO Guard Pack                                              │
├─────────────────────────────────────────────────────────────┤
│ Compliance Frameworks:                                      │
│   ✓ FISMA (Federal Information Security)                   │
│   ✓ SOC 2 Type II (Contingency planning)                   │
│   ✓ NIST SP 800-53 (CA-2, AU-2)                            │
│   ✓ DFARS (CUI control, access logging)                    │
│                                                             │
│ Government Buyers:                                          │
│   • Defense (FISMA ATO evidence)                           │
│   • NASA (Audit compliance)                                │
│   • All federal agencies                                   │
│                                                             │
│ Contract Value: $500K–$5M/year                             │
│ TAI 2030 SKU Bundle: 5-SKU Defense Pack                    │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Permission Drift Guard                                      │
├─────────────────────────────────────────────────────────────┤
│ Compliance Frameworks:                                      │
│   ✓ FISMA (IA-2, IA-5, AC-2)                              │
│   ✓ NIST SP 800-53 (15+ controls)                         │
│   ✓ DFARS (Access control)                                │
│   ✓ CIS Controls (v8: 5.3, 6.2)                           │
│                                                             │
│ Government Buyers:                                          │
│   • Defense (privilege management)                          │
│   • Intelligence (compartmentalization)                    │
│   • All agencies with zero-trust requirements              │
│                                                             │
│ Contract Value: $200K–$1M/year                             │
│ TAI 2030 SKU Bundle: 3-SKU IAM Pack                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Change Governance Guard                                     │
├─────────────────────────────────────────────────────────────┤
│ Compliance Frameworks:                                      │
│   ✓ FISMA (CM-3)                                           │
│   ✓ NIST SP 800-53 (CM controls)                           │
│   ✓ ITIL (Change Management)                               │
│   ✓ CIS Controls (v8: 2.2, 3.3)                            │
│                                                             │
│ Government Buyers:                                          │
│   • NASA (Certification maintenance)                        │
│   • Defense (Change control gates)                          │
│   • All agencies with change board requirements             │
│                                                             │
│ Contract Value: $150K–$500K/year                           │
│ TAI 2030 SKU Bundle: Safe Deployment Bundle                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Data Integrity Guard                                        │
├─────────────────────────────────────────────────────────────┤
│ Compliance Frameworks:                                      │
│   ✓ HIPAA (Audit controls, data integrity)                │
│   ✓ 21 CFR Part 11 (Audit trail)                          │
│   ✓ FISMA (SI-4: Monitoring)                              │
│   ✓ SOC 2 Type II (C1: Availability)                      │
│                                                             │
│ Government Buyers:                                          │
│   • EPA (Data quality + compliance)                         │
│   • HHS (HIPAA compliance)                                  │
│   • Pharma/Medical Device (FDA regulated)                   │
│                                                             │
│ Contract Value: $250K–$1M/year                             │
│ TAI 2030 SKU Bundle: Data Governance Pack                  │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Provenance Ledger                                           │
├─────────────────────────────────────────────────────────────┤
│ Compliance Frameworks:                                      │
│   ✓ FedRAMP (Audit logs, event logging)                    │
│   ✓ 21 CFR Part 11 (Audit trail, immutability)            │
│   ✓ FISMA (AU-2, AU-12)                                    │
│   ✓ CIS Controls (v8: 8.2, 8.3)                            │
│                                                             │
│ Government Buyers:                                          │
│   • NASA (Provenance + certification)                       │
│   • FDA-regulated (Electronic records)                      │
│   • All cloud service providers (FedRAMP)                   │
│                                                             │
│ Contract Value: $300K–$2M/year                             │
│ TAI 2030 SKU Bundle: Compliance Audit Pack                 │
└─────────────────────────────────────────────────────────────┘
```

---

## Part 4: Government Procurement Strategy by Agency

### Defense (DoD, DISA)
**Primary Need**: FISMA ATO + continuous compliance evidence
**Best Fit SKUs**: ATO Guard Pack + Permission Drift Guard + Change Governance Guard + Signal Storm Governor
**Compliance**: FISMA, NIST 800-53, DFARS
**Contract Value**: $500K–$5M/year
**Decision Maker**: CIO, ATO Coordinator, Cyber Team

### NASA
**Primary Need**: Provenance + baseline enforcement (certification maintenance)
**Best Fit SKUs**: Provenance Ledger + Environment Baseline Guard + Regression Rollback Guard
**Compliance**: FedRAMP (for SaaS components), NASA-specific policies
**Contract Value**: $300K–$2M/year
**Decision Maker**: Safety/QA Team, Mission Assurance

### EPA
**Primary Need**: Data integrity + cost governance + compliance monitoring
**Best Fit SKUs**: Data Integrity Guard + Budget Spike Guard + Compliance Monitor
**Compliance**: FISMA (baseline), environmental data standards
**Contract Value**: $400K–$3M/year
**Decision Maker**: Data Stewardship, Finance, Compliance

### Intelligence Community (IC)
**Primary Need**: Multi-tenant governance + compartmentalization
**Best Fit SKUs**: Tenant Isolation Governors + Policy Pack Compiler + Receipt Verifier
**Compliance**: IC-specific (compartmentalization rules)
**Contract Value**: $1M–$10M/year (enterprise)
**Decision Maker**: Enterprise Architecture, Compliance, CIO

### DoD Health / VA / Civilian Agencies
**Primary Need**: Support reduction + compliance automation
**Best Fit SKUs**: No On-Call Pack + Audit Readiness Pack
**Compliance**: FISMA (for health data: HIPAA additional)
**Contract Value**: $250K–$2M/year
**Decision Maker**: Operations, Finance, Audit

---

## Part 5: Compliance Certification Roadmap

### Year 1 (2025–2026): Prototype Hardening
- ✅ Complete security assessment
- ✅ SOC 2 Type II audit prep
- ⏳ FedRAMP JAB pre-assessment

### Year 2 (2026–2027): Provisional Authorizations
- 🎯 FedRAMP Provisional Authorization (JAB approved)
- ✅ ISO 27001:2022 certification
- ✅ SOC 2 Type II audit completion

### Year 3 (2027–2028): Full Authorizations
- 🎯 FedRAMP Full Authorization (18 months monitoring complete)
- ✅ CMMI Level 2 Maturity Model
- ✅ In-toto supply chain provenance

### Ongoing: Per-Engagement Certifications
- 21 CFR Part 11 (per FDA-regulated customer)
- HIPAA BAA (per healthcare customer)
- DFARS compliance addendum (per defense contractor)

---

## Part 6: Contract Vehicle Options

### 1. GSA Schedule 70 (IT Professional Services)
**Pros**: Government-standard procurement
**Cons**: 2-3 year approval process
**Timeline**: Year 2–3 (2027–2028)
**Expected Annual Revenue**: $50M+ at maturity

### 2. GCP Marketplace
**Pros**: Immediate availability, low friction, organic discovery
**Cons**: GCP takes 30% fee
**Timeline**: Year 1 (2026)
**Expected Annual Revenue**: $5–10M in Year 1

### 3. Direct Government Contracts (IT Schedule)
**Pros**: Higher margin, larger contracts
**Cons**: RFP-driven, longer sales cycles
**Timeline**: Year 1–2 (2026–2027)
**Expected Annual Revenue**: $20–30M in Year 2

### 4. VOSB/WOSB Set-Asides (if eligible)
**Pros**: Access to reserved procurement
**Cons**: Size/ownership restrictions
**Timeline**: Upon business eligibility (2026)
**Expected Annual Revenue**: $5–10M

### 5. Federal Contracts Vehicle (NIH, DOE, etc.)
**Pros**: Longer-term relationships
**Cons**: Agency-specific negotiations
**Timeline**: Year 2+ (2027+)
**Expected Annual Revenue**: $5M+ per agency

---

## Summary Table: All SKUs + Compliance Coverage

| SKU | FISMA | FedRAMP | SOC 2 | HIPAA | 21 CFR | NIST 800-53 | DFARS | Est. Contract Value |
|-----|-------|---------|-------|-------|--------|------------|-------|-------------------|
| ATO Guard Pack | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | $500K–$5M |
| Permission Drift Guard | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | $200K–$1M |
| Change Governance Guard | ✓ | ✓ | ✓ |   |   | ✓ | ✓ | $150K–$500K |
| Signal Storm Governor | ✓ |   |   |   |   | ✓ | ✓ | $100K–$300K |
| Zero-Trust Enforcer | ✓ | ✓ |   |   |   | ✓ | ✓ | $150K–$500K |
| Provenance Ledger |   | ✓ |   |   | ✓ | ✓ |   | $300K–$2M |
| Regression Rollback Guard |   | ✓ |   |   |   |   |   | $100K–$300K |
| Environment Baseline Guard |   | ✓ |   |   |   |   |   | $100K–$300K |
| Data Integrity Guard | ✓ |   | ✓ | ✓ | ✓ | ✓ |   | $250K–$1M |
| Budget Spike Guard |   |   |   | ✓ |   |   |   | $100K–$300K |
| Compliance Monitor | ✓ | ✓ |   | ✓ | ✓ | ✓ |   | $150K–$500K |
| Tenant Isolation Governors | ✓ |   |   |   |   | ✓ | ✓ | $200K–$1M |
| Policy Pack Compiler | ✓ |   |   |   |   | ✓ | ✓ | $100K–$300K |
| Receipt Verifier |   | ✓ | ✓ | ✓ | ✓ | ✓ |   | $100K–$300K |
| Audit Readiness Pack | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | $150K–$500K |

---

**Government Procurement Ready**: January 2027 (FedRAMP Provisional Auth)
**Enterprise-Ready**: January 2030 (Full compliance certification suite)

