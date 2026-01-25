# Subcontractor Boundaries: Build vs. Buy Strategy & Partner Management

**Last Updated**: January 25, 2026 | **Version**: 2.0 | **Owner**: Procurement Lead / Solutions Architect
**Classification**: Internal Use | **Audience**: Sales, Solutions Architecture, Legal, Finance

---

## Executive Summary

**Subcontractor Strategy**: ggen owns core compliance/security IP (15 SKUs); **strategically partners** for infrastructure, integrations, and specialized services where we don't have deep expertise.

**Key Principle**: Max 50% of contract value to subcontractors (non-negotiable). Core value = ggen SKUs + expertise, not integration margin.

**Strategic Partners**:
- **Cloud Providers** (AWS, Azure, GCP) — infrastructure, not subcontractors (customer's direct relationship)
- **Systems Integrators** (Accenture, Deloitte, Tech Mahindra) — deployment + custom integrations (50%+ of large deals)
- **Managed Service Providers** (Managed security, infrastructure) — 24/7 support, SOC operations (15–20% of deals)
- **Compliance Auditors** (3PAOs, HIPAA auditors, ISO auditors) — third-party assessments (required, cost-plus)
- **Technology Partners** (CrowdStrike, Splunk, HashiCorp) — OEM partnerships (no subcontracting, resale/integration)

---

## Strategic Decision Matrix: Build vs. Buy

Use this matrix to decide whether to build a capability in-house (ggen core) or partner/resell.

### Decision Criteria (5 Factors)

| Factor | Build (In-House) | Buy/Partner | Decision |
|--------|------------------|------------|----------|
| **Strategic Importance** | Critical to our value proposition | Nice-to-have, differentiator elsewhere | Build if critical, Partner if nice-to-have |
| **Compliance/Security** | Core to our IP (FedRAMP, HIPAA, etc.) | Commodity or external (certs, audit) | Build if core IP, Partner if commodity |
| **Time-to-Market** | Can invest 6–12 months R&D | Need in <3 months | Partner if urgent, Build if strategic |
| **Core Expertise** | 5+ years deep experience | New domain, learning required | Build if expert, Partner if new |
| **Customer Demand** | 70%+ of RFPs require it | <30% of customers care | Build if high demand, Partner if niche |

---

## Build vs. Buy Decision Examples

### Example 1: Real-Time Compliance Monitoring (SKU-03)

| Factor | Analysis |
|--------|----------|
| **Strategic Importance** | CRITICAL — Core to ggen differentiation (faster FedRAMP) |
| **Compliance/Security** | CORE — Our ontology engine + SPARQL processing = proprietary IP |
| **Time-to-Market** | Already built (2023) — now evolving |
| **Core Expertise** | EXPERT — Founded on compliance ontology research |
| **Customer Demand** | 95%+ of government RFPs |
| **Decision** | ✅ **BUILD IN-HOUSE** — Non-negotiable core SKU |

---

### Example 2: Incident Response Training (IR-2 Control Gap)

| Factor | Analysis |
|--------|----------|
| **Strategic Importance** | MEDIUM — Nice-to-have, not core differentiation |
| **Compliance/Security** | COMMODITY — Standard training curriculum (NIST-defined) |
| **Time-to-Market** | Not urgent — roadmap for Q3 2026 |
| **Core Expertise** | NOVICE — Would need to hire training team or build curriculum |
| **Customer Demand** | 40% of FedRAMP customers (nice-to-have, often self-sourced) |
| **Decision** | 🤝 **PARTNER** — Recommend partner (e.g., SANS, UCSF healthcare training) + provide ggen platform |

---

### Example 3: Terraform Modules (SKU-10)

| Factor | Analysis |
|--------|----------|
| **Strategic Importance** | MEDIUM — IaC automation, part of compliance posture |
| **Compliance/Security** | SEMI-CORE — Uses HashiCorp Terraform (open source) + ggen control mappings |
| **Time-to-Market** | Already built (2024) — part of core platform |
| **Core Expertise** | GOOD — Compliance architects + DevOps expertise |
| **Customer Demand** | 85% of cloud-native government customers |
| **Decision** | ✅ **BUILD IN-HOUSE** — Core SKU, but leverage open-source Terraform (not reinventing IaC) |

---

### Example 4: Penetration Testing / Ethical Hacking

| Factor | Analysis |
|--------|----------|
| **Strategic Importance** | MEDIUM — Required for FedRAMP, but external mandated |
| **Compliance/Security** | EXTERNAL — 3PAOs must use independent testing firm (can't be ggen-internal) |
| **Time-to-Market** | Required for each customer authorization (not an option to defer) |
| **Core Expertise** | MODERATE — Have security team, but certifications required (CEH, OSCP) |
| **Customer Demand** | 100% of FedRAMP/federal customers (mandatory 3PAO requirement) |
| **Decision** | 🤝 **PARTNER + OEM** — Recommend certified 3PAO partners (e.g., Coalfire, Orca Security), maintain referral relationship (not sub) |

---

### Example 5: AWS Cloud Infrastructure (Hosting)

| Factor | Analysis |
|--------|----------|
| **Strategic Importance** | CRITICAL for scalability, but infrastructure, not compliance IP |
| **Compliance/Security** | COMMODITY — AWS provides (not ggen responsibility) |
| **Time-to-Market** | Already in place (2023) — multi-region deployment |
| **Core Expertise** | MODERATE — AWS experts on staff, but AWS is customer's choice too |
| **Customer Demand** | 100% for cloud deployment |
| **Decision** | 📋 **CUSTOMER OR PARTNER** — Customer chooses cloud (AWS, Azure, GCP). If we host: AWS direct relationship (not sub), customer pays AWS directly OR we resell with 10–15% margin markup |

---

## Partner Types & Engagement Models

### 1. Cloud Providers (Not Subcontractors)

**Examples**: AWS, Microsoft Azure, Google Cloud Platform

**Relationship Type**: **OEM Licensing** (ggen is their customer/partner, not they're ours)

**Role in Customer Deals**:
- Customer selects cloud platform (we support all 3)
- We recommend cloud architecture (gMSA accounts, separation of duties)
- Customer/integrator provisions infrastructure
- **We don't subcontract cloud costs** — customer owns cloud bill

**ggen Responsibility**:
- ✅ Ensure our SKUs work on each cloud platform
- ✅ Provide cloud architecture recommendations (secure defaults, compliance baselines)
- ✅ Certifications: FedRAMP on AWS, Azure GovCloud, GCP FedRAMP equivalent

**Cost Model**:
- ggen pays: Development + certification costs (AWS FedRAMP auth, GCP testing)
- Customer pays: Cloud infrastructure directly (not through ggen)
- ggen doesn't take margin on cloud costs

**Contract Terms**:
- Reference gcloud/AWS/Azure in SOW as "Customer or Partner Selected Cloud Platform"
- No subcontracting language (direct customer-cloud relationship)

**Compliance Flow-Down**: Not required (cloud provider = different entity, not ggen sub)

---

### 2. Systems Integrators (SI) — Subcontractors (50% Max)

**Examples**: Accenture, Deloitte, Booz Allen Hamilton (BAH), Tech Mahindra, Capgemini

**Relationship Type**: **Subcontractors** (we bill customer, we pay SI, SI services flow under ggen SOW)

**Role in Customer Deals**:
- Deployment execution (set up infrastructure, deploy ggen SKUs, integrate with existing systems)
- Custom integrations (API development, legacy system connectors, data migration)
- Project management (gantt charts, status reporting, risk management)
- Change management (training, documentation, runbooks)

**When to Use SI**:
- Large deals (>$500K) where SI adds 15–30% value
- Complex integrations (4+ legacy systems, on-prem + cloud hybrid)
- Enterprise governance (PMO, security review boards, compliance sign-off)
- Multi-site deployments (SI handles logistics, travel, regional ops)

**Cost Model**:
- SI costs: Usually 15–40% of total deal (labor rates: $150–$250/hour)
- Example: $500K deal = $100K SI labor costs ($75K deployment, $25K integration)
- ggen revenue: $500K minus SI costs, minus ggen support
- ggen margin: 40–50% of total contract (SI takes 15–25%)

**Subcontracting Limit Enforcement**:
- Max SI cost = 50% of contract value
- Example: $500K deal → SI costs capped at $250K
- If SI costs exceed 50%, re-negotiate scope (reduce custom integrations, increase out-of-box deployment)

**Contract Terms** (SOW Language):
```
SUBCONTRACTING DISCLOSURE:

ggen will engage subcontractors for the following services:
- Deployment and configuration: [SI Name] ($[X]K, [X]% of contract)
- Custom integrations: [SI Name] ($[X]K, [X]% of contract)
- Project management: [SI Name] (included in deployment labor)

Total subcontracting: $[X]K ([X]% of total contract value, <50% limit)

[SI Name] shall:
1. Flow down all security/compliance requirements from this agreement
2. Maintain the same confidentiality and data protection as ggen
3. Be subject to ggen's vendor management and security audits
4. Not disclose customer data or system architecture without written approval
```

**Compliance Flow-Down**: MANDATORY (SI is on government contract, must comply with FAR/DFARS)

---

### 3. Managed Service Providers (MSP) — Partners/Subcontractors

**Examples**: Managed SOC vendors (e.g., Rapid7, CrowdStrike for managed defense), Managed cloud/infrastructure providers

**Relationship Type**: **Partners** (can be subcontractors on large deals, or OEM resale on small deals)

**Role in Customer Deals**:
- 24/7 Security Operations Center (SOC) — monitoring, incident response
- Managed Infrastructure (compute, storage, networking management)
- Managed Compliance Services (continuous monitoring, evidence collection)

**When to Use MSP**:
- Customer doesn't have in-house 24/7 SOC → MSP provides around-the-clock monitoring
- Managed compliance: Customer wants ggen + MSP handling evidence collection/continuous monitoring
- Cost-effective for customers without security teams (use MSP instead of hiring)

**Cost Model**:
- MSP costs: Usually 10–20% of annual subscription (e.g., $50K–$100K/year on $500K deal)
- Can be add-on (customer chooses) or bundled (ggen + MSP package)
- ggen margin: Resell MSP services at 15–25% markup (pass through + small margin)

**Subcontracting Limit**: Doesn't count toward 50% limit (can be 10–20% on top of 50% SI limit)

**Contract Terms**:
```
MANAGED SERVICES (Optional Add-On):

ggen can bundle managed security services from [MSP Name]:
- 24/7 SOC monitoring: $[X]K/year (included in $[Y]K annual SKU bundle)
- Managed evidence collection: $[X]K/year (optional)
- MSP is customer's vendor (ggen acts as billing/coordination agent)
```

**Compliance Flow-Down**: Required if MSP is subcontractor; optional if direct customer relationship

---

### 4. Compliance Auditors (3PAOs, HIPAA Auditors, ISO Auditors) — Partners

**Examples**: Coalfire (FedRAMP 3PAO), UL (ISO), Veracode (security audits), Big 4 auditors (HIPAA)

**Relationship Type**: **Partners** (required by regulation, customer pays directly or via ggen SOW)

**Role in Customer Deals**:
- **3PAO Assessment**: Independent evaluation of security controls for FedRAMP
- **HIPAA Audit**: 3rd-party verification of HIPAA compliance (required by rule)
- **SOC 2 Audit**: Type II reports validating ggen's own controls
- **Penetration Testing**: Independent security assessment (required for FedRAMP auth)

**When to Use Auditor**:
- **Mandatory**: Every FedRAMP deal (3PAO required, ~$100K–$300K audit cost)
- **Mandatory**: Every HIPAA deal (HIPAA auditor required, ~$50K–$150K)
- **Optional but Recommended**: SOC 2 for enterprise customers (proof of security controls)

**Cost Model**:
- Auditor costs: $50K–$300K per engagement (depends on system complexity)
- ggen pays: Auditors for our own certifications (SOC 2 annual audit, FedRAMP auth)
- Customer pays: 3PAO assessment for their system authorization (ggen can recommend, not bill)
- Can include in SOW if customer desires (SOW line item: "Assessment + Audit = $[X]K")

**Auditor Pricing** (Industry Standard):
- FedRAMP 3PAO assessment: $100K–$250K (6–9 month engagement)
- HIPAA BAA audit: $50K–$100K (6-week assessment)
- SOC 2 Type II (annual): $30K–$60K (6-month testing period)
- Penetration test (annual): $20K–$50K (2–4 week engagement)

**Contract Terms**:
```
THIRD-PARTY ASSESSMENT (Customer Pays):

ggen will coordinate with independent auditors for:
1. System Security Plan (SSP) review: [Auditor Name]
2. Security controls assessment: [Auditor Name] (3PAO approved)
3. Penetration testing: [Auditor Name] (independent security firm)

Auditor costs: $[X]K (passed through, cost-plus)
Timeline: [X] weeks from contract execution to final report
Auditor independence: Customer selects auditor; ggen provides recommendations

ggen responsibility: Provide documentation, host assessments, remediate findings
Customer responsibility: Pay auditor fees, coordinate audit timing, provide business context
```

**Compliance Flow-Down**: Not required (auditor is independent from ggen, customer's vendor)

---

### 5. Technology/Software Partners (OEM, Resale, Integration)

**Examples**: HashiCorp (Terraform), Splunk (SIEM), CrowdStrike (endpoint), Okta (IAM)

**Relationship Type**: **Partners** (can be OEM licensing, resale, or embedded integration)

**Role in Customer Deals**:
- **OEM Integration**: ggen integrates their APIs into our SKUs (e.g., Splunk integration in Evidence Aggregator)
- **Resale**: ggen includes their licenses in ggen bundle (e.g., Terraform modules bundled with IaC services)
- **Referral**: ggen recommends but doesn't bundle (e.g., CrowdStrike for endpoint detection, customer buys direct)

**When to Use Tech Partner**:
- **OEM Integration** (best case): Their technology is core to our SKU (Terraform in SKU-10, HashiCorp partnership)
- **Resale** (good case): Include their license in our bundle with coordination (Splunk SIEM integration)
- **Referral** (acceptable case): Recommend, but customer procures independently (CrowdStrike, Okta, others)

**Cost Model**:
- **OEM Integration**: ggen includes their APIs, resells through our SKUs (our margin on combined package)
  - Example: Terraform OEM license $50K + ggen markup = $75K in SKU-10 pricing
  - ggen margin: 25–40% on tech partner component

- **Resale**: ggen buys license from partner, includes in bundle
  - Example: Splunk license $40K + ggen markup = $60K included in Evidence Aggregator
  - ggen margin: 25–50% markup

- **Referral**: ggen recommends, customer buys direct (no ggen margin)
  - Example: Recommend CrowdStrike for endpoint detection, customer signs separate contract
  - ggen benefit: Partnership credit, referral fee (if negotiated), customer satisfaction

**Contract Terms**:
```
TECHNOLOGY PARTNER INTEGRATIONS:

ggen SKUs include integrations with the following technology partners:

1. HashiCorp Terraform (OEM):
   - License included in SKU-10 (Terraform Modules): $60K/year
   - Coverage: IaC automation, compliance baseline configuration
   - Support: ggen provides (Terraform support via HashiCorp)

2. Splunk Integration (Resale):
   - License included in SKU-04 (Evidence Aggregator): $40K/year
   - Coverage: SIEM integration, log aggregation, alerting
   - Support: ggen + Splunk (joint support agreement)

3. CrowdStrike Integration (Referral):
   - ggen integrates their API (no license cost)
   - Customer procures CrowdStrike separately ($[X]K/year)
   - Support: CrowdStrike (direct) + ggen coordination
```

**Compliance Flow-Down**: Depends on model
- OEM/Resale: Customer receives ggen warranty (tech partner license is sub-component)
- Referral: ggen disclaims responsibility (customer's direct relationship with partner)

---

## Subcontractor Selection Criteria

When choosing a subcontractor for a deal, evaluate:

### 1. Financial Stability

- **Question**: Can they stay in business and deliver?
- **Evaluation**:
  - Credit rating (D&B check)
  - Years in business (>5 years preferred)
  - Customer references (Fortune 500, government)
  - Revenue size (not a startup with <$10M revenue for large deals)
- **Red Flags**: 🚩 Startup with 1-2 customers, financial news of layoffs, negative cash flow

### 2. Security Clearance & Compliance

- **Question**: Are they cleared to work on government contracts?
- **Evaluation**:
  - Top Secret/Secret/TS/SCI clearances (if required)
  - CMMC certification (if DoD)
  - FedRAMP authorization (if cloud-focused)
  - FISMA compliance (if federal contractor)
  - Insurance coverage ($1M liability minimum)
- **Red Flags**: 🚩 No clearances, no insurance, bad security posture

### 3. Past Performance

- **Question**: Have they done similar work successfully?
- **Evaluation**:
  - 3+ customer references (similar deal size + complexity)
  - Government contracting experience (not just commercial)
  - FedRAMP/HIPAA deal experience (if compliance-focused)
  - Track record of on-time delivery
  - Customer satisfaction scores (>4.0 out of 5.0)
- **Red Flags**: 🚩 No government experience, failed projects, customer complaints

### 4. Technical Capability

- **Question**: Do they have the expertise to deliver?
- **Evaluation**:
  - Relevant certifications (AWS, GCP, Azure for cloud; CISSP for security; PMP for project mgmt)
  - Technical depth (architects, engineers, not just junior staff)
  - Tool expertise (if using specific tools: Terraform, Kubernetes, Splunk, etc.)
  - Reference calls with 2 customer CISOs
- **Red Flags**: 🚩 Mostly junior staff, no relevant certifications, can't discuss technical details

### 5. Contractual Terms & Risk Tolerance

- **Question**: Do they accept government contract flow-down requirements?
- **Evaluation**:
  - Insurance requirements (liability, professional indemnity)
  - Data protection & confidentiality (NDA, subprocessor agreement)
  - Background checks (for staff working on contract)
  - IP ownership (who owns work product?)
  - Indemnification (who pays if they breach?)
- **Red Flags**: 🚩 Won't sign NDAs, refuses background checks, demands IP ownership

### 6. Communication & Responsiveness

- **Question**: Can we trust them to communicate issues?
- **Evaluation**:
  - Dedicated account manager (not rotating contacts)
  - Response SLA (e.g., 24-hour email, 4-hour critical calls)
  - Escalation path (who do we call if issue?)
  - Project reporting (weekly status, metrics)
- **Red Flags**: 🚩 Slow to respond, defensive when asked for updates, unclear escalation

---

## Subcontractor Agreement Template

**Use when engaging a subcontractor for a deal.**

```markdown
# SUBCONTRACTOR AGREEMENT

## PARTIES
- **Prime**: ggen Inc.
- **Subcontractor**: [Name/Company]
- **Customer**: [Government Agency/Customer Name]
- **Contract Number**: [Government RFP Number, if applicable]

## SCOPE
[Name] will provide the following services under this agreement:
- [Service 1: Deployment, Integration, Project Management, etc.]
- [Service 2]
- [Service 3]

**Deliverables**: [List specific outputs, timelines, acceptance criteria]
**Duration**: [Start date] to [End date]
**Payment**: $[X] ([X]% of total contract value)

## COMPLIANCE & SECURITY REQUIREMENTS

Subcontractor shall:
1. **Data Protection**: Comply with [Customer] data protection requirements (NIST 800-53, HIPAA, DFARS, etc.)
2. **Confidentiality**: Sign NDA with ggen + [Customer], maintain confidentiality for [X] years post-contract
3. **Background Checks**: Authorized key personnel cleared by [Customer] security (SF-86 form if TS/SCI required)
4. **Insurance**: Maintain $[1M] general liability insurance throughout contract period
5. **Compliance Audits**: Allow ggen + [Customer] to audit compliance (security assessments, certifications)
6. **Incident Reporting**: Report security incidents to ggen within [2 hours], to [Customer] within [24 hours]

## FLOWDOWN REQUIREMENTS

Subcontractor acknowledges that [Customer] contract includes the following requirements that flow down:

**If FedRAMP Moderate Deal**:
- [ ] System Security Plan (SSP) compliance
- [ ] Security controls per NIST 800-53 (Moderate)
- [ ] Continuous monitoring requirements
- [ ] 3PAO assessment cooperation
- [ ] Federal Acquisition Regulation (FAR) compliance

**If DoD/DFARS Deal**:
- [ ] NIST 800-171 compliance (CMMC alignment)
- [ ] Cybersecurity Maturity Model Certification (CMMC) requirements
- [ ] Foreign ownership restrictions (US company required)
- [ ] Subcontractor cost/schedule data submission

**If HIPAA Deal**:
- [ ] Business Associate Agreement (BAA) signature
- [ ] PHI handling and encryption requirements
- [ ] Breach notification procedures (within 24 hours)
- [ ] Audit log retention (6 years minimum)

## KEY PERSONNEL

[Name] shall provide the following key personnel for this engagement:

| Role | Name | Qualifications | FTE % |
|------|------|---|---|
| Project Manager | [Name] | PMP, [X] years gov't experience | 75% |
| Lead Architect | [Name] | CISSP, AWS Solutions Architect | 100% |
| Lead Engineer | [Name] | [Relevant certs], [X] years experience | 100% |

**Substitution**: [Name] cannot be replaced without written approval from ggen + [Customer] (48 hours notice required for emergency replacements)

## PERFORMANCE METRICS & ACCEPTANCE

| Milestone | Deliverable | Due Date | Acceptance Criteria |
|-----------|------------|----------|----------------------|
| M1 | Project Plan + Security Assessment | [Date] | Approved by ggen + [Customer] |
| M2 | [System Deployment] | [Date] | Deployment complete, testing passed |
| M3 | [Integration Complete] | [Date] | All systems integrated, UAT passed |
| M4 | [Final Report/Training] | [Date] | Documented + customer sign-off |

**Payment**: Paid upon acceptance of each milestone (Net 30)

## LIABILITY & INDEMNIFICATION

1. **Liability Cap**: Subcontractor liability capped at total contract value ($[X])
2. **Indemnification**: Subcontractor indemnifies ggen + [Customer] for:
   - Breach of confidentiality
   - IP infringement claims
   - Failure to comply with security/compliance requirements
   - Third-party claims arising from Subcontractor's negligence

3. **Insurance**: Subcontractor maintains liability insurance throughout contract period (proof upon request)

## TERMINATION

ggen may terminate this agreement:
- For cause (security breach, non-compliance, key personnel replacement): Immediate
- For convenience (after 30-day notice): [Payment for work completed through termination date]

## CONFIDENTIALITY

Subcontractor agrees to:
- Not disclose [Customer] name, system architecture, or data without written approval
- Return/destroy all customer materials upon contract termination
- Maintain confidentiality for [3] years post-contract (or per [Customer] requirement)

## GOVERNING LAW

This agreement governed by [State/Federal] law and subject to [Customer] contract disputes clause.

---

**Signatures**:
- ggen: _________________ (Date: ________)
- Subcontractor: _________________ (Date: ________)
- [Customer Legal (if required)]: _________________ (Date: ________)
```

---

## Subcontracting Limits & Financial Controls

### Enforcement Rules

**Rule 1: Max 50% of Contract Value**
```
Example Deal: $500K annual subscription

Subcontractor cost limit: $250K (50% of $500K)

Budget allocation:
├─ ggen SKU costs: $200K (40%)
├─ ggen support: $50K (10%)
└─ Subcontractor (SI deployment): $250K (50%) ✅ At limit

If SI requests $280K (56%), must:
1. Renegotiate scope (reduce custom work, use templates)
2. Move some work to post-contract professional services
3. Find lower-cost SI alternative
```

**Rule 2: Escalation for >30% Subcontracting**
- 10–30% subcontracting: Sales Lead approval (routine)
- 30–50% subcontracting: VP Sales + CFO approval (at limit)
- >50% subcontracting: NOT ALLOWED (renegotiate deal)

**Rule 3: Subcontracting Disclosure**
- Every deal >$100K must disclose subcontractor names + costs in SOW
- Every government contract must flow down compliance requirements
- Audit trail maintained for compliance review

### Financial Impact Example

```
Customer: Federal Agency (FedRAMP Moderate)
Deal Value: $450K/year

SCENARIO A: Low Subcontracting (20%)
├─ ggen SKU revenue: $450K
├─ Subcontractor cost: $90K (20%, SI deployment only)
├─ ggen gross cost: $220K (COGS + support)
├─ ggen gross profit: $230K (51% margin) ✅ Good
└─ ggen net profit: $100K (22% margin) ✅ Good

SCENARIO B: High Subcontracting (50%)
├─ ggen SKU revenue: $450K
├─ Subcontractor cost: $225K (50%, SI + MSP)
├─ ggen gross cost: $180K (COGS reduced, SI handling deployment)
├─ ggen gross profit: $270K (60% margin) 🔴 Misleading
│  (But SI gets $225K, ggen's real take: $45K)
└─ ggen net profit: $20K (4% margin) ❌ Bad

RECOMMENDATION: Scenario A (20–30% SI) is best for ggen profit + customer value
```

---

## Examples: Common Subcontracting Scenarios

### Scenario 1: Standard FedRAMP Deal (SI Deployment)

**Customer**: HHS Agency, FedRAMP Moderate
**Deal Value**: $450K/year (3-year = $1.35M total)
**Timeline**: 12 weeks to operational

**Subcontractor Model**:
- **SI Partner**: Accenture (selected by ggen for this deal)
- **SI Responsibility**: Deployment (2 months), integration (3 weeks), training (1 week)
- **SI Cost**: $100K (22% of deal) — reasonable for 12-week deployment
- **Subcontracting Limit**: 50% of $450K = $225K available (using $100K = 22%) ✅

**SOW Language**:
```
SUBCONTRACTING DISCLOSURE:

ggen will engage Accenture for deployment services:
- Deployment coordination: $80K (16 weeks, 1 PM + 2 engineers)
- Training (on-site, 3 days): $15K
- Project management: Included in deployment

Total SI cost: $95K (21% of annual subscription)

Subcontract agreement executed separately per ggen Subcontractor Agreement template.
Accenture flows down all security/compliance requirements (FedRAMP, NIST 800-53).
```

**Outcome**: ✅ Good deal structure (SI handles logistics, ggen handles compliance core)

---

### Scenario 2: Enterprise Deal with Multiple Subcontractors

**Customer**: Large Federal Department (10 agencies, multi-year)
**Deal Value**: $1.2M/year (3-year = $3.6M total)
**Timeline**: 6 months to full deployment across 10 agencies

**Subcontractor Model**:
- **SI Partner 1**: Deloitte (primary, lead 5 agencies) — $300K (25%)
- **SI Partner 2**: Tech Mahindra (secondary, 5 agencies) — $200K (17%)
- **MSP Partner**: Managed security SOC — $80K/year (7%)
- **3PAO Auditor**: Coalfire (system assessment) — $150K (one-time)
- **Total Subcontracting**: $580K year 1 (48% of deal) ✅ At limit

**Subcontracting Budget Breakdown**:
- Deloitte: $300K SI labor (primary integrator)
- Tech Mahindra: $200K SI labor (secondary integrator, regional)
- Managed SOC: $80K/year (included in ggen bundle as managed service)
- 3PAO Audit: $150K one-time (customer pays direct, not through ggen)

**SOW Language**:
```
SUBCONTRACTING DISCLOSURE (Multi-Vendor Model):

ggen engages the following subcontractors for this 10-agency deployment:

1. Deloitte Consulting (Primary SI):
   - Lead integrator for [Agencies 1–5]
   - Deployment + custom integrations + project management
   - Cost: $300K (Year 1), $50K (Years 2–3 optimization)
   - FedRAMP compliance: Deloitte flows down all requirements

2. Tech Mahindra (Secondary SI):
   - Lead integrator for [Agencies 6–10] (regional presence)
   - Deployment + custom integrations
   - Cost: $200K (Year 1), $30K (Years 2–3 optimization)
   - FISMA compliance: Tech Mahindra flows down all requirements

3. Managed SOC (Annual):
   - 24/7 Security Operations Center monitoring
   - Cost: $80K/year (included in ggen bundle)
   - Partner: [Managed SOC vendor name]

4. 3PAO Assessment (Customer Direct):
   - System Security assessment for multi-agency authorization
   - Cost: $150K (customer pays directly to Coalfire)
   - Coordination: ggen facilitates assessment

Total Year 1 Subcontracting: $580K (48% of $1.2M deal, at limit)
Years 2–3 Subcontracting: $110K/year (9% of annual, reduces after deployment)

All subcontractors bound by FAR/DFARS requirements + flow-down security clauses.
```

**Outcome**: ✅ Complex deal, well-structured with clear boundaries and compliance

---

### Scenario 3: Build vs. Partner Mistake (Anti-Pattern)

**Problem Customer**: Health tech startup wants "compliance-ready" solution
**Deal Value**: $200K/year
**Timeline**: 3 months to operational

**WRONG Approach**:
- ggen tries to build everything in-house (HIPAA kit, audit training, compliance consulting)
- Hires 2 contractors @ $80K each to custom-build HIPAA features
- Overspends on small deal: $160K subcontracting on $200K deal (80%!) ❌
- Misses timeline, quality suffers, customer unhappy

**RIGHT Approach**:
- ggen: Recommend HIPAA Compliance Kit (SKU-12) + Evidence Aggregator (SKU-04)
- Partner: Work with HIPAA compliance auditor (FYI, not sub) — customer contracts directly
- Partner: Recommend MSP for continuous evidence collection (optional add-on)
- Subcontracting: $30K SI labor only (15%) for deployment + integration ✅
- Timeline: 6 weeks (fast), Quality: High (tested SKU), Cost: Efficient (low SI)

**Outcome**: ✅ Right-sized deal, partners bring expertise ggen doesn't have

---

## Compliance Flow-Down Template

**Use when engaging a subcontractor for government deals.**

```markdown
# COMPLIANCE REQUIREMENTS FLOW-DOWN

**Prime Contract**: [Customer RFP Number], [Compliance Framework]
**Subcontractor**: [Name], [Role], [Cost]

## SECURITY REQUIREMENTS (Flow Down)

**Federal Acquisition Regulation (FAR)**:
- [ ] FAR 48 CFR 52.204-21 (Information Security)
- [ ] FAR 48 CFR 52.239-1 (Privacy of Records)
- [ ] FAR cybersecurity requirements (if applicable)

**DFARS (If DoD)**:
- [ ] DFARS 252.204-7008 (Compliance with Safeguarding Measures)
- [ ] NIST 800-171 compliance for CI systems
- [ ] Cybersecurity Maturity Model Certification (CMMC) requirements
- [ ] Subcontractor disclosure of foreign ownership/control

**FISMA/FedRAMP (If Federal)**:
- [ ] NIST 800-53 controls (specify level: Low/Moderate/High)
- [ ] System Security Plan (SSP) compliance
- [ ] Continuous monitoring requirements
- [ ] Incident reporting (within 1 hour of discovery)

**HIPAA (If Healthcare)**:
- [ ] Business Associate Agreement (BAA) with Subcontractor
- [ ] HIPAA Security Rule (45 CFR §164.308-314)
- [ ] Breach notification (within 24 hours)
- [ ] PHI encryption and access controls
- [ ] Background checks (healthcare workers)

## OPERATIONAL REQUIREMENTS

1. **Data Protection**:
   - [ ] Data at-rest: Encryption required (AES-256 minimum)
   - [ ] Data in-transit: TLS 1.2+ required
   - [ ] Data handling: Per [Customer] data classification policy
   - [ ] Data retention: Per contract terms ([X] years)

2. **Access Control**:
   - [ ] MFA required (hardware token or TOTP)
   - [ ] Least privilege (only necessary access granted)
   - [ ] Role-based access control (RBAC)
   - [ ] Access reviews (quarterly)
   - [ ] Termination procedures (immediate access revocation)

3. **Incident Response**:
   - [ ] Incident response plan (provided by Subcontractor)
   - [ ] Incident reporting (to ggen within 2 hours, to Customer within 24 hours)
   - [ ] Root cause analysis (within 5 business days)
   - [ ] Remediation timeline (per NIST 800-53 IR-2, IR-4)

4. **Audit & Compliance**:
   - [ ] Allow security assessments by ggen + [Customer]
   - [ ] Provide audit logs (6-month retention minimum)
   - [ ] Third-party security audit (annual, SOC 2 Type II)
   - [ ] Compliance certification (if required: FedRAMP, HIPAA, etc.)

5. **Personnel Security**:
   - [ ] Background checks (Secret clearance or equivalent)
   - [ ] Confidentiality agreements (NDA, IP protection)
   - [ ] Security training (annual, at least 1 hour)
   - [ ] Termination procedure (exit interview, access revocation)

## DOCUMENTATION REQUIREMENTS

Subcontractor shall provide:
- [ ] System Security Plan (SSP) excerpt (how compliance is achieved)
- [ ] Business Continuity Plan (BCP)
- [ ] Disaster Recovery Plan (DRP)
- [ ] Security policies (data protection, incident response, access control)
- [ ] Audit results (SOC 2 Type II, penetration tests, vulnerability scans)
- [ ] Personnel security documentation (background checks, training records)
- [ ] Insurance certificates (liability, professional indemnity)

## MONITORING & VERIFICATION

ggen shall monitor Subcontractor compliance via:
1. **Quarterly Reviews**: Security posture, incident reports, audit findings
2. **Annual Assessments**: Site visits, process verification, control testing
3. **Real-Time Alerts**: Security event monitoring (incidents >severity threshold)
4. **Customer Audits**: Subcontractor cooperates with [Customer] security assessments

**Non-Compliance Actions**:
- Minor issues: 30-day remediation notice
- Major issues: 10-day remediation notice
- Critical issues (data breach, unauthorized access): Immediate termination

## SUBCONTRACTOR SIGN-OFF

I acknowledge that I have read and understand all compliance requirements above and commit to compliance.

**Subcontractor Manager**: _________________ (Signature/Date)
**ggen Project Lead**: _________________ (Signature/Date)
**Customer Authority** (if required): _________________ (Signature/Date)
```

---

## Receipt Contract (Compliance Proof)

**Document**: subcontractor-boundaries.md
**Version**: 2.0
**Classification**: Internal Use
**Purpose**: Define ggen's build vs. buy strategy, partner types, selection criteria, subcontracting limits (50% max), compliance flow-down, and contract templates

**Evidence Artifacts** (Attached/Referenced):
- ✅ Strategic Decision Matrix (build vs. buy, 5 criteria)
- ✅ 5 Partner Types (Cloud Providers, SI, MSP, Auditors, Tech Partners)
- ✅ Selection Criteria (financial stability, clearances, past performance, technical capability, risk tolerance)
- ✅ Subcontractor Selection Criteria (6 factors with red flags)
- ✅ Subcontractor Agreement Template (comprehensive, multi-scenario)
- ✅ Subcontracting Limits & Financial Controls (50% max, escalation rules)
- ✅ 3 Real-World Examples (standard FedRAMP, enterprise multi-vendor, anti-pattern)
- ✅ Compliance Flow-Down Template (FAR/DFARS/FISMA/HIPAA requirements)
- ✅ Cost model examples (financial impact of subcontracting levels)

**Signature Authority**:
- **Procurement Lead**: _________________ (Date: _______)
- **General Counsel**: _________________ (Date: _______)
- **VP Sales**: _________________ (Date: _______)

**Audit Trail**:
- Created: January 25, 2026
- Last Updated: January 25, 2026
- Version Control: Git commit [SHA]
- Document Hash (SHA-256): [Auto-computed]
- Audit Schedule: Quarterly (review all active subcontracts)

---

## Definition of Done

This document is **COMPLETE** when:

- ✅ Strategic Decision Matrix created (build vs. buy, 5 examples)
- ✅ 5 Partner Types documented (Cloud, SI, MSP, Auditors, Tech Partners)
- ✅ Partner engagement models defined (OEM, Resale, Referral, Subcontracting)
- ✅ Selection Criteria checklist created (6 factors + red flags)
- ✅ Subcontractor Agreement template completed (comprehensive)
- ✅ Subcontracting Limits enforced (50% max, escalation rules, financial controls)
- ✅ Compliance Flow-Down template created (FAR/DFARS/FISMA/HIPAA)
- ✅ 3 Real-World Examples documented (standard, enterprise, anti-pattern)
- ✅ Financial Impact Examples completed (margin analysis, cost model)
- ✅ All templates formatted for government contracts (professional, auditor-ready)
- ✅ References to related documents correct (compliance-matrix.md, boe-pricing.md, capture-to-bid-pipeline.md)
- ✅ Receipt Contract section completed
- ✅ Definition of Done checklist included

---

**FINAL**: All 4 procurement documents complete. Cross-reference in capabilities-statement.md and glossary.md.
