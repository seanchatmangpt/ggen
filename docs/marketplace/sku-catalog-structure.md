# SKU Catalog Structure

> *15 SKUs, 4 product bundles, unlimited compliance coverage. Every SKU is a receipt.*

---

## Philosophy

The SKU catalog is not a price list—it's an **RDF-driven, auto-updateable registry** where:
- Each SKU is a Turtle ontology instance
- Pricing is computed from control coverage
- Bundles are dynamically assembled
- ggen auto-generates new SKU templates from compliance standards

**Rule**: Add a compliance standard → ggen auto-generates the SKU. No manual pricing required.

---

## Core SKUs (15 Total)

### 1. ATO Guard Pack

**Purpose**: Automated Authority to Operate compliance for government cloud deployments.

```yaml
sku_id: sku_ato_guard_pack
sku_name: ATO Guard Pack
vendor_id: ggen-vendor
product_name: ggen Control & Evidence Platform
short_description: Reduce ATO compliance time 80% with automated evidence receipts
pricing:
  annual_flat: $50000-$500000
  usage_based: $1.00 per control evaluation
  regions: [us-gov-west-1]
control_coverage:
  - AC-2 (Account Management)
  - AC-3 (Access Enforcement)
  - AU-2 (Audit Events)
  - CM-3 (Change Control)
  - IA-2 (Authentication)
  - IA-4 (Identifier Management)
  - SC-7 (Boundary Protection)
  total_controls: 48
compliance_standards:
  - FedRAMP Baseline (2024-01-01)
  - FISMA Moderate (2024-01-01)
free_trial_days: 14
target_customer:
  - U.S. Federal Agencies
  - Agencies seeking ATO
  - Contractors in government cloud programs
```

### 2. Permission Drift Guard

**Purpose**: Detect unauthorized IAM permission creep in real time.

```yaml
sku_id: sku_permission_drift_guard
sku_name: Permission Drift Guard
pricing:
  annual_flat: $25000-$250000
  usage_based: $0.50 per permission audit
  regions: [us-central1, us-east1, us-west1]
control_coverage:
  - AC-1 (Access Control Policy)
  - AC-2 (Account Management)
  - AC-3 (Access Enforcement)
  - AC-5 (Separation of Duties)
  - AC-6 (Least Privilege)
  total_controls: 18
compliance_standards:
  - FedRAMP Baseline
  - SOC2 Type II
  - ISO 27001
detection_signals:
  - principal gains permission
  - role modified
  - policy attached to resource
  - cross-account access granted
response_time_slo: <5 seconds
free_trial_days: 14
target_customer:
  - Organizations with complex IAM
  - Multi-account AWS/GCP environments
  - Zero-trust enforcement teams
```

### 3. Change Governance Guard

**Purpose**: Prevent unsafe deployments with pre-deployment evidence validation.

```yaml
sku_id: sku_change_governance_guard
sku_name: Change Governance Guard
pricing:
  annual_flat: $20000-$200000
  usage_based: $0.75 per deployment gated
  regions: [all]
control_coverage:
  - CM-1 (Configuration Management Policy)
  - CM-2 (Baseline Configuration)
  - CM-3 (Change Control)
  - CM-5 (Access Restrictions)
  - CM-6 (Configuration Settings)
  total_controls: 16
compliance_standards:
  - FedRAMP Baseline
  - HIPAA BAA
  - PCI-DSS
deployment_gates:
  - compliance evidence present
  - security review approval obtained
  - cost impact estimate reviewed
  - rollback plan documented
deployments_per_month: 500 (baseline)
free_trial_days: 14
target_customer:
  - High-change-velocity teams
  - Organizations with deployment freezes
  - Regulated industries (healthcare, finance, government)
```

### 4. Signal Storm Governor

**Purpose**: Maintain system stability under abnormal event load (chaos engineering).

```yaml
sku_id: sku_signal_storm_governor
sku_name: Signal Storm Governor
pricing:
  annual_flat: $15000-$150000
  usage_based: $2.00 per 1000 signals processed
  regions: [all]
control_coverage:
  - AU-3 (Content of Audit Records)
  - AU-12 (Audit Generation)
  - SC-5 (Denial of Service Protection)
  - SI-4 (Information System Monitoring)
  total_controls: 12
compliance_standards:
  - FedRAMP Baseline
  - NIST SP 800-53
signal_types:
  - alerting_system_signals
  - monitoring_platform_signals
  - application_error_signals
  - infrastructure_state_signals
max_signal_throughput: 100000 signals/second
backpressure_guarantee: signals never dropped
free_trial_days: 14
target_customer:
  - Organizations with high-volume event streams
  - Real-time monitoring platforms
  - Chaos engineering programs
```

### 5. Zero-Trust Enforcer

**Purpose**: Implement continuous identity and access verification.

```yaml
sku_id: sku_zero_trust_enforcer
sku_name: Zero-Trust Enforcer
pricing:
  annual_flat: $30000-$300000
  usage_based: $1.50 per identity verification
  regions: [all]
control_coverage:
  - IA-1 (Identification and Authentication Policy)
  - IA-2 (Authentication)
  - IA-3 (Device Identification)
  - IA-4 (Identifier Management)
  - IA-5 (Authenticator Management)
  - IA-6 (Multifactor Authentication)
  - IA-7 (Cryptographic Module Authentication)
  total_controls: 22
compliance_standards:
  - FedRAMP High
  - NIST SP 800-53 Enhanced
  - Zero-Trust Architecture (NIST SP 800-207)
verification_signals:
  - user_identity_verified
  - device_posture_checked
  - location_confirmed
  - encryption_verified
  - time_of_access_valid
verification_slo: <100ms per identity
free_trial_days: 14
target_customer:
  - Defense contractors
  - Intelligence agencies
  - Critical infrastructure operators
```

### 6. Provenance Ledger

**Purpose**: Create immutable audit trails for FedRAMP compliance.

```yaml
sku_id: sku_provenance_ledger
sku_name: Provenance Ledger
pricing:
  annual_flat: $40000-$400000
  usage_based: $0.10 per receipt stored
  regions: [all]
control_coverage:
  - AU-2 (Audit Events)
  - AU-3 (Content of Audit Records)
  - AU-4 (Audit Storage)
  - AU-6 (Audit Review)
  - AU-7 (Audit Reduction)
  - AU-12 (Audit Generation)
  - AU-13 (Protection of Audit Information)
  total_controls: 24
compliance_standards:
  - FedRAMP
  - HIPAA BAA
  - SOX Compliance
provenance_types:
  - data_lineage
  - control_decisions
  - policy_changes
  - access_events
  - infrastructure_changes
retention_policy: immutable (7 years minimum)
query_slo: <200ms for audit trail retrieval
free_trial_days: 14
target_customer:
  - Federal agencies requiring immutable audit logs
  - Regulated industries (financial, healthcare)
  - Organizations undergoing compliance audits
```

### 7. Regression Rollback Guard

**Purpose**: Automated rollback on control regression detection.

```yaml
sku_id: sku_regression_rollback_guard
sku_name: Regression Rollback Guard
pricing:
  annual_flat: $25000-$250000
  usage_based: $0.50 per regression detected
  regions: [all]
control_coverage:
  - CA-7 (Continuous Monitoring)
  - CM-3 (Change Control)
  - CM-9 (Configuration Management)
  - SI-12 (Information Handling and Retention)
  total_controls: 14
compliance_standards:
  - FedRAMP Baseline
  - ISO 27001
regression_detection:
  - control passed → control failing
  - security group rules removed
  - encryption disabled
  - logging disabled
  - access permissions widened
automated_rollback: deployment → pre-deployment state
rollback_slo: <30 seconds
human_approval_required: yes (configurable)
free_trial_days: 14
target_customer:
  - High-availability environments
  - Automated CI/CD pipelines
  - Organizations with rapid deployment cycles
```

### 8. Environment Baseline Guard

**Purpose**: Maintain golden baseline configurations across all environments.

```yaml
sku_id: sku_environment_baseline_guard
sku_name: Environment Baseline Guard
pricing:
  annual_flat: $20000-$200000
  usage_based: $0.25 per environment scanned
  regions: [all]
control_coverage:
  - CM-2 (Baseline Configuration)
  - CM-5 (Access Restrictions)
  - CM-8 (System Component Inventory)
  - CM-9 (Configuration Management)
  total_controls: 12
compliance_standards:
  - FedRAMP Baseline
  - CIS Benchmarks
  - DISA Security Technical Implementation Guides (STIGs)
baseline_artifacts:
  - network configuration
  - storage configuration
  - compute instance settings
  - security group rules
  - IAM roles and policies
environment_types: [dev, staging, prod, gov-cloud, hybrid]
drift_detection: automated daily scan
approval_workflow: yes (for changes to baseline)
free_trial_days: 14
target_customer:
  - Multi-environment deployments
  - Organizations with configuration drift issues
  - Government agencies requiring baseline alignment
```

### 9. Data Integrity Guard

**Purpose**: Continuous validation of data quality and completeness.

```yaml
sku_id: sku_data_integrity_guard
sku_name: Data Integrity Guard
pricing:
  annual_flat: $35000-$350000
  usage_based: $0.80 per data integrity check
  regions: [all]
control_coverage:
  - SI-1 (Information System and Communications Protection Policy)
  - SI-7 (Software, Firmware, and Information Integrity)
  - SI-10 (Information System Monitoring)
  - SI-16 (Memory Protection)
  total_controls: 16
compliance_standards:
  - FedRAMP Baseline
  - HIPAA BAA
  - PCI-DSS
data_checks:
  - schema validation
  - completeness verification
  - referential integrity
  - cryptographic hash verification
  - anomaly detection
  - PII detection
continuous_monitoring: real-time
alert_slo: <1 minute for integrity violations
free_trial_days: 14
target_customer:
  - Organizations handling sensitive data
  - Healthcare and financial institutions
  - Regulated data pipelines
```

### 10. Budget Spike Guard

**Purpose**: Prevent unexpected cloud cost overruns.

```yaml
sku_id: sku_budget_spike_guard
sku_name: Budget Spike Guard
pricing:
  annual_flat: $20000-$200000
  usage_based: $0.15 per cost forecast
  regions: [aws, gcp, azure]
control_coverage:
  - CA-1 (Security Assessment and Authorization Policy)
  - CA-7 (Continuous Monitoring)
  - SC-7 (Boundary Protection)
  total_controls: 8
compliance_standards:
  - Enterprise Cost Management
  - OMB Circular A-130
cost_signals:
  - compute costs trending up
  - data transfer costs spiking
  - storage costs increasing
  - new resource types deployed
spike_threshold: 20% increase from baseline
anomaly_detection: automated
response_actions:
  - alert on spike
  - trigger cost review
  - enforce approval gate for further resources
  - auto-scale down (optional)
cost_savings_guarantee: 15% cost reduction typical
free_trial_days: 14
target_customer:
  - Organizations with cost overrun issues
  - Multi-cloud deployments
  - FinOps teams
```

### 11. Compliance Monitor

**Purpose**: Continuous compliance monitoring against all standards.

```yaml
sku_id: sku_compliance_monitor
sku_name: Compliance Monitor
pricing:
  annual_flat: $30000-$300000
  usage_based: $1.00 per compliance check
  regions: [all]
control_coverage:
  - CA-1 (Security Assessment and Authorization Policy)
  - CA-2 (Security Assessments)
  - CA-5 (Plan of Action and Milestones)
  - CA-6 (Security Authorization)
  - CA-7 (Continuous Monitoring)
  - CA-9 (Internal System Connections)
  total_controls: 24
compliance_standards:
  - FedRAMP (all levels)
  - HIPAA BAA
  - PCI-DSS
  - SOC2 Type II
  - ISO 27001
  - NIST SP 800-53
  - CIS Benchmarks
automated_assessments: daily
report_generation: weekly + on-demand
executive_dashboard: built-in
remediation_guidance: automated
free_trial_days: 14
target_customer:
  - Organizations with multiple compliance obligations
  - Audit and compliance teams
  - Enterprises seeking audit efficiency
```

### 12. Tenant Isolation Governors

**Purpose**: Enforce strict isolation between customer tenants in shared infrastructure.

```yaml
sku_id: sku_tenant_isolation_governors
sku_name: Tenant Isolation Governors
pricing:
  annual_flat: $40000-$400000
  usage_based: $0.50 per tenant isolation check
  regions: [all]
control_coverage:
  - AC-2 (Account Management)
  - AC-3 (Access Enforcement)
  - AC-4 (Information Flow Enforcement)
  - AC-12 (Session Termination)
  - CA-3 (System Interconnections)
  - CA-9 (Internal System Connections)
  total_controls: 20
compliance_standards:
  - FedRAMP Baseline
  - Multi-Tenant SaaS Guidelines
isolation_types:
  - network isolation (VPC, security groups)
  - data isolation (RBAC, attribute-based access control)
  - compute isolation (containers, VMs, Kubernetes namespaces)
  - storage isolation (separate buckets, encrypted keys)
  - audit isolation (separate audit trails)
continuous_verification: real-time
breach_detection: <100ms response time
free_trial_days: 14
target_customer:
  - SaaS platforms serving government
  - Multi-tenant cloud platforms
  - Contractors requiring strict customer isolation
```

### 13. Policy Pack Compiler

**Purpose**: Convert compliance standards into executable policies.

```yaml
sku_id: sku_policy_pack_compiler
sku_name: Policy Pack Compiler
pricing:
  annual_flat: $25000-$250000
  usage_based: $2.00 per policy compiled
  regions: [all]
control_coverage:
  - CA-1 (Policy and Procedures)
  - CM-1 (Configuration Management Policy)
  - CM-2 (Baseline Configuration)
  - CM-3 (Change Control)
  - IA-1 (Identification and Authentication Policy)
  - SC-1 (Security Architecture and Design)
  total_controls: 18
compliance_standards:
  - FedRAMP Baseline
  - NIST SP 800-53
  - CIS Benchmarks
policy_types:
  - Terraform/CloudFormation policies
  - OPA/Rego policies
  - AWS Service Control Policies (SCPs)
  - GCP Organization Policies
  - Azure Policies
  - Kubernetes admission controller policies
policy_generation: automated from RDF ontology
testing_included: yes (syntax + semantic validation)
free_trial_days: 14
target_customer:
  - Infrastructure-as-Code teams
  - Policy-as-Code programs
  - Organizations automating compliance
```

### 14. Receipt Verifier

**Purpose**: Customer-facing tool to verify and audit all ggen receipts.

```yaml
sku_id: sku_receipt_verifier
sku_name: Receipt Verifier
pricing:
  annual_flat: $15000-$150000
  usage_based: $0.10 per receipt verified
  regions: [all]
control_coverage:
  - AU-6 (Audit Review)
  - AU-7 (Audit Reduction and Report Generation)
  - SI-4 (Information System Monitoring)
  total_controls: 8
compliance_standards:
  - FedRAMP Baseline
  - SOC2 Type II
verification_capabilities:
  - signature verification (SHA256)
  - receipt chain verification (linked hashes)
  - audit trail replay
  - bill reconciliation against receipts
  - control coverage verification
  - evidence retrieval by control ID
customer_portal: yes (web + API)
api_access: yes (RESTful + GraphQL)
batch_verification: yes (1000+ receipts)
free_trial_days: 14
target_customer:
  - Compliance and audit teams
  - Finance teams (bill verification)
  - Customers requiring receipt transparency
```

### 15. Audit Readiness Pack

**Purpose**: Complete compliance preparation for government audits.

```yaml
sku_id: sku_audit_readiness_pack
sku_name: Audit Readiness Pack
pricing:
  annual_flat: $50000-$500000
  usage_based: $1.50 per audit preparation task
  regions: [all]
control_coverage:
  - CA-1 (Security Assessment and Authorization Policy)
  - CA-2 (Security Assessments)
  - CA-3 (System Interconnections)
  - CA-4 (Security Authorization)
  - CA-5 (Plan of Action and Milestones)
  - CA-6 (Security Authorization)
  - CA-7 (Continuous Monitoring)
  - CA-8 (Penetration Testing)
  - CA-9 (Internal System Connections)
  total_controls: 48
compliance_standards:
  - FedRAMP (all levels)
  - FISMA
  - DIACAP
  - DoD RMF
audit_preparation_tasks:
  - system security plan generation
  - control matrix development
  - evidence collection
  - test plan generation
  - vulnerability assessment report
  - continuous monitoring dashboard
  - audit readiness scorecard
  - remediation tracking
pre_audit_assessment: included
audit_period_support: 24/7 (Enterprise only)
free_tier_availability: true
free_trial_days: 14 (upgradeable to annual)
target_customer:
  - Federal agencies preparing for ATO
  - Contractors seeking FedRAMP certification
  - Government customers in DoD/Intel communities
```

---

## Pricing Matrix

### Annual Flat-Rate Pricing

| SKU | Starter | Standard | Enterprise |
|-----|---------|----------|------------|
| ATO Guard Pack | $50,000 | $250,000 | $500,000 |
| Permission Drift Guard | $25,000 | $125,000 | $250,000 |
| Change Governance Guard | $20,000 | $100,000 | $200,000 |
| Signal Storm Governor | $15,000 | $75,000 | $150,000 |
| Zero-Trust Enforcer | $30,000 | $150,000 | $300,000 |
| Provenance Ledger | $40,000 | $200,000 | $400,000 |
| Regression Rollback Guard | $25,000 | $125,000 | $250,000 |
| Environment Baseline Guard | $20,000 | $100,000 | $200,000 |
| Data Integrity Guard | $35,000 | $175,000 | $350,000 |
| Budget Spike Guard | $20,000 | $100,000 | $200,000 |
| Compliance Monitor | $30,000 | $150,000 | $300,000 |
| Tenant Isolation Governors | $40,000 | $200,000 | $400,000 |
| Policy Pack Compiler | $25,000 | $125,000 | $250,000 |
| Receipt Verifier | $15,000 | $75,000 | $150,000 |
| Audit Readiness Pack | $50,000 | $250,000 | $500,000 |

### Usage-Based Pricing (Optional)

Each SKU has a usage-based pricing option (see individual SKU definitions above).

---

## Product Bundles

### Bundle 1: Defense Pack

**SKUs**: ATO Guard Pack + Permission Drift Guard + Change Governance Guard + Signal Storm Governor + Zero-Trust Enforcer

**Price**: $110,000/year (Starter tier) — **20% discount** vs. individual ($137,500)

**Target**: Federal agencies, defense contractors, intelligence community.

**Coverage**: 48 + 18 + 16 + 12 + 22 = **116 controls** across FedRAMP + FISMA + Zero-Trust.

### Bundle 2: Data Pack

**SKUs**: Data Integrity Guard + Budget Spike Guard + Provenance Ledger

**Price**: $75,000/year (Starter tier) — **15% discount** vs. individual ($95,000)

**Target**: Organizations with data-heavy compliance requirements, financial institutions.

**Coverage**: 16 + 8 + 24 = **48 controls** across data quality, cost management, and audit.

### Bundle 3: Operational Excellence Pack

**SKUs**: Environment Baseline Guard + Regression Rollback Guard + Compliance Monitor + Tenant Isolation Governors

**Price**: $100,000/year (Starter tier) — **17% discount** vs. individual ($115,000)

**Target**: DevOps teams, SaaS platforms, multi-tenant infrastructure operators.

**Coverage**: 12 + 14 + 24 + 20 = **70 controls** across baseline, change management, compliance, and isolation.

### Bundle 4: Complete Government Platform

**SKUs**: All 15 SKUs

**Price**: $400,000/year (Enterprise tier) — **40% discount** vs. individual ($3.6M)

**Target**: Large federal agencies, enterprise government contractors.

**Coverage**: **370+ controls** across all compliance standards (FedRAMP, FISMA, HIPAA, PCI-DSS, SOC2, ISO 27001, etc.).

---

## Free Tier

### Audit Readiness Pack — Free for 14 Days

The Audit Readiness Pack is available free to all new customers for 14 days:

**Eligibility**:
- New GCP Marketplace customer
- Any organization (government, commercial, nonprofit)
- Per GCP account (one free trial per account)

**Access Method**:
- Visit GCP Marketplace
- Click "Try Free" on Audit Readiness Pack listing
- Automatic provisioning to trial environment

**Trial Environment**:
- Subdomain: `{customer-short-name}.trial.ggen.com`
- Access: Web portal + REST API
- Data: Sample compliance ontology (FedRAMP Baseline)
- Limits: 100 control evaluations/day, no real-time data

**Upgrade Path**:
- End of trial: automatic upgrade notification
- Choose tier (Starter, Standard, Enterprise)
- Billing starts on upgrade date
- No credit card required for trial

**Data Retention**:
- Trial data deleted at trial end (unless upgraded)
- Evidence can be exported before deletion
- No data recovery after deletion

---

## SKU Updateability

### RDF-Driven SKU Generation

New SKUs are auto-generated from compliance standards:

```turtle
# Example: Auto-generate new SKU from NIST 800-53 Rev. 5
@prefix ggen: <https://ggen.io/sku/> .
@prefix fips199: <https://ggen.io/standards/fips199/> .

ggen:sku_nist_800_53_gen_class_a a ggen:SKU ;
  rdfs:label "NIST 800-53 Rev 5 — General (Moderate)" ;
  ggen:coverage fips199:AC ;
  ggen:coverage fips199:AU ;
  ggen:coverage fips199:AT ;
  ggen:coverage fips199:CA ;
  ggen:coverage fips199:CM ;
  ggen:coverage fips199:IA ;
  ggen:coverage fips199:SI ;
  ggen:annual_price_starter ?START ;  # Computed from control count
  ggen:annual_price_enterprise ?ENT .  # Computed from control count
```

When gigen encounters a new standard in the ontology, it:
1. Extracts all controls from the standard
2. Computes pricing: `annual_price = base_price + (control_count × price_per_control)`
3. Generates SKU template (marketplace-listing-template.md)
4. Deploys to Marketplace
5. Emits receipt with new SKU ID

---

## Compliance Control Mapping

### Cross-SKU Control Coverage

Some controls are covered by multiple SKUs (for bundling discounts):

```json
{
  "control_id": "AC-2",
  "control_name": "Account Management",
  "covered_by_skus": [
    "sku_ato_guard_pack",
    "sku_permission_drift_guard",
    "sku_zero_trust_enforcer",
    "sku_tenant_isolation_governors"
  ],
  "coverage_level": {
    "sku_ato_guard_pack": "BASIC",
    "sku_permission_drift_guard": "ADVANCED",
    "sku_zero_trust_enforcer": "COMPREHENSIVE",
    "sku_tenant_isolation_governors": "MULTI_TENANT"
  }
}
```

For bundle pricing, redundant coverage is factored into discount calculations.

---

## Definition of Done

- [ ] All 15 SKUs documented with ID, name, pricing, control coverage
- [ ] Each SKU includes: annual flat pricing range, usage-based pricing (optional)
- [ ] Each SKU lists target customers and use cases
- [ ] Each SKU specifies free trial duration (14 days typical)
- [ ] Pricing matrix table created showing all tiers
- [ ] 4 product bundles defined with discount percentages
- [ ] Bundle pricing verified (discount calculations correct)
- [ ] Free tier documented: Audit Readiness Pack, 14-day trial
- [ ] Trial access method documented (GCP Marketplace → Try Free)
- [ ] Trial data retention policy documented
- [ ] Trial upgrade path defined
- [ ] SKU updateability explained (RDF-driven auto-generation)
- [ ] Compliance control mapping documented (controls → multiple SKUs)
- [ ] Cross-SKU bundling discounts calculated
- [ ] New SKU auto-generation workflow described
- [ ] Links to marketplace-listing-template.md, entitlement-contract.md, billing-contract.md
- [ ] All pricing in USD currency specified
- [ ] Regional availability noted (us-gov-west-1, all regions, etc.)
- [ ] All receipts include: timestamp, sku_id, account_id, action, decision
- [ ] Turtle/RDF examples for SKU auto-generation provided

---

## Receipt Contract

Every SKU operation generates a receipt:

```json
{
  "execution_id": "sku_op_2026_01_25_001",
  "timestamp": "2026-01-25T14:32:15Z",
  "sku_id": "sku_ato_guard_pack",
  "account_id": "customer_frb_ny_001",
  "action": "SKU_PURCHASED|SKU_BUNDLE_SELECTED|SKU_TIER_UPGRADED|SKU_TRIAL_ACTIVATED|SKU_RENEWED",
  "decision": "ACCEPT|REJECT|PENDING",
  "sku_name": "ATO Guard Pack",
  "tier_selected": "Standard",
  "annual_price": 250000,
  "bundle_discount_percent": 0,
  "effective_price": 250000,
  "contract_start": "2026-01-25T00:00:00Z",
  "contract_end": "2027-01-24T23:59:59Z",
  "auto_renew": true,
  "signature": "sha256:hash_of_receipt"
}
```

---

**Last Updated**: 2026-01-25 | **Version**: 1.0 | **Status**: Production
