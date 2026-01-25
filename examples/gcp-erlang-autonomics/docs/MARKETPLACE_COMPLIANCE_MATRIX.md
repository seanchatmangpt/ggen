# Marketplace Compliance Matrix

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Compliance-Ready
**Audience**: Compliance, Legal, Auditors

---

## Table of Contents

1. [GDPR (EU Data Protection)](#gdpr-eu-data-protection)
2. [HIPAA (Healthcare Privacy)](#hipaa-healthcare-privacy)
3. [SOC2 Type II (Cloud Security)](#soc2-type-ii-cloud-security)
4. [PCI-DSS (Payment Card Security)](#pci-dss-payment-card-security)
5. [FedRAMP (US Government)](#fedramp-us-government)
6. [Compliance Control Framework](#compliance-control-framework)
7. [Audit & Evidence Collection](#audit--evidence-collection)
8. [Remediation Procedures](#remediation-procedures)

---

## GDPR (EU Data Protection)

### Overview

GDPR governs personal data processing for EU residents. The marketplace must:
- Obtain explicit consent (Article 6)
- Enable data subject rights (Articles 12-22)
- Maintain data retention policies (Article 5)
- Report breaches within 72h (Article 33)
- Maintain DPA with data processors (Article 28)

### Control Mapping

| GDPR Article | Requirement | Implementation | Governor | Evidence |
|--------------|-------------|-----------------|----------|----------|
| **Art 6: Lawfulness** | Consent/legitimate interest | Consent form on signup, T&Cs acceptance | ComplianceGovernor | `audit/gdpr/consent_records.json` |
| **Art 12-22: Subject Rights** | Access, rectification, erasure, objection | Customer Account Gov + Compliance Gov | CustomerAccountGovernor, ComplianceGovernor | `audit/gdpr/subject_access_requests.log` |
| **Art 17: Right to Erasure** | "Right to be forgotten" | Anonymize PII on deletion | ComplianceGovernor | `audit/gdpr/deletion_audits.json` |
| **Art 5: Data Minimization** | Collect only necessary data | No SSN storage (payment processor handles) | Billing Governor | Schema validation in product catalog |
| **Art 32: Security** | Appropriate security measures | Encryption at rest, TLS in transit, IAM controls | Multi-Tenant Governor | Cloud Security Command Center |
| **Art 33: Breach Notification** | Notify DPA within 72h | Automated incident response | ComplianceGovernor | `audit/breaches/incident_reports.json` |
| **Art 28: DPA** | Data Processor Agreement | Stripe DPA, GCP DPA signed | Billing Governor | Legal agreements in contracts repo |
| **Art 25: Data Protection by Design** | Privacy-first architecture | FSM validates data flows, minimal storage | All governors | Architecture documentation |

### GDPR-Specific Controls

#### Control: Consent Management

```bash
# Verify consent records exist
ggen audit-export --control GDPR \
  --start-date 2026-01-01 \
  --metric consent_rate

# Output should show:
# consent_rate: 100%  (all customers have explicit consent)
# consent_types: [marketing_email, sms_notifications, data_processing]
# consent_version: 1.2  (customer agreed to latest version)
```

#### Control: Subject Access Requests (SAR)

```bash
# Process SAR within 30 days
ggen gdpr-subject-access-request \
  --customer-id cust_abc123 \
  --export-format json

# Outputs:
# - All personal data held
# - All transactions
# - All audit trail entries
# - Data source documentation

# Deadline tracking
ggen gdpr-sar-deadline \
  --request-id sar_001 \
  --deadline-days 30
```

#### Control: Data Retention

```bash
# GDPR requires data deletion after retention period expires
# Default retention: 2 years from account closure
ggen gdpr-data-retention \
  --policy gdpr_default \
  --execute true

# What happens:
# 1. Identify data > 2 years old
# 2. Anonymize: Remove names, emails, addresses
# 3. Pseudonymize: Replace customer_id with hash
# 4. Archive to cold storage
# 5. Verify deletion: Test that PII is gone
```

#### Control: Right to Erasure

```bash
# Customer requests deletion (Art. 17)
ggen gdpr-right-to-erasure \
  --customer-id cust_abc123 \
  --reason customer_request

# Execution:
# 1. Validate request (ownership verification)
# 2. Audit: Mark all data for deletion
# 3. Anonymize: PII → null or hash
# 4. Verify: Confirm PII no longer accessible
# 5. Deadline: Complete within 30 days
# 6. Notification: Send deletion confirmation
```

### GDPR Compliance Checklist

```yaml
GDPR Compliance Checklist:
  Consent Management:
    ✓ Consent form prominent on signup (Art. 6)
    ✓ Separate checkboxes for each use (granular consent)
    ✓ Consent records timestamped and immutable
    ✓ Version control for T&Cs changes
    ✓ Easy withdrawal mechanism

  Subject Rights:
    ✓ SAR response within 30 days (Art. 12)
    ✓ Data export in portable format (Art. 20)
    ✓ Right to rectification (Art. 16)
    ✓ Right to objection (Art. 21)
    ✓ Decision transparency (Art. 22)

  Data Protection:
    ✓ Encryption at rest (AES-256)
    ✓ TLS 1.2+ for transit
    ✓ Data retention policies (<2 years)
    ✓ Breach notification (72h)
    ✓ DPA signed with processors

  Accountability:
    ✓ Privacy impact assessment (DPIA)
    ✓ Processor list maintained
    ✓ Audit trail immutable
    ✓ Documentation complete
```

---

## HIPAA (Healthcare Privacy)

### Overview

HIPAA applies if customer uses system to process Protected Health Information (PHI):
- Maintain confidentiality of PHI
- Ensure integrity of PHI
- Ensure availability of PHI (when needed)
- Audit PHI access
- Manage business associate agreements

### Control Mapping

| HIPAA Rule | Requirement | Implementation | Governor | Evidence |
|------------|-------------|-----------------|----------|----------|
| **§164.308(a)(1): Access controls** | Restrict PHI access by role | IAM policies with least-privilege | MultiTenantGovernor | Cloud IAM audit logs |
| **§164.312(a)(2): Encryption** | Encrypt PHI at rest & transit | AES-256 + TLS 1.2+ | All governors | GCP crypto module logs |
| **§164.312(b): Audit controls** | Log all PHI access | CloudAudit logs every access | ComplianceGovernor | `audit/hipaa/phi_access.log` |
| **§164.308(a)(5): Breach notification** | Notify HHS if breach affects >500 patients | Automated incident response | ComplianceGovernor | Breach incident tracker |
| **§164.410: Notification rule** | Individual notification within 60 days | Customer notification automation | CustomerAccountGovernor | Notification audit trail |
| **§164.308(a)(7): Minimum necessary** | Only access needed PHI | Data minimization in FSM | All governors | Schema audits |
| **§164.308(a)(8): Workforce security** | Employee access controls | 2FA + role-based access | Multi-Tenant Governor | Employee access logs |

### HIPAA-Specific Controls

#### Control: PHI Access Audit

```bash
# All PHI access must be logged
ggen hipaa-phi-access-audit \
  --start-date 2026-01-01 \
  --end-date 2026-01-31 \
  --export audit.csv

# Output columns:
# timestamp, actor, action (read|write|delete), phi_field, result
# Examples:
# 2026-01-15T10:23:45Z, user_123, read, patient_mrn, success
# 2026-01-15T10:24:12Z, user_123, read, insurance_plan, success
# 2026-01-15T10:25:00Z, app_service, write, diagnosis_code, success
```

#### Control: Encryption & Access

```bash
# Verify PHI encryption
ggen hipaa-encryption-verify \
  --data-class sensitive \
  --check-at-rest true \
  --check-in-transit true

# Output should confirm:
# ✓ All sensitive data encrypted at rest (AES-256)
# ✓ All APIs use TLS 1.2+
# ✓ Database encryption enabled
# ✓ Backups encrypted
```

#### Control: Business Associate Agreement

```bash
# Track BAA status with vendors
ggen hipaa-baa-status \
  --vendor stripe \
  --status signed

ggen hipaa-baa-status \
  --vendor gcp \
  --status signed

ggen hipaa-baa-status \
  --vendor sendgrid \
  --status pending  # ← Action required
```

#### Control: Breach Notification

```bash
# If breach affects PHI:
ggen hipaa-breach-notification \
  --breach-id brch_001 \
  --affected-individuals 250 \
  --notification-deadline $(date -d '+60 days' +%Y-%m-%d)

# Workflow:
# 1. Notify HHS Breach Notification System
# 2. Notify each affected individual (within 60 days)
# 3. Notify media if >500 affected (without unreasonable delay)
# 4. Document: breach details, individuals affected, steps taken
```

### HIPAA Compliance Checklist

```yaml
HIPAA Compliance Checklist:
  Administrative Safeguards:
    ✓ Security officer designated
    ✓ Risk analysis completed
    ✓ BAA signed with business associates
    ✓ Employee training documented
    ✓ Sanction policies in place

  Physical Safeguards:
    ✓ Facility access controls (VPC restrictions)
    ✓ Workstation security policies
    ✓ Device & media controls (encryption)
    ✓ No physical access without MFA

  Technical Safeguards:
    ✓ Access controls (IAM, RBAC)
    ✓ Encryption at rest & in transit
    ✓ Audit controls (logging all access)
    ✓ Integrity controls (data validation)
    ✓ Transmission security (TLS)

  Organizational & Policies:
    ✓ Documentation of all controls
    ✓ Business associate agreements
    ✓ Breach notification procedures
    ✓ Breach log maintained
```

---

## SOC2 Type II (Cloud Security)

### Overview

SOC2 Type II audits security, availability, integrity, and confidentiality controls over time. Certificate valid for 6 months post-audit.

### Control Mapping

| SOC2 TSC | Requirement | Implementation | Governor | Evidence |
|----------|-------------|-----------------|----------|----------|
| **CC6.1: Logical/Physical Access** | Restrict access by role | IAM policies, VPC controls | Multi-Tenant Governor | Cloud IAM audit logs |
| **CC7.1: System Monitoring** | Detect anomalies & deviations | Cloud Monitoring + alerts | All governors | Monitoring dashboard |
| **CC7.2: Monitoring Activity** | Log user/system activity | Cloud Audit Logs (30-day retention) | ComplianceGovernor | Audit log retention policy |
| **CC9.1: Logical/Physical Segregation** | Isolate customer data | Multi-tenant isolation + encryption | Multi-Tenant Governor | Tenant isolation tests |
| **A1.1: Availability SLO** | 99.95% uptime | Cloud Run auto-scaling | Orchestrator | Monthly uptime reports |
| **A1.2: Performance SLOs** | Latency <500ms P99 | Decision latency monitoring | Orchestrator | Latency dashboard |
| **I1.1: Completeness & Accuracy** | Data integrity | FSM validation + cryptographic proofs | All governors | Integrity verification logs |
| **C1.1: Confidentiality Protection** | Encrypt sensitive data | AES-256 + key rotation | All governors | Encryption audit logs |

### SOC2-Specific Controls

#### Control: CC6.1 — Logical Access

```bash
# Export IAM policy for audit
ggen soc2-iam-policy-export \
  --format json > cc6.1-iam-policy.json

# Verify:
# - No default "Editor" roles
# - Least-privilege roles assigned
# - Service accounts rotated annually
# - API keys have 90-day rotation
# - No hardcoded credentials in code

ggen soc2-verify-least-privilege \
  --check-service-accounts true \
  --check-api-keys true
```

#### Control: CC7.1 — System Monitoring

```bash
# Monitoring dashboard: real-time health
# URL: https://console.cloud.google.com/monitoring/dashboards/custom/marketplace-governors

ggen soc2-monitoring-verify \
  --metrics-collected true \
  --alerts-configured true \
  --alert-count 15  # Minimum 15 alerts

# Expected alerts:
# 1. CPU > 80% for 5 min
# 2. Error rate > 1% for 10 min
# 3. Decision latency P99 > 5 sec
# 4. Unauthorized API calls
# ... 12 more
```

#### Control: CC7.2 — Activity Logging

```bash
# Verify audit logs are immutable and retained
ggen soc2-audit-log-verify \
  --retention-days 30 \
  --export-sample true

# Check:
# ✓ All API calls logged
# ✓ User identity in every log
# ✓ Action (read/write/delete) clear
# ✓ Timestamp immutable
# ✓ Retention >= 30 days
```

#### Control: A1.1 — Availability

```bash
# Monthly uptime calculation
ggen soc2-availability-report \
  --month 2026-01 \
  --target-slo 99.95

# Output should show:
# Total time: 744 hours (31 days)
# Downtime allowed: 22.2 minutes (99.95% SLO)
# Actual downtime: 8 minutes
# Achievement: 99.98% ✓ COMPLIANT

# Export evidence:
# - Incident reports
# - Maintenance logs
# - Recovery time logs
```

#### Control: I1.1 — Data Integrity

```bash
# Verify data consistency via FSM invariants
ggen soc2-integrity-verify \
  --check-entitlements true \
  --check-billing true \
  --check-subscriptions true

# Tests:
# 1. Invoice-payment pairs match (no orphans)
# 2. Entitlement states valid (no impossible combos)
# 3. Subscription timeline consistent (no gaps)
# 4. Audit trail hash chain unbroken (no tampering)
# 5. Version numbers monotonic increasing
```

### SOC2 Compliance Checklist

```yaml
SOC2 Type II Compliance (CC & A Criteria):
  CC6: Logical Access Controls
    ✓ User access provisioning documented
    ✓ Least-privilege access verified
    ✓ Quarterly access reviews documented
    ✓ Termination procedures in place
    ✓ Service account rotation annual

  CC7: System Monitoring
    ✓ Real-time monitoring dashboard
    ✓ Alerts for security events
    ✓ Incident response procedures
    ✓ Change log maintained
    ✓ Reviews documented

  CC9: Logical Security Segregation
    ✓ Tenant data isolated by VPC
    ✓ No cross-tenant data access
    ✓ Encryption prevents plaintext access
    ✓ Network policies enforced
    ✓ Tests verify isolation

  A1: Availability & Performance
    ✓ 99.95% uptime SLO
    ✓ <500ms latency P99
    ✓ Auto-scaling configured
    ✓ Backup/recovery tested
    ✓ Disaster recovery plan

  I1: Integrity & Accuracy
    ✓ Data validation on input
    ✓ Audit trail immutable
    ✓ Cryptographic checksums
    ✓ Batch integrity reconciliation
    ✓ Error detection mechanisms
```

---

## PCI-DSS (Payment Card Security)

### Overview

PCI-DSS applies if system processes payment card data (even if Stripe handles processing). Requirements focus on cardholder data protection.

### Control Mapping

| PCI-DSS Req | Requirement | Implementation | Governor | Evidence |
|-------------|-------------|-----------------|----------|----------|
| **Req 2.4: Secure Config** | Change default passwords | Cloud Run no SSH, disabled defaults | Multi-Tenant Governor | GCP configuration audit |
| **Req 3.4: Render Card Data Unreadable** | Never store full PAN | Use Stripe tokenization | Billing Governor | Code audit shows tokens only |
| **Req 6.2: Security Patches** | Apply patches in 30 days | GCP handles patches, monthly rust updates | Build pipeline | Dependency audit logs |
| **Req 8.2.3: Account Lockout** | Lock after 6 failed attempts | Failed login tracking | Customer Account | `audit/security/failed_logins.log` |
| **Req 8.3: MFA** | Multi-factor authentication for admins | 2FA enforced, U2F/TOTP | IAM | GCP identity audit logs |
| **Req 10: Audit Logging** | Log all access to card data | Cloud Audit Logs, immutable | ComplianceGovernor | Audit log exports |
| **Req 11.3: Penetration Testing** | Annual pen test | Third-party pen test report | Security | `audit/pentesting/2026_report.pdf` |

### PCI-DSS-Specific Controls

#### Control: Never Store Full PAN

```rust
// ✅ CORRECT: Use tokenized payment
impl BillingGovernor {
    pub fn process_payment(&mut self, event: &PaymentEvent) -> Result<()> {
        // event contains Stripe token, not card number
        let stripe_token = &event.payment_token;  // "tok_visa_123"

        // Never store raw card number
        // Stripe handles card data, we only store:
        // - Last 4 digits
        // - Card expiry (masked)
        // - Token (opaque reference to Stripe)

        let payment_method = PaymentMethod {
            last_4: "4242",
            expiry_month: 12,
            expiry_year: 2027,
            stripe_token: stripe_token.clone(),  // Stripe's reference only
        };

        // Rest of processing...
        Ok(())
    }
}

// ❌ NEVER DO THIS:
// let pan = "4532 1234 5678 9010";  // VIOLATION
// store_in_database(pan);             // VIOLATION
```

#### Control: Secure Configuration

```bash
# Verify no SSH access to production
gcloud run services describe marketplace-orchestrator \
  --format='value(spec.template.spec.containers[0].securityContext)'

# Should output:
# allowPrivilegeEscalation: False
# runAsNonRoot: True
# readOnlyRootFilesystem: True

# Verify no default passwords
ggen pci-dss-default-password-scan \
  --check-admin-panel true \
  --check-database true

# Should find: NONE (all passwords changed)
```

#### Control: Account Lockout

```bash
# Failed login attempts tracked
ggen pci-dss-failed-login-report \
  --period last_30_days \
  --threshold 6

# Output:
# user_id | failed_attempts | locked_until | reason
# usr_123 | 6               | 2026-01-26   | Lockout triggered

# Verify lockout released after 30 minutes
# usr_123 | 0               | (not locked) | Lockout released
```

#### Control: Penetration Testing

```bash
# Annual third-party pen test
# Evidence file: audit/pentesting/2026_q1_report.pdf

ggen pci-dss-pentesting-verify \
  --report-path audit/pentesting/2026_q1_report.pdf \
  --verify-remediation true

# Pen test should cover:
# - Network security
# - Application security
# - Authentication/authorization
# - Data protection
# - Incident response
```

### PCI-DSS Compliance Checklist

```yaml
PCI-DSS Compliance Checklist (Top Requirements):
  Requirement 2: Default Security
    ✓ Vendor default passwords changed
    ✓ Unnecessary services disabled
    ✓ Security protocols configured
    ✓ Documentation of config changes

  Requirement 3: Card Data Protection
    ✓ PCI-compliant data flow (Stripe handles cards)
    ✓ No full PAN storage
    ✓ No sensitive authentication data stored
    ✓ PCI DSS scoping documented

  Requirement 8: Authentication
    ✓ MFA required for all admin access
    ✓ Account lockout after 6 failed attempts
    ✓ Password minimum 8 characters
    ✓ Passwords changed every 90 days

  Requirement 10: Audit Logging
    ✓ All access to cardholder data logged
    ✓ Logs retained for 1 year minimum
    ✓ Logs protected from unauthorized access
    ✓ Log reviews documented

  Requirement 11: Testing
    ✓ Annual vulnerability scan
    ✓ Annual penetration test
    ✓ Quarterly network scanning
    ✓ Remediation of findings
```

---

## FedRAMP (US Government)

### Overview

FedRAMP authorizes cloud services for use by US federal agencies. Requires:
- Compliance with NIST SP 800-53 controls
- Continuous monitoring
- Annual assessment
- Authority to Operate (ATO)

### Control Mapping

| NIST Control | Requirement | Implementation | Governor | Evidence |
|--------------|-------------|-----------------|----------|----------|
| **AC-2: Account Management** | Authorized user accounts | GCP Cloud Identity | IAM | Account provisioning logs |
| **AC-3: Access Control** | Least-privilege access | IAM roles + VPC | Multi-Tenant Governor | IAM audit logs |
| **AU-2: Audit Events** | Determine events to audit | All API calls logged | ComplianceGovernor | Cloud Audit Logs |
| **SC-7: Boundary Protection** | Prevent unauthorized access | VPC + Cloud Armor | Multi-Tenant Governor | Network policy logs |
| **SI-4: Monitoring & Analysis** | Detect malicious activity | Cloud Monitoring + SOAR | ComplianceGovernor | Alert logs |
| **CP-9: System Backup** | Backup & recovery procedures | GCP automated backups | All governors | Backup verification logs |
| **IR-4: Incident Handling** | Incident response procedures | Automated + manual procedures | ComplianceGovernor | Incident logs |

### FedRAMP-Specific Controls

#### Control: AC-2 — Account Management

```bash
# Federal user accounts provisioned through IAM
ggen fedramp-account-provisioning \
  --audit-period 30days

# Output should show:
# - All federal users authorized
# - Access provisioned within 2 business days
# - Account reviews performed quarterly
# - Deprovisioning within 1 day of offboarding
```

#### Control: AU-2 — Audit Events

```bash
# FedRAMP requires audit of 14+ event types
ggen fedramp-audit-events-verify \
  --required-events [
    "authentication",
    "authorization_checks",
    "administrative_actions",
    "data_access",
    "data_modification",
    "data_deletion",
    "policy_changes",
    "privilege_escalation",
    "firewall_changes",
    "network_config_changes",
    "system_shutdowns",
    "security_events",
    "audit_log_access",
    "backup_restore"
  ]

# Verify all 14+ event types logged
```

#### Control: SI-4 — Monitoring

```bash
# Real-time monitoring for malicious activity
ggen fedramp-monitoring-verify \
  --check-ids-ips true \
  --check-log-monitoring true \
  --check-security-alerts true

# IDS/IPS: Detect intrusion attempts
# Log monitoring: Detect suspicious patterns
# Alerts: Notify on security events (<5 min)
```

#### Control: CP-9 — Backup & Recovery

```bash
# Backup testing required for FedRAMP
ggen fedramp-backup-test \
  --restore-point latest \
  --verify-data-integrity true \
  --document-recovery-time true

# Test should show:
# ✓ Backup created
# ✓ Backup restored to test environment
# ✓ Data integrity verified (checksums match)
# ✓ Recovery time < 4 hours (RTO)
```

### FedRAMP Compliance Checklist

```yaml
FedRAMP Compliance (NIST SP 800-53 Controls):
  Access Control (AC):
    ✓ AC-2: Account management procedures
    ✓ AC-3: Least-privilege access enforced
    ✓ AC-4: Information flow control
    ✓ AC-5: Separation of duties
    ✓ AC-6: Privilege minimization

  Audit & Accountability (AU):
    ✓ AU-2: 14+ audit event types logged
    ✓ AU-3: Audit records content
    ✓ AU-4: Audit storage (1 year)
    ✓ AU-5: Response to audit failures

  System & Communications Protection (SC):
    ✓ SC-7: Boundary protection (VPC)
    ✓ SC-13: Cryptographic controls (TLS, AES-256)
    ✓ SC-28: Encryption at rest

  Configuration Management (CM):
    ✓ CM-2: Baseline configuration
    ✓ CM-3: Change control procedures
    ✓ CM-5: Access restrictions on changes

  Contingency Planning (CP):
    ✓ CP-9: System backup & recovery
    ✓ CP-10: Information system recovery
    ✓ CP-13: Alternate processing site

  Incident Response (IR):
    ✓ IR-1: Incident response policy
    ✓ IR-4: Incident handling procedures
    ✓ IR-5: Incident monitoring
    ✓ IR-6: Incident reporting
```

---

## Compliance Control Framework

### Compliance Governor Orchestration

```
┌─────────────────────────────────────────────────────────┐
│  MarketplaceOrchestrator (All Governors)               │
├─────────────────────────────────────────────────────────┤
│                                                         │
│ ComplianceGovernor (FSM: GDPR/HIPAA/SOC2/PCI/FedRAMP)  │
│  ├─ Monitor: regulatory_framework_status               │
│  ├─ Detect: violation events                           │
│  ├─ Verify: controls implemented                       │
│  ├─ Audit: generate compliance reports                 │
│  └─ Escalate: violations → incident response           │
│                                                         │
│ All Other Governors:                                   │
│  ├─ EntitlementGovernor → Audit feature access        │
│  ├─ BillingGovernor → Audit payment processing        │
│  ├─ SubscriptionGovernor → Audit subscription data    │
│  ├─ CustomerAccountGovernor → Audit user data         │
│  ├─ QuotaSlaGovernor → Audit usage data               │
│  ├─ ProductCatalogGovernor → Audit product data       │
│  └─ MultiTenantGovernor → Audit data isolation        │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Compliance Event Types

```rust
pub enum ComplianceEvent {
    // Verification events
    VerificationDeadlineReached { framework: ComplianceFramework },
    VerificationPassed { framework: ComplianceFramework },
    VerificationFailed { framework: ComplianceFramework },

    // Violation events
    DataRetentionExpiring { days_remaining: u32 },
    DataRetentionExpired { framework: ComplianceFramework },
    UnauthorizedDataAccess { user_id: String },
    BreachDetected { breach_id: String, affected_count: u32 },

    // Audit events
    SubjectAccessRequest { customer_id: String, request_date: DateTime<Utc> },
    DataDeletionRequested { customer_id: String },
    AuditLogAccessed { audit_period: String },

    // Control events
    ControlMissing { framework: ComplianceFramework, control: String },
    ControlImplemented { framework: ComplianceFramework, control: String },
    ControlVerified { framework: ComplianceFramework },
}
```

---

## Audit & Evidence Collection

### Automated Evidence Collection

```bash
# Generate daily compliance report
ggen compliance-report-daily \
  --date 2026-01-25 \
  --frameworks [gdpr, hipaa, soc2, pci, fedramp] \
  --export-path audit/daily/2026-01-25.json

# Monthly compliance status
ggen compliance-report-monthly \
  --month 2026-01 \
  --include-violations true \
  --include-remediation-status true

# Audit-readiness check
ggen compliance-audit-readiness \
  --frameworks all \
  --check-evidence-completeness true \
  --check-documentation-quality true
```

### Evidence Retention

```yaml
Evidence Retention Requirements:
  GDPR:
    - Consent records: Lifetime + 7 years after account closure
    - Access logs: 1 year minimum
    - Subject access requests: 1 year
    - Data deletion records: Lifetime

  HIPAA:
    - PHI access logs: 6 years minimum
    - Breach incidents: 6 years
    - Business associate agreements: Lifetime

  SOC2 Type II:
    - Audit logs: 30 days (live) + 1 year archived
    - Incident reports: 2 years
    - Risk assessments: 1 year (rolling)

  PCI-DSS:
    - Audit logs: 1 year online, 3 years archived
    - Penetration test results: 1 year
    - Vulnerability scans: 1 year

  FedRAMP:
    - Audit logs: 1 year minimum
    - Change logs: 3 years
    - Incident reports: 3 years
    - Annual assessments: Lifetime
```

---

## Remediation Procedures

### Procedure #1: GDPR Violation Remediation

**Violation**: Customer data retained beyond 2-year retention policy

```bash
# 1. Identify affected customers
ggen gdpr-retention-violation-identify \
  --over-retention-days 30 \
  --output /tmp/affected_customers.csv

# 2. Calculate impact
wc -l /tmp/affected_customers.csv  # 523 affected customers

# 3. Notify legal team
# Email: legal@company.com
# Subject: GDPR Retention Violation - 523 customers
# Body: See detailed report in /tmp/affected_customers.csv

# 4. Remediation: Delete retained data
ggen gdpr-data-delete \
  --customer-file /tmp/affected_customers.csv \
  --dry-run true  # Preview first

# 5. Execute (after legal approval)
ggen gdpr-data-delete \
  --customer-file /tmp/affected_customers.csv \
  --execute true

# 6. Verify deletion
ggen gdpr-deletion-verify \
  --customer-file /tmp/affected_customers.csv \
  --confirm-pii-deleted true

# 7. Report to regulators (if required)
ggen gdpr-violation-report \
  --violation-id gdpr_ret_001 \
  --affected-count 523 \
  --remediation-date $(date +%Y-%m-%d) \
  --export /tmp/gdpr_violation_report.pdf
```

### Procedure #2: HIPAA Breach Remediation

**Violation**: Unauthorized access to PHI (500 patients affected)

```bash
# 1. Immediate containment (within 1 hour)
ggen hipaa-breach-containment \
  --breach-id brch_001 \
  --affected-patients 500 \
  --affected-data-types [medical_records, insurance_info] \
  --containment-actions [
    "revoke_attacker_access",
    "disable_compromised_credentials",
    "isolate_affected_database",
    "enable_enhanced_monitoring"
  ]

# 2. Notification to HHS (within 24 hours)
ggen hipaa-breach-notify-hhs \
  --breach-id brch_001 \
  --hhs-notification-id HHS_2026_001

# 3. Individual notification (within 60 days)
ggen hipaa-breach-notify-individuals \
  --breach-id brch_001 \
  --affected-patients 500 \
  --notification-method [email, certified_mail] \
  --deadline $(date -d '+60 days' +%Y-%m-%d)

# 4. Investigation & root cause analysis
ggen hipaa-breach-investigation \
  --breach-id brch_001 \
  --investigation-deadline $(date -d '+30 days' +%Y-%m-%d)

# 5. Remediation (implement fixes)
ggen hipaa-breach-remediation \
  --breach-id brch_001 \
  --remediation-deadline $(date -d '+90 days' +%Y-%m-%d)

# 6. Verification (confirm fixes work)
ggen hipaa-breach-verification \
  --breach-id brch_001 \
  --penetration-test true  # Re-test fixes
```

### Procedure #3: SOC2 Finding Remediation

**Finding**: Unencrypted backup storage discovered in audit

```bash
# 1. Assess severity
ggen soc2-finding-severity \
  --finding-id soc2_001 \
  --severity high  # Can impact availability

# 2. Create remediation plan
ggen soc2-remediation-plan-create \
  --finding-id soc2_001 \
  --root-cause "Backup storage created before encryption requirement"
  --remediation-steps [
    "1. Enable encryption on backup storage (AES-256)",
    "2. Re-encrypt existing backups",
    "3. Verify encryption via compliance check",
    "4. Update backup policy to enforce encryption",
    "5. Train team on backup procedures"
  ]
  --target-completion-date $(date -d '+30 days' +%Y-%m-%d)

# 3. Execute remediation
ggen backup-storage-enable-encryption \
  --location us-central1 \
  --encryption-algorithm AES-256 \
  --dry-run true

ggen backup-storage-enable-encryption \
  --location us-central1 \
  --encryption-algorithm AES-256 \
  --execute true

# 4. Verify remediation
ggen soc2-finding-verification \
  --finding-id soc2_001 \
  --confirm-encrypted true \
  --confirm-policy-updated true

# 5. Document evidence
ggen soc2-finding-close \
  --finding-id soc2_001 \
  --evidence-file audit/soc2/findings/soc2_001_remediation.pdf
```

---

**Last Updated**: January 2026
**Next Audit Date**: July 2026 (SOC2 Type II)
**Owner**: @compliance-team
**Slack**: #compliance-governance
