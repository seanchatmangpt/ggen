# Marketplace Incident Response Runbook

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Production-Ready
**Audience**: On-Call Engineers, SREs, Incident Commanders

---

## Table of Contents

1. [Incident Classification](#incident-classification)
2. [Payment Processing Failures](#payment-processing-failures)
3. [Compliance Violations](#compliance-violations)
4. [Customer Account Compromises](#customer-account-compromises)
5. [Cascade Failures](#cascade-failures)
6. [Data Breach Response](#data-breach-response)
7. [Decision Trees](#decision-trees)
8. [Communication Templates](#communication-templates)

---

## Incident Classification

### Severity Levels

**SEV-1 (Critical - Page Immediately)**
- Payment processing completely down
- Compliance violation (GDPR/HIPAA/PCI)
- Data corruption or security breach
- All customers affected
- Financial impact: > $10k/hour
- Response SLO: 5 minutes
- Resolution SLO: 1 hour

**SEV-2 (High - Page within 5 minutes)**
- Partial outage (some customers affected)
- SLA violation (customer not meeting uptime target)
- Governor in error state
- Data consistency issue
- Financial impact: $1k-$10k/hour
- Response SLO: 15 minutes
- Resolution SLO: 4 hours

**SEV-3 (Medium - Email, no immediate page)**
- Minor feature unavailable
- Performance degradation
- Quota/rate limiting issue
- No data loss
- Financial impact: < $1k/hour
- Response SLO: 1 hour
- Resolution SLO: 8 hours

**SEV-4 (Low - Monitoring alert only)**
- Cosmetic issues
- Documentation updates
- Non-critical alerts
- No customer impact
- No time pressure
- Can be resolved during business hours

---

## Payment Processing Failures

### Symptom: Billing Payment Success Rate < 95%

**Severity**: SEV-1

**Detection**:
```
Alert triggered: billing_payment_success_rate < 0.95 for 10 minutes
```

**Initial Assessment** (< 5 min):
```bash
# 1. Verify alert is real
ggen billing-status --check-payment-processor true

# Output should show:
# ✓ Stripe API: UP
# ✗ Payment success rate: 92% (BELOW SLO 99%)
# Current failures: 8 of 100 attempts in past hour

# 2. Check failure reason
ggen billing-failures-analyze \
  --start-time 2026-01-25T14:25:00Z \
  --end-time 2026-01-25T14:35:00Z

# Outputs:
# Failure reason distribution:
# - insufficient_funds: 40%
# - card_declined: 35%
# - processing_error: 15%  # ← THIS IS OUR FAULT
# - network_timeout: 10%
```

**Root Cause Identification**:

| Failure Reason | Root Cause | Action |
|---------------|-----------|---------
| insufficient_funds | Customer doesn't have money | Customer education, email support |
| card_declined | Card issue or fraud protection | Contact customer, offer retry |
| processing_error | Our system or Stripe API error | Investigate our logs + Stripe status |
| network_timeout | Network connectivity | Failover to backup processor |

**Decision Tree**:
```
Payment success rate < 95%?
├─ Check Stripe API status (https://status.stripe.com)
│  ├─ Stripe DOWN → Enable fallback payment processor (section 4.1.1)
│  └─ Stripe UP → Continue investigation
│
├─ Check our payment request validation
│  └─ Validation failing → Fix validation, re-deploy
│
├─ Check for idempotency key collisions
│  └─ Collisions found → Investigate payment deduplication
│
├─ Check database transaction state
│  └─ Deadlocks/locks → Investigate database
│
└─ Check for fraud filter activation
   └─ Filter too aggressive → Contact Stripe support
```

### Procedure: Failover to Backup Payment Processor

**Preconditions**:
- Primary processor (Stripe) unavailable for > 5 min
- Backup processor configured and tested
- Business approval to use backup

**Steps**:
```bash
# 1. Verify backup processor is ready
ggen payment-processor-health --processor backup_processor
# Expected: UP, ready to process

# 2. Check current queue depth
ggen billing-queue-status
# Shows: 234 pending payments in queue

# 3. Enable failover
ggen payment-processor-failover \
  --from stripe \
  --to backup_processor \
  --drain-existing-queue true

# 4. Monitor transition
watch -n 5 'ggen billing-status --check-all true'
# Watch for success rate to recover

# 5. Once Stripe recovers, failback
ggen payment-processor-failover \
  --from backup_processor \
  --to stripe \
  --drain-existing-queue true

# 6. Verify no duplicate charges
ggen billing-verify-no-duplicates \
  --failover-period 2026-01-25T14:25:00Z_2026-01-25T14:50:00Z \
  --check-idempotency true
```

**Verification**:
```
After failover:
✓ Backup processor receiving payments
✓ Success rate > 99%
✓ No duplicate charges
✓ No payment loss

After failback:
✓ Stripe receiving payments again
✓ All pending payments cleared from backup
✓ Success rate stable
✓ No customer impact
```

### Procedure: Payment Retry Queue Handling

**Symptoms**:
- Retry-1, Retry-2, Retry-3 queues growing
- Collection agency count increasing
- Customers calling with payment issues

**Steps**:
```bash
# 1. Assess queue size
ggen billing-retry-queues-status

# Output:
# retry_1: 542 payments
# retry_2: 128 payments
# retry_3: 23 payments
# collection_agency: 8 payments

# 2. If queues growing, investigate
ggen billing-retry-analysis \
  --queue retry_1 \
  --recent true

# Output:
# Stuck in retry_1:
# - Wrong payment method on file: 312 (45%)
# - Card expired: 181 (33%)
# - Insufficient funds: 49 (7%)
# - Network error: 23 (3%)
# - Processor issue: 12 (2%)

# 3. Notify customers with actionable steps
ggen customer-notification-send \
  --template payment_method_invalid \
  --audience "customers_with_stuck_payments" \
  --priority high \
  --include_update_payment_link true

# 4. Monitor queue clearance
watch -n 10 'ggen billing-retry-queues-status'

# 5. If queues not clearing, escalate to payments team
ggen incident-escalate \
  --severity SEV-1 \
  --team payments_team \
  --reason "Payment retry queues not clearing after customer notifications"
```

---

## Compliance Violations

### Symptom: GDPR/HIPAA/SOC2 Violation Detected

**Severity**: SEV-1

**Detection**:
```
Alert triggered: compliance_framework_status = non_compliant
```

**Immediate Actions** (< 15 min):
```bash
# 1. Identify which framework is non-compliant
ggen compliance-status --all-frameworks true

# Output:
# GDPR: COMPLIANT
# HIPAA: NON-COMPLIANT ← ISSUE HERE
# SOC2: COMPLIANT
# PCI-DSS: COMPLIANT
# FedRAMP: COMPLIANT

# 2. Identify specific violation
ggen compliance-violation-details \
  --framework HIPAA \
  --detail verbose

# Output:
# Violation type: PHI_Access_Unlogged
# Affected data: Patient MRN (Medical Record Number)
# Scope: Patient records from 2026-01-20 to 2026-01-25
# Affected patients: 2,847
# Severity: CRITICAL (unauthorized access)

# 3. Immediately notify compliance officer & legal
ggen incident-escalate \
  --severity SEV-1 \
  --team legal,compliance \
  --message "HIPAA violation: 2,847 patients affected"

# 4. If breach suspected, activate data breach response (section 6)
```

**Decision Tree: Compliance Violation**:
```
Compliance violation detected?
├─ Data retention expired?
│  ├─ YES → Execute retention policy (section 4.1)
│  └─ NO → Continue
│
├─ Unauthorized data access?
│  ├─ YES → Activate data breach response (section 6)
│  └─ NO → Continue
│
├─ KYC/AML verification failed?
│  ├─ YES → Lock customer account, escalate to fraud team (section 4.2)
│  └─ NO → Continue
│
├─ Encryption control missing?
│  ├─ YES → Enable encryption immediately (section 4.3)
│  └─ NO → Continue
│
└─ Audit trail integrity broken?
   └─ YES → CRITICAL: Activate data breach response (section 6)
```

### Procedure: Data Retention Expiration

**Scenario**: Customer account closed 2 years ago, retention period expired

```bash
# 1. Verify retention period exceeded
ggen data-retention-check \
  --customer-id cust_old_001 \
  --framework GDPR

# Output:
# Account closed: 2024-01-25
# Retention period: 2 years
# Expiration date: 2026-01-25 (TODAY)
# Status: EXPIRED (action required)

# 2. Execute anonymization
ggen data-anonymize \
  --customer-id cust_old_001 \
  --frameworks GDPR \
  --dry-run true

# Preview what will be deleted/anonymized:
# - customer_name: John Doe → NULL
# - email: john@example.com → hash_xxxx
# - phone: +1-555-1234 → NULL
# - billing_address: 123 Main St → NULL
# - transaction_history: REDACTED
# - audit_logs: Timestamp preserved, actor anonymized

# 3. Execute (after legal/compliance approval)
ggen data-anonymize \
  --customer-id cust_old_001 \
  --frameworks GDPR \
  --execute true

# 4. Verify deletion
ggen data-retention-verify \
  --customer-id cust_old_001 \
  --check-pii-deleted true

# Output:
# PII fields deleted: 7/7 (100%)
# Audit trail preserved: Yes (for 7-year retention)
# Data accessible: No (cold storage archive only)
```

### Procedure: KYC/AML Verification Failure

**Scenario**: Customer failed identity verification during annual review

```bash
# 1. Investigate verification failure
ggen kyc-verification-details \
  --customer-id cust_abc \
  --request-id kyc_req_001

# Output:
# Verification provider: Veriff
# Status: FAILED
# Failure reason: Document_Expired
# Customer document: Driver's license (expired 2020)
# Retry attempts: 1/3

# 2. Lock account to prevent data access
ggen customer-account-lock \
  --customer-id cust_abc \
  --reason "KYC verification failed"

# 3. Notify customer
ggen customer-notification-send \
  --template kyc_verification_failed \
  --customer-id cust_abc \
  --action_required "Resubmit valid ID within 7 days"
  --deadline $(date -d '+7 days' +%Y-%m-%d)

# 4. Escalate to compliance review team
ggen incident-escalate \
  --severity SEV-2 \
  --team compliance_team \
  --customer-id cust_abc \
  --reason "Failed KYC verification, requires manual review"

# 5. Track remediation
watch -n 60 'ggen kyc-status --customer-id cust_abc'

# If customer resubmits:
ggen kyc-verify-resubmission \
  --customer-id cust_abc \
  --request-id kyc_req_002
```

---

## Customer Account Compromises

### Symptom: Suspicious Account Activity

**Severity**: SEV-1

**Detection**:
```
Alert triggered: fraud_detection_alert
Risk indicators:
- Large transaction from unusual location
- Multiple failed login attempts (6+)
- Rapid API calls from new IP
```

**Initial Response** (< 15 min):
```bash
# 1. Verify compromise
ggen fraud-analysis --customer-id cust_abc

# Output:
# Risk score: 8.5/10 (HIGH)
# Risk factors:
# - Login from Australia (unusual for US customer)
# - Failed login attempts: 8 (threshold: 6)
# - Large payment $5,000 (10x normal)
# - API calls: 2,000 req/min (100x normal)

# 2. Lock account immediately
ggen customer-account-lock \
  --customer-id cust_abc \
  --reason "Unauthorized access detected" \
  --auto-unlock false  # Manual review required

# 3. Suspend active entitlements
ggen entitlement-suspend-all \
  --customer-id cust_abc \
  --reason "Account security lockdown"

# 4. Reverse fraudulent transactions
ggen billing-reverse-transactions \
  --customer-id cust_abc \
  --date-range "2026-01-25T00:00:00Z_2026-01-25T08:00:00Z" \
  --suspicious true \
  --dry-run true

# Preview fraud reversal:
# Transaction 1: $5,000 payment to attacker → REVERSE
# Transaction 2: $200 API call overage charge → REVERSE
# Total reversed: $5,200
# Review required before execution
```

**Customer Notification**:
```bash
# Send security alert
ggen customer-notification-send \
  --template account_security_alert \
  --customer-id cust_abc \
  --priority urgent \
  --channels [email, sms] \
  --message "Suspicious activity detected. Account locked for security."

# Include action items
ggen customer-notification-send \
  --template account_recovery_instructions \
  --customer-id cust_abc \
  --include_links [
    "verify_identity_link",
    "reset_password_link",
    "enable_mfa_link",
    "support_contact"
  ]
```

**Investigation**:
```bash
# 1. Export full audit trail
ggen customer-audit-trail-export \
  --customer-id cust_abc \
  --start-date 2026-01-24 \
  --end-date 2026-01-26 \
  --output /tmp/cust_abc_audit.json

# 2. Analyze suspicious transactions
ggen fraud-investigation \
  --customer-id cust_abc \
  --audit-trail /tmp/cust_abc_audit.json \
  --suspected-unauthorized-activity true

# Output investigation report:
# - Timeline of account compromise
# - List of affected transactions
# - Estimated fraud amount
# - Remediation recommendations
```

**Recovery**:
```bash
# 1. Customer verifies identity (via phone call + document verification)
ggen customer-identity-verification \
  --customer-id cust_abc \
  --method manual_call \
  --agent_id support_agent_123

# 2. After verification passed
ggen customer-account-unlock \
  --customer-id cust_abc \
  --verified-by support_agent_123

# 3. Reset password + enable MFA
ggen customer-password-reset-forced \
  --customer-id cust_abc

ggen customer-mfa-enable-required \
  --customer-id cust_abc

# 4. Restore entitlements
ggen entitlement-restore-all \
  --customer-id cust_abc \
  --reason "Account security restored"

# 5. Monitor for further suspicious activity
watch -n 300 'ggen fraud-risk-score --customer-id cust_abc'
```

---

## Cascade Failures

### Symptom: Multi-Tenant System Experiencing Cascade

**Severity**: SEV-1

**Detection**:
```
Alert triggered: cascade_prevention_activated
Error rate: 25% (baseline 0.1%)
Failed requests: 12,500/50,000
Response: Severely degraded
```

**Immediate Actions** (< 5 min):
```bash
# 1. Verify cascade is happening
ggen system-health --check all

# Output:
# System Status: DEGRADED
# Error rate: 25% (SLO: <0.1%)
# Failed requests: 12,500
# Affected tenants: 150+
# Noisy neighbor: tenant_008 (87% CPU usage)

# 2. Identify cause
ggen cascade-analysis --show-root-cause true

# Output:
# Primary cause: Single tenant (tenant_008) consuming 87% CPU
# Secondary effects:
# - API latency increased 50x
# - Other tenants throttled
# - Database connection pool exhausted
# - Circuit breakers tripped

# 3. Apply circuit breaker to noisy tenant
ggen multi-tenant-governor \
  --circuit-breaker enable \
  --tenant-id tenant_008 \
  --error-threshold 50 \
  --timeout-ms 1000

# 4. Reduce noisy tenant quota
ggen quota-sla-governor \
  --set-quota --tenant-id tenant_008 \
  --resource-type cpu \
  --value 25 --unit percent  # Reduce to 25% of tier allocation

# 5. Monitor for improvement
watch -n 5 'ggen system-health --check all'

# Expected recovery: < 2 min after circuit breaker
```

**Recovery Phase**:
```bash
# 1. Once system stable (error rate < 5%, improving)
ggen cascade-prevention --status check

# Output:
# Cascade prevention active: YES
# System health improving: YES
# Noisy tenant quarantined: YES

# 2. Begin gradual quota restoration
ggen quota-sla-governor \
  --set-quota --tenant-id tenant_008 \
  --resource-type cpu \
  --value 50 --unit percent  # Restore to 50%

# Monitor for 5 minutes

# 3. If stable, restore full quota
ggen quota-sla-governor \
  --set-quota --tenant-id tenant_008 \
  --resource-type cpu \
  --value 100 --unit percent  # Full restoration

# 4. Disable circuit breaker
ggen multi-tenant-governor \
  --circuit-breaker disable \
  --tenant-id tenant_008

# 5. Verify full recovery
watch -n 10 'ggen system-health --check all'
# Should show: ALL GREEN, error rate < 0.1%, latency normalized
```

**Post-Incident Analysis**:
```bash
# 1. Contact noisy tenant to understand spike
ggen customer-notification-send \
  --template service_incident_notification \
  --customer-id tenant_008 \
  --severity high \
  --include_root_cause true \
  --include_what_we_did "Circuit breaker enabled, quota reduced, service restored"
  --ask_for_cooperation "Please notify us of planned usage spikes in advance"

# 2. Investigate root cause
ggen cascade-investigation \
  --incident-id cascade_001_2026-01-25
  --include_tenant_activity true

# Output:
# Tenant activity timeline:
# 14:15 - Normal load (5,000 req/min)
# 14:20 - Spike begins (50,000 req/min)
# 14:25 - CPU at 87%, cascade triggers
# 14:25:30 - Circuit breaker applied, system stabilizes
# 14:30 - System fully recovered

# 3. Determine if legitimate traffic or attack
ggen threat-assessment \
  --tenant-id tenant_008 \
  --activity-log /tmp/cascade_investigation.json

# Output might indicate:
# - Legitimate traffic spike (announced campaign)
# - DDoS attack (attacker IP ranges)
# - Application bug (recursive API calls)

# 4. Recommendations
ggen incident-report-generate \
  --incident-id cascade_001_2026-01-25 \
  --recommendations [
    "1. Tenant should use async processing for bulk operations",
    "2. We should implement request deduplication",
    "3. Increase resource quotas for this tenant (if legitimate traffic)",
    "4. Implement more granular circuit breakers by feature"
  ]
```

---

## Data Breach Response

### Symptom: Unauthorized Data Access Detected

**Severity**: SEV-1 (Immediate Incident Commander engagement required)

**Initial Response** (< 15 min):
```bash
# 1. Confirm breach
ggen security-incident-confirm \
  --incident-id brch_001 \
  --affected-data-type phi,pii,payment_info

# 2. Assess scope
ggen breach-scope-assessment \
  --incident-id brch_001

# Output:
# Affected customers: 2,847
# Affected data types:
# - PHI (HIPAA-sensitive): 1,200 patients
# - PII (GDPR-sensitive): 2,847 customers
# - Payment info: 450 payment methods
# Attack vector: SQL injection
# Estimated breach date: 2026-01-20
# Discovery date: 2026-01-25 (5 days later)

# 3. Contain breach
ggen breach-containment \
  --incident-id brch_001 \
  --containment-actions [
    "Revoke attacker database credentials",
    "Close SQL injection vulnerability",
    "Isolate affected customer data",
    "Disable API endpoint used in attack",
    "Increase monitoring/alerting"
  ]

# 4. Disable affected systems
gcloud sql instances patch marketplace-db \
  --require-ssl=true \
  --backup-configuration-binary-log-enabled=true

# 5. Notify executive team
ggen incident-escalate \
  --severity SEV-1 \
  --broadcast true \
  --teams legal,compliance,security,ir,ciso \
  --message "DATA BREACH CONFIRMED: 2,847 customers affected, 5-day exposure window"
```

**Regulatory Notification** (timelines are strict):
```bash
# GDPR: Notify DPA within 72 hours
ggen gdpr-breach-notification \
  --incident-id brch_001 \
  --authority "EU Data Protection Authority" \
  --submit true
  --deadline $(date -d '+72 hours' +%Y-%m-%dT%H:%M:%SZ)

# HIPAA: Notify HHS within 60 calendar days
ggen hipaa-breach-notification \
  --incident-id brch_001 \
  --authority "HHS Breach Notification System" \
  --submit true
  --deadline $(date -d '+60 days' +%Y-%m-%d)

# PCI-DSS: Notify card issuing banks within 30 days
ggen pci-dss-breach-notification \
  --incident-id brch_001 \
  --payment-processors [stripe] \
  --deadline $(date -d '+30 days' +%Y-%m-%d)
```

**Customer Notification** (GDPR: without unreasonable delay):
```bash
# Batch notification to all 2,847 affected customers
ggen customer-breach-notification \
  --incident-id brch_001 \
  --affected-customers 2847 \
  --notification-deadline $(date -d '+1 day' +%Y-%m-%d) \
  --include [
    "what_happened",
    "data_affected",
    "timeline",
    "what_we_are_doing",
    "what_you_can_do",
    "credit_monitoring_offer",
    "support_contact"
  ]

# Media notification (if breach > 500 residents of US state)
ggen media-breach-notification \
  --incident-id brch_001 \
  --notify true
```

**Forensics & Investigation**:
```bash
# 1. Preserve evidence
ggen breach-evidence-collection \
  --incident-id brch_001 \
  --evidence-types [
    "database_logs",
    "application_logs",
    "network_logs",
    "cloud_audit_logs",
    "system_logs"
  ] \
  --retention-days 2555  # 7 years per regulations

# 2. Engage forensics firm
# (Company policy: external forensics for breaches)
# Assign case to: [External Forensics Company]
# Evidence location: /secure/evidence/brch_001/

# 3. Root cause analysis
ggen breach-rca \
  --incident-id brch_001 \
  --timeline-detail verbose

# Output RCA:
# 1. SQL injection vulnerability in login endpoint
# 2. Attacker discovered vulnerability via fuzzing
# 3. Attacker gained database access 2026-01-20 at 14:23:15 UTC
# 4. Attacker exfiltrated customer table (2,847 rows)
# 5. We detected anomaly 2026-01-25 at 09:47:00 UTC
# 6. Detection latency: 5 days (incident response process broken)

# 4. Remediation
ggen breach-remediation \
  --incident-id brch_001 \
  --fixes [
    "1. Patch SQL injection (commit: abc123)",
    "2. Implement input validation middleware",
    "3. Deploy Web Application Firewall (WAF)",
    "4. Enable query audit logging",
    "5. Implement least-privilege database access",
    "6. Enhanced monitoring for data exfiltration"
  ]
```

**Prevention for Future**:
```bash
# 1. Vulnerability scanning
ggen security-scan-dependencies \
  --all-packages true \
  --frequency weekly

ggen security-scan-code \
  --sast-tool snyk \
  --frequency continuous

# 2. Incident detection improvements
ggen detection-tuning \
  --metric "database_exfiltration" \
  --alert-threshold "1000 rows/minute"
  --alert-latency "< 5 minutes"

# 3. Penetration testing
ggen penetration-test-schedule \
  --frequency monthly
  --focus-areas [sql-injection, authentication, authorization]
```

---

## Decision Trees

### Tree: Payment Failure Response

```
Payment Processing Issue Detected
│
├─ Success rate < 95%?
│  ├─ YES → Are multiple payment processors failing?
│  │  ├─ YES → Check GCP infrastructure (networking, compute)
│  │  │  └─ Incident: GCP Regional Outage
│  │  │     Action: Failover to backup region
│  │  │
│  │  └─ NO → Only Stripe failing?
│  │     ├─ YES → Check Stripe API status
│  │     │  └─ Stripe down → Failover to backup processor
│  │     │
│  │     └─ NO → Our system issue?
│  │        └─ Check payment request validation
│  │           Likely cause: Schema mismatch, timeout, or rate limiting
│  │
│  └─ NO → Continue monitoring, incident under threshold

├─ Retry queues growing?
│  ├─ YES → Are customers still authorized?
│  │  ├─ NO → Auto-suspend entitlements, notify customers
│  │  └─ YES → Send payment method update notifications
│  │
│  └─ NO → Normal operation

└─ End incident or escalate
```

### Tree: Compliance Violation Response

```
Compliance Violation Alert
│
├─ Which framework violated?
│  ├─ GDPR → Data processing, consent, retention
│  │  ├─ Unauthorized data access → DATA BREACH (section 6)
│  │  ├─ Retention period exceeded → Delete/anonymize data
│  │  └─ Consent missing → Pause processing, contact customer
│  │
│  ├─ HIPAA → PHI protection, access control
│  │  ├─ Unauthorized access to PHI → DATA BREACH (section 6)
│  │  ├─ Encryption missing → Enable encryption immediately
│  │  └─ Audit logging missing → Enable logging, investigate
│  │
│  ├─ SOC2 → Security, availability, integrity controls
│  │  ├─ Access control violation → Review IAM permissions
│  │  ├─ Monitoring alert config missing → Enable alerts
│  │  └─ Incident response broken → Initiate IR procedures
│  │
│  ├─ PCI-DSS → Payment card protection
│  │  ├─ PAN stored in plaintext → CRITICAL: Enable tokenization
│  │  ├─ Encryption missing → Enable TLS + field encryption
│  │  └─ Audit logs missing → Enable logging
│  │
│  └─ FedRAMP → US Government requirements
│     └─ Escalate to Chief Information Security Officer
│
└─ Notify compliance officer & legal team
```

### Tree: Account Compromise Response

```
Suspicious Account Activity Detected
│
├─ What's the threat level?
│  ├─ HIGH (multiple fraud signals) → LOCK ACCOUNT IMMEDIATELY
│  │  │
│  │  ├─ Suspend entitlements (prevent further damage)
│  │  ├─ Reverse fraudulent transactions
│  │  ├─ Notify customer via multiple channels
│  │  └─ Escalate to fraud investigation team
│  │
│  ├─ MEDIUM (1-2 fraud signals) → MONITOR CLOSELY
│  │  │
│  │  ├─ Enable enhanced monitoring (rule-based alerting)
│  │  ├─ Flag for manual review (by fraud analyst)
│  │  ├─ Send suspicious activity notification to customer
│  │  └─ Prepare lock action if signals increase
│  │
│  └─ LOW (background risk score) → NORMAL OPERATIONS
│     └─ Continue standard fraud detection
│
└─ Customer Recovery Plan
   ├─ Identity verification required (phone + document)
   ├─ Password reset + MFA enforcement
   ├─ Restore entitlements after verification
   └─ Offer credit monitoring (PCI breach) or identity theft protection (data breach)
```

---

## Communication Templates

### Template: Customer Notification (Payment Failure)

```
Subject: Action Required: Payment Method Failed

Dear [CUSTOMER_NAME],

We attempted to process your monthly payment on [DATE] but it was declined.
Payment method: [CARD_LAST_4]

ACTION REQUIRED: Please update your payment method immediately to prevent service interruption.

How to update:
1. Visit: [PAYMENT_UPDATE_LINK]
2. Enter new payment method
3. We'll retry payment within 24 hours

Timeline:
- Retry attempt: [DATE + 1 day]
- Final attempt: [DATE + 7 days]
- Service suspension if unpaid: [DATE + 14 days]

IMPORTANT: If you believe this is an error, contact us immediately:
- Phone: [SUPPORT_PHONE]
- Email: support@company.com
- Chat: [CHAT_LINK]

We're here to help,
The [COMPANY] Team

---
Incident ID: INC_[ID]
Reference: [PAYMENT_ID]
```

### Template: Internal Incident Notification (SEV-1)

```
CRITICAL INCIDENT - SEV-1

Incident ID: INC_2026_012345
Severity: SEV-1 (Critical)
Status: ACTIVE

INCIDENT TITLE: Payment Processing Down (Stripe Unavailable)

IMPACT:
- Service: Billing payments
- Customers affected: 1,247 (Starter tier: 945, Professional: 302)
- Est. revenue impact: $50k/hour
- Timeline: 14:25 - ongoing (35 minutes)

ROOT CAUSE:
Stripe API experiencing global outage (https://status.stripe.com)

ACTIONS TAKEN:
1. [14:25] Incident detected via automated alert
2. [14:26] Failover to backup payment processor initiated
3. [14:27] Success rate recovered to 98%

NEXT STEPS:
1. Monitor Stripe recovery (follow status page)
2. Failback to Stripe once recovered
3. Clear payment retry queue
4. Verify no duplicate charges

INCIDENT COMMANDER: @sre_oncall
ON-CALL CONTACTS:
- Payments Team: @payments_lead
- Stripe Liaison: @stripe_contact
- VP Engineering: @vp_eng

Slack Channel: #critical-incident-ic
War Room: [ZOOM_LINK]

Updates every 5 minutes
```

### Template: Data Breach Notification (Customer)

```
URGENT: Security Incident Affecting Your Account

Dear [CUSTOMER_NAME],

We are writing to inform you of a security incident that may have affected
your personal information.

WHAT HAPPENED:
On [BREACH_DATE], an attacker gained unauthorized access to our customer database
through a SQL injection vulnerability. We discovered and contained the breach on
[DISCOVERY_DATE].

WHAT DATA MAY BE AFFECTED:
- Name and email address
- Phone number
- Billing address
- Last 4 digits of payment method (credit card number was NOT stored)
- Your account activity logs

DATA NOT AT RISK:
- Full payment card numbers (we don't store these - Stripe does)
- Passwords (stored encrypted and cannot be decrypted)
- API keys (rotated immediately)

TIMELINE:
- Breach occurred: [DATE/TIME]
- Breach discovered: [DATE/TIME]
- Breach contained: [DATE/TIME]
- You are being notified: [DATE/TIME]

WHAT WE ARE DOING:
1. ✓ Contained the breach (attacker access revoked)
2. ✓ Patched the vulnerability
3. ✓ Deployed Web Application Firewall (WAF)
4. ✓ Enhanced monitoring for similar attacks
5. ✓ Notified law enforcement

WHAT YOU CAN DO:
1. Reset your password: [PASSWORD_RESET_LINK]
2. Enable multi-factor authentication (MFA): [MFA_SETUP_LINK]
3. Monitor your account for unusual activity
4. Consider credit monitoring/identity theft protection (we're offering 2 years free)
   - Sign up: [CREDIT_MONITORING_LINK]
   - PIN: [UNIQUE_PIN]

QUESTIONS?
- Email: security@company.com
- Phone: 1-800-XXX-XXXX
- Chat: [LIVE_CHAT_LINK] (priority support)

We take your security seriously and apologize for this incident. We're committed
to preventing this from happening again.

Sincerely,
[CEO_NAME]
Chief Executive Officer
[COMPANY_NAME]

---
Incident ID: INC_BREACH_2026_001
Reference: Case #[CASE_NUMBER]
```

---

**Last Updated**: January 2026
**Next Review**: April 2026
**Owner**: @incident-response-team
**Escalation**: @sre-oncall → @engineering-manager → @vp-engineering
**Contact**: #incident-response (Slack)
