# Security Architecture: Value-Indexed Pricing System

**Document Classification:** CONFIDENTIAL - FINANCIAL SECURITY
**Version:** 1.0.0
**Last Updated:** 2026-01-25
**Threat Level:** CRITICAL - Revenue System

---

## Executive Summary

This document specifies the complete security architecture for protecting value-indexed pricing calculations from fraud, manipulation, and attack. The system operates under **worst-case assumptions**: hostile customers, compromised insiders, and sophisticated adversaries with financial incentives to manipulate value metrics.

**Core Principle:** Value calculations are immutable, cryptographically verified, and audit-logged at every step. No customer or employee can modify historical pricing data without detection.

---

## Table of Contents

1. [Threat Model](#threat-model)
2. [Attack Surface Analysis](#attack-surface-analysis)
3. [Cryptographic Receipt System](#cryptographic-receipt-system)
4. [Data Isolation & Access Control](#data-isolation--access-control)
5. [Administrative Controls](#administrative-controls)
6. [Audit Logging](#audit-logging)
7. [Incident Response Protocol](#incident-response-protocol)
8. [Insurance & Risk Management](#insurance--risk-management)
9. [Monitoring & Alerting](#monitoring--alerting)
10. [Penetration Testing](#penetration-testing)

---

## 1. Threat Model

### 1.1 Adversary Profiles

#### A. Malicious Customer
- **Motivation:** Reduce pricing liability; claim lower value realization
- **Capabilities:** Access own metrics/data; limited system knowledge
- **Attack Vectors:**
  - Manipulate submitted value metrics
  - Dispute historical charges; claim calculation errors
  - Collude with internal staff
  - Submit false usage data
- **Incentive:** $10K-$1M per manipulation (depending on deal size)

#### B. Insider Threat (Employee)
- **Motivation:** Bribery ($50K-$500K); revenge; cover embezzlement
- **Capabilities:** Database access; code deployment; credential theft
- **Attack Vectors:**
  - Direct database manipulation
  - Modify calculation formulas
  - Delete/forge audit logs
  - Create backdoor pricing rules
  - Steal customer data for competitors
- **Incentive:** High (direct financial payment or extortion)

#### C. Sophisticated Attacker (Hacktivist/Competitor)
- **Motivation:** Revenue destruction; market disruption; IP theft
- **Capabilities:** Zero-day exploits; infrastructure breach
- **Attack Vectors:**
  - Compromise production database
  - Inject malicious code into calculation engine
  - Exfiltrate customer PII/metrics
  - Disable audit logging
  - Create cascading billing errors
- **Incentive:** Competitive advantage; publicity; extortion

#### D. Regulator/Auditor
- **Motivation:** Compliance verification; fraud detection
- **Capabilities:** Legal authority; can demand historical data
- **Audit Vectors:**
  - Verify calculation accuracy for 12-24 months
  - Trace every pricing decision to source metrics
  - Confirm no unauthorized modifications
  - Validate audit trail integrity
- **Incentive:** Consumer protection; market integrity

### 1.2 Risk Assessment Matrix

| Threat | Likelihood | Impact | Priority |
|--------|-----------|--------|----------|
| Customer disputes calculation | HIGH | MEDIUM | CRITICAL |
| Insider DB manipulation | MEDIUM | CRITICAL | CRITICAL |
| Competitor metric injection | LOW | CRITICAL | HIGH |
| Regulatory audit failure | MEDIUM | CRITICAL | CRITICAL |
| Ransomware billing system | LOW | CRITICAL | HIGH |
| Customer PII exfiltration | MEDIUM | CRITICAL | CRITICAL |
| Calculation logic tampering | LOW | CRITICAL | CRITICAL |
| Disputed revenue (legal) | HIGH | MEDIUM | CRITICAL |

---

## 2. Attack Surface Analysis

### 2.1 Critical Components & Vulnerabilities

#### **Value Input Layer**
**Risk:** Metric injection, false data submission
- Customer submits usage/outcome metrics
- Metrics feed pricing calculations
- **Vulnerability:** Customer can forge metrics without authentication
- **Mitigation:** Cryptographic attestation of all metrics; real-time validation

#### **Calculation Engine**
**Risk:** Formula manipulation, wrong output
- Performs pricing calculation
- Executes business rules
- **Vulnerability:** Insider can modify calculation code; formula can have bugs
- **Mitigation:** Immutable versioned formulas; cryptographic proofs; calculation audit trail

#### **Storage Layer (Database)**
**Risk:** Direct value manipulation, audit log deletion
- Stores historical pricing records
- Houses audit trail
- **Vulnerability:** DBA can modify rows; incomplete access controls
- **Mitigation:** Event sourcing; cryptographic hashing; write-once logs; role separation

#### **API/Integration Points**
**Risk:** Unauthorized calls; parameter tampering
- Pricing API called by billing system
- Customer portal access
- Admin/override endpoints
- **Vulnerability:** Weak authentication; missing audit trails
- **Mitigation:** mTLS; cryptographic request signing; comprehensive logging

#### **Administrative Interfaces**
**Risk:** Unauthorized overrides; silent modifications
- Manual pricing adjustments
- Customer dispute resolutions
- Calculation formula updates
- **Vulnerability:** Insider can make changes without verification
- **Mitigation:** Approval workflows; cryptographic signing; immutable audit trail

#### **Reports & Analytics**
**Risk:** Metric exfiltration; customer comparison
- Revenue reporting
- Customer performance analytics
- Metrics dashboards
- **Vulnerability:** Customer sees metrics of competitors
- **Mitigation:** Strict data isolation; row-level security; audit all access

---

## 3. Cryptographic Receipt System

### 3.1 Architecture

The system generates **deterministic cryptographic receipts** for every pricing calculation. These receipts prove:
1. **Authenticity:** Calculation authorized by specific actor
2. **Integrity:** Metrics/formula not modified since calculation
3. **Non-Repudiation:** Actor cannot deny participation
4. **Immutability:** Modification detection (any change invalidates receipt)

### 3.2 Receipt Generation Pipeline

```
PHASE 1: Collection
├─ Collect input metrics from customer/system
├─ Validate metric signatures (if remote source)
├─ Hash metric bundle: H_metrics = SHA-256(sorted_metrics)
└─ Record timestamp: T_start (UTC, nanosecond precision)

PHASE 2: Calculation
├─ Load pricing formula version: V_formula
├─ Hash formula: H_formula = SHA-256(formula_code)
├─ Execute calculation: Value = f(metrics, formula_version)
├─ Record formula inputs/outputs
└─ Record calculation state changes

PHASE 3: Witness & Attestation
├─ Create witness statement: W = sign(H_metrics || H_formula || Value || T_start, KEY_system)
├─ Include regulatory witness (optional): W_audit = sign(..., KEY_auditor)
├─ Record who authorized calc: ACTOR_id, ACTOR_signature
└─ Include machine metadata: CPU_id, Memory_hash, Timestamp_hw

PHASE 4: Receipt Construction
├─ Create receipt: R = {
│   receipt_id: UUID (unique, immutable)
│   timestamp: T_start + T_calc
│   tenant_id: customer_id (immutable)
│   calculation_id: H_metrics || H_formula || H_value
│   metrics_hash: H_metrics
│   formula_version: V_formula
│   formula_hash: H_formula
│   input_snapshot: {metrics...} (immutable copy)
│   output_value: Value
│   signature_system: W
│   signature_actor: ACTOR_signature
│   signature_witness: W_audit (optional)
│   proof_of_calculation: {entropy, CPU_sample, RAM_sample}
│   merkle_root: MerkleTree(historical_receipts).root
│ }
└─ Sign receipt: RECEIPT_sig = sign(R, KEY_system)

PHASE 5: Immutable Storage
├─ Write to append-only log (no updates/deletes)
├─ Hash receipt: H_receipt = SHA-256(R)
├─ Link to chain: Link(H_prev_receipt → H_receipt)
├─ Store in 3x replication (geographically diverse)
├─ Write backup to immutable storage (GCS, S3 with MFA-delete)
└─ Log fingerprint in blockchain (Ethereum, Hyperledger) for high-value deals
```

### 3.3 Receipt Data Structure (Erlang)

```erlang
-type receipt_id() :: binary(). % UUID v7 (sortable)
-type actor_id() :: binary().   % tenant_id | employee_id
-type signature() :: binary().  % Ed25519(512 bits)

-record(pricing_receipt, {
    receipt_id :: receipt_id(),                      % Unique ID
    timestamp :: non_neg_integer(),                  % Nanoseconds
    tenant_id :: binary(),                           % Customer

    % Calculation input
    metrics_hash :: binary(),                        % SHA-256
    metrics_snapshot :: map(),                       % Immutable copy
    formula_version :: binary(),                     % e.g., "v2.3.1-201605"
    formula_hash :: binary(),                        % SHA-256 of code

    % Calculation output
    calculated_value :: float(),                     % Result
    currency :: binary(),                            % "USD", "EUR"

    % Cryptographic proof
    system_signature :: signature(),                 % Ed25519 by system
    actor_id :: actor_id(),                         % Who initiated
    actor_signature :: signature(),                 % Employee approval
    audit_witness :: signature() | undefined,        % Auditor signature

    % Proof of work (prevent calculation spoofing)
    cpu_entropy :: binary(),                         % CPU RNG sample
    memory_sample :: binary(),                       % RAM content hash
    execution_time_ms :: non_neg_integer(),         % Wall clock

    % Chain of custody
    previous_receipt_hash :: binary(),              % Link to prior
    merkle_proof :: [binary()],                     % Path to root

    % Audit trail
    created_by :: actor_id(),                       % Initial creator
    modified_by :: [actor_id()],                    % Any overrides
    dispute_flag :: boolean(),                      % Customer disputed?

    % Metadata
    billing_id :: binary(),                         % Associated bill
    customer_id :: binary(),                        % Tenant ID
    reason :: binary(),                             % Why calculated
    metadata :: map()                               % Additional context
}).
```

### 3.4 Receipt Verification

```erlang
%% Verify receipt integrity - detects ANY modification
verify_receipt(Receipt) ->
    case verify_signature(Receipt) of
        false -> {error, signature_invalid};
        true ->
            case verify_chain_link(Receipt) of
                false -> {error, chain_broken};
                true ->
                    case verify_metrics_hash(Receipt) of
                        false -> {error, metrics_tampered};
                        true -> {ok, verified}
                    end
            end
    end.

%% Detect tampering with historical receipts
detect_receipt_tampering(ReceiptId) ->
    case get_receipt(ReceiptId) of
        {ok, Receipt} ->
            case verify_receipt(Receipt) of
                {ok, verified} -> {ok, not_tampered};
                {error, Reason} ->
                    {error, {tampering_detected, Reason}}
            end;
        {error, not_found} -> {error, receipt_missing}
    end.
```

---

## 4. Data Isolation & Access Control

### 4.1 Multi-Tenant Isolation

**Rule:** No customer can access another customer's metrics, calculations, or pricing.

#### Implementation Strategy

```erlang
%% Strict tenant isolation enforced at database layer
-define(TENANT_ISOLATION_FILTER,
    "WHERE tenant_id = ? AND
            created_by MATCHES 'SAME_TENANT_OR_SYSTEM'
    ").

%% Query pattern: ALL queries MUST include tenant_id filter
get_customer_pricing(TenantId, CustomerId) ->
    Query = "SELECT * FROM pricing_receipts
             WHERE tenant_id = ? AND customer_id = ?",
    db_query(Query, [TenantId, CustomerId]).

%% Anti-pattern (FORBIDDEN - triggers alert):
get_all_pricing(CustomerId) ->
    % ERROR: Missing tenant_id filter
    % This query BLOCKS with immediate PagerDuty alert
    Query = "SELECT * FROM pricing_receipts
             WHERE customer_id = ?",
    db_query(Query, [CustomerId]). % WILL FAIL
```

#### Access Control Matrix

| Role | Metrics | Calculations | Receipts | Audit Log | Override |
|------|---------|--------------|----------|-----------|----------|
| Customer | Own only | Own only | Own only | Own only | DENY |
| Support | Read tenant | Read tenant | Read tenant | Read tenant | ASK |
| Finance | Read all | Read all | Read all | Read all | APPROVE |
| Admin | Full | Full | Full | Full | CREATE |
| Auditor (ext) | Audit-tagged | Audit-tagged | All | All | DENY |

### 4.2 Row-Level Security (RLS)

```sql
-- PostgreSQL RLS policies for pricing tables

-- Policy 1: Customers see only own data
CREATE POLICY tenant_isolation ON pricing_receipts
    USING (tenant_id = current_setting('app.tenant_id'));

-- Policy 2: Employees see only permitted tenant data
CREATE POLICY employee_tenant_filter ON pricing_receipts
    USING (tenant_id IN (
        SELECT authorized_tenants FROM employee_roles
        WHERE employee_id = current_setting('app.employee_id')
    ));

-- Policy 3: Auditors (external) see nothing unless explicitly tagged
CREATE POLICY audit_visibility ON pricing_receipts
    USING (
        audit_tagged = true
        AND current_setting('app.role') = 'auditor'
    );

-- Prevent UPDATE/DELETE on historical receipts
ALTER TABLE pricing_receipts ENABLE ROW LEVEL SECURITY;
CREATE POLICY immutable_receipts ON pricing_receipts
    FOR UPDATE USING (false); -- No updates allowed
CREATE POLICY immutable_receipts_delete ON pricing_receipts
    FOR DELETE USING (false); -- No deletes allowed
```

### 4.3 API Authentication & Authorization

```erlang
%% mTLS + API Key + Rate Limiting

-record(api_request, {
    api_key :: binary(),           % Cryptographic key
    tls_cert :: cert(),            % mTLS client certificate
    tenant_id :: binary(),          % Extracted from cert CN
    actor_id :: binary(),           % Who in tenant
    timestamp :: integer(),         % Request time
    signature :: binary()           % HMAC-SHA256
}).

validate_api_request(Request) ->
    [
        validate_mtls(Request),      % TLS cert valid?
        validate_api_key(Request),   % Key not revoked?
        validate_signature(Request), % Request not tampered?
        validate_tenant(Request),    % Tenant active?
        validate_rate_limit(Request) % Not exceed quota?
    ].

%% Request signature: HMAC(api_key, method || path || body_hash || timestamp)
generate_request_signature(ApiKey, Method, Path, BodyHash, Timestamp) ->
    Message = <<Method/binary, Path/binary, BodyHash/binary,
                Timestamp/binary>>,
    crypto:hmac(sha256, ApiKey, Message).
```

---

## 5. Administrative Controls

### 5.1 Pricing Override Workflow

**Rule:** No single person can override pricing. Requires approval + cryptographic verification.

```erlang
%% Pricing override - requires multi-approval

-record(pricing_override, {
    override_id :: binary(),           % UUID
    tenant_id :: binary(),             % Customer
    original_value :: float(),         % Before
    override_value :: float(),         % After
    reason :: binary(),                % "Dispute resolution", "Contract adjustment"

    % Approval chain
    requested_by :: actor_id(),        % Who asked (Support/Finance)
    requested_at :: integer(),         % When
    requested_signature :: binary(),   % Proof of who asked

    requested_approval_1 :: {actor_id, signature, integer()} | undefined,
    requested_approval_2 :: {actor_id, signature, integer()} | undefined,
    final_approval :: {actor_id, signature, integer()} | undefined,

    % Audit trail
    rationale :: binary(),             % Detailed reason
    supporting_docs :: [binary()],     % Links to evidence

    % Execution
    status :: pending | approved | executed | rejected,
    executed_at :: integer() | undefined,
    executed_by :: actor_id() | undefined,

    % Reversibility
    reversal_possible :: boolean(),    % Can customer ask to undo?
    reversal_deadline :: integer()     % Until when?
}).

%% Override approval process

request_override(TenantId, Value, Reason) ->
    Override = #pricing_override{
        override_id = uuid:v7(),
        tenant_id = TenantId,
        original_value = get_current_value(TenantId),
        override_value = Value,
        reason = Reason,
        requested_by = current_user(),
        requested_at = timestamp(),
        status = pending
    },
    store_override(Override),
    % Trigger approval workflow (email, Slack notification)
    notify_approvers(Override),
    {ok, Override}.

%% APPROVAL RULES:
%% - $0-$1K: 1 approval (Finance Manager)
%% - $1K-$10K: 2 approvals (Finance Manager + Director)
%% - $10K-$100K: 3 approvals (Finance Manager + Director + CFO)
%% - $100K+: 4 approvals (Finance Manager + Director + CFO + General Counsel)
%% - Each approval must include signed timestamp within 30 days
%% - Any rejection requires escalation to CFO

execute_override(OverrideId) ->
    Override = get_override(OverrideId),
    case validate_approvals(Override) of
        {error, Reason} -> {error, Reason};
        {ok, _} ->
            % Create receipt for override
            Receipt = #pricing_receipt{
                reason = <<"pricing_override">>,
                metadata = #{override_id => OverrideId, approvals => [...]}
            },
            store_receipt(Receipt),
            % Store old value for comparison
            log_value_change(Override),
            {ok, Receipt}
    end.
```

### 5.2 Calculation Formula Updates

**Rule:** Formula changes require code review, testing, AND operational approval before deployment.

```erlang
%% Formula versioning with immutable history

-record(pricing_formula, {
    formula_id :: binary(),            % UUID
    version :: binary(),               % "v2.3.1-202412"
    created_at :: integer(),           % Deploy timestamp
    created_by :: actor_id(),          % Who deployed

    % Code
    formula_code :: binary(),          % Erlang function source
    formula_hash :: binary(),          % SHA-256 (immutable)

    % Testing
    test_suite_hash :: binary(),       % Test code hash
    test_results :: map(),             % {passed, failed, coverage}

    % Approval chain
    code_review :: {reviewer_id, signature, comments},
    qa_approval :: {qa_lead, signature, coverage},
    ops_approval :: {ops_manager, signature, risk_assessment},

    % Activation
    active :: boolean(),               % Currently in use?
    activation_time :: integer(),      % When activated
    deprecation_time :: integer(),     % When no longer used

    % Impact
    affected_customers :: [binary()],  % Who uses this formula
    impact_metrics :: map()            % {avg_price_change, affected_revenue}
}).

%% FORMULA UPDATE WORKFLOW:
%% 1. Create new formula version (immutable once created)
%% 2. Write 100% test coverage for new logic
%% 3. Run tests on production-like data (anonymized)
%% 4. Code review (must pass 2 reviews, no single person can approve)
%% 5. QA sign-off (verify test coverage >= 90%)
%% 6. Ops risk assessment (identify affected customers, potential impact)
%% 7. Staged rollout:
%%    - Stage 1: 1% customers (5 days)
%%    - Stage 2: 10% customers (10 days)
%%    - Stage 3: 100% customers
%% 8. Monitoring: Watch for pricing anomalies, customer disputes
%% 9. Rollback capability: Can revert to prior formula within 60 days

deploy_formula(FormulaId) ->
    Formula = get_formula(FormulaId),
    case validate_deployment(Formula) of
        {error, Reason} -> {error, Reason};
        {ok, _} ->
            % Log deployment
            log_formula_deployment(Formula),
            % Activate gradually
            activate_staged_rollout(Formula, [0.01, 0.1, 1.0]),
            % Monitor for issues
            start_monitoring(Formula),
            {ok, Formula}
    end.
```

---

## 6. Audit Logging

### 6.1 What Gets Logged

Every action on pricing data generates an immutable audit entry:

| Action | Logged Fields | Example |
|--------|---------------|---------|
| **Metric submitted** | tenant, metric_id, values, timestamp, source_system | Customer reports $150K ARR |
| **Calculation executed** | formula_version, inputs, output, actor, timestamp | Value = $5,250 (ARR * 0.035) |
| **Receipt created** | receipt_id, hash, signature, actor | Receipt#abc123 signed by system |
| **Receipt accessed** | actor, tenant, purpose, timestamp | Admin viewed receipt for customer X |
| **Override approved** | override_id, actor, decision, reason, timestamp | Finance Manager approved $1K adjustment |
| **Formula updated** | formula_version, code_hash, test_results, actor, timestamp | Deployed v2.3.1 after code review |
| **Database write** | table, column, old_value, new_value, actor, timestamp | pricing_receipts row inserted |
| **Access denied** | actor, resource, reason, timestamp, ip | Customer tried to access competitor data |

### 6.2 Audit Log Storage

```erlang
%% Immutable append-only audit log

-record(audit_entry, {
    entry_id :: binary(),              % UUID v7 (sortable by time)
    timestamp :: integer(),            % Nanoseconds (precision)

    % Action
    action :: atom(),                  % calculate | override | access | deploy
    resource_type :: atom(),           % receipt | formula | customer | metrics
    resource_id :: binary(),           % What was affected

    % Actor
    actor_id :: binary(),              % Who did it
    actor_role :: atom(),              % customer | employee | admin
    actor_ip :: binary(),              % Source IP
    actor_user_agent :: binary(),      % Browser/client

    % Details
    description :: binary(),           % Human-readable description
    old_value :: term(),               % Before (if applicable)
    new_value :: term(),               % After (if applicable)
    metadata :: map(),                 % Additional context

    % Security
    entry_hash :: binary(),            % SHA-256(entry)
    previous_hash :: binary(),         % Link to prior entry
    signature :: binary(),             % Sign(entry, system_key)

    % Compliance
    compliance_tags :: [binary()],     % ["gdpr", "soc2", "sox"] etc
    customer_id :: binary()            % Multi-tenant trace
}).

%% Store audit entries in append-only log
append_audit_entry(Entry) ->
    % Compute hash chain
    PriorEntry = get_last_entry(),
    PriorHash = PriorEntry#audit_entry.entry_hash,

    NewEntry = Entry#audit_entry{
        entry_id = uuid:v7(),
        timestamp = system:timestamp_ns(),
        entry_hash = crypto:hash(sha256, term_to_binary(Entry)),
        previous_hash = PriorHash,
        signature = sign_entry(Entry)
    },

    % Write to immutable storage (cannot update or delete)
    append_to_log(NewEntry),

    % Replicate to 3 geographically diverse locations
    replicate_entry(NewEntry),

    {ok, NewEntry}.

%% Query audit log - verify integrity
query_audit_log(TenantId, StartTime, EndTime) ->
    Entries = db_query(
        "SELECT * FROM audit_log
         WHERE tenant_id = ? AND timestamp BETWEEN ? AND ?
         ORDER BY timestamp ASC",
        [TenantId, StartTime, EndTime]
    ),

    % Verify chain integrity
    case verify_audit_chain(Entries) of
        {error, gap} ->
            {error, {audit_tampering_detected, "Gap in chain"}};
        {error, bad_hash} ->
            {error, {audit_tampering_detected, "Hash mismatch"}};
        {error, signature_invalid} ->
            {error, {audit_tampering_detected, "Signature invalid"}};
        ok ->
            {ok, Entries}
    end.
```

### 6.3 Audit Log Exports

Audit logs exported monthly to tamper-proof storage:

```bash
# Monthly export to immutable storage
ggen audit export \
  --tenant-id=all \
  --month=2025-01 \
  --format=csv.gpg \
  --recipients=[cfo@company.com, audit@company.com]

# Output:
#   audit_log_2025_01.csv.gpg (encrypted, signed)
#   audit_log_2025_01.sha256 (cryptographic proof)
#   audit_log_2025_01.json (metadata)
```

---

## 7. Incident Response Protocol

### 7.1 Detection

**Automated Alerts** trigger on suspicious patterns:

```erlang
%% Anomaly detection

-record(anomaly_alert, {
    alert_id :: binary(),
    alert_type :: atom(),
    severity :: critical | high | medium | low,
    detected_at :: integer(),
    description :: binary(),
    evidence :: [binary()],
    recommended_action :: binary(),
    customer_affected :: binary() | [binary()]
}).

%% Alert Types

% Type 1: Calculation Mismatch
detect_calculation_mismatch(ReceiptId) ->
    Receipt = get_receipt(ReceiptId),
    Recalculated = recalculate_value(
        Receipt#pricing_receipt.metrics_snapshot,
        Receipt#pricing_receipt.formula_version
    ),
    case abs(Recalculated - Receipt#pricing_receipt.calculated_value) > 0.01 of
        true ->
            alert_pagerduty({
                alert_type = calculation_mismatch,
                severity = critical,
                description = "Calculated value doesn't match stored value"
            });
        false -> ok
    end.

% Type 2: Unauthorized Access Attempt
detect_unauthorized_access(ActorId, ResourceId) ->
    case check_access(ActorId, ResourceId) of
        {error, unauthorized} ->
            alert_pagerduty({
                alert_type = unauthorized_access,
                severity = high,
                actor = ActorId,
                resource = ResourceId
            });
        ok -> ok
    end.

% Type 3: Override Abuse
detect_override_abuse(ActorId) ->
    Overrides = count_overrides(ActorId, last_30_days),
    ThresholdHighRisk = 10,
    case Overrides > ThresholdHighRisk of
        true ->
            alert_pagerduty({
                alert_type = override_abuse,
                severity = high,
                actor = ActorId,
                count = Overrides
            });
        false -> ok
    end.

% Type 4: Audit Log Gap
detect_audit_gap() ->
    case verify_audit_continuity() of
        {error, gap} ->
            alert_pagerduty({
                alert_type = audit_tampering,
                severity = critical,
                description = "Gap detected in audit log chain"
            });
        ok -> ok
    end.

% Type 5: Formula Deviation
detect_formula_deviation(ReceiptId) ->
    Receipt = get_receipt(ReceiptId),
    Formula = get_formula(Receipt#pricing_receipt.formula_version),
    case formula_matches_deployed(Formula) of
        false ->
            alert_pagerduty({
                alert_type = formula_mismatch,
                severity = critical,
                receipt = ReceiptId
            });
        true -> ok
    end.

% Type 6: Suspicious Value Pattern
detect_suspicious_value_pattern(TenantId) ->
    Values = get_recent_values(TenantId, last_7_days),
    Mean = lists:sum(Values) / length(Values),
    StdDev = calculate_stddev(Values, Mean),
    Outliers = [V || V <- Values, abs(V - Mean) > 3 * StdDev],
    case length(Outliers) / length(Values) > 0.2 of  % 20%+ outliers = suspicious
        true ->
            alert_pagerduty({
                alert_type = value_anomaly,
                severity = medium,
                customer = TenantId,
                outlier_count = length(Outliers)
            });
        false -> ok
    end.
```

### 7.2 Incident Response Workflow

```erlang
%% Incident Response State Machine

-record(incident, {
    incident_id :: binary(),
    alert_id :: binary(),
    severity :: critical | high | medium | low,
    status :: detected | confirmed | investigating | resolved | closed,

    % Detection
    detected_at :: integer(),
    detected_by :: binary(),  % System alert

    % Investigation
    assigned_to :: actor_id(),
    started_investigation :: integer() | undefined,
    findings :: binary(),

    % Remediation
    root_cause :: binary(),
    remediation_plan :: binary(),
    remediation_started :: integer() | undefined,

    % Closure
    resolved_at :: integer() | undefined,
    post_mortem :: binary(),
    closed_at :: integer() | undefined
}).

%% RESPONSE WORKFLOW:

incident_detected(Alert) ->
    Incident = #incident{
        incident_id = uuid:v7(),
        alert_id = Alert#anomaly_alert.alert_id,
        severity = Alert#anomaly_alert.severity,
        status = detected,
        detected_at = Alert#anomaly_alert.detected_at,
        detected_by = <<"system">>
    },
    store_incident(Incident),

    % Severity-based response
    case Alert#anomaly_alert.severity of
        critical ->
            % IMMEDIATE: Page on-call engineer
            page_oncall({critical, Alert}),
            % Isolate affected systems
            isolate_systems(Alert),
            % Disable overrides temporarily
            disable_overrides(),
            % Alert customer if needed
            notify_customer(Alert);
        high ->
            % WITHIN 1 HOUR: Notify team lead
            notify_team_lead(Alert),
            % Prepare isolation plan
            prepare_rollback(Alert);
        medium ->
            % WITHIN 4 HOURS: Assign to engineer
            assign_investigation(Alert);
        low ->
            % WITHIN 24 HOURS: Log for review
            log_for_review(Alert)
    end,

    {ok, Incident}.

%% CRITICAL SEVERITY RESPONSE (0-15 minutes):
%% 1. Page on-call engineer (Slack + SMS)
%% 2. Create incident war room (Zoom link auto-posted)
%% 3. Disable problematic feature (pricing overrides)
%% 4. Preserve evidence (snapshot current state)
%% 5. Notify affected customers (within 30 mins)
%% 6. CEO/CFO notification if revenue impact > $100K

%% HIGH SEVERITY RESPONSE (0-60 minutes):
%% 1. Notify engineering team lead
%% 2. Create investigation ticket
%% 3. Assign senior engineer
%% 4. Review audit logs for root cause
%% 5. Prepare rollback plan
%% 6. Customer notification (optional, if impact unclear)

%% RESOLUTION REQUIREMENTS:
%% - Root cause documented
%% - Remediation plan written
%% - 2-step approval (Engineer + Manager)
%% - Post-mortem within 48 hours
%% - Preventive control added before closure
```

### 7.3 Investigation & Forensics

```erlang
%% Forensic Investigation

forensic_investigation(IncidentId) ->
    Incident = get_incident(IncidentId),

    % Step 1: Preserve evidence
    preserve_evidence(Incident),

    % Step 2: Timeline reconstruction
    Timeline = reconstruct_timeline(Incident),

    % Step 3: Audit log analysis
    AuditEntries = query_audit_log_forensic(Incident),

    % Step 4: Receipt verification
    ReceiptStatus = verify_all_affected_receipts(Incident),

    % Step 5: Actor analysis
    ActorBehavior = analyze_actor_behavior(Incident),

    % Step 6: System state
    SystemSnapshot = capture_system_state(Incident),

    {ok, #{
        timeline => Timeline,
        audit_entries => AuditEntries,
        receipt_status => ReceiptStatus,
        actor_behavior => ActorBehavior,
        system_state => SystemSnapshot
    }}.

%% Example: Customer disputes pricing
investigate_pricing_dispute(TenantId, ReceiptId) ->
    Receipt = get_receipt(ReceiptId),

    % Verify receipt integrity
    case verify_receipt(Receipt) of
        {ok, verified} ->
            % Receipt is authentic - calculation was correct
            {ok, {pricing_correct, Receipt}};
        {error, tampering_detected} ->
            % Customer or insider modified receipt - fraud detected!
            alert_pagerduty({
                alert_type = receipt_tampering,
                severity = critical,
                customer = TenantId,
                receipt = ReceiptId
            }),
            {error, {fraud_detected, tampering_evidence}}
    end.
```

---

## 8. Insurance & Risk Management

### 8.1 What's Insurable vs Self-Insured

| Risk | Type | Coverage | Premium | Self-Insure |
|------|------|----------|---------|-------------|
| **Customer dispute (legitimate)** | Operational | E&O | 2-5% revenue | NO - insurable |
| **Insider fraud** | Fidelity Bond | Crime | $5-20M | NO - insurable |
| **Ransomware** | Cyber | Cyber Liability | $50-100K/yr | NO - insurable |
| **Data breach (customer PII)** | Cyber | Privacy Liability | 1-3% revenue | NO - insurable |
| **Calculation errors** | Professional | E&O | 0.5-2% revenue | YES - self-insure |
| **Disputed contract interpretation** | Legal | D&O | $1-5M | NO - insurable |
| **Regulatory fines** | Compliance | Warranty | varies | YES - self-insure |
| **Reputational damage** | Business | Crisis Management | 1% revenue | YES - self-insure |

### 8.2 Insurance Requirements

**Minimum Insurance Policies:**

1. **Professional Liability (E&O)**: $10M minimum
   - Covers calculation errors
   - Pricing methodology disputes
   - Claims-made (with tail coverage)

2. **Fidelity Bond**: $5M minimum
   - Covers insider fraud
   - Employee dishonesty
   - Embezzlement

3. **Cyber Liability**: $50M minimum
   - Data breach response
   - System breach remediation
   - Customer notification costs
   - Regulatory fines (partial)

4. **Directors & Officers (D&O)**: $5M minimum
   - Legal disputes
   - Shareholder lawsuits
   - Regulatory actions

5. **Crime Insurance**: $5M minimum
   - Malicious code injection
   - System sabotage
   - Theft of intellectual property

### 8.3 Self-Insurance Fund

For calculation errors and operational disputes:

```erlang
%% Self-insurance reserve

-record(insurance_fund, {
    fund_id :: binary(),
    target_balance :: float(),         % Maintain $500K-$2M
    current_balance :: float(),

    % Claims
    claims :: [claim_record()],
    total_claims_current_year :: float(),
    total_claims_history :: float(),

    % Funding
    monthly_contribution :: float(),   % 0.5% of revenue
    funding_rule :: 'quarterly_review'
}).

%% Claim from self-insurance fund

claim_from_insurance_fund(TenantId, Amount, Reason) ->
    case validate_claim(TenantId, Amount, Reason) of
        {error, invalid} -> {error, claim_denied};
        {ok, _} ->
            Claim = #claim_record{
                claim_id = uuid:v7(),
                tenant_id = TenantId,
                amount = Amount,
                reason = Reason,
                status = pending,
                approval_chain = []
            },
            store_claim(Claim),
            % Require approval for claims > $10K
            case Amount > 10000 of
                true -> notify_approver(Claim);
                false -> process_claim(Claim)
            end,
            {ok, Claim}
    end.

%% CLAIM APPROVAL RULES:
%% - $0-$1K: Auto-approved (system)
%% - $1K-$10K: Requires Finance Manager approval (within 48 hours)
%% - $10K-$100K: Requires Finance Director approval (within 5 business days)
%% - $100K+: Requires CFO + General Counsel approval (within 10 business days)
%% - All claims must be documented with supporting evidence
%% - Claims must be reported to insurers within 60 days
```

---

## 9. Monitoring & Alerting

### 9.1 Real-Time Monitoring Dashboard

```erlang
%% Value pricing system monitoring

monitoring_dashboard() ->
    #{
        % Real-time metrics
        active_customers => count_active_customers(),
        today_revenue => sum_today_revenue(),
        avg_calculation_time_ms => avg_calculation_latency(),

        % Anomaly counters
        disputed_values => count_disputed_values_30days(),
        override_count => count_overrides_30days(),
        failed_calculations => count_calculation_failures_30days(),

        % Security metrics
        unauthorized_access_attempts => count_access_denials_30days(),
        tampering_alerts => count_tampering_alerts_30days(),
        audit_log_gaps => detect_audit_gaps(),

        % System health
        receipt_generation_rate => receipts_per_second(),
        database_replication_lag => replication_lag_ms(),
        backup_status => {last_backup_time, status}
    }.

%% Alert thresholds (trigger PagerDuty)

alert_rules() ->
    [
        % Performance
        {calculation_latency_p99 > 500, high, <<"Slow calculations">>},
        {receipt_backlog > 10000, critical, <<"Calculation queue backlog">>},

        % Security
        {unauthorized_access_attempts > 100_per_hour, high, <<"Access attack">>},
        {tampering_alerts > 0, critical, <<"Tampering detected">>},
        {audit_log_gap > 0, critical, <<"Audit integrity failure">>},

        % Data quality
        {override_requests_per_customer > 10_per_month, medium, <<"Override abuse">>},
        {dispute_rate > 5_percent, medium, <<"High dispute rate">>},
        {calculation_mismatch > 0, critical, <<"Calculation error">>},

        % System
        {database_replication_lag > 5_seconds, high, <<"Replication lag">>},
        {backup_failure > 0, high, <<"Backup failed">>},
        {disk_usage > 90_percent, medium, <<"Storage critical">>}
    ].
```

### 9.2 Metrics to Track

| Metric | Target | Alert |
|--------|--------|-------|
| P99 Calculation Latency | <500ms | >1s |
| Receipt Generation Rate | 1000/sec | <100/sec |
| Calculation Accuracy | 100% | <99.9% |
| Dispute Rate | <1% | >5% |
| Override Rate | <0.1% | >1% |
| Unauthorized Access | 0 | >5/hour |
| Audit Log Gaps | 0 | >0 |
| Database Replication Lag | <100ms | >5s |

---

## 10. Penetration Testing

### 10.1 Penetration Testing Plan

See [FRAUD_PREVENTION_GUIDE.md](./FRAUD_PREVENTION_GUIDE.md) for detailed pentest scenarios and execution plan.

**Key Test Areas:**
1. Direct database modification
2. API parameter tampering
3. Receipt forgery
4. Audit log manipulation
5. Formula injection
6. Cross-customer data access
7. Admin interface bypass
8. Cryptographic signature forgery

---

## Implementation Checklist

- [ ] Cryptographic receipt system (Erlang implementation)
- [ ] Row-level security policies (PostgreSQL)
- [ ] mTLS certificate infrastructure (Ops)
- [ ] Pricing override workflow (UI + backend)
- [ ] Formula deployment pipeline (CI/CD)
- [ ] Audit logging system (append-only storage)
- [ ] Anomaly detection alerts (PagerDuty integration)
- [ ] Incident response procedures (documented, drilled)
- [ ] Insurance policies (reviewed by legal)
- [ ] Penetration testing (external firm)
- [ ] Security training (all employees)
- [ ] Backup & recovery procedures (tested)
- [ ] Regulatory compliance audit (external auditor)

---

## Appendix: References

- [NIST Cybersecurity Framework](https://www.nist.gov/cyberframework)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [Payment Card Industry Data Security Standard (PCI-DSS)](https://www.pcisecuritystandards.org/)
- [SOC 2 Type II Audit](https://www.aicpa.org/interestareas/informationmanagement/sodp-system-and-organization-controls)
- [HIPAA Security Rule](https://www.hhs.gov/hipaa/for-professionals/security/index.html) (if handling health data)

---

**Document Owner:** Chief Security Officer
**Last Reviewed:** 2026-01-25
**Next Review:** 2026-04-25
**Classification:** CONFIDENTIAL - FINANCIAL SECURITY
