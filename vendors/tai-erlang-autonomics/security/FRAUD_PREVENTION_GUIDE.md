# Fraud Prevention Guide: Value-Indexed Pricing System

**Document Classification:** CONFIDENTIAL - INTERNAL USE ONLY
**Version:** 1.0.0
**Last Updated:** 2026-01-25
**Audience:** Security Team, Compliance, Engineering, C-Suite

---

## Executive Summary

This guide details fraud prevention controls and penetration testing scenarios for the value-indexed pricing system. It covers attack methodologies, detection techniques, and counter-measures for each major attack vector.

**Philosophy:** "Assume breach" - every control assumes attackers have already penetrated the system and are trying to:
1. Modify historical pricing
2. Underreport customer value to reduce fees
3. Create false receipts to justify disputes
4. Hide evidence in audit logs
5. Steal customer data for leverage

---

## Table of Contents

1. [Fraud Categories](#fraud-categories)
2. [Attack Vectors & Countermeasures](#attack-vectors--countermeasures)
3. [Detection Techniques](#detection-techniques)
4. [Penetration Testing Playbook](#penetration-testing-playbook)
5. [Fraud Investigation Procedures](#fraud-investigation-procedures)
6. [Customer Dispute Resolution](#customer-dispute-resolution)

---

## 1. Fraud Categories

### Category A: Value Underreporting (Customer Self-Service)

**Description:** Customer submits false metrics to reduce their billing
**Frequency:** MEDIUM-HIGH (20-30% of customers attempt at some level)
**Severity:** MEDIUM (average impact: $5K-$50K per incident)
**Detection:** Medium difficulty

#### Attack Examples:

```
Scenario 1: Revenue Underreporting
├─ Actual metrics: Revenue = $10M/year, Fee = $35K (0.35%)
├─ Submitted metrics: Revenue = $2M/year, Fee = $7K (0.35%)
├─ Underreported by: $280K over contract term
└─ Customer justification: "Metrics include internal estimates"

Scenario 2: Usage Manipulation
├─ Actual usage: 100K API calls/month
├─ Submitted usage: 50K API calls/month
├─ Reduced fee: 50% discount for lower-tier pricing
└─ Incentive: $10K-$20K monthly savings

Scenario 3: Partial Metrics Submission
├─ Actual business unit: 3 divisions generating value
├─ Submitted metrics: Only Division 1 (largest division excluded)
├─ Impact: 70% value underreported
└─ Customer claim: "Other divisions are separate contracts"
```

#### Prevention Controls:

```erlang
%% Validate metric sources - ensure submitted metrics match reality

validate_metric_submission(TenantId, Metrics) ->
    % Cross-check with system data
    case {
        validate_revenue_metrics(TenantId, Metrics),
        validate_usage_metrics(TenantId, Metrics),
        validate_coverage_completeness(TenantId, Metrics)
    } of
        {{ok, _}, {ok, _}, {ok, _}} ->
            {ok, metrics_valid};

        {{error, Reason1}, _, _} ->
            {error, {revenue_mismatch, Reason1}};

        {_, {error, Reason2}, _} ->
            {error, {usage_mismatch, Reason2}};

        {_, _, {error, Reason3}} ->
            {error, {incomplete_coverage, Reason3}}
    end.

%% Revenue validation: Compare submitted vs actual (if available)
validate_revenue_metrics(TenantId, Metrics) ->
    SubmittedRevenue = Metrics#submitted_metrics.revenue,

    % If we have integration with customer's system (Salesforce, Stripe, etc):
    case get_verified_revenue_source(TenantId) of
        {ok, VerifiedRevenue} ->
            Variance = abs(SubmittedRevenue - VerifiedRevenue) / VerifiedRevenue,
            case Variance < 0.05 of  % Allow 5% variance for timing
                true -> {ok, variance_acceptable};
                false ->
                    {error, {variance_too_high, Variance}}
            end;

        {error, no_integration} ->
            % No integration - require customer attestation
            case Metrics#submitted_metrics.attestation_signature of
                undefined ->
                    {error, missing_attestation};
                Sig ->
                    % Verify customer signed off on these numbers
                    case verify_customer_signature(TenantId, Sig) of
                        true -> {ok, customer_attested};
                        false -> {error, invalid_signature}
                    end
            end
    end.

%% Usage validation: Compare API call logs vs submitted
validate_usage_metrics(TenantId, Metrics) ->
    SubmittedCalls = Metrics#submitted_metrics.api_calls,
    LoggedCalls = count_actual_api_calls(TenantId),

    Variance = abs(SubmittedCalls - LoggedCalls) / LoggedCalls,
    case Variance < 0.02 of  % Allow 2% variance for timing/batching
        true -> {ok, usage_matches_logs};
        false ->
            {error, {usage_mismatch, SubmittedCalls, LoggedCalls}}
    end.

%% Coverage completeness: Ensure all business units reported
validate_coverage_completeness(TenantId, Metrics) ->
    EnrolledBizUnits = get_enrolled_business_units(TenantId),
    ReportedBizUnits = Metrics#submitted_metrics.business_units,

    Missing = EnrolledBizUnits -- ReportedBizUnits,
    case Missing of
        [] -> {ok, all_units_reported};
        _ ->
            {error, {incomplete_coverage, Missing}}
    end.
```

### Category B: Historical Receipt Tampering (Insider Fraud)

**Description:** Employee or compromised account modifies historical pricing records
**Frequency:** LOW (but devastating if occurs)
**Severity:** CRITICAL (can create $500K+ fraudulent charges or refunds)
**Detection:** Very difficult without cryptographic controls

#### Attack Examples:

```
Scenario 1: Direct Database Modification
├─ Attacker: Database administrator or compromised account
├─ Target: pricing_receipts table, historical records
├─ Attack: UPDATE pricing_receipts SET calculated_value = 0 WHERE created_at < '2025-01-01'
├─ Cover-up: Delete audit log entries showing the modification
└─ Impact: Create fake $1M refund claim without trace

Scenario 2: Calculation Engine Backdoor
├─ Attacker: Deployment pipeline access (DevOps engineer)
├─ Target: Calculation formula code
├─ Attack: Inject code that underreports by 50% for specific customers
├─ Stealth: Make change look like legitimate formula update
└─ Impact: $500K/year fraud, affecting 20-50 customers

Scenario 3: Audit Log Deletion
├─ Attacker: Database access or log system access
├─ Target: audit_log table (should be immutable)
├─ Attack: DELETE FROM audit_log WHERE action = 'pricing_override'
├─ Cover-up: Modify previous/next log entry hashes to hide gap
└─ Impact: Hide evidence of $100K+ fraudulent override
```

#### Prevention Controls:

```erlang
%% Immutable receipts - CANNOT update or delete

%% PostgreSQL: Enforce immutability at database level
CREATE TABLE pricing_receipts (
    receipt_id UUID PRIMARY KEY,
    tenant_id UUID NOT NULL,
    calculated_value FLOAT NOT NULL,

    -- Cryptographic proof this record cannot be modified
    receipt_hash BYTEA NOT NULL,
    previous_hash BYTEA NOT NULL,
    signature BYTEA NOT NULL,

    -- Audit trail
    created_at TIMESTAMP NOT NULL,
    created_by UUID NOT NULL,

    -- This prevents ANY update or delete
    CONSTRAINT immutable_receipt CHECK (created_at = created_at)
);

-- Disable all UPDATE/DELETE
CREATE POLICY immutable_receipts_prevent_update ON pricing_receipts
    FOR UPDATE USING (false);

CREATE POLICY immutable_receipts_prevent_delete ON pricing_receipts
    FOR DELETE USING (false);

-- Only allow INSERT
CREATE POLICY immutable_receipts_allow_insert ON pricing_receipts
    FOR INSERT WITH CHECK (true);

%% Application-level verification

verify_receipt_not_tampered(ReceiptId) ->
    Receipt = get_receipt(ReceiptId),

    % Verify cryptographic chain
    case verify_chain_integrity(Receipt) of
        true -> {ok, not_tampered};
        false ->
            % Tampering detected - alert security immediately
            alert_security({tampering_detected, ReceiptId}),
            {error, {tampering_detected, ReceiptId}}
    end.

%% Prevent formula tampering via code review + signing

deploy_formula_safely(FormulaCode, ReviewerSignature, QASignature) ->
    % Step 1: Verify code hasn't been modified
    CodeHash = crypto:hash(sha256, FormulaCode),

    % Step 2: Verify reviewer approval
    case verify_signature(ReviewerSignature, CodeHash) of
        false -> {error, invalid_reviewer_signature};
        true ->
            % Step 3: Verify QA sign-off
            case verify_signature(QASignature, CodeHash) of
                false -> {error, invalid_qa_signature};
                true ->
                    % Step 4: Store immutable formula record
                    Formula = #pricing_formula{
                        code = FormulaCode,
                        code_hash = CodeHash,
                        reviewer_sig = ReviewerSignature,
                        qa_sig = QASignature,
                        deployed_at = timestamp()
                    },
                    store_formula(Formula),
                    {ok, Formula}
            end
    end.

%% Audit log immutability - detect any modification

verify_audit_log_integrity() ->
    % Get all entries in order
    AllEntries = query_audit_log_all(),

    % Verify each entry
    verify_audit_chain_recursive(AllEntries, crypto:hash(sha256, <<>>)).

verify_audit_chain_recursive([], _PriorHash) ->
    {ok, audit_log_valid};

verify_audit_chain_recursive([Entry | Rest], PriorHash) ->
    CurrentHash = Entry#audit_entry.entry_hash,
    PreviousHash = Entry#audit_entry.previous_hash,

    case PreviousHash =:= PriorHash of
        false ->
            % TAMPERING DETECTED: Chain is broken
            alert_security({audit_tampering, Entry#audit_entry.entry_id}),
            {error, {tampering_detected, Entry#audit_entry.entry_id}};
        true ->
            % Continue verification
            verify_audit_chain_recursive(Rest, CurrentHash)
    end.
```

### Category C: Pricing Override Abuse (Internal Fraud)

**Description:** Employee approves unauthorized discounts or adjustments
**Frequency:** MEDIUM (10-20% of override requests are questionable)
**Severity:** MEDIUM-HIGH (average impact: $10K-$100K per incident)
**Detection:** Medium difficulty

#### Attack Examples:

```
Scenario 1: Unauthorized Refund
├─ Attacker: Support agent or Finance employee
├─ Method: Override pricing for favored customer without proper justification
├─ Example: "Grant $50K refund to Customer X for goodwill"
├─ Reality: Customer paid $50K bribe to employee
└─ Detection: Pattern of refunds to same customer, no supporting docs

Scenario 2: Competitive Pricing Manipulation
├─ Attacker: Sales rep or Finance analyst
├─ Method: Lower pricing for specific customer to close deal
├─ Example: "Grant 50% discount to land strategic account"
├─ Reality: No board approval; undermines pricing integrity
└─ Detection: Discounts inconsistent with deal value; no customer request

Scenario 3: Reciprocal Fraud
├─ Attacker: Multiple employees colluding
├─ Method: Customer X pays employee, employee approves override for Customer Y
├─ Example: Exchange favors between employees
└─ Detection: Unusual pattern of approvals; cluster analysis shows collusion
```

#### Prevention Controls:

```erlang
%% Strict override approval workflow - no single approver

request_override(TenantId, OverrideValue, Reason) ->
    Amount = abs(OverrideValue - get_current_value(TenantId)),
    Requester = current_user(),

    % Check for suspicious patterns BEFORE creating override
    case detect_override_abuse(Requester, Amount) of
        {suspicious, Pattern} ->
            % Auto-reject if pattern is suspicious
            alert_compliance({override_suspicious, Requester, Pattern}),
            {error, {suspicious_pattern, Pattern}};

        safe ->
            % Determine approval chain based on amount
            ApprovalChain = compute_approval_chain(Amount),

            Override = #pricing_override{
                override_id = uuid:v7(),
                requested_by = Requester,
                amount = Amount,
                reason = Reason,
                required_approvals = ApprovalChain,
                approvals_received = [],
                status = pending
            },

            store_override(Override),
            notify_approvers(Override),
            {ok, Override}
    end.

%% Compute approval chain - more complex for larger amounts

compute_approval_chain(Amount) when Amount =< 1000 ->
    [
        {finance_manager, within_24_hours},
        {payment_verification, automatic}  % Verify with customer
    ];

compute_approval_chain(Amount) when Amount =< 10000 ->
    [
        {finance_manager, within_24_hours},
        {director_of_finance, within_48_hours},
        {payment_verification, automatic}
    ];

compute_approval_chain(Amount) when Amount =< 100000 ->
    [
        {finance_manager, within_24_hours},
        {director_of_finance, within_48_hours},
        {cfo, within_5_business_days},
        {payment_verification, automatic},
        {customer_attestation, required}  % Customer must confirm acceptance
    ];

compute_approval_chain(Amount) ->
    [
        {finance_manager, within_24_hours},
        {director_of_finance, within_48_hours},
        {cfo, within_5_business_days},
        {general_counsel, within_10_business_days},
        {board_approval, required},  % Board must approve for >$100K
        {payment_verification, automatic},
        {customer_attestation, required},
        {audit_committee_notification, required}
    ].

%% Detect suspicious override patterns

detect_override_abuse(UserId, Amount) ->
    [
        % Pattern 1: Too many overrides from same user
        case count_user_overrides(UserId, last_30_days) > 10 of
            true -> {suspicious, too_many_overrides};
            false -> safe
        end,

        % Pattern 2: Concentrated approvals to same user
        case get_overrides_by_approver(UserId, last_30_days) |> length() > 5 of
            true -> {suspicious, cluster_of_approvals};
            false -> safe
        end,

        % Pattern 3: Overrides to same customer
        case count_overrides_to_customer(UserId, get_customer(UserId), last_30_days) > 3 of
            true -> {suspicious, repeated_customer};
            false -> safe
        end,

        % Pattern 4: Large amounts for new customers
        case {Amount > 50000, is_customer_new(get_customer(UserId))} of
            {true, true} -> {suspicious, large_discount_new_customer};
            _ -> safe
        end,

        % Pattern 5: Overrides without supporting documentation
        case Reason =:= undefined of
            true -> {suspicious, no_reason_provided};
            false -> safe
        end,

        % Pattern 6: Overrides for related parties (conflict of interest)
        case has_relationship_with_customer(UserId, get_customer(UserId)) of
            true -> {suspicious, conflict_of_interest};
            false -> safe
        end
    ]
    |> lists:filter(fun(R) -> R =/= safe end)
    |> case
        [] -> safe;
        Patterns -> {suspicious, Patterns}
    end.

%% Verification: Override must match customer expectation

verify_override_acceptance(OverrideId) ->
    Override = get_override(OverrideId),
    TenantId = Override#pricing_override.tenant_id,
    OverrideAmount = Override#pricing_override.override_value,

    % Send email to customer: "Your pricing has been adjusted from $X to $Y"
    % Require customer to confirm within 48 hours
    % If not confirmed, override is automatically rejected

    case wait_for_customer_response(TenantId, OverrideId, timeout_hours(48)) of
        {customer_confirmed, Timestamp} ->
            % Customer explicitly accepted - override proceeds
            execute_override(OverrideId),
            log_approval(OverrideId, Timestamp);

        {customer_rejected, Timestamp} ->
            % Customer rejected - override cancelled
            reject_override(OverrideId, Timestamp);

        timeout ->
            % Customer didn't respond - override rejected (conservative)
            reject_override(OverrideId, "Customer did not confirm")
    end.
```

### Category D: Calculation Engine Bugs (Accidental)

**Description:** Genuine calculation logic errors causing wrong pricing
**Frequency:** LOW (if testing is comprehensive)
**Severity:** MEDIUM (can affect many customers)
**Detection:** Easy with monitoring

#### Examples & Prevention

```erlang
%% Common calculation bugs and how to prevent them

% Bug 1: Integer division instead of float
% WRONG: Value = Revenue / 100  (integer division)
% RIGHT: Value = Revenue / 100.0  (float division)

% Bug 2: Rounding errors
% WRONG: round(Value * 100) / 100  (loses precision)
% RIGHT: Use decimal library for financial calculations

% Bug 3: Off-by-one in date calculations
% WRONG: count_days(StartDate, EndDate) = EndDate - StartDate
% RIGHT: count_days(...) = EndDate - StartDate + 1

% Bug 4: Timezone confusion
% WRONG: Compare timestamps without converting to same timezone
% RIGHT: Always use UTC, convert only for display

% Bug 5: Null/undefined handling
% WRONG: Value = A + B  (crash if B is nil)
% RIGHT: Value = A + get_with_default(B, 0)

%% Prevent via testing + monitoring

test_calculation_formula() ->
    % Test Cases
    [
        {revenue_10k, {10000, {10000 * 0.035}},
        {revenue_100k, {100000, {100000 * 0.035}},
        {revenue_1m, {1000000, {1000000 * 0.035}},

        % Edge cases
        {zero_revenue, {0, 0}},
        {negative_revenue, {-100, {error, invalid_input}},
        {very_large_revenue, {999999999999, {999999999999 * 0.035}},
        {float_precision, {100.50, {100.50 * 0.035}},

        % Null cases
        {missing_metric, {undefined, {error, missing_metric}},
    ]
    |> Enum.each(fn {name, {input, expected}} ->
        result = calculate_value(input),
        assert result == expected, "Test failed: #{name}"
    end).

%% Monitor for calculation mismatches

detect_calculation_errors() ->
    % Every hour, recalculate 10% random sample of receipts
    SampleReceipts = query_random_receipts(percent(10), last_7_days),

    Mismatches = SampleReceipts
        |> Enum.filter(fn receipt ->
            recalculated = recalculate_from_metrics(receipt),
            recalculated != receipt.calculated_value
        end),

    case length(Mismatches) > 0 of
        true ->
            alert_pagerduty({
                alert_type = calculation_mismatch,
                severity = critical,
                affected_count = length(Mismatches),
                receipts = Mismatches
            });
        false -> ok
    end.
```

---

## 2. Attack Vectors & Countermeasures

### Vector 1: Direct Database Manipulation

#### Attack Method
```sql
-- Attacker: DBA or compromised account
-- Goal: Modify pricing without leaving trace

UPDATE pricing_receipts
SET calculated_value = 0
WHERE receipt_id = 'abc-123';

DELETE FROM audit_log
WHERE receipt_id = 'abc-123';

-- Cover tracks by updating hash chain
UPDATE audit_log
SET entry_hash = NEW_HASH
WHERE entry_id = (SELECT id BEFORE abc-123);
```

#### Countermeasures
1. **Immutable Tables:** No UPDATE/DELETE allowed at database level
2. **Role Separation:** DBA cannot execute pricing-related queries
3. **Append-Only Logs:** Audit entries can only be inserted, never modified
4. **Cryptographic Hashing:** Any modification breaks hash chain (detectable)
5. **External Verification:** Monthly export of audit logs to tamper-proof storage (S3 with MFA-delete)

#### Detection

```erlang
%% Detect database manipulation attempts

monitor_database_access() ->
    % Flag any UPDATE/DELETE on pricing_receipts or audit_log
    case execute_query("SELECT COUNT(*) FROM audit_log WHERE action IN ('UPDATE', 'DELETE') AND table_name IN ('pricing_receipts', 'audit_log')") of
        Count when Count > 0 ->
            alert_security({database_tampering_detected, Count});
        _ -> ok
    end,

    % Verify audit log chain integrity
    case verify_audit_chain() of
        {ok, valid} -> ok;
        {error, gap} ->
            alert_security({audit_log_gap_detected})
    end,

    % Check for large data exports
    case query_recent_large_exports() of
        [] -> ok;
        Exports ->
            alert_security({suspicious_data_export, Exports})
    end.
```

### Vector 2: API Parameter Tampering

#### Attack Method
```bash
# Attacker: Customer or compromised API client
# Goal: Modify request to underreport metrics

curl -X POST https://pricing.example.com/api/submit-metrics \
  -H "Authorization: Bearer TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "revenue": 1000000,    # False: actually $10M
    "api_calls": 50000,    # False: actually 500K
    "signature": "XXXX"    # Fake signature
  }'
```

#### Countermeasures
1. **Request Signing:** Every API request must be signed (HMAC-SHA256)
2. **mTLS:** Certificate-based authentication (immune to stolen tokens)
3. **Timestamp Validation:** Reject requests older than 5 minutes (prevent replay)
4. **Rate Limiting:** Max 10 metric submissions per day per customer
5. **Parameter Validation:** Each metric cross-checked against system data

#### Detection

```erlang
validate_api_request(Request) ->
    case {
        validate_signature(Request),
        validate_mtls_cert(Request),
        validate_timestamp(Request),
        validate_rate_limit(Request),
        validate_parameters(Request)
    } of
        {true, true, true, true, true} -> {ok, valid};
        {false, _, _, _, _} -> {error, invalid_signature};
        {_, false, _, _, _} -> {error, invalid_certificate};
        {_, _, false, _, _} -> {error, request_expired};
        {_, _, _, false, _} -> {error, rate_limit_exceeded};
        {_, _, _, _, false} -> {error, invalid_parameters}
    end.
```

### Vector 3: Calculation Formula Injection

#### Attack Method
```erlang
%% Attacker: DevOps engineer or compromised account
%% Goal: Inject malicious calculation formula

% Current formula (correct)
calculate_value(Metrics) ->
    Metrics#metrics.revenue * 0.035.

% Injected formula (malicious)
calculate_value(Metrics) ->
    % Always report 50% of actual value
    ActualValue = Metrics#metrics.revenue * 0.035,
    ActualValue * 0.5.  % Fraud: underreports by 50%
```

#### Countermeasures
1. **Code Review:** All formula changes require 2 approvals
2. **Testing:** New formulas must pass 100% of test cases
3. **Immutable Versions:** Formula code hash-signed and versioned
4. **Gradual Rollout:** New formula tested on 1% customers first
5. **Recalculation Audits:** Regularly recalculate historical values with formula hash verification

#### Detection

```erlang
%% Detect formula injection

detect_formula_injection() ->
    % Get currently deployed formula
    DeployedFormula = get_active_formula(),
    DeployedHash = crypto:hash(sha256, DeployedFormula#formula.code),

    % Check against stored formula hash
    StoredHash = get_formula_hash(DeployedFormula#formula.version),

    case DeployedHash =:= StoredHash of
        true -> {ok, formula_unmodified};
        false ->
            % Formula code doesn't match stored hash - possible injection!
            alert_security({formula_injection_detected,
                DeployedFormula#formula.version}),
            {error, formula_modified}
    end.

%% Recalculation audit: Verify results match stored values
recalculation_audit() ->
    % Sample 5% of receipts from last 30 days
    Receipts = query_random_receipts(percent(5), last_30_days),

    Mismatches = Receipts
        |> Enum.filter(fn receipt ->
            % Recalculate using CURRENT formula
            recalculated = recalculate_value(
                receipt#pricing_receipt.metrics_snapshot,
                receipt#pricing_receipt.formula_version
            ),

            % Compare with stored value
            stored = receipt#pricing_receipt.calculated_value,
            abs(recalculated - stored) > 0.01
        end),

    case length(Mismatches) > 0 of
        true ->
            alert_security({formula_mismatch, Mismatches});
        false ->
            ok
    end.
```

### Vector 4: Audit Log Deletion

#### Attack Method
```sql
-- Attacker: DBA or root access
-- Goal: Hide evidence of fraudulent override

DELETE FROM audit_log WHERE action = 'override' AND customer_id = '123';

-- Modify surrounding entries to hide the gap
UPDATE audit_log SET previous_hash = prev_prev_hash WHERE id = next_id;
```

#### Countermeasures
1. **Append-Only Storage:** No DELETE at database level
2. **Hash Chain:** Gap in hash chain is immediately detectable
3. **Cryptographic Signatures:** Each entry signed; modification breaks signature
4. **External Copies:** Monthly export to immutable storage (S3, GCS)
5. **Blockchain Notarization:** Option to hash audit logs to blockchain for critical events

#### Detection

```erlang
%% Detect audit log gaps

verify_audit_log_integrity() ->
    AllEntries = query_all_audit_entries(),
    verify_chain(AllEntries, undefined, 0).

verify_chain([], _PriorHash, _Count) ->
    {ok, audit_valid};

verify_chain([Entry | Rest], PriorHash, Count) ->
    case verify_entry(Entry, PriorHash) of
        false ->
            % Entry doesn't link to prior - gap detected!
            alert_security({audit_log_gap_detected,
                entry_id => Entry#audit_entry.entry_id,
                position => Count,
                prior_hash => PriorHash}),
            {error, {gap_detected, Count}};

        true ->
            verify_chain(Rest, Entry#audit_entry.entry_hash, Count + 1)
    end.

verify_entry(Entry, PriorHash) ->
    % Verify signature
    case crypto:verify(sha256, Entry#audit_entry.signature, entry_data(Entry), system_public_key()) of
        false -> false;  % Signature doesn't match
        true ->
            % Verify chain link
            Entry#audit_entry.previous_hash =:= PriorHash
    end.
```

---

## 3. Detection Techniques

### Real-Time Anomaly Detection

```erlang
%% Detect suspicious patterns in real-time

anomaly_detector() ->
    % Monitor 5 streams of data concurrently
    [
        detect_value_outliers(),      % Unusual pricing for customer
        detect_override_abuse(),      % Too many approvals per person
        detect_access_anomalies(),    % Unusual data access patterns
        detect_calculation_drift(),   % Values changing unexpectedly
        detect_formula_modifications() % Code changes without approval
    ]
    |> Enum.map(fn detector -> spawn(detector) end).

%% Technique 1: Statistical anomaly detection

detect_value_outliers() ->
    % For each customer, track value distribution
    % Alert if customer's new value is > 3 standard deviations from mean

    AllCustomers = query_all_customers(),
    Enum.each(AllCustomers, fn customer ->
        Values = query_customer_values(customer, last_180_days),
        Mean = statistics:mean(Values),
        StdDev = statistics:stddev(Values),
        LatestValue = hd(Values),

        ZScore = abs(LatestValue - Mean) / StdDev,
        case ZScore > 3 do
            true ->
                alert_medium({value_anomaly,
                    customer: customer,
                    z_score: ZScore,
                    latest_value: LatestValue,
                    expected_range: {Mean - 3*StdDev, Mean + 3*StdDev}});
            false -> ok
        end
    end).

%% Technique 2: Behavioral analysis

detect_override_abuse() ->
    % Identify employees with unusual override patterns
    Employees = query_all_employees(),
    Enum.each(Employees, fn emp ->
        Overrides = query_overrides_by(emp, last_30_days),
        OverrideValue = Enum.sum([O#override.amount || O <- Overrides]),
        ApprovalRate = count_approvals_by(emp) / length(Overrides),

        case {length(Overrides) > 10, OverrideValue > 100000, ApprovalRate > 0.3} of
            {true, true, true} ->
                alert_high({override_abuse_pattern,
                    employee: emp,
                    override_count: length(Overrides),
                    total_value: OverrideValue,
                    approval_rate: ApprovalRate});
            _ -> ok
        end
    end).

%% Technique 3: Access pattern analysis

detect_access_anomalies() ->
    % Flag unusual data access:
    % - Employee accessing competitor's data
    % - Bulk data export
    % - Access from unusual location

    LoopbackAccess = query_access_logs(last_24_hours),
    Enum.each(LoopbackAccess, fn access ->
        case {
            is_cross_customer_access(access),
            is_bulk_export(access),
            is_unusual_geolocation(access)
        } of
            {true, _, _} ->
                alert_high({cross_customer_access, access});
            {_, true, _} ->
                alert_high({bulk_export_detected, access});
            {_, _, true} ->
                alert_medium({unusual_geolocation, access});
            _ -> ok
        end
    end).

%% Technique 4: Formula drift detection

detect_calculation_drift() ->
    % Compare current formula results to historical baseline
    % Alert if calculation changes unexpectedly

    Receipts = query_receipts(last_7_days),
    Enum.each(Receipts, fn receipt ->
        Recalculated = recalculate_value(
            receipt#pricing_receipt.metrics_snapshot,
            receipt#pricing_receipt.formula_version
        ),
        Stored = receipt#pricing_receipt.calculated_value,

        Variance = abs(Recalculated - Stored) / Stored,
        case Variance > 0.01 of  % > 1% difference
            true ->
                alert_high({calculation_drift,
                    receipt_id: receipt#pricing_receipt.receipt_id,
                    stored_value: Stored,
                    recalculated_value: Recalculated,
                    variance: Variance});
            false -> ok
        end
    end).
```

---

## 4. Penetration Testing Playbook

### PT Scenario 1: Direct Database Modification

**Objective:** Attempt to modify pricing_receipts table directly

**Attack Steps:**
```bash
1. Obtain database credentials (social engineering / credential theft)
2. Connect to production database
3. Modify historical receipt: UPDATE pricing_receipts SET calculated_value = 0
4. Delete audit log entry to cover tracks
5. Verify change persists

Expected Result: Attempt FAILS due to:
  - Table-level immutability enforcement (no UPDATE allowed)
  - Append-only audit log (no DELETE allowed)
  - Hash chain validation (gap detected)
```

**Verification:**
```erlang
%% PT Test: Verify database immutability

pt_test_database_immutability() ->
    % Attempt 1: Direct UPDATE
    case execute_raw_sql("UPDATE pricing_receipts SET calculated_value = 0 WHERE receipt_id = 'test'") of
        {error, "permission denied"} ->
            {pass, "Database prevents UPDATE"};
        {ok, _} ->
            {fail, "Database allowed UPDATE - immutability broken!"}
    end,

    % Attempt 2: Direct DELETE
    case execute_raw_sql("DELETE FROM pricing_receipts WHERE receipt_id = 'test'") of
        {error, "permission denied"} ->
            {pass, "Database prevents DELETE"};
        {ok, _} ->
            {fail, "Database allowed DELETE - immutability broken!"}
    end,

    % Attempt 3: Trigger tampering alert
    case verify_audit_chain() of
        {ok, valid} ->
            {pass, "Audit chain intact"};
        {error, gap} ->
            {fail, "Audit chain has gap - tampering detected"}
    end.
```

### PT Scenario 2: Receipt Forgery

**Objective:** Create fake receipt that passes verification

**Attack Steps:**
```
1. Create receipt with fabricated values
2. Compute fake hash
3. Sign fake hash with compromised key
4. Insert into database
5. Hope verification doesn't catch it

Expected Result: Forgery FAILS because:
  - Timestamp must link to previous receipt (chain breaks)
  - Signature requires system private key (not available)
  - Metrics hash must match submitted metrics (data mismatch)
```

**Verification:**
```erlang
%% PT Test: Verify receipt integrity

pt_test_receipt_forgery() ->
    % Create fake receipt
    FakeReceipt = #pricing_receipt{
        receipt_id = uuid:v7(),
        calculated_value = 1000.0,  % Fake value
        signature = crypto:strong_rand_bytes(64)  % Random signature
    },

    % Attempt verification
    case verify_receipt(FakeReceipt) of
        {ok, verified} ->
            {fail, "Fake receipt passed verification - CRITICAL BUG"};
        {error, {signature_invalid, _}} ->
            {pass, "Signature verification failed"};
        {error, {chain_broken, _}} ->
            {pass, "Chain verification failed"};
        {error, {metrics_mismatch, _}} ->
            {pass, "Metrics verification failed"}
    end.
```

### PT Scenario 3: Cross-Customer Data Access

**Objective:** Customer A accesses Customer B's pricing data

**Attack Steps:**
```
1. Customer A authenticates to API
2. Requests data with customer_id = B's customer_id
3. Server returns B's data due to missing tenant_id filter
4. Customer A exfiltrates B's metrics

Expected Result: Access DENIED because:
  - Row-level security filters by tenant_id
  - All queries must include tenant_id in WHERE clause
  - Missing filter triggers alert
```

**Verification:**
```erlang
%% PT Test: Verify row-level security

pt_test_cross_customer_access() ->
    CustomerA = create_test_customer(<<"customer_a">>),
    CustomerB = create_test_customer(<<"customer_b">>),

    % Customer A tries to query Customer B's data
    case query_as_customer(CustomerA, "SELECT * FROM pricing_receipts WHERE customer_id = ?", [CustomerB]) of
        {ok, []} ->
            {pass, "Row-level security enforced - no data leaked"};
        {ok, Results} when length(Results) > 0 ->
            {fail, "Customer A can see Customer B's data - CRITICAL BUG"};
        {error, _} ->
            {pass, "Query rejected"}
    end.
```

### PT Scenario 4: Formula Injection

**Objective:** Inject malicious calculation formula

**Attack Steps:**
```
1. Gain DevOps/deployment access
2. Modify calculation formula code
3. Deploy new version
4. Fraudulently underreport customer values
5. Hide the change

Expected Result: Injection PREVENTED because:
  - Code review required (2 approvals)
  - Formula hash verified before execution
  - Change triggers audit log entry
  - Staged rollout catches issues early
```

**Verification:**
```erlang
%% PT Test: Verify formula integrity

pt_test_formula_injection() ->
    OriginalFormula = get_deployed_formula(),
    OriginalHash = crypto:hash(sha256, OriginalFormula#formula.code),

    % Attempt to inject malicious formula
    MaliciousCode = <<"calculate_value(M) -> M#metrics.revenue * 0.5.">>,  % Fraud

    % Try to deploy without code review
    case deploy_formula_without_review(MaliciousCode) of
        {error, code_review_required} ->
            {pass, "Formula injection blocked - code review required"};
        {ok, _} ->
            {fail, "Formula deployed without review - CRITICAL BUG"}
    end,

    % Verify hash hasn't changed
    CurrentHash = crypto:hash(sha256, get_deployed_formula()#formula.code),
    case CurrentHash =:= OriginalHash of
        true ->
            {pass, "Formula hash matches - no injection detected"};
        false ->
            {fail, "Formula changed - injection possible"}
    end.
```

### PT Scenario 5: Audit Log Manipulation

**Objective:** Delete audit log entries to hide fraudulent override

**Attack Steps:**
```
1. Gain database access
2. Delete audit_log entry documenting override
3. Modify surrounding entries' hash chain to hide gap
4. Verify no evidence remains

Expected Result: Manipulation DETECTED because:
  - Append-only storage prevents DELETE
  - Hash chain breaks (any modification breaks link)
  - Gap detected by audit chain verification
```

**Verification:**
```erlang
%% PT Test: Verify audit immutability

pt_test_audit_manipulation() ->
    % Get count of audit entries
    OriginalCount = count_audit_entries(),

    % Attempt to delete an entry
    case execute_raw_sql("DELETE FROM audit_log WHERE id = 123") of
        {error, "permission denied"} ->
            {pass, "Delete prevented"};
        {ok, _} ->
            {fail, "Delete allowed - CRITICAL BUG"}
    end,

    % Verify chain integrity
    case verify_audit_chain() of
        {ok, valid} ->
            {pass, "Audit chain is valid"};
        {error, {gap_detected, Position}} ->
            {pass, "Gap detected at position #{Position}"}
    end,

    % Verify count unchanged
    NewCount = count_audit_entries(),
    case NewCount =:= OriginalCount of
        true ->
            {pass, "No entries deleted"};
        false ->
            {fail, "Entries missing - audit log compromised"}
    end.
```

---

## 5. Fraud Investigation Procedures

### Investigation Checklist

When fraud is suspected:

```
[ ] 1. IMMEDIATE: Isolate affected systems (don't shut down)
[ ] 2. IMMEDIATE: Preserve evidence (snapshot database, logs, memory)
[ ] 3. Within 1 hour: Notify Security, Legal, Finance leadership
[ ] 4. Within 4 hours: Engage external forensics firm
[ ] 5. Within 24 hours: Notify affected customers (if material impact)
[ ] 6. Within 7 days: Complete forensic investigation
[ ] 7. Within 14 days: File incident report with regulators (if required)
[ ] 8. Within 30 days: Complete remediation and re-test controls
```

### Investigation Workflow

```erlang
investigate_fraud(IncidentId) ->
    Incident = get_incident(IncidentId),

    % Phase 1: Evidence Preservation
    preserve_database_snapshot(),
    preserve_audit_logs(),
    preserve_system_state(),

    % Phase 2: Timeline Reconstruction
    Timeline = reconstruct_timeline(Incident),
    io:format("Timeline: ~p~n", [Timeline]),

    % Phase 3: Actor Analysis
    ActorBehavior = analyze_suspicious_actor(Incident),
    io:format("Suspicious Actor: ~p~n", [ActorBehavior]),

    % Phase 4: Receipt Verification
    AffectedReceipts = find_affected_receipts(Incident),
    ReceiptStatus = [verify_receipt(R) || R <- AffectedReceipts],
    io:format("Affected Receipts: ~p~n", [ReceiptStatus]),

    % Phase 5: Financial Impact
    ImpactAnalysis = calculate_fraud_impact(Incident),
    io:format("Financial Impact: ~p~n", [ImpactAnalysis]),

    % Phase 6: Root Cause
    RootCause = determine_root_cause(Incident),
    io:format("Root Cause: ~p~n", [RootCause]),

    {ok, #{
        incident => Incident,
        timeline => Timeline,
        actor_behavior => ActorBehavior,
        affected_receipts => AffectedReceipts,
        receipt_status => ReceiptStatus,
        impact_analysis => ImpactAnalysis,
        root_cause => RootCause
    }}.
```

---

## 6. Customer Dispute Resolution

### Dispute Verification Process

When customer disputes a charge:

```erlang
%% Verify disputed receipt

verify_disputed_receipt(TenantId, ReceiptId) ->
    Receipt = get_receipt(ReceiptId),

    % Step 1: Verify receipt signature
    case verify_receipt(Receipt) of
        {error, Reason} ->
            % Receipt is invalid - customer might have a point
            {invalid_receipt, Reason};

        {ok, verified} ->
            % Receipt is authentic - verify calculation

            % Step 2: Recalculate value
            Recalculated = recalculate_value(
                Receipt#pricing_receipt.metrics_snapshot,
                Receipt#pricing_receipt.formula_version
            ),
            Stored = Receipt#pricing_receipt.calculated_value,

            case abs(Recalculated - Stored) < 0.01 of
                true ->
                    % Calculation is correct
                    {calculation_correct, Receipt};

                false ->
                    % Calculation mismatch - raise alert
                    {calculation_error, {
                        stored: Stored,
                        recalculated: Recalculated
                    }}
            end
    end.

%% Resolution process

resolve_dispute(TenantId, ReceiptId, DisputeReason) ->
    case verify_disputed_receipt(TenantId, ReceiptId) of
        {invalid_receipt, _Reason} ->
            % Receipt is invalid - grant refund
            issue_refund(TenantId, ReceiptId, "Invalid receipt"),
            {resolution, refund_issued};

        {calculation_correct, Receipt} ->
            % Calculation is correct - deny dispute
            % But offer investigation to customer
            {resolution, dispute_denied_calculation_verified},
            notify_customer(TenantId, "Your dispute was reviewed. Calculation verified as accurate.");

        {calculation_error, {stored, S, recalculated, R}} ->
            % Error found - issue credit for difference
            Difference = abs(S - R),
            issue_credit(TenantId, Difference),
            {resolution, partial_credit_issued, Difference}
    end.
```

---

## Compliance Checklist

- [ ] Cryptographic receipt system deployed
- [ ] Row-level security policies enabled
- [ ] Pricing override workflow implemented
- [ ] Audit logging system operational
- [ ] Anomaly detection alerts configured
- [ ] Penetration tests completed
- [ ] Incident response procedures documented and drilled
- [ ] Insurance policies active
- [ ] Customer communication templates prepared
- [ ] External audit scheduled

---

**Document Owner:** Chief Security Officer
**Distribution:** Security, Compliance, Finance, Engineering, Legal
**Classification:** CONFIDENTIAL - INTERNAL USE ONLY
