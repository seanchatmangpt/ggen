<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Signal Contracts: Ingestion & Validation](#signal-contracts-ingestion--validation)
  - [Executive Summary](#executive-summary)
  - [HTTP Contract: Signal Ingestion](#http-contract-signal-ingestion)
    - [Endpoint](#endpoint)
    - [Path Parameters](#path-parameters)
    - [Headers](#headers)
    - [Request Body](#request-body)
    - [Required Fields](#required-fields)
    - [Optional Fields](#optional-fields)
    - [Response: 200 OK (Signal Accepted)](#response-200-ok-signal-accepted)
    - [Response: 400 Bad Request (Signal Rejected - Validation)](#response-400-bad-request-signal-rejected---validation)
    - [Response: 403 Forbidden (Signal Rejected - Security)](#response-403-forbidden-signal-rejected---security)
    - [Response: 429 Too Many Requests (Signal Throttled - Storm)](#response-429-too-many-requests-signal-throttled---storm)
  - [Validation Contract](#validation-contract)
    - [Pre-Validation Checks (Before Processing)](#pre-validation-checks-before-processing)
    - [Check 1: Required Fields](#check-1-required-fields)
    - [Check 2: Field Value Validation](#check-2-field-value-validation)
    - [Check 3: Timestamp Age Validation](#check-3-timestamp-age-validation)
  - [Normalization Contract](#normalization-contract)
    - [Normalization Rules](#normalization-rules)
      - [1. Source Normalization](#1-source-normalization)
      - [2. Timestamp Normalization](#2-timestamp-normalization)
      - [3. Severity Normalization](#3-severity-normalization)
      - [4. Value Normalization](#4-value-normalization)
    - [Normalized Signal Output](#normalized-signal-output)
  - [Signal Storm Detection](#signal-storm-detection)
    - [Throttling Logic](#throttling-logic)
    - [Queue Management During Storm](#queue-management-during-storm)
    - [Storm Recovery](#storm-recovery)
  - [Examples: GCP Signal Schemas](#examples-gcp-signal-schemas)
    - [Example 1: GCP Cloud Monitoring Signal](#example-1-gcp-cloud-monitoring-signal)
    - [Example 2: GCP Cloud Logging Signal](#example-2-gcp-cloud-logging-signal)
    - [Example 3: GCP Billing Signal](#example-3-gcp-billing-signal)
  - [Receipt Contract for Signal Ingestion](#receipt-contract-for-signal-ingestion)
    - [Receipt 1: signal_received](#receipt-1-signal_received)
    - [Receipt 2: signal_rejected](#receipt-2-signal_rejected)
    - [Receipt 3: signature_invalid](#receipt-3-signature_invalid)
    - [Receipt 4: signal_storm_detected](#receipt-4-signal_storm_detected)
    - [Receipt 5: signal_postponed](#receipt-5-signal_postponed)
  - [Definition of Done](#definition-of-done)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Signal Contracts: Ingestion & Validation

**Version**: 1.0.0 (Production-Ready)
**Last Updated**: 2026-01-25
**Owner**: Autonomics Constitution Lead

---

## Executive Summary

This document specifies how signals flow from monitoring systems into the Governor FSM. Signals are **normalized, validated, and receipted** — no silent failures. Every signal either succeeds with a receipt or fails with explicit refusal.

---

## HTTP Contract: Signal Ingestion

### Endpoint

```
POST /signal/{sku_id}/{tenant_id}
```

### Path Parameters

| Parameter | Type | Required | Example | Notes |
|-----------|------|----------|---------|-------|
| `sku_id` | string | Yes | `acme-catalog-v1` | SKU identifier from marketplace |
| `tenant_id` | string | Yes | `customer-123` | Customer tenant ID |

### Headers

```
Content-Type: application/json
Authorization: Bearer <service_token>
X-Webhook-Signature: sha256=<hmac_signature>
X-Webhook-ID: <uuid>
X-Webhook-Timestamp: <ISO8601>
```

| Header | Required | Purpose | Validation |
|--------|----------|---------|-----------|
| `Content-Type` | Yes | Request format | Must be `application/json` |
| `Authorization` | Yes | Service authentication | Bearer token verified via OIDC metadata server |
| `X-Webhook-Signature` | Yes | Signal authenticity | HMAC-SHA256 verified against body + timestamp |
| `X-Webhook-ID` | Yes | Webhook instance ID | Tracked for traceability |
| `X-Webhook-Timestamp` | Yes | Webhook emission time | Validated (must be recent, <1 hour) |

### Request Body

```json
{
  "source": "monitoring",
  "type": "cpu_utilization",
  "timestamp": "2026-01-25T14:32:15.123Z",
  "severity": "MEDIUM",
  "value": 82.5,
  "threshold": 75.0,
  "metadata": {
    "service_name": "production-catalog-service",
    "region": "us-central1",
    "zone": "us-central1-a",
    "instance_id": "instance-12345",
    "additional_context": "optional"
  },
  "correlation_id": "trace-uuid-12345"
}
```

### Required Fields

| Field | Type | Values | Example | Notes |
|-------|------|--------|---------|-------|
| `source` | string | enum | `"monitoring"`, `"logging"`, `"billing"`, `"custom"` | Identifies monitoring system |
| `type` | string | enum | `"cpu_utilization"`, `"memory_usage"`, `"error_rate"`, ... | Signal classification |
| `timestamp` | string | RFC3339 | `"2026-01-25T14:32:15.123Z"` | UTC timestamp (must be recent) |
| `severity` | string | enum | `"CRITICAL"`, `"HIGH"`, `"MEDIUM"`, `"LOW"` | Escalation level |

### Optional Fields

| Field | Type | Example | Notes |
|-------|------|---------|-------|
| `value` | number | `82.5` | Signal magnitude (if applicable) |
| `threshold` | number | `75.0` | Policy threshold for this signal |
| `metadata` | object | `{ "service_name": "..." }` | Free-form context (max 10KB) |
| `correlation_id` | string | `"trace-uuid-12345"` | Tracing ID (for distributed correlation) |

### Response: 200 OK (Signal Accepted)

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "signal_received",
  "context": {
    "signal_type": "cpu_utilization",
    "source": "monitoring",
    "severity": "MEDIUM",
    "value": 82.5,
    "threshold": 75.0,
    "exceeds_threshold": true,
    "normalized": true,
    "queue_length": 5
  }
}
```

### Response: 400 Bad Request (Signal Rejected - Validation)

**Reason**: Missing required field, invalid format, or validation failure.

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "missing_source_field",
  "context": {
    "error": "Required field 'source' missing",
    "http_code": 400,
    "validation_errors": [
      { "field": "source", "error": "missing" },
      { "field": "severity", "error": "unknown_value: CRITICAL2" }
    ]
  }
}
```

### Response: 403 Forbidden (Signal Rejected - Security)

**Reason**: Signature invalid or authorization failed.

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signature_invalid",
  "context": {
    "error": "HMAC signature verification failed",
    "http_code": 403,
    "expected_signature": "sha256=...",
    "provided_signature": "sha256=..."
  }
}
```

### Response: 429 Too Many Requests (Signal Throttled - Storm)

**Reason**: Signal rate exceeds 100/minute (throttling in effect).

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signal_storm_throttle",
  "context": {
    "error": "Signal rate exceeded (147/minute > 100/minute limit)",
    "http_code": 429,
    "current_rate": 147,
    "limit": 100,
    "retry_after_seconds": 30,
    "queue_length": 1000
  }
}
```

---

## Validation Contract

### Pre-Validation Checks (Before Processing)

```erlang
validate_signal_request(Req) ->
    %% Step 1: Verify headers
    case verify_headers(Req) of
        {error, HeaderError} ->
            {refuse, 403, header_validation_failed, HeaderError};
        {ok, _} ->
            %% Step 2: Verify signature
            case verify_signature(Req) of
                {error, SigError} ->
                    {refuse, 403, signature_invalid, SigError};
                {ok, _} ->
                    %% Step 3: Parse body
                    case parse_body(Req) of
                        {error, ParseError} ->
                            {refuse, 400, body_parse_error, ParseError};
                        {ok, Body} ->
                            %% Step 4: Check required fields
                            case check_required_fields(Body) of
                                {error, FieldErrors} ->
                                    {refuse, 400, missing_required_fields, FieldErrors};
                                {ok, _} ->
                                    %% Step 5: Normalize + validate
                                    validate_signal(Body)
                            end
                    end
            end
    end.
```

### Check 1: Required Fields

| Field | Missing → | Error Code |
|-------|-----------|-----------|
| `source` | Refuse 400 | `missing_source_field` |
| `type` | Refuse 400 | `missing_type_field` |
| `timestamp` | Refuse 400 | `missing_timestamp_field` |
| `severity` | Refuse 400 | `missing_severity_field` |

### Check 2: Field Value Validation

```erlang
validate_field(source, Value) ->
    case Value of
        <<"monitoring">> -> {ok, <<"monitoring">>};
        <<"logging">> -> {ok, <<"logging">>};
        <<"billing">> -> {ok, <<"billing">>};
        <<"custom">> -> {ok, <<"custom">>};
        _ -> {error, {unknown_source, Value}}
    end;

validate_field(type, Value) when is_binary(Value) ->
    case Value of
        <<"cpu_utilization">> -> {ok, <<"cpu_utilization">>};
        <<"memory_usage">> -> {ok, <<"memory_usage">>};
        <<"error_rate">> -> {ok, <<"error_rate">>};
        <<"disk_usage">> -> {ok, <<"disk_usage">>};
        <<"billing_spend">> -> {ok, <<"billing_spend">>};
        _ -> {error, {unknown_signal_type, Value}}
    end;

validate_field(severity, Value) ->
    case Value of
        <<"CRITICAL">> -> {ok, 'CRITICAL'};
        <<"HIGH">> -> {ok, 'HIGH'};
        <<"MEDIUM">> -> {ok, 'MEDIUM'};
        <<"LOW">> -> {ok, 'LOW'};
        _ -> {error, {unknown_severity, Value}}
    end;

validate_field(timestamp, Value) when is_binary(Value) ->
    case rfc3339:parse(Value) of
        {ok, Ts} ->
            case is_timestamp_recent(Ts) of
                true -> {ok, Ts};
                false -> {error, timestamp_too_old}  % >1 hour
            end;
        {error, _} ->
            {error, invalid_timestamp_format}
    end.
```

### Check 3: Timestamp Age Validation

```erlang
is_timestamp_recent(Ts) ->
    Now = erlang:monotonic_time(microsecond),
    Diff = Now - Ts,
    MaxAge = 3600_000_000,  % 1 hour in microseconds
    Diff < MaxAge.
```

| Condition | Action |
|-----------|--------|
| Timestamp missing | Refuse 400 |
| Timestamp invalid format | Refuse 400 |
| Timestamp >1 hour old | Refuse 400 |
| Timestamp future (>1min) | Refuse 400 |

---

## Normalization Contract

### Normalization Rules

Every accepted signal is normalized to canonical form before Governor processing.

#### 1. Source Normalization

```erlang
normalize_source(Source) ->
    case Source of
        <<"gcp-monitoring">> -> <<"monitoring">>;
        <<"gcp-cloud-monitoring">> -> <<"monitoring">>;
        <<"stackdriver">> -> <<"monitoring">>;
        <<"cloudwatch">> -> <<"monitoring">>;
        <<"prometheus">> -> <<"monitoring">>;
        <<"datadog">> -> <<"monitoring">>;
        <<"gcp-logging">> -> <<"logging">>;
        <<"cloudwatch-logs">> -> <<"logging">>;
        <<"stackdriver-logging">> -> <<"logging">>;
        <<"gcp-billing">> -> <<"billing">>;
        <<"aws-billing">> -> <<"billing">>;
        <<"custom">> -> <<"custom">>;
        Other -> Other
    end.
```

#### 2. Timestamp Normalization

```erlang
normalize_timestamp(Ts) ->
    %% Convert to UTC, microsecond precision
    case rfc3339:parse(Ts) of
        {ok, MicrosUTC} ->
            MicrosUTC;  % Already UTC, microsecond precision
        {error, _} ->
            error(invalid_timestamp)
    end.
```

#### 3. Severity Normalization

```erlang
normalize_severity(Sev) ->
    case string:uppercase(Sev) of
        <<"CRITICAL">> -> 'CRITICAL';
        <<"CRITICAL_PLUS">> -> 'CRITICAL';  % Map to CRITICAL
        <<"SEVERITY_CRITICAL">> -> 'CRITICAL';
        <<"HIGH">> -> 'HIGH';
        <<"MEDIUM">> -> 'MEDIUM';
        <<"LOW">> -> 'LOW';
        <<"INFO">> -> 'LOW';
        Other -> error({unknown_severity, Other})
    end.
```

#### 4. Value Normalization

```erlang
normalize_value(Value) when is_number(Value) ->
    Value;
normalize_value(Value) when is_binary(Value) ->
    case binary_to_float(Value) of
        {Float, _} -> Float;
        _ -> error({invalid_numeric_value, Value})
    end;
normalize_value(Value) ->
    error({invalid_value_type, Value}).
```

### Normalized Signal Output

```json
{
  "source": "monitoring",
  "type": "cpu_utilization",
  "timestamp": 1674667935123000,
  "severity": "MEDIUM",
  "value": 82.5,
  "threshold": 75.0,
  "metadata": {
    "service_name": "production-catalog-service",
    "region": "us-central1",
    "zone": "us-central1-a"
  },
  "correlation_id": "trace-uuid-12345",
  "normalized": true,
  "normalized_at": 1674667935234000
}
```

---

## Signal Storm Detection

### Throttling Logic

```erlang
handle_signal_storm(TenantId) ->
    %% Count signals from TenantId in last 60 seconds
    Rate = count_signals_per_minute(TenantId),
    case Rate > 100 of
        true ->
            %% Storm detected: throttle
            {refuse, 429, signal_storm_throttle, #{
                current_rate => Rate,
                limit => 100,
                retry_after_seconds => 30
            }};
        false ->
            {accept, signal}
    end.

count_signals_per_minute(TenantId) ->
    Now = erlang:monotonic_time(microsecond),
    OneMinuteAgo = Now - 60_000_000,  % 60 seconds in microseconds
    %% Count signals from TenantId received after OneMinuteAgo
    firestore:count_signals(TenantId, OneMinuteAgo, Now).
```

### Queue Management During Storm

```erlang
buffer_signal_during_storm(Signal, TenantId, MaxBuffer) ->
    case queue:len(get_signal_buffer(TenantId)) < MaxBuffer of
        true ->
            %% Buffer has space: enqueue
            SignalBuffer = get_signal_buffer(TenantId),
            NewBuffer = queue:in(Signal, SignalBuffer),
            store_signal_buffer(TenantId, NewBuffer),
            {ok, queued};
        false ->
            %% Buffer full: drop oldest
            {SignalBuffer, DroppedSignal} = queue:out(get_signal_buffer(TenantId)),
            NewBuffer = queue:in(Signal, SignalBuffer),
            store_signal_buffer(TenantId, NewBuffer),
            emit_receipt(signal_dropped, #{
                dropped_signal_id => DroppedSignal#signal.id,
                dropped_type => DroppedSignal#signal.type
            }),
            {ok, queued_with_drop}
    end.
```

### Storm Recovery

```erlang
process_buffered_signals(TenantId) ->
    Rate = count_signals_per_minute(TenantId),
    case Rate =< 100 of
        true ->
            %% Storm ended: drain buffer
            SignalBuffer = get_signal_buffer(TenantId),
            drain_buffer(SignalBuffer, TenantId);
        false ->
            %% Still in storm: retry later
            {postpone, buffer_drain_task}
    end.

drain_buffer(SignalBuffer, TenantId) ->
    case queue:out(SignalBuffer) of
        {empty, _} ->
            %% Buffer empty: cleanup
            store_signal_buffer(TenantId, queue:new());
        {{value, Signal}, NewBuffer} ->
            %% Process signal
            governor:signal(TenantId, Signal),
            store_signal_buffer(TenantId, NewBuffer),
            drain_buffer(NewBuffer, TenantId)
    end.
```

---

## Examples: GCP Signal Schemas

### Example 1: GCP Cloud Monitoring Signal

**Source**: Google Cloud Monitoring (Stackdriver)

**Raw Signal** (from GCP):
```json
{
  "incident": {
    "incident_id": "0.lvjkm9db3rw5",
    "resource_id": "projects/acme-project/service:catalog-api",
    "resource_display_name": "catalog-api",
    "state": "OPEN",
    "started_at": "2026-01-25T14:30:00Z",
    "metric_display_name": "CPU utilization",
    "metric_kind": "GAUGE",
    "metric_type": "compute.googleapis.com/instance/cpu/utilization",
    "condition_name": "CPU > 75%",
    "condition_threshold_value": 0.75,
    "observed_value": 0.825,
    "url": "https://console.cloud.google.com/monitoring/alertpolicies/..."
  }
}
```

**Normalized to Governor Signal**:
```json
{
  "source": "monitoring",
  "type": "cpu_utilization",
  "timestamp": "2026-01-25T14:30:15.000Z",
  "severity": "HIGH",
  "value": 82.5,
  "threshold": 75.0,
  "metadata": {
    "service_name": "catalog-api",
    "metric_type": "compute.googleapis.com/instance/cpu/utilization",
    "gcp_incident_id": "0.lvjkm9db3rw5",
    "alert_policy_url": "https://console.cloud.google.com/monitoring/alertpolicies/..."
  },
  "correlation_id": "0.lvjkm9db3rw5"
}
```

### Example 2: GCP Cloud Logging Signal

**Source**: Google Cloud Logging (structured logs)

**Raw Log Entry** (from GCP Logging):
```json
{
  "timestamp": "2026-01-25T14:32:15.234Z",
  "severity": "ERROR",
  "logName": "projects/acme-project/logs/catalog-api",
  "jsonPayload": {
    "error_type": "OutOfMemory",
    "error_message": "Heap memory exceeded 95%",
    "heap_usage_percent": 95.3,
    "pod_name": "catalog-api-deployment-abcdef-12345",
    "namespace": "production"
  },
  "trace": "projects/acme-project/traces/12345abcdef"
}
```

**Normalized to Governor Signal**:
```json
{
  "source": "logging",
  "type": "memory_usage",
  "timestamp": "2026-01-25T14:32:15.234Z",
  "severity": "CRITICAL",
  "value": 95.3,
  "threshold": 90.0,
  "metadata": {
    "service_name": "catalog-api",
    "pod_name": "catalog-api-deployment-abcdef-12345",
    "namespace": "production",
    "error_type": "OutOfMemory",
    "gcp_trace_id": "projects/acme-project/traces/12345abcdef"
  },
  "correlation_id": "projects/acme-project/traces/12345abcdef"
}
```

### Example 3: GCP Billing Signal

**Source**: Google Cloud Billing API

**Raw Billing Event** (from Cloud Billing):
```json
{
  "timestamp": "2026-01-25T14:35:00Z",
  "event_type": "billing_threshold_exceeded",
  "billing_account_id": "0123AB-CDEF45-67890G",
  "project_id": "acme-project",
  "cumulative_cost": 45328.65,
  "cost_threshold": 45000.00,
  "currency": "USD"
}
```

**Normalized to Governor Signal**:
```json
{
  "source": "billing",
  "type": "billing_spend",
  "timestamp": "2026-01-25T14:35:00.000Z",
  "severity": "HIGH",
  "value": 45328.65,
  "threshold": 45000.00,
  "metadata": {
    "billing_account_id": "0123AB-CDEF45-67890G",
    "project_id": "acme-project",
    "currency": "USD",
    "threshold_type": "cumulative_cost"
  },
  "correlation_id": "0123AB-CDEF45-67890G"
}
```

---

## Receipt Contract for Signal Ingestion

### Receipt 1: signal_received

**Emitted when**: Signal accepted and normalized.

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "signal_received",
  "context": {
    "signal_type": "cpu_utilization",
    "source": "monitoring",
    "severity": "MEDIUM",
    "value": 82.5,
    "threshold": 75.0,
    "exceeds_threshold": true,
    "queue_length": 5
  }
}
```

### Receipt 2: signal_rejected

**Emitted when**: Signal validation failed.

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signal_rejected",
  "context": {
    "error": "Required field 'source' missing",
    "validation_errors": [
      { "field": "source", "error": "missing" }
    ]
  }
}
```

### Receipt 3: signature_invalid

**Emitted when**: HMAC signature verification failed.

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signature_invalid",
  "context": {
    "error": "HMAC signature verification failed",
    "http_code": 403
  }
}
```

### Receipt 4: signal_storm_detected

**Emitted when**: Signal rate exceeds 100/minute (throttle engaged).

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "refuse",
  "reason": "signal_storm_detected",
  "context": {
    "current_rate": 147,
    "limit": 100,
    "retry_after_seconds": 30,
    "queue_length": 1000,
    "action": "throttle"
  }
}
```

### Receipt 5: signal_postponed

**Emitted when**: Signal accepted but postponed (queued due to concurrency).

```json
{
  "receipt_id": "uuid-v4",
  "timestamp": "ISO 8601",
  "sku_id": "acme-catalog-v1",
  "tenant_id": "customer-123",
  "status": "accept",
  "reason": "signal_postponed",
  "context": {
    "reason": "action_in_flight",
    "signal_type": "cpu_utilization",
    "queue_length": 8
  }
}
```

---

## Definition of Done

- [ ] HTTP endpoint `/signal/{sku_id}/{tenant_id}` documented (method, headers, path params)
- [ ] Request body schema documented (required + optional fields)
- [ ] Response schemas documented (200, 400, 403, 429 cases)
- [ ] Validation contract defined (5 checks: headers, signature, parse, required fields, values)
- [ ] Normalization contract defined (source, timestamp, severity, value)
- [ ] Field validation rules documented per field (enum values, constraints)
- [ ] Timestamp validation (age check, format, future check)
- [ ] Signal storm detection + throttling logic documented
- [ ] Queue management during storm (max 1000, FIFO drop)
- [ ] 3 GCP examples documented (Monitoring, Logging, Billing)
- [ ] 5 receipt types documented (received, rejected, signature_invalid, storm, postponed)
- [ ] Cross-reference: governor-contract.md for receipt structure
- [ ] Cross-reference: refusal-modes.md for refusal scenarios

---

## References

- **governor-contract.md** — Receipt schema, FSM states
- **refusal-modes.md** — Refusal modes 1-6 (signal validation)
- **action-contracts.md** — Action execution (what happens after signal accepted)
- **invariants.md** — Invariant 6 (signal storm prevention)
- **RFC 3339** — Timestamp format (https://tools.ietf.org/html/rfc3339)
- **HMAC-SHA256** — Signature verification (https://tools.ietf.org/html/rfc4868)

---

**Last Updated**: 2026-01-25 | **Status**: Production-Ready
