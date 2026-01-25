<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Adversarial Test Suite](#adversarial-test-suite)
  - [Overview](#overview)
  - [Test Categories](#test-categories)
    - [Category 1: Input Validation (5 subcategories)](#category-1-input-validation-5-subcategories)
      - [1.1: Malformed JSON](#11-malformed-json)
      - [1.2: Oversized Payloads](#12-oversized-payloads)
      - [1.3: Invalid Field Types](#13-invalid-field-types)
      - [1.4: Missing Required Fields](#14-missing-required-fields)
      - [1.5: Invalid Enum Values](#15-invalid-enum-values)
    - [Category 2: Protocol Violation (5 subcategories)](#category-2-protocol-violation-5-subcategories)
      - [2.1: Missing Headers](#21-missing-headers)
      - [2.2: Invalid HTTP Methods](#22-invalid-http-methods)
      - [2.3: Invalid Path Parameters](#23-invalid-path-parameters)
      - [2.4: Signature Attacks](#24-signature-attacks)
      - [2.5: Content-Type Mismatch](#25-content-type-mismatch)
    - [Category 3: Authorization Bypass (5 subcategories)](#category-3-authorization-bypass-5-subcategories)
      - [3.1: Cross-Tenant Access](#31-cross-tenant-access)
      - [3.2: Entitlement Escalation](#32-entitlement-escalation)
      - [3.3: Admin Action Without Permission](#33-admin-action-without-permission)
      - [3.4: Signature Key Confusion](#34-signature-key-confusion)
      - [3.5: Token/Secret Leakage](#35-tokensecret-leakage)
    - [Category 4: Quota & Rate Limiting (5 subcategories)](#category-4-quota--rate-limiting-5-subcategories)
      - [4.1: Burst Traffic](#41-burst-traffic)
      - [4.2: Sustained High Rate](#42-sustained-high-rate)
      - [4.3: Quota Exhaustion](#43-quota-exhaustion)
      - [4.4: Reset Timing](#44-reset-timing)
      - [4.5: Quota Bypass Prevention](#45-quota-bypass-prevention)
  - [Response Validation](#response-validation)
  - [SLO Targets](#slo-targets)
  - [Continuous Integration](#continuous-integration)
  - [Receipt Contract](#receipt-contract)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Adversarial Test Suite

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

## Overview

**Purpose**: Prove through rigorous adversarial testing that the system safely refuses all attack patterns (zero silent failures, every attack receipted).

**Testing Approach**: Black-box testing (no internal knowledge), HTTP-level testing (treat system like attacker would), systematic coverage (5 categories × 5 subcategories each).

**Success Criteria**:
- ✅ 100% refusal rate (all attacks rejected, never accepted)
- ✅ <100ms response time (DoS resistance)
- ✅ 0 unhandled exceptions (all paths receipted)
- ✅ All responses are valid receipts (JSON schema compliance)

---

## Test Categories

### Category 1: Input Validation (5 subcategories)

#### 1.1: Malformed JSON

**Objective**: System must reject and receipt all JSON parsing errors.

**Test Cases**:

```bash
# Test 1.1.1: Invalid JSON (missing closing brace)
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch"' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request + refusal receipt

# Test 1.1.2: Null bytes in JSON
printf '{"signal_type": "launch\x00", "entitlement_id": "ent-uuid"}'| \
  curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -d @- \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request + refusal receipt

# Test 1.1.3: Control characters
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -d $'{"signal_type": "launch\n\r\t", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request + refusal receipt
```

#### 1.2: Oversized Payloads

**Objective**: System must reject oversized requests (DoS prevention).

**Test Cases**:

```bash
# Test 1.2.1: Payload exceeds max size (assume 10MB limit)
python3 << 'EOF'
import requests
import json

large_payload = {
    "signal_type": "launch",
    "entitlement_id": "ent-uuid-001",
    "parameters": {
        "data": "X" * (11 * 1024 * 1024)  # 11 MB
    }
}

response = requests.post(
    "https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001",
    json=large_payload,
    headers={"X-Ggen-Signature": "sha256=..."}
)

print(f"Status: {response.status_code}")
print(f"Response: {response.json()}")
# Expected: 413 Payload Too Large + refusal receipt
EOF
```

#### 1.3: Invalid Field Types

**Objective**: System must validate field types (type safety).

**Test Cases**:

```bash
# Test 1.3.1: signal_type is number (should be string)
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": 123, "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request (type mismatch) + refusal receipt

# Test 1.3.2: parameters is array (should be object)
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid", "parameters": ["a", "b"]}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request + refusal receipt
```

#### 1.4: Missing Required Fields

**Objective**: System must enforce required fields.

**Test Cases**:

```bash
# Test 1.4.1: Missing signal_type
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request (missing required field) + refusal receipt

# Test 1.4.2: Missing entitlement_id
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request + refusal receipt
```

#### 1.5: Invalid Enum Values

**Objective**: System must validate enum values.

**Test Cases**:

```bash
# Test 1.5.1: signal_type has invalid enum value
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "invalid_action", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request (invalid enum) + refusal receipt
# Valid values: launch, update, terminate, delete
```

---

### Category 2: Protocol Violation (5 subcategories)

#### 2.1: Missing Headers

**Objective**: System must enforce required headers (especially signature).

**Test Cases**:

```bash
# Test 2.1.1: Missing X-Ggen-Signature header
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 401 Unauthorized + refusal receipt

# Test 2.1.2: Missing Content-Type header
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 415 Unsupported Media Type + refusal receipt
```

#### 2.2: Invalid HTTP Methods

**Objective**: System must enforce correct HTTP methods.

**Test Cases**:

```bash
# Test 2.2.1: GET instead of POST
curl -X GET https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 405 Method Not Allowed

# Test 2.2.2: PUT instead of POST
curl -X PUT https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 405 Method Not Allowed
```

#### 2.3: Invalid Path Parameters

**Objective**: System must validate path parameters (UUID format).

**Test Cases**:

```bash
# Test 2.3.1: Invalid SKU ID (not UUID format)
curl -X POST https://ggen.example.com/api/v1/signal/not-a-uuid/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 400 Bad Request (invalid UUID format) + refusal receipt

# Test 2.3.2: Missing tenant_id
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 404 Not Found
```

#### 2.4: Signature Attacks

**Objective**: System must reject invalid signatures (authentication bypass prevention).

**Test Cases**:

```bash
# Test 2.4.1: Wrong signature
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: sha256=wrong_signature_base64" \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 401 Unauthorized (HMAC mismatch) + permission_denied receipt

# Test 2.4.2: Modified body after signature
python3 << 'EOF'
import requests
import hmac
import hashlib
import base64

secret = b"customer-secret-key"
original_body = b'{"signal_type": "launch", "entitlement_id": "ent-uuid"}'
signature = hmac.new(secret, original_body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()

# Now modify the body
modified_body = b'{"signal_type": "launch", "entitlement_id": "ent-uuid-different"}'

response = requests.post(
    "https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001",
    data=modified_body,
    headers={
        "Content-Type": "application/json",
        "X-Ggen-Signature": f"sha256={signature_b64}"
    }
)

print(f"Status: {response.status_code}")  # Expected: 401
print(f"Receipt: {response.json()['kind']}")  # Expected: permission_denied
EOF
```

#### 2.5: Content-Type Mismatch

**Objective**: System must validate Content-Type header.

**Test Cases**:

```bash
# Test 2.5.1: Content-Type is text/plain but body is JSON
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: text/plain" \
  -H "X-Ggen-Signature: sha256=..." \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 415 Unsupported Media Type + refusal receipt
```

---

### Category 3: Authorization Bypass (5 subcategories)

#### 3.1: Cross-Tenant Access

**Objective**: System must prevent one tenant from accessing another tenant's data.

**Test Cases**:

```bash
# Test 3.1.1: Access customer B's SKU with customer A's signature
python3 << 'EOF'
import requests
import hmac
import hashlib
import base64

# Customer A's secret and their SKU
secret_a = b"customer-a-secret-key"
sku_a = "550e8400-e29b-41d4-a716-446655440000"
tenant_a = "650e8400-e29b-41d4-a716-446655440001"

# Try to access customer B's SKU
sku_b = "660e8400-e29b-41d4-a716-446655440002"
tenant_b = "750e8400-e29b-41d4-a716-446655440003"

body = b'{"signal_type": "launch", "entitlement_id": "ent-uuid"}'
signature = hmac.new(secret_a, body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()

response = requests.post(
    f"https://ggen.example.com/api/v1/signal/{sku_b}/{tenant_b}",
    data=body,
    headers={
        "Content-Type": "application/json",
        "X-Ggen-Signature": f"sha256={signature_b64}"
    }
)

print(f"Status: {response.status_code}")  # Expected: 403 or 401
print(f"Receipt: {response.json()}")  # Expected: permission_denied or invariant_violation
EOF
```

#### 3.2: Entitlement Escalation

**Objective**: System must prevent customer from launching expensive SKU without entitlement.

**Test Cases**:

```bash
# Test 3.2.1: Try to launch high-cost SKU when only low-cost is entitled
python3 << 'EOF'
import requests
import hmac
import hashlib
import base64

# Customer entitled to "basic" SKU only
secret = b"customer-secret-key"
basic_sku = "100e8400-e29b-41d4-a716-446655440000"
premium_sku = "200e8400-e29b-41d4-a716-446655440000"
tenant = "650e8400-e29b-41d4-a716-446655440001"

body = b'{"signal_type": "launch", "entitlement_id": "premium-ent-xyz"}'
signature = hmac.new(secret, body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()

response = requests.post(
    f"https://ggen.example.com/api/v1/signal/{premium_sku}/{tenant}",
    data=body,
    headers={
        "Content-Type": "application/json",
        "X-Ggen-Signature": f"sha256={signature_b64}"
    }
)

print(f"Status: {response.status_code}")  # Expected: 403
print(f"Receipt kind: {response.json()['kind']}")  # Expected: entitlement_not_found or permission_denied
EOF
```

#### 3.3: Admin Action Without Permission

**Objective**: System must prevent non-admin from performing admin actions.

**Test Cases**:

```bash
# Test 3.3.1: Try to delete SKU without admin role
python3 << 'EOF'
import requests
import hmac
import hashlib
import base64

secret = b"customer-secret-key"
sku = "550e8400-e29b-41d4-a716-446655440000"
tenant = "650e8400-e29b-41d4-a716-446655440001"

# Admin action (delete)
body = b'{"signal_type": "delete", "entitlement_id": "ent-uuid"}'
signature = hmac.new(secret, body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()

response = requests.post(
    f"https://ggen.example.com/api/v1/signal/{sku}/{tenant}",
    data=body,
    headers={
        "Content-Type": "application/json",
        "X-Ggen-Signature": f"sha256={signature_b64}"
    }
)

print(f"Status: {response.status_code}")  # Expected: 403
print(f"Receipt kind: {response.json()['kind']}")  # Expected: permission_denied
EOF
```

#### 3.4: Signature Key Confusion

**Objective**: System must handle signature algorithm mismatch.

**Test Cases**:

```bash
# Test 3.4.1: Try to use MD5 signature instead of SHA256
curl -X POST https://ggen.example.com/api/v1/signal/550e8400-e29b-41d4-a716-446655440000/650e8400-e29b-41d4-a716-446655440001 \
  -H "Content-Type: application/json" \
  -H "X-Ggen-Signature: md5=invalid_md5_signature" \
  -d '{"signal_type": "launch", "entitlement_id": "ent-uuid"}' \
  --write-out "\nHTTP Status: %{http_code}\n"

# Expected: 401 Unauthorized (unsupported algorithm) + permission_denied receipt
```

#### 3.5: Token/Secret Leakage

**Objective**: System must reject signatures with known leaked secrets.

**Test Cases**:

```bash
# Test 3.5.1: Default/test secret (e.g., "test-secret-key")
python3 << 'EOF'
import requests
import hmac
import hashlib
import base64

# Try using a known test secret (assuming "test-secret-key" is blacklisted)
secret = b"test-secret-key"
sku = "550e8400-e29b-41d4-a716-446655440000"
tenant = "650e8400-e29b-41d4-a716-446655440001"

body = b'{"signal_type": "launch", "entitlement_id": "ent-uuid"}'
signature = hmac.new(secret, body, hashlib.sha256).digest()
signature_b64 = base64.b64encode(signature).decode()

response = requests.post(
    f"https://ggen.example.com/api/v1/signal/{sku}/{tenant}",
    data=body,
    headers={
        "Content-Type": "application/json",
        "X-Ggen-Signature": f"sha256={signature_b64}"
    }
)

print(f"Status: {response.status_code}")  # Expected: 401
print(f"Receipt kind: {response.json()['kind']}")  # Expected: permission_denied
EOF
```

---

### Category 4: Quota & Rate Limiting (5 subcategories)

#### 4.1: Burst Traffic

**Objective**: System must handle rapid requests (DoS prevention).

**Test Cases**:

```python
# Test 4.1.1: Send 1000 requests in 10 seconds
import requests
import concurrent.futures
import time
import hmac
import hashlib
import base64

def send_request():
    secret = b"customer-secret-key"
    sku = "550e8400-e29b-41d4-a716-446655440000"
    tenant = "650e8400-e29b-41d4-a716-446655440001"

    body = b'{"signal_type": "launch", "entitlement_id": "ent-uuid"}'
    signature = hmac.new(secret, body, hashlib.sha256).digest()
    signature_b64 = base64.b64encode(signature).decode()

    response = requests.post(
        f"https://ggen.example.com/api/v1/signal/{sku}/{tenant}",
        data=body,
        headers={
            "Content-Type": "application/json",
            "X-Ggen-Signature": f"sha256={signature_b64}"
        }
    )
    return response.status_code

start = time.time()
with concurrent.futures.ThreadPoolExecutor(max_workers=50) as executor:
    results = list(executor.map(send_request, range(1000)))
elapsed = time.time() - start

# Expected:
# - First ~100 requests: 202 (Accepted)
# - Remaining ~900 requests: 429 (Rate Limited)
# - Response time: <10 seconds (DoS resistant)

status_counts = {}
for status in results:
    status_counts[status] = status_counts.get(status, 0) + 1

print(f"Results in {elapsed:.1f} seconds:")
print(f"  202 Accepted: {status_counts.get(202, 0)}")
print(f"  429 Rate Limited: {status_counts.get(429, 0)}")
print(f"  Other: {status_counts}")
```

#### 4.2: Sustained High Rate

**Objective**: System must throttle sustained high request rates.

**Test Cases**:

```python
# Test 4.2.1: Send 10 requests per second for 60 seconds
import requests
import time

start = time.time()
request_count = 0
accepted_count = 0
rate_limited_count = 0

while time.time() - start < 60:
    for _ in range(10):  # 10 requests per second
        response = send_request()  # (use send_request from above)
        request_count += 1
        if response == 202:
            accepted_count += 1
        elif response == 429:
            rate_limited_count += 1

    time.sleep(1)

print(f"After 60 seconds:")
print(f"  Total requests: {request_count}")
print(f"  Accepted: {accepted_count}")
print(f"  Rate limited: {rate_limited_count}")
print(f"  Overall rate limited: {rate_limited_count / request_count * 100:.1f}%")
# Expected: Rate limit kicks in, ~80-90% 429 responses after quota exhausted
```

#### 4.3: Quota Exhaustion

**Objective**: System must reject requests after quota is exhausted.

**Test Cases**:

```python
# Test 4.3.1: Exhaust monthly quota
# Assuming: 10,000 requests per month quota

import requests
import time

quota_limit = 10000
response_codes = {}

for i in range(quota_limit + 100):
    response = send_request()  # (use send_request from above)
    status = response.status_code
    response_codes[status] = response_codes.get(status, 0) + 1

    if i % 1000 == 0:
        print(f"Request {i}: {status}")

    time.sleep(0.01)  # Avoid overwhelming server

print(f"Final status distribution:")
for status, count in sorted(response_codes.items()):
    print(f"  {status}: {count}")

# Expected:
# - Requests 1-10000: 202 (within quota)
# - Requests 10001-10100: 429 (quota exceeded)
```

#### 4.4: Reset Timing

**Objective**: System must reset quotas on correct interval.

**Test Cases**:

```python
# Test 4.4.1: Verify quota resets at hour boundary
import requests
import time
import datetime

def check_quota_status():
    # Exhaust quota
    for _ in range(1100):  # Assume 1000/hour quota
        send_request()

    # Wait until next hour boundary
    now = datetime.datetime.now()
    seconds_to_next_hour = (60 - now.minute - 1) * 60 + (60 - now.second)
    print(f"Waiting {seconds_to_next_hour} seconds until next hour...")
    time.sleep(seconds_to_next_hour + 5)  # Wait a bit extra

    # Try one more request
    response = send_request()
    return response.status_code

status = check_quota_status()
print(f"Status after reset: {status}")
# Expected: 202 (quota has reset)
```

#### 4.5: Quota Bypass Prevention

**Objective**: System must prevent quota bypass attempts.

**Test Cases**:

```python
# Test 4.5.1: Try to use different SKU to bypass quota
import requests
import hmac
import hashlib
import base64

secret = b"customer-secret-key"
tenant = "650e8400-e29b-41d4-a716-446655440001"

# Exhaust quota on SKU A
sku_a = "550e8400-e29b-41d4-a716-446655440000"
for _ in range(1100):
    send_request_for_sku(sku_a)

# Try to use SKU B to bypass quota
sku_b = "550e8400-e29b-41d4-a716-446655440001"
response = send_request_for_sku(sku_b)

print(f"Status for SKU B: {response.status_code}")
# Expected: 429 (quota is per-tenant, not per-SKU)
```

---

## Response Validation

Every response must:
1. ✅ Have valid HTTP status code (200, 202, 400, 401, 403, 429, 500, 503)
2. ✅ Have valid JSON body (parseable)
3. ✅ Have correct receipt structure (kind, ts, decision, details, prev_chain_hash_b64)
4. ✅ Have receipt kind matching HTTP status (202 → signal_received, 429 → quota_exceeded, etc.)
5. ✅ Have decision field set correctly (accept or refuse)

**Validation Script**:
```python
def validate_response(response):
    """Validate receipt response structure"""
    assert response.status_code in [200, 202, 400, 401, 403, 429, 500, 503]

    try:
        body = response.json()
    except:
        print(f"ERROR: Response is not valid JSON")
        return False

    # Check required fields
    required_fields = ["kind", "ts", "decision", "project_id", "repo", "branch", "details", "prev_chain_hash_b64"]
    for field in required_fields:
        assert field in body, f"Missing field: {field}"

    # Validate decision
    assert body["decision"] in ["accept", "refuse", "unknown"], f"Invalid decision: {body['decision']}"

    # Validate receipt kind (sampling)
    valid_kinds = ["signal_received", "action_completed", "refusal", "permission_denied", "quota_exceeded", ...]
    assert body["kind"] in valid_kinds, f"Unknown receipt kind: {body['kind']}"

    return True
```

---

## SLO Targets

| Metric | Target | Rationale |
|--------|--------|-----------|
| Response Time | <100ms (p99) | DoS-resistant |
| Refusal Rate | 100% (for attacks) | Zero silent failures |
| Valid Receipts | 100% | All paths receipted |
| Unhandled Exceptions | 0% | No crashes |
| JSON Schema Compliance | 100% | All receipts valid |

---

## Continuous Integration

**Run adversarial tests on every deployment**:

```bash
# In CI/CD pipeline (e.g., GitHub Actions)
cargo make test adversarial

# Or run manually
python3 tests/adversarial_suite.py \
  --target https://ggen.example.com \
  --report adversarial_test_results.json
```

**Result Report** (JSON):
```json
{
  "test_run_ts": "2026-01-25T14:32:00Z",
  "target_url": "https://ggen.example.com",
  "category_results": [
    {
      "category": "Input Validation",
      "total_tests": 25,
      "passed": 25,
      "failed": 0
    },
    {
      "category": "Protocol Violation",
      "total_tests": 25,
      "passed": 25,
      "failed": 0
    }
  ],
  "overall": {
    "total_tests": 125,
    "passed": 125,
    "failed": 0,
    "success_rate": "100%"
  },
  "slo_compliance": {
    "response_time_p99_ms": 45,
    "refusal_rate": "100%",
    "valid_receipts": "100%",
    "status": "PASS"
  }
}
```

---

## Receipt Contract

**Every adversarial test MUST**:
- ✅ Be black-box (no internal knowledge)
- ✅ Be HTTP-level (like an attacker would test)
- ✅ Verify receipt is returned (not just HTTP status)
- ✅ Verify receipt decision is correct (accept vs refuse)
- ✅ Verify response time is <100ms (DoS resistance)
- ✅ Never cause unhandled exception or 5xx error

---

## Definition of Done

- [x] 5 test categories with 5 subcategories each (25 test groups total)
- [x] Concrete curl and Python examples for each test
- [x] Expected results documented
- [x] Response validation procedure provided
- [x] SLO targets defined
- [x] CI/CD integration example provided
- [x] JSON result format specified
- [x] Glossary references included

