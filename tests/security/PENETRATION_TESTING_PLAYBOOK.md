# Penetration Testing Playbook

## Overview

Comprehensive penetration testing guide for ggen v6 security validation. This playbook covers automated and manual testing scenarios for identifying security vulnerabilities before production deployment.

**Target System**: ggen v6 API (ggen-api crate)
**Framework**: OWASP Testing Guide v4.2
**Scope**: Weeks 11-12 security roadmap deliverables
**Date**: January 2026

---

## Table of Contents

1. [Pre-Testing Setup](#pre-testing-setup)
2. [Automated Attack Scenarios](#automated-attack-scenarios)
3. [Manual Testing Procedures](#manual-testing-procedures)
4. [Vulnerability Remediation](#vulnerability-remediation)
5. [Reporting and Documentation](#reporting-and-documentation)

---

## Pre-Testing Setup

### Environment Preparation

```bash
# 1. Start isolated test environment
docker-compose -f tests/security/docker-compose.yml up -d

# 2. Configure test API server
export GGEN_API_URL=http://localhost:8080
export GGEN_API_KEY=test_key_12345

# 3. Install security testing tools
cargo install cargo-fuzz
cargo install cargo-audit
pip install pytest pytest-asyncio requests

# 4. Verify baseline security posture
cargo make audit
cargo make test security::
```

### Test User Accounts

| Username | Role | Purpose |
|----------|------|---------|
| `admin@test.com` | Admin | Privilege escalation tests |
| `user@test.com` | User | Normal user operations |
| `attacker@test.com` | Unprivileged | Attack simulation |

### Network Configuration

- **Internal Network**: `172.16.0.0/24`
- **External Access**: Port 8080 (HTTPS)
- **Database**: PostgreSQL on `172.16.0.2:5432`
- **Redis**: Cache on `172.16.0.3:6379`

---

## Automated Attack Scenarios

### Scenario 1: Brute Force Authentication

**Objective**: Test rate limiting and account lockout mechanisms.

```python
# tests/security/attacks/brute_force.py

import asyncio
import aiohttp

async def brute_force_attack(target_email, password_list):
    """
    Attempt multiple login attempts to test rate limiting.

    Expected: Rate limiting triggers after 5-10 attempts.
    Expected: Account locks after 5 failed attempts.
    """
    async with aiohttp.ClientSession() as session:
        success_count = 0
        locked = False

        for i, password in enumerate(password_list):
            response = await session.post(
                f"{GGEN_API_URL}/api/auth/login",
                json={"email": target_email, "password": password}
            )

            if response.status == 200:
                success_count += 1
                print(f"[FAIL] Login succeeded with password: {password}")
            elif response.status == 429:
                print(f"[PASS] Rate limited after {i} attempts")
                break
            elif response.status == 423:
                print(f"[PASS] Account locked after {i} attempts")
                locked = True
                break

        assert success_count == 0, "Brute force succeeded"
        assert locked or i < 10, "No rate limiting detected"

# Run with: pytest tests/security/attacks/brute_force.py
```

### Scenario 2: SQL Injection

**Objective**: Verify parameterized queries prevent SQL injection.

```bash
# Manual curl tests
curl -X GET "$GGEN_API_URL/api/users?id=1' OR '1'='1"
# Expected: 400 Bad Request or sanitized response

curl -X GET "$GGEN_API_URL/api/users?id=1; DROP TABLE users;--"
# Expected: 400 Bad Request

curl -X GET "$GGEN_API_URL/api/search?q=' UNION SELECT password FROM users--"
# Expected: 400 Bad Request or empty results
```

**Automated Test**:

```python
# tests/security/attacks/sql_injection.py

SQL_INJECTION_PAYLOADS = [
    "1' OR '1'='1",
    "admin'--",
    "1'; DROP TABLE users;--",
    "' UNION SELECT password FROM users--",
    "1' AND 1=1--",
    "1' AND 1=2--",
]

async def test_sql_injection_protection():
    """Test all endpoints against SQL injection."""
    async with aiohttp.ClientSession() as session:
        for payload in SQL_INJECTION_PAYLOADS:
            # Test query parameters
            response = await session.get(
                f"{GGEN_API_URL}/api/users",
                params={"id": payload}
            )
            assert response.status in [400, 404], \
                f"SQL injection not blocked: {payload}"

            # Test POST body
            response = await session.post(
                f"{GGEN_API_URL}/api/users/search",
                json={"query": payload}
            )
            assert response.status in [400, 422], \
                f"SQL injection in POST not blocked: {payload}"
```

### Scenario 3: Cross-Site Scripting (XSS)

**Objective**: Verify input sanitization prevents XSS.

```python
# tests/security/attacks/xss.py

XSS_PAYLOADS = [
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<iframe src='javascript:alert(\"XSS\")'></iframe>",
    "<svg onload=alert('XSS')>",
]

async def test_xss_protection():
    """Test input sanitization against XSS."""
    async with aiohttp.ClientSession() as session:
        for payload in XSS_PAYLOADS:
            # Create user with XSS in name
            response = await session.post(
                f"{GGEN_API_URL}/api/users",
                json={"name": payload, "email": "xss@test.com"}
            )

            if response.status == 200:
                user = await response.json()
                # Verify payload was sanitized
                assert "<script>" not in user["name"]
                assert "javascript:" not in user["name"]
                print(f"[PASS] XSS payload sanitized: {payload}")
```

### Scenario 4: Directory Traversal

**Objective**: Prevent unauthorized file access.

```python
# tests/security/attacks/path_traversal.py

PATH_TRAVERSAL_PAYLOADS = [
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "....//....//....//etc/passwd",
    "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
    "..%252f..%252f..%252fetc%252fpasswd",
]

async def test_path_traversal_protection():
    """Test file access endpoints against path traversal."""
    async with aiohttp.ClientSession() as session:
        for payload in PATH_TRAVERSAL_PAYLOADS:
            response = await session.get(
                f"{GGEN_API_URL}/api/files",
                params={"path": payload}
            )

            assert response.status in [400, 403, 404], \
                f"Path traversal not blocked: {payload}"

            # Verify response doesn't contain sensitive data
            text = await response.text()
            assert "root:" not in text
            assert "Administrator" not in text
```

### Scenario 5: CSRF Attack

**Objective**: Verify CSRF token validation.

```python
# tests/security/attacks/csrf.py

async def test_csrf_protection():
    """Test CSRF token requirement for state-changing operations."""
    async with aiohttp.ClientSession() as session:
        # 1. Authenticate to get session
        await session.post(
            f"{GGEN_API_URL}/api/auth/login",
            json={"email": "user@test.com", "password": "password"}
        )

        # 2. Try POST without CSRF token
        response = await session.post(
            f"{GGEN_API_URL}/api/users/delete",
            json={"user_id": 123}
        )
        assert response.status == 403, "CSRF protection not enforced"

        # 3. Get CSRF token
        csrf_response = await session.get(f"{GGEN_API_URL}/api/csrf-token")
        csrf_token = (await csrf_response.json())["token"]

        # 4. Try POST with valid CSRF token
        response = await session.post(
            f"{GGEN_API_URL}/api/users/update",
            json={"user_id": 123, "name": "Updated"},
            headers={"X-CSRF-Token": csrf_token}
        )
        assert response.status in [200, 404], "CSRF token rejected incorrectly"
```

### Scenario 6: DoS Attack

**Objective**: Verify DoS protection (Week 11 deliverable).

```python
# tests/security/attacks/dos.py

async def test_dos_protection():
    """Test DoS protection mechanisms."""

    # Test 1: Connection limit per IP
    async with aiohttp.ClientSession() as session:
        tasks = []
        for i in range(150):  # Exceed 100/IP limit
            task = session.get(f"{GGEN_API_URL}/api/health")
            tasks.append(task)

        responses = await asyncio.gather(*tasks, return_exceptions=True)

        # At least 50 should be blocked (150 - 100 = 50)
        errors = [r for r in responses if isinstance(r, Exception)]
        assert len(errors) >= 50, "Connection limit not enforced"

    # Test 2: Rate limiting
    start = time.time()
    success_count = 0
    for _ in range(200):  # Exceed 100/min limit
        response = await session.get(f"{GGEN_API_URL}/api/health")
        if response.status == 200:
            success_count += 1

    elapsed = time.time() - start
    assert success_count < 110, f"Rate limit not enforced: {success_count}/200"

    # Test 3: Circuit breaker
    # Trigger 50% error rate
    for _ in range(10):
        await session.get(f"{GGEN_API_URL}/api/trigger-error")

    # Next requests should be circuit-broken
    response = await session.get(f"{GGEN_API_URL}/api/trigger-error")
    assert response.status == 503, "Circuit breaker not triggered"
```

### Scenario 7: Session Hijacking

**Objective**: Verify session security.

```python
# tests/security/attacks/session_hijacking.py

async def test_session_fixation_protection():
    """Verify session ID changes after authentication."""
    async with aiohttp.ClientSession() as session:
        # Get initial session
        response = await session.get(f"{GGEN_API_URL}/")
        cookies_before = session.cookie_jar.filter_cookies(GGEN_API_URL)

        # Authenticate
        await session.post(
            f"{GGEN_API_URL}/api/auth/login",
            json={"email": "user@test.com", "password": "password"}
        )

        # Verify session ID changed
        cookies_after = session.cookie_jar.filter_cookies(GGEN_API_URL)
        assert cookies_before != cookies_after, \
            "Session fixation vulnerability: session ID not changed"

async def test_session_timeout():
    """Verify sessions expire after timeout."""
    async with aiohttp.ClientSession() as session:
        # Authenticate
        await session.post(
            f"{GGEN_API_URL}/api/auth/login",
            json={"email": "user@test.com", "password": "password"}
        )

        # Verify active session
        response = await session.get(f"{GGEN_API_URL}/api/users/me")
        assert response.status == 200

        # Wait for timeout (30 seconds)
        await asyncio.sleep(35)

        # Verify session expired
        response = await session.get(f"{GGEN_API_URL}/api/users/me")
        assert response.status == 401, "Session timeout not enforced"
```

---

## Manual Testing Procedures

### Procedure 1: Privilege Escalation

**Steps**:

1. Authenticate as normal user (`user@test.com`)
2. Attempt to access admin endpoints:
   - `GET /api/admin/users`
   - `POST /api/admin/settings`
   - `DELETE /api/users/{other_user_id}`
3. Verify all return 403 Forbidden
4. Attempt to modify own user role in profile update
5. Verify role changes are rejected

**Expected**: All privilege escalation attempts blocked.

### Procedure 2: API Key Leakage

**Steps**:

1. Generate API key
2. Check HTTP responses for key exposure
3. Check error messages
4. Check logs for plaintext keys
5. Verify keys are hashed/encrypted in database

**Expected**: API keys never exposed in responses or logs.

### Procedure 3: HTTPS Enforcement

**Steps**:

```bash
# Attempt HTTP connection
curl http://localhost:8080/api/health

# Expected: Redirect to HTTPS or connection refused

# Verify TLS version
openssl s_client -connect localhost:8443 -tls1_2

# Expected: TLS 1.2 or higher
```

### Procedure 4: Security Headers

**Steps**:

```bash
curl -I https://localhost:8443/

# Verify headers present:
# - X-Frame-Options: DENY or SAMEORIGIN
# - X-Content-Type-Options: nosniff
# - X-XSS-Protection: 1; mode=block
# - Strict-Transport-Security: max-age=31536000
# - Content-Security-Policy: frame-ancestors 'none'
```

---

## Vulnerability Remediation

### Critical Severity

| Vulnerability | Remediation | Verification |
|---------------|-------------|--------------|
| SQL Injection | Use parameterized queries, ORM | Run automated SQL injection tests |
| Authentication Bypass | Implement proper auth middleware | Manual privilege testing |
| Path Traversal | Whitelist file paths | Path traversal attack tests |

### High Severity

| Vulnerability | Remediation | Verification |
|---------------|-------------|--------------|
| XSS | Sanitize all user inputs | XSS payload tests |
| CSRF | Implement CSRF tokens | CSRF attack simulation |
| Session Fixation | Regenerate session on auth | Session hijacking tests |

### Medium Severity

| Vulnerability | Remediation | Verification |
|---------------|-------------|--------------|
| Weak Password Policy | Enforce complexity requirements | Password validation tests |
| Missing Rate Limiting | Implement token bucket | Brute force simulation |
| Information Leakage | Generic error messages | Error message inspection |

---

## Reporting and Documentation

### Vulnerability Report Template

```markdown
# Vulnerability Report

**ID**: VULN-2026-001
**Severity**: [Critical|High|Medium|Low]
**Discoverer**: [Name]
**Date**: 2026-01-24

## Summary
[Brief description]

## Technical Details
[Detailed technical explanation]

## Reproduction Steps
1. [Step 1]
2. [Step 2]

## Impact Assessment
- **Confidentiality**: [High|Medium|Low]
- **Integrity**: [High|Medium|Low]
- **Availability**: [High|Medium|Low]

## Remediation
[Specific fix recommendations]

## Verification
[Steps to verify fix]
```

### Penetration Test Report Structure

1. **Executive Summary**
   - Scope and methodology
   - Key findings summary
   - Risk assessment
   - Recommendations

2. **Technical Findings**
   - Detailed vulnerabilities
   - Proof-of-concept exploits
   - Attack paths diagram

3. **Remediation Plan**
   - Prioritized action items
   - Implementation timeline
   - Verification checklist

4. **Appendices**
   - Test scripts
   - Tool configurations
   - Network diagrams

---

## Security Testing Checklist

- [ ] All OWASP Top 10 tests passing
- [ ] Fuzzing completed for 5+ targets
- [ ] DoS protection verified (100/IP, 10000 total)
- [ ] Circuit breaker tested (50% threshold)
- [ ] Load test passed (10000 concurrent)
- [ ] All security headers present
- [ ] TLS 1.2+ enforced
- [ ] API keys encrypted in database
- [ ] Audit logs capturing security events
- [ ] Dependency audit clean (`cargo audit`)
- [ ] All tests documented and automated

---

## Continuous Security Testing

```bash
# Add to CI/CD pipeline
.github/workflows/security.yml:

jobs:
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run security tests
        run: |
          cargo make audit
          cargo make test security::
          pytest tests/security/attacks/
      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: security-report
          path: target/security-report.html
```

---

**Last Updated**: January 24, 2026
**Version**: 1.0.0
**Maintained By**: Security Team
