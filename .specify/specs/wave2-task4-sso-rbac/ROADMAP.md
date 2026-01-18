# Wave 2, Task 4: SSO/RBAC Framework Implementation Roadmap

**Status**: Specification Complete (Wave 2) | Implementation Ready (Wave 3)
**Target Completion**: Wave 3 Weeks 4-8
**SOC 2 Type II Ready**: Wave 3 Exit Gate
**External Audit Date**: Q1 2027 (planned)

---

## Executive Summary

The ggen-disney SSO/RBAC framework establishes enterprise-grade security infrastructure for SOC 2, HIPAA, and GDPR compliance. This roadmap guides implementation from specification (Wave 2, complete) through testing, certification, and audit readiness (Wave 3).

**Key Components**:
1. **SSO Integration**: Disney AD (SAML) + Okta (OIDC) with failover
2. **RBAC Hierarchy**: 5-tier role model with granular permission scoping
3. **Audit Logging**: Immutable, cryptographically-signed event trail
4. **Data Retention**: Automated compliance with GDPR/HIPAA/CCPA
5. **Session Management**: MFA, timeout, revocation, device fingerprinting

**Success Criteria**:
- [ ] All SSO flows tested end-to-end (Disney AD + Okta)
- [ ] All RBAC permissions enforced (no privilege escalation)
- [ ] All audit events signed and validated (tamper detection)
- [ ] All compliance deadlines automated (GDPR 30d, CCPA 45d)
- [ ] SOC 2 Type II audit scheduled and evidence collected

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Enterprise Users                         │
│              (Disney, Operators, Auditors)                  │
└──────────────────────────┬──────────────────────────────────┘
                           │ OIDC/SAML
                    ┌──────▼──────────┐
                    │  Login Gateway  │
                    │ (Auth Middleware)│
                    └──────┬──────────┘
         ┌──────────────────┼──────────────────┐
         │                  │                  │
    ┌────▼────┐        ┌───▼────┐      ┌─────▼────┐
    │ Disney  │        │ Okta   │      │ Fallback │
    │   AD    │        │ OIDC   │      │ (Redis)  │
    │ (SAML)  │        │        │      │          │
    └────┬────┘        └───┬────┘      └─────┬────┘
         └──────────────────┼──────────────────┘
                            │
                    ┌───────▼────────┐
                    │ Session Store  │
                    │ (PostgreSQL +  │
                    │   Redis Cache) │
                    └───────┬────────┘
                            │
       ┌────────────────────┼────────────────────┐
       │                    │                    │
   ┌───▼────┐        ┌─────▼─────┐      ┌──────▼──────┐
   │  RBAC  │        │   Audit   │      │    Token    │
   │ Engine │        │   Logger  │      │  Blacklist  │
   │        │        │ (Immutable)│      │   (Redis)   │
   └────┬───┘        └─────┬─────┘      └──────┬──────┘
        │                  │                    │
        └──────────────────┼────────────────────┘
                           │
                    ┌──────▼──────────┐
                    │ API/CLI Layer   │
                    │ (Protected)     │
                    └─────────────────┘
```

---

## Phase 1: Identity Integration (Weeks 1-2)

**Objective**: Establish SSO foundation with Disney AD and Okta.

### Tasks

#### 1.1 Okta OIDC Configuration (Days 1-3)
- [ ] Create Okta application (OIDC type)
- [ ] Configure scopes: `openid profile email`
- [ ] Set redirect URIs: `http://localhost:3000/oauth/callback`, `https://api.ggen-disney.internal/oauth/callback`
- [ ] Generate client_id, client_secret (store in HashiCorp Vault)
- [ ] Enable MFA rules per role
- [ ] Configure device compliance policies (Okta Verify enrollment)
- **Test**: Okta OIDC authorization flow end-to-end
- **Evidence**: `oauth-flow-test.sh` (recorded output)

#### 1.2 Disney AD SAML Configuration (Days 2-4)
- [ ] Coordinate with Disney Identity Services for AD integration
- [ ] Configure SAML 2.0 IdP (Assertion Consumer Service URL)
- [ ] Request metadata: `SingleSignOnService` endpoint, signing certificate
- [ ] Map attributes: `sAMAccountName` → email, `department` → org
- [ ] Test SAML assertion validation
- **Test**: Disney AD SAML flow with signed assertions
- **Evidence**: SAML assertion validation test output

#### 1.3 Failover Logic (Day 5)
- [ ] Implement IdP failover strategy:
  - Primary: Okta OIDC (99.99% SLA)
  - Secondary: Disney AD SAML (99.5% SLA)
  - Fallback: Redis-cached session (sub-second latency)
- [ ] Implement circuit breaker for dead IdP detection
- [ ] Test all failover scenarios
- **Test**: IdP outage simulation, validate fallback activation
- **Evidence**: Failover test report, latency measurements

#### 1.4 Token Management (Days 3-5)
- [ ] Implement token refresh logic (OIDC refresh_token grant)
- [ ] Implement token revocation (Okta revocation endpoint)
- [ ] Implement token validation (JWKS key rotation)
- [ ] Set token expiry: access=15min, refresh=7days
- **Test**: Token refresh, revocation, expiry enforcement
- **Evidence**: Token lifecycle test output

**Acceptance Criteria**:
- ✓ Okta OIDC flow functional, tokens issued and validated
- ✓ Disney AD SAML flow functional, assertions parsed correctly
- ✓ Failover triggers on IdP latency > 2 seconds
- ✓ All token operations logged to audit trail

**Estimated Hours**: 40

---

## Phase 2: Session & RBAC Engine (Weeks 3-4)

**Objective**: Implement session lifecycle and permission model.

### Tasks

#### 2.1 Session Store Implementation (Days 6-9)
- [ ] Design session schema:
  ```rust
  Session {
    session_id: UUID,
    user_id: UUID,
    created_at: DateTime<Utc>,
    expires_at: DateTime<Utc>,
    idle_timeout: Duration,
    token_hash: SHA256,
    device_fingerprint: String,
    is_revoked: bool,
    concurrent_sessions_count: u32,
  }
  ```
- [ ] Implement session store (PostgreSQL primary, Redis cache)
- [ ] Implement session creation, validation, refresh, revocation
- [ ] Implement session timeout (idle + absolute)
- [ ] Implement session concurrency limit (max 3 concurrent)
- **Test**: Session CRUD operations, timeout enforcement, concurrency limits
- **Evidence**: Session lifecycle test suite (cargo test)

#### 2.2 RBAC Engine Implementation (Days 7-10)
- [ ] Define role hierarchy:
  - ADMIN (full system access)
  - MANAGER (team management + Operator permissions)
  - OPERATOR (own resources CRUD + dashboards)
  - AUDITOR (read-only audit logs + compliance reports)
  - VIEWER (read public dashboards + own profile)
- [ ] Implement permission model:
  ```rust
  Permission {
    resource_type: ResourceType, // user, role, audit_log, config, report, dashboard
    action: Action, // create, read, update, delete, export, audit
    scope: Scope, // own, team, org, public
  }
  ```
- [ ] Implement permission caching (TTL=5min, invalidation on change)
- [ ] Implement deny-by-default authorization
- [ ] Implement permission conflict resolution
- **Test**: All role-permission combinations, cache invalidation
- **Evidence**: Permission matrix test coverage report

#### 2.3 Role Assignment & Delegation (Days 8-10)
- [ ] Implement role assignment (user → role at T)
- [ ] Implement role delegation (temporary elevation, expiry)
- [ ] Implement delegation revocation
- [ ] Implement permission exception model (deny override)
- **Test**: Assignment, delegation, revocation flows
- **Evidence**: Delegation lifecycle test output

**Acceptance Criteria**:
- ✓ Session creation/validation completes in <10ms
- ✓ Permission check completes in <1ms (cached)
- ✓ No permission escalation possible (all edges covered)
- ✓ All role changes audited with full context

**Estimated Hours**: 48

---

## Phase 3: Audit Logging & Compliance (Weeks 5-6)

**Objective**: Implement immutable audit trail with cryptographic integrity.

### Tasks

#### 3.1 Audit Event Schema (Days 11-12)
- [ ] Design audit event structure:
  ```rust
  AuditEvent {
    event_id: UUID,
    event_type: EventType, // user.login, role.assign, data.delete, etc.
    timestamp: DateTime<Utc>,
    actor_user_id: UUID,
    resource_type: ResourceType,
    resource_id: UUID,
    action: String,
    result: Result, // success, failure
    error_message: Option<String>,
    client_ip: IpAddr,
    user_agent: String,
    session_id: UUID,
    request_id: UUID,
    correlation_id: UUID,
    signature: String, // HMAC-SHA256
    previous_hash: String, // Link to predecessor
    key_version: u32, // Signing key version
  }
  ```
- [ ] Implement event normalization (API, CLI, UI → canonical form)
- [ ] Implement event validation against schema
- **Test**: Event creation, validation, schema compliance
- **Evidence**: Schema validation test output

#### 3.2 Cryptographic Signing (Days 12-14)
- [ ] Implement HMAC-SHA256 signing:
  - Secret stored in Vault
  - Event serialized as deterministic JSON
  - Signature = HMAC-SHA256(event_json || key_version)
- [ ] Implement chain hashing:
  - previous_hash = SHA256(E_n-1_json || E_n-1_signature)
  - Links form immutable sequence
- [ ] Implement key rotation (v1 → v2 → v3)
- **Test**: Signature verification, chain integrity, key rotation
- **Evidence**: Cryptography test report (tamper detection tests)

#### 3.3 Immutable Storage (Days 13-15)
- [ ] Create audit log table (PostgreSQL):
  - Columns: event_id, event_json, signature, previous_hash, created_at
  - Constraints: UNIQUE(event_id), PRIMARY KEY(event_id), CHECK (previous_hash matches predecessor)
  - No UPDATE/DELETE permissions on table
- [ ] Implement append-only write pattern
- [ ] Implement fallback queue (in case DB unavailable)
- [ ] Test audit logging during system failures
- **Test**: Append-only enforcement, no data loss on failure
- **Evidence**: Failure scenario test report

#### 3.4 Audit Search & Indexing (Days 14-16)
- [ ] Create indexes:
  - (actor_user_id, timestamp)
  - (event_type, timestamp)
  - (resource_id, timestamp)
  - (client_ip, timestamp)
- [ ] Implement search API:
  ```rust
  search_audit_logs(filters: AuditFilter, limit: u32) → Vec<AuditEvent>
  ```
- [ ] Implement authorization for audit access
- [ ] Implement search evidence logging (audit the audit query)
- **Test**: Query latency (<500ms for 1M event log), authorization
- **Evidence**: Benchmark results, authorization test output

**Acceptance Criteria**:
- ✓ Every event signed and verified, tampering detectable
- ✓ Chain integrity validates (no missing/modified events)
- ✓ Search latency <500ms for any indexed combination
- ✓ Audit log proves continuous operation (no gaps)

**Estimated Hours**: 52

---

## Phase 4: Data Retention & Compliance (Weeks 7-8)

**Objective**: Automate GDPR/HIPAA/CCPA compliance.

### Tasks

#### 4.1 Retention Policy Engine (Days 17-19)
- [ ] Define retention schedules per data type:
  - Session data: 90 days (SOC 2)
  - Audit logs: 7 years (HIPAA)
  - User profiles: 30 days after deletion (GDPR)
  - PHI: 7 years (HIPAA)
- [ ] Implement retention policy storage (TTL field on each record)
- [ ] Implement background job for expiry detection
- **Test**: Retention schedule enforcement, no premature deletion
- **Evidence**: Retention test report

#### 4.2 GDPR Right-to-Erasure (Days 18-20)
- [ ] Implement deletion request workflow:
  1. User/data subject submits request
  2. System verifies identity
  3. Request stored with deadline (T + 30 days)
  4. Background job executes deletion before deadline
- [ ] Implement cryptographic erase (NIST SP 800-88):
  - 3-pass overwrite (zeros, ones, random)
  - Verification hash computed
- [ ] Implement deletion evidence logging (what was deleted, not the data itself)
- [ ] Implement GDPR data export (XML/JSON with all personal data)
- **Test**: Deletion deadline enforcement, data truly gone, export completeness
- **Evidence**: GDPR compliance test report, deletion evidence sample

#### 4.3 HIPAA De-identification (Days 19-21)
- [ ] Implement Safe Harbor de-identification:
  - Remove 18 HIPAA identifiers: names, SSN, MRN, dates (keep year only), etc.
  - Deterministic removal (reproducible)
- [ ] Implement Safe Harbor validation:
  - Verify all identifiers removed
  - Compute re-identification risk (<0.04)
- [ ] Implement de-identification certificate (signed by auditor)
- **Test**: De-identification completeness, re-id risk calculation, certificate generation
- **Evidence**: De-identification test report, sample certificate

#### 4.4 CCPA Compliance (Days 20-22)
- [ ] Implement deletion request (45-day SLA)
- [ ] Implement opt-out (do not sell personal data)
- [ ] Implement disclosure request (portable format)
- [ ] Implement verification (identity confirmation via email/auth)
- **Test**: CCPA deadline enforcement, opt-out enforcement, disclosure completeness
- **Evidence**: CCPA compliance test report

#### 4.5 Backup & Archival Strategy (Days 21-23)
- [ ] Define backup retention:
  - Daily backups: 30 days (PostgreSQL)
  - Weekly backups: 1 year (S3)
  - Monthly backups: 7 years (Glacier)
- [ ] Implement backup cleanup:
  - Exclude deleted data from new backups
  - Expire old backups per schedule
  - Test restore doesn't recover deleted data
- [ ] Implement audit log archival:
  - Hot: 90 days (PostgreSQL indexed)
  - Warm: 3 years (S3 Standard)
  - Cold: 7 years (S3 Glacier)
- **Test**: Backup cleanup, restore verification, archival retrieval latency
- **Evidence**: Backup test report

**Acceptance Criteria**:
- ✓ GDPR deletion requests completed within 30-day deadline
- ✓ HIPAA de-identification removes all identifiers
- ✓ CCPA opt-outs enforced (no data sales)
- ✓ Deleted data never reappears in backups
- ✓ Retention policy enforcement automated

**Estimated Hours**: 48

---

## Phase 5: Testing, Validation & Certification (Weeks 8-9)

**Objective**: Comprehensive testing and SOC 2 Type II readiness.

### Tasks

#### 5.1 Security Testing (Days 24-28)
- [ ] Penetration testing (external contractor)
  - Test SSO bypass attempts
  - Test RBAC privilege escalation
  - Test audit log tampering
  - Test session hijacking
- [ ] Fuzzing tests (audit event malformation, permission boundary conditions)
- [ ] Load testing (1M audit log search latency, session concurrency)
- [ ] Chaos testing (IdP outages, database partitions, key rotation)
- **Evidence**: Pentest report, load test results, chaos engineering analysis

#### 5.2 Compliance Validation (Days 25-29)
- [ ] GDPR readiness audit:
  - [ ] 30-day deletion SLA enforcement verified
  - [ ] Data export completeness verified
  - [ ] Audit trail shows data subject rights honored
- [ ] HIPAA readiness audit:
  - [ ] PHI access audit complete
  - [ ] De-identification Safe Harbor verified
  - [ ] Access control enforcement verified
- [ ] CCPA readiness audit:
  - [ ] 45-day deletion SLA enforcement verified
  - [ ] Opt-out enforcement verified
- [ ] SOC 2 Type II readiness:
  - [ ] Change management (CC7): all changes logged
  - [ ] Access control (CA): role-based access enforced
  - [ ] Monitoring (SI): audit logs complete and signed
- **Evidence**: Compliance audit checklist (signed by compliance officer)

#### 5.3 Operational Readiness (Days 26-29)
- [ ] Runbook creation:
  - [ ] SSO troubleshooting (IdP outages, token issues)
  - [ ] Session revocation (mass logout, incident response)
  - [ ] Audit log forensics (search, export, validation)
  - [ ] Compliance report generation (SOC 2, HIPAA, GDPR, CCPA)
- [ ] Incident response playbooks:
  - [ ] Potential insider threat detection
  - [ ] Unusual data access pattern response
  - [ ] Audit log tampering response
- [ ] Documentation:
  - [ ] API documentation (SSO, RBAC, audit APIs)
  - [ ] Compliance documentation (evidence mappings)
  - [ ] Architecture diagrams (security, data flow)
- **Evidence**: Runbooks, playbooks, architecture diagrams in `/docs`

#### 5.4 SOC 2 Type II Audit Prep (Days 27-30)
- [ ] Evidence collection:
  - [ ] System design documentation
  - [ ] Testing reports (security, compliance)
  - [ ] Change log (all modifications during audit period)
  - [ ] Monitoring evidence (continuous operation logs)
  - [ ] Training records (staff security awareness)
- [ ] External auditor coordination:
  - [ ] Engage SOC 2 Type II auditor
  - [ ] Schedule audit fieldwork (Weeks 10-12)
  - [ ] Provide evidence package
  - [ ] Schedule management interviews
- [ ] Certification:
  - [ ] Target: SOC 2 Type II certificate within 60 days of audit completion
- **Evidence**: Audit evidence package, auditor engagement contract

**Acceptance Criteria**:
- ✓ No critical security vulnerabilities (pentest clean)
- ✓ All compliance requirements met (audit checklist 100%)
- ✓ Operational procedures documented and tested
- ✓ SOC 2 Type II audit scheduled and evidence collected

**Estimated Hours**: 60

---

## Implementation Schedule

| Week | Phase | Tasks | Lead | Status |
|------|-------|-------|------|--------|
| W3-4 | Identity Integration | Okta, Disney AD, failover, tokens | Security Eng | 🚀 Ready |
| W5-6 | Session & RBAC | Session store, role engine, delegation | Backend Lead | 🚀 Ready |
| W7-8 | Audit Logging | Event schema, signing, storage, search | Backend + Security | 🚀 Ready |
| W9-10 | Data Retention | GDPR, HIPAA, CCPA, backups | Data Eng + Compliance | 🚀 Ready |
| W11-12 | Testing & Cert | Security tests, compliance audit, SOC 2 | QA + External Auditors | 🚀 Ready |

**Total Estimated Effort**: 248 hours (~31 person-weeks)
**Critical Path**: Audit logging (depends on session store) → Compliance testing

---

## Testing Strategy

### Unit Tests
- Session CRUD operations (50 tests)
- Permission evaluation (100 tests)
- Role inheritance (50 tests)
- Event signing and validation (50 tests)
- **Target Coverage**: >95% for security-critical code

### Integration Tests
- SSO flows (Disney AD, Okta, failover)
- Session lifecycle (create, refresh, revoke, concurrency)
- Permission scoping (own, team, org, public)
- Audit event logging and search
- Compliance deadline enforcement (GDPR, CCPA)
- **Target Execution Time**: <30 seconds

### End-to-End Tests
- Complete user login → authenticated → RBAC check → audit log → session revocation
- Data deletion request → 30-day countdown → auto-execute → verify gone
- Audit log search → export → sign → verify offline
- **Target Execution Time**: <2 minutes per scenario

### Security Tests
- Penetration testing (external contractor)
- Privilege escalation attempts
- Session hijacking attempts
- Audit log tampering detection
- Token replay prevention
- **Target**: 100% of critical threats tested

### Compliance Tests
- GDPR 30-day deletion enforcement
- HIPAA PHI access audit completeness
- CCPA opt-out enforcement
- SOC 2 change management logging
- **Target**: 100% of audit criteria verified

---

## Evidence Artifacts

All evidence stored in `.specify/specs/wave2-task4-sso-rbac/evidence/`:

| Artifact | Owner | Due | Status |
|----------|-------|-----|--------|
| `sso-integration-test-report.md` | Security Eng | W4 | 📋 Planned |
| `rbac-permission-matrix-test.json` | Backend Lead | W6 | 📋 Planned |
| `audit-log-tamper-detection-test.sh` | Backend + Security | W8 | 📋 Planned |
| `gdpr-compliance-audit.md` | Data Eng + Legal | W10 | 📋 Planned |
| `soc2-evidence-package.zip` | QA + Security | W12 | 📋 Planned |
| `penetration-test-report.pdf` | External Security Firm | W11 | 📋 Planned |
| `soc2-type2-certificate.pdf` | External Auditor | W14 | 📋 Planned |

---

## Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Okta SLA < 99.99% | Medium | High | Implement fallback to Disney AD; cache tokens in Redis |
| GDPR deletion edge case discovered | Medium | High | Legal review of deletion logic; run GDPR audit monthly |
| Audit log tampering undetected | Low | Critical | Chain hashing + offline validator; security team monthly verification |
| Permission escalation vulnerability | Low | Critical | Penetration testing; deny-by-default architecture review |
| Performance regression on large audit logs | Medium | Medium | Load testing (1M events); latency SLA enforcement |
| External auditor availability | Low | Medium | Engage auditor by Week 8; maintain audit-ready evidence |

---

## FMEA (Failure Mode & Effects Analysis)

### SSO Integration Failures

| Failure Mode | Cause | Effect | Mitigation |
|--------------|-------|--------|-----------|
| Okta outage | Network/SaaS | Users cannot login | Failover to Disney AD SAML, fallback to Redis session cache |
| Disney AD overload | Peak authentication load | >2s latency | Implement Okta as primary (lower latency), Disney AD secondary |
| SAML signature invalid | Key rotation mismatch | Assertions rejected | Maintain key versions, auto-retry with latest key |

### RBAC Failures

| Failure Mode | Cause | Effect | Mitigation |
|--------------|-------|--------|-----------|
| Privilege escalation | Cache corruption | User gains unauthorized access | Deny-by-default design, cache TTL=5min, external pentest |
| Permission deadlock | Circular role inheritance | Roles cannot be assigned | Cycle detection on role change, DAG validation |

### Audit Logging Failures

| Failure Mode | Cause | Effect | Mitigation |
|--------------|-------|--------|-----------|
| Audit event lost | DB outage | Event not logged | Fallback queue, async retry with exponential backoff |
| Chain hash mismatch | Database corruption | Tampering undetected | Monthly verification, offline validator tool |
| Performance regression | Full audit log scan | Slow searches | Mandatory indexes, archive hot logs after 90 days |

### Compliance Failures

| Failure Mode | Cause | Effect | Mitigation |
|--------------|-------|--------|-----------|
| GDPR deadline missed | Automation failure | Legal violation | Daily deadline reminder report, manual override 5 days before SLA |
| HIPAA data retention violated | Schedule misconfiguration | Breach risk | Compliance officer review quarterly, automated alerts |

---

## Success Metrics

### Performance Metrics
- [ ] SSO login latency: <2 seconds (primary IdP) + <5 seconds (failover)
- [ ] Permission check: <1 millisecond (cached)
- [ ] Audit log search: <500 milliseconds (1M event log)
- [ ] Session creation: <50 milliseconds
- [ ] Compliance report generation: <30 seconds (1 year of events)

### Security Metrics
- [ ] Penetration test: 0 critical vulnerabilities
- [ ] Audit log: 0% data loss (chain integrity 100%)
- [ ] Session security: 0% unauthorized access attempts (blocked)
- [ ] RBAC: 0% privilege escalation (all edges tested)

### Compliance Metrics
- [ ] GDPR: 100% of deletion requests completed within 30 days
- [ ] HIPAA: 100% of PHI access audited and authorized
- [ ] CCPA: 100% of opt-outs enforced (no data sales)
- [ ] SOC 2: 0% audit findings (clean audit)

---

## Go/No-Go Decision Criteria

**Go Decision (Wave 3 Exit Gate)**: All of:
- ✓ All security tests pass (>95% coverage, 0 critical vulnerabilities)
- ✓ All compliance tests pass (GDPR, HIPAA, CCPA, SOC 2 checklists 100%)
- ✓ Performance benchmarks meet SLA targets
- ✓ Operational runbooks documented and rehearsed
- ✓ External auditor confirms SOC 2 audit entry readiness

**No-Go Decision**: Any of:
- ✗ Critical security vulnerability discovered (cannot be mitigated)
- ✗ Compliance deadline cannot be met (e.g., GDPR legal risk)
- ✗ Performance SLA unachievable (system architecture change required)

---

## Post-Wave 3 Roadmap (Wave 4+)

### Wave 4 (Q2 2027)
- [ ] Multi-tenant RBAC (per-customer role hierarchies)
- [ ] Advanced MFA (biometrics, hardware tokens)
- [ ] Continuous authentication (session strength evaluation)
- [ ] Machine learning anomaly detection (insider threat)

### Wave 5 (Q3 2027)
- [ ] Fine-grained attribute-based access control (ABAC)
- [ ] Policy-as-code (Rego/OPA for complex rules)
- [ ] Real-time compliance dashboard (SOC 2, HIPAA, CCPA)
- [ ] Automated forensic incident response (playbook execution)

---

## Appendices

### A. TTL Specification Files

All specifications defined in `.specify/specs/wave2-task4-sso-rbac/`:
1. `sso-integration-schema.ttl` - SSO architecture, identity mapping
2. `rbac-hierarchy.ttl` - Role definitions, permission model
3. `audit-logging-schema.ttl` - Event structure, signatures, tamper detection
4. `data-retention-policies.ttl` - GDPR, HIPAA, CCPA compliance rules

### B. Compliance Frameworks

**GDPR** (General Data Protection Regulation)
- Art. 17: Right to Erasure (30-day SLA)
- Art. 20: Data Portability (7-day export)
- Art. 5: Accountability (audit trail)

**HIPAA** (Health Insurance Portability & Accountability Act)
- Security Rule: Access control, audit logs
- Privacy Rule: PHI protection, de-identification
- Breach Notification: 60-day notification requirement

**CCPA** (California Consumer Privacy Act)
- Sec. 1798.100: Right to Delete (45-day SLA)
- Sec. 1798.120: Right to Opt-Out (no data sale)
- Sec. 1798.145: Disclosure request (machine-readable format)

**SOC 2 Type II** (System & Organization Controls)
- CC7: Change Management (all changes logged)
- CA: Access Control (role-based authorization)
- SI: Monitoring (continuous operation)

### C. External Dependencies

| Component | Vendor | SLA | Cost | Contact |
|-----------|--------|-----|------|---------|
| Okta OIDC | Okta Inc. | 99.99% uptime | $X/month | AM contact |
| Disney AD | Disney IT | 99.5% uptime | Included | security-ops@disney.com |
| PostgreSQL | Managed AWS RDS | 99.95% (HA) | $Y/month | AWS Support |
| Redis | AWS ElastiCache | 99.95% uptime | $Z/month | AWS Support |
| S3/Glacier | AWS | 99.999% durability | $W/month | AWS Support |

### D. Key Contacts

| Role | Name | Email | Phone |
|------|------|-------|-------|
| Security Architect | [TBD] | security-arch@ggen.internal | +1-XXX-XXX-XXXX |
| Compliance Officer | [TBD] | compliance@ggen.internal | +1-XXX-XXX-XXXX |
| Legal Counsel | [TBD] | legal@ggen.internal | +1-XXX-XXX-XXXX |
| Okta Account Manager | [TBD] | [email] | [phone] |
| Disney Identity Ops | [TBD] | security-ops@disney.com | [phone] |
| SOC 2 Auditor | [TBD] | [email] | [phone] |

---

**Document Version**: 1.0
**Last Updated**: 2026-01-18
**Status**: 🟢 Ready for Implementation
**Next Review**: Week 4 of Wave 3
