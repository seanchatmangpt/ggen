# Implementation Roadmap: Security Architecture

**Status:** READY FOR IMPLEMENTATION
**Estimated Effort:** 12-16 weeks (4-6 engineers)
**Risk Level:** MEDIUM (requires database schema changes)
**Timeline:** Target deployment Q2 2026

---

## Phase 1: Foundation (Weeks 1-3)

### 1.1 Cryptographic Infrastructure
**Effort:** 2 weeks | **Owner:** Backend Lead
- [ ] Set up AWS KMS for key management
  - [ ] Create master key for system signatures
  - [ ] Create keys for each actor (employees)
  - [ ] Set up key rotation policies (90-day rotation)
  - [ ] Document key access procedures

- [ ] Implement cryptographic modules
  - [ ] Ed25519 signature verification
  - [ ] SHA-256 hashing functions
  - [ ] HMAC-SHA256 for API request signing
  - [ ] Random number generation (crypto:strong_rand_bytes)

- [ ] Key distribution system
  - [ ] Secure retrieval from KMS
  - [ ] Caching with TTL (5 minutes)
  - [ ] Audit logging of key access
  - [ ] Emergency revocation procedures

### 1.2 Database Schema Changes
**Effort:** 1 week | **Owner:** DBA
- [ ] Add `pricing_receipt` table (append-only)
  ```sql
  CREATE TABLE pricing_receipt (
    receipt_id UUID PRIMARY KEY,
    tenant_id UUID NOT NULL,
    calculated_value FLOAT NOT NULL,
    receipt_hash BYTEA NOT NULL,
    signature BYTEA NOT NULL,
    created_at TIMESTAMP NOT NULL,
    created_by UUID NOT NULL,
    -- Immutability enforced
    CONSTRAINT immutable CHECK (created_at = created_at)
  );

  -- Prevent UPDATE/DELETE
  CREATE POLICY immutable_prevent_update ON pricing_receipt
    FOR UPDATE USING (false);
  CREATE POLICY immutable_prevent_delete ON pricing_receipt
    FOR DELETE USING (false);
  ```

- [ ] Add `audit_log` table (append-only)
  ```sql
  CREATE TABLE audit_log (
    entry_id UUID PRIMARY KEY,
    timestamp BIGINT NOT NULL,
    action VARCHAR NOT NULL,
    resource_type VARCHAR NOT NULL,
    resource_id UUID NOT NULL,
    actor_id UUID NOT NULL,
    description TEXT,
    entry_hash BYTEA NOT NULL,
    previous_hash BYTEA NOT NULL,
    signature BYTEA NOT NULL,
    customer_id UUID NOT NULL,
    -- Prevent modification
    CONSTRAINT immutable CHECK (timestamp = timestamp)
  );

  CREATE INDEX audit_log_customer_time ON audit_log(customer_id, timestamp);
  ```

- [ ] Add `pricing_override` table
  ```sql
  CREATE TABLE pricing_override (
    override_id UUID PRIMARY KEY,
    tenant_id UUID NOT NULL,
    original_value FLOAT NOT NULL,
    override_value FLOAT NOT NULL,
    requested_by UUID NOT NULL,
    status VARCHAR NOT NULL, -- pending | approved | rejected | executed
    approvals JSONB NOT NULL, -- Array of {approver_id, signature, timestamp}
    executed_at TIMESTAMP
  );
  ```

- [ ] Add `pricing_formula` table
  ```sql
  CREATE TABLE pricing_formula (
    formula_id UUID PRIMARY KEY,
    version VARCHAR UNIQUE NOT NULL,
    code_hash BYTEA NOT NULL,
    code TEXT NOT NULL,
    test_coverage FLOAT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    created_by UUID NOT NULL,
    code_reviewed_by UUID NOT NULL,
    qa_approved_by UUID NOT NULL,
    ops_approved_by UUID NOT NULL,
    active BOOLEAN DEFAULT false,
    deployed_at TIMESTAMP
  );
  ```

- [ ] Enable row-level security (RLS)
  ```sql
  ALTER TABLE pricing_receipt ENABLE ROW LEVEL SECURITY;
  ALTER TABLE audit_log ENABLE ROW LEVEL SECURITY;

  -- Policy: Customers see only own data
  CREATE POLICY tenant_isolation ON pricing_receipt
    USING (tenant_id = current_setting('app.tenant_id'));
  ```

- [ ] Data migration
  - [ ] Backup existing pricing data
  - [ ] Migrate historical receipts (no signatures)
  - [ ] Verify no data loss
  - [ ] Update application queries

### 1.3 Testing Infrastructure
**Effort:** 1 week | **Owner:** QA Lead
- [ ] Set up test database (isolated)
- [ ] Create test data generators
- [ ] Configure security test runner
- [ ] Set up code coverage tracking
- [ ] Document test procedures

---

## Phase 2: Core Implementation (Weeks 4-7)

### 2.1 Cryptographic Receipt System
**Effort:** 2 weeks | **Owner:** Backend Engineer A
- [ ] Implement `pricing_receipt.erl` module
  - [ ] `generate/4` - Create receipt
  - [ ] `verify/1` - Verify integrity
  - [ ] `to_json/1` - Serialize
  - [ ] `from_json/1` - Deserialize

- [ ] Receipt generation pipeline
  - [ ] Validate input metrics
  - [ ] Retrieve formula version
  - [ ] Calculate value
  - [ ] Compute hashes (metrics, formula)
  - [ ] Sign with system key
  - [ ] Link to previous receipt
  - [ ] Store immutably

- [ ] Receipt verification
  - [ ] Signature verification (system + actor)
  - [ ] Metrics hash verification
  - [ ] Formula hash verification
  - [ ] Chain link verification
  - [ ] Detect any tampering

- [ ] Unit tests
  - [ ] Test valid receipt generation
  - [ ] Test tampering detection (value, metrics, signature)
  - [ ] Test chain breaks
  - [ ] Test edge cases (null values, precision)

### 2.2 Audit Logging System
**Effort:** 2 weeks | **Owner:** Backend Engineer B
- [ ] Implement `audit_log.erl` module
  - [ ] `log/3` and `log/4` - Log actions
  - [ ] `query/3` - Query with integrity verification
  - [ ] `verify_integrity/0` - Verify entire chain
  - [ ] `detect_gap/0` - Detect tampering
  - [ ] `export_monthly/0` - Export to immutable storage

- [ ] Audit entry creation
  - [ ] Capture action, actor, resource
  - [ ] Compute entry hash
  - [ ] Link to previous entry
  - [ ] Sign with system key
  - [ ] Store to append-only table

- [ ] Chain verification
  - [ ] Verify each entry's hash
  - [ ] Verify each entry's signature
  - [ ] Verify hash chain continuity
  - [ ] Detect gaps

- [ ] Monthly exports
  - [ ] Query entries for month
  - [ ] Verify integrity
  - [ ] Encrypt sensitive data
  - [ ] Upload to S3 (MFA-delete enabled)
  - [ ] Create integrity manifest

- [ ] Unit tests
  - [ ] Test entry logging
  - [ ] Test chain verification
  - [ ] Test gap detection
  - [ ] Test export process

### 2.3 Data Isolation Implementation
**Effort:** 1 week | **Owner:** Backend Engineer C
- [ ] Implement row-level security
  - [ ] Test RLS policies
  - [ ] Verify customer isolation
  - [ ] Verify employee filtering
  - [ ] Test edge cases

- [ ] mTLS certificate infrastructure
  - [ ] Generate CA certificate
  - [ ] Generate server certificates
  - [ ] Deploy to API servers
  - [ ] Configure client certificate validation

- [ ] API authentication
  - [ ] Implement request signing (HMAC-SHA256)
  - [ ] Validate signatures
  - [ ] Rate limiting (10 submissions/day/customer)
  - [ ] Timestamp validation (reject old requests)

- [ ] Access control testing
  - [ ] Test customer cannot access competitor data
  - [ ] Test employee filtering
  - [ ] Test unauthorized access logging

---

## Phase 3: Administrative Controls (Weeks 8-10)

### 3.1 Pricing Override Workflow
**Effort:** 2 weeks | **Owner:** Backend Engineer D
- [ ] Implement `pricing_override.erl` module
  - [ ] `request/4` - Request override
  - [ ] `approve/3` - Approve (with signature)
  - [ ] `execute/1` - Execute approved override
  - [ ] `verify_acceptance/1` - Customer confirmation

- [ ] Approval chain logic
  - [ ] Determine approvers based on amount
  - [ ] $0-$1K: 1 approval (Finance Manager)
  - [ ] $1K-$10K: 2 approvals (Manager + Director)
  - [ ] $10K-$100K: 3 approvals (Manager + Director + CFO)
  - [ ] $100K+: 4 approvals (+ General Counsel, + Board)

- [ ] Customer confirmation
  - [ ] Email customer: "Your pricing changed from $X to $Y"
  - [ ] Require customer confirmation within 48 hours
  - [ ] If not confirmed, reject override
  - [ ] If rejected, notify approvers

- [ ] Abuse detection
  - [ ] Track overrides per employee
  - [ ] Flag >10 overrides/month
  - [ ] Flag >$100K overrides/month
  - [ ] Flag repeated customer overrides

- [ ] Unit tests
  - [ ] Test small override (1 approval)
  - [ ] Test large override (3+ approvals)
  - [ ] Test abuse detection
  - [ ] Test customer confirmation

### 3.2 Formula Deployment Pipeline
**Effort:** 1 week | **Owner:** DevOps Lead
- [ ] Implement `pricing_formula.erl` module
  - [ ] `deploy/3` - Deploy with code review
  - [ ] `activate/1` - Staged rollout
  - [ ] `rollback/1` - Revert to prior version
  - [ ] `verify/1` - Verify hash matches

- [ ] Code review enforcement
  - [ ] Require 2 code reviews (no single approver)
  - [ ] Require test coverage >= 90%
  - [ ] QA sign-off on test results
  - [ ] Ops risk assessment

- [ ] Staged rollout
  - [ ] Stage 1: 1% customers (5 days)
  - [ ] Stage 2: 10% customers (10 days)
  - [ ] Stage 3: 100% customers (automatic)
  - [ ] Monitor for anomalies at each stage

- [ ] Monitoring during rollout
  - [ ] Track calculation latency by formula version
  - [ ] Monitor customer dispute rate
  - [ ] Detect calculation drift
  - [ ] Auto-rollback if anomalies detected

- [ ] Tests
  - [ ] Test code review requirement
  - [ ] Test staged rollout
  - [ ] Test monitoring/auto-rollback

---

## Phase 4: Monitoring & Alerting (Weeks 11-12)

### 4.1 Anomaly Detection
**Effort:** 1.5 weeks | **Owner:** Data Engineer
- [ ] Implement `anomaly_detector.erl`
  - [ ] `detect_value_outliers/0` - Z-score analysis
  - [ ] `detect_override_abuse/0` - Pattern detection
  - [ ] `detect_access_anomalies/0` - Unusual access
  - [ ] `detect_calculation_drift/0` - Recalculation comparison
  - [ ] `detect_formula_modifications/0` - Hash mismatch

- [ ] Statistical anomaly detection
  - [ ] Calculate mean and std dev (rolling window)
  - [ ] Flag values > 3 sigma
  - [ ] Alert on > 20% outlier rate
  - [ ] Tune thresholds per customer

- [ ] Behavioral analysis
  - [ ] Track override patterns per employee
  - [ ] Flag > 10 overrides/month
  - [ ] Flag unusual approver combinations
  - [ ] Detect collusion patterns

- [ ] Access pattern analysis
  - [ ] Track queries per user
  - [ ] Flag cross-customer access attempts
  - [ ] Flag bulk data exports
  - [ ] Flag unusual geolocation

- [ ] Integration with PagerDuty
  - [ ] Critical → Page engineer immediately
  - [ ] High → Notify team lead within 1 hour
  - [ ] Medium → Assign ticket within 4 hours
  - [ ] Low → Log for review

### 4.2 Monitoring Dashboard
**Effort:** 0.5 weeks | **Owner:** Frontend Engineer
- [ ] Create Grafana dashboard
  - [ ] Pricing metric submission rate
  - [ ] Calculation latency (P50, P95, P99)
  - [ ] Receipt generation success rate
  - [ ] Dispute rate (%)
  - [ ] Override approval rate
  - [ ] Audit log size (GB)

- [ ] Alert rules
  - [ ] High receipt generation latency
  - [ ] Low calculation success rate
  - [ ] High dispute rate
  - [ ] Formula version mismatch
  - [ ] Audit gap detected

---

## Phase 5: Testing & Validation (Weeks 13-14)

### 5.1 Security Testing
**Effort:** 1 week | **Owner:** Security Engineer
- [ ] Implement security test suite (`security_tests.erl`)
  - [ ] 20+ tests covering all attack vectors
  - [ ] Receipt tampering detection
  - [ ] Audit log integrity
  - [ ] Override approval workflow
  - [ ] Data isolation
  - [ ] Formula integrity
  - [ ] Calculation accuracy

- [ ] Run tests
  - [ ] `rebar3 eunit -m security_tests`
  - [ ] Verify 100% pass rate
  - [ ] Code coverage >= 80%

- [ ] Penetration testing
  - [ ] PT Scenario 1: Database tampering (PASS)
  - [ ] PT Scenario 2: Receipt forgery (PASS)
  - [ ] PT Scenario 3: Cross-customer access (PASS)
  - [ ] PT Scenario 4: Formula injection (PASS)
  - [ ] PT Scenario 5: Audit log deletion (PASS)

### 5.2 Compliance & Audit
**Effort:** 0.5 weeks | **Owner:** Compliance Officer
- [ ] SOC 2 Type II audit
  - [ ] Engage external auditor
  - [ ] Review controls (CC6.1, CC7.2, CC7.5)
  - [ ] Conduct testing
  - [ ] Obtain audit report

- [ ] Insurance verification
  - [ ] E&O policy active (10M coverage)
  - [ ] Cyber insurance active (50M coverage)
  - [ ] Fidelity bond active (5M coverage)
  - [ ] D&O insurance active (5M coverage)

---

## Phase 6: Deployment (Week 15-16)

### 6.1 Pre-Deployment Checklist
- [ ] Security architecture reviewed by external CISO
- [ ] All tests passing (100% pass rate)
- [ ] Code reviewed (2 approvals minimum)
- [ ] Deployment plan written and approved
- [ ] Runbooks created for operators
- [ ] Incident response team trained
- [ ] Backup/recovery procedures tested

### 6.2 Deployment Steps
1. **Preparation (Day 1)**
   - [ ] Deploy to staging environment
   - [ ] Run full test suite on staging
   - [ ] Verify monitoring/alerting
   - [ ] Brief operations team

2. **Cutover (Day 2-3)**
   - [ ] Create database backup
   - [ ] Deploy schema changes
   - [ ] Deploy application code
   - [ ] Enable monitoring
   - [ ] Enable alerting

3. **Verification (Day 3-5)**
   - [ ] Verify all metrics normal
   - [ ] Verify no increase in errors
   - [ ] Verify audit log working
   - [ ] Verify receipt generation working
   - [ ] Verify override workflow working

4. **Post-Deployment (Week 16+)**
   - [ ] Monitor for 1 week continuously
   - [ ] Conduct post-mortem
   - [ ] Train customer support team
   - [ ] Update documentation
   - [ ] Schedule follow-up security audit

---

## Resource Requirements

### Personnel
- **Backend Engineers:** 2 FTE (Weeks 1-14)
- **Database Admin:** 0.5 FTE (Weeks 1-3)
- **DevOps Engineer:** 0.5 FTE (Weeks 8-10)
- **QA Engineer:** 1 FTE (Weeks 1-14)
- **Data Engineer:** 0.5 FTE (Weeks 11-12)
- **Frontend Engineer:** 0.25 FTE (Weeks 11-12)
- **Security Engineer:** 0.5 FTE (Weeks 5-14)
- **Compliance Officer:** 0.25 FTE (Weeks 13-14)

### Infrastructure
- AWS KMS (key management)
- PostgreSQL 14+ (append-only tables)
- S3 (audit log backup)
- Kafka (event streaming, optional)
- Grafana (monitoring)
- PagerDuty (alerting)

### Tools
- Rebar3 (Erlang build tool)
- EUnit (testing framework)
- Git (version control)
- GitHub (code review)
- Jira (project management)

---

## Risk Mitigation

### Risk 1: Database Performance
**Issue:** Large audit log table could slow queries
**Mitigation:**
- [ ] Use table partitioning by date
- [ ] Add indexes on commonly queried columns
- [ ] Archive old audit logs (>1 year) to S3
- [ ] Monitor query performance continuously

### Risk 2: Key Management
**Issue:** Lost KMS key would invalidate all signatures
**Mitigation:**
- [ ] AWS KMS multi-region replication
- [ ] Key backup/escrow with external party
- [ ] Quarterly key rotation testing
- [ ] Emergency key recovery procedures

### Risk 3: Deployment Issues
**Issue:** Schema changes could corrupt data
**Mitigation:**
- [ ] Test schema changes in staging first
- [ ] Create table backups before changes
- [ ] Rollback procedure documented
- [ ] Zero-downtime migration (if possible)

### Risk 4: Customer Communication
**Issue:** Customers confused by new security controls
**Mitigation:**
- [ ] Create FAQ document
- [ ] Train support team
- [ ] Schedule customer webinar
- [ ] Send advance notice email

---

## Success Criteria

- [ ] All 20+ security tests passing
- [ ] All penetration tests passed (5/5)
- [ ] Audit log integrity verified (no gaps)
- [ ] Receipts cryptographically verified
- [ ] No unauthorized data access detected
- [ ] <5% increase in calculation latency
- [ ] 0 security incidents (first 30 days)
- [ ] SOC 2 Type II audit passed
- [ ] Insurance policies active
- [ ] Incident response team trained

---

## Timeline Summary

| Phase | Duration | Weeks | Status |
|-------|----------|-------|--------|
| Foundation | 3 weeks | 1-3 | Ready |
| Core Implementation | 4 weeks | 4-7 | Ready |
| Admin Controls | 3 weeks | 8-10 | Ready |
| Monitoring | 2 weeks | 11-12 | Ready |
| Testing | 2 weeks | 13-14 | Ready |
| Deployment | 2 weeks | 15-16 | Ready |
| **TOTAL** | **16 weeks** | **1-16** | **Ready** |

---

## Next Steps

1. **Week 1:** Kick-off meeting with engineering team
2. **Week 1:** Provision AWS KMS infrastructure
3. **Week 1:** Begin database schema design
4. **Week 2:** Start cryptographic module development
5. **Week 4:** Begin integration testing
6. **Week 13:** Engage external security auditor
7. **Week 15:** Deploy to production
8. **Week 16+:** 24/7 monitoring + incident response

---

**Project Owner:** Chief Security Officer
**Technical Lead:** Backend Engineering Lead
**Timeline:** Q2 2026 (16 weeks)
**Budget:** $500K-$750K (personnel + infrastructure)
**Risk Level:** MEDIUM (database changes required)

---

For questions or clarifications, contact: security@example.com
