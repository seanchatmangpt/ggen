# Statement of Work (SOW) Template - TAI Erlang Autonomics

**THIS DOCUMENT IS ATTACHED TO THE MSA AS EXHIBIT A**

---

## STATEMENT OF WORK (SOW)

**Effective Date**: [Same as MSA]

**BETWEEN:**

**SERVICE PROVIDER**: TAI Erlang Autonomics, Inc.

**CUSTOMER**: [Legal Name]

---

## 1. EXECUTIVE SUMMARY

### 1.1 Problem Statement

**Customer's Current Challenge**:

[CUSTOMER COMPLETES: Describe their specific pain point]

*Example*:
"Your team spends 40+ hours per month manually managing 150+ SKUs across five product tiers, with inconsistent enforcement across US/EU/APAC regions. This causes 2-3% of entitlement violations annually, costing approximately $50K-$500K in lost deals and compliance penalties. Additionally, your audit trail for SOC 2 compliance is incomplete, requiring 12 weeks of manual reconciliation per audit cycle."

### 1.2 Proposed Solution

TAI will deploy an **Autonomous SKU Management & Entitlement Governance Engine** on your Google Cloud Platform tenant that will:

- **Automate SKU provisioning** (87% reduction in manual work)
- **Enforce entitlements in real-time** (sub-50ms decision latency)
- **Generate cryptographic receipts** (100% audit trail for compliance)
- **Integrate seamlessly** with your existing systems (API + Cloud Pub/Sub options)

### 1.3 Expected Outcomes (Success Metrics)

By the end of this engagement, you will achieve:

| Metric | Baseline (Current) | Target (Post-TAI) | Measurement Method |
|--------|-------------------|-------------------|-------------------|
| **Manual SKU Work/Month** | 40 hours | 5 hours | Time tracking via Clockify/Harvest |
| **Entitlement Error Rate** | 2.1% | <0.1% | Automated error reconciliation report |
| **Audit Trail Coverage** | 60% | 100% | Cryptographic receipt count |
| **Compliance Audit Cycle** | 12 weeks | 4 weeks | Project timeline |
| **Time to Market for New Tier** | 5 days (engineering) | 4 hours (configuration) | Change request log |
| **System Availability** | 97% | 99.5%+ | Cloud Monitoring dashboard |

### 1.4 Investment & Timeline

**Total Investment**: $[PRICE]/year

**Implementation Timeline**: 8 weeks (Weeks 7-14, starting [DATE])

**Go-Live Target Date**: [8 weeks from start date]

**Payment Schedule**:
- 50% upon contract signature: $[X]
- 50% upon production go-live: $[X]

### 1.5 Scope Overview

This SOW covers:
1. Discovery & configuration (Weeks 1-2)
2. Testing & UAT (Weeks 3-4)
3. Production cutover (Weeks 5-6)
4. Training & optimization (Weeks 7-8)

---

## 2. DETAILED SCOPE OF WORK

### 2.1 Phase 1: Discovery & Configuration (Weeks 7-8)

**Objective**: Understand Customer's SKU model and entitlement rules; configure TAI

#### 2.1.1 SKU Structure Audit

**Activities**:
- [ ] Customer exports current SKU definitions (spreadsheet format: SKU ID, name, tier, region, pricing)
- [ ] TAI team analyzes structure (identify gaps, inconsistencies, orphaned SKUs)
- [ ] Workshop session (4 hours) to review findings and clarify edge cases
- [ ] Create data migration plan (mappings from current system to TAI)

**Deliverables**:
- [ ] SKU audit report (gap analysis, duplicates, inconsistencies identified)
- [ ] Data migration mapping (old SKU IDs → new SKU IDs in TAI)
- [ ] Migration risk assessment (high/medium/low severity issues)

**Success Criteria**: "95% of SKU definitions migrated with Customer approval"

**Timeline**: Week 7 (Days 1-5)

**Customer Responsibilities**:
- [ ] Provide SKU export within 24 hours of contract signature
- [ ] Assign 1 primary technical contact for daily check-ins
- [ ] Clarify entitlement logic for edge cases (e.g., "Does a Trial account see the full feature set?")

---

#### 2.1.2 Entitlement Rules Definition

**Activities**:
- [ ] Customer provides entitlement rule specifications (in collaborative Google Doc)
  - Rules = logical statements mapping SKU → feature access
  - Example: "Premium tier = all features; Basic tier = core only; Trial = core + limited usage"
- [ ] TAI team translates rules into TAI configuration (YAML or JSON format)
- [ ] Review session (2 hours) to validate translation accuracy
- [ ] Document complex rules or exceptions

**Deliverables**:
- [ ] Entitlement rule specifications document (shared Google Doc)
- [ ] TAI configuration file (YAML/JSON, ready for import)
- [ ] Validation report (rules vs. current system behavior, any conflicts identified)

**Success Criteria**: "All entitlement rules documented and agreed upon"

**Timeline**: Week 7 (Days 3-7)

**Customer Responsibilities**:
- [ ] Define rules clearly (including edge cases, overrides, exceptions)
- [ ] Identify who has authority to request rule changes post-deployment

---

#### 2.1.3 GCP Infrastructure Setup

**Activities**:
- [ ] Customer provisions GCP Cloud Run project (or TAI provisions if Customer lacks access)
  - [ ] Project ID: [Customer provides]
  - [ ] Service account with Cloud Run deploy permissions
  - [ ] Firewall rules allowing TAI IP range [X.X.X.X/24]
- [ ] TAI deploys TAI Erlang Autonomics container on Cloud Run
- [ ] Configure database backend (Cloud Firestore or Cloud SQL, Customer choice)
- [ ] Set up logging to Cloud Logging (for audit trail)
- [ ] Configure autoscaling (target: handle 10,000 requests/min)

**Deliverables**:
- [ ] GCP project provisioning checklist (infrastructure verified)
- [ ] TAI Erlang Autonomics deployment (live on Cloud Run)
- [ ] Database schema (Firestore collections or SQL tables created)
- [ ] Logging configuration (audit trail pipeline tested)

**Success Criteria**: "TAI health check endpoint responding 200 OK"

**Timeline**: Week 7-8 (Days 6-10)

**Customer Responsibilities**:
- [ ] Provide GCP project details (project ID, billing account, contact)
- [ ] Create service account with appropriate IAM roles (provided by TAI in setup guide)
- [ ] Approve firewall rule changes (if required by Customer's security policy)

---

#### 2.1.4 API Integration & Connectivity

**Activities**:
- [ ] Design integration architecture (Customer's system → TAI API)
  - [ ] Option A: Real-time REST API (recommended for <5k SKUs)
  - [ ] Option B: Batch CSV imports (recommended for high-volume, offline scenarios)
  - [ ] Option C: Cloud Pub/Sub topic (recommended for event-driven systems)
- [ ] Customer provides authentication credentials (service account, API key)
- [ ] TAI provisions API endpoints and rate limits
- [ ] Connectivity testing (TAI → Customer's current system, if needed)

**Deliverables**:
- [ ] API documentation (curl examples, request/response format)
- [ ] Integration architecture diagram (Customer system ↔ TAI)
- [ ] Authentication setup (API key/token provisioned)
- [ ] Connectivity test results (successful API call log)

**Success Criteria**: "Customer can make authenticated API calls to TAI endpoints"

**Timeline**: Week 8 (Days 8-10)

**Customer Responsibilities**:
- [ ] Provide current system API documentation (if available)
- [ ] Designate 1 engineer to configure Customer-side integration
- [ ] Approve data flow and API authentication method

---

### 2.2 Phase 2: Testing & User Acceptance Testing (UAT) (Weeks 9-10)

**Objective**: Validate TAI functionality against Customer's entitlement logic; gain confidence in production deployment

#### 2.2.1 Load Testing & Performance Verification

**Activities**:
- [ ] TAI team simulates 1 month of production traffic (historical data from Customer)
  - [ ] Load profile: [CUSTOMER COMPLETES: avg requests/min, peak requests/min]
  - [ ] Data volume: [CUSTOMER COMPLETES: number of SKUs, number of unique customers]
- [ ] Run sustained load test (24 hours minimum)
- [ ] Monitor performance metrics:
  - [ ] Response time (p50, p95, p99)
  - [ ] Error rate (5xx responses)
  - [ ] Database latency
  - [ ] Cloud Run CPU/memory utilization
- [ ] Capture baseline performance report

**Success Criteria**:
- [ ] p99 response time <50ms
- [ ] Error rate <0.1%
- [ ] Database throughput 10,000+ queries/sec

**Deliverables**:
- [ ] Load test report (response time histogram, error analysis)
- [ ] Performance baseline dashboard (Cloud Monitoring export)
- [ ] Scaling recommendations (if load profile indicates need for more resources)

**Timeline**: Week 9 (Days 11-14)

---

#### 2.2.2 Parallel Run (Shadow Mode) - 7 Days

**Objective**: Run TAI alongside current system; compare entitlement decisions without Customer impact

**Activities**:
- [ ] Customer routes 100% of entitlement check traffic to BOTH systems (current + TAI)
  - [ ] Current system: Production system-of-record
  - [ ] TAI: Shadow mode (decisions logged but not enforced)
- [ ] Compare decisions daily
  - [ ] Expected: 99.9%+ agreement (TAI vs. current system)
  - [ ] Investigate any discrepancies
- [ ] Duration: 7 days (covers weekday traffic patterns and weekend edge cases)

**Success Criteria**:
- [ ] Decision variance <0.1% (99.9%+ match)
- [ ] Zero false negatives (TAI doesn't block legitimate access)
- [ ] Zero false positives (TAI doesn't grant illegitimate access)

**Deliverables**:
- [ ] Daily variance reports (decisions compared, discrepancies logged)
- [ ] Final parallel run report (sign-off from Customer's product team)

**Timeline**: Week 9-10 (Days 14-21)

**Customer Responsibilities**:
- [ ] Configure traffic routing to TAI shadow endpoint
- [ ] Monitor logs and report discrepancies
- [ ] Participate in daily variance review calls (15 min each)

---

#### 2.2.3 User Acceptance Testing (UAT) - 10 Scenarios

**Objective**: Customer team validates TAI handles their specific business scenarios

**Test Scenarios** (Customer designs these, TAI provides test environment):

| Scenario # | Description | Expected Result | Pass/Fail |
|------------|-------------|-----------------|-----------|
| 1 | Trial customer upgrades to Premium → sees new features within 30s | Feature access provisioned | [ ] |
| 2 | Regional override: EU customer with US IP → enforce EU tier rules | Correct tier enforced | [ ] |
| 3 | Entitlement reversal: cancelled subscription → feature access removed | Access denied on next check | [ ] |
| 4 | Concurrent requests: 100 simultaneous entitlement checks | All respond <50ms p99 | [ ] |
| 5 | API malformed request: missing required field | 400 Bad Request (not 500 error) | [ ] |
| 6 | Rate limit: 1000 requests/sec → exceeds limit | 429 Too Many Requests | [ ] |
| 7 | Database failure (simulated): temporary Firestore outage | Auto-recovery within 5 min | [ ] |
| 8 | Audit trail: verify receipt for each entitlement check | SHA-256 hash verified | [ ] |
| 9 | Multi-tenant isolation: Tenant A cannot see Tenant B data | Access denied cleanly | [ ] |
| 10 | Edge case: Customer-specific business rule [describe] | [Expected result] | [ ] |

**Execution**:
- [ ] TAI provides staging environment (identical to production)
- [ ] Customer's QA team executes test scenarios
- [ ] TAI provides troubleshooting support (escalation SLA: <4 hours)
- [ ] Defects logged and prioritized
  - [ ] Critical (blocks production): Fix within 24 hours
  - [ ] High (workaround available): Fix within 48 hours
  - [ ] Medium/Low: Fix within 1 week or defer to Year 2

**Success Criteria**:
- [ ] 10/10 scenarios pass
- [ ] Zero critical defects
- [ ] Customer QA sign-off obtained in writing

**Deliverables**:
- [ ] UAT test plan (10 scenarios documented)
- [ ] UAT execution report (test results, defects, sign-off)
- [ ] Defect tracker (all issues logged + resolution status)

**Timeline**: Week 10 (Days 22-25)

**Customer Responsibilities**:
- [ ] Assign 2-3 QA engineers to execute tests
- [ ] Provide test data (realistic SKU/customer records)
- [ ] Sign-off on test results

---

### 2.3 Phase 3: Production Cutover (Weeks 11-12)

**Objective**: Move Customer's production traffic from current system to TAI with zero downtime

#### 2.3.1 Cutover Plan & Rehearsal

**Activities**:
- [ ] Document cutover plan (step-by-step runbook)
- [ ] Identify rollback procedures (revert to current system if issues occur)
- [ ] Define communication plan (who notifies whom if problems arise)
- [ ] Schedule dry run (cut over to staging environment, fully exercise)
- [ ] Full dress rehearsal (Saturday evening or Sunday morning before actual cutover)

**Cutover Plan Contents**:
1. **Pre-cutover checks** (Friday evening before cutover)
   - [ ] TAI staging environment passes all UAT tests
   - [ ] Database is in sync with production
   - [ ] Monitoring dashboards ready
   - [ ] Incident response team assembled

2. **Cutover window** (typically off-peak: 2 AM - 4 AM UTC)
   - [ ] Maintenance window announced to Customer stakeholders
   - [ ] Current system traffic cutover to TAI (DNS change or load balancer switch)
   - [ ] Verify traffic flowing to TAI (monitoring dashboard confirms)
   - [ ] Sample live transactions (5-10 real requests) and verify results

3. **Post-cutover monitoring** (hours 1-8)
   - [ ] Monitor error rates (alert if >0.5%)
   - [ ] Monitor response times (alert if p99 >100ms)
   - [ ] Monitor database throughput
   - [ ] Check audit trail (verify all transactions logged)
   - [ ] Customer team on standby for escalations

4. **Rollback procedure** (if critical issues found within 2 hours)
   - [ ] Revert DNS/load balancer to current system
   - [ ] Resume audit trail in current system
   - [ ] Root cause analysis (post-incident review)

**Success Criteria**:
- [ ] Dress rehearsal completed without critical issues
- [ ] Cutover plan reviewed & signed off by Customer
- [ ] Rollback procedure tested

**Deliverables**:
- [ ] Detailed cutover runbook (step-by-step procedures)
- [ ] Rollback procedures (tested)
- [ ] Communication plan (email templates, escalation list)
- [ ] Dress rehearsal report

**Timeline**: Week 11 (Days 26-30)

---

#### 2.3.2 Production Go-Live

**Activities**:
- [ ] Execute cutover plan (cut traffic to TAI production)
- [ ] Verify all transactions flowing through TAI
- [ ] Monitor continuously for 8 hours post-cutover
- [ ] Customer team monitors their system's behavior

**Success Criteria**:
- [ ] Cutover completed within planned window (off-peak)
- [ ] TAI processing 100% of entitlement requests
- [ ] Error rate <0.1%
- [ ] No customer-facing incidents
- [ ] All post-cutover checks passed

**Deliverables**:
- [ ] Go-live confirmation email (timestamp when cutover complete)
- [ ] Post-cutover monitoring report (first 24 hours)

**Timeline**: Week 12 (Day 31 - off-peak window)

---

#### 2.3.3 Stabilization Period (48-72 Hours Post-Cutover)

**Activities**:
- [ ] Continuous monitoring (TAI team on-call)
- [ ] Customer team monitoring their system (on-call)
- [ ] Daily check-in calls (brief 15-min sync)
- [ ] Log analysis (verify no suspicious patterns)
- [ ] Address any minor issues (non-blocking)

**Success Criteria**:
- [ ] Zero critical incidents
- [ ] System performance stable
- [ ] No customer complaints
- [ ] Audit trail verified

**Deliverables**:
- [ ] Stabilization report (72-hour monitoring summary)

**Timeline**: Week 12 (Days 32-34)

---

### 2.4 Phase 4: Training & Optimization (Weeks 13-14)

**Objective**: Train Customer's operations team; hand off to Customer; optimize for production load patterns

#### 2.4.1 Operator Training

**Objective**: Train Customer's ops team to manage TAI independently

**Training Content** (conducted via 2-day in-person or virtual workshop):

**Day 1: TAI Basics & Monitoring**
- [ ] Architecture overview (entitlement pipeline)
- [ ] Dashboard walkthrough (Cloud Monitoring, TAI admin console)
- [ ] Common operations: Add SKU, modify tier, check audit trail
- [ ] Troubleshooting: How to debug entitlement issues
- [ ] Hands-on lab: Create a test SKU and verify entitlement

**Day 2: Advanced Operations & Compliance**
- [ ] API integration (if Customer is making direct API calls)
- [ ] Change management process (how to request new tiers/rules)
- [ ] Audit trail review (compliance reporting)
- [ ] Backup & disaster recovery (data recovery procedures)
- [ ] Performance tuning (how to read performance dashboards)
- [ ] Escalation procedures (when to contact TAI support)

**Attendees**: 3-5 Customer operations engineers + 1 product manager

**Deliverables**:
- [ ] Training materials (slide deck + hands-on lab guide)
- [ ] Operator runbook (reference documentation)
- [ ] Video recordings (for team members who couldn't attend)

**Success Criteria**: "Customer's ops team can independently handle 80% of common tasks"

**Timeline**: Week 13 (Days 35-36)

**Customer Responsibilities**:
- [ ] Assign 3-5 operations engineers to attend training
- [ ] Allocate 2 full days for training
- [ ] Prepare environment for hands-on labs

---

#### 2.4.2 Post-Implementation Audit & Success Metrics Verification

**Objective**: Verify TAI delivered promised ROI; document success

**Activities**:
- [ ] Pull historical data (4 weeks of production metrics)
- [ ] Compare baseline vs. deployed metrics
- [ ] Calculate time savings (manual work reduction)
- [ ] Validate compliance audit trail
- [ ] Document lessons learned

**Metrics Verification**:

| Metric | Baseline | Target | Actual (Post-Deploy) | Variance |
|--------|----------|--------|----------------------|----------|
| Manual work/month | 40 hrs | 5 hrs | [MEASURE] | [%] |
| Error rate | 2.1% | <0.1% | [MEASURE] | [%] |
| Audit coverage | 60% | 100% | [MEASURE] | [%] |
| Compliance cycle | 12 wks | 4 wks | [MEASURE] | [%] |

**Deliverables**:
- [ ] Post-implementation audit report (ROI validated)
- [ ] Success metrics summary (Customer success team shares with leadership)
- [ ] Lessons learned document (what went well, what could improve)

**Timeline**: Week 14 (Days 37-40)

---

#### 2.4.3 Optimization & Performance Tuning

**Objective**: Optimize TAI for Customer's specific production traffic patterns

**Activities**:
- [ ] Analyze 4-week production logs
- [ ] Identify slow queries or bottlenecks
- [ ] Propose optimizations (database indexes, caching, etc.)
- [ ] Implement recommended optimizations
- [ ] Re-baseline performance (verify improvements)

**Deliverables**:
- [ ] Performance optimization report (before/after metrics)
- [ ] Recommendations for Year 2 improvements

**Timeline**: Week 14 (Days 38-40)

---

#### 2.4.4 Documentation Handoff

**Objective**: Customer has all documentation needed to operate independently

**Deliverables**:
- [ ] Operator runbook (operational procedures)
- [ ] API documentation (if applicable)
- [ ] Troubleshooting guide (common issues + solutions)
- [ ] Disaster recovery procedures (backup/restore)
- [ ] Compliance documentation (audit trail format, retention policy)
- [ ] TAI support contact info + SLAs

**Timeline**: Week 14 (Days 38-40)

---

## 3. SUPPORT & ESCALATION

### 3.1 Implementation Support (Included in Year 1)

During the 8-week implementation and 30 days post-launch:

- **Support Hours**: Monday-Friday, 9 AM - 5 PM PT
- **Response SLA**:
  - Critical (production impaired): <2 hours
  - High (workaround available): <4 hours
  - Medium/Low: Next business day

### 3.2 Post-Launch Support (30+ days after go-live)

After 30-day support period, support is available as add-on service:
- **Monthly support plan**: $[X]/month (optional)
- **Per-incident support**: $[X] per incident (pay-as-you-go)

---

## 4. SUCCESS METRICS & ACCEPTANCE CRITERIA

### 4.1 TAI's Success Criteria

TAI has successfully completed this SOW when:

- [ ] **All 4 phases delivered** on schedule (or documented delays with root cause)
- [ ] **UAT sign-off obtained** (all 10 test scenarios passing)
- [ ] **Production cutover completed** with zero critical incidents
- [ ] **Training completed** (Customer's ops team trained and confident)
- [ ] **Post-implementation audit** shows metrics within ±10% of targets

### 4.2 Customer's Acceptance Criteria

Customer accepts TAI deployment when:

- [ ] **Go-live achieved** (production cutover completed successfully)
- [ ] **Manual work reduced** by 80%+ (vs. baseline)
- [ ] **Error rate < 0.1%** (vs. 2.1% baseline)
- [ ] **Audit trail 100% coverage** (vs. 60% baseline)
- [ ] **Operations team trained** (can operate independently)

---

## 5. CHANGE MANAGEMENT & SCOPE ADJUSTMENTS

### 5.1 Change Order Process

If either party needs to modify this SOW:

1. **Change request submitted** (in writing, describing proposed change)
2. **Impact assessment** (TAI assesses effort, timeline, cost impact)
3. **Change order created** (signed by both parties, specifies cost & timeline impact)
4. **Work proceeds** only after change order signed

### 5.2 Scope Creep Control

Examples of **out-of-scope** requests (require change order):
- [ ] Integration with [third-party system not mentioned in this SOW]
- [ ] Custom API endpoints beyond standard TAI API
- [ ] Data migration from [legacy system] not mentioned
- [ ] Extended training beyond 2-day workshop
- [ ] Performance optimization beyond Phase 4

Examples of **in-scope** requests (no change order):
- [ ] Clarifying ambiguous requirements (within Phase 1 discovery)
- [ ] Bug fixes discovered during UAT
- [ ] Standard support questions during Phase 4
- [ ] Reasonable scope adjustments (Customer changes rule definitions)

### 5.3 Timeline Adjustments

If delays occur:
- **TAI-caused delays** (e.g., TAI misses Phase 2 deadline): TAI absorbs cost; timeline slips 1:1
- **Customer-caused delays** (e.g., Customer late providing SKU definitions): Timeline slips; no cost impact to TAI
- **External delays** (e.g., GCP service outage): Both parties accept delay; no penalty

---

## 6. CUSTOMER RESPONSIBILITIES

### 6.1 During Implementation

- [ ] **Assign dedicated resources**: 1 primary technical contact, 1 backup
- [ ] **Allocate time commitments**:
  - [ ] Discovery phase: 5-10 hours/week
  - [ ] Testing phase: 15-20 hours/week (UAT)
  - [ ] Cutover phase: 20 hours (cutover week)
  - [ ] Training phase: 10 hours (2-day workshop)
- [ ] **Provide data**: SKU definitions, entitlement rules, test data
- [ ] **Timely approvals**: Sign off on deliverables within 48 hours
- [ ] **Infrastructure access**: GCP project access, firewall rule changes (if needed)

### 6.2 Post-Launch

- [ ] **Maintain TAI infrastructure**: GCP project, billing, resource quotas
- [ ] **Monitor TAI health**: Customer responsible for alerting if service degrades
- [ ] **Provide feedback**: Report issues or enhancement requests to TAI support
- [ ] **Compliance responsibility**: Customer responsible for data governance, PII handling in TAI

---

## 7. DELIVERABLES SUMMARY

| Deliverable | Phase | Owner | Due Date |
|-------------|-------|-------|----------|
| SKU audit report | 1 | TAI | Week 7, Day 5 |
| Entitlement rules document | 1 | Both | Week 7, Day 7 |
| GCP infrastructure checklist | 1 | TAI | Week 8, Day 10 |
| API documentation | 1 | TAI | Week 8, Day 10 |
| Load test report | 2 | TAI | Week 9, Day 14 |
| 7-day parallel run report | 2 | TAI | Week 10, Day 21 |
| UAT execution report | 2 | TAI | Week 10, Day 25 |
| Cutover runbook | 3 | TAI | Week 11, Day 30 |
| Go-live confirmation | 3 | TAI | Week 12, Day 31 |
| Training materials & recordings | 4 | TAI | Week 13, Day 36 |
| Operator runbook | 4 | TAI | Week 14, Day 40 |
| Post-implementation audit report | 4 | TAI | Week 14, Day 40 |

---

## 8. PRICING & PAYMENT

**Total Service Fee**: $[Price from MSA]

**Payment Schedule** (matches MSA):
- 50% upon contract signature: $[X]
- 50% upon production go-live (Week 12): $[X]

**Additional Services** (if needed, outside this SOW):
- Training beyond 2-day workshop: $[X]/hour
- Performance consulting: $[X]/hour
- Custom integration development: $[X]/hour (estimated)

---

## 9. SIGNATURES

**THIS SOW IS BINDING UPON BOTH PARTIES WHEN SIGNED**

**FOR TAI ERLANG AUTONOMICS, INC.**:

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________


**FOR CUSTOMER**:

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________

---

**Document Version**: 1.0
**Status**: READY TO CUSTOMIZE
**Last Updated**: 2026-01-26

