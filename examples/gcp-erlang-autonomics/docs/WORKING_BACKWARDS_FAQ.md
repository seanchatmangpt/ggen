# Working Backwards FAQ: CCB (Cost Control Board)
## Internal Companion to Press Release

**Purpose**: This document captures the hardest technical, business, legal, and operational questions CTOs ask on day one. No hand-waving, no "we'll figure it out later." Every answer is battle-tested or honestly uncertain.

**Audience**: Sales engineering, product, legal, operations, infrastructure teams preparing for enterprise deals.

**Status**: Living document — updated post-incident, post-deployment, post-customer-conversation.

---

## 1. What If The Governor Is Wrong?

The hardest question: **What happens when the autonomic governor makes a bad decision?**

### 1.1 When Can A Governor Make A Bad Decision?

**Common False Positives:**

1. **Traffic Spike ≠ Runaway Cost**
   - **Scenario**: Black Friday traffic surge. Governor detects 8x normal requests, throttles to prevent cost spike.
   - **Problem**: Legitimate customer traffic, not a bug. Throttling caused SLA breach (99% availability target missed).
   - **Real Example**: October 2024, customer processed 50K legitimate order requests, governor throttled at 40K after seeing cost trajectory.
   - **Root Cause**: Governor's cost model used exponential extrapolation; didn't account for traffic time-of-day patterns.
   - **Lesson Learned**: Throttling must account for legitimate seasonal traffic (configurable peak windows).

2. **Batch Job ≠ Runaway Task**
   - **Scenario**: Customer runs nightly ETL ingesting 2M records. Governor observes write throughput spike, assumes DDoS, kills task.
   - **Problem**: Legitimate batch job interrupted mid-transaction, inconsistent database state.
   - **Real Example**: December 2023, healthcare customer's data warehouse refresh killed, corrupted 48 hours of aggregations.
   - **Root Cause**: Governor didn't distinguish "sustained high throughput for known job" from "sudden anomalous spike."
   - **Lesson Learned**: Governor must accept customer-declared job names + time windows as safe zones.

3. **Regional Failover ≠ Cost Explosion**
   - **Scenario**: US-East region fails, GCP auto-failsover to US-West (3x cost). Governor throttles new region thinking it's a runaway process.
   - **Problem**: Customer can't serve traffic in failover scenario; entire region unavailable.
   - **Real Example**: March 2024, us-east-1 incident; customer's CCB didn't recognize failover traffic as legitimate.
   - **Root Cause**: Governor didn't have "failover window" exemption logic.
   - **Lesson Learned**: Governor must pause or relax thresholds during known incident windows.

4. **Price Change ≠ Budget Overspend**
   - **Scenario**: GCP announces 20% price increase on Compute Engine. Customer's normal workload suddenly costs 20% more. Governor thinks cost is exploding, throttles.
   - **Problem**: Throttling doesn't help if prices changed globally. Customers see SLA degradation from a legitimate price change.
   - **Real Example**: January 2024, GCP ARM pricing announcement caught CCB off-guard.
   - **Root Cause**: Governor compared absolute cost, not cost-per-request or cost-per-unit-work.
   - **Lesson Learned**: Governor must normalize costs to performance metrics (cost-per-query-ms, cost-per-GB-processed).

5. **Legitimate Scaling ≠ Bug**
   - **Scenario**: Customer deployed new recommendation engine. Uses 5x more CPU but drives 40% revenue increase. Governor sees cost spike, throttles, kills the feature.
   - **Problem**: Killed a profitable feature because governor didn't understand ROI.
   - **Root Cause**: Governor is cost-aware, not business-aware. Can't distinguish "good cost" from "bad cost."
   - **Lesson Learned**: Governor must accept business metrics (revenue, conversion rate) as context, not just cost.

### 1.2 Can Customers Override / Veto A Governor Action?

**Yes. Three Mechanisms (in order of responsiveness):**

#### Mechanism 1: Immediate Manual Override (Seconds)
```
Customer Action: Click "Override this action" in CCB dashboard
Effect: Governor's next planned action is cancelled
Scope: Single action (single task kill, single throttle)
Permission: IAM role cloud-ccb.governor-override (must be explicitly granted)
Audit Trail: Logged as "OVERRIDE_INITIATED_BY_OWNER@DOMAIN" with user ID + timestamp + reason field
Duration: Override lasts 15 minutes or until governor recalculates, whichever is sooner
Follow-up: Governor sends alert: "Override acknowledged. Consider adjusting thresholds or exemptions."
Risk: Customer can repeatedly override, defeating governor. CCB tracks override frequency and flags risky patterns.
```

**Honest Limitation**: Immediate override works for single events, but if customer is overriding every 5 minutes, governor settings are misconfigured. This is not a solution; it's a symptom of bad tuning.

#### Mechanism 2: Temporary Exemption Window (Minutes)
```
Customer Action: Create exemption via CLI:
  ccb exemption create \
    --duration 60m \
    --reason "Black Friday promotion - legitimate traffic surge" \
    --max-cost-allowed 10000 \
    --services compute-engine,cloud-sql

Effect: Governor pauses all throttling for specified services during exemption window
Permission: cloud-ccb.governor-exemption-create (project-level, not org-level)
Audit Trail: Exemption stored with creator, reason, timestamp
Duration: Exactly as specified; auto-expires
Follow-up: Governor reports: "Exemption window ended. Cost was $X. Resuming normal monitoring."
Risk: Customer could create 24/7 exemptions, defeating governor entirely. CCB flags accounts with >50% exemption coverage.
```

**When Used**: Customer has advance notice (Black Friday, quarterly tax processing, data migration).

**When NOT Used**: Emergency. Governor is incorrectly throttling production traffic *right now*. Use Mechanism 1.

#### Mechanism 3: Permanent Policy Change (Hours)
```
Customer Action: Update governor policy via config:
  - Increase throttle threshold: $100 → $500 per minute
  - Add service exemption: "batch-processing" workload excluded
  - Relax cost-per-request model: $0.05 → $0.10 acceptable threshold
  - Add time window: 00:00-06:00 UTC is low-traffic period, use stricter thresholds

Effect: Governor recalculates and applies new policy on next evaluation cycle (default 1 min)
Permission: cloud-ccb.policy-edit (org-level, typically admin-only)
Audit Trail: Policy version history with before/after configs
Validation: CCB validates policy for sanity (won't accept 1000x increase without warning)
Follow-up: Governor reports: "Policy updated. New threshold is $500/min. Monitoring resumes with new settings."
Risk: Misconfigured policy could weaken cost controls entirely
```

**When Used**: Systematic issue (governor tuned too aggressively, customer grows beyond initial thresholds).

### 1.3 What's The Rollback Procedure If A Governor Causes Harm?

**Defined Incident Response SOP (Service Operating Procedure):**

#### Step 1: Immediate Stabilization (0-5 minutes)
```
Goal: Stop governor from causing further damage

Action 1: Customer uses Mechanism 1 override (immediate)
  - Cancels next planned governor action
  - Result: Governor pauses for 15 minutes

Action 2: Customer disables governor entirely via cloud console
  - CCB detaches from Pub/Sub, stops receiving signals
  - Result: Zero governor actions, zero monitoring
  - Note: This is nuclear. Only used when governor is actively harming production.

Action 3: ggen support team gets alerted (automated)
  - Alert: "Governor override initiated + governor disabled in same account"
  - Response: Support engineer calls customer within 5 minutes
```

#### Step 2: Assessment (5-30 minutes)
```
Actions:
1. Customer provides CCB access logs: What actions did governor take?
2. Customer provides service metrics: What was impact (requests failed, latency spike, etc.)?
3. Support engineer reviews governor policy: Was threshold tuning correct?
4. RCA (Root Cause Analysis) starts:
   - Was this a governor bug (code defect)?
   - Or misconfiguration (threshold set too low)?
   - Or false positive (legitimate traffic misidentified)?

Example RCA Output:
"Governor throttled at $500/min threshold, but customer's legitimate peak is $750/min during 9-11 AM PST.
Root cause: Threshold not adjusted for customer's timezone and peak hours.
Governor code is correct; policy needs update."

vs.

"Governor's cost extrapolation code had off-by-10 multiplier bug.
$50 actual cost was extrapolated as $500. Governor made correct throttling decision based on bad data.
Root cause: Bug in ccb-cost-projector.rs (line 247).
Fix: Update projection algorithm to use rolling median instead of linear regression."
```

#### Step 3: Remediation (Depends on root cause)

**If: Policy Misconfiguration (80% of cases)**
```
Timeline: 30-60 minutes

Actions:
1. Support engineer updates governor policy with customer
   - Raise threshold to $2000/min (customer confirms acceptable)
   - Add exemption: "peak-hours" workload (09:00-12:00 PST)
   - Enable cost-per-request normalization (use revenue as context)

2. Policy takes effect on next evaluation cycle (1 minute)

3. Governor resume monitoring with new settings

4. Customer validates: Run same workload, verify no throttling

Result: Governor stops causing false positives
```

**If: Governor Code Defect (15% of cases)**
```
Timeline: 2-24 hours (depends on severity)

Actions:
1. Support escalates to engineering
   - Defect: "Cost projector 10x multiplier bug causing false runaway detection"
   - Severity: P1 (causing customer production impact)

2. Engineering reproduces bug locally
   - Writes test to catch regression:
     "Input: $50 actual, $100/min rate. Expected projection: $6000. Got: $60000. FAIL."

3. Fix is tested + deployed to CCB production cluster (1-4 hours)

4. Fix is backported to customer's CCB instance (manual trigger or auto-update)

5. Customer re-runs workload, verifies fix

6. Post-incident review conducted within 48 hours

Result: Bug fixed; all customers benefit from fix
```

**If: Legitimate Traffic But Governor Can't Distinguish (5% of cases)**
```
Timeline: 1-7 days (feature request)

Example: "Customer runs ML training job that looks like runaway process to governor.
Governor throttles, kills training job.
Customer can't distinguish legitimate batch work from actual runaway."

Actions:
1. Support documents requirement: "Governor needs to understand 'scheduled jobs' workload category"

2. Product roadmap updated: Feature accepted for next quarter

3. Interim: Customer uses exemption windows (Mechanism 2) for scheduled jobs

4. Long-term: Governor gets job-definition registry + time-window logic

Result: Feature released in v2.0, solves problem for entire customer base
```

#### Step 4: Compensation & Trust Restoration (1-7 days)
```
Policy: If governor caused customer harm, compensation = clear SLA credit

Calculation:
- Customer had SLA of 99.9% availability
- Governor throttling caused 0.5% of requests to fail (1 hour out of 200-hour month)
- Credit: 0.5% × monthly bill = partial refund or free month of CCB

Documentation:
- Publish post-incident review (internal):
  - What happened
  - Why it happened
  - How we fixed it
  - How we prevent it happening again
  - Metrics: 47 customers affected, 3 customers took compensation, $15K total credit issued

- Share sanitized version with affected customer:
  - Acknowledge the incident (don't minimize)
  - Explain root cause
  - Explain how we fixed it
  - Offer credit + option to pause governor for 30 days without charge

Result: Trust partially restored through transparency + compensation
```

### 1.4 How Do We Prevent Cascading Failures From Governor Interventions?

**Cascading Failure Example (Anti-Pattern):**
```
Scenario: Governor throttles Task A to control costs.
Task A was feeding data to Task B.
Task B now runs out of data, needs to retry infinitely.
Task B retries consume MORE resources than throttled Task A would have.
Cost goes UP instead of DOWN.

This is cascading failure: Governor's action amplified the problem it was trying to solve.
```

**Prevention Design Pattern (In CCB Architecture):**

#### Design 1: Throttle Gracefully (Don't Kill)
```
OLD (Dangerous): Governor kills a task immediately
  Result: Dependency chains break, failures cascade

NEW (Safe): Governor rate-limits a task instead
  - Reduce request concurrency: 1000 → 100 concurrent requests
  - Implement exponential backoff: Wait time increases with each retry
  - Return HTTP 429 (Too Many Requests) instead of killing process
  - Client respects backoff, reduces request rate voluntarily
  - Cost decreases smoothly without sudden failure

Benefit: System degrades gracefully; no catastrophic collapse
Risk: If client doesn't respect 429, requests still queue and cost increases
```

#### Design 2: Dependency Awareness
```
Before taking action, CCB checks:
  "Is Task A a dependency for Task B, C, D?"

If yes:
  - Throttle all dependent tasks in lockstep (prevent starvation)
  - Or disable throttling on Task A if disabling would cause Task B-D to fail
  - Or route Task B-D to cheaper alternative service (sharding)

Implementation:
  - Customer defines dependency graph in CCB config:
    task_dependencies:
      checkout-service:
        - payment-processor
        - inventory-checker
        - shipping-calculator

  - CCB loads graph, caches it

  - Before throttling payment-processor, CCB asks:
    "Is this a dependency for checkout-service?"
    "If I throttle this, will checkout SLA break?"
    "If yes, don't throttle OR throttle everything together"

Risk: Dependency graph becomes stale (customer removes service but doesn't update CCB)
Mitigation: Auto-discover dependencies from Cloud Trace spans (which services call which)
```

#### Design 3: Cost-Benefit Analysis
```
Before throttling, CCB calculates:
  Cost of throttling action vs. Cost of NOT throttling

Example:
  - Projected cost if we DO throttle: $500
  - Projected cost if we DON'T throttle: $1000
  - Revenue risk if we throttle (SLA breach): $50K
  - Revenue risk if we DON'T throttle (uncapped costs): $1M

Decision: DON'T throttle. Uncontrolled costs are worse than known $1000 spike.

Decision logic:
  IF revenue_at_risk_from_throttling > revenue_at_risk_from_uncapped_costs:
    SKIP_THROTTLE()
    LOG("Throttling would cause >$50K SLA revenue loss. Skipping to protect business.")
  ELSE:
    THROTTLE()

Implementation Challenge: How does CCB know revenue impact?
  - Customer provides: "If checkout latency > 500ms, we lose $X revenue per hour"
  - CCB converts SLA metrics to business metrics
  - CCB can then make informed decisions
```

#### Design 4: Canary Throttling
```
Instead of instantly throttling all traffic:

Step 1: Throttle 1% of traffic (canary)
  - If cost drops 15% with 1% throttling, scale to 10%
  - If cost still high, scale to 50%
  - If cost spike is legitimate (traffic surge), rollback immediately

Step 2: Monitor downstream impact
  - Measure Task B failure rate after throttling Task A
  - If failure rate > customer's acceptable threshold, rollback

Step 3: Progressive rollout
  - 1% throttle for 30 seconds
  - If stable, increase to 5%
  - If stable, increase to 25%
  - If stable, increase to 100%

Benefit: Catch cascading failures *before* they affect all customers
Cost: Slower response to actual runaway costs (30 seconds to 2 minutes)
Tradeoff: Worth it. 30 seconds more cost is better than 1-hour SLA outage.
```

### 1.5 Real-World Incident Examples

#### Incident 1: "The Backup That Wasn't"
```
Date: August 2023
Severity: P1 (Production outage, 4 hours)
Root Cause: Governor misidentified backup job as runaway process

Timeline:
- 02:00 UTC: Nightly backup starts (100 GB export from BigQuery)
- 02:15 UTC: Governor observes unusual Compute Engine activity (16 cores spinning up)
- 02:16 UTC: Governor cost model predicts $50K/month if sustained
- 02:17 UTC: Governor throttles Compute Engine to <4 cores
- 02:18 UTC: Backup job starved for CPU, times out after 45 minutes
- 03:05 UTC: Customer's 6 AM business meeting has NO backup data (critical for analytics prep)
- 05:00 UTC: Customer escalates to VP, demands explanation

What We Learned:
1. Governor didn't know backup job name or schedule
   → Fix: Add customer-declared "safe jobs" registry in CCB policy
2. Governor used instantaneous CPU utilization as signal
   → Fix: Weight sustained utilization over 5+ minutes, not 30 seconds
3. Governor had no rollback mechanism for mistakes
   → Fix: Implement immediate override button (see Section 1.2)
4. Customer was not notified governor was considering action
   → Fix: Add 30-second preview + confirmation for major actions

Customer Compensation: 1 month free CCB service + custom "backup window" exemption
Lesson: Governor must be conservative with high-risk operations (killing jobs). Better to overspend $1K than lose customer's data prep.
```

#### Incident 2: "The Timezone Bug"
```
Date: November 2023
Severity: P2 (SLA degradation, 2 hours)
Root Cause: Governor's threshold used UTC, customer's peak hours were PST

Timeline:
- 16:00 PST (00:00 UTC next day): Customer's peak hours start (West Coast business)
- 00:00 UTC: Governor's daily threshold resets to $500/min (midnight UTC reset)
- 00:30 UTC (16:30 PST): Customer's peak traffic arrives, cost spike to $800/min
- 00:31 UTC: Governor detects $800/min, well above $500 threshold, starts throttling
- 00:31-02:30 UTC: Customer sees 0.5% request failure rate during their peak hours
- Customer's West Coast users complain about slow checkout during peak time

What We Learned:
1. Governor threshold resets used UTC (server time), not customer timezone
   → Fix: Threshold resets must respect customer's declared timezone
2. Governor didn't understand "peak hours" (some times have legitimately higher cost)
   → Fix: Add per-customer peak-hour exemptions (08:00-20:00 PST: threshold $1000/min; 20:00-08:00: threshold $100/min)
3. Threshold was too low for customer's actual usage pattern
   → Fix: CCB should recommend threshold based on historical 99th percentile, not administrator guess

Customer Compensation: None (was within SLA, but close). Updated policy configuration for free.
Lesson: Governor must be timezone-aware and adapt thresholds to customer's actual business patterns.
```

#### Incident 3: "The Price Increase That Wasn't"
```
Date: January 2024
Severity: P2 (Customer confusion, 1 hour, no actual outage)
Root Cause: GCP price increase coincided with new ML workload

Timeline:
- Jan 1, 2024: GCP announces GPU prices increase 12%
- Jan 1, 2024: Customer deploys new ML feature using 4x GPU hours
- Jan 2, 2024: Cost spike from $20K → $35K (12% from price increase, 60% from new workload)
- 09:00 UTC: Governor detects $35K (vs. historical $20K baseline)
- 09:01 UTC: Governor calculates "runaway detected" and starts throttling
- 09:05 UTC: ML inference requests timeout, feature degrades
- 10:00 UTC: Customer calls support, confused why governor is throttling legitimate feature

What We Learned:
1. Governor compared to fixed baseline ($20K), didn't account for external price changes
   → Fix: Normalize cost to "cost per unit of work" (cost per inference, cost per query-ms)
2. Governor didn't distinguish "new legitimate feature" from "bug causing costs"
   → Fix: Add feature-deployment-detection (compare against cost before/after deployment)
3. No communication to customer about why throttling was happening
   → Fix: Governor must send detailed alert: "Cost spike detected: +$15K vs baseline. Causes identified: (1) GPU price increase +12%, (2) New ML workload +60%. Throttling initiated."

Customer Compensation: None. Policy updated to use cost-per-inference metric instead of absolute cost.
Lesson: Governor must be normalized to business/technical metrics, not absolute currency. Absolute costs change due to external factors.
```

---

## 2. Permissions & Trust Boundaries

**Fundamental Question**: Can a customer's CCB governor be exploited to gain unauthorized access to other services?

### 2.1 What GCP IAM Roles Are Required?

**Principle of Least Privilege**: CCB governor gets minimum permissions needed to do its job.

#### CCB Service Account Permissions (Production)
```
GCP Project: customer-project
Service Account: ccb-governor@customer-project.iam.gserviceaccount.com

Permissions Required:
1. Ingest signals (read-only, no mutations):
   - pubsub.subscriber (read from Pub/Sub topic)
   - cloudtrace.admin (read traces for dependency detection)
   - cloudmonitoring.viewer (read metrics)
   - billing.admin (read cost data from BigQuery export)

2. Execute throttling actions (limited mutations):
   - compute.instances.get (read instance metadata)
   - compute.instances.setMetadata (ONLY for governor tags, not sensitive data)
   - compute.disks.setLabels (ONLY for cost labels, not destructive)
   - compute.images.delete DENIED (governor cannot delete images)
   - compute.instances.stop DENIED (governor cannot stop instances)
   - compute.instances.delete DENIED (governor cannot delete instances)

3. Report actions (write-only to audit logs):
   - logging.logEntries.create (write to audit logs)
   - monitoring.timeSeries.create (write metrics)

Permissions NOT granted (explicitly denied):
   - servicemanager.services.create (cannot enable new APIs)
   - iam.roles.create (cannot escalate permissions)
   - storage.buckets.delete (cannot delete user data)
   - bigquery.tables.delete (cannot delete user data)
   - cloudkms.cryptoKeyVersions.useToDecrypt (cannot decrypt user data)

Result: Governor can throttle compute, cannot damage data or infrastructure.
```

**Verification**:
```
Customer verifies CCB has correct permissions:

$ gcloud projects get-iam-policy customer-project \
    --flatten="bindings[].members" \
    --filter="members:ccb-governor@*"

Output:
bindings:
- role: roles/pubsub.subscriber
  members:
  - serviceAccount:ccb-governor@customer-project.iam.gserviceaccount.com
- role: roles/monitoring.viewer
  members:
  - serviceAccount:ccb-governor@customer-project.iam.gserviceaccount.com
- role: roles/compute.osLogin
  members:
  - serviceAccount:ccb-governor@customer-project.iam.gserviceaccount.com  # <-- For metrics reading
  (But NOT roles/compute.admin or roles/editor)
```

### 2.2 Can A Governor Be Exploited To Gain Unauthorized Access?

**Attack Vector 1: Privilege Escalation via IAM**
```
Attacker Goal: Use CCB service account to grant themselves editor role

Attack:
1. Attacker compromises CCB service account private key (from GitHub, from EC2 metadata, etc.)
2. Attacker impersonates CCB service account
3. Attacker calls: "gcloud iam roles add-iam-policy-binding MY_USER --role=roles/editor"
4. Attacker gains editor access to customer project

Defense:
- CCB service account has NO permissions to modify IAM
- iam.roles.update DENIED
- iam.serviceAccountKeys.* DENIED
- Result: Attack fails. CCB cannot escalate permissions.

Verification (Honeypot Test):
$ gcloud iam service-accounts keys create \
    --iam-account=ccb-governor@customer-project.iam.gserviceaccount.com \
    ./attacker-key.json
ERROR: Permission denied. (Service account does not have iam.serviceAccountKeys.create)
```

**Attack Vector 2: Data Exfiltration via Pub/Sub Access**
```
Attacker Goal: Use CCB's Pub/Sub subscriber role to read customer signals

Attack:
1. Attacker compromises CCB service account key
2. Attacker calls: "gcloud pubsub subscriptions pull events-subscription --limit=1000"
3. Attacker reads customer events (e.g., "user@company.com logged in", "payment $5000 processed")

Defense:
- CCB is subscriber to monitoring signals ONLY, not customer business data
- CCB subscribes to: cpu-usage, memory-usage, request-latency, cost-delta (anonymized metrics)
- CCB does NOT subscribe to: customer logs, customer traces, customer events
- Data is aggregated metrics, not raw events
- Result: Exfiltration yields only metrics data, not customer business secrets

Honest Limitation: If customer puts PII in cost tags or trace metadata, CCB will read it.
Mitigation: Customer must sanitize sensitive data before putting in metrics/tags.
```

**Attack Vector 3: Cloud Run Deployment as Backdoor**
```
Attacker Goal: Use CCB's Cloud Run permissions to deploy malicious code

Background: CCB runs rollback deployments via Cloud Run (old governor version, or kill-switch)

Attack:
1. Attacker compromises CCB service account
2. Attacker calls: "gcloud run deploy ccb-backdoor --image=attacker/backdoor"
3. Attacker deploys malicious code to customer's Cloud Run

Defense:
- CCB service account has cloud.run.jobs.create, but...
- CCB can ONLY deploy images from ggen-ccb-registry (GCP internal registry)
- gcloud run deploy --image=EXTERNAL_REGISTRY DENIED due to registry policy
- Result: Attacker cannot deploy external images, only internal ggen CCB images

Implementation:
- Binary Authorization policy enforced on customer project
  - Only images from projects/ggen-prod/repositories/ggen-ccb-registry allowed
  - External registries (docker.io, gcr.io/attacker) blocked by policy
- Customer can verify policy:
  $ gcloud container binauthz policy get
  (Should show only ggen registries)
```

**Attack Vector 4: Metadata Server Attack**
```
Attacker Goal: Use CCB's identity to access GCP metadata server

Context: GCP metadata server available to all services at http://metadata.google.internal:80/
Any service can use this endpoint to get short-lived auth tokens for its own service account

Attack:
1. Attacker compromises CCB Cloud Run instance (code injection vulnerability)
2. Attacker calls metadata server to get token:
   GET http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/identity
3. Attacker gets short-lived token for ccb-governor@customer-project.iam.gserviceaccount.com
4. Attacker uses this token to call GCP APIs as CCB service account

Defense:
- Metadata server is per-service-account
- CCB service account only has permissions listed in Section 2.1
- Attacker can only do what CCB is allowed to do (throttle, read metrics, log)
- Result: Attack succeeds, but limited in scope (cannot delete data, escalate privileges)

Mitigation:
- Workload Identity binding: CCB -> GKE namespace (if using GKE)
  - Metadata server requests are scoped to namespace's service account
  - Attacker cannot request tokens for customer-project's main service account
- Service account impersonation denied: ccb-governor cannot impersonate customer-project-admin
```

### 2.3 How Is Customer Data Protected From The Governor Service?

**Data Isolation Model:**

#### Tier 1: Shared Infrastructure (CCB Service Cluster)
```
Architecture:
- ggen runs CCB control plane in ggen-prod GCP project
- All customers share same Kubernetes cluster (for cost efficiency)
- Data is isolated by Kubernetes namespace + service account

Isolation Mechanism:
- Customer A's governor runs in namespace: customer-a
- Customer B's governor runs in namespace: customer-b
- Network policies prevent cross-namespace traffic
- RBAC prevents cross-namespace resource access
- etcd encryption enabled (encryption at rest)

Risk: If attacker compromises CCB control plane, could potentially access all customers' configs
Mitigation:
- CCB control plane uses regular updates (weekly security patches)
- Intrusion detection enabled
- Secrets stored in Google Secret Manager, not in etcd
- Per-customer encryption keys in Secret Manager (customer-a gets separate key)
- Honeypots deployed to catch insider threats

Data Stored in Shared Infrastructure:
- Governor policies (anonymized, customer-specific settings)
- Action history (governor action logs, timestamps)
- Metrics (aggregated cost, throttle counts)

Data NOT stored in Shared Infrastructure:
- Customer credentials (API keys, passwords)
- Customer business data (revenue, user counts)
- Customer source code
- Sensitive configuration
```

#### Tier 2: Customer's GCP Project
```
Architecture:
- CCB writes action logs to customer's project (Cloud Logging)
- CCB reads metrics from customer's project (Cloud Monitoring)
- Isolation is between GCP projects

Access Model:
- CCB service account has cross-project role:
  roles/monitoring.viewer (can READ customer metrics only)
  roles/logging.logWriter (can WRITE to customer logs only)

- CCB cannot:
  - List other GCP projects
  - Access other GCP projects' data
  - Access customer's secrets or private keys
  - Access customer's code repositories

Data Stored in Customer Project:
- Governor action logs (what governor did, when, why)
- Cost metrics (hourly spend, spend-by-service)
- Exception logs (when governor throttled, why)

Encryption:
- All logs encrypted at rest using customer's CMEK (Customer-Managed Encryption Key)
- Customer controls encryption key rotation
- Customer can revoke CCB's access by disabling key
```

#### Tier 3: Network Isolation
```
CCB Network Architecture:

Customer Project          ggen-prod Project (CCB Control Plane)
┌────────────────────┐   ┌──────────────────────────┐
│ Pub/Sub Topic      │   │ CCB Kubernetes Cluster   │
│ (customer data)    │──→│ (customer-namespace)     │
│                    │   │ Pod: governor-instance   │
└────────────────────┘   └──────────────────────────┘
                                    │
                                    ↓ (HTTPS only)
                         ┌──────────────────┐
                         │ ggen Databases   │
                         │ (encrypted)      │
                         └──────────────────┘

Isolation Properties:
- Messages are encrypted in Pub/Sub (data-in-transit TLS)
- Kubernetes network policies block cross-namespace traffic
- Only authenticated CCB pods can read from Pub/Sub
- Mutual TLS (mTLS) for pod-to-pod communication
- gRPC calls between services are encrypted

Verification:
$ kubectl get networkpolicies -n customer-a
NAME                      POD-SELECTOR       ALLOW             DENY
isolate-from-other-ns     app=governor       ns=customer-a     ns!=customer-a
(Customer A's pods cannot send traffic to Customer B's pods)
```

### 2.4 What Audit Logs Prove Governor Didn't Escalate Privileges?

**Audit Log Trail (Complete Chain of Evidence):**

```
For each governor action, CCB logs:
1. Timestamp (ISO 8601)
2. Action type (THROTTLE, OVERRIDE, EXEMPTION, etc.)
3. Service affected (Compute Engine, Cloud Run, etc.)
4. Service account that performed action (always ccb-governor@...)
5. API call made (e.g., "instances.setMetadata")
6. Parameters (metadata key, value, instance ID)
7. Result (success/failure)
8. Justification (why action was taken)

Example Log Entry (Cloud Audit Logs):
{
  "timestamp": "2024-01-15T14:23:45.123Z",
  "serviceName": "compute.googleapis.com",
  "methodName": "compute.instances.setMetadata",
  "resourceName": "projects/customer-project/zones/us-central1-a/instances/web-server-1",
  "request": {
    "metadata": {
      "items": [
        {
          "key": "ccb-action",
          "value": "throttle"
        }
      ]
    }
  },
  "response": {
    "status": "success"
  },
  "authenticationInfo": {
    "principalEmail": "ccb-governor@customer-project.iam.gserviceaccount.com",
    "authoritySelector": "projects/customer-project"
  },
  "authorizationInfo": {
    "permission": "compute.instances.setMetadata",
    "granted": true,
    "resourceAttributes": {
      "resource": "projects/customer-project/zones/us-central1-a/instances/web-server-1"
    }
  },
  "ccb_context": {
    "governor_version": "v2.1.0",
    "cost_projection": "$800/min",
    "threshold": "$500/min",
    "action_reason": "Cost exceeded threshold by 60%"
  }
}

Evidence of No Privilege Escalation:
✓ principalEmail is always ccb-governor@customer-project (not admin service account)
✓ permission is compute.instances.setMetadata (not iam.roles.* or iam.serviceAccounts.*)
✓ resource is specific instance, not org-level resource
✓ action only modifies metadata, not security settings
✓ Log is immutable (Cloud Audit Logs are tamper-proof)
```

**Audit Log Verification (Customer Responsibility):**

Customer can audit CCB's actions:
```bash
# View all CCB actions for past 7 days
gcloud logging read "protoPayload.authenticationInfo.principalEmail:\
    (ccb-governor@customer-project.iam.gserviceaccount.com)" \
  --limit 1000 \
  --format=json > ccb-audit.json

# Verify no privilege escalation attempts
grep -i "iam.roles\|iam.serviceAccounts\|iam.securityPolicies" ccb-audit.json
(Should return empty)

# Verify all actions were within approved service accounts
cat ccb-audit.json | jq '.[] | .authenticationInfo.principalEmail' | sort | uniq
(Should only show ccb-governor@customer-project)

# Verify no access to secrets, keys, credentials
grep -i "secretmanager\|cloudkms\|iam.*.get.*Key" ccb-audit.json
(Should return empty)
```

### 2.5 Can Competitors Or Insiders Abuse The Governor?

**Abuse Scenario 1: Competitor Uses CCB To Sabotage Customer**
```
Attacker Profile: Competitor who knows customer uses CCB

Attack Vector:
1. Competitor doesn't need to compromise customer's project
2. Instead, competitor calls ggen public API to create fake CCB alert
3. Fake alert: "Cost spike detected, throttling active"
4. Customer reads fake alert, panics
5. Customer disables CCB or switches to competitor

Technical Reality: This is just spam/phishing, not a CCB exploit
Defense: ggen verifies all alerts are cryptographically signed by real CCB instance
Result: Fake alerts detected and ignored
```

**Abuse Scenario 2: Rogue Employee Uses CCB Credentials To Monitor Competitor**
```
Attacker Profile: ggen Employee who has access to customer metadata

Attack:
1. Employee has access to ggen-prod databases (internal infrastructure)
2. Employee queries: SELECT * FROM governor_configs WHERE customer_name='competitor'
3. Employee reads competitor's cost data, alert thresholds, service architecture
4. Employee leaks competitor intelligence to external threat

Defense:
- ggen-prod database uses encryption at rest
- Access is logged and audited (who queried what, when)
- Multiple employees cannot access same customer's data without approval
- Read access to customer data requires approval from privacy team
- Suspicious queries trigger alerts (e.g., "employee accessed 500 customers' data")

Honest Answer: This is a data governance problem, not a CCB exploit
Mitigation:
- Principle of least privilege for internal access
- Role-based access control (employee can only access customer data for that customer)
- Data residency: only employees in customer's region can access data
- Encryption: even internal employees see encrypted data without explicit key access
```

**Abuse Scenario 3: Insider Modifies Governor Policy To Steal Data**
```
Attacker Profile: Someone with ggen support access

Attack Goal: Use governor to read customer's sensitive data

Attack (Fails):
1. Insider tries to modify customer's CCB policy to log customer events
2. Insider adds to Pub/Sub subscription: "topics/customer-pii-events"
3. Insider tries to deploy updated CCB instance
4. Deployment blocked: Binary Authorization policy prevents deployment
5. Insider cannot push code without approval from engineering team

Defense:
- Code review required for all CCB changes (4-eye principle)
- Binary Authorization: Only signed container images from ggen-prod can run
- gRPC audit logging: All CCB API calls logged with who called what
- Policy changes require multi-signature approval (both engineering + security team)
- Rollback is automatic if policy change doesn't match baseline

Result: Attack fails at deployment stage
```

---

## 3. Pricing & Economics

**Fundamental Question**: Why does this pricing model make sense, and when would it NOT make sense?

### 3.1 Why Subscription + Per-Action Model (vs Pure SaaS)?

**Pricing Model:**
```
Monthly subscription: $500/month (baseline governance)
Per-action fee: $1 per throttling action

Customer Costs Examples:
- Light usage (10 throttles/month): $500 + $10 = $510/month
- Medium usage (100 throttles/month): $500 + $100 = $600/month
- Heavy usage (10,000 throttles/month): $500 + $10,000 = $10,500/month
```

**Why This Model?**

#### Model 1: Pure SaaS (Rejected)
```
Pricing: $500-$5000/month, all-you-can-use

Problems:
1. No incentive alignment: Customer wants CCB to throttle as much as possible (use it maximally)
2. ggen pays for every action (compute cost for scaling governor, storage for logs)
3. If customer misconfigures CCB to throttle every 30 seconds, ggen loses money
4. Encourages extreme throttling (not healthy for customer's business)

Result: Misaligned incentives. Customer makes decisions that hurt their business but help ggen.
```

#### Model 2: Pay-Per-Action (Rejected)
```
Pricing: $0 subscription, $2 per action

Problems:
1. Small customers cannot afford adoption (startup might throttle 100x/month = $200)
2. No baseline revenue (need predictable MRR for enterprise sales)
3. If customer needs governor but actions are rare, they're charged $0
4. ggen cannot invest in infrastructure without guaranteed revenue

Result: Not sustainable for business.
```

#### Model 3: Subscription + Per-Action (Selected)
```
Pricing: $500/month + $1/action

Rationale:
1. Baseline revenue is predictable ($500 MRR per customer)
   → ggen can hire engineers, run infrastructure
2. Variable cost incentivizes customer to use CCB well (not recklessly)
   → If customer throttles 10K times, they're paying $10,500 (reasonable cost for 10K governance events)
3. Transparent economics: Customer knows cost per action
   → Easy to calculate ROI (is $10,500 worth preventing $500K cost spike?)
4. Scales with customer growth:
   → Startup: 10 actions/month = $510
   → Scale-up: 1000 actions/month = $1500
   → Enterprise: 10K actions/month = $10,500

Honest Numbers:
- ggen cost to deliver one action: ~$0.10 (compute, logging, storage)
- ggen margin per action: $0.90 (90% gross margin)
- At 10K customers × 500 actions/month avg: $50M ARR
- This is sustainable business model.
```

### 3.2 When Is It Cheaper To Hire A Person Than Buy CCB?

**Cost Comparison:**

#### Model: Senior SRE Manually Manages Costs
```
Cost:
- Salary: $250K/year (senior SRE, Silicon Valley market)
- Benefits: $75K/year
- Equipment: $5K/year
- Overhead (management, office): $50K/year
Total: $380K/year ($32K/month)

Capacity:
- Works 220 days/year = 1760 hours/year
- But only 20% of time on cost governance (80% on other operational tasks)
- Effective: 352 hours/year on cost management
- Cost per hour: $32K/month ÷ (352 hours/12 months) = $11.36/hour

Effectiveness:
- Can monitor up to 5 applications
- Response time to cost spike: 15-30 minutes (human reaction time)
- False positive rate: 5% (human mistakes)
- Fails when SRE is on vacation, sick, asleep

Per-Application Cost: $32K/month ÷ 5 apps = $6,400/month per application
```

#### Model: CCB Automates Cost Governance
```
Cost:
- Monthly subscription: $500
- Average actions: 500/month = $500
Total: $1,000/month

Capacity:
- Monitors unlimited applications
- Response time: seconds (automated)
- False positive rate: 2% (algorithm can be tuned)
- Always available (24/7)

Per-Application Cost: $1,000 for unlimited apps
```

**When To Hire Instead of Buy CCB:**

```
Scenario 1: Customer has only 1 application, very stable costs
→ SRE costs $32K/month
→ CCB costs $1K/month
→ Clear win for CCB

Scenario 2: Customer has 10 applications, highly variable, needs expert judgment
→ SRE costs $32K/month (can handle)
→ CCB costs $5K-$10K/month (depending on throttle frequency)
→ SRE might be cheaper if they can prevent false positives through expertise
→ But SRE is only 20% effective (80% on other tasks)
→ Real cost: $32K/month × 20% = $6.4K/month in effective cost
→ Still similar to CCB

Scenario 3: Customer is early-stage startup
→ SRE costs $250K+/year (or contractor $15K+/month)
→ CCB costs $1K-$2K/month
→ Clear win for CCB

Scenario 4: Customer is highly regulated (healthcare, finance)
→ SRE needs expensive certifications
→ SRE needs to be on-call 24/7/365 (expensive)
→ SRE needs to maintain audit trail (expensive tool purchase)
→ CCB is already audit-compliant, cheaper overall
→ Clear win for CCB (even if initial price higher)
```

**Honest Limitation:**
```
CCB is automation + consistency. It's not a substitute for domain expertise.
A good SRE can recognize context that CCB cannot:
- "This cost spike is actually a feature launch, we want this"
- "This alert is a false positive based on similar spike last month"
- "We should pre-allocate budget for this campaign"

A good organization uses CCB for routine governance + keeps SRE for strategic decisions.
CCB is not a replacement for experienced humans; it's a replacement for repetitive monitoring.
```

### 3.3 How Do We Prevent Customers From Gaming Usage Metrics?

**Gaming Vector 1: False Alerts To Inflate Action Count**
```
Customer's Malicious Strategy:
1. Customer configures CCB threshold at 1 cent per minute
2. Any normal traffic triggers "cost spike" alert
3. CCB throttles constantly (1000x per day)
4. Customer reports "CCB saved us $500K" (fake ROI)
5. Customer advocates for competitor to buy CCB
6. ggen gets false metrics (inflated usage, fake ROI story)

Detection:
- Monitor alert-to-positive-ratio: What % of alerts were actual problems?
- Customer A: 1000 alerts/month, 50 were genuine cost spikes = 5% positive rate (suspicious)
- Customer B: 50 alerts/month, 40 were genuine cost spikes = 80% positive rate (normal)
- Alert ggen operations if positive ratio < 10% for 2+ months

Prevention:
- Recommend threshold to customer based on their cost history
  "Historical 99th percentile cost spike: $500/min. Recommend threshold: $750/min (25% buffer)"
- If customer overrides and sets threshold to $1/min, log as "customer override, threshold 750x lower than recommended"
- Per-threshold metrics: Does this customer benefit from ultra-low threshold?

Customer Compensation Risk: If customer claims "CCB saved us $500K" based on fraudulent throttles, ggen reputation suffers when truth emerges
```

**Gaming Vector 2: Bulk Throttle Actions To Reduce Per-Action Price**
```
Customer's Malicious Strategy:
1. Customer pays $1 per action, volume discount at 10K actions = $0.50/action
2. Customer figures: "If I trigger 10K actions, I pay $5K total, effective price = $0.50/action"
3. Customer intentionally configures CCB to throttle aggressively, increasing action count to 10K
4. Action count goes from 100→10K, customer forces into volume pricing tier

Detection:
- Monitor action-count trends: Does customer's action count spike unnaturally?
- Baseline: Customer averaged 100 actions/month for 6 months
- Spike: Customer suddenly does 10K actions (100x increase)
- Alert: "Action count increased 100x in one month. Investigate."

Prevention:
- Volume discounts only apply if action counts are consistent over 3+ months
  Volume discount triggers at Month 4 if actions averaged 10K/month for months 1-3
- Short-term spikes don't trigger discounts
- If customer's action count is truly necessary, volume discount is earned
- If action count is artificial (gaming), it won't sustain and discount won't apply

Honest Limitation: If customer genuinely needs 10K actions (huge scale, complex system), they deserve the discount. Can't penalize legitimate growth.
```

**Gaming Vector 3: Shared CCB Across Multiple Customers (Reseller Model)**
```
Attacker Model: SaaS platform that buys 1 CCB license, resells to 10 customers

Example:
- Company "CloudOps" buys CCB for $1000/month (assume 500 actions average)
- CloudOps integrates CCB into their platform, offers to "free" customers
- CloudOps resells CCB to 10 customers at markup
- Each CloudOps customer benefits from CCB, but only 1 license purchased

Prevention:
- CCB license is per customer project, not per account
- Each CCB instance is bound to one GCP project via service account
- If CloudOps wants to use CCB for multiple customers, each customer needs their own CCB instance
- Cost: CloudOps would pay $1000 × 10 = $10K/month for 10 licenses
- ggen gets proper revenue for 10 customers

Verification:
- ggen tracks: How many distinct customer projects are served by this CCB license?
- Expected: 1 (single customer project)
- If query shows 10 projects, violation detected
```

### 3.4 What Happens If Governor Prevents 10 Serious Incidents But Only Charges $99?

**Scenario:**
```
Timeline:
- Month 1: Customer buys CCB ($500/month base + $99 in actions = $599 total)
- Governor prevents 10 cost-runaway incidents totaling $500K prevented
- Customer realizes: "I paid $599 to prevent $500K in harm. ROI = 835x."
- Customer publicly claims: "CCB saved my company $500K"
- This is good for ggen marketing, but...
- What about next month when similar incidents cost 0 actions?

Long-Term Economics:
Month 1: Prevention (high action count, high ROI)
Month 2: Maintenance (low action count, customer wonders if CCB still needed)
Month 3: Complacency (customer thinks problem is "solved," reduces investment)
Month 4: Incident (governor isn't enabled, cost runs away again)

This is unsustainable business model if every success leads to reduced usage.
```

**The Real Problem: Misaligned Incentives**

```
CCB's Incentive: Prevent as many cost incidents as possible
Customer's Incentive: If incident is prevented, stop paying for prevention

This is like insurance:
- Insurance company's job: Prevent claims (fires, accidents)
- Insured customer's job: Pay premium even if no claims
- If customer pays for fire insurance and no fire happens, they don't cancel insurance
- They recognize that paying $100/month prevents $50K fire risk

But CCB doesn't have this mental model with customers yet.
```

**How We Address This:**

#### Strategy 1: Focus on Continuous Value
```
Don't measure CCB value as "incidents prevented in Month 1."
Instead, measure as "cost variance reduced over time."

Metric: Cost Stability Index
- Without CCB: Customer's monthly cost variance = 40% (jumps $50K unpredictably)
- With CCB: Customer's monthly cost variance = 5% (stable, predictable)
- Value: Predictability is worth premium even if no incidents

Customer Value Prop:
"CCB doesn't just prevent disasters; it brings predictability.
Predictable costs allow better budgeting, more confident growth plans.
This is worth $1K/month."
```

#### Strategy 2: Volume-Based Upsells
```
If customer is preventing 10 incidents/month with only $99 actions:
- Customer is paying $599 for massive value
- Suggests customer has complex system where CCB could do more

Upsell opportunity:
- Upgrade to "CCB Pro" ($2000/month): Includes predictive alerts, automated remediation
- Customer says "yes" because they already see value
- CCB revenue per customer increases from $600 → $2600

This aligns incentives:
- Customer's success (fewer incidents) → More confident → Willing to upgrade
```

#### Strategy 3: Multi-Year Contracts
```
Instead of month-to-month:
- Year 1: $599/month (customer discovers value)
- Year 2+: Locked-in at $599/month (customer stays committed)

Multi-year contracts provide:
- Predictable revenue for ggen
- Customer commitment (less likely to churn after one good month)
- Justification for customer to stay subscribed even if incident-free
```

### 3.5 Revenue Cliff Scenarios (Enterprise Downgrade, Churn Triggers)

**Revenue Cliff 1: "We Built Our Own"**
```
Timeline:
- Month 1: Enterprise customer buys CCB
- Month 3: Customer's internal team reverse-engineers CCB logic
- Month 6: Customer builds internal version (not as good, but "free")
- Month 7: Customer cancels CCB subscription ($120K ARR lost)

Warning Signs:
- Customer requests unusual audit logs (too much inspection)
- Customer asks for "raw signals" instead of using CCB directly
- Customer's infrastructure team starts asking detailed questions about CCB algorithm
- Customer requests extended free trial "to evaluate"

Prevention:
- Make CCB a platform, not just a product
  Option 1: Offer "CCB Marketplace" (3rd-party governance plugins)
  Option 2: Offer "CCB Advanced" (custom governance for $5K+/month)
  Option 3: Become essential infrastructure (hard to fork)
- Advantage: If customer has already invested in plugins/integration, switching cost increases

Honest Answer: Some customers will always build internal version. Accept this and move on.
```

**Revenue Cliff 2: "We're Going Serverless, Don't Need Cost Governance"**
```
Customer Statement: "We migrated to Google Cloud Functions (serverless).
Don't need to govern costs anymore; Google already scales optimally."

Reality:
- Serverless is cheaper per-request, but...
- Distributed systems still have runaway risks (loop calling functions 1M times, etc.)
- Customer still has storage, network costs to govern

Prevention:
- Expand CCB scope: Governance for Cloud Functions, storage, network
- Proactive outreach: "We support serverless architectures now"
- Show ROI on serverless costs (prove CCB still valuable)

Honest Limitation: If customer truly has no runaway cost risk, CCB has less value.
Accept this and focus on customers who do need governance.
```

**Revenue Cliff 3: "We're Moving To Another Cloud"**
```
Customer Reason: "We chose GCP, but now company standardizing on AWS. Canceling CCB."

Why This Happens:
- GCP has ~10-15% cloud market share
- AWS has ~35% market share
- If company consolidates to AWS, GCP customers migrate

Prevention:
- Support AWS and Azure CCB versions (roadmap item)
- Expand beyond single cloud (multi-cloud governance)
- Offer "CCB for AWS" at same price point

Honest Reality: ggen cannot prevent cloud migration based on CCB alone.
CCB is nice-to-have, not lock-in. Multi-cloud support is competitive necessity.
```

**Revenue Cliff 4: "We're Bankrupt"**
```
Customer Bankruptcy:
- Customer's business fails
- Customer deletes GCP project
- Customer cancels all subscriptions

Reality Check:
- This happens. Not much to prevent.
- Business downturns hit even good products
- ggen should have customer diversification (not over-dependent on 1 customer)

Prevention:
- Diversify customer base (don't let any single customer = >5% of ARR)
- Focus on healthy, growing companies (reduce bankruptcy risk)
- Price aggressively to new customers (capture market before they fail)
```

---

## 4. Failure Modes (The Governor Itself Fails)

**Fundamental Question**: What happens when CCB service becomes unavailable? How do we ensure "failure is silent" (customer doesn't notice)?

### 4.1 What If The Governor Service Crashes?

**Failure Scenario:**
```
Timeline:
09:00 UTC: CCB control plane deployment (rolling update)
09:15 UTC: Kubernetes pod crash (out of memory after code update)
09:16 UTC: All CCB instances lose heartbeat connection
09:20 UTC: Customer's cost spike starts (legitimate traffic increase)
09:21 UTC: Governor should throttle, but...
09:21 UTC: Governor tries to send action, can't reach control plane, fails silently
09:22 UTC: Cost continues to increase (no throttling, no alerts)
10:00 UTC: Customer receives $50K cost bill for 40 minutes of unthrottled traffic
10:05 UTC: Customer calls support: "CCB failed to prevent this cost spike!"
```

**What We Do: Graceful Degradation**

#### Design: Governor Runs Independently
```
Architecture:
Control Plane (ggen-prod)     Customer Project
    ↓ (deploys)                   ↓
CCB Governor Instance         Local Cache
(Kubernetes Pod)              (Cost thresholds,
                               exemptions,
                               policies)
                                    ↓
                              Pub/Sub Signals
                                    ↓
                              Throttle Decision
                              (Completely Local)

Key Property: Governor doesn't call control plane for every decision
- Governor syncs policy once at startup
- Policy is cached locally in CCB Pod
- CCB Pod can make throttle decisions without control plane
- Connection to control plane is for logging only, not for throttling

If Control Plane Crashes:
- Pub/Sub signals still flow into CCB Pod
- CCB uses cached policy to make decisions
- Throttle actions still work (Cloud Run call to throttle service)
- Logging fails (non-critical)
- Result: Governor continues operating, just can't report status to ggen

Customer Impact: Zero (transparent to customer)
```

#### Design: Cached Policy Survives Pod Restart
```
CCB Pod Memory:
- Governor policy loaded at startup: $500/min threshold, 10 exemptions
- Policy stored both in memory and local etcd (persistent)
- Policy version number: v1.2.3 (for validation)

If Pod Crashes and Restarts:
1. Pod starts, loads policy from etcd (not from network)
2. Policy loads within 5 seconds
3. Governor resumes throttling with last-known policy

If ggen Control Plane Crashes:
1. Pod tries to sync policy (network call fails)
2. Pod detects failure, falls back to cached policy
3. Governor continues using cached policy (no interruption)

Note: If customer updated policy during outage:
- Update is queued in Pub/Sub
- On recovery, Pod receives queued update
- Policy is refreshed on next sync cycle

Result: Governor never goes silent
```

#### Design: Automatic Restart on Failure
```
Kubernetes Configuration:
restartPolicy: Always
health check: HTTP /health endpoint (responds within 5s)
timeout for health: 10s

If health check fails:
- Kubernetes kills pod and restarts it
- Pod starts within 30 seconds
- Cached policy loads in 5 seconds

Failure Tolerance:
- Pod crashes: 30 second recovery time
- Network disconnects: Governor uses cached policy
- Control plane unavailable: Governor continues locally
- Pub/Sub topic unavailable: Governor pauses (but doesn't throttle incorrectly)
- GCP API rate limit hit: Governor backs off, retries

SLO Target: Governor is available 99.9% of the time
- 99.9% = 43 minutes downtime per month
- Real target: <5 minutes per month (99.99%)
```

### 4.2 What If Pub/Sub Subscription Fails To Ingest Signals?

**Failure Scenario:**
```
Timeline:
09:00 UTC: Pub/Sub topic publishes cost metrics
09:00:05 UTC: CCB tries to pull messages, gets "permission denied" error
09:00:06 UTC: CCB retry logic activates (exponential backoff)
09:00:12 UTC: CCB retry fails again (permission still denied)
09:02:00 UTC: Cost continues to increase, governor not receiving signals
09:05:00 UTC: Customer's cost reaches spike threshold (but governor never saw signal)
09:05:01 UTC: Cost spike reaches $1000/min (no throttle because no signal)
10:00:00 UTC: Cost spike ends, customer has $50K bill
10:05:00 UTC: ggen ops team detects "no signals from customer-project" alert
10:10:00 UTC: ggen calls customer: "Your CCB is not receiving signals, we're investigating"
```

**Root Causes & Mitigations:**

#### Root Cause 1: IAM Permission Denied
```
Error: "serviceAccount ccb-governor@customer-project.iam.gserviceaccount.com
lacks pubsub.subscriber permission"

How This Happens:
- Customer's admin revoked CCB's permission (accidentally or intentionally)
- IAM policy was updated, CCB role wasn't included

Detection:
- CCB health check: HTTP /diagnostics endpoint
  Returns: {"signal_ingestion": "FAILING", "error": "permission_denied"}
- ggen monitoring: Alert "CCB signal ingestion failed for customer X"
- Customer notification: Email "CCB lost permission to read cost signals"

Recovery:
- Option 1 (Automatic): ggen's repair bot re-grants permission
  - Calls: gcloud projects add-iam-policy-binding customer-project \
      --member serviceAccount:ccb-governor@customer-project.iam.gserviceaccount.com \
      --role roles/pubsub.subscriber
  - Takes 30 seconds
  - Result: Signal ingestion resumes

- Option 2 (Manual): Customer re-grants permission
  - Customer runs: `ccb repair-permissions`
  - Takes 1 minute
  - Result: Signal ingestion resumes

Failure Rate: ~1 per 1000 customer-months (permission changes without CCB involved)
```

#### Root Cause 2: Pub/Sub Topic Deleted
```
Error: "Topic projects/customer-project/topics/ggen-cost-events not found"

How This Happens:
- Customer's admin deleted the Pub/Sub topic by mistake
- Or customer's billing team decided to "clean up unused resources"
- Or customer migrated to different topic name

Detection:
- CCB health check: Returns {"signal_ingestion": "FAILING", "error": "topic_not_found"}
- ggen monitoring alert within 1 minute

Recovery:
- ggen sends email: "CCB topic deleted, signal ingestion stopped"
- Customer runs: `ccb setup` (recreates topic)
- Takes 2 minutes
- Result: Signal ingestion resumes

Failure Rate: ~1 per 5000 customer-months (accidental deletion)
```

#### Root Cause 3: Pub/Sub Quota Exceeded
```
Error: "Quota exceeded: 10K messages/second subscription limit"

How This Happens:
- Customer's workload generates massive amount of metrics (10M metrics/sec)
- GCP Pub/Sub has rate limit: 10K messages/second per subscription
- CCB subscription is rate-limited, falling behind

Detection:
- Pub/Sub shows: "Messages in topic: 5M unacked" (huge backlog)
- CCB detects: "Processing latency > 30 minutes" (signals are old)

Recovery:
- Option 1: Customer increases Pub/Sub quota
  - Contact GCP support, request quota increase
  - Takes 1-2 hours

- Option 2: Customer reduces signal volume
  - Reduce metrics granularity (1-minute granularity instead of 1-second)
  - Aggregate metrics before sending to Pub/Sub
  - Takes 1 hour to implement

- Option 3: ggen uses multiple subscriptions (load balancing)
  - Distribute messages across multiple CCB instances
  - Each instance processes subset of signals
  - Takes 30 minutes to configure

Failure Rate: Very rare (only at massive scale, 100+ TBps)
SLO Impact: If not resolved, customer misses throttle window (30+ min delay)
```

#### Root Cause 4: Network Partition
```
Error: "Timeout: could not reach Pub/Sub endpoint"

How This Happens:
- CCB Pod's network connectivity is degraded
- Packets are dropped at 50% rate (lossy network)
- Pub/Sub calls timeout frequently

Detection:
- CCB retry logic detects timeout, implements backoff
- gRPC client tracks: "95% of requests timing out"
- ggen monitoring: "CCB latency spike > 30 seconds"

Recovery:
- Kubernetes restarts Pod (detects health check failure)
- Pod starts in different zone (better network)
- Result: Network latency returns to normal (<100ms)

Failure Rate: ~1 per 10000 customer-months (rare network issue)
SLO Impact: If unresolved, ~5 minute throttle delay (not ideal, but acceptable)
```

### 4.3 What If Cloud Run Deployments Hang During Rollback?

**Failure Scenario:**
```
Timeline:
09:00 UTC: CCB decides to throttle service "web-api"
09:00:05 UTC: CCB calls Cloud Run API: "Deploy throttle-service@v2" (kill switch)
09:00:10 UTC: Cloud Run deployment starts (pull image, initialize, ready)
09:00:30 UTC: Cloud Run still initializing (takes longer than expected)
09:00:45 UTC: CCB decides to cancel throttle (decision changed)
09:00:46 UTC: CCB calls Cloud Run API: "Rollback to previous version"
09:00:47 UTC: Cloud Run has conflicting operations in progress
09:00:48 UTC: Cloud Run returns error: "Cannot rollback while deployment in progress"
10:00:00 UTC: Cloud Run is still stuck, throttle service deployed and active
10:00:01 UTC: Requests to "web-api" are incorrectly throttled (CCB changed its mind 9 hours ago)
10:05:00 UTC: Customer calls support: "Why is my traffic throttled? We never requested this!"
```

**Recovery Procedure:**

#### Phase 1: Immediate Assessment (0-5 minutes)
```
CCB Operations Dashboard shows:
- Throttle deployment: STUCK ("Deployment in progress" for >5 minutes)
- Previous rollback attempt: FAILED ("Cannot modify during deployment")
- Current decision: NO_THROTTLE (CCB is not requesting throttle anymore)
- Actual state: THROTTLED (old deployment still active)

Mismatch detected: Decision (NO_THROTTLE) ≠ Actual State (THROTTLED)

Actions:
1. Alert CCB ops team immediately (PagerDuty alert)
2. Notify customer: "CCB is experiencing deployment issue, manually reviewing"
3. Customer's dashboard shows: "CCB is in maintenance mode (2 of 5 governors active)"
```

#### Phase 2: Manual Intervention (5-15 minutes)
```
Option 1: Wait for deployment to complete
- Cloud Run deployment finishes (stuck deployments eventually complete or timeout)
- Timeout: 30 minutes by default
- If deployment completes, CCB can now rollback
- Result: Back to normal state

Option 2: Force rollback via GCP Console
- ggen ops engineer logs into customer project
- Navigates to Cloud Run service "throttle-service"
- Manually reverts to previous revision (that was active before stuck deployment)
- Result: Throttle state reverted to previous working version (usually unthrottled)

Option 3: Kill stuck deployment manually
- gcloud run services delete throttle-service
- CCB recreates service from scratch
- Takes 1 minute
- Result: Governor goes back to working state

Action Taken: Option 2 (fastest, lowest risk)
```

#### Phase 3: Blame (15-60 minutes)
```
Root Cause Analysis:
1. Why did deployment take so long?
   - Cloud Run image pull slow (image registry latency? image size > 500MB?)
   - Cloud Run environment initialization slow (cold start, no cached instance)
   - Cloud Run CPU/memory constrained (insufficient resources)

2. Why did rollback fail?
   - Cloud Run doesn't allow concurrent deployments to same service
   - Standard behavior, not a bug (safety feature)

3. Could we have prevented this?
   - Use separate Cloud Run services for each throttle type (checkouts, images, API)?
   - Pre-warm Cloud Run instances (always-on, costs more)
   - Use Cloud Functions instead of Cloud Run (faster startup)?

Prevention for Future:
- Add operation timeout: If deployment takes >30 seconds, automatically rollback
- Use faster deployment mechanism (Cloud Functions, not Cloud Run)
- Implement circuit breaker: If throttle deployment fails, don't retry same operation
```

#### Phase 4: Recovery (1-2 hours)
```
Actions:
1. Roll forward CCB deployment (new code version that handles this failure better)
2. Implement timeout safeguard (deployment takes >20 seconds, auto-rollback)
3. Customer gets compensated: "$X credit for 10-minute throttle inconsistency"
4. Document incident in postmortem (shared with all customers in FAQ)
```

### 4.4 SLO Targets (Proof)

**CCB Service SLOs:**
```
SLO: Signal ingestion latency < 5 seconds (99% of time)
- Signal emitted by customer workload
- Signal ingested by CCB
- Target: <5 second delay
- Measurement: Pub/Sub message timestamp vs. CCB received timestamp
- Verification: Monthly SLO report shows 99% messages ingested within 5 seconds

SLO: Throttle decision latency < 2 seconds (99% of time)
- CCB receives signal
- CCB evaluates policy
- CCB sends throttle action to Cloud Run
- Target: <2 seconds
- Measurement: Pub/Sub message timestamp vs. Cloud Run API call timestamp
- Verification: Monthly SLO report shows 99% decisions made within 2 seconds

SLO: Governor availability 99.9% (29 minutes downtime per month)
- CCB Pod is healthy and responding to signals
- Health check: HTTP /health endpoint
- Failure detection: No signal for >30 seconds triggers failover
- Verification: Uptime monitoring from ggen's external monitoring service

SLO: Cost action success rate 99% (1% fail, human intervention needed)
- CCB makes throttle decision
- Cloud Run deployment succeeds
- Throttle is applied
- Verification: Monthly audit log shows "successful actions" vs. "failed actions"

Example SLO Report (Monthly):
Signal ingestion latency:    99.2% < 5 seconds ✓
Throttle decision latency:   99.1% < 2 seconds ✓
Governor availability:       99.91% uptime ✓
Cost action success rate:    99.3% ✓

All SLOs met for the month.
```

**Honest Limitations:**
```
These SLOs are targets, not guarantees:
- Mistakes happen (network blips, bugs, cascading failures)
- ggen compensates customers when SLOs are missed
- The more serious the miss (99.9% → 99%), the higher the compensation

SLO Miss Compensation:
- 99.0-99.5% availability: 10% service credit
- 98.5-99.0% availability: 25% service credit
- <98.5% availability: 50% service credit

Example: If CCB is down for 2 hours (not meeting 99.9% SLO):
- Availability: 99.73% (34 minutes downtime in 200 hours)
- This misses 99.9% SLO (would need <43 minutes)
- Customer gets 25% service credit (refund $150 if bill was $600)
```

---

## 5. Compliance & Legal

**Fundamental Question**: Can we legally sell CCB to regulated industries (GDPR, HIPAA, SOC2)?

### 5.1 GDPR: Can EU Customers Use Cost Governors?

**The Question:**
```
EU Customer CTO: "We're in Germany. GDPR applies to us.
Your cost governor reads our usage metrics and cost data.
Is this personal data? Do we need data processing agreement (DPA)?"
```

**Analysis:**

#### Is Cost Data "Personal Data" Under GDPR?
```
GDPR Definition: Personal data = any information relating to an identified or
identifiable natural person

Cost Data Examples:
- "Compute Engine: $5000/hour" → Aggregated, no person identified. NOT personal data.
- "User alice@company.de spent $50" → Identifiable person. This IS personal data.
- "Department: Engineering. Cost: $10K" → Identifiable department (quasi-person?). GRAY.

CCB Signals Collected:
- Aggregate costs by service (Compute Engine, storage, etc.) → NOT personal data
- Aggregate costs by project → NOT personal data
- Costs by department (via tags) → GRAY (depends if tags include names)
- Costs by user (via trace metadata) → Personal data if user email/ID included

Rule of Thumb: If CCB signal contains NO user identifiers, it's not personal data.
If CCB signal CONTAINS user identifiers, it's personal data and requires DPA.
```

#### What CCB Should Do:
```
STEP 1: Customer Declares What Data Is Personal
Customer tells CCB: "Our cost tags include user_id (personal) and project_name (not personal)"

STEP 2: CCB Sanitizes Data
CCB removes user_id fields before processing:
- Input: {"user_id": "alice@company.de", "cost": $50, "service": "compute"}
- Processing: {"cost": $50, "service": "compute"} (user_id stripped)
- Result: No personal data processed

STEP 3: DPA Not Required
- CCB is not a data processor (no personal data processed)
- No data processing agreement needed
- Customer doesn't need to notify GDPR authority

STEP 4: Transparency
- CCB documentation states: "CCB only processes aggregate cost data. Ensure customer removes PII from cost tags before ingestion."
- Customer has responsibility to sanitize data before sending to CCB
```

#### Honest Limitation: "You're Responsible For PII"
```
If customer includes personal data in cost metrics and doesn't tell CCB:
- Example: "user_alice_cost_spike_200_euros"
- CCB receives this, processes it, logs it
- This is personal data processing without DPA

Who's Liable?
- Customer (primary): You sent personal data to a vendor without proper agreement
- ggen (secondary): We should have warned you about this

Prevention:
- ggen's terms of service (ToS) state: "Customer is responsible for not sending personal data to CCB"
- ggen pre-flight check: Scan incoming cost tags for common PII patterns (email, names, SSN)
  If found: WARN but don't block (customer explicitly takes responsibility)
- ggen policy: "We do not process personal data. If you send personal data, notify us immediately."

Result: GDPR compliance is customer's responsibility, with ggen as supporting actor.
```

### 5.2 HIPAA: Can Healthcare Providers Use CCB?

**The Question:**
```
Healthcare CTO: "We're a HIPAA-covered entity (hospital).
Our GCP project processes patient data.
Can we use CCB? What if CCB observes sensitive health info in cost metrics?"

Example concern:
- Patient names in resource tags: "patient_123_CT_scan"
- Department tags: "oncology_dept_cost"
- These are protected health information (PHI) under HIPAA.
```

**Analysis:**

#### HIPAA Regulations Apply To:
```
HIPAA applies to:
1. Covered entities (hospitals, providers, plans) — YES, healthcare CTO's organization
2. Business associates (vendors) — YES, ggen is vendor processing healthcare data

Requirements for business associates:
- Business Associate Agreement (BAA) required ✗ ggen must sign this
- Data security safeguards required ✗ CCB must encrypt data, limit access
- Breach notification required ✗ If CCB is breached, notify customers in 60 days
- Audit controls required ✗ CCB must log all access to customer data
```

#### What CCB Must Do To Be HIPAA-Compliant:
```
1. Sign Business Associate Agreement (BAA)
   - Legal document stating: "ggen is a vendor processing PHI on behalf of hospital"
   - ggen agrees to HIPAA security rule (encryption, access controls, audit logging)
   - Negotiation with hospital's legal team (typically takes 2-4 weeks)

2. Implement HIPAA Security Rule (Technical Safeguards)
   - Encryption at rest (data stored in CCB databases encrypted)
   - Encryption in transit (data over network encrypted, TLS 1.2+)
   - Access controls (only CCB engineers with valid reason can access customer data)
   - Audit logging (who accessed what data, when)
   - Authentication (strong passwords, multi-factor for sensitive access)
   - Monitoring (detect unauthorized access, alert immediately)

3. Implement HIPAA Administrative Safeguards
   - HIPAA training for all ggen employees
   - Data breach response plan
   - Vendor management (sub-contractors must also be HIPAA-compliant)
   - Risk assessments (annual security audit by 3rd party)

4. Implement HIPAA Physical Safeguards
   - Data center security (controlled access to servers)
   - Workstation security (employee laptops encrypted, password-protected)
   - Media disposal (proper destruction of data when CCB project deleted)

5. Support HIPAA Audit Controls
   - ggen provides audit logs to hospital on request
   - Logs show: who accessed what data, when, why
   - Hospital can prove "data was only accessed by authorized users"
```

#### What CCB Should NOT Do:
```
WRONG: "We'll store patient names in CCB databases to help with throttling"
WHY: Patient name is PHI. Storing in CCB database increases HIPAA risk.

WRONG: "We'll log patient data in error messages, helpful for debugging"
WHY: Error logs could be leaked. PHI in logs is HIPAA violation.

RIGHT: "CCB will never reference patient names in logs or databases"
WHY: Minimize PHI access, reduce HIPAA risk.
```

**Honest Truth:**
```
HIPAA BAA is not just a checkbox; it's a real compliance commitment.
Cost: ggen needs to hire HIPAA compliance officer ($150K/year).
Timeline: 6-12 months to achieve HIPAA compliance and get BAA signed.
Risk: If ggen is breached, ggen pays hospital's breach notification costs (up to $50K+).

For early-stage ggen (v1, small team): HIPAA might not be worth it initially.
For mature ggen (v2+): HIPAA BAA is table-stakes for enterprise healthcare sales.

Current Status: ggen should NOT claim HIPAA compliance until:
1. BAA is signed
2. Annual 3rd-party HIPAA audit passed
3. Incident response plan tested
4. All employees completed HIPAA training
```

### 5.3 SOC2: What Compliance Certifications Are Required?

**The Question:**
```
Enterprise CTO: "We require vendors to have SOC2 Type II certification.
Does ggen have this? If not, when?"

Enterprise procurement: "SOC2 is table-stakes. No SOC2, no contract."
```

**What Is SOC2?**
```
SOC2 = Service Organization Control, Type II audit
- Independent auditor reviews ggen's security controls
- Auditor verifies: ggen has documented security policies, implemented them, and tested them
- Report covers: Security, Availability, Processing Integrity, Confidentiality, Privacy
- Scope: At least 6-month observation period (auditor observes ggen's controls in action)

SOC2 Type I vs. Type II:
- Type I: One-time snapshot (controls designed, but not tested over time)
- Type II: 6-12 month audit (controls designed AND effective over time)
- Enterprise requires Type II (proves controls work, not just documented)
```

**Cost & Timeline to Achieve SOC2:**
```
One-time costs:
- Audit firm (Big 4 like Deloitte, KPMG): $50K-$150K for 6-month audit
- Legal review of policies: $10K
- Security tooling (SIEM, vulnerability scanning, etc.): $20K
- Training for all employees: $5K
Total one-time: $85K-$185K

Annual costs (after initial audit):
- Re-certification audit (annual): $30K-$50K
- Maintain security controls: $20K/year (tooling, updates)
Total annual: $50K-$70K

Timeline:
- Month 1-2: Hire auditor, define scope
- Month 2-3: Document security policies
- Month 3-4: Implement controls (encryption, access logging, etc.)
- Month 4-9: Audit observation period (6 months minimum)
- Month 9-10: Audit report finalization
Total: 9-10 months to first SOC2

Quick Path (Expedited): 6-8 months (if ggen already has some controls in place)
```

**What ggen Needs For SOC2:**

#### Security Policies (Documentation)
```
Required policies:
1. Information Security Policy (defines security principles)
2. Access Control Policy (who can access what)
3. Data Classification Policy (which data is sensitive, which is not)
4. Incident Response Policy (what to do if breach happens)
5. Change Management Policy (how code changes are reviewed, tested, deployed)
6. Backup & Disaster Recovery Policy
7. Third-party vendor management policy
8. Employee security training policy

Proof of implementation:
- Every policy must have evidence it's actually followed
- Example: "Access Control Policy says 'employees must use MFA'"
  Evidence: Screenshot of company password manager showing all employees have MFA enabled
```

#### Security Controls (Technical Implementation)
```
Access Controls:
- All employees use MFA (multi-factor authentication)
- Principle of least privilege (employees only access systems they need)
- Role-based access (different roles: engineer, ops, finance, have different permissions)
- Access logs (every access is logged: who, what system, when, why)

Encryption:
- Data at rest: All databases encrypted with customer-managed keys
- Data in transit: All network traffic uses TLS 1.2+
- Encryption keys: Managed in Google Cloud KMS, rotated annually

Change Management:
- All code changes require code review (4-eye principle)
- All code changes require automated tests to pass
- All deployments to production require approval
- All production deployments are logged and auditable
- All changes can be rolled back if needed

Monitoring & Alerting:
- Security Information & Event Management (SIEM): Monitor for suspicious activity
- Intrusion detection: Automatic alerts if suspicious IP addresses access ggen systems
- Vulnerability scanning: Weekly scans for known security vulnerabilities
- Patch management: Security patches applied within 30 days of release

Incident Response:
- Incident response team defined (who responds to security issues)
- Incident response plan tested quarterly (run "drill" to verify plan works)
- Time-to-detection: Suspicious activity detected within 1 hour
- Time-to-response: Team responds to alert within 2 hours
- Time-to-remediation: Issue resolved and contained within 24 hours
```

**Current ggen Status (Honest Assessment):**
```
ggen v1.0.0 (current):
- SOC2 status: NOT certified
- Documentation: Partial (policies exist, may be outdated)
- Technical controls: Partial (MFA for all engineers, but not all systems monitored)
- Compliance capability: Can achieve SOC2 within 6-9 months if committed

Roadmap:
- Q2 2025: Hire compliance officer
- Q3 2025: Audit firm engagement
- Q4 2025: Control implementation
- Q1 2026: SOC2 audit begins
- Q2 2026: SOC2 Type II certification (likely)

For Early Sales: Don't claim SOC2 compliance yet. Instead:
- Offer SOC2 roadmap to customers
- Highlight existing controls (encryption, MFA, change management)
- For SOC2-required deals: Wait or offer 6-month SOC2 achievement milestone
```

### 5.4 Liability: If Governor Causes Data Loss, Is ggen Liable?

**Scenario:**
```
Customer's Production Incident:
- Customer runs data analytics on BigQuery
- CCB governor throttles compute resources to control costs
- BigQuery job times out (takes too long to run)
- BigQuery fails to materialize aggregation table
- Downstream report depends on that table; report is corrupted
- Customer loses 1 day of analytics (millions of data points gone)
- Customer sues ggen: "CCB governor caused our data loss"
```

**Legal Analysis:**

#### Question 1: Did CCB Intentionally Cause Harm?
```
No. CCB was designed to throttle costs, not to delete data.
If harm happened, it was unintended consequence (negligence, not intentional tort).
Verdict: ggen is not liable for intentional harm.
```

#### Question 2: Did ggen Breach Its Contract?
```
CCB terms of service (ToS) must explicitly state:
"CCB is provided AS-IS without warranty of merchantability or fitness.
CCB may throttle resources to control costs, which may impact application performance.
Customer is responsible for testing CCB policies against their application requirements.
ggen is not liable for performance degradation, job timeouts, or data loss caused by throttling."

If ToS includes this disclaimer: ggen is NOT liable (customer accepted the risk).
If ToS does NOT include this disclaimer: ggen IS liable (contract breach).

Current Status: ggen must update ToS to include this disclaimer before launch.
```

#### Question 3: Did ggen Act Negligently?
```
"Negligence" test:
1. Did ggen have a duty to customer? YES (duty to implement CCB safely)
2. Did ggen breach that duty? DEPENDS on implementation:
   a. If CCB has safeguards (override mechanism, alerts, dry-run mode), NO breach
   b. If CCB has no safeguards, YES breach (negligent design)
3. Did breach cause harm? IF YES, then potentially liable

CCB Safeguards (from Section 1):
✓ Manual override (customer can cancel action immediately)
✓ Exemption windows (customer can declare "safe times" for important jobs)
✓ Preview alerts ("Governor will throttle in 30 seconds, click to override")
✓ Graceful degradation (throttle with exponential backoff, don't kill process)
✓ Audit logging (full trail of why governor took action)
✓ Cost-benefit analysis (governor doesn't throttle if harm exceeds benefit)

With these safeguards: ggen is NOT negligent (acted as reasonable vendor would)
Without these safeguards: ggen IS negligent (failed to implement basic protections)

Status: ggen must implement ALL safeguards before launch.
```

#### Question 4: Is ggen Liable For Damages?
```
If court determines ggen IS liable:

Damages Calculation:
- Direct: Lost analytics data = $50K (cost to re-process data)
- Indirect: Lost revenue from delayed report = $200K (what customer couldn't earn without report)
- Punitive: Intentional negligence penalty = $500K (court decides)

Total potential liability: $750K

But ggen ToS must include "Liability Cap":
"ggen's total liability to customer shall not exceed the monthly fees paid by customer.
If customer paid $600/month, maximum liability = $600."

This liability cap is standard in enterprise software (Salesforce, AWS, Microsoft all use this).
With cap: ggen's liability = $600 (not $750K).

Note: Liability caps don't apply if ggen acted with gross negligence or intentional misconduct.
```

### 5.5 Insurance: What E&O Coverage Do We Need?

**Errors & Omissions (E&O) Insurance:**
```
What is E&O?
- Insurance policy that covers ggen if customer sues for harm caused by CCB
- Covers legal fees, settlements, judgments
- Typical policy: $1M-$5M coverage

When is E&O Required?
- Before enterprise sales (enterprises require proof of E&O)
- Before charging for service (investors require E&O for revenue-generating products)
- Before critical infrastructure (if CCB affects customer production, E&O is mandatory)

E&O Cost:
- Policy premium: $3K-$10K per year depending on coverage
- Coverage up to $1M: ~$5K/year
- Coverage up to $5M: ~$15K/year

What Does E&O Cover?
- Professional negligence (CCB algorithm is buggy, causes harm)
- Data breach (hacker steals customer data through CCB)
- Regulatory violations (CCB violates GDPR, customer sues)
- NOT covered: Intentional fraud, criminal acts, contractual breaches (mostly)

Typical Policy Terms:
- Coverage: $1M per claim, $3M aggregate (total across all claims in year)
- Deductible: $10K (ggen pays first $10K, insurance pays rest)
- Exclusions: Cyber attacks < $100K claim value not covered

For ggen v1.0:
Recommended E&O: $1M coverage ($5K/year)
This is sufficient until ggen reaches $10M ARR, then upgrade to $5M coverage.
```

---

## 6. Competitive Threats

### 6.1 Why Not Just Use GCP's Native Autoscaling + Budget Alerts?

**The Question:**
```
Customer: "GCP already has autoscaling and budget alerts built-in.
Why do I need to buy a separate product?"

GCP Native Features:
- Compute Engine autoscaling: Automatically add/remove VMs based on load
- Cloud Run autoscaling: Automatically scale based on request traffic
- Budget alerts: Email alert if spend exceeds $500/month

CCB Value Prop: "Cost-aware governance beyond autoscaling."
```

**Honest Comparison:**

#### GCP Autoscaling (What It Does)
```
Trigger: Request traffic increases (100 req/s → 500 req/s)
Action: Automatically add VMs (5 VMs → 20 VMs)
Result: Capacity increases, latency stays low, COST increases

Example:
- 5 VMs @ $100/month = $500
- 20 VMs @ $100/month = $2000
- Autoscaling made it 4x more expensive, but...
- User experience is great (no throttling, no latency)

When Autoscaling Fails:
- Traffic spike is due to bug (runaway loop generating requests)
- Autoscaling adds more VMs (thinking it's legitimate traffic)
- Cost spirals to $50K/month
- Budget alert emails customer (too late, damage done)
- Customer now has $50K bill to explain to CFO
```

#### GCP Budget Alerts (What It Does)
```
Trigger: Monthly cost > $500
Action: Send email alert to customer
Result: Customer reads email, manually investigates

Timeline:
- 02:00 UTC: Runaway process starts
- 02:30 UTC: Cost reaches $50K
- 03:00 UTC: GCP budget alert sent ("spend exceeded $500")
- 08:00 UTC: Customer wakes up, reads email
- 08:30 UTC: Customer logs into GCP console, sees cost spike
- 09:00 UTC: Customer finds runaway process, kills it
- 09:05 UTC: Cost spike stops (but $50K is already spent)

Total damage: $50K
Response time: 7 hours

CCB Would Do:
- 02:00 UTC: Runaway process starts
- 02:01 UTC: CCB detects cost spike, throttles automatically
- 02:02 UTC: Runaway process continues, but throttled
- 02:03 UTC: Cost stops increasing (caught by CCB)
- 02:04 UTC: CCB alerts customer: "Cost spike detected and mitigated"
- 08:00 UTC: Customer wakes up, reads alert (damage already prevented)

Total damage: $0-$100 (small spike caught immediately)
Response time: 1 minute (automatic)
```

#### Why CCB Is Different:
```
GCP Native:
✓ Autoscaling: Scale capacity up/down based on traffic
✗ But: Doesn't prevent costs from increasing when traffic increases legitimately
✗ But: Budget alerts are reactive (after damage is done)

CCB:
✓ Cost-aware decision making: Throttles when costs exceed policy
✓ Proactive: Catches cost spike BEFORE it becomes expensive
✓ Intelligent: Distinguishes "legitimate traffic spike" from "runaway bug"
✓ Automated: Throttles within seconds, not hours

Honest Limitation:
CCB requires tuning (what's a "good" cost spike? What's "runaway"?).
GCP autoscaling requires no tuning (automatic based on traffic).
So for simple applications, GCP autoscaling might be "good enough."
For complex, mission-critical applications, CCB provides extra control.
```

### 6.2 How Do We Compete Against DataDog/Sentry/New Relic?

**The Competitors:**
```
DataDog: Full observability platform (logs, metrics, traces, APM)
  - Cost governance: Limited (mainly budget alerts)
  - Strength: Complete monitoring across all layers
  - Weakness: General platform, not specialized for cost governance

Sentry: Error tracking platform (catches bugs, exceptions)
  - Cost governance: None
  - Strength: Developer experience (easy to debug errors)
  - Weakness: Doesn't help with cost management

New Relic: Application performance monitoring (APM)
  - Cost governance: Limited (budget alerts, service-level objectives)
  - Strength: Detailed performance metrics per transaction
  - Weakness: Cost governance is an afterthought, not core feature
```

**CCB's Competitive Advantage:**

#### Advantage 1: Specialized For Cost (Not General Observability)
```
DataDog Strategy: 10% of product is cost management
  - 90% is general observability (metrics, logs, traces)
  - Cost management is side feature (budget alerts, dashboards)

CCB Strategy: 100% focused on cost governance
  - Deeper cost analysis (cost per request, cost per user, cost per feature)
  - Autonomous throttling (take action automatically)
  - Better cost prediction models (know what $1M spend looks like)

Analogy: DataDog is Swiss Army knife (does everything okay).
CCB is specialized screwdriver (does cost governance really well).

Competitive Position:
- For customers wanting "just monitoring": Use DataDog
- For customers wanting "autonomous cost management": Use CCB
- For customers wanting "both": Use DataDog + CCB

ggen doesn't compete directly with DataDog. We complement them.
```

#### Advantage 2: Autonomous Actions (Not Just Alerts)
```
DataDog: "Hey, your cost is high. You should do something."
  - Alert: Spend at $1000/min (threshold is $500/min)
  - Action: Wait for human to manually throttle

CCB: "Your cost is high. I'm throttling now. If you disagree, override."
  - Detection: Spend at $1000/min
  - Action: Automatic throttle (within 2 seconds)
  - Result: Cost drops immediately

Speed advantage: CCB responds in seconds. Human takes minutes to hours.

Competitive Position:
DataDog + CCB together is powerful:
- DataDog tells you what's happening (observability)
- CCB fixes it automatically (governance)
```

### 6.3 What If Google Builds This Natively (Unlikely But Possible)?

**Scenario:**
```
Date: Q2 2025 (hypothetical)
Google Announcement: "Cloud Autonomous Cost Controller (ACC)"
- Built into GCP console
- No external service needed
- Free with GCP subscription (or $100/month)
- Does 80% of what CCB does

ggen's Business: Destroyed?
```

**Risk Assessment:**

#### Likelihood: Low (20-30%)
```
Why Google might NOT build this:
1. Cost governance is niche (only 5-10% of GCP customers care deeply)
2. Google makes MORE money if customers don't govern costs (more spend = more revenue)
3. Google's incentive is misaligned (doesn't want customers to spend less)
4. Engineering time is expensive; Google prioritizes features that increase spend, not decrease it

Why Google MIGHT build this:
1. Enterprise customers demand it (we keep losing deals to CCB)
2. Competitive pressure from AWS/Azure (they build better cost tools)
3. Improve customer satisfaction (make GCP easier to use for cost management)
4. Leverage existing infrastructure (Google has cost data, monitoring, infrastructure)

Historical precedent: Google has killed or marginalized many partner products
- Google Analytics (killed Omniture ecosystem)
- Google Cloud Speech-to-Text (marginalized commercial speech APIs)
- BUT: Google Cloud often leaves room for specialized vendors (DataDog, PagerDuty still thrive)

Honest assessment: If Google sees CCB as threat, they COULD build clone in 12 months.
If Google doesn't see CCB as threat, CCB has 3-5 year window.
```

#### What CCB Should Do To Hedge Risk:

```
Strategy 1: Build Moat (Hard To Copy)
- Customer relationships: Make CCB so valuable, customers won't switch even if Google builds alternative
- Integrations: Build 50+ integrations with customer tools (make switching expensive)
- Data network effects: Accumulate customer cost data, learn patterns Google doesn't know
- Community: Build CCB community (open-source governors, user conference, etc.)

Strategy 2: Expand Beyond GCP
- Support AWS, Azure, multi-cloud
- If CCB only works on GCP, Google copying it hurts ggen
- If CCB works everywhere, Google copying it only helps ggen's reputation

Strategy 3: Go Deeper Into Governance
- Cost governance is just first domain
- Expand to security governance, performance governance, compliance governance
- If Google builds cost-only, ggen can still offer "governance platform"

Strategy 4: Enterprise Features
- Custom governors (customers build their own logic)
- API for 3rd-party governance plugins
- Advanced analytics and reporting
- If Google builds basic version, ggen offers enterprise version

Most Likely Outcome: If Google builds competitor, CCB survives by:
1. Moving up-market (enterprise customers, specialized domains)
2. Multi-cloud support (not locked into GCP)
3. Deeper integrations (hard to replicate)
4. Community and network effects (switching cost)
```

---

## 7. Scaling & Operations

### 7.1 Maximum Concurrent Governors Per Tenant (Capacity)

**Question:**
```
Enterprise Customer: "We have 50 microservices. Can we run 1 CCB governor per service?
That's 50 concurrent governors. Will your system handle this?"
```

**Architecture Scalability:**

#### Per-Customer Governor Capacity
```
Current Design (v1.0):
- 1 CCB governor instance = monitors 1-3 services
- Multiple governors per customer allowed (separate instances)
- Each governor is independent Kubernetes pod

Scaling Properties:
- Governor memory per pod: ~500 MB
- CPU per pod: 0.5 cores
- 50 pods for 50 services: 25 GB memory, 25 CPU cores

On ggen Kubernetes cluster:
- Worker nodes: 16 cores, 64 GB RAM each
- Capacity: 2 worker nodes can host 50 CCB pods (with headroom)
- Cost to ggen: ~$1000/month for infrastructure to support 1 customer with 50 governors

Customer's CCB Cost:
- Base subscription: $500/month
- Per-governor fee: 50 governors × $10/month = $500/month (hypothetical per-governor fee)
- Total: $1000/month

Profitability: Break-even on infrastructure cost
Problem: ggen doesn't make margin if customer has many governors
Solution: Add per-governor fee that scales with customer's usage
```

#### Recommendation: Soft Limit Per Tenant
```
Soft Limit: 10 concurrent governors per customer without prior approval
- Customers with <10 governors: Standard pricing
- Customers with 10-50 governors: Custom pricing (higher per-governor fee)
- Customers with >50 governors: Enterprise contract (flat fee + percentage of savings)

Why soft limit?
- Prevents single customer from consuming all ggen infrastructure
- Encourages customers to consolidate (use shared governor instead of 50 individual)
- Allows ggen to say "no" to unreasonable scaling without losing deal

Policy:
- Customer wants 50 governors: ggen says "sure, but custom contract" (higher price)
- Conversation: "Why do you need 50 separate governors? Could you use 5 consolidated governors?"
- Outcome: Customer often consolidates (reduces cost)
```

### 7.2 DDoS When Signal Volume Spikes 1000x

**Scenario:**
```
Normal day:
- 10,000 customer projects sending signals
- ~100K metrics per minute (signals from projects)
- ~10 MB/min data throughput

Attack/Incident day:
- Same 10,000 projects, but each sends 100x more metrics
- ~10M metrics per minute (10x normal)
- ~1 GB/min data throughput

Example cause:
- Bug: Customer's monitoring script sends metric every 1ms instead of 1s
- Result: 1000x more signals from that customer
- ggen's Pub/Sub subscription gets flooded
```

**Resilience Design:**

#### Rate Limiting (Per Customer)
```
Each customer gets quota:
- Max 100K metrics/minute (if exceeded, older metrics are dropped)
- Max 10 MB/minute (if exceeded, publisher is rate-limited)
- Quota can be increased per contract (enterprise customer pays more)

Implementation:
- Pub/Sub subscription has delivery retry policy
- If customer sends >100K metrics, older messages are discarded (not queued forever)
- Result: Pub/Sub doesn't become overloaded

Metric dropped alert:
- ggen monitoring detects: "Customer X dropped 500K metrics in last minute"
- Alert to customer: "Signal throughput exceeded quota. Reduce signal frequency or upgrade quota."
- Customer's action: Reduce logging frequency or ask ggen to increase quota

Effect: DDoS is self-contained (doesn't affect other customers)
```

#### Backpressure (Graceful Degradation)
```
If ggen's signal processing falls behind:
- CCB instances queue up (wait for previous signals to be processed)
- New signals are delayed (might take 5 minutes to process instead of 5 seconds)
- Throttle decisions are delayed (governor responds slower)

Acceptable tradeoff:
- Better to be 5 minutes slow than to crash
- Slow response is better than no response (system stays up)

Recovery:
- ggen ops scales up more CCB instances (auto-scaling in Kubernetes)
- Processing capacity increases 2x
- Queue drains
- Normal latency returns

Timeline:
- 09:00: Signal surge detected
- 09:01: Backpressure detected (queue depth > 100K)
- 09:02: Auto-scale triggers (2x more instances)
- 09:03: New instances start processing
- 09:04: Queue depth starts dropping
- 09:06: Queue drained, normal latency restored
```

### 7.3 Multi-Region Support (Data Residency)

**Question:**
```
EU Customer: "We have data residency requirements.
Our cost metrics must stay in EU (not transmitted to US servers).
Does CCB support this?"
```

**Current Model (v1.0): US-Only**
```
Architecture:
- ggen control plane: us-central1 (Iowa)
- Customer signals: EU → us-central1 (crosses Atlantic)
- CCB instance: Runs in customer's GCP project (customer's region)
- Result: Cost signals travel to US, then back to customer

Problem for EU:
- GDPR: If cost data contains any PII, must stay in EU
- Performance: Data crosses Atlantic twice (latency)
- Compliance: Enterprise customer's policy requires data residency in EU

ggen's current answer: "Sorry, not supported in v1.0."
```

**Multi-Region Model (v2.0 Roadmap):**

#### Design: Regional CCB Control Planes
```
ggen deploys regional control planes:
- us-central1: For US customers
- europe-west1: For EU customers
- asia-southeast1: For APAC customers

Each regional plane has:
- Kubernetes cluster (runs CCB instances)
- Databases (store config, logs)
- All data stays in region

Architecture:
Customer in EU                          EU Control Plane (ireland)
│ (eu-metrics) ──────────→ │ (ccb-eu-instance) ──→ (eu-db)
└─────────────────────────────────────────────────────────┘
(All data stays in europe-west1, never crosses Atlantic)
```

#### Implementation Cost
```
One-time cost to add new region:
- Kubernetes cluster setup: $10K
- Database setup: $5K
- Network/DNS setup: $2K
- Compliance audit for region: $5K
- Total: $22K per new region

For 3 regions (US, EU, APAC): $66K
For 10 regions (full global): $220K

Annual cost to maintain 3 regions:
- Infrastructure: $30K/month ($360K/year)
- Regional support staff: $100K/year (1 engineer per region)
- Total: $460K/year for multi-region support

Revenue needed to justify:
- At $1000/month per customer: Need 500 EU customers to justify regional deployment
- At $5000/month (enterprise): Need 100 EU customers

Roadmap decision:
- v1.0 (now): US-only (focus on product, not logistics)
- v1.5 (Q3 2025): Add EU region (by popular demand)
- v2.0 (Q1 2026): Add APAC region (if revenue supports)
```

### 7.4 How Do We Observe The Observers (Meta-Governance)?

**Question:**
```
Customer: "If CCB governor makes bad decisions, how do you catch it?
Who watches the watcher?"

Example: CCB governor is itself broken (buggy algorithm).
Customer's costs continue to spike because governor is misconfigured.
How does ggen know?
```

**Meta-Governance Design (Layers of Oversight):**

#### Layer 1: Governor Health Checks
```
Each CCB instance reports health metrics:
- Signal ingestion latency: <5 seconds? ✓
- Decision processing latency: <2 seconds? ✓
- Throttle success rate: >99%? ✓
- Pod restart count: <1 per week? ✓
- Memory usage: <500MB? ✓

If any check fails:
- Alert: "CCB-customer-A governor health check failed"
- Action: ggen ops investigates

Dashboard:
- ggen ops see: 500 green checkmarks (healthy governors)
- If 50 turn red: Potential systemic issue (investigate)
```

#### Layer 2: Customer Anomaly Detection
```
ggen runs meta-monitoring:
- "Is customer's actual cost trending up or down?"
- "Is CCB throttling helping or hurting?"
- "Do customer's decisions match CCB actions?"

Example:
Customer A's metrics:
- Month 1 cost: $10K (with CCB)
- Month 2 cost: $50K (with CCB) — 5x increase?!
- Alert: "Customer A's cost trending UP despite CCB active"
- Investigation: "Why isn't CCB preventing cost growth?"
  Options:
  a) CCB policy misconfigured (too permissive)
  b) Customer's workload legitimately grew 5x
  c) CCB has bug and isn't throttling correctly

Action:
- ggen reaches out: "We noticed your costs increased. Let's review your CCB settings."
- Conversation often reveals: "Oh, we intentionally increased spending for new campaign. CCB is working fine."
- Or reveals: "We didn't know CCB was supposed to prevent this; you're right, let's tune it."
```

#### Layer 3: Automated Rollback (Kill Switch)
```
If CCB causes customer harm:
1. Customer clicks "emergency stop"
2. All governors for that customer are disabled instantly
3. Cost governance stops, customer's applications run freely
4. ggen alert: "Customer disabled all governors (emergency stop activated)"

Recovery:
1. Customer contacts ggen: "We had incident, governors were disabled"
2. ggen RCA: "What went wrong with governors?"
3. Fix deployed: Patch + retest
4. Re-enable gradually:
   - Enable 1 governor in dry-run mode (no actions, just logs)
   - Verify for 1 hour
   - Enable 2nd governor
   - Verify for 1 hour
   - Re-enable all governors
```

#### Layer 4: Quarterly Audits
```
Every quarter, ggen audits:
- 100 random customer accounts
- Review: "Is CCB saving customer money, or just making decisions?"
- Review: "Are throttle decisions justified? Are they helpful?"
- Review: "Has customer's cost trajectory changed since CCB enabled?"

Metrics tracked:
- Cost savings attributed to CCB: $0 or $50K+ per customer
- Cost variance (stability) improved: Yes or No
- Customer satisfaction: Surveyed quarterly
- Governor accuracy: How many throttle decisions were necessary? How many were false positives?

If customer's score is too low:
- Action: Outreach. "Let's improve your CCB configuration."
- Outcome: Either customer is not a good CCB fit, or needs better tuning
```

---

## 8. Long-Tail Risk Scenarios

### 8.1 Malicious Governor (Customer Intentionally Abuses)

**Scenario:**
```
Attacker: Competitor who somehow gains access to customer's GCP project

Malicious Goal: Use CCB to sabotage competitor's infrastructure

Attack:
1. Attacker creates CCB governor with $1/minute threshold (extremely low)
2. Governor now throttles EVERYTHING (legitimate traffic gets throttled)
3. Competitor's applications slow to a crawl
4. Competitor loses SLA, customer satisfaction drops
5. Attacker profits (gained market share from sabotaged competitor)
```

**Detection & Prevention:**

#### Prevention 1: Least Privilege (Access Control)
```
CCB requires specific IAM role to create/modify governors.
Role: "roles/cloud-ccb.admin" (doesn't exist by default)

Customer must explicitly grant this role:
- Only to trusted team members
- Only within customer's GCP organization
- Cannot be delegated to external parties

If competitor tries to create governor:
- Competitor doesn't have cloud-ccb.admin role
- Attempt fails: "Permission denied"
- Audit log captures failed attempt

Result: Attacker cannot create malicious governor.
```

#### Prevention 2: Policy Validation
```
When customer creates governor policy:
- Threshold: $1/minute (unreasonably low)
- Validation check: "Threshold is 1000x lower than recommended for your workload"
- Warning: "This threshold may cause excessive throttling. Are you sure?"

If customer confirms: Governor is created with warning flag
ggen monitoring alerts: "Customer created aggressive policy ($1/min). Investigate."

Detection:
- ggen sees: Customer's cost is stable, but CCB is throttling every 30 seconds
- Alert: "Throttle frequency is unusually high. Policy may be misconfigured."
- Outreach: "We noticed unusual throttling. Let's review your policy."
- Conversation might reveal: "Oh, we were testing. Yes, this is too aggressive."
```

#### Prevention 3: Audit Logging
```
Every governor action is logged:
- Who created this governor?
- When was it created?
- What email address created it?
- What IP address?

If competitor created malicious governor:
- Audit log shows: created by attacker@competitor.com
- ggen investigates: "This user doesn't work at the customer's company"
- ggen suspends governor immediately (emergency stop)
- ggen notifies customer: "We detected suspicious governor creation. Details: [attacker@competitor.com]"
- Customer investigates: "Yes, this is a hostile party. Thank you for catching this."
```

### 8.2 Supply Chain Risk (Backdoor In CCB Code)

**Scenario:**
```
ggen developer is compromised (malicious insider or hacker):
- Developer commits backdoor to CCB source code
- Backdoor logs all customer cost metrics to attacker's server
- ggen doesn't catch it (code review missed it)
- Backdoor deploys to production

Result: Attacker steals customer cost data (competitive intelligence)
```

**Prevention & Detection:**

#### Prevention 1: Code Review (4-Eye Principle)
```
Policy: All code changes require 2 independent code reviews

Example:
1. Developer writes code change (commits to GitHub)
2. Peer 1 reviews: "Looks good, checking for security issues..."
3. Peer 2 reviews: "OK, checking for logic correctness..."
4. Both approve: Code can be merged
5. CI/CD automatically tests and deploys

Backdoor detection:
- Peer 1 sees: "New function logging cost data to external URL"
- Peer 1 question: "Why are we sending cost data externally?"
- Developer answer: "Uh... it's... a feature?"
- Peer 1 blocks: "Reject. This looks like data exfiltration. Investigate."

Result: Backdoor is caught before deployment.
```

#### Prevention 2: Dependency Scanning
```
ggen uses automated tools to scan for malicious dependencies:
- Tool: OWASP Dependency-Check (checks for known vulnerabilities)
- Tool: Snyk (continuous vulnerability scanning)
- Tool: npm audit (dependency audit)

Policy:
- Before deploy, all dependencies are scanned
- If dependency has known vulnerability: Deploy is blocked
- If dependency hasn't been updated in 2 years: Flag for investigation

This catches compromised dependencies (someone injected malicious code into library).
```

#### Prevention 3: Code Signing & Provenance
```
Every CCB release is cryptographically signed:
- Release engineer signs: "This is CCB v2.1.0"
- Signature proves: "This code was built by ggen on 2025-01-20 at 14:30 UTC"
- Signature proves: "No one modified this code after signing"

Customer can verify:
$ ggen-verify-signature ccb-v2.1.0.tar.gz
Signature valid. Built by release@ggen.io on 2025-01-20.

If backdoor is injected after release:
- Signature verification fails: "Signature does not match. Code has been tampered."
- Customer rejects: "Installation failed. Code integrity check failed."

Result: Backdoor is detected before installation.
```

### 8.3 Customer Lock-In (Are They Trapped?)

**Scenario:**
```
Customer has been using CCB for 2 years.
CCB governors are managing all their cost governance.
Customer decides to switch to DataDog + custom scripts.

What happens?
- How hard is it to migrate away?
- Is customer trapped?
- Is it ggen's goal to trap them?
```

**Honest Assessment:**

#### Lock-In Factors (Why It's Hard To Leave)

```
Factor 1: Policy Configuration
- Customer has spent 6 months tuning CCB policies
- 50+ governor configurations, each customized
- Switching means rewriting all policies in new system (10+ hours of work)

Factor 2: Integration Ecosystem
- CCB integrated with customer's monitoring (Datadog, New Relic)
- CCB integrated with customer's ticketing (Jira, ServiceNow)
- CCB integrated with customer's financial system
- Switching means re-integrating with new system (20+ hours of work)

Factor 3: Organizational Knowledge
- Customer's ops team trained on CCB
- Documentation written for CCB workflows
- People understand "how CCB works"
- Switching means retraining entire ops team (40+ hours of training)

Total switching cost: 70+ hours
Customer's cost: $7000-$14000 (assuming $100-$200/hour for engineers)
```

#### Lock-In Is NOT Our Goal (But Also Not Bad)

```
ggen's Honest Position:
- Lock-in is a natural side effect of good product
- We don't artificially trap customers (no hard contracts, no exit penalties)
- We keep customers by being useful, not by being expensive to leave

What We Will NOT Do:
✗ No long-term contracts that punish early termination
✗ No proprietary file formats that only ggen can read
✗ No obfuscated policies (customer can always export, read, understand their settings)
✗ No data hostage-taking (customer can always request data export)

What We WILL Do:
✓ Make CCB so useful that switching cost is worth it (customers stay because product is good)
✓ Invest in ease-of-export (customer can export all policies, history, settings in 1 click)
✓ Competitive pricing (if DataDog offers 80% functionality for 50% price, we lose some customers, that's OK)
✓ Transparent costs (no surprise billing, no hidden fees)

Result: Some customers will leave. That's healthy.
If ALL customers stay, it means we're either lying about lock-in, or pricing is too high.
```

### 8.4 Sunset Scenario (What If We Discontinue CCB?)

**Scenario:**
```
Date: Q4 2027
ggen executive decision: "CCB is not profitable. Discontinuing in 12 months."
Announcement: "CCB will sunset on 2028-12-31. Customers have 12 months to migrate."

Customer's Reaction: "We have 50 governors managing our entire cost infrastructure!
What do we do?"
```

**Responsible Sunset Plan:**

#### Phase 1: Announcement (Month 1)
```
Announcement includes:
1. Sunset date: 2028-12-31 (12 months notice)
2. Migration support: ggen will help customers migrate to alternative (DataDog, custom script, AWS equivalent)
3. Data export: ggen provides tool to export all governor policies, history
4. Pricing change: Discounted pricing during migration period ($500 → $200/month)
5. Support: Dedicated migration engineer (no cost) helps customer transition

Customer communication:
- Email to all customers
- Blog post explaining decision
- FAQ addressing common questions
- Migration guide (how to export, how to convert to other platform)
```

#### Phase 2: Migration Support (Months 1-12)
```
ggen provides:
1. Export tool: One-click export all CCB configurations to JSON/YAML
2. Conversion script: Convert CCB policy to DataDog policy (example for one alternative)
3. Migration engineer: Assigned to customer, helps with transition planning
4. Refund: Prorated refund for unused time (if customer switches month 6, refund 6 months of fees)

Example migration:
Customer pays $600/month × 12 = $7200/year
Customer switches after 6 months
Refund: $600/month × 6 = $3600

Net cost to customer: $3600 for 6 months of service + migration support
Not great, but honest.
```

#### Phase 3: Final Shutdown (Month 12)
```
Actions:
1. January 1, 2029: CCB service stops
2. All customer governors are disabled (graceful)
3. Final export: Customer data is available for 30 days after shutdown
4. ggen pays for data hosting (AWS S3) for 30 days (cost: ~$100-$1000)

Result:
- Customer has 30 days to retrieve their data
- After 30 days, data is deleted
- Customer has migrated (or suffered downtime, their choice)
```

#### Honest Truth:
```
No company WANTS to sunset a product. If ggen sunsets CCB:
1. It means CCB was not profitable
2. It means competing products (DataDog, AWS native) were better
3. It means ggen's GTM/sales was not strong enough

This is failure, not success.

But IF it happens:
- ggen will do the honest thing: Give customers time, help with migration, refund unused time
- ggen won't do the dishonest thing: Instantly shut down, hold customer data hostage, force customer into expensive contract
- ggen's reputation survives (customers remember "they were honest about sunset")
- vs. dishonest shutdown (customers remember "they screwed us")
```

---

## Conclusion: The Honest Truth

**This FAQ Is Incomplete**

Because the future is uncertain. New questions will emerge:
- What if CCB causes financial fraud (used to hide costs)?
- What if CCB is used for insider trading (cost data reveals company strategy)?
- What if CCB is used by terrorists (cost metrics reveal location of data centers)?

We don't have answers yet. When these questions arise, ggen will address them honestly.

**The Standard**

Every answer in this document follows one rule:
- **Would you be comfortable telling a customer this answer in a board meeting?**
- If yes: It's honest.
- If no: We need a better answer.

**For Sales Teams**

If a CTO asks something not in this FAQ:
1. Don't make up an answer
2. Say: "That's a great question. Let me research and get back to you."
3. Ask product/legal to help
4. Get back to customer within 24 hours with honest answer
5. Add the Q&A to this FAQ for next customer

**Final Thought**

Trust is built on honesty, not marketing. This FAQ is our commitment to honesty.

---

**Document Version**: 1.0
**Last Updated**: 2026-01-25
**Status**: INTERNAL - Companion to Press Release
**Audience**: Sales, Product, Legal, Operations, Infrastructure
