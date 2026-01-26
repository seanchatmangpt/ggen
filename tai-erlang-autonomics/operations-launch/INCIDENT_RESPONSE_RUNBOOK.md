# TAI Erlang Autonomics - Incident Response Runbook
## 10+ Detailed Scenario Response Plans

**Version:** 1.0.0
**Last Updated:** January 25, 2026
**Audience:** On-call engineers, incident commanders, support team
**Purpose:** Step-by-step procedures for responding to critical incidents

---

## Quick Reference: Severity Levels

| Severity | Definition | Response Time | Escalation Path |
|----------|-----------|----------------|-----------------|
| **P1 - CRITICAL** | Service down, revenue at risk, customers can't use product | Acknowledge within 5 min, begin fix within 15 min | On-call eng → VP Product → CEO |
| **P2 - HIGH** | Service degraded (slow/errors), customer experiencing issues | Acknowledge within 15 min, begin fix within 30 min | On-call eng → VP Product |
| **P3 - MEDIUM** | Minor issue, workaround exists, customer not blocked | Acknowledge within 1 hour | Support team, escalate if needed |
| **P4 - LOW** | Cosmetic/documentation issue | Next business day | Support team/backlog |

---

## Universal Incident Response Flow (All Scenarios)

```
1. ACKNOWLEDGE (5 minutes)
   ├─ Confirm you received the alert
   ├─ Page secondary on-call if you don't respond within 10 min
   └─ Post to #incident-response Slack channel

2. ASSESS (5-10 minutes)
   ├─ Verify incident is real (not false alarm)
   ├─ Determine severity (P1-P4)
   ├─ Estimate impact (# customers affected, revenue at risk)
   └─ Create incident ticket (title + severity)

3. GATHER TEAM (0-10 minutes for P1)
   ├─ If P1: Page incident commander + VP Product + tech lead
   ├─ If P2: Notify VP Product
   ├─ If P3/P4: Can be handled solo
   └─ Create war room (Slack channel or Zoom call)

4. COMMUNICATE (Every 15-30 minutes until resolved)
   ├─ Update Slack #incident-response (internal status)
   ├─ Update status.tai.ai (external customer notification)
   ├─ Customer communication: 30-min updates if ongoing
   └─ No news is bad news - keep communicating

5. INVESTIGATE & FIX (Parallel)
   ├─ Check dashboards (Datadog, GCP console)
   ├─ Review logs for errors
   ├─ Check deployment history (did something just change?)
   ├─ Run diagnostic commands (endpoint tests, database queries)
   ├─ Implement fix (code change, config change, scaling, or rollback)
   └─ Test fix in staging before production

6. VERIFY FIX (5-10 minutes)
   ├─ Check dashboard: Error rate back to normal?
   ├─ Check customer impact: Can they use the product?
   ├─ Run smoke test: Make actual API call, verify success
   └─ Monitor for 5 min: Any regression?

7. RESOLVE & COMMUNICATE (10 minutes)
   ├─ Close incident ticket
   ├─ Send "all clear" notification to customers
   ├─ Update status page: "Resolved"
   ├─ Schedule post-mortem for tomorrow (if P1/P2)
   └─ Log incident details for post-mortem

8. POST-MORTEM (Within 24 hours)
   ├─ What triggered the incident? (root cause)
   ├─ Why wasn't it caught sooner? (detection gap)
   ├─ What did we do well? (praise)
   ├─ What will we improve? (action items)
   └─ Who will own each improvement? By when?
```

---

## SCENARIO 1: Service Completely Down (HTTP 5xx Errors)

**Alert Trigger:** Error rate >10% for 5 minutes OR availability <99%

### Symptoms Customers See
- All API requests return 500 Internal Server Error
- Status page shows red
- No functionality works

### Symptoms You See (Dashboard)
- Datadog: Error rate spike to 80-100%
- GCP Console: Cloud Run shows recent restart or deployment
- Datadog: No requests being processed (throughput = 0)

---

### Step 1: Quick Assessment (5 minutes)

```bash
# Terminal 1: Check Cloud Run status
gcloud run services describe tai-autonomics --region=us-central1

# Terminal 2: Check recent errors in logs
gcloud logging read "resource.type=cloud_run_revision AND
  jsonPayload.level=ERROR" --limit=50 --format=json | head -20

# Terminal 3: Check if service is actually down
curl -v http://localhost:8080/health  # If running locally
# OR
curl -v https://api.tai.ai/health  # Production endpoint
```

### Step 2: Determine Root Cause (Which of these?)

**Scenario A: Recent deployment broke it**
```bash
# Check last 3 deployments
gcloud run services describe tai-autonomics --region=us-central1 \
  --format="value(status.traffic)"

# See what changed
git log --oneline -5
git diff HEAD~1 HEAD

# If you deployed in last 5 min: Likely culprit
# ACTION: Rollback (see below)
```

**Scenario B: Service crashed/restarted**
```bash
# Check restart count
gcloud run revisions list --service=tai-autonomics --region=us-central1

# Check if memory/CPU limits exceeded
gcloud logging read "resource.type=cloud_run_revision AND
  textPayload~'Memory exceeded'" --limit=10

# If many restarts: Memory leak or infinite loop
# ACTION: Scale up temporarily, investigate logs
```

**Scenario C: Dependency unavailable (Pub/Sub, Firestore)**
```bash
# Check Pub/Sub health
gcloud pubsub subscriptions describe erlang-autonomics-signals \
  --format="table(state)"

# Check Firestore connectivity
gcloud firestore databases list

# If red: Dependency issue
# ACTION: See SCENARIO 7 (Dependency Down)
```

**Scenario D: Something else crashing the app**
```bash
# Check application logs for FATAL errors
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=100 | grep -i "fatal\|panic\|crash"

# Look for pattern: What request triggers crash?
# ACTION: See SCENARIO 6 (Crash on Specific Input)
```

### Step 3: Fix It

**If Scenario A (Bad Deployment): ROLLBACK**
```bash
# Get previous healthy revision
gcloud run revisions list --service=tai-autonomics --region=us-central1 \
  --format="table(name, status.conditions[0].message, createTime)"

# Identify last healthy revision (look at create time + messages)
LAST_GOOD_REVISION="tai-autonomics-abc1234xyz"

# Rollback traffic to previous revision
gcloud run services update-traffic tai-autonomics \
  --region=us-central1 \
  --to-revisions="$LAST_GOOD_REVISION=100"

# Verify: Check health endpoint
curl https://api.tai.ai/health

# Expected: 200 OK within 10 seconds
```

**If Scenario B (Memory/Crash): Scale Up Temporarily**
```bash
# Increase memory limit from 512MB to 1GB
gcloud run services update tai-autonomics \
  --memory=1Gi \
  --region=us-central1

# Wait for deployment (2-3 min)
# Check health
curl https://api.tai.ai/health

# Once stable: Investigate root cause (memory leak?)
# Create P2 ticket: "Investigate why service using >512MB"
```

**If Scenario C (Dependency Down): See Scenario 7**

**If Scenario D (Crash on Input): See Scenario 6**

### Step 4: Verify Fix

```bash
# 1. Health check
curl https://api.tai.ai/health
# Expect: 200 OK, status: "healthy"

# 2. Make actual API call
curl -X POST https://api.tai.ai/marketplace \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "test-001"}'
# Expect: 200 OK, pricing response

# 3. Check dashboard for 5 minutes
# - Error rate: Should drop to <1%
# - Throughput: Should resume normal level
# - Latency p95: Should return to <500ms
# - Active connections: Should be >0

# 4. Check customer complaints
# - Slack: #support - any new complaints?
# - Email: support@tai.ai - any bounce-backs?
# - Status page: Customers seeing green now?
```

### Step 5: Communicate

```
STATUS UPDATE TO CUSTOMERS (Via status.tai.ai):

RESOLVED: Service fully restored

We experienced a service outage from 14:32-14:47 UTC due to [ROOT CAUSE].
All functionality is now restored. No data loss occurred.

We apologize for the disruption. Detailed incident report will be available
in 24 hours. Contact support@tai.ai with any questions.
```

### Step 6: Post-Mortem Template

```markdown
## Incident: Service Down - 2026-01-25

**Duration:** 14:32-14:47 UTC (15 minutes)
**Severity:** P1
**Customers Affected:** All

### Root Cause
[What actually caused this?]
- Bad deployment? What was the code change?
- Memory leak? What was leaking?
- Dependency failure? Which service?

### Why Wasn't It Caught?
- [Detection gap 1: Should have caught this sooner?]
- [Detection gap 2: What alarm should we have triggered?]

### Timeline
- 14:32: First alert fires
- 14:35: On-call engineer acknowledges
- 14:40: Cause identified
- 14:47: Fix deployed and verified
- 14:50: Customers notified

### Impact
- Revenue lost: $X (if any customers couldn't charge)
- Customers impacted: N
- Data loss: None

### What We'll Do Better
1. [Action item 1] - Owner: [Name], Due: [Date]
2. [Action item 2] - Owner: [Name], Due: [Date]
3. [Action item 3] - Owner: [Name], Due: [Date]
```

---

## SCENARIO 2: High Error Rate But Service Up (5-10% Errors)

**Alert Trigger:** Error rate 5-10% for 10 minutes

### Symptoms
- Most requests work, but ~7% fail with 5xx or 4xx
- Response time normal
- Service partially degraded

### Quick Assessment

```bash
# Check which endpoint is failing
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=50 | grep -o "POST /marketplace\|POST /pubsub\|GET /health"

# Look for pattern: Is it specific endpoint or random?
# Check error messages
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=50 --format=json | jq '.[] | .jsonPayload.error_message'

# Common causes:
# 1. Flaky dependency (intermittent Pub/Sub failures)
# 2. Memory leaks (some requests fail as memory exhausted)
# 3. Concurrency bug (race condition under load)
# 4. Bad data from customer (invalid input → crash)
```

### Investigation Steps

**Step 1: Check which endpoint**
```bash
# Get error distribution by endpoint
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=100 --format=json | \
  jq '.[] | .jsonPayload.path' | sort | uniq -c | sort -rn
```

**Step 2: Check error types**
```bash
# Group by error type
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=100 --format=json | \
  jq '.[] | .jsonPayload.error_code' | sort | uniq -c | sort -rn

# Examples:
# 20 "connection_timeout" - Pub/Sub slow
# 15 "out_of_memory" - Memory leak
# 12 "rate_limit_exceeded" - Too much traffic
```

### Fixes by Error Type

**If "connection_timeout" errors:**
```bash
# Pub/Sub is slow. Check if:
# 1. Pub/Sub quotas exceeded
gcloud pubsub subscriptions describe erlang-autonomics-signals

# 2. Network latency spike
# Check GCP monitoring dashboard

# 3. Pub/Sub backlog growing
# (means we're not keeping up with message rate)

# TEMPORARY FIX: Increase message processing timeout
# (config change, requires restart)

# PERMANENT FIX: Optimize message processing, or scale up workers
```

**If "out_of_memory" errors:**
```bash
# Memory leak. Check:
# 1. What's consuming memory?
gcloud logging read "resource.type=cloud_run_revision" --limit=200 \
  --format=json | jq '.[] | .jsonPayload.memory_used_mb'

# 2. Is it growing over time?
# (collect samples every minute for 10 min)

# TEMPORARY: Restart service (kill instance, let new one start)
# gcloud run services update-traffic tai-autonomics \
#   --region=us-central1 --to-revisions=CURRENT_REVISION=0
# (takes traffic off, forces new instance)

# PERMANENT: Code review for memory leaks
# - Unbounded lists/queues?
# - Caching without eviction?
# - Background processes not stopping?
```

**If "rate_limit_exceeded" errors:**
```bash
# Too much traffic. Check:
# 1. Traffic spike?
gcloud monitoring time-series list --filter='metric.type="run.googleapis.com/request_count"'

# 2. Legitimate traffic or attack?
# Check: Are requests from known customers or random IPs?

# TEMPORARY: Scale up (increase max instances)
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --max-instances=10

# PERMANENT: Rate limit per API key
# Add middleware to enforce per-customer quota
```

### Verify Fix

```bash
# Monitor error rate for 5 minutes
# Should drop below 1% within 5 min
gcloud logging read "resource.type=cloud_run_revision AND
  severity=ERROR" --limit=10 --tail

# Make test API call
curl -X POST https://api.tai.ai/marketplace \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "test-001"}'

# Should succeed
```

---

## SCENARIO 3: Response Time Spike (p95 >1000ms)

**Alert Trigger:** p95 latency >1000ms for 5 minutes

### Symptoms
- Customers report: "API calls are taking 10+ seconds"
- Service still responding, but slow
- Some calls timeout (>60s)

### Quick Assessment

```bash
# Check which endpoint is slow
gcloud monitoring read-timeseries \
  --filter='metric.type="run.googleapis.com/request_latencies" AND
  resource.type="cloud_run_revision"' \
  --format=json | jq '.[] | .metric.labels.method' | sort | uniq -c

# Is it all endpoints or specific one?
# Check CPU/memory - are we hitting limits?
gcloud run services describe tai-autonomics --region=us-central1 \
  --format="table(status.conditions[].message)"

# Common causes:
# 1. Dependency is slow (Pub/Sub, Firestore, Stripe taking 10s)
# 2. CPU at 100% (compute-bound request)
# 3. Memory at 100% (slow allocation, garbage collection)
# 4. Database query slow (lock contention, index missing)
```

### Investigation Steps

**Step 1: Identify bottleneck**
```bash
# If using Datadog APM, trace slow requests
# Filter: response_time > 1000ms
# Look at: Which service called? How long did it take?
# Example trace might show:
# - HTTP request: 0-50ms
# - Auth check: 50-100ms
# - Firestore query: 100-5000ms  <- SLOW
# - Response serialization: 5000-10000ms  <- SLOW

# OR check logs for slow operations
gcloud logging read "resource.type=cloud_run_revision" --limit=50 \
  --format=json | jq '.[] | select(.jsonPayload.duration_ms > 1000)'
```

**Step 2: Common fixes**

**If Firestore query slow:**
```bash
# 1. Add index
gcloud firestore indexes create --collection=receipts \
  --field-indexes=timestamp:Descending

# 2. Or simplify query
# Instead of: Query all receipts, filter by timestamp range
# Do: Query receipts with partition key first, then filter

# 3. Or optimize pagination
# Limit: return top 10 instead of all 1000

# TEMPORARY: Cache responses for 1 minute
# PERMANENT: Add database index or optimize query logic
```

**If CPU at 100%:**
```bash
# Scale up: Increase memory (also increases CPU allocation)
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --memory=1Gi \
  --max-instances=5

# Profile: What's using CPU?
# (Would need to add profiling to code - defer to post-launch)
```

**If Stripe payment processing slow:**
```bash
# 1. Check Stripe API status
# https://status.stripe.com

# 2. If Stripe slow, add timeout
# def charge_with_timeout():
#   try:
#     charge = stripe.Charge.create(..., timeout=5)
#   except stripe.error.Timeout:
#     # Cache result, retry later async
#     enqueue_retry_job(customer_id)
#     return "queued"

# TEMPORARY: Use cached pricing if Stripe times out
# PERMANENT: Implement async payment processing
```

### Verify Fix

```bash
# Monitor p95 latency for 5 minutes
# Should return to <500ms baseline

# Make test API call
time curl -X POST https://api.tai.ai/marketplace \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "test-001"}'

# Should complete in <1 second
```

---

## SCENARIO 4: Payment Processing Failures (Stripe Down)

**Alert Trigger:** Payment failures spike to >10/hour OR Stripe API returns 5xx errors

### Symptoms
- Customers sign up but can't add payment method
- Charges fail silently
- Error messages: "Connection refused" or "Service unavailable"

### Quick Assessment

```bash
# Check Stripe API status
curl https://status.stripe.com/api/v2/incidents.json | jq '.incidents[] | {title, status}'

# If Stripe status is operational:
# Check your integration
gcloud logging read "resource.type=cloud_run_revision AND
  textPayload~'stripe' AND severity=ERROR" --limit=50

# Common errors:
# 1. "Connection refused" - Network issue
# 2. "401 Unauthorized" - API key wrong
# 3. "429 Too Many Requests" - Rate limit hit
# 4. "503 Service Unavailable" - Stripe having issues
```

### Fixes

**If Stripe API is down:**
```bash
# 1. Check status.stripe.com (confirmed down)
# 2. Set GRACEFUL DEGRADATION mode
#    - Customers can sign up (email + app env)
#    - Payment NOT processed
#    - Email sent: "We'll process your payment when systems recover"
#    - Scheduled job retries every 5 minutes

# Implementation:
# In payment handler, catch Stripe timeout/unavailable:
# try {
#   charge = stripe.create_charge(...)
# } catch stripe.ServiceUnavailable {
#   enqueue_async_retry(customer_id, charge_details)
#   log "GRACEFUL_DEGRADE: Payment queued for retry"
#   return {success: false, reason: "service_recovering", retry_after: 300}
# }

# 3. Notify customers
# Status page: "Stripe integration temporarily unavailable,
#   payment processing paused. Will resume when recovered."

# 4. Monitor Stripe status for recovery
# When recovered: Retry all queued payments automatically
```

**If YOUR API key is wrong:**
```bash
# Check if key was rotated
# gcloud secrets list | grep stripe

# Verify current key in env
echo $STRIPE_API_KEY | head -c 10  # Should start with sk_test_ or sk_live_

# If wrong: Update in GCP Secret Manager
gcloud secrets versions add stripe-api-key --data-file=<(echo "sk_test_correct_key")

# Redeploy Cloud Run to pick up secret
gcloud run deploy tai-autonomics --region=us-central1 \
  --image=gcr.io/[PROJECT]/tai-autonomics:latest
```

**If rate-limited by Stripe:**
```bash
# Implement exponential backoff
# First retry: 5 seconds
# Second retry: 25 seconds
# Third retry: 125 seconds
# After 3 retries: Manual review

# Also: Check if you're making redundant calls
# (calling create_charge 3 times for same customer?)
# Deduplicate using idempotency key

stripe.create_charge({
  idempotency_key: f"customer_{customer_id}_invoice_{invoice_id}",
  amount: 2500,
  ...
})
# Stripe returns same charge if retried with same idempotency key
```

### Verify Fix

```bash
# 1. Make test payment
curl -X POST https://api.tai.ai/marketplace \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "customer_id": "test-payment-001",
    "stripe_token": "tok_visa"
  }'

# Should get: 200 OK, charge_id in response

# 2. Verify in Stripe dashboard
# https://dashboard.stripe.com -> Payments
# Should see charge created 10 seconds ago

# 3. Monitor error rate
# Should drop to 0 within 5 minutes
```

---

## SCENARIO 5: Database (Firestore) Unavailable

**Alert Trigger:** Firestore errors spike OR write latency >5000ms

### Symptoms
- Receipts not persisting
- Customer data not saved
- Errors: "Firestore unavailable" or "Permission denied"

### Quick Assessment

```bash
# Check Firestore status
gcloud firestore databases list --format="table(name, state)"
# Should show: state = RUNNING

# Check if credentials are valid
gcloud auth list
# Should show: account is active

# Check if service account has permission
gcloud projects get-iam-policy [PROJECT_ID] \
  --flatten="bindings[].members" \
  --filter="bindings.role:roles/datastore.user"

# Check Firestore API enabled
gcloud services list --enabled | grep firestore
```

### Fixes

**If Firestore service is down:**
```bash
# 1. Wait for recovery (Google will notify)
# 2. Enable GRACEFUL DEGRADATION:
#    - Customers can still use API
#    - Receipts cached in application memory (ETS in Erlang)
#    - When Firestore recovers: batch-write all cached receipts

# Implementation in code:
# try {
#   write_receipt_to_firestore(receipt)
# } catch firestore.Unavailable {
#   cache_receipt_in_memory(receipt)  # ETS table
#   enqueue_firestore_retry(receipt, retry_after: 60)
#   log "GRACEFUL_DEGRADE: Receipt cached, will persist when DB available"
# }

# 3. Status page: "Database temporarily unavailable,
#    all data will be persisted when recovered"

# 4. Monitor: When Firestore recovers, flush cache
background_job() {
  while true:
    if is_firestore_available():
      cached_receipts = get_cached_receipts()
      for receipt in cached_receipts:
        write_to_firestore(receipt)
        remove_from_cache(receipt)
      log f"Flushed {len(cached_receipts)} cached receipts"
    sleep(10)
}
```

**If credential/permission issue:**
```bash
# Verify service account has Datastore User role
PROJECT_ID=$(gcloud config get-value project)
SERVICE_ACCOUNT="tai-autonomics@${PROJECT_ID}.iam.gserviceaccount.com"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SERVICE_ACCOUNT}" \
  --role="roles/datastore.user"

# Restart Cloud Run to use new credentials
gcloud run services update tai-autonomics --region=us-central1
```

**If Firestore quota exceeded:**
```bash
# Check current usage
gcloud firestore monitor --help  # (not available, manual check)

# In Firestore console:
# - Click on Database
# - Scroll to "Usage"
# - Check: Reads, writes, deletes

# If quota exceeded:
# 1. Temporarily reduce write frequency
#    (batch receipts, write every 10 sec instead of every 1 sec)
# 2. Request quota increase
# 3. Monitor until quota resets (usually daily)

# Batch write implementation:
receipt_queue = []
def record_receipt(receipt):
  receipt_queue.append(receipt)
  if len(receipt_queue) >= 100 or time_since_last_batch > 10:
    batch_write_to_firestore(receipt_queue)
    receipt_queue = []
```

### Verify Fix

```bash
# 1. Make API call with record creation
curl -X POST https://api.tai.ai/marketplace \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "test-001"}'

# Should return: 200 OK

# 2. Verify in Firestore
# gcloud firestore databases list
# Click on "Databases" -> "receipts" collection
# Should see new document created

# 3. Check Firestore query latency
# Should be <1000ms
```

---

## SCENARIO 6: Crash on Specific Input (Certain Requests Cause 500)

**Alert Trigger:** Spike in 500 errors + pattern in request parameters

### Symptoms
- Most requests work fine
- Specific request causes 500
- Error: "Internal Server Error"
- Pattern: Always fails for customer X, but works for Y

### Quick Assessment

```bash
# Find the failing request
gcloud logging read "resource.type=cloud_run_revision AND
  httpRequest.status >= 500" --limit=50 --format=json | \
  jq '.[] | {
    path: .httpRequest.requestUrl,
    method: .httpRequest.requestMethod,
    status: .httpRequest.status,
    customer_id: .jsonPayload.customer_id,
    error: .jsonPayload.error_message
  }'

# Look for pattern: What's different about failing requests?
# Common patterns:
# - Large input: customer_id has 500 characters
# - Special characters: name contains emoji or quotes
# - Null values: optional field is null, code assumes non-null
# - Numeric: value is negative, code assumes positive
```

### Fixes

**Step 1: Reproduce the bug**
```bash
# You found the pattern: crashes when customer_id has 500 chars
# Reproduce locally:

curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "customer_id": "'$(python3 -c 'print("x" * 500)')'"
  }'

# Should reproduce the 500 error
```

**Step 2: Find the bug in code**
```bash
# Check logs for stack trace
gcloud logging read "resource.type=cloud_run_revision AND
  httpRequest.status = 500 AND
  jsonPayload.customer_id~'x{500}'" \
  --limit=1 --format=json | jq '.[] | .jsonPayload.error_trace'

# Stack trace might show:
# at marketplace.erl:234 in format_receipt
# String concatenation failed: expected <256 chars, got 500

# Root cause: Code assumes customer_id <256 chars
# String field not validated on input
```

**Step 3: Fix**
```erlang
% In marketplace.erl, add input validation:

-spec validate_customer_id(string()) -> ok | {error, string()}.
validate_customer_id(CustomerId) ->
    if
        length(CustomerId) > 256 ->
            {error, "customer_id must be <256 characters"};
        length(CustomerId) < 1 ->
            {error, "customer_id required"};
        true ->
            ok
    end.

% In handle_marketplace_request:
case validate_customer_id(CustomerId) of
    ok ->
        % process request
    {error, Reason} ->
        {400, #{error => Reason}}  % Return 400 Bad Request, not 500
end.
```

**Step 4: Test the fix**
```bash
# Recompile
rebar3 compile

# Unit test
rebar3 eunit

# Smoke test locally
curl -X POST http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "'$(python3 -c 'print("x" * 500)')'"}'

# Should return 400 Bad Request, not 500
```

**Step 5: Deploy fix**
```bash
# Build and deploy
rebar3 release
gcloud run deploy tai-autonomics --region=us-central1 \
  --source=.

# Verify: Make request again
curl -X POST https://api.tai.ai/marketplace \
  -H "Content-Type: application/json" \
  -d '{"customer_id": "'$(python3 -c 'print("x" * 500)')'"}'

# Should return 400 with error message
```

---

## SCENARIO 7: External Dependency Unavailable (Pub/Sub Down)

**Alert Trigger:** Pub/Sub subscription errors spike OR acknowledgement timeout

### Symptoms
- Autonomic signal processing stops
- Customers see: "Unable to process signal" or delays
- Error messages reference Pub/Sub

### Quick Assessment

```bash
# Check Pub/Sub subscription
gcloud pubsub subscriptions describe erlang-autonomics-signals \
  --format="table(name, state, backlog_bytes, oldest_unacked_age, num_undelivered_messages)"

# Check subscription dead letter queue (if configured)
gcloud pubsub subscriptions list | grep -i dlq

# Common issues:
# 1. Subscription in "RESOURCE_ERROR" state - fix permissions
# 2. Backlog growing - message processing too slow
# 3. old_unacked_age > 600s - messages not acknowledged
```

### Fixes

**If subscription in RESOURCE_ERROR state:**
```bash
# Verify service account has permission
PROJECT_ID=$(gcloud config get-value project)
SERVICE_ACCOUNT="tai-autonomics@${PROJECT_ID}.iam.gserviceaccount.com"

# Grant permission
gcloud pubsub subscriptions add-iam-policy-binding erlang-autonomics-signals \
  --member="serviceAccount:${SERVICE_ACCOUNT}" \
  --role="roles/pubsub.subscriber"

# Verify subscription recovers
gcloud pubsub subscriptions describe erlang-autonomics-signals \
  --format="table(state)" --wait
```

**If backlog growing (message processing slow):**
```bash
# 1. Check: Is Cloud Run processing messages?
gcloud logging read "resource.type=cloud_run_revision AND
  textPayload~'Received.*signal'" --limit=20

# 2. If no logs: Cloud Run not receiving messages
#    (permission issue, subscription misconfigured)

# 3. If logs exist but sparse: Cloud Run processing slow
#    Check logs for individual message latency:
gcloud logging read "resource.type=cloud_run_revision" --limit=100 \
  --format=json | jq '.[] | select(.jsonPayload.message_processing_ms > 100)'

# 4. If messages take >5 seconds to process:
#    Optimize message processing (see code profiling)
#    Or scale up (more Cloud Run instances)

# Scale up:
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --max-instances=10
```

**If Pub/Sub service is down:**
```bash
# Check https://status.cloud.google.com

# While down:
# 1. Disable message processing (don't crash trying to connect)
# 2. Queue signals in-memory
# 3. When Pub/Sub recovers: Process queued signals

# Implementation:
try {
  message = pubsub_client.pull(subscription)
  process_message(message)
  pubsub_client.acknowledge(subscription, message)
except pubsub.Unavailable:
  # Queue in memory
  local_queue.append(signal_from_request)
  return {status: "queued"}
```

### Verify Fix

```bash
# 1. Publish test message
gcloud pubsub topics publish erlang-autonomics \
  --message='{"signal": "test", "timestamp": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'"}'

# 2. Check Cloud Run logs
sleep 5
gcloud logging read "resource.type=cloud_run_revision AND
  textPayload~'Received.*signal'" --limit=5 --tail

# Should see test signal processed

# 3. Check backlog is not growing
gcloud pubsub subscriptions describe erlang-autonomics-signals \
  --format="table(num_undelivered_messages)"

# Should be close to 0
```

---

## SCENARIO 8: High Memory Usage / OOM Kill

**Alert Trigger:** Memory usage >90% sustained for 5 minutes

### Symptoms
- Service becomes slow, then crashes
- Errors: "Out of memory" (if you see them)
- Container restarts repeatedly

### Quick Assessment

```bash
# Check memory usage over time
gcloud monitoring read-timeseries \
  --filter='metric.type="run.googleapis.com/container/memory/working_set_bytes"' \
  --format=json | jq '.[] | .points[] | {interval: .interval, value: .value.int64_value}'

# Check if memory is growing (leak) or stable
# Growing = memory leak
# Stable at high level = underprovisioned

# Check restart count
gcloud run revisions list --service=tai-autonomics --region=us-central1 \
  --format="table(name, status.conditions[0].message)"
```

### Fixes

**If memory growing (leak):**
```bash
# 1. Restart service (temporary fix)
gcloud run services update-traffic tai-autonomics \
  --region=us-central1 \
  --to-revisions=CURRENT=0
# This removes all traffic, forces new instance to start

# 2. Investigate memory leak
# Common causes in Erlang:
# - Atoms being created dynamically (atom table never garbage collected)
# - Large binary being retained in message queue
# - ETS table growing unbounded (no eviction policy)
# - Process accumulating state (should use gen_statem cleanup)

# Check ETS tables:
gcloud logging read "resource.type=cloud_run_revision" --limit=100 \
  --format=json | jq '.[] | select(.jsonPayload.ets_size_mb)'

# If ETS growing: Add maximum size limit
# Example: limit receipts table to 100K entries (with LRU eviction)

# 3. Add memory monitoring
# Every minute, log: {timestamp, memory_used_mb, ets_sizes, process_counts}
# If memory usage >400MB: Restart service gracefully
```

**If underprovisioned (stable but high):**
```bash
# Increase memory allocation
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --memory=1Gi  # Increase from 512Mi to 1Gi

# This gives more headroom and more CPU (bonus)

# Monitor: Does service stabilize?
gcloud monitoring read-timeseries \
  --filter='metric.type="run.googleapis.com/container/memory/working_set_bytes"' \
  --format=json | jq '.[] | .points[0].value.int64_value' | tail -1
# Should be <50% of 1Gi (0.5Gi)
```

### Verify Fix

```bash
# 1. Monitor memory for 5 minutes
for i in {1..10}; do
  gcloud monitoring read-timeseries \
    --filter='metric.type="run.googleapis.com/container/memory/working_set_bytes"' \
    --format=json | jq '.[] | .points[0].value.int64_value'
  sleep 30
done

# Should be stable, not growing

# 2. Check restart count hasn't increased
gcloud run revisions list --service=tai-autonomics --region=us-central1 \
  --limit=3 --format="table(name, createTime)"

# Should only show 1 new revision (not multiple restarts)
```

---

## SCENARIO 9: Rate Limiting / DDoS Attack

**Alert Trigger:** Request rate >1000 req/sec OR spike in 429 errors

### Symptoms
- Legitimate customers getting 429 (Too Many Requests)
- Legitimate customers experiencing slow responses
- Dashboard shows request rate 10x normal

### Quick Assessment

```bash
# Check request rate by source IP
gcloud logging read "resource.type=cloud_run_revision AND
  httpRequest.status = 200" --limit=1000 --format=json | \
  jq '.[] | .httpRequest.userAgent' | sort | uniq -c | sort -rn | head -20

# Check if legitimate traffic or attack
# Legitimate: Different user agents, normal API usage patterns
# Attack: Same user agent, repeated requests to /marketplace, random customer_ids

# Check request rate
gcloud monitoring read-timeseries \
  --filter='metric.type="run.googleapis.com/request_count"' \
  --format=json | jq '.[] | .points[0].value.int64_value'
```

### Fixes

**If legitimate traffic spike (announcement, press, viral):**
```bash
# Scale up automatically
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --min-instances=5 \
  --max-instances=100

# Cloud Run will auto-scale up to handle the traffic

# Monitor: Let it scale
# Should handle 10x traffic without degradation

# Once traffic subsides: Scale down again
gcloud run services update tai-autonomics \
  --region=us-central1 \
  --min-instances=1 \
  --max-instances=10
```

**If DDoS attack (suspicious pattern):**
```bash
# 1. Enable Cloud Armor (GCP built-in DDoS protection)
# Already configured if using Cloud Run with Load Balancer

# 2. Block suspicious IPs
gcloud logging read "resource.type=cloud_run_revision AND
  httpRequest.status = 429" --limit=100 --format=json | \
  jq '.[] | .httpRequest.clientIp' | sort | uniq -c | sort -rn

# If one IP >50 requests in 1 minute, likely attacker
ATTACKER_IP="203.0.113.42"

# Add to Cloud Armor blocklist (if configured)
# OR manually respond with 403:

# In Erlang middleware, add:
-spec block_attacker_ips(string()) -> boolean().
block_attacker_ips(ClientIp) ->
    BlockedIps = ["203.0.113.42", "203.0.113.43"],
    lists:member(ClientIp, BlockedIps).

% In handle_request:
case block_attacker_ips(ClientIp) of
    true -> {403, "Access denied"};
    false -> process_request(...)
end.

# 3. Or use rate limiting per API key
% In handle_request:
case check_rate_limit(ApiKey) of
    {ok, remaining} ->
        process_request(...),
        {200, Response}
    {exceeded, retry_after} ->
        {429, #{retry_after => retry_after, message => "Rate limit exceeded"}}
end.
```

### Verify Fix

```bash
# 1. Monitor request rate
# Should return to normal (or new baseline if legitimate surge)

# 2. Make test request
curl https://api.tai.ai/marketplace \
  -H "Authorization: Bearer $API_KEY" \
  -X POST \
  -d '{"customer_id": "test-001"}'

# Should return 200 OK (not 429)

# 3. Check error rate
# Should be <1%
```

---

## SCENARIO 10: Payment Receipt Ledger Corruption

**Alert Trigger:** Receipt hash verification fails OR duplicate receipts detected

### Symptoms
- Audit trail shows duplicate entries
- Receipt chain is broken (receipt[N].previous_hash != hash(receipt[N-1]))
- Finance audit fails

### Quick Assessment

```bash
# Verify receipt chain integrity
gcloud firestore databases list --format=value | while read db; do
  gcloud firestore export gs://backup-${RANDOM}
done

# Or locally:
# Connect to Firestore emulator
FIRESTORE_EMULATOR_HOST=localhost:8081 erlang_client query_all_receipts()

# Check for duplicates
gcloud logging read "resource.type=cloud_run_revision AND
  jsonPayload.receipt_id" --limit=10000 --format=json | \
  jq '.[] | .jsonPayload.receipt_id' | sort | uniq -d | head -20

# If duplicates found: Someone created receipts twice for same action
```

### Fixes

**If duplicate receipts:**
```bash
# 1. Identify the duplicates
RECEIPT_ID="receipt_12345"

gcloud firestore databases export gs://backup-repair \
  --async

# 2. Manual remediation (if small number)
# Delete duplicate receipts from Firestore:
gcloud firestore databases delete --database-id=receipts

# 3. Restore from backup
gcloud firestore databases restore \
  --source=gs://backup-before-corruption \
  --destination-database=receipts

# 4. Verify chain integrity
gcloud logging read "resource.type=cloud_run_revision" --limit=100 \
  --format=json | jq '.[] | {receipt_id, hash, prev_hash}' | \
  python3 verify_chain.py

# (verify_chain.py checks that each receipt[N].prev_hash == hash(receipt[N-1]))
```

**If chain is broken:**
```bash
# Root cause: Someone inserted a receipt out of order
# Or: Someone modified a receipt (hash changed)

# To fix:
# 1. Identify where chain broke
#    Find receipt[N] where receipt[N].prev_hash != hash(receipt[N-1])

# 2. Investigate: What changed?
#    - Was it a legitimate transaction?
#    - Or malicious tampering?
#    - Or bug in receipt generation?

# 3. If bug: Fix code, regenerate from source

# 4. If malicious: Restore from backup, investigate access logs

# 5. Verify: Run chain verification script

# 6. Notify customers (if personal data affected)
#    "Our audit trail was temporarily inconsistent. We've restored
#     from backup. All your transactions are accurate."
```

### Verify Fix

```bash
# 1. Run receipt chain verification
python3 verify_receipt_chain.py

# Should output:
# ✓ All receipts present
# ✓ Chain is unbroken (all hashes match)
# ✓ No duplicates found
# ✓ Timestamps in order

# 2. Verify on a few receipts manually
RECEIPT_ID="receipt_abc123"
gcloud firestore databases --database=receipts documents \
  get receipts/$RECEIPT_ID

# Check: hash, previous_hash, timestamp all present and valid

# 3. Finance audit
# Sum all revenue from receipts
gcloud firestore databases query \
  "SELECT SUM(amount) FROM receipts WHERE status='completed'"

# Compare to Stripe balance
stripe balance retrieve

# Should match (within rounding)
```

---

## SCENARIO 11: Cascading Failures (Multiple Systems Down)

**Alert Trigger:** P1 + P2 + P3 alerts firing simultaneously

### Symptoms
- Everything is broken at once
- Service down, payment failing, database slow
- Feels like the world is ending

### Root Cause Pattern
Usually one failure cascades:
- Example 1: Database slow → requests timeout → memory accumulates → Cloud Run crashes → more timeouts → payment failures
- Example 2: Pub/Sub down → queue backs up → memory grows → Cloud Run out of memory → crashes → cascading failure

### Investigation

```bash
# Check: What failed FIRST?
gcloud logging read "resource.type=cloud_run_revision" --limit=1000 \
  --format=json | jq '.[] | {time: .timestamp, severity, message}' | \
  sort -k1

# Find first error, work forward
# Example timeline:
# 14:30 - Firestore latency spike detected
# 14:31 - Cloud Run request timeout (waiting for Firestore)
# 14:32 - Memory usage grows (requests accumulating)
# 14:33 - Cloud Run OOM kill (out of memory)
# 14:34 - Service unavailable, payment failures
# 14:35 - All downstream systems fail
```

### Fix (Isolation + Recovery)

**Step 1: Stop the Cascade**
```bash
# Isolate failing component:
# If Firestore slow: Disable writes to Firestore
#   Cache in memory instead, retry when Firestore recovers

# If payment processing slow: Disable charge attempts
#   Respond "queued" and retry asynchronously

# If Pub/Sub slow: Stop pulling messages
#   Prevent message processing from consuming all memory

# Implementation:
% In each external call, add circuit breaker:
circuit_breaker(firestore, write, Receipt) ->
  case firestore:is_healthy() of
    true ->
      write_to_firestore(Receipt);
    false ->
      % Firestore unhealthy, use fallback
      cache_in_memory(Receipt),
      enqueue_retry(Receipt, retry_after=60)
  end.
```

**Step 2: Graceful Degradation**
```bash
# System can still process requests with degraded functionality

# Example: All services down
# - /health: Returns yellow (not green)
# - /marketplace: Returns cached pricing (not real-time)
# - /pubsub: Queues signals locally (doesn't process yet)

# All systems respond quickly (no timeouts)
# No cascading memory issues

# When services recover: Flush caches, process queued items
```

**Step 3: Staged Recovery**
```bash
# Don't try to recover everything at once

# 1. Restart Cloud Run (clean slate)
gcloud run deploy tai-autonomics --region=us-central1

# 2. Wait 60 seconds, verify service healthy
curl https://api.tai.ai/health

# 3. Verify Firestore accessible
gcloud firestore databases list

# 4. Verify Pub/Sub accessible
gcloud pubsub subscriptions describe erlang-autonomics-signals

# 5. Enable features one at a time:
#    - First: HTTP requests (no external calls)
#    - Second: Database reads
#    - Third: Database writes
#    - Fourth: Pub/Sub processing
#    - Fifth: Payment processing

# If any step fails: Stop, investigate, fix
```

### Verify Full System Recovery

```bash
# 1. Health check
curl https://api.tai.ai/health
# {status: "healthy", db: "connected", pubsub: "connected"}

# 2. Make API call with all features
curl -X POST https://api.tai.ai/marketplace \
  -H "Authorization: Bearer $API_KEY" \
  -d '{"customer_id": "test-full-001"}'

# Should return 200 with receipt

# 3. Monitor for 10 minutes
# Error rate: Should be <1%
# Memory: Should be <300MB (not growing)
# Response time: Should be <500ms
# Pub/Sub: Should be processing messages

# 4. Validate ledger integrity
python3 verify_receipt_chain.py
# Should show: All checks passed

# 5. Check customer impact
# Email support: Any complaints in last hour?
# Stripe: Any failed charges?
# Dashboard: Revenue tracking correctly?
```

---

## Quick Reference: Command Cheat Sheet

### Monitoring & Diagnostics

```bash
# Check service status
gcloud run services describe tai-autonomics --region=us-central1

# View recent errors (last 50)
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" \
  --limit=50 --format=short

# View error spike
gcloud logging read "resource.type=cloud_run_revision AND
  httpRequest.status >= 500" --limit=100 --format=short

# Check request latency distribution
gcloud monitoring read-timeseries \
  --filter='metric.type="run.googleapis.com/request_latencies"' \
  --format=json | jq '.[] | .points[0]'

# View all metrics
gcloud monitoring metrics-descriptors list | grep run.googleapis.com
```

### Deployment & Rollback

```bash
# Deploy latest
gcloud run deploy tai-autonomics --region=us-central1 --source=.

# See deployment history
gcloud run revisions list --service=tai-autonomics --region=us-central1

# Rollback to previous revision
gcloud run services update-traffic tai-autonomics \
  --region=us-central1 \
  --to-revisions=PREVIOUS_REVISION_ID=100
```

### Database Operations

```bash
# Export Firestore backup
gcloud firestore export gs://backup-bucket

# Restore from backup
gcloud firestore restore --source=gs://backup-bucket

# Query Firestore directly
gcloud firestore databases query "SELECT * FROM receipts LIMIT 10"
```

### Incident Communication

```bash
# Post to Slack
curl -X POST $SLACK_WEBHOOK_URL \
  -d '{"text": "Service down: error rate spiked. Investigating..."}'

# Update status page (via API if available)
curl -X PATCH https://api.statuspage.io/v1/pages/$PAGE_ID/incidents/$INCIDENT_ID \
  -H "Authorization: OAuth $STATUS_PAGE_TOKEN" \
  -d '{"incident": {"status": "investigating"}}'
```

---

## Post-Incident Review Template

```markdown
# Incident Post-Mortem: [SERVICE NAME] [DATE]

## Executive Summary
[1-2 sentences about what happened and impact]

## Timeline
| Time | Event |
|------|-------|
| 14:32 | Alert fires: error rate >10% |
| 14:35 | On-call engineer acknowledges |
| 14:40 | Root cause identified: [cause] |
| 14:47 | Fix deployed to production |
| 14:50 | Verification complete, all clear |

## Impact
- **Duration**: 15 minutes
- **Customers Affected**: All / [N customers]
- **Revenue Impact**: $[X] (estimated)
- **Data Loss**: None / [Details if any]

## Root Cause
[Detailed explanation of what caused the incident]

## Detection & Response
- **Detected by**: [Alert name / Manual report]
- **Detection lag**: [X minutes] (from incident start to alert)
- **Response time**: [X minutes] (from alert to fix start)

## What We Did Well
1. [Praise for team]
2. [Praise for process]

## What We'll Improve
1. [Action item] - Owner: [Name], Due: [Date]
2. [Action item] - Owner: [Name], Due: [Date]
3. [Action item] - Owner: [Name], Due: [Date]

## Followup Actions
- [ ] [Action 1] - [Owner] by [Date]
- [ ] [Action 2] - [Owner] by [Date]
- [ ] [Action 3] - [Owner] by [Date]

## Attendees
- [Person 1] - Role
- [Person 2] - Role
- [Person 3] - Role
```

---

## Escalation Contacts

```
PRIMARY ON-CALL ENGINEER
Name: ___________
Phone: +1-___________
Slack: @___________

SECONDARY ON-CALL ENGINEER
Name: ___________
Phone: +1-___________
Slack: @___________

VP PRODUCT / MANAGER
Name: ___________
Phone: +1-___________
Email: ___________

CEO (Emergency)
Name: ___________
Phone: +1-___________
Email: ___________

VENDOR CONTACTS
Stripe Support: https://support.stripe.com
GCP Support: https://cloud.google.com/support
Datadog Support: https://support.datadoghq.com
```

---

**This runbook is a living document. Update based on actual incidents.**

