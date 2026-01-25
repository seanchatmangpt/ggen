<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Incident Response Runbook for TPS Systems](#incident-response-runbook-for-tps-systems)
  - [Quick Incident Checklist](#quick-incident-checklist)
  - [Incident Types](#incident-types)
    - [Type 1: Service Unavailable (Severity 1 - CRITICAL)](#type-1-service-unavailable-severity-1---critical)
      - [Diagnosis: Service Unavailable](#diagnosis-service-unavailable)
      - [Immediate Actions: Service Unavailable](#immediate-actions-service-unavailable)
      - [Rollback Procedure (if deployment caused outage)](#rollback-procedure-if-deployment-caused-outage)
      - [Escalation: Service Unavailable](#escalation-service-unavailable)
    - [Type 2: Performance Degradation (Severity 2 - HIGH)](#type-2-performance-degradation-severity-2---high)
      - [Diagnosis: Performance Degradation](#diagnosis-performance-degradation)
      - [Immediate Actions: Performance Degradation](#immediate-actions-performance-degradation)
      - [Escalation: Performance Degradation](#escalation-performance-degradation)
    - [Type 3: Data Loss (Severity 1-2 - CRITICAL/HIGH)](#type-3-data-loss-severity-1-2---criticalhigh)
      - [Diagnosis: Data Loss](#diagnosis-data-loss)
      - [Immediate Actions: Data Loss](#immediate-actions-data-loss)
    - [Type 4: Cascading Failure (Severity 1 - CRITICAL)](#type-4-cascading-failure-severity-1---critical)
      - [Immediate Actions: Cascading Failure](#immediate-actions-cascading-failure)
  - [Post-Incident Workflow](#post-incident-workflow)
    - [Incident Classification & Severity](#incident-classification--severity)
    - [Post-Incident Review (24-72 hours later)](#post-incident-review-24-72-hours-later)
  - [Communication During Incident](#communication-during-incident)
    - [Status Updates](#status-updates)
    - [Post-Incident Communication](#post-incident-communication)
  - [Incident Metrics](#incident-metrics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Incident Response Runbook for TPS Systems

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: On-call engineers, incident commanders
**Severity Levels**: 1=Critical, 2=High, 3=Medium, 4=Low

---

## Quick Incident Checklist

**Time: 0-5 minutes - Triage & Assessment**
- [ ] Verify there IS an incident (not false alarm)
- [ ] Assess severity level (1-4)
- [ ] Page on-call if Severity 1 or 2
- [ ] Gather initial metrics (queue, error rate, latency, circuit status)
- [ ] Identify affected users/services
- [ ] Start incident in incident tracking system (Slack thread, PagerDuty, etc.)
- [ ] Assign incident commander (usually whoever detected it)

**Time: 5-15 minutes - Stabilize & Diagnose**
- [ ] Stabilize system (stop bleeding): Circuit open? Queue full? Error rate 100%?
- [ ] Run diagnostics: Check logs, traces, metrics
- [ ] Form hypothesis of root cause
- [ ] Assign investigation tasks to team members

**Time: 15-60 minutes - Fix & Verify**
- [ ] Implement fix based on diagnosis
- [ ] Deploy fix or workaround
- [ ] Monitor metrics for improvement
- [ ] Verify SLOs are met again

**Time: After - Investigate & Learn**
- [ ] Root cause analysis
- [ ] Document what happened
- [ ] Identify prevention for next time

---

## Incident Types

### Type 1: Service Unavailable (Severity 1 - CRITICAL)

**Definition**: Service is not responding, users cannot use the system

**Detection Metrics**:
- Error rate: 100% (all requests fail)
- Circuit breaker: Open for > 5 minutes
- Queue: 0 depth (no work being processed)
- Throughput: 0 req/sec
- Alerts: "Service Unavailable" or "All Requests Failing"

**Immediate Impact**:
- Users cannot use system
- Revenue is at risk (if SaaS)
- Data might be lost (if transactions not processing)

---

#### Diagnosis: Service Unavailable

**Question 1: Is my service broken or downstream?**

Check circuit breaker:
```
IF circuit breaker is OPEN for main service:
  → My service is broken
ELSE IF circuit breaker is OPEN for downstream:
  → Downstream service is broken
ELSE IF circuit breaker is CLOSED:
  → Neither service is broken (network issue? configuration?)
```

**Question 2: If MY service is broken, where is the failure?**

Check logs:
```
Error logs should show:
  - What part of code failed
  - Why it failed (null pointer, out of memory, disk full, etc.)

Check traces:
  - Which service in the chain returned an error?
  - At what point did the error occur?
```

**Question 3: If downstream service is broken, what happened to it?**

Contact downstream team:
```
- Can they see their service is broken?
- When did it break?
- Are they working on fixing it?
- Do you need to implement a fallback?
```

---

#### Immediate Actions: Service Unavailable

**Step 1: Declare Incident (< 1 minute)**
```
- Create incident in tracking system
- Page on-call (if not already on call)
- Post status: "We are investigating service outage"
- Set expectation: "Updates every 5 minutes"
```

**Step 2: Gather Info & Assess (1-3 minutes)**
```
1. Check circuit breaker:
   - Open? → Service unhealthy or downstream broken
   - Closed? → Network issue or configuration

2. Check logs for errors:
   - grep for "ERROR" or "FATAL" in service logs
   - Check system logs (out of disk, out of memory?)

3. Check resource usage:
   - CPU at 100%?
   - Memory at 100%?
   - Disk at 100%?
   - Network at 100%?

4. Check external dependencies:
   - Can you reach database?
   - Can you reach downstream service?
   - Are they responding?
```

**Step 3: Stabilize (3-5 minutes)**

If service is broken:
```
1. Check for obvious fix:
   - Did something just deploy? → Rollback
   - Did configuration change? → Revert config
   - Is there an obvious error? → Fix it
   - Otherwise, restart service as emergency measure
```

If downstream is broken:
```
1. Implement fallback:
   - Disable call to broken service
   - Return cached response (if available)
   - Return error with clear message
   - This allows your service to stay up

2. If no fallback available:
   - Check if circuit breaker can auto-recover
   - If service is recovering, wait
   - If service is not recovering, possible manual intervention
```

**Step 4: Root Cause Analysis (while stabilizing)**
```
1. Check git log: Did something deploy in the last 30 minutes?
2. Check infrastructure: Did anything change? (deployment, config, scaling)
3. Check metrics: Did anything spike when outage started?
4. Check logs: What's the first error message?
```

**Step 5: Verify Fix (5-10 minutes)**
```
1. Service should be responding
2. Error rate should drop to < 1%
3. Requests should be going through
4. Circuit breaker should be recovering (closing)
5. Users should be able to access system again
```

---

#### Rollback Procedure (if deployment caused outage)

```
IF outage correlates with deployment:
  1. Identify problematic version (git log, deployment log)
  2. Get previous version number
  3. Trigger rollback:
     - Kubernetes: kubectl rollout undo [deployment]
     - Docker: docker-compose up with old image
     - Or manually: Stop new version, start old version
  4. Monitor: Service should come back up within 2 minutes
  5. Verify: Error rate returns to < 0.1%, latency normal
```

---

#### Escalation: Service Unavailable

**When to escalate**:
- Service down for > 5 minutes and you can't fix it
- Multiple attempts to fix have failed
- Unclear what the problem is

**Escalation path**:
1. Page on-call engineer (if you haven't already)
2. Page engineering manager (if on-call engineer can't help)
3. Page director (if problem affects multiple services)
4. Consider customer communication: What will you tell them?

---

### Type 2: Performance Degradation (Severity 2 - HIGH)

**Definition**: System is responding but much slower than normal

**Detection Metrics**:
- Latency p99: 2-10x baseline
- Error rate: Still low (< 0.1%)
- Queue: Filling up but being drained
- Throughput: Declining
- Users notice: "Your service is slow"

**Immediate Impact**:
- User experience degraded
- Might violate SLOs
- Not critical (better than unavailable)

---

#### Diagnosis: Performance Degradation

**Question 1: Is it latency spike or throughput decline?**

```
IF latency is high BUT throughput is normal:
  → It's a latency problem (requests are slow)
ELSE IF latency is normal BUT throughput is declining:
  → It's a throughput problem (can't process enough)
ELSE IF both are bad:
  → System is overloaded
```

**Question 2: Where is the latency?**

Check traces:
```
1. Find a slow request in Jaeger
2. Look at each span (step in the request):
   - My Service Step 1: 2ms
   - My Service Step 2: 50ms <- This is slow!
   - My Service Step 3: 1ms
3. Focus on the slow span
4. What is it doing? Database query? Network call? Computation?
```

**Question 3: Why is that span slow?**

```
IF it's a database query:
  → Check query performance, add index, or cache result
IF it's a network call:
  → Check if downstream service is slow, add timeout, or add retries
IF it's computation:
  → Check if CPU is maxed out, or if algorithm is inefficient
IF it's waiting for a resource:
  → Check if lock contention, connection pool exhausted, or disk I/O
```

---

#### Immediate Actions: Performance Degradation

**Step 1: Declare Incident**
```
- Create incident in tracking system
- Do NOT page on-call immediately (wait 2 minutes to see if it auto-recovers)
- Post status: "We are aware of slower performance"
```

**Step 2: Gather Metrics (2-5 minutes)**
```
1. Check dashboard:
   - Latency p50/p90/p99
   - Throughput
   - Queue depth
   - Worker utilization
   - Resource usage (CPU, memory, disk)

2. Find slow span in traces:
   - Open Jaeger UI
   - Select service and filter by high latency
   - Click on a slow trace
   - Find which span takes most time

3. Check if load increased:
   - Did incoming requests increase?
   - Or is system slower with same load?
```

**Step 3: Stabilize (5-15 minutes)**

Option A: Scale workers (if CPU-bound)
```
1. Check worker utilization: Is it 100%?
2. If yes, increase worker count by 50%
3. Monitor: Does latency improve?
4. If yes, continue scaling until latency returns to normal
```

Option B: Scale downstream (if network-bound)
```
1. Check if downstream service is slow
2. If yes, contact downstream team
3. Ask them to scale or investigate
4. Implement timeout: Don't wait forever for slow service
```

Option C: Optimize code (if throughput-bound)
```
1. Find slow span in trace
2. If it's a database query, can you add cache?
3. If it's a network call, can you parallelize?
4. If it's computation, can you optimize algorithm?
5. Short-term: Deploy fix if available
6. Long-term: Add performance test to prevent regression
```

**Step 4: Verify Fix (5-10 minutes)**
```
1. Latency p99 should return to < 2x baseline
2. Throughput should increase
3. Queue should start draining
4. CPU/Memory should return to normal
5. Users should report better performance
```

---

#### Escalation: Performance Degradation

**When to escalate**:
- Latency 5x baseline and you can't identify cause (30 minutes)
- Latency not improving despite scaling (1 hour)
- Affecting > 10% of users for > 30 minutes

**Escalation path**:
1. Page on-call engineer
2. Page engineering manager (if on-call engineer can't help)
3. Consider customer communication

---

### Type 3: Data Loss (Severity 1-2 - CRITICAL/HIGH)

**Definition**: Items are disappearing from queue, not being processed, possible data loss

**Detection Metrics**:
- Queue depth: Decreasing without being processed
- Throughput: Items disappearing instead of succeeding/failing
- Alerts: "Items lost" or "Queue inconsistency"
- Dead-letter queue: Not growing (items just vanishing)

**Immediate Impact**:
- Data is lost
- Customer data might be lost
- Legal/compliance implications
- Critical incident

---

#### Diagnosis: Data Loss

**Question 1: Are items actually lost or just not visible?**

```
IF queue depth is decreasing AND nothing is being processed:
  → Items are disappearing, likely data loss
ELSE IF items are in dead-letter queue:
  → Items are failing (not lost, just failed)
ELSE IF items are somewhere but not visible:
  → Might be stuck in intermediate state
```

**Question 2: What happened to the queue backend?**

```
1. Check queue service (NATS, Kafka, RabbitMQ):
   - Is it running?
   - Is it healthy?
   - Are there any errors in its logs?
   - Disk space available?

2. Check network:
   - Can your service reach queue?
   - Are there network errors?
```

---

#### Immediate Actions: Data Loss

**Step 1: STOP THE PRESSES (< 1 minute)**
```
- This is critical incident
- Page on-call, manager, director immediately
- Create incident bridge (Slack call or Zoom)
- Prepare for customer communication
```

**Step 2: Assess Scope (1-3 minutes)**
```
1. How many items lost? (Compare queue depth now vs 1 hour ago)
2. How long have items been lost? (Check logs when loss started)
3. What kind of items? (Orders, payments, user data?)
4. Can you recover them? (Do you have backups, retries, etc.?)
```

**Step 3: Stop Data Loss (immediately)**
```
1. Disable processing (stop workers) to prevent more loss
2. Backup current state (queue dump, logs, metrics)
3. Investigate root cause while queue is frozen
4. Only resume processing once you're confident items won't be lost
```

**Step 4: Root Cause Analysis (in parallel)**
```
1. Check queue service logs: What errors?
2. Check if queue backend disk is full
3. Check if network connection to queue is failing
4. Check if there's a bug in queue client code
5. Check git log: Recent changes to queue handling?
```

**Step 5: Recovery Plan**
```
1. If queue backend is broken:
   - Restore from backup
   - Or restart queue service

2. If network connection broken:
   - Fix network issue
   - Resume processing

3. If code bug:
   - Fix bug
   - Deploy fixed version
   - Replay lost items if possible

4. Recovery steps:
   - Fix the problem
   - Verify queue is working
   - Resume processing carefully (monitor for more losses)
   - Process lost items if recoverable
```

**Step 6: Communicate with Customers**
```
- Admit problem immediately: "We had a data loss incident"
- Explain scope: "X items were affected"
- Explain cause: "Root cause was..."
- Explain recovery: "Here's how we're recovering the data"
- Timeline: "Recovery will be complete by X time"
```

---

### Type 4: Cascading Failure (Severity 1 - CRITICAL)

**Definition**: One component fails, which causes others to fail, which causes others to fail (cascade)

**Detection Metrics**:
- Circuit breaker: Open for multiple services
- Error rate: Spreading across services
- Throughput: Declining across system
- Latency: Spiking across system

**Scenario**:
```
1. Downstream Service A becomes slow
2. Your service retries, queue fills
3. Your service runs out of memory
4. Your service crashes
5. Service B can't reach your service
6. Service B circuit opens
7. Service C can't reach Service B
8. Service C circuit opens
9. Entire system down (cascade complete)
```

---

#### Immediate Actions: Cascading Failure

**Step 1: Detect Cascade (immediately)**
```
Check if multiple services are failing:
- If only your service broken: Not a cascade
- If your service + downstream broken: Might be cascade
- If your service + downstream + others: Definitely cascade
```

**Step 2: Stop the Cascade (immediately)**
```
Goal: Prevent failure from spreading

Actions:
1. Open circuit breaker to broken service (stop retrying)
2. Implement fallback (return cached response)
3. Rate limit incoming traffic (don't amplify load)
4. Scale resources (give broken service room to recover)
5. If service can't recover, disable it (let others keep running)
```

**Step 3: Recover in Order (next 15-30 minutes)**
```
Strategy: Fix the root cause service first

1. Identify root cause: Which service failed first?
   - Usually the most downstream service (farthest from users)
2. Fix that service (restart, patch, scale)
3. Monitor for recovery
4. Once recovered, check next service up the chain
5. Continue up the chain until everything is working
```

**Step 4: Prevent Cascade Next Time**
```
Improvements:
1. Add timeouts: Don't wait forever
2. Add circuit breakers: Fail fast instead of cascading
3. Add rate limiting: Don't amplify load upstream
4. Add fallbacks/caching: Can still serve if dependency fails
5. Add load leveling: Spread load evenly to prevent spikes
```

---

## Post-Incident Workflow

### Incident Classification & Severity

**After incident is resolved (still on incident call)**:

1. **Confirm All Metrics Healthy** (10 minutes)
   - Error rate < 0.1%
   - Latency p99 < SLO
   - Throughput = expected
   - Queue depth < 80%
   - Circuit breakers closed
   - All alerts green

2. **Document Incident** (15 minutes)
   - Start time: When did it start?
   - Detection time: When did we detect it?
   - Severity: How bad was it?
   - Impact: How many users? What data?
   - Duration: When was it resolved?
   - Root cause: Why did it happen?

3. **Incident Report Template**
   ```
   Incident: [Service] [Type] at [Time]

   Summary: [One line description]

   Timeline:
   - 10:00 AM: [Event that started incident]
   - 10:05 AM: [How we detected it]
   - 10:10 AM: [First action taken]
   - 10:20 AM: [Resolution deployed]
   - 10:25 AM: [Incident resolved]

   Root Cause: [Why did it happen?]

   Impact: [X users affected, Y minutes down, Z dollars lost]

   What Went Well: [Good response, good communication]

   What We Can Improve: [Slow detection, could have prevented, etc.]

   Action Items:
   1. [Prevent this next time] - Owner: [Name] - Due: [Date]
   2. [Improve detection] - Owner: [Name] - Due: [Date]
   3. [Add test case] - Owner: [Name] - Due: [Date]
   ```

### Post-Incident Review (24-72 hours later)

**Meeting**: 45-60 minutes with engineering team

**Attendees**: Incident commander, on-call engineer, affected services owners, engineering manager

**Agenda**:
1. **What Happened**: Timeline of events
2. **Root Cause**: Why did it happen (not "human error", dig deeper)
3. **Action Items**: What will we do to prevent next time?
4. **Follow-up**: Who will do it, by when?

**Ground Rules**:
- No blame: Focus on systems, not individuals
- Assume good intent: People did the best they could with info available
- Ask 5 Whys: Don't accept "operator error", understand the context

**Example 5 Whys**:
```
Q1: Why did database go down?
A1: Disk was full

Q2: Why did disk get full?
A2: Logs weren't rotating

Q3: Why weren't logs rotating?
A3: Log rotation config was wrong

Q4: Why was config wrong?
A4: No one tested it in staging

Q5: Why didn't we test it?
A5: No test coverage for log rotation config

Action: Add test coverage for critical configs
```

---

## Communication During Incident

### Status Updates

**To Stakeholders** (every 5-10 minutes):
- What's the status? (investigating, fixing, resolved)
- What do we know? (root cause, impact)
- What's next? (ETA for fix, what we're doing)
- Transparent: Don't hide bad news

### Post-Incident Communication

**To Customers**:
- Admit problem: "We had an outage"
- Apologize: "We're sorry for the impact"
- Explain: "Root cause was..."
- Action: "Here's what we're doing to prevent it"

**To Team**:
- Share incident report: "Here's what happened"
- Share action items: "Here's what we're improving"
- Celebrate: "Great response, everyone did well"

---

## Incident Metrics

**Track**:
- Detection time: How long before we noticed?
- Response time: How long before we started fixing?
- Resolution time: How long to fix?
- Communication quality: How well did we communicate?
- Prevention effectiveness: Did we prevent recurrence?

**Goal**: Improve all of these over time
- Detection time: < 2 minutes
- Response time: < 5 minutes
- Resolution time: < 30 minutes
- Communication: Timely and transparent
- Prevention: 100% of past incidents stayed fixed

---

**End of Incident Response Runbook**
