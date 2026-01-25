# TPS Operator Certification Program

**Version**: 1.0
**Last Updated**: January 2026
**Certification Duration**: 2 years (must recertify after)
**Program Owner**: TPS Operations Training

---

## Program Overview

**Purpose**: Certify operators to safely and effectively run production TPS systems.

**Structure**: Three-level certification (like pilot licenses):
- **Level 1**: Awareness (theoretical knowledge)
- **Level 2**: Basic Operations (hands-on with reference system)
- **Level 3**: Advanced Operations (complex scenarios and tuning)

**Total Duration**: ~2 weeks full-time (or 4-6 weeks part-time)

**Delivery Format**: Self-paced with guided hands-on labs

---

## Level 1: Awareness Certification

**Duration**: 1-2 hours (reading + self-assessment)
**Prerequisite**: None
**Goal**: Understand TPS principles and system metrics

### Level 1 Curriculum

#### Module 1.1: TPS Principles (30 minutes)
- Read: `handbook-tps-operations.md` Section 1: TPS Principles Recap
- Topics:
  - What is TPS and why it matters
  - Jidoka (autonomation with human touch)
  - Kanban (pull-based work)
  - Andon (problem visibility)
  - Kaizen (continuous improvement)
  - Heijunka (load leveling)
  - Value Stream (see the waste)

#### Module 1.2: Understanding Metrics (30 minutes)
- Read: `handbook-tps-operations.md` Section 3: Interpreting Metrics
- Topics:
  - Queue depth and what it means
  - Processing latency (p50, p90, p99)
  - Throughput (requests per second)
  - Error rate (% failures)
  - Worker utilization (% busy)
  - Dead-letter queue (failed items)
  - Circuit breaker state (open/closed)

#### Module 1.3: Dashboard Reading (30 minutes)
- Read: `handbook-tps-operations.md` Section 2: Reading Dashboards
- Topics:
  - Circuit breaker panel
  - Queue depth panel
  - Worker utilization panel
  - Error rate panel
  - Latency distribution panel
  - Throughput panel
  - Resource usage panel

#### Module 1.4: Glossary (15 minutes)
- Read: `tps-glossary.md`
- Learn manufacturing terms in software context

### Level 1 Self-Assessment Quiz

**20 questions, 15 minutes, 80% passing score**

**Sample Questions**:

1. **What does "Jidoka" mean?**
   - A) Just-in-time production
   - B) Autonomation with human touch, stop when broken ← CORRECT
   - C) Continuous improvement
   - D) Load leveling

2. **If queue depth is 100% of max size, what should you do?**
   - A) Wait for it to drain on its own
   - B) Increase queue size
   - C) Scale workers or reduce incoming load ← CORRECT
   - D) Restart the system

3. **What does latency p99 mean?**
   - A) 99% of requests take this time or longer
   - B) 99% of requests are faster than this time ← CORRECT
   - C) The average request latency
   - D) The fastest 1% of requests

4. **When should a circuit breaker open?**
   - A) When the queue is full
   - B) When downstream service is failing or too slow ← CORRECT
   - C) When latency is high
   - D) Every hour to reset

5. **What's the purpose of the dead-letter queue?**
   - A) Store successful items
   - B) Store failed items that can't be retried ← CORRECT
   - C) Store very fast items
   - D) It has no purpose

### Level 1 Certificate

**Upon Passing**: Receive "TPS Operator Level 1: Awareness"

**What This Means**:
- You understand TPS principles
- You can read basic dashboards
- You know what metrics mean
- You're ready for Level 2

---

## Level 2: Basic Operations Certification

**Duration**: 1 day (6-8 hours hands-on)
**Prerequisite**: Level 1 certification completed
**Goal**: Run system under load and troubleshoot basic issues

### Level 2 Prerequisites
- Complete Level 1 self-assessment (score ≥ 80%)
- Read the TPS Operations Handbook Section 4 (common scenarios)
- Laptop with Docker and basic tools

### Level 2 Hands-On Labs

#### Lab 2.1: System Setup (30 minutes)

**Objective**: Get the reference system running locally

**Tasks**:
1. Clone the TPS reference system repository
2. Run `docker-compose up`
3. Verify all services healthy
4. Access Grafana dashboard on http://localhost:3000
5. Access Jaeger traces on http://localhost:16686
6. Document system URLs and credentials

**Success Criteria**:
- Docker services running (at least 5 containers)
- Grafana dashboard accessible, showing green health
- Jaeger accessible, showing no errors
- Can see example requests in Grafana

---

#### Lab 2.2: Generate Load (30 minutes)

**Objective**: Create realistic load on the system

**Tasks**:
1. Run load generator: `make load` (or docker command)
2. Monitor dashboard while load is running
3. Observe queue depth increasing
4. Observe workers scaling up
5. Observe throughput metric
6. Stop load generator after 2 minutes
7. Observe queue draining and workers scaling down

**What to Watch**:
- Queue depth should increase to ~50% of max
- Workers should scale up
- Throughput should match load
- Error rate should stay low
- Circuit breaker should stay closed (green)

**Success Criteria**:
- Load generator runs without errors
- Dashboard shows metrics changing
- System handles load gracefully
- No errors or circuit breaker opening

---

#### Lab 2.3: Reading Dashboards Under Load (30 minutes)

**Objective**: Interpret dashboard metrics

**Tasks**:
1. Start load generator (keep it running)
2. Spend 5 minutes observing each dashboard panel:
   - Circuit breaker status
   - Queue depth
   - Worker utilization
   - Error rate
   - Latency (p50, p90, p99)
   - Throughput
   - Resource usage (CPU, Memory)
3. For each panel, answer:
   - What does the metric show?
   - Is the value normal?
   - What would indicate a problem?
4. Document your observations

**Success Criteria**:
- Can explain what each panel shows
- Can identify normal vs abnormal values
- Understand relationships between metrics

---

#### Lab 2.4: Generate Failure and Recover (45 minutes)

**Objective**: Cause a failure and practice recovery

**Tasks**:
1. Start load generator
2. Generate failure: `make chaos` (simulates downstream service failure)
3. Observe dashboard:
   - Which metrics change?
   - Does circuit breaker open?
   - Does queue grow?
   - Do error rates spike?
4. Document what you observe
5. Stop chaos: `make chaos-stop`
6. Observe recovery:
   - How long does it take?
   - Do metrics return to normal?
   - Does circuit breaker close?

**What to Expect**:
- Circuit breaker opens (red)
- Error rate spikes
- Queue might grow slightly
- Latency stays reasonable (because circuit is rejecting, not queueing)
- After chaos stops, circuit should recover in ~60 seconds

**Success Criteria**:
- Identify which metrics change during failure
- Understand circuit breaker's role
- Observe recovery process
- Recognize that circuit breaker protects the system

---

#### Lab 2.5: Troubleshoot Queue Buildup (45 minutes)

**Objective**: Diagnose why queue is growing

**Tasks**:
1. Start load generator at high intensity: `make load RATE=500` (high load)
2. Monitor queue depth: Is it growing?
3. Check worker utilization: Are workers at 100%?
4. Check if system is rejecting work or queueing it
5. Make decision: Scale workers or reduce load?
6. Scale workers: Edit docker-compose.yml, increase worker replicas
7. Restart system: `docker-compose up`
8. Monitor if queue depth stabilizes
9. Gradually reduce load and observe scaling down

**Success Criteria**:
- Diagnose reason for queue buildup
- Take appropriate action (scale)
- Verify fix works

---

#### Lab 2.6: Read Traces to Find Bottleneck (45 minutes)

**Objective**: Use distributed tracing to identify slow requests

**Tasks**:
1. Start load generator
2. Open Jaeger UI (http://localhost:16686)
3. Select a trace with high latency
4. Examine each span (step in the request flow):
   - Service name
   - Operation (what did it do?)
   - Duration (how long?)
5. Identify which span is slowest (bottleneck)
6. Find 5-10 slow traces, note which span is always slow
7. Document findings

**What to Look For**:
- One service consistently slower than others
- One operation taking most of the time
- Database queries taking 80% of time
- Waiting for external service taking 90% of time

**Success Criteria**:
- Read traces accurately
- Identify bottleneck
- Could explain how to fix it (e.g., add caching, optimize query)

---

### Level 2 Practical Exam

**Duration**: 2 hours
**Format**: Hands-on scenario

**Exam Scenario**:
```
It's Monday morning. You're starting your shift.

Situation:
- System received high load over the weekend
- Some metrics are not back to normal
- You need to investigate what happened and verify system is healthy

Tasks:
1. Check dashboard: What metrics are abnormal?
2. Investigate: Use logs and traces to understand what happened
3. Root cause analysis: What caused the issue?
4. Verify recovery: Is system now healthy?
5. Recommendations: What should we do to prevent this next time?

You have 2 hours. Document your findings in a report.
```

**Grading Criteria** (80% passing):
- Correctly identify abnormal metrics (20%)
- Use logs and traces effectively (20%)
- Accurately diagnose root cause (20%)
- Verify system health (20%)
- Make reasonable recommendations (20%)

---

### Level 2 Certificate

**Upon Passing**: Receive "TPS Operator Level 2: Basic Operations"

**What This Means**:
- You can run TPS systems on day-to-day basis
- You can handle basic issues (scaling, circuit breaker)
- You can read dashboards and traces
- You understand system behavior
- You're ready for Level 3 or production operations

---

## Level 3: Advanced Operations Certification

**Duration**: 1 week (40+ hours hands-on labs)
**Prerequisite**: Level 2 certification completed
**Goal**: Handle complex scenarios, tune parameters, optimize performance

### Level 3 Hands-On Labs

#### Lab 3.1: Cascade Failure Investigation (8 hours)

**Objective**: Understand how failures cascade and how to prevent them

**Scenario**:
- You have Service A → Service B → Service C
- Service C becomes slow
- This causes Service B to overload
- This causes Service A to overload
- Entire system fails

**Tasks**:
1. Run system with multiple service tiers
2. Make Service C slow: `make chaos TARGET=service-c LATENCY=5000`
3. Observe how failure cascades:
   - Service C latency increases
   - Service B queue fills (waiting for Service C)
   - Service B latency increases
   - Service A queue fills (waiting for Service B)
   - Service A error rate increases
4. Identify cascade at each level
5. Analyze circuit breaker behavior:
   - Does it help prevent cascade?
   - At which point does it open?
6. Implement fixes:
   - Reduce timeout for slow service
   - Add circuit breaker between each tier
   - Implement fallback/cache
7. Verify cascade is prevented

**Success Criteria**:
- Understand cascade failure mechanism
- Identify where circuit breaker should open
- Implement multi-tier circuit breaker strategy

---

#### Lab 3.2: Parameter Tuning (6 hours)

**Objective**: Tune system parameters for optimal performance

**Parameters to Tune**:
1. **Circuit breaker threshold**: When should it open?
   - Too low: Opens on transient failures
   - Too high: Doesn't protect system
   - Goal: Find sweet spot

2. **Queue max size**: How much work can queue hold?
   - Too small: Reject work too early
   - Too large: Memory waste, long queues
   - Goal: Queue at 50% full under normal load

3. **Worker pool size**: How many workers needed?
   - Too few: Can't handle load
   - Too many: Waste resources
   - Goal: 60-80% utilization under normal load

4. **Auto-scaling thresholds**: When to scale?
   - Scale up at: 80% utilization
   - Scale down at: 20% utilization
   - Goal: Responsive but not jittery

5. **Timeout values**: How long to wait?
   - Too short: Fail transient issues
   - Too long: Wait for unavailable service
   - Goal: Fail fast without being too aggressive

**Tasks**:
1. Baseline: Document current parameter values and performance
2. For each parameter:
   - Change value (up and down)
   - Measure impact on: latency, throughput, error rate, resources
   - Find optimal value
   - Document why that value is optimal
3. Implement optimized parameters
4. Run full load test to verify improvement
5. Document all changes

**Success Criteria**:
- Systematically tune each parameter
- Measure impact of each change
- Improve performance by 10-20%
- Document reasoning

---

#### Lab 3.3: Custom Dashboard Creation (4 hours)

**Objective**: Create dashboard for specific use case

**Task**:
1. Design dashboard for: "Is my system handling load well?"
2. Select metrics:
   - What 5-10 metrics best show system health?
   - How should they be displayed (chart, gauge, heatmap)?
3. Create dashboard in Grafana:
   - Add panels for selected metrics
   - Configure alerts for each metric
   - Create links to relevant logs/traces
4. Test dashboard:
   - Does it clearly show health?
   - Can you quickly spot problems?
   - Are alerts useful?
5. Document dashboard purpose and interpretation

**Success Criteria**:
- Dashboard is clear and actionable
- Metrics selected are relevant
- Alerts are properly configured
- Can explain every metric

---

#### Lab 3.4: Performance Optimization (8 hours)

**Objective**: Improve system performance

**Scenario**:
- System is hitting latency SLO (p99 must be < 100ms)
- Currently p99 is 150ms
- Need to optimize to meet SLO

**Tasks**:
1. Baseline: Measure current performance
2. Identify bottleneck: Which component is slowest?
3. Optimize (multiple approaches):
   - Add caching (in-memory or distributed)
   - Optimize database queries (add indexes, cache queries)
   - Reduce payload size (compress, filter unnecessary data)
   - Parallelize operations (do multiple things at once)
   - Scale component (add replicas)
4. Measure impact of each optimization
5. Combine optimizations to meet SLO
6. Document before/after performance

**Success Criteria**:
- Meet latency SLO (p99 < 100ms)
- Explain which optimizations helped most
- Implement without increasing error rate

---

#### Lab 3.5: Capacity Planning (6 hours)

**Objective**: Plan capacity for growth

**Scenario**:
- Current load: 100 req/sec
- Historical growth: 20% per month
- Predict load in 3 months, 6 months, 1 year
- Plan scaling strategy

**Tasks**:
1. Gather historical data:
   - Last 3 months of metrics (CPU, memory, queue depth, throughput)
   - Load trends
2. Create forecast:
   - Linear projection (20% per month)
   - Peak load during peak hours (might be 2-3x average)
3. For each forecast point, calculate needed resources:
   - Workers needed to handle load at 80% utilization
   - Queue size needed
   - Memory allocation
   - CPU cores
4. Cost analysis:
   - Cost per unit of capacity
   - Total cost over time
5. Scaling strategy:
   - When to scale? (at what load threshold)
   - How much to scale? (50% or 100%)
   - Cost optimization: Spot instances, reserved capacity
6. Document plan and assumptions

**Success Criteria**:
- Forecast is reasonable (based on data)
- Capacity plan is conservative (don't run out)
- Cost is accounted for
- Strategy is documented

---

#### Lab 3.6: Deployment Strategy (6 hours)

**Objective**: Deploy changes safely without service disruption

**Scenario**:
- New code version is ready
- Need to deploy to production
- Can't have downtime
- Need to verify no degradation

**Tasks**:
1. Set up canary deployment:
   - 10% of traffic to new version
   - 90% to old version
2. Monitor metrics during canary:
   - Error rate comparison
   - Latency comparison
   - Resource usage
   - Circuit breaker behavior
3. Gradual rollout:
   - If canary good: Increase to 50%
   - If still good: 100%
   - If problem: Rollback to 0%
4. Rollback procedure:
   - If error rate spikes
   - If latency increases
   - If any SLO broken
5. Post-deployment verification:
   - Run 30 minutes of monitoring
   - Verify metrics return to normal
   - Verify no issues in logs
   - Verify SLOs are still met

**Success Criteria**:
- Deploy new version safely
- No service disruption
- Metrics show no degradation
- Rollback is available if needed

---

#### Lab 3.7: Failure Recovery (6 hours)

**Objective**: Recover from different types of failures

**Failure Types**:

1. **Jidoka Failure**: Worker unhealthy
   - Symptom: One worker's queue is growing
   - Recovery: Remove unhealthy worker, add new one
   - Verification: Queue returns to normal

2. **Kanban Failure**: Queue backend (NATS) down
   - Symptom: No work is being pulled, system stopped
   - Recovery: Restart queue, replay in-flight items
   - Verification: Items are being processed again

3. **Andon Failure**: Can't see metrics (monitoring down)
   - Symptom: Dashboard is blank, no metrics
   - Recovery: Restart monitoring stack (Prometheus, Grafana)
   - Verification: Dashboard shows data again

4. **Heijunka Failure**: Can't scale (autoscaler broken)
   - Symptom: Load increases but workers don't scale
   - Recovery: Manually scale, disable autoscaler, restart it
   - Verification: Scaling works again

**Tasks**:
1. For each failure type:
   - Cause the failure
   - Observe impact on system
   - Implement recovery procedure
   - Verify system is healthy
   - Document recovery steps

**Success Criteria**:
- Can recover from each failure type
- Know how to detect each failure
- Know recovery steps
- Understand prevention (can avoid failure)

---

### Level 3 Practical Exam

**Duration**: 8 hours (can be split over 2 days)
**Format**: Complex scenario with multiple components failing

**Exam Scenario**:
```
It's Friday before a holiday weekend.

Situation at 2 PM Friday:
- System is getting holiday weekend load (2x normal)
- You're the sole on-call engineer
- You need to keep system stable through Monday morning
- You can't add infrastructure (all resources allocated)
- You need to optimize what you have

Multiple failures occur during the day:
1. 3 PM: Downstream service becomes slow (latency increases)
2. 4 PM: One worker crashes (node failure)
3. 5 PM: Database query becomes slow (new data pattern)
4. 6 PM: Memory on workers is increasing (possible leak)
5. 7 PM: Network bandwidth is approaching limit (saturation)

For each failure:
- Detect it (before users complain)
- Diagnose root cause
- Implement fix
- Verify fix works
- Prevent recurrence

Your goal: System handles holiday load without SLO violations.

You have 8 hours. Document:
- What happened
- How you detected it
- How you fixed it
- How to prevent it
```

**Exam Grading** (80% passing):
- Detection (can you see problems before they're critical?): 15%
- Root cause analysis (do you understand what went wrong?): 20%
- Fixing (can you implement fixes that work?): 25%
- Verification (did you verify the fix worked?): 20%
- Prevention (can you prevent recurrence?): 20%

---

### Level 3 Certificate

**Upon Passing**: Receive "TPS Operator Level 3: Advanced Operations"

**What This Means**:
- You can handle complex, multi-component failures
- You understand system internals and can tune parameters
- You can plan capacity and predict performance
- You can deploy safely and recover from disasters
- You're ready to be primary on-call engineer

**Roles You Can Do**:
- Production on-call engineer
- System optimization specialist
- Capacity planning lead
- Incident commander

---

## Recertification Requirements

**Certification Valid For**: 2 years

**Recertification Process**:
1. Complete refresher training (4 hours) covering:
   - Recent changes to system
   - New failure modes discovered
   - Best practices learned
2. Take quick assessment (1 hour)
3. Pass practical exam (4 hours) similar to original level

**Reasons to Recertify Early**:
- System architecture changes significantly
- Major incident happened and lessons learned
- Operator hasn't touched system in 6+ months
- Operator wants to stay sharp

---

## Continuing Education

**Optional Advanced Topics** (after Level 3):

- **Advanced Performance Tuning**: Machine learning for parameter optimization
- **Multi-Region Deployment**: Managing TPS across regions
- **Chaos Engineering**: Systematically breaking things to improve reliability
- **Custom Metrics**: Adding domain-specific metrics to dashboard
- **Compliance and Audit**: Logging, tracing, compliance requirements

---

## Program Metrics

**Certification Statistics**:
- Average time to Level 1: 2 hours
- Average time to Level 2: 8 hours (plus study time)
- Average time to Level 3: 40 hours
- Average passing rate: Level 1: 95%, Level 2: 85%, Level 3: 75%
- Most common failure reason: Time pressure on practical exams

---

**End of TPS Operator Certification Program**
