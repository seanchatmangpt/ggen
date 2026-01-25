# TPS Operations Handbook: Complete Reference

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: System operators, on-call engineers, platform teams
**Certified By**: TPS Operations Team

---

## Table of Contents

1. [Section 1: TPS Principles Recap](#section-1-tps-principles-recap) (Pages 1-30)
2. [Section 2: Reading Dashboards](#section-2-reading-dashboards) (Pages 31-60)
3. [Section 3: Interpreting Metrics](#section-3-interpreting-metrics) (Pages 61-80)
4. [Section 4: Common Scenarios](#section-4-common-scenarios) (Pages 81-120)
5. [Section 5: Decision Trees](#section-5-decision-trees) (Pages 121-130)
6. [Section 6: Troubleshooting Guide](#section-6-troubleshooting-guide) (Pages 131-160)
7. [Section 7: Maintenance Operations](#section-7-maintenance-operations) (Pages 161-180)

---

## Section 1: TPS Principles Recap

### 1.1 What is TPS and Why It Matters

**Toyota Production System (TPS)** is a manufacturing methodology designed to:
- Eliminate waste (overproduction, waiting, motion, defects)
- Make problems visible immediately (don't hide failures)
- Enable continuous improvement (kaizen)
- Respect people and processes

In software/systems context, TPS principles become:
- **Waste elimination**: Don't queue work unnecessarily, don't over-provision resources
- **Problem visibility**: Make failures visible, make metrics visible, make bottlenecks visible
- **Continuous improvement**: Measure everything, analyze failures, improve systematically
- **Respect for people**: Give operators clear information and decision-making tools

### 1.2 The Six Core Principles

#### Principle 1: Jidoka (Autonomation with Human Touch)

**Definition**: The system should detect problems automatically and stop if something is wrong. Don't amplify failures.

**In TPS Systems**:
- **Circuit breaker**: When the downstream service is failing, the circuit opens immediately (stops sending requests)
- **Dead-letter queue**: Failed items are automatically separated so they don't block the main flow
- **Health checks**: System constantly monitors its own health and reports problems
- **Fail fast**: Reject work early if you can't handle it (don't pretend to queue it)

**What You See as Operator**:
- Circuit breaker open (service is failing)
- Dead-letter queue growing (something is broken, needs investigation)
- Health check failing (component is unhealthy)
- Requests rejected (system is overloaded, not queueing)

**Why This Matters**: If you queue work when the system can't handle it, that work just accumulates. Instead, fail immediately and alert the user. Better to tell them "I'm overloaded, try again later" than to queue their request and process it 2 hours later.

**Your Job**:
1. When circuit is open: Is the downstream service actually broken, or are you just rejecting work?
2. When DLQ is growing: Something is broken. Find out what. Don't ignore it.
3. When requests are rejected: This means your system is at capacity. Scale or reduce load.

---

#### Principle 2: Kanban (Pull-Based Work)

**Definition**: Workers pull work based on capacity, not push from upstream. The queue has a fixed limit (work-in-progress limit).

**In TPS Systems**:
- **Queue has a maximum size**: If the queue is full, new work is rejected (applying Jidoka)
- **Workers pull from queue**: Each worker grabs work when ready, not when you push it to them
- **Backpressure**: When queue is full, upstream systems feel the backpressure (have to wait or retry)
- **Visibility**: You can see how many items are queued, waiting to be processed

**What You See as Operator**:
- Queue depth: How many items are waiting? Is it growing or stable?
- Queue full: Queue hit its limit, new work is being rejected
- Processing rate: How many items per second are workers processing?
- Worker utilization: Are all workers busy, or are some idle?

**Why This Matters**: Push-based systems can overload and collapse. Pull-based systems naturally limit work-in-progress and maintain stability.

**Your Job**:
1. Monitor queue depth: Is it stable, growing, or shrinking?
2. If queue is full: Workers can't keep up. Need more workers or reduce incoming load.
3. If queue is empty: Workers are idle. You have spare capacity.
4. Balance: Queue should be 40-60% full under normal load. If consistently empty, reduce workers. If always full, add workers.

---

#### Principle 3: Andon (Problem Visibility)

**Definition**: Make problems visible immediately. Don't hide alerts. Signal problems visibly so everyone knows.

**In TPS Systems**:
- **Metrics**: Every action is measured (latency, throughput, errors, queue depth)
- **Dashboards**: Real-time visualization of system health
- **Alerts**: When something goes wrong, you get notified immediately
- **Traces**: Detailed record of each transaction (where did it go, how long did it take, where did it fail)
- **Logs**: Structured records of events (errors, warnings, interesting events)

**What You See as Operator**:
- Grafana dashboard: Real-time view of system health (green = good, red = problems)
- Alert notifications: Slack, email, pagerduty alerts when thresholds are breached
- Traces in Jaeger: Click on a request, see exactly where it went and how long each step took
- Logs: Query logs to understand why something failed

**Why This Matters**: If problems are hidden, you can't fix them. If you don't know the system is broken until customers complain, you've already lost. Make problems visible immediately.

**Your Job**:
1. Check dashboards regularly (at least once per shift)
2. Investigate every alert (don't ignore them, there's usually a reason)
3. Look at traces when latency is high (find the bottleneck)
4. Check logs when errors are high (understand what's failing)

---

#### Principle 4: Kaizen (Continuous Improvement)

**Definition**: Measure everything. Analyze failures. Improve systematically. Small improvements add up.

**In TPS Systems**:
- **SLOs**: Agree on what "good" means (e.g., 99% of requests < 100ms latency)
- **Measurement**: Capture actual performance against SLOs
- **Analysis**: When you miss SLOs, understand why
- **Action**: Take action to improve (tune parameters, add capacity, fix bugs)
- **Feedback loop**: Measure again to verify improvement

**What You See as Operator**:
- SLO dashboard: Are we meeting our SLOs? How close are we?
- Performance trends: Is performance improving or degrading over time?
- Incident reports: What went wrong, and what did we learn?
- Tuning data: Based on historical data, what should we tune?

**Why This Matters**: Systems don't stay optimized. As load changes, as code changes, as infrastructure changes, performance drifts. Continuous measurement and improvement keeps the system healthy.

**Your Job**:
1. Review performance metrics weekly
2. When you miss an SLO, understand why (not just "we missed it", but "why did we miss it")
3. When you fix a problem, verify that performance actually improved
4. Share learnings with the team (post-incident reviews, performance trends)

---

#### Principle 5: Heijunka (Load Leveling)

**Definition**: Distribute load evenly over time. Don't have spikes and valleys. Don't overload single components.

**In TPS Systems**:
- **Multiple worker pools**: Some workers for fast tasks, some for slow tasks. Don't make fast tasks wait for slow tasks.
- **Rate limiting**: Spread incoming requests over time, don't allow bursts
- **Auto-scaling**: Add workers when load increases, remove when load decreases
- **Circuit breaker**: When downstream is slow, don't send more requests to it
- **Dead-letter queue**: Separate slow/failing tasks so they don't block fast tasks

**What You See as Operator**:
- Worker pool sizes: How many fast workers? How many slow workers?
- Rate of requests: Requests per second (should be relatively stable)
- Worker utilization: Are some workers always busy while others are idle?
- Auto-scaling events: Did the system add/remove workers? When? Why?

**Why This Matters**: Spikes cause overload, timeouts, errors. Even distribution means stable performance and predictable resource usage.

**Your Job**:
1. Configure multiple worker pools (don't make all workers the same)
2. Set appropriate rate limits (prevent bursts)
3. Monitor auto-scaling: Is it adding/removing workers appropriately?
4. Identify hotspots: Are some workers always busy while others are idle?
5. Tune thresholds: When should auto-scaling trigger?

---

#### Principle 6: Value Stream (See the Waste)

**Definition**: Track the entire flow from input to output. Identify where work gets stuck. Eliminate waste.

**In TPS Systems**:
- **Request flow**: Trace from user request → system → response (see how long each step takes)
- **Bottlenecks**: Which step is slowest? Is it network, processing, I/O?
- **Queue depth**: Where does work accumulate? Why?
- **Error rates**: Where are errors happening? Early (reject fast) or late (wasted work)?
- **Latency distribution**: Some requests are fast, some are slow. Why?

**What You See as Operator**:
- Traces: Visual representation of request flow and timing
- Latency percentiles: 50th, 90th, 99th percentile latencies (see distribution)
- Error distribution: Where do errors happen in the flow?
- Resource usage: CPU, memory, I/O - which is the bottleneck?

**Why This Matters**: To improve performance, you must understand where the work is actually going. Optimize the bottleneck, not the fast path.

**Your Job**:
1. Regularly review traces to understand request flow
2. When latency is high, trace a slow request to find the bottleneck
3. When errors are high, understand at which step errors occur
4. Identify waste: What work is being done that doesn't add value?
5. Optimize systematically: Fix the bottleneck, then measure again

---

### 1.3 How These Principles Work Together

**Jidoka** (fail fast) + **Kanban** (pull-based) = System naturally limits work, stops when broken

**Andon** (visibility) + **Kaizen** (continuous improvement) = You can see problems and fix them systematically

**Heijunka** (load leveling) = Prevents overload, keeps system stable

**Value Stream** (see the waste) = Understand where to improve

### 1.4 Anti-Patterns (What NOT to Do)

**Anti-Pattern 1: Ignore circuit breaker**
- Problem: You keep retrying failed requests, amplifying failure
- Result: Cascading failure, entire system goes down
- Solution: When circuit is open, stop sending requests until service recovers

**Anti-Pattern 2: Unbounded queue**
- Problem: Queue grows infinitely, work gets stuck for hours
- Result: Users wait forever, system uses memory until it crashes
- Solution: Set maximum queue size, reject new work if queue is full

**Anti-Pattern 3: Hide alerts**
- Problem: You mute alerts or ignore them
- Result: Problems go unnoticed, compound over time
- Solution: Investigate every alert, understand why it happened

**Anti-Pattern 4: Optimize without measurement**
- Problem: You "optimize" something without knowing if it actually matters
- Result: Waste time on things that don't improve performance
- Solution: Measure first, identify bottleneck, optimize that

**Anti-Pattern 5: All workers the same**
- Problem: Fast tasks wait for slow tasks to finish (fast worker becomes slow)
- Result: Overall throughput is limited by slowest task type
- Solution: Create separate worker pools for different task types

**Anti-Pattern 6: No visibility**
- Problem: You don't have dashboards, metrics, or alerts
- Result: You don't know there's a problem until customers complain
- Solution: Instrument everything, create dashboards, set up alerts

---

## Section 2: Reading Dashboards

### 2.1 Dashboard Overview

A TPS system dashboard shows:
1. **System health**: Is the system healthy or broken?
2. **Queue status**: How much work is waiting?
3. **Worker status**: Are workers processing work or idle?
4. **Error rate**: What percentage of work is failing?
5. **Latency**: How long does work take to process?
6. **Throughput**: How much work is being processed?
7. **Resource usage**: CPU, memory, disk - what's being used?

### 2.2 Key Dashboard Panels

#### Panel 1: Circuit Breaker Status

**Visual**: Traffic light (green = closed, red = open)

**What It Means**:
- **Green (closed)**: Downstream service is healthy, requests are being sent
- **Red (open)**: Downstream service is failing or too slow, requests are being rejected
- **Yellow (half-open)**: Circuit is testing if downstream service recovered

**How to Read It**:
- Green is normal
- Red is a problem (but might be temporary)
- If red for >5 minutes, investigate

**Questions to Ask**:
- Is downstream service actually broken, or just slow?
- Did the circuit open at the same time something else broke?
- How long has the circuit been open?

---

#### Panel 2: Queue Depth

**Visual**: Line chart showing queue size over time

**What It Means**:
- **Y-axis**: Number of items in queue (0-max queue size)
- **X-axis**: Time (current view usually 1 hour)
- **Line going up**: Items are arriving faster than being processed (queue growing)
- **Line going down**: Items are being processed faster than arriving (queue shrinking)
- **Line flat at top**: Queue is full, new items being rejected

**How to Read It**:
- Normal: Queue is 40-60% of max size, fluctuating slightly
- Problem 1: Queue is 100% (queue full, requests rejected)
- Problem 2: Queue is consistently > 80% (system near capacity)
- Good: Queue is < 30% (system has spare capacity)

**Questions to Ask**:
- Is the queue stable or growing?
- Did the queue fill up at a specific time? What else happened then?
- How long does an item take to process given current queue depth?

---

#### Panel 3: Worker Utilization

**Visual**: Bar chart showing busy/idle workers

**What It Means**:
- **Busy workers**: Currently processing an item
- **Idle workers**: Waiting for work

**How to Read It**:
- Normal: 60-80% of workers are busy
- Problem 1: 100% of workers busy (system at capacity, need to scale)
- Problem 2: < 20% of workers busy (over-provisioned, could reduce workers)

**Questions to Ask**:
- Are workers in one pool busy while another pool is idle?
- Did busy percentage increase at same time as queue filled?
- Are workers processing items in reasonable time, or taking too long?

---

#### Panel 4: Error Rate

**Visual**: Percentage or count of errors over time

**What It Means**:
- **Low error rate** (< 0.1%): Normal, expected transient errors
- **Moderate error rate** (0.1-1%): Something is wrong, investigate
- **High error rate** (> 1%): Serious problem, immediate action needed

**How to Read It**:
- Error rate should be steady and low
- If error rate spikes, something just broke
- If error rate is high across the board, systematic problem (maybe all items are failing)
- If error rate is high for one worker, that worker might be broken

**Questions to Ask**:
- When did error rate increase? What else happened then?
- Are errors happening in a specific component or across the board?
- Are errors transient or persistent?

---

#### Panel 5: Latency Distribution

**Visual**: Line chart showing p50, p90, p99 latency over time

**What It Means**:
- **p50 (median)**: 50% of requests are faster than this, 50% are slower
- **p90**: 90% of requests are faster than this (slow requests)
- **p99**: 99% of requests are faster than this (very slow requests)

**How to Read It**:
- All lines should move together (if p50 goes up, p90 usually goes up too)
- p99 should not be more than 10x p50 (if p50 is 10ms, p99 should be < 100ms)
- If p99 is way higher than p90, you have some very slow outliers

**Questions to Ask**:
- Is latency stable or increasing?
- Did latency spike at a specific time?
- Is p99 reasonable given p50 (or are there weird outliers)?

---

#### Panel 6: Throughput

**Visual**: Requests per second over time

**What It Means**:
- How much work the system is processing per second
- Should be relatively stable (not spiking up and down)
- Should match incoming request rate (if it doesn't, you're losing work)

**How to Read It**:
- Normal: Stable throughput matching incoming load
- Problem 1: Throughput < incoming load (you're rejecting work)
- Problem 2: Throughput is declining (system is slowing down)
- Problem 3: Spiky throughput (work is being processed in bursts, not evenly)

**Questions to Ask**:
- Is throughput keeping up with incoming load?
- When did throughput change?
- Is throughput consistent across all worker types?

---

#### Panel 7: Resource Usage

**Visual**: CPU %, Memory %, Disk % over time

**What It Means**:
- **CPU**: Is the system CPU-bound? (if CPU is 100%, system is limited by CPU)
- **Memory**: Is the system using too much memory? (if memory is 100%, system might crash)
- **Disk**: Is the system running out of disk space? (if disk is 100%, system might stop)

**How to Read It**:
- Normal: CPU 40-70%, Memory 50-80%, Disk < 80%
- Problem 1: CPU 100% (need to optimize code or add CPU)
- Problem 2: Memory 100% (need to optimize memory usage or reduce load)
- Problem 3: Disk 100% (need to clean up old data or add storage)

**Questions to Ask**:
- Which resource is the bottleneck?
- Did resource usage spike at a specific time?
- Is resource usage growing over time (leak)?

---

### 2.3 Dashboard Examples

#### Example 1: Healthy System

```
Circuit Breaker:     ✓ Green (closed)
Queue Depth:         ▁▂▃▃▂▂▃▁  (40% of max, stable)
Worker Utilization:  ████████░ (80% busy, 20% idle)
Error Rate:          0.05% (normal)
Latency p50/p90/p99: 10ms / 25ms / 45ms (p99 is 4.5x p50, reasonable)
Throughput:          ▂▃▃▃▂▃▂▃  (100 req/sec, stable)
Resource Usage:      CPU 45%, Mem 60%, Disk 35% (all normal)
```

**Interpretation**: System is healthy. Queue is stable. Latency is reasonable. No problems.

---

#### Example 2: Approaching Capacity

```
Circuit Breaker:     ✓ Green (closed)
Queue Depth:         ▃▅▆█████▇ (80% of max, trending up)
Worker Utilization:  ████████░ (90% busy, need scale soon)
Error Rate:          0.1% (still acceptable)
Latency p50/p90/p99: 15ms / 45ms / 120ms (p99 is 8x p50, starting to spike)
Throughput:          ▃▅▆▇▆▆▇▆  (120 req/sec, up from 100)
Resource Usage:      CPU 70%, Mem 75%, Disk 40% (trending up)
```

**Interpretation**: System is approaching capacity. Queue is filling. Latency is starting to spike. Time to scale.

---

#### Example 3: System Overloaded

```
Circuit Breaker:     ✗ Red (open, trying to recover)
Queue Depth:         █████████ (100% of max, requests being rejected)
Worker Utilization:  ████████░ (85% busy, stuck at this capacity)
Error Rate:          2.5% (high, something is broken)
Latency p50/p90/p99: 45ms / 200ms / 500ms (p99 is 11x p50, very bad)
Throughput:          ▇▄▁▆▃▄▂▅  (80-120 req/sec, spiky and declining)
Resource Usage:      CPU 95%, Mem 90%, Disk 45% (CPU-bound)
```

**Interpretation**: System is overloaded. Circuit is open (downstream is failing). Queue is full. Error rate is high. Immediate action needed.

---

#### Example 4: Partial Failure

```
Circuit Breaker:     ✗ Red (open on slow-queue, green on fast-queue)
Queue Depth:         Fast: ▂▃▂▃ (30%), Slow: ████████ (95%)
Worker Utilization:  Fast: ████░ (70%), Slow: ████████ (95%)
Error Rate:          Total: 0.5%, Slow queue: 3%
Latency p50/p90/p99: Overall: 20ms / 60ms / 200ms
Throughput:          ▃▄▄▃▃▄▃▄  (but slow-queue declining)
Resource Usage:      CPU 55% (I/O-bound, disk is slow)
```

**Interpretation**: Slow queue is failing or overloaded. Circuit opened for slow-queue to protect system. Fast queue is healthy. Need to investigate slow-queue issue.

---

### 2.4 Dashboard Monitoring Schedule

**Frequency of Checking**:
- Every 15 minutes during business hours (automated alert should notify you of problems)
- Every 4 hours during off-hours (unless on-call)
- Weekly trend analysis (is performance improving or degrading?)
- Monthly capacity planning review (will we need to scale soon?)

**What to Check**:
- Circuit breaker status: All green?
- Queue depth: Stable? Filling up?
- Error rate: Low? Spiked?
- Latency: Stable? Increasing?
- Resource usage: Approaching limits?

---

## Section 3: Interpreting Metrics

### 3.1 Core Metrics Glossary

#### Metric 1: Queue Depth (items in queue)

**Definition**: Number of items currently waiting in queue

**Why It Matters**:
- Tells you how much work is backed up
- Too deep: Users are waiting, system is slow
- Too shallow: Workers are idle, you're over-provisioned

**Target Range**: 40-60% of max queue size under normal load

**How to Interpret**:
- Growing trend: System is overloading, need to scale
- Shrinking trend: System is catching up, good
- Spiky: Load is bursty, need rate limiting
- Flat at 100%: System is rejecting work, critical problem

**Action if High**:
1. Check if upstream traffic increased
2. Check if workers slowed down
3. Scale workers if throughput is stable but queue is high
4. Reduce incoming load if workers can't keep up

---

#### Metric 2: Processing Latency (time to process one item)

**Definition**: Time from when a worker starts processing until it finishes

**Why It Matters**:
- Tells you if workers are fast or slow
- High latency = long wait time for users
- Increasing latency = system is degrading

**Target Range**: Depends on task type
- Fast tasks: < 50ms
- Normal tasks: 100-500ms
- Slow tasks: 500ms-10s (these should be in separate pool)

**How to Interpret**:
- p50 is median (half faster, half slower)
- p99 is 99th percentile (the slow outliers)
- If p99/p50 ratio > 10, you have slow outliers (investigate)
- If latency is increasing over time, something is degrading

**Action if High**:
1. Check traces to find bottleneck
2. Check if CPU/memory/disk is high (resource bottleneck)
3. Check if external service is slow (downstream bottleneck)
4. Optimize slow path or separate slow tasks to different pool

---

#### Metric 3: Throughput (items processed per second)

**Definition**: Number of items successfully processed per second

**Why It Matters**:
- Tells you system capacity
- Should match incoming load (or you're rejecting work)
- Should be consistent (not spiking up and down)

**Target Range**: Whatever your business requires (100 req/sec, 1000 req/sec, etc.)

**How to Interpret**:
- Throughput == incoming load: System is handling everything
- Throughput < incoming load: System is rejecting work (circuit open or queue full)
- Spiky throughput: Work is processing in bursts (load leveling problem)
- Declining throughput: System is slowing down (degradation)

**Action if Low**:
1. Check why system can't keep up (CPU, memory, disk maxed out?)
2. Check if workers are actually processing or stuck
3. Scale workers if they're at capacity
4. Check if external service is slow

---

#### Metric 4: Error Rate (% of items that fail)

**Definition**: Percentage of processed items that result in error

**Why It Matters**:
- Tells you system reliability
- High error rate = users are getting failures
- Spiking error rate = something just broke

**Target Range**: < 0.1% (1 error per 1000 items is acceptable)

**How to Interpret**:
- Steady low error rate: Normal, expected transient errors
- Spiked error rate: Something broke at that moment
- Persistently high error rate: Systematic problem
- Error rate in specific component: That component is broken

**Action if High**:
1. Check logs to understand what's failing
2. Look at traces of failed items (where in the flow do they fail?)
3. Check if a new deployment or change happened
4. Check if external service is failing
5. If circuit is open, downstream service might be failing

---

#### Metric 5: Worker Utilization (% of workers busy)

**Definition**: Percentage of workers currently processing an item

**Why It Matters**:
- Tells you if system is fully utilized or over-provisioned
- 100% utilization: System is at max capacity, need to scale
- 0% utilization: Workers are idle, could reduce workers

**Target Range**: 60-80% utilization under normal load

**How to Interpret**:
- Consistently 100%: System is at capacity, need to scale
- Consistently < 30%: Over-provisioned, could reduce workers
- Growing trend: Load is increasing, plan to scale
- Dropping trend: Load is decreasing, could reduce workers

**Action if High**:
1. Check if queue is also high (yes = scale workers; no = latency problem)
2. Check if incoming load increased
3. Add more workers if workload is constant

---

#### Metric 6: Dead-Letter Queue Size (failed items)

**Definition**: Number of items that failed processing and can't be retried

**Why It Matters**:
- Tells you what's broken
- Growing DLQ = systematic problem
- Items in DLQ need manual attention

**Target Range**: As close to 0 as possible

**How to Interpret**:
- DLQ empty: No failures, good
- DLQ growing: Something is broken, need investigation
- DLQ stable at low number: Some expected failures (transient)
- DLQ stable at high number: Systematic problem that's being ignored

**Action if Growing**:
1. Check logs of failing items
2. Find out why they're failing
3. Fix the root cause
4. Manually reprocess old DLQ items if appropriate

---

#### Metric 7: Circuit Breaker State

**Definition**: Is the circuit breaker open, closed, or half-open?

**Why It Matters**:
- Open circuit = system is rejecting work to protect itself
- Closed circuit = system is sending requests normally
- Half-open = system is testing if the problem is fixed

**States**:
- **Closed**: Normal operation, requests flowing
- **Open**: Requests are being rejected, system is protecting itself
- **Half-open**: Circuit is testing if issue is resolved, allowing some requests through

**How to Interpret**:
- Closed is normal
- Open usually means downstream service is broken/slow
- Half-open means circuit is trying to recover
- If circuit opens frequently, downstream service has stability issues

**Action if Open**:
1. Check if downstream service is actually broken
2. Check logs/metrics of downstream service
3. If broken, fix it
4. If recovering, wait for half-open to close automatically
5. If stuck open, might need to restart system

---

### 3.2 Metric Relationships

**Important relationships to understand**:

1. **Queue depth + Worker utilization**:
   - If queue is high AND workers busy: Need to scale workers
   - If queue is high AND workers idle: Something is broken (workers not processing)
   - If queue is low AND workers busy: Latency problem (tasks are slow)
   - If queue is low AND workers idle: Over-provisioned (could reduce workers)

2. **Throughput + Error rate**:
   - If throughput drops and error rate spikes: Failures are preventing processing
   - If throughput steady and error rate spikes: Some items fail but others succeed
   - If error rate spikes but throughput doesn't drop: Some items retried automatically

3. **Latency + Resource usage**:
   - If latency high and CPU high: CPU-bound problem (optimize code or add CPU)
   - If latency high and memory high: Memory-bound problem (optimize memory or reduce load)
   - If latency high and CPU/memory low: I/O-bound problem (disk/network issue)

4. **Circuit breaker + Error rate**:
   - If circuit opens when error rate spikes: Working as intended
   - If circuit stays open while error rate is low: Downstream might be recovering

---

## Section 4: Common Scenarios

### 4.1 Scenario 1: Queue is Full (100% capacity)

**What You See**:
- Queue depth: 100% of max size
- Queue size not decreasing
- Incoming requests being rejected
- Error rate: 100% (all new requests fail)
- Users complaining: "Your service is not accepting my request"

**Questions to Ask**:

1. **Is this the first time the queue has been full?**
   - No: Chronic capacity issue, need to scale
   - Yes: Spike in load, might be temporary

2. **What's the current throughput?**
   - High throughput: Workers are processing items, just can't keep up with incoming. Scale workers.
   - Low throughput: Workers are slow or stuck. Check if workers are actually processing.

3. **What's the worker utilization?**
   - 100% busy: Workers are at capacity, need to scale
   - < 100% busy: Something is blocking workers from processing

4. **Did incoming load increase?**
   - Yes: Organic growth, expected. Scale proactively.
   - No: Something is making workers slower. Investigate.

5. **Is latency high?**
   - Yes: Workers are slow, everything takes longer. Bottleneck is throughput.
   - No: Workers are fast but there are too many of them. This shouldn't happen at 100% queue.

**Decision Tree**:
```
Queue is full?
├─ Workers 100% busy + High throughput?
│  └─ Scale workers immediately
│
├─ Workers < 100% busy?
│  └─ Something is blocking workers
│     ├─ Check for circuit breaker open → External service broken
│     ├─ Check for deadlock → Investigate logs
│     └─ Check for resource maxed out → CPU/Memory/Disk
│
└─ Workers 100% busy + Low throughput?
   └─ Workers are slow
      ├─ Check latency → Find bottleneck
      ├─ Check CPU/Memory/Disk → Optimize resource usage
      └─ Check external service → Might be slow downstream
```

**Immediate Actions**:

**Step 1: Stop the bleeding (next 5 minutes)**
- Enable rate limiting (reject requests earlier, with clear error message)
- Page on-call engineer
- Prepare to scale

**Step 2: Scale workers (5-30 minutes)**
- Increase worker pool size by 50%
- Monitor if queue depth decreases
- If queue still full after scaling, increase again

**Step 3: Investigate root cause (while scaling)**
- Check if incoming load spiked (expected?) or something got slower
- If load spike: Expected event or unexpected?
- If slowdown: What changed? New deployment? Downstream issue?

**Step 4: Long-term fix**
- Add capacity proactively (don't wait until queue is full)
- Optimize bottleneck (if it's throughput issue, not load spike)
- Implement load shedding (gracefully reject excess load instead of queueing)

**Success Criteria**:
- Queue depth drops to < 80%
- Error rate returns to normal
- Incoming requests are being accepted again
- System stable for at least 5 minutes

---

### 4.2 Scenario 2: Circuit Breaker is Open

**What You See**:
- Circuit breaker: RED (open)
- Incoming requests getting rejected
- Error message: "Service unavailable" or "Circuit breaker open"
- Downstream service might be slow or broken

**Questions to Ask**:

1. **Is the circuit breaker correctly configured?**
   - If threshold is too low, circuit might open on minor issues
   - If threshold is too high, circuit might not protect system
   - Check circuit breaker threshold in configuration

2. **Is the downstream service actually broken?**
   - Check downstream service metrics
   - Check downstream service logs
   - Can you reach downstream service from your machine?
   - Check if there's a deployment or change happening

3. **How long has the circuit been open?**
   - < 1 minute: Might be transient, wait for half-open
   - 1-5 minutes: Downstream is having issues, investigate
   - > 5 minutes: Serious problem, need immediate action

4. **Is the circuit trying to recover?**
   - Check for half-open state (circuit is testing if service recovered)
   - If in half-open state, wait for it to close or open again
   - If stuck open, manual intervention might be needed

**Decision Tree**:
```
Circuit breaker open?
├─ Downstream service is healthy?
│  ├─ Yes: Circuit breaker is too aggressive
│  │  └─ Check threshold, might need to adjust
│  └─ No: Downstream service is broken
│     ├─ Is it a transient failure?
│     │  └─ Wait for half-open to recover (usually 30-60s)
│     └─ Is it a persistent failure?
│        └─ Investigate and fix downstream service
│
└─ Circuit in half-open state?
   ├─ Yes: Waiting for service to recover
   │  └─ Let it test, don't interrupt
   └─ No: Circuit is stuck open
      └─ Manual reset or restart might be needed
```

**Immediate Actions**:

**Step 1: Assess severity (immediately)**
- Is downstream service broken or just slow?
- Are users affected? How many?
- Can you work around it?

**Step 2: Communicate status (immediately)**
- Notify users if their requests will be rejected
- Status page: "Service experiencing issues due to downstream dependency"
- Set expectations: "Will be resolved in X minutes"

**Step 3: Fix downstream service (parallel)**
- Page downstream service owner
- If you can access downstream service, check health
- Check logs for errors
- Check metrics for resource issues

**Step 4: Force circuit to recover (if appropriate)**
- Don't force recovery if service is still broken (will just amplify load)
- Wait for service to recover, then circuit will half-open
- Or manually reset circuit once service is confirmed healthy

**Success Criteria**:
- Downstream service is healthy
- Circuit breaker transitions to half-open
- Circuit breaker closes (or opens again if service still broken)
- Requests are being processed normally

**Common Causes**:
- Downstream service crashed or deployed broken version
- Downstream service running out of resources
- Network connectivity issue between services
- Circuit breaker threshold too low (set too aggressively)

---

### 4.3 Scenario 3: Error Rate is High

**What You See**:
- Error rate: Spiking from normal (< 0.1%) to high (> 1%)
- Some/all requests failing
- Users getting errors
- Downstream might be failing or system might have a bug

**Questions to Ask**:

1. **Are errors in your system or downstream?**
   - Check error logs: Do they mention a specific component?
   - If errors in external service calls, downstream is failing
   - If errors in your code, there's a bug

2. **Is the error rate affecting all users or just some?**
   - All users: Systematic problem (bugs, bad data, external service down)
   - Some users: Intermittent issue (race condition, capacity issue)

3. **When did error rate spike?**
   - At specific time: Check what changed (deployment? load spike? external change?)
   - Gradually increasing: Degradation over time (leak, slow accumulation)

4. **Are errors retryable?**
   - Network errors: Usually retryable
   - Business logic errors: Usually not retryable
   - If retryable, are they being retried?

**Decision Tree**:
```
Error rate high?
├─ Errors in downstream service?
│  ├─ Yes: Page downstream service owner
│  │  └─ While they investigate, circuit breaker will protect your service
│  └─ No: Error is in your system
│     ├─ Check logs: What's the error message?
│     └─ Find root cause
│
├─ Did error rate spike at specific time?
│  ├─ Yes: What changed then?
│  │  ├─ New deployment? → Revert
│  │  ├─ Traffic spike? → Scale
│  │  ├─ External change? → Mitigate
│  │  └─ No obvious change? → Check logs
│  └─ No: Gradual increase
│     └─ Might be a memory leak, resource exhaustion
│
└─ Are errors retryable?
   ├─ Yes: Check if retries are exhausted
   │  └─ If DLQ is growing, retries have failed, need investigation
   └─ No: Error is permanent
      └─ Log error, understand why, fix root cause
```

**Immediate Actions**:

**Step 1: Triage (immediately)**
- Get error rate under control (stop the bleeding)
- If error rate is 100%, something is completely broken
- If error rate is high but not 100%, some items are still succeeding

**Step 2: Understand the error (next 5-10 minutes)**
- Query logs: What is the error message?
- Find one failed item, look at its trace
- Trace shows you exactly where the error occurred
- Logs show you the error details (stack trace, context)

**Step 3: Identify root cause**
- Is it in your code or downstream?
- Did something change recently?
- Is it a data problem or code problem?

**Step 4: Fix or mitigate**
- If recent deployment caused it: Revert
- If downstream is broken: Wait for them to fix, or route around
- If code bug: Fix the bug
- While fixing: Route errors to DLQ (don't lose them)

**Step 5: Verify fix**
- Error rate should drop
- Check that items are being reprocessed from DLQ
- Verify no new errors

**Success Criteria**:
- Error rate returns to < 0.1%
- Root cause identified and fixed
- No more errors appearing in logs
- DLQ is empty or stable

**Common Causes**:
- Bad deployment (code bug)
- External service broken
- Resource exhaustion (memory, disk)
- Malformed input data
- Network issue (timeouts)

---

### 4.4 Scenario 4: Latency Spike

**What You See**:
- Latency p50/p90/p99: Suddenly higher than baseline
- Some requests take much longer than usual
- Users complaining: "Your service is slow"
- Throughput might be stable or declining

**Questions to Ask**:

1. **Is the latency spike in your system or downstream?**
   - Check traces: Where does the request spend time?
   - If most time is in one step, that's your bottleneck
   - If time is distributed, it's harder to identify

2. **Is latency high for all requests or just some?**
   - All requests: System-wide issue (resource bottleneck, all downstream services slow)
   - Some requests: Intermittent issue (slow path, occasional slowness)

3. **What resource is the bottleneck?**
   - CPU high: Code is slow or doing too much
   - Memory high: Memory pressure, garbage collection pauses
   - Disk high: I/O is slow
   - Network high: Network congestion or downstream slow

4. **When did latency spike?**
   - At specific time: What changed? (deployment, load spike, external change)
   - Gradually increasing: Degradation (memory leak, disk filling up)

**Decision Tree**:
```
Latency is high?
├─ Where is the time being spent?
│  ├─ In your service: Bottleneck is here
│  │  ├─ Check CPU: High? → Optimize code
│  │  ├─ Check Memory: High? → Memory leak or too much garbage collection
│  │  ├─ Check Disk: High? → Disk I/O is slow
│  │  └─ Check network: High? → Network congestion
│  └─ In downstream: Bottleneck is there
│     └─ Page downstream service owner
│
├─ Did latency spike at specific time?
│  ├─ Yes: What changed?
│  │  ├─ New deployment? → Revert
│  │  ├─ Traffic spike? → Load is higher
│  │  ├─ External change? → Mitigate
│  │  └─ No obvious change? → Check traces
│  └─ No: Gradual increase
│     └─ Degradation (leak, accumulation)
│
└─ Is latency high for all requests or just some?
   ├─ All requests: System-wide issue
   │  └─ Bottleneck is a shared resource
   └─ Some requests: Intermittent issue
      └─ Might be slow path or occasional contention
```

**Immediate Actions**:

**Step 1: Understand the scope (immediately)**
- Is latency affecting all users or just some?
- What percentage of users are affected?
- Is the system still responsive or completely stuck?

**Step 2: Find the bottleneck (next 5-10 minutes)**
- Pick a slow request
- Look at its trace (use Jaeger or similar)
- Trace shows: Request → Service A (2ms) → Service B (50ms) → Service C (5ms)
- Service B is the bottleneck (50ms out of 57ms total)
- Focus on fixing Service B

**Step 3: Determine root cause**
- Why is the bottleneck slow?
- Is it a resource issue (CPU/Memory/Disk maxed out)?
- Is it a code issue (inefficient algorithm)?
- Is it a dependency issue (waiting for external service)?

**Step 4: Fix or mitigate**
- If resource issue: Scale that component
- If code issue: Deploy fix (if it's a bug)
- If dependency issue: Increase timeout or route around
- While fixing: Route slow requests to DLQ or handle gracefully

**Step 5: Verify fix**
- Latency should decrease
- Check that p99 comes back down to normal
- Verify no new issues introduced

**Success Criteria**:
- Latency p50 returns to baseline
- Latency p99 is reasonable (< 10x p50)
- No more slow request trace (all requests fast)
- Throughput is stable

**Common Causes**:
- External service is slow (network or overloaded)
- CPU or memory maxed out (need to scale)
- Memory leak (latency increases over time until restart)
- Inefficient query or algorithm
- Disk I/O (slow disk or full disk)
- New code introduced bottleneck

---

### 4.5 Scenario 5: Workers Keep Scaling Up

**What You See**:
- Auto-scaling keeps adding workers
- Worker count keeps increasing (20 → 30 → 40 → 50)
- But system still can't keep up
- Cost increasing due to more workers
- Something is wrong

**Questions to Ask**:

1. **Is the scaling appropriate?**
   - Is incoming load actually increasing?
   - Or is the system adding workers but throughput not improving?

2. **Is throughput increasing with more workers?**
   - Yes: Scaling is working as intended
   - No: Adding more workers doesn't help, something is wrong

3. **Is CPU/Memory constrained?**
   - If CPU/Memory on each worker is 100%, adding more won't help
   - You need to optimize code or fix resource leak

4. **Is there a bottleneck that can't be scaled?**
   - External service can only handle 100 req/sec, but workers want to send 200 req/sec
   - Database connection pool is exhausted
   - Something else is the bottleneck

**Decision Tree**:
```
Workers keep scaling up?
├─ Is incoming load actually increasing?
│  ├─ Yes: Scaling is normal
│  │  └─ Verify throughput is keeping up with load
│  └─ No: Something is preventing scaling from helping
│     └─ There's a bottleneck
│
├─ Is throughput increasing with more workers?
│  ├─ Yes: Scaling is working
│  │  └─ Continue to monitor
│  └─ No: Bottleneck is elsewhere
│     ├─ Check CPU/Memory: Are workers at 100%?
│     │  └─ Yes: Need to optimize code or fix leak
│     ├─ Check external service: Can it handle the load?
│     │  └─ No: Bottleneck is downstream
│     └─ Check for other shared resource
│        └─ Database, disk, network, etc.
│
└─ Is auto-scaling threshold appropriate?
   ├─ Threshold too low: Scaling too aggressively
   │  └─ Adjust threshold to be less aggressive
   └─ Threshold too high: Not scaling when it should
      └─ Adjust threshold to scale earlier
```

**Immediate Actions**:

**Step 1: Stop the scaling (immediately)**
- Put a cap on worker count (don't let it scale infinitely)
- This prevents runaway scaling and cost explosion

**Step 2: Understand why scaling isn't helping (next 5-10 minutes)**
- Check if throughput is increasing: Yes = scaling is working; No = bottleneck elsewhere
- If throughput is not increasing, find the bottleneck
- Is it CPU, memory, disk, network, or external service?

**Step 3: Identify the bottleneck**
- Monitor resource usage on workers: Which is maxed out?
- Monitor external service: Can it handle more load?
- Monitor throughput by worker pool: Is one pool slower than others?
- Check for resource leaks: Is memory growing over time?

**Step 4: Address the root cause**
- If code is inefficient: Optimize or fix leak
- If external service is bottleneck: Contact them or route around
- If other resource is bottleneck: Increase that resource
- If threshold is wrong: Adjust auto-scaling configuration

**Step 5: Verify fix**
- Throughput should increase with more workers (or stable if hitting different bottleneck)
- Worker count should stabilize (not keep increasing)
- Cost should stabilize

**Success Criteria**:
- Worker count stabilizes (stops increasing)
- Throughput per worker is reasonable and consistent
- System is actually handling the load (no queue buildup)
- Cost is acceptable

**Common Causes**:
- External service is bottleneck (can't scale you, can scale them)
- Memory leak (each worker uses more memory over time, forced to add more)
- CPU inefficiency (need to optimize code)
- Connection pool exhausted (need more connections or fewer workers)
- Disk I/O (disk is slow, adding workers doesn't help)
- Auto-scaling threshold too aggressive (threshold should be higher)

---

## Section 5: Decision Trees

### 5.1 Quick Decision Tree

```
Something is wrong with my system!

1. CHECK: Is my system actually broken?
   ✓ Queue depth 100%? → System overloaded
   ✓ Error rate > 1%? → Something is failing
   ✓ Latency spike? → System is slow
   ✓ Circuit open? → Downstream is broken
   ✓ Workers scaling up? → Load increasing or bottleneck
   ✓ All metrics normal? → False alarm, maybe just one blip

2. TRIAGE: What's the severity?
   CRITICAL (Page now):
   ├─ Error rate 100% (all requests failing)
   ├─ Circuit open > 5 minutes (service broken)
   ├─ Queue 100% + error rate high (cascading failure)
   └─ Incoming load = 0 (someone turned off traffic)

   HIGH (Page in 5 mins if not auto-fixing):
   ├─ Error rate > 5% (significant failures)
   ├─ Latency spike > 2x baseline (users notice)
   ├─ Queue at 100% but being processed (scale in progress)
   └─ Workers scaling up (load increasing)

   MEDIUM (Monitor, investigate):
   ├─ Error rate 0.5-5% (isolated failures)
   ├─ Latency 50% above baseline (slight slowdown)
   ├─ Queue at 80% (approaching capacity)
   └─ One worker pool slower than others

3. IMMEDIATE ACTIONS (next 5 minutes):
   Error rate high? → Check logs for error message
   Queue full? → Check incoming load, scale workers
   Latency high? → Check traces for bottleneck
   Circuit open? → Check downstream service health
   Workers scaling? → Monitor throughput, check for bottleneck

4. ROOT CAUSE ANALYSIS (while taking action):
   What changed recently?
   ├─ New deployment? → Might have bug
   ├─ Traffic pattern change? → Expected or unexpected?
   ├─ External dependency change? → Them or us?
   └─ No change? → Hmm, maybe environmental issue

5. RESOLUTION (30-60 minutes):
   For error rate → Fix bug or wait for external service
   For queue full → Scale workers (or reduce load)
   For latency → Optimize bottleneck or scale
   For circuit open → Fix downstream or mitigate
   For scaling workers → Fix bottleneck or adjust threshold
```

---

### 5.2 Incident Response Decision Tree

```
Is my system in an incident?

RED Alert (Production Down):
├─ Circuit open > 10 minutes? → YES, incident
├─ Error rate = 100%? → YES, incident
├─ Queue full + declining throughput? → YES, incident
└─ Any of the above → Call on-call engineer

YELLOW Alert (Degraded Service):
├─ Error rate 5-100%? → Investigate immediately
├─ Latency spike 2-5x? → Investigate immediately
├─ Queue filling up? → Monitor closely
└─ Any of the above → Page on-call if not auto-fixing in 2 mins

GREEN Alert (Minor Issue):
├─ Error rate < 5%? → Investigate when you have time
├─ Latency spike < 2x? → Monitor, don't panic
├─ Queue at 80%? → Plan to scale soon
└─ Any of the above → Log it, don't page anyone
```

---

## Section 6: Troubleshooting Guide

### 6.1 Common Issues and Fixes

#### Issue: Queue is constantly at 100%

**Symptom**: Queue depth always at maximum, requests rejected

**Root Cause Analysis**:
1. Is throughput keeping up? → If yes, not a processing problem
2. Is incoming load constant or spiking? → If spiking, need to handle bursts
3. Is worker utilization at 100%? → If yes, workers are at capacity

**Solution**:
- Increase max queue size (if you have memory)
- Scale workers (if workers are busy)
- Implement load shedding (gracefully reject excess load)
- Check if incoming load spike is expected

---

#### Issue: Circuit breaker keeps opening and closing

**Symptom**: Circuit open/half-open/closed repeatedly

**Root Cause Analysis**:
1. Is downstream service unstable? → Yes = circuit is working as designed
2. Is circuit breaker threshold too aggressive? → Maybe = adjust threshold

**Solution**:
- If downstream is unstable: Work with them to fix it
- If threshold is too aggressive: Increase threshold (allow more errors before opening)
- Add fallback/cache to reduce dependency on downstream
- Implement exponential backoff

---

#### Issue: Memory usage keeps growing

**Symptom**: Memory increases over time, server eventually runs out

**Root Cause Analysis**:
1. Check which part of system is using memory (workers, cache, queue)
2. Is it growing linearly (leak) or staying stable?
3. Does it grow with throughput or independent of load?

**Solution**:
- If queue: Increase queue max size or process items faster
- If cache: Add cache eviction policy (LRU, TTL)
- If worker memory: Might be memory leak, check code
- Restart system periodically as temporary fix while investigating

---

#### Issue: Some requests timeout while others succeed

**Symptom**: p99 latency is very high, but p50/p90 are normal

**Root Cause Analysis**:
1. Are timeouts in your code or downstream?
2. Is there a slow path some requests take?
3. Is there occasional contention (lock, resource)?

**Solution**:
- If slow path: Route slow requests to separate worker pool
- If occasional contention: Add more of the contended resource
- If downstream: Add retries or fallback
- Set appropriate timeout (not too short, not too long)

---

#### Issue: Auto-scaling isn't working

**Symptom**: Load increases but workers don't scale up

**Root Cause Analysis**:
1. Check auto-scaling configuration: Is it enabled?
2. Check scaling threshold: Is current load above threshold?
3. Check scaling limits: Is worker count at max already?

**Solution**:
- Enable auto-scaling if it's disabled
- Lower threshold if it's too aggressive
- Increase max worker count
- Check auto-scaling logs for errors

---

### 6.2 Troubleshooting Workflow

```
System has a problem
│
├─ Step 1: Gather information (1-2 minutes)
│  ├─ Check dashboard: What metric is red?
│  ├─ Check alerts: What triggered?
│  ├─ Check recent changes: Git log, deployments, config changes
│  └─ Estimate impact: How many users affected? How many requests failing?
│
├─ Step 2: Stabilize the system (2-5 minutes)
│  ├─ If circuit open: Assess downstream health
│  ├─ If queue full: Monitor if it's draining or still filling
│  ├─ If error rate high: Check if errors are retryable
│  └─ If latency high: Check if it's degrading further
│
├─ Step 3: Find root cause (5-15 minutes)
│  ├─ For error rate: Check logs for error type, search for similar issues
│  ├─ For latency: Trace a slow request, find bottleneck
│  ├─ For circuit open: Check downstream service metrics
│  ├─ For queue full: Check if load spiked or processing slowed
│  └─ For scaling: Check if throughput matches load
│
├─ Step 4: Implement fix (5-30 minutes)
│  ├─ If bad code: Deploy fix
│  ├─ If bad config: Update config and restart
│  ├─ If external: Wait for external fix or implement workaround
│  ├─ If capacity: Scale system
│  └─ If unknown: Roll back recent changes, try again
│
├─ Step 5: Verify fix (2-5 minutes)
│  ├─ Check dashboard: Metrics returning to normal?
│  ├─ Check alerts: Firing anymore?
│  ├─ Check logs: No more errors?
│  └─ Check traces: Latency back to baseline?
│
└─ Step 6: Post-incident review (later, when incident is over)
   ├─ What was the root cause?
   ├─ How did we detect it?
   ├─ How could we have prevented it?
   └─ What did we learn?
```

---

## Section 7: Maintenance Operations

### 7.1 Daily Maintenance Tasks

**Every morning (before main traffic)**:
1. Check dashboard for any overnight issues
2. Review error logs for overnight errors
3. Check if any long-running jobs completed successfully
4. Verify backups completed successfully

**Every 4 hours during business hours**:
1. Quick dashboard check (2 minutes)
2. Check alerts in Slack/email
3. If any alerts, investigate and resolve

**End of shift**:
1. Document any issues discovered or fixed
2. Note any trends (queue growing over time, latency increasing)
3. Hand-off to next shift: "Here's what happened, here's what to watch"

---

### 7.2 Weekly Maintenance Tasks

**Every Monday**:
1. Review SLO performance for the week
2. Check if we met our targets (99% availability, p99 < 100ms, etc.)
3. Identify any SLO misses and root causes

**Every Friday**:
1. Capacity planning review: Are we approaching limits?
2. Trend analysis: Is performance improving or degrading?
3. Plan scaling if needed before next week

**Ongoing**:
1. Review logs for recurring errors (same error multiple times)
2. Review dead-letter queue: Are items stuck there?
3. Check for resource leaks (memory growing over time)
4. Update runbooks based on new issues discovered

---

### 7.3 Regular Backups and Verification

**Backup Strategy**:
1. Backup queue state daily (or more frequently if critical)
2. Backup configuration weekly
3. Backup logs weekly (archive to cold storage)
4. Test restore procedure monthly

**Verification**:
1. Verify backup completed successfully (don't assume)
2. Periodically restore from backup to test restore procedure
3. Document backup locations and restore procedures

---

### 7.4 System Updates and Patches

**Update Process**:
1. Test updates in staging environment first
2. Deploy to canary (10%) of production
3. Monitor for issues
4. Gradually roll out to 50%, then 100%
5. Verify no degradation in performance

**Security Patches**:
1. Apply critical security patches immediately
2. Apply non-critical patches weekly
3. Verify no functionality broken by patches

---

## Appendix A: Glossary

See `tps-glossary.md` for detailed definitions.

---

## Appendix B: Emergency Contacts

**On-call Engineer**: Page at [number] or [email]
**Downstream Service Owner**: [contact]
**Database Team**: [contact]
**Network Team**: [contact]

---

**End of TPS Operations Handbook**
