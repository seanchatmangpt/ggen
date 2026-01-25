# TPS Glossary: Manufacturing Terms in Software Context

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: All skill levels (non-technical translation provided)

---

## Toyota Production System (TPS) Terms

### Jidoka (自働化) - Autonomation with Human Touch

**Manufacturing Original**: Looms that stop automatically when thread breaks (don't amplify defects)

**Software Translation**: System detects problems and stops processing, instead of amplifying failures downstream.

**Example in TPS Systems**:
- Worker detects an error and stops
- Circuit breaker detects downstream failure and rejects requests (don't keep retrying)
- Dead-letter queue catches failed items (don't queue them forever)

**Why It Matters**: Without jidoka, one failure becomes many failures. With jidoka, one failure stops the line but nothing else is affected.

**For Operators**: When circuit breaker opens, that's jidoka protecting you from cascading failure.

---

### Kanban (看板) - Pull-Based Work

**Manufacturing Original**: Visual cards signaling need for work (pull what you need, not push what you have)

**Software Translation**: Workers pull work from queue when ready, instead of work being pushed to them.

**Example in TPS Systems**:
- Worker pulls item from queue when idle
- Queue has maximum size (limits work-in-progress)
- If queue is full, new work is rejected (backpressure)

**Why It Matters**: Push systems overload and collapse. Pull systems naturally limit work and maintain stability.

**For Operators**: When queue is full, that's kanban telling you to add workers. When queue is empty, you have spare capacity.

---

### Andon (安灯) - Problem Visibility

**Manufacturing Original**: Red light/bell that alerts everyone when problem detected (don't hide failures)

**Software Translation**: Metrics, dashboards, alerts, and traces make all problems visible immediately.

**Example in TPS Systems**:
- Dashboard shows metrics in real-time (red=problem, green=good)
- Alerts notify team when thresholds exceeded
- Traces show exact path and timing of each request
- Logs record every error with context

**Why It Matters**: Hidden problems compound. Visible problems get fixed.

**For Operators**: Andon is your dashboard, alerts, and traces. Don't ignore them.

---

### Kaizen (改善) - Continuous Improvement

**Manufacturing Original**: System of small improvements (1% better every week = 38x better per year)

**Software Translation**: Measure performance, identify bottlenecks, improve systematically, repeat.

**Example in TPS Systems**:
- SLO targets define "good" (99% availability, p99 < 100ms)
- Metrics show actual performance vs target
- When miss SLO, analyze why and fix
- Repeat every week/month

**Why It Matters**: Systems degrade over time without active improvement.

**For Operators**: When you miss SLO, understand why. Don't just re-tune knobs.

---

### Heijunka (平準化) - Load Leveling

**Manufacturing Original**: Spread production evenly over time (no spikes, no valleys)

**Software Translation**: Distribute requests evenly, prevent spikes, separate fast/slow tasks.

**Example in TPS Systems**:
- Multiple worker pools: fast tasks separate from slow tasks
- Rate limiting: spread incoming requests over time
- Auto-scaling: add workers gradually (not all at once)
- Circuit breaker: reduce load to overloaded service

**Why It Matters**: Spikes cause overload and cascading failure. Level load = stable performance.

**For Operators**: When you see spiky throughput, that's a heijunka problem. Implement rate limiting.

---

### Muda (無駄) - Waste

**Definition**: Work that doesn't add value (should be eliminated).

**Examples in TPS Systems**:
- Overproduction: Queueing items that aren't needed yet
- Waiting: Items waiting in queue (latency)
- Motion: Unnecessary computation or data movement
- Defects: Failed items that need rework
- Over-processing: Doing more than customer needs
- Inventory: Items sitting in queue (work-in-progress)

**For Operators**: Look at your value stream. Where is work getting stuck?

---

### Value Stream (バリューストリーム) - Path from Input to Output

**Definition**: Track the complete flow from user request to response.

**Example Flow**:
```
User → Load Balancer → Service A → Service B → Database → Service B → Service A → User
    2ms      1ms         10ms      5ms      50ms      5ms      10ms    2ms
= 85ms total latency

Waste Analysis:
- Database is bottleneck (50ms out of 85ms)
- Other components fast
- Optimize database (add cache, index, etc.)
```

**For Operators**: Trace requests to find bottlenecks. Optimize the slowest step.

---

## Software-Specific Terms

### Circuit Breaker

**Definition**: Pattern to prevent cascading failure by stopping requests to failing service.

**States**:
- **Closed**: Service healthy, requests flowing normally
- **Open**: Service unhealthy/slow, requests being rejected
- **Half-Open**: Testing if service recovered, allowing few requests through

**Example**:
```
Service A calls Service B
- If Service B responding ok: Circuit closed (normal)
- If Service B failing: Circuit opens (reject requests to B)
  - Prevents amplifying load to already-failing B
  - Allows B to recover without overload
- Once B recovers: Circuit closes (back to normal)
```

**For Operators**: When circuit opens, don't panic. System is protecting itself.

---

### Dead-Letter Queue (DLQ)

**Definition**: Special queue for items that failed processing and can't be retried.

**Example**:
```
Main Queue: Item → Process → Success
            Item → Process → Failure (retry) → Failure again → Dead-Letter Queue

Dead-Letter Queue: Items stuck here until manual review/fix
```

**For Operators**: Growing DLQ means something is broken. Investigate why items are failing.

---

### Throughput

**Definition**: How much work the system processes per unit time (items/sec, requests/sec).

**Example**:
- System processing 100 req/sec
- This is throughput
- Goal: Maintain steady throughput matching incoming load

**For Operators**: If throughput is dropping, system is slowing down. Find bottleneck.

---

### Latency

**Definition**: How long one item takes to process (from start to finish).

**Percentiles**:
- **p50 (median)**: 50% of items faster, 50% slower (usually what users feel)
- **p90**: 90% of items faster than this (slow items)
- **p99**: 99% of items faster than this (very slow outliers)

**Example**:
```
p50 = 10ms: Half the requests are < 10ms, half are > 10ms
p99 = 50ms: 99% of requests are < 50ms, 1% are very slow
```

**For Operators**: Watch p99 (that's what your worst users experience).

---

### Queue Depth

**Definition**: Number of items currently waiting in queue.

**Interpretation**:
- Empty queue: Workers are idle (spare capacity)
- 50% full: Normal (good load balance)
- 100% full: Workers can't keep up (need to scale)

**For Operators**: Queue depth shows if system is balanced.

---

### Worker Utilization

**Definition**: Percentage of workers that are busy (processing items).

**Interpretation**:
- 0% busy: Workers idle (over-provisioned, can reduce)
- 60-80% busy: Optimal (good balance of capacity and efficiency)
- 100% busy: Workers at capacity (need to scale)

**For Operators**: Target 60-80% utilization. Below = waste, above = risk.

---

### Cascade/Cascading Failure

**Definition**: One component fails, which causes others to fail, which causes others to fail.

**Example**:
```
Service C slow → Service B queue fills → Service B slow
→ Service A queue fills → Service A slow
→ Users experience slow system
→ More retries → More load → Everyone fails
```

**Prevention**: Circuit breaker + Jidoka + Heijunka

**For Operators**: Circuit breaker prevents cascades by failing fast.

---

### Auto-Scaling

**Definition**: System automatically adds/removes workers based on load.

**How It Works**:
```
Load increases → Utilization > 80% → Add workers
Load decreases → Utilization < 30% → Remove workers
```

**For Operators**: Good auto-scaling should be "boring" (happens automatically).

---

### Canary Deployment

**Definition**: Deploy to small percentage of traffic first, verify before full rollout.

**Process**:
```
1. Deploy to 10% of traffic (canary)
2. Monitor for issues
3. If good: Deploy to 50% (staging)
4. If still good: Deploy to 100% (production)
5. If bad at any point: Rollback to previous version
```

**For Operators**: Canary catches issues before they affect everyone.

---

### SLO (Service Level Objective)

**Definition**: Target for system performance (what "good" means).

**Examples**:
- Availability: 99% of requests succeed (0.1% error rate acceptable)
- Latency: 99% of requests < 100ms (p99 < 100ms)
- Throughput: 1000 requests/sec

**For Operators**: SLOs define when system is healthy vs degraded.

---

### SLI (Service Level Indicator)

**Definition**: Actual measurement of performance (how system is doing vs target).

**Example**:
```
SLO: p99 latency < 100ms
SLI: Actual p99 latency = 85ms (meeting SLO)
```

**For Operators**: SLI shows if you're meeting SLO.

---

### RPS (Requests Per Second)

**Definition**: Throughput measured in requests per second.

**Example**: "System handling 1000 RPS" = 1000 requests per second.

**For Operators**: RPS shows system load magnitude.

---

## Monitoring Terms

### Metric

**Definition**: Measured value over time (number that changes).

**Examples**:
- Error rate: 0.05% (number)
- Latency: 25ms (number)
- Queue depth: 500 items (number)
- CPU usage: 45% (number)

**For Operators**: Metrics are what you watch on dashboard.

---

### Trace

**Definition**: Detailed record of one request's journey through system.

**Example**:
```
Request 123:
├─ Load Balancer (2ms)
├─ Service A (10ms)
│  ├─ Parse request (1ms)
│  └─ Call Service B (8ms)
├─ Service B (5ms)
│  └─ Database query (4ms)
└─ Response to user (2ms)
Total: 19ms
```

**For Operators**: Traces show where time is being spent (find bottlenecks).

---

### Log

**Definition**: Timestamped record of an event.

**Example**:
```
2026-01-25T10:30:45Z ERROR database query timeout (5000ms)
2026-01-25T10:30:46Z WARNING retry attempt 1 of 3
2026-01-25T10:30:47Z ERROR query failed, routing to dead-letter queue
```

**For Operators**: Logs show what happened (for debugging).

---

### Alert

**Definition**: Notification when metric crosses threshold.

**Example**:
```
Alert: Error rate > 1%
Triggered when: Actual error rate = 2%
Notification: Slack message, PagerDuty, email
```

**For Operators**: Alerts tell you something needs attention.

---

## Common Acronyms

| Acronym | Meaning | Definition |
|---------|---------|-----------|
| TPS | Toyota Production System | Manufacturing methodology applied to software |
| SLO | Service Level Objective | Target for system performance |
| SLI | Service Level Indicator | Actual performance vs target |
| RPS | Requests Per Second | Throughput measurement |
| p50/p90/p99 | Percentiles | Latency percentiles (50th, 90th, 99th) |
| QoS | Quality of Service | Ability to meet performance targets |
| DLQ | Dead-Letter Queue | Queue for failed items |
| RTO | Recovery Time Objective | How long to recover from failure |
| RPO | Recovery Point Objective | How much data loss is acceptable |
| MTTF | Mean Time To Failure | Average time before failure occurs |
| MTTR | Mean Time To Repair | Average time to fix when failure occurs |
| TTD | Time To Detect | How long to detect a problem |
| TTR | Time To Resolve | How long to fix the problem |

---

## Metric Interpretation Quick Reference

| Metric | Good Value | Concerning | Critical |
|--------|-----------|-----------|----------|
| Error Rate | < 0.1% | 0.1-1% | > 1% |
| Latency p99 | < 100ms | 100-500ms | > 500ms |
| Queue Depth | 40-60% | 60-80% | > 80% |
| Worker Util. | 60-80% | 40-60% or 80-95% | > 95% |
| Throughput | Stable | Growing or declining | > 20% variance |
| Circuit Breaker | Closed | Half-open | Open > 5min |
| Availability | > 99.9% | 99.0-99.9% | < 99% |

---

**End of TPS Glossary**
