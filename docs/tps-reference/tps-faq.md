# TPS Frequently Asked Questions (FAQ)

**Version**: 1.0
**Last Updated**: January 2026
**Purpose**: Answer common questions about TPS and system operations

---

## Design Philosophy Questions

### Q: Why does the system reject work instead of queueing?

**A**: This is a core TPS principle (Jidoka). Here's why:

**Scenario: Queueing Everything**
```
Incoming: 500 req/sec
Throughput: 100 req/sec
Queue: Grows by 400 items/sec

After 1 hour:
- Queue has 1.44 million items waiting
- User who sent request 1 hour ago still waiting
- User eventually times out (request lost)
- System consumed massive memory (probably crashes)
```

**Scenario: Reject When Full**
```
Incoming: 500 req/sec
Throughput: 100 req/sec
Queue max: 1000 items

When queue full:
- New request rejected immediately (< 100ms)
- System has spare capacity for other requests
- User gets instant feedback: "System busy, try again"
- After a few seconds, queue drains, system accepts requests again
```

**The Key Insight**: It's better to tell a user "I'm overloaded, try again" than to let them wait 1 hour and then fail anyway.

**In Real World**:
- "This elevator is full, please use next elevator" (Jidoka)
- vs "Get in, wait in line for hours" (Push queue)

---

### Q: Why do we need different worker pools?

**A**: This is Heijunka (load leveling). Here's why:

**Scenario: All workers same**
```
10 workers, all configured for medium complexity

Fast task (1ms to process):
- Worker starts: 0ms
- Processing: 1ms
- Free: 1ms

Slow task (100ms to process):
- Worker starts: 0ms
- Processing: 100ms
- Free: 100ms

If queue is 90% fast, 10% slow:
- 10 workers process mix of fast/slow
- But any worker assigned slow task is blocked for 100ms
- That worker could have processed 100 fast tasks instead
- Throughput is limited by slowest task type
```

**Scenario: Separate pools**
```
10 fast-workers (optimized for fast tasks)
2 slow-workers (optimized for slow tasks)

Fast queue: 500 items/sec → 10 workers = 50 items/sec per worker (fine)
Slow queue: 50 items/sec → 2 workers = 25 items/sec per worker (fine)

Throughput for fast tasks: Limited by fast-workers only
Throughput for slow tasks: Limited by slow-workers only
Total throughput: Higher than mixed pool
```

**Real World Analogy**:
- Fast-food line (fast orders)
- Sit-down restaurant (slow orders)
- If you mix them: Everyone waits for slow orders
- If separate: Fast orders served quickly, slow orders get attention

---

### Q: What's the difference between circuit breaker and load limiter?

**A**: Different purposes, often used together:

**Circuit Breaker**:
- Purpose: Prevent cascading failure
- Trigger: Service is failing or timing out
- Action: Stop sending requests (let service recover)
- Example: "Service B is down, stop calling it"

**Load Limiter**:
- Purpose: Prevent overload (rate limiting)
- Trigger: Incoming load too high
- Action: Slow down or reject requests (spread load over time)
- Example: "Users are sending too many requests, spread them out"

**Together**:
```
User sends 1000 req/sec
↓
Load limiter: "I can handle 100 req/sec, queue the rest"
↓
Queued requests slowly sent to Service A
↓
Service A sends requests to Service B
↓
If Service B fails: Circuit breaker opens
↓
Service A returns error to user (fail fast)
↓
User retries later when Service B recovers
```

**Analogy**:
- Load limiter = Bouncer at club: "We're at capacity, wait outside"
- Circuit breaker = Emergency stop button: "Fire detected, stop elevator"

---

### Q: How do I know if my system is following TPS?

**A**: Check these characteristics:

**Does your system have...**

- [ ] **Jidoka**: System detects failures and stops (doesn't amplify)
  - Circuit breaker opens when service fails?
  - Dead-letter queue for failed items?
  - Health checks failing when unhealthy?

- [ ] **Kanban**: Pull-based work with limits
  - Queue has maximum size?
  - Workers pull work when ready?
  - Full queue rejects new work?

- [ ] **Andon**: Problem visibility
  - Real-time dashboard showing health?
  - Alerts when problems occur?
  - Traces for debugging?
  - Logs for understanding?

- [ ] **Kaizen**: Continuous improvement
  - SLOs defined (targets for performance)?
  - Metrics measured (actual performance)?
  - Post-incident reviews (learn from failures)?
  - Regular optimization (not just react to problems)?

- [ ] **Heijunka**: Load leveling
  - Multiple worker pools (fast/slow separate)?
  - Rate limiting (prevent spikes)?
  - Auto-scaling (add workers when needed)?
  - Even distribution of work?

- [ ] **Value Stream**: See the waste
  - Traces showing request flow?
  - Bottleneck identified (slowest step)?
  - Optimizing bottleneck (not just the fast path)?

**If all checked**: You're doing TPS right.

---

### Q: Can TPS be used outside manufacturing?

**A**: Absolutely. TPS principles apply to any system with:
- **Work arriving**: Items to process (manufacturing: parts, software: requests)
- **Workers**: Things that process work (manufacturing: machines, software: threads/processes)
- **Queues**: Work waiting (manufacturing: part bins, software: message queues)
- **Bottlenecks**: Where work gets slow (manufacturing: machine speed, software: network, CPU)

**Examples**:
- **Customer service**: Tickets in queue, agents process them
- **Hospital**: Patients in waiting room, doctors treat them
- **Restaurants**: Orders in queue, cooks prepare them
- **Cloud infrastructure**: API requests in queue, servers process them
- **Data pipeline**: Data in queue, workers transform/process it

**Key**: Wherever you have work arriving, queuing, and getting processed, TPS can help.

---

## Operational Questions

### Q: Why is observability so important?

**A**: You can't improve what you don't measure. Here's why:

**Without observability**:
```
"System is slow"
↓ (No visibility)
"I don't know why, let me guess..."
↓ (Random tuning)
"Did that help? I have no idea..."
↓
Cycle repeats
```

**With observability**:
```
"System is slow (p99 latency = 500ms, SLO = 100ms)"
↓ (Trace shows bottleneck)
"Database queries taking 400ms out of 500ms total"
↓ (Specific problem identified)
"Add index to frequently-queried column"
↓ (Measured improvement)
"New p99 latency = 50ms (5x faster!)"
```

**Observability gives you**:
- **Visibility**: See what's happening
- **Diagnosis**: Understand why it's happening
- **Optimization**: Know what to improve
- **Verification**: Confirm improvement worked

**Investment**: Good observability takes effort but saves time (10 hours debugging → 30 minutes with good traces).

---

### Q: How do I get alerts for every problem?

**A**: Set up multi-level alerting:

**Level 1: Metric-based Alerts** (automatically triggered)
- Error rate > 1%: Page on-call immediately
- Latency p99 > 3x baseline: Page on-call
- Circuit breaker open > 5 minutes: Page on-call
- Queue at 100%: Page on-call
- Etc.

**Level 2: Log Pattern Alerts** (specific error detection)
- Any ERROR logs: Email alert
- Panic/crash logs: Page on-call
- Database connection failed: Page on-call
- Etc.

**Level 3: Trend Alerts** (degradation detection)
- Latency increasing 2% per hour: Email alert (investigate trend)
- Error rate gradually increasing: Email alert (memory leak?)
- Queue depth increasing over days: Email alert (capacity issue)

**Level 4: Manual Checks** (regular review)
- Daily: Review dashboard health
- Weekly: Review performance trends
- Monthly: Review SLO performance

**Key**: Automate what you can, augment with manual review.

---

### Q: Why is SLO violation important?

**A**: SLOs define "acceptable" vs "broken":

**Without SLO**:
- "Is system working?" → "Depends how you measure"
- No clear target = no clear failure detection

**With SLO**:
- "Is system working?" → "Meet SLO or not"
- Clear target = clear pass/fail

**SLO Example**:
```
SLO: 99% availability (< 0.1% errors) + p99 latency < 100ms

Monday:
- Error rate: 0.08% ✓
- Latency: 85ms ✓
- SLO: MET

Tuesday:
- Error rate: 0.15% ✗ (exceeds 0.1%)
- Latency: 95ms ✓
- SLO: VIOLATED

Action: Investigate why error rate increased, fix issue
```

**Benefits**:
- **Accountability**: Can measure if you're meeting promises
- **Planning**: Know when to scale (before violating SLO)
- **Optimization**: Measure if improvements actually help

---

### Q: What if my system is doing everything right but still failing?

**A**: You might be hitting a limit of your architecture:

**Scenarios**:
1. **Network limit**: All network bandwidth used
2. **Database limit**: Database can't handle throughput (even with optimization)
3. **Storage limit**: Disk full
4. **Cost limit**: Scaling would cost too much
5. **Hardware limit**: Can't add more workers (cloud quota reached)

**Solutions**:
1. **Network**: Add CDN, optimize protocol, implement caching
2. **Database**: Add caching, implement read replicas, denormalize
3. **Storage**: Archive old data, compress, clean up
4. **Cost**: Optimize efficiency, use cheaper resources, negotiate pricing
5. **Hardware**: Ask for quota increase, migrate to different platform

**Key**: Not every problem is a code bug. Sometimes you've optimized everything and need to change architecture.

---

## Troubleshooting Questions

### Q: My circuit breaker keeps opening and closing. Is that bad?

**A**: Depends on why:

**If downstream service is unstable**:
- Circuit opening/closing is expected
- Circuit is working as designed
- Fix: Stabilize downstream service (they have a problem)

**If circuit is overly sensitive**:
- Circuit opening on transient failures (rare network blip)
- Hurts availability (should fail gracefully instead)
- Fix: Increase circuit threshold (allow more errors before opening)

**If circuit is stuck open**:
- Circuit won't close even though service recovered
- This is bad (blocking all traffic even though service is fine)
- Fix: Manual reset or check circuit implementation

**Healthy behavior**:
- Circuit closed most of the time
- Opens occasionally when service actually fails
- Closes again once service recovers
- Don't sees 100+ open/close cycles per hour

---

### Q: What should I do when auto-scaling isn't keeping up?

**A**: Multi-step approach:

1. **Immediate** (next 1 minute):
   - Manually scale up (don't wait for auto-scaler)
   - This buys time while investigating

2. **Short-term** (next 5 minutes):
   - Check auto-scaling metrics (is it seeing the load spike?)
   - Check scaling limits (is worker count at max?)
   - Check resource limits (is cloud quota hit?)

3. **Medium-term** (next 30 minutes):
   - Optimize scaling threshold (trigger earlier)
   - Increase max worker count (allow more scaling)
   - Add cooldown (prevent scaling from being too jittery)

4. **Long-term**:
   - Predict load spikes (pre-scale before spike)
   - Reduce latency per worker (handle more requests per worker)
   - Implement request throttling (prevent spike in first place)

---

### Q: Error rate spiked but circuit breaker is still closed. What's happening?

**A**: Not all errors trigger circuit breaker:

**Circuit breaker triggers**:
- Timeout (request too slow)
- Connection refused (service down)
- Service unavailable (500 errors)

**Circuit breaker doesn't trigger**:
- Business logic errors (400 errors, bad request)
- User not found error (404)
- Validation errors
- Downstream slow (but still responding)

**When error rate spikes but circuit is closed**:
1. Check error type: What specific error are users getting?
2. Check logs: Any error messages?
3. Check for bad data: Did something invalid get sent?
4. Check recent deployment: Did code change?
5. Check downstream: Is downstream service slow (not failing)?

**If errors are transient**:
- Circuit may not open (not enough to trigger threshold)
- Errors might auto-recover
- Keep monitoring

**If errors persistent**:
- Circuit should eventually open
- Or adjust circuit threshold (if too high)
- Or fix the underlying issue

---

## Advanced Questions

### Q: Should I use circuit breaker or bulkhead pattern?

**A**: Use both (they're complementary):

**Circuit Breaker**:
- Stops sending requests when service fails
- Protects downstream service from overload
- Example: "Service B is down, stop calling it"

**Bulkhead**:
- Separate resource pools (don't let one service starve others)
- Prevents cascade when one service fails
- Example: "Slow requests use pool B, fast requests use pool A"

**Together**:
```
Fast Worker Pool ──→ Service A ─→ Service C (fast)
                  ↘ Circuit breaker (protects from cascade)

Slow Worker Pool ──→ Service B ─→ Service C (slow)
                  ↘ Circuit breaker (protects from cascade)

If Service C becomes slow:
- Circuit breaker opens for all calls
- Fast worker pool still handles fast requests
- Slow worker pool fails gracefully
- System continues (degraded but not down)
```

---

### Q: How do I tune circuit breaker thresholds?

**A**: Empirical approach:

1. **Understand the distribution**:
   - How often do transient failures occur? (1 per hour? per day?)
   - How long do they usually take to resolve? (seconds? minutes?)

2. **Set threshold too low first**:
   ```
   - Consecutive failures before open: 2
   - Results: Circuit opens too easily
   - Observe: Are transient failures being amplified?
   ```

3. **Gradually increase**:
   ```
   - Consecutive failures before open: 5
   - Observe: Does circuit still open for real failures?
   - Does it stay open for transient failures?
   ```

4. **Find sweet spot**:
   ```
   - Consecutive failures before open: 10
   - Timeout: 60 seconds
   - Half-open requests: 3
   - Results: Opens for real failures, closes quickly after recovery
   ```

5. **Verify in staging**:
   - Test with various failure scenarios
   - Confirm behavior is correct

---

### Q: How do I know if I need to refactor my system?

**A**: Look for these warning signs:

**Architecture Signs**:
- [ ] Can't add workers anymore (hitting a bottleneck)
- [ ] Database is bottleneck (even with caching/optimization)
- [ ] Network is bottleneck (even with compression)
- [ ] Single point of failure that can't be worked around

**Operational Signs**:
- [ ] Incidents caused by architectural limitations
- [ ] SLOs can't be met without massive over-provisioning
- [ ] Scaling cost is prohibitive (too much money)

**Performance Signs**:
- [ ] Optimized everything but still not meeting SLO
- [ ] Latency is 10x+ what reasonable architecture would be
- [ ] Resource usage is inefficient (high CPU for low throughput)

**If all of above**: Might need to refactor
**If some of above**: Optimize what you have first

---

**End of TPS FAQ**
