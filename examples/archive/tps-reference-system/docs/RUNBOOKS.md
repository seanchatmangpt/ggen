# Alert Runbooks

Runbooks provide step-by-step procedures for responding to alerts. Each alert links to a runbook with specific troubleshooting steps.

## Table of Contents

- [SLO Violations](#slo-violations)
- [Performance Issues](#performance-issues)
- [Error Handling](#error-handling)
- [Resource Management](#resource-management)
- [Infrastructure](#infrastructure)

---

## SLO Violations

### SLO Availability Miss

**Alert:** `SLOAvailabilityMiss`
**Severity:** CRITICAL
**Expected Resolution Time:** 15 minutes

#### Symptoms
- Availability SLO target not being met
- Users unable to reach service

#### Root Cause Analysis (5 Whys)
1. Why is availability degraded? → Service is down or error rate is too high
2. Why is service down? → Deployment failure, resource exhaustion, or dependency failure
3. Why did resource exhaust? → Load increased beyond capacity or scaling failed
4. Why did scaling fail? → Resource limits hit or auto-scaling misconfigured
5. Why are limits misconfigured? → Capacity planning didn't account for growth

#### Response Procedure

1. **Assess Severity (1 min)**
   - Check how many users are affected
   - Check if service is completely down or partially degraded
   - Estimate recovery time

2. **Identify Root Cause (3 mins)**
   - Check service health dashboard
   - Review recent deployments
   - Check logs for errors
   - Look for resource exhaustion (CPU, memory, disk, connections)

3. **Immediate Mitigation (5 mins)**
   - Option A: Rollback recent deployment if correlation detected
   - Option B: Scale resources if load-related
   - Option C: Enable circuit breakers to fail fast

4. **Permanent Fix (30 mins - 2 hours)**
   - Fix underlying issue (code, config, or capacity)
   - Test fix in staging
   - Deploy fix with gradual rollout

5. **Verification (5 mins)**
   - Confirm availability SLO returning to normal
   - Check error rate trending down
   - Verify user-facing latency acceptable

6. **Post-Incident Review (Next business day)**
   - Document what failed
   - Document how it was fixed
   - Identify preventive measures
   - Update runbook with new information

### SLO Latency Miss

**Alert:** `SLOLatencyMiss`
**Severity:** CRITICAL
**Expected Resolution Time:** 20 minutes

#### Symptoms
- P99 latency exceeding SLO threshold (>2 seconds)
- Users experience slow API responses

#### Investigation Steps

1. **Verify Alert (1 min)**
   - Open Latency SLO Compliance dashboard
   - Confirm P99 latency > 2 seconds
   - Check which endpoints are affected

2. **Identify Bottleneck (3 mins)**
   - Check Tracing dashboard for slowest services
   - Look for service with highest latency contribution
   - Check if latency spike or sustained degradation

3. **Root Cause Analysis (5 mins)**
   - CPU High? → Profile CPU, identify hot functions
   - Memory High? → Check for memory leaks, GC pauses
   - Database Slow? → Review slow query log, check indexes
   - External Service Slow? → Check timeout configuration, retry logic

4. **Resolution Options**
   - Option A: Scale horizontally (add more instances)
   - Option B: Optimize code (profile and fix hot path)
   - Option C: Increase cache TTL (reduce backend calls)
   - Option D: Add connection pooling (for database/RPC)

### SLO Error Budget Low/Critical

**Alert:** `SLOErrorBudgetLow` / `SLOErrorBudgetCritical`
**Severity:** WARNING / CRITICAL
**Expected Resolution Time:** Varies

#### Symptoms
- Error budget remaining < 25% (LOW) or < 5% (CRITICAL)
- Days until budget exhaustion displayed

#### Actions

**For LOW Alert:**
1. Review which services are burning budget fastest
2. Prioritize performance improvements
3. Plan SLO adjustment if baseline changed
4. Consider reducing deployment frequency to minimize blast radius

**For CRITICAL Alert:**
1. STOP all feature deployments immediately
2. Enter incident response mode
3. Focus 100% resources on stability improvements
4. Prioritize bug fixes over feature work
5. Review all recent changes for issues
6. Implement temporary rate limiting if needed
7. Brief executive team on status

---

## Performance Issues

### Latency Spike Detected

**Alert:** `LatencySpikeDetected`
**Severity:** WARNING
**Expected Resolution Time:** 10-30 minutes

#### Investigation Checklist

1. **Check Metric Correlation Dashboard**
   - Which metrics spiked with latency?
   - CPU usage? Memory? Lock contention? Database queries?

2. **Check Infrastructure**
   ```
   - CPU usage % by service
   - Memory usage % by service
   - Disk I/O utilization
   - Network I/O utilization
   - Database connection count
   ```

3. **Profile Slow Path**
   - Use Profiling dashboard
   - Identify CPU hot spots
   - Check lock contention
   - Review GC pause times

4. **Check Dependencies**
   - Is database slow? (check query times)
   - Is cache slow? (check hit rate)
   - Is external API slow? (check response times)

5. **Correlate with Changes**
   - Recent deployments
   - Increased load
   - Resource limit reached
   - Configuration changes

### P99 Latency Critical

**Alert:** `P99LatencyCritical`
**Severity:** CRITICAL
**Expected Resolution Time:** 15-30 minutes

#### Emergency Response

1. **Page On-Call Immediately** ✓ (Automated)

2. **Start Incident Channel**
   - Create Slack incident thread
   - Invite relevant teams
   - Document timeline

3. **Identify Affected Users**
   - How many? (via error count)
   - Which features? (check endpoint breakdown)
   - How long affected?

4. **Root Cause Detection (Automated)**
   - Run Metric Correlation engine
   - Check which metrics spiked with latency
   - Follow diagnosis tree:
     - CPU High → Profile code, scale, or optimize
     - Memory High → Check leaks, GC settings
     - Lock Contention → Reduce lock scope
     - Database Slow → Optimize queries, add indexes

5. **Implement Mitigation**
   - Option A: Scale horizontally (fastest, less risky)
   - Option B: Circuit break slow dependency
   - Option C: Increase timeout threshold
   - Option D: Reduce feature complexity (degrade gracefully)

6. **Deploy Fix**
   - Prepare hotfix or configuration change
   - Test in staging
   - Deploy to canary first
   - Monitor SLO recovery

---

## Error Handling

### High Error Rate

**Alert:** `HighErrorRate`
**Severity:** CRITICAL
**Expected Resolution Time:** 15-30 minutes

#### Error Investigation

1. **Check Error Type Distribution**
   - 5xx (server errors) → Application bug
   - 4xx (client errors) → API contract violation or rate limiting

2. **Identify Affected Service**
   ```bash
   # Check which service is failing
   curl http://monitoring.internal/api/errors?last=5m | jq '.[].service' | sort | uniq -c | sort -rn
   ```

3. **Root Cause by Error Code**

   **500 (Internal Server Error)**
   - Check service logs for exceptions
   - Check if recent deployment caused it
   - Check if dependent service is down

   **502 (Bad Gateway)**
   - Check if backend service is down
   - Check load balancer health
   - Check upstream service logs

   **503 (Service Unavailable)**
   - Check if service is scaling
   - Check if resource limits reached
   - Check connection pool status

   **429 (Too Many Requests)**
   - Check if rate limiting is too strict
   - Check if load increased
   - Check if retry storms happening

4. **Immediate Mitigation**
   - Option A: Rollback recent deployment
   - Option B: Scale resources if load-related
   - Option C: Enable circuit breaker to fail fast
   - Option D: Increase timeout for slow dependency

5. **Permanent Fix**
   - Fix root cause in code
   - Add test case for regression
   - Deploy with gradual rollout

### Error Rate Increasing Trend

**Alert:** `ErrorRateIncreasing`
**Severity:** WARNING
**Expected Resolution Time:** 30 minutes - 2 hours

#### Preventive Actions

1. **Don't Wait for Critical Alert** - Act now
2. **Identify Correlation**
   - Did error increase after deployment?
   - Did load increase?
   - Did configuration change?

3. **Prepare for Rollback**
   - Be ready to rollback last deployment
   - Test rollback procedure
   - Have on-call ready

4. **Investigate Root Cause**
   - Review recent commits
   - Check metrics correlation
   - Analyze error logs

5. **Monitor Trend**
   - If trend continues → Rollback
   - If trend stops → Continue investigation
   - If trend reverses → Deployment worked

---

## Resource Management

### High CPU Usage

**Alert:** `HighCPUUsage`
**Severity:** WARNING
**Expected Resolution Time:** 30 minutes

#### Diagnostics

1. **Confirm Alert**
   - Open Profiling dashboard
   - Verify CPU usage > 85%

2. **Check What's Using CPU**
   ```bash
   # View CPU profile (requires profiling enabled)
   curl http://service:6060/debug/pprof/profile?seconds=30 > cpu.prof
   go tool pprof cpu.prof
   ```

3. **Identify Hot Code Path**
   - CPU flame graph on Profiling dashboard
   - Look for expensive operations (loops, allocations, syscalls)

4. **Resolution Options**
   - Option A: Optimize hot path code (fastest long-term)
   - Option B: Scale horizontally (quick relief)
   - Option C: Reduce load (rate limiting, prioritization)
   - Option D: Adjust CPU limits in config

### High Memory Usage

**Alert:** `HighMemoryUsage`
**Severity:** WARNING
**Expected Resolution Time:** 30-60 minutes

#### Investigation

1. **Check Memory Profile**
   ```bash
   # Get memory allocation profile
   curl http://service:6060/debug/pprof/heap > heap.prof
   go tool pprof heap.prof
   ```

2. **Identify Memory Leak**
   - Are heap objects growing over time?
   - Check garbage collection statistics
   - Review object allocations by type

3. **Resolution Options**
   - Option A: Fix memory leak in code (long-term)
   - Option B: Increase heap size (quick fix)
   - Option C: Reduce cache size
   - Option D: Tune GC aggressiveness

### High GC Pause Times

**Alert:** `HighGCPauseTimes`
**Severity:** WARNING
**Expected Resolution Time:** 30 minutes - 2 hours

#### GC Optimization

1. **Check GC Statistics**
   - Current heap size
   - GC frequency
   - Allocation rate

2. **Reduce Allocation Churn**
   - Profile allocations on Profiling dashboard
   - Identify hot allocation sites
   - Use object pooling for frequently allocated objects
   - Reduce string concatenation

3. **Tune GC Settings**
   - Adjust GOGC environment variable
   - Increase heap size to reduce GC frequency
   - Consider different GC algorithm (if available)

---

## Infrastructure

### Disk Space Low

**Alert:** `DiskSpaceLow`
**Severity:** WARNING
**Expected Resolution Time:** 15-30 minutes

#### Emergency Actions

1. **Immediate Mitigation**
   - Stop log rotation temporarily
   - Identify and delete old logs
   - Clean up /tmp files
   - Delete old container images

2. **Permanent Fix**
   - Increase disk size
   - Implement log rotation with smaller retention
   - Implement disk cleanup jobs

### Database Connection Pool Exhausted

**Alert:** `DatabaseConnPoolExhausted`
**Severity:** CRITICAL
**Expected Resolution Time:** 5-15 minutes

#### Emergency Response

1. **Immediate Actions**
   - Increase connection pool size (requires restart)
   - Kill idle connections from database
   - Check for connection leaks in application

2. **Investigation**
   - Are connections stuck?
   - Is there a slow query holding connections?
   - Is connection recycling broken?

3. **Long-Term Fix**
   - Implement connection timeout
   - Add connection pool monitoring
   - Implement slow query alert
   - Review connection usage patterns

### Service Unhealthy

**Alert:** `ServiceUnhealthy`
**Severity:** CRITICAL
**Expected Resolution Time:** 5-10 minutes

#### Troubleshooting

1. **Check Service Logs**
   ```bash
   kubectl logs -f deployment/service-name
   ```

2. **Check Dependencies**
   - Is database up? (check connectivity)
   - Is cache up? (check Redis/Memcached)
   - Are external APIs reachable?

3. **Recovery Options**
   - Option A: Restart service (quick recovery)
   - Option B: Redeploy (if config issue)
   - Option C: Investigate logs and fix root cause

---

## Alert Acknowledgment & Escalation

### For On-Call Engineer

1. **Acknowledge Alert** (within 2 minutes)
   - Click "acknowledge" in PagerDuty
   - Post in Slack: "I'm investigating [ALERT_NAME]"

2. **Investigate** (next 5-15 minutes)
   - Follow runbook procedure
   - Check dashboards
   - Review logs

3. **Update Status**
   - Post progress updates in Slack
   - If stuck: Escalate to on-call manager
   - If resolved: Post root cause in Slack thread

### Escalation Criteria

- **Can't identify root cause in 10 mins** → Escalate to on-call manager
- **Fix requires code changes** → Page engineering manager
- **Affects multiple services** → Page incident commander
- **Requires infrastructure changes** → Page infrastructure team

---

## Dashboard & Tool Links

- **Grafana Dashboards:** https://grafana.internal/
- **Prometheus Queries:** https://prometheus.internal/
- **Service Logs:** https://elasticsearch.internal/
- **Trace Analysis:** https://jaeger.internal/
- **Status Page:** https://status.company.com/

---

## Contact Information

- **On-Call Rotation:** [PagerDuty Schedule](https://company.pagerduty.com)
- **SRE Team:** @sre-team in Slack (#sre-incidents)
- **Engineering Manager:** [Contact info]
- **Infrastructure Team:** #infrastructure in Slack
