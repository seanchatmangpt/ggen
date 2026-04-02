<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Grafana Dashboard Guide](#grafana-dashboard-guide)
  - [Dashboard 1: TPS Overview (Main Dashboard)](#dashboard-1-tps-overview-main-dashboard)
    - [Panel 1: System Status (Top-Left)](#panel-1-system-status-top-left)
    - [Panel 2: Queue Depth (Top-Center)](#panel-2-queue-depth-top-center)
    - [Panel 3: Circuit Breaker State (Top-Right)](#panel-3-circuit-breaker-state-top-right)
    - [Panel 4: P99 Latency (Center-Left)](#panel-4-p99-latency-center-left)
    - [Panel 5: Error Rate (Center-Right)](#panel-5-error-rate-center-right)
    - [Panel 6: Processing Rate (Bottom-Left)](#panel-6-processing-rate-bottom-left)
    - [Panel 7: Worker Utilization (Bottom-Center)](#panel-7-worker-utilization-bottom-center)
    - [Panel 8: Throughput Trend (Bottom-Right)](#panel-8-throughput-trend-bottom-right)
  - [Dashboard 2: Jidoka (Circuit Breaker)](#dashboard-2-jidoka-circuit-breaker)
    - [Key Panels](#key-panels)
  - [Dashboard 3: Kanban (Queue Management)](#dashboard-3-kanban-queue-management)
    - [Key Panels](#key-panels-1)
  - [Dashboard 4: Andon (Alerts)](#dashboard-4-andon-alerts)
    - [Design](#design)
    - [Panels](#panels)
  - [Dashboard 5: Kaizen (Performance Analysis)](#dashboard-5-kaizen-performance-analysis)
    - [Key Panels](#key-panels-2)
  - [Dashboard 6: Heijunka (Load Balancing)](#dashboard-6-heijunka-load-balancing)
    - [Key Panels](#key-panels-3)
  - [Dashboard 7: Tracing (Distributed Tracing Metrics)](#dashboard-7-tracing-distributed-tracing-metrics)
    - [Key Panels](#key-panels-4)
  - [Alert Thresholds (Can be Tuned)](#alert-thresholds-can-be-tuned)
  - [Dashboard Access & Login](#dashboard-access--login)
  - [Common Dashboard Combinations](#common-dashboard-combinations)
  - [Dashboard Tips & Tricks](#dashboard-tips--tricks)
  - [Integration with Alerting](#integration-with-alerting)
  - [Troubleshooting Dashboard Issues](#troubleshooting-dashboard-issues)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Grafana Dashboard Guide

**Version**: 1.0
**Audience**: SRE/DevOps engineers
**Purpose**: Detailed guide to understanding each dashboard panel
**Format**: Panel-by-panel breakdown with interpretation guide

---

## Dashboard 1: TPS Overview (Main Dashboard)

**Access**: http://localhost:3000 > Dashboards > TPS > Overview

**Purpose**: Daily health check. 60-second scan tells you if system is healthy.

### Panel 1: System Status (Top-Left)

**Type**: Stat gauge
**Metric**: `tps_system_health_status` (0=unhealthy, 1=healthy)
**Interpretation**:
- GREEN (1.0) = All components healthy
- RED (0.0) = One or more components down

**What to do if RED**:
1. Click the panel to drill down
2. See which component is unhealthy
3. Check component logs: `kubectl logs -l component=<name> -n tps-system`

**Example**: If "circuit_breaker" component shows unhealthy, check downstream service health.

---

### Panel 2: Queue Depth (Top-Center)

**Type**: Time-series graph with threshold band
**Metric**: `tps_queue_current_depth`
**Max Threshold**: 100 (red line)
**Healthy Range**: 40-60 (yellow zone)

**Interpretation**:
- **Flat line at 0**: No incoming load, workers idle
- **Flat line at 50**: Balanced load, good state
- **Climbing line**: Incoming rate > processing rate
- **Hitting red line (100)**: Queue full, rejections happening

**What to do if climbing**:
1. Is it a spike (temporary) or sustained growth?
   - Spike: Wait and watch, probably OK
   - Sustained: Scale workers or reduce load

2. Check processing rate (see Panel 7)
3. If processing rate is 0, workers are stuck (see Decision Tree)

**Real example**:
```
Normal: Queue oscillates 30-70
Overload: Queue climbs to 95 in 5 minutes
Recovery: After scaling workers, queue drops to 50 in 10 minutes
```

---

### Panel 3: Circuit Breaker State (Top-Right)

**Type**: Gauge (red/green)
**Metric**: `tps_circuit_breaker_state` (0=Closed, 1=Open)

**Interpretation**:
- **GREEN (0)**: Circuit Closed, accepting requests, downstream healthy
- **RED (1)**: Circuit Open, rejecting requests, downstream broken

**Why this matters**:
- When Open, all signals get 503 (rejected)
- Prevents cascading failure to downstream
- Automatically recovers in 30 seconds

**What to do if RED**:
1. Circuit will auto-recover in ~30 seconds
2. If it doesn't recover:
   - Check downstream service: `curl http://downstream/health`
   - Restart downstream: `kubectl rollout restart deployment/downstream-service -n production`
   - Check downstream logs for errors

---

### Panel 4: P99 Latency (Center-Left)

**Type**: Time-series graph with SLA threshold
**Metric**: `histogram_quantile(0.99, tps_signal_latency_bucket)`
**SLA Threshold**: 200ms (red line)

**Interpretation**:
- Shows worst-case latency (99th percentile)
- **< 100ms**: Excellent
- **100-200ms**: Good
- **200-400ms**: Degraded (approaching SLA breach)
- **> 400ms**: Critical

**Why P99 spikes**:
1. **Queue wait**: Workers busy, signal waits in queue
   - Fix: Scale workers
   - Check: Queue depth panel (Panel 2)

2. **Task slow**: ProcessWork taking long
   - Fix: Optimize task or scale resources
   - Check: Jaeger traces for slow spans

3. **Downstream slow**: Jidoka check taking long
   - Fix: Scale downstream or reduce calls
   - Check: Downstream service metrics

**Real example**:
```
Baseline: P99 = 45ms (good)
Load spike: P99 = 280ms (queue fills, workers busy)
After scaling: P99 = 50ms (recovered)
```

---

### Panel 5: Error Rate (Center-Right)

**Type**: Gauge showing percentage
**Metric**: `100 * rate(tps_signal_errors[5m]) / rate(tps_signal_total[5m])`

**Interpretation**:
- **0-1%**: Normal (acceptable)
- **1-5%**: Warning (investigate)
- **> 5%**: Critical (immediate action needed)

**Error sources**:
1. **Circuit breaker rejections** (503)
   - Check: Circuit Breaker panel (Panel 3)
   - If circuit is Open, errors are expected

2. **Bad input** (400 errors)
   - Check: Application logs
   - Issue: User sending invalid data

3. **Timeout** (504 errors)
   - Check: Downstream latency
   - Issue: Task or downstream taking too long

4. **Internal errors** (500 errors)
   - Check: Application panic logs
   - Issue: Bug in system

**What to do if > 5%**:
1. Check which error type: `kubectl logs -l app=tps-system | grep error_type`
2. Cross-reference with other panels:
   - Circuit Breaker: if Open, rejections are expected
   - Queue Depth: if full, timeout errors expected
   - P99 Latency: if spiked, timeouts likely

---

### Panel 6: Processing Rate (Bottom-Left)

**Type**: Gauge showing signals/second
**Metric**: `rate(tps_signal_completed[1m])`

**Interpretation**:
- Expected: 5-20 signals/sec (depends on task complexity)
- **Flat line at 0**: Workers stopped, critical issue
- **Dropping**: Workers being scaled down or restarting
- **Increasing**: More workers coming online

**Correlation**:
- When queue grows AND processing rate drops → workers slow or stuck
- When queue grows AND processing rate constant → too much incoming load
- When queue shrinks AND processing rate constant → load reduced

**What to do if = 0**:
1. Critical: No signals are completing
2. Check: Are worker pods running?
   - `kubectl get pods -l app=tps-system -n tps-system`
3. If pods exist but processing = 0:
   - Check: `kubectl logs -l app=tps-system -n tps-system | tail -50`
   - Likely: Panic, deadlock, or hung thread
   - Action: `kubectl rollout restart deployment/tps-system -n tps-system`

---

### Panel 7: Worker Utilization (Bottom-Center)

**Type**: Gauge showing percentage
**Metric**: `100 * rate(tps_worker_busy_time[1m]) / rate(tps_worker_total_time[1m])`

**Interpretation**:
- **0-40%**: Underutilized (spare capacity, could reduce workers)
- **40-80%**: Healthy (good utilization, some headroom)
- **80-100%**: Overutilized (at capacity, risk of overload)

**Correlation with queue depth**:
- High utilization + high queue depth = add workers
- High utilization + low queue depth = tasks are slow (optimize)
- Low utilization + low queue depth = reduce workers (cost optimization)

**Real example**:
```
Normal: Worker util = 60%, queue = 50%
Overload: Worker util = 98%, queue = 85%
Solution: Scale workers from 2 to 6
Result: Worker util = 35%, queue = 45% (re-balanced)
```

---

### Panel 8: Throughput Trend (Bottom-Right)

**Type**: Time-series showing signals/sec over 24 hours
**Metric**: `rate(tps_signal_total[5m])`

**Interpretation**:
Shows traffic pattern (not processing rate, but incoming rate)

**Patterns**:
- **Smooth curve**: Well-behaved incoming load
- **Spiky curve**: Bursty incoming load
- **Sudden drop**: Load generator stopped or failed
- **Sudden climb**: Cache miss or load increase

**Use case**: Helps with capacity planning
- Peak: 30 signals/sec
- Off-peak: 2 signals/sec
- Average: 12 signals/sec

---

## Dashboard 2: Jidoka (Circuit Breaker)

**Access**: http://localhost:3000 > Dashboards > TPS > Jidoka

**Purpose**: Monitor fault isolation and recovery behavior

### Key Panels

**Panel 1: Circuit Breaker State**
- Current state (0=Closed, 1=Open)
- State change events (when did it flip?)
- Recovery time after opening

**Panel 2: Health Checks**
- Downstream health check status (pass/fail)
- Last check time
- Check latency (how long does health check take?)

**Panel 3: Rejection Count**
- Total signals rejected by circuit breaker
- Rate of rejections/second (during incident)
- Should be 0 in normal operation

**Panel 4: Recovery Time**
- How long from "circuit opens" to "circuit closes"
- Should be ~30 seconds (configurable)
- If > 60 seconds, downstream has lingering issues

**Interpretation Guide**:
```
Healthy state:
- Circuit state: Closed (0)
- Health checks: All passing
- Rejections: 0
- Recovery time: N/A (not in incident)

Incident state:
- Circuit state: Open (1)
- Health checks: Failing
- Rejections: climbing (1000+/sec)
- Recovery time: Waiting for fix

Recovery state:
- Circuit state: Closed (0)
- Health checks: Passing again
- Rejections: stopped
- Recovery time: <30 seconds elapsed
```

---

## Dashboard 3: Kanban (Queue Management)

**Access**: http://localhost:3000 > Dashboards > TPS > Kanban

**Purpose**: Monitor queue health and work-in-progress

### Key Panels

**Panel 1: Queue Depth Over Time**
- Current vs. max capacity
- Visual indication: green (healthy), yellow (warning), red (critical)

**Panel 2: Enqueue Rate vs Dequeue Rate**
- Two lines: incoming rate and processing rate
- When enqueue > dequeue: queue grows
- When dequeue > enqueue: queue shrinks
- Should be roughly equal in steady state

**Panel 3: Queue Full Events**
- Count of times queue reached capacity
- When this happens, new signals are rejected (Jidoka)
- Should be 0 or very low

**Panel 4: Imbalance Factor**
- How evenly work is distributed across workers
- 0% = perfectly balanced
- > 20% = imbalanced (some workers overloaded)
- Heijunka automatically rebalances

---

## Dashboard 4: Andon (Alerts)

**Access**: http://localhost:3000 > Dashboards > TPS > Andon

**Purpose**: See problems at a glance (visual signal system)

### Design

Dashboard is divided into 3 color zones:
- **RED**: Problems detected (firing alerts)
- **YELLOW**: Warnings (metrics approaching threshold)
- **GREEN**: Healthy (all metrics OK)

### Panels

**Panel 1: Active Alerts**
- List of currently firing alerts
- Alert name, severity, time firing
- Click to see full details and runbook

**Example alerts**:
```
- HighQueueDepth (FIRING 2m 30s)
  Queue depth > 80% for 2+ minutes
  Action: Check processing rate, may need scaling

- HighLatency (FIRING 5m 12s)
  P99 latency > 200ms for 5+ minutes
  Action: Check Jaeger for bottleneck

- CircuitBreakerOpen (FIRING 30s)
  Circuit breaker is open
  Action: Check downstream service health
```

**Panel 2: Recent Alert Transitions**
- When alerts fired/cleared over last 24 hours
- Helps you see patterns (e.g., "every Monday 9 AM")

**Panel 3: Alert Histogram**
- Count of how many times each alert fired
- Helps identify chronic vs. one-time issues

---

## Dashboard 5: Kaizen (Performance Analysis)

**Access**: http://localhost:3000 > Dashboards > TPS > Kaizen

**Purpose**: Deep-dive performance metrics for optimization

### Key Panels

**Panel 1: Latency Percentiles**
- P0 (min), P25, P50 (median), P75, P90, P95, P99 (max)
- Shows full distribution, not just single point

**Interpretation**:
```
Healthy distribution:
- P50 = 25ms (half of signals < 25ms)
- P90 = 45ms (90% < 45ms)
- P99 = 85ms (99% < 85ms)
- Max = 120ms (longest signal)

Skewed distribution (possible bottleneck):
- P50 = 20ms (median OK)
- P90 = 80ms (big jump)
- P99 = 250ms (tail is very long)
- Indicates: some signals get stuck (queue wait or slow task)
```

**Panel 2: Request Duration Breakdown**
- Components of total latency:
  - ReceiveSignal
  - Enqueue
  - QueueWait
  - ProcessWork
  - Check
  - RecordMetric
  - UpdateLoad

**Use**: Identify which component takes longest
- If QueueWait is largest: add workers
- If ProcessWork is largest: optimize task

**Panel 3: Throughput vs. Latency Trade-off**
- As throughput increases, latency increases
- Shows elasticity of system
- Helps with capacity planning

**Panel 4: Percentile Trends**
- P50, P90, P99 over 24 hours
- Helps detect degradation patterns
- If trending up, investigate cause

---

## Dashboard 6: Heijunka (Load Balancing)

**Access**: http://localhost:3000 > Dashboards > TPS > Heijunka

**Purpose**: Monitor work distribution across workers

### Key Panels

**Panel 1: Worker Load Distribution**
- Bar chart showing items in queue per worker
- All bars should be roughly equal height
- Unequal heights = imbalance

**Panel 2: Imbalance Factor Over Time**
- Percentage deviation from mean
- Should oscillate around 5-10%
- Spikes indicate new signal burst

**Panel 3: Rebalancing Events**
- Count of times Heijunka rebalanced work
- Should happen periodically (every ~30 seconds)

**Panel 4: Work Steal Operations**
- When rebalancing, how many items moved from one worker to another
- Helps tune rebalancing algorithm

---

## Dashboard 7: Tracing (Distributed Tracing Metrics)

**Access**: http://localhost:3000 > Dashboards > TPS > Tracing

**Purpose**: Monitor distributed tracing system itself

### Key Panels

**Panel 1: Traces Per Minute**
- How many traces are being recorded
- Should match signal rate (each signal = 1 trace)

**Panel 2: Trace Completion Rate**
- % of traces that complete successfully
- Should be 100%
- If < 100%, some traces are partial (investigate)

**Panel 3: Span Distribution**
- How many spans per trace (on average)
- Should be ~7-10 spans per signal trace

**Panel 4: Tracing Latency**
- How long does it take to record and export traces
- Should be < 100ms (not affecting main system)

---

## Alert Thresholds (Can be Tuned)

| Alert | Threshold | Duration |
|-------|-----------|----------|
| HighQueueDepth | Queue > 80% | 2 minutes |
| HighLatency | P99 > 200ms | 5 minutes |
| HighErrorRate | Error rate > 5% | 2 minutes |
| CircuitBreakerOpen | State = 1 | 30 seconds |
| HighWorkerUtilization | Utilization > 90% | 5 minutes |
| ProcessingRateZero | Rate = 0 | 1 minute |

---

## Dashboard Access & Login

**Default Credentials**:
- URL: http://localhost:3000
- Username: admin
- Password: admin (change in production!)

**Change Password**:
1. Click user icon (top-right)
2. Select "Change Password"
3. Enter new password
4. Save

---

## Common Dashboard Combinations

**Quick Health Check (1 minute)**:
1. Look at TPS Overview
2. Check: System Status (green?), Circuit Breaker (closed?), Error Rate (low?)
3. If all green, system is healthy

**Deep Investigation (5 minutes)**:
1. Start at TPS Overview
2. Identify problem area (queue, latency, error rate)
3. Go to specific dashboard (Kanban, Kaizen, Jidoka, etc.)
4. Understand root cause
5. Open Jaeger for detailed traces

**Capacity Planning (15 minutes)**:
1. Look at Kaizen dashboard (24-hour view)
2. Check peak throughput and latency
3. Check Heijunka for load distribution
4. Decide on target SLA and required worker count

---

## Dashboard Tips & Tricks

**Tip 1: Time Range Selection**
- Default: Last 6 hours
- Use "Last hour" for recent incidents
- Use "Last 24 hours" for trends
- Use "Last 7 days" for week-over-week comparison

**Tip 2: Pause Auto-Refresh**
- Top-right corner has refresh interval
- Set to "Off" if you're comparing two snapshots
- Set to "5s" for real-time monitoring

**Tip 3: Drill Down**
- Click any panel to see larger view
- Click metric name to see full query
- Click time point to filter to that time

**Tip 4: Create Custom Dashboards**
- "+" button > Dashboard
- Add panels with custom queries
- Save with team name (e.g., "Team A - Custom Monitoring")

---

## Integration with Alerting

Dashboards feed into alert system:
1. Panel shows metric
2. Metric crosses threshold
3. Alert fires
4. Notification sent (Slack, PagerDuty, email)
5. Runbook link provided to operator

**Example flow**:
```
Queue depth spikes to 85%
→ HighQueueDepth alert fires
→ Slack message: "Queue depth exceeding threshold"
→ Operator clicks runbook link
→ Runbook says: "Check processing rate and scale workers if needed"
→ Operator scales workers
→ Queue drains
→ Alert clears
```

---

## Troubleshooting Dashboard Issues

**Problem: Dashboard shows "No Data"**
- Solution: Check if Prometheus is collecting metrics
- Go to http://localhost:9090/targets
- If target is "Down", check Prometheus logs

**Problem: Panels show old data**
- Solution: Refresh browser (Cmd+R or Ctrl+R)
- Or click "Refresh" button in dashboard

**Problem: Alerts not firing**
- Solution: Check if alert rules are configured
- In Grafana, go to Alerting > Alert Rules
- Verify rule expression is correct

---

## Next Steps

1. **Real-time monitoring**: Keep TPS Overview dashboard open in your browser
2. **Learn by doing**: Run load tests and watch dashboards respond
3. **Customize**: Create team-specific dashboard with relevant panels
4. **Integrate**: Set up alerts and notifications to your Slack/PagerDuty
5. **Tune**: Adjust alert thresholds based on your SLA

---

**Version**: 1.0
**Last Updated**: January 2026
**Tested On**: Grafana 8.0+ with Prometheus 2.25+
