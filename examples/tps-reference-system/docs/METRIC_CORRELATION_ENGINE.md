# Metric Correlation Engine

Automated root cause analysis through metric correlation and dependency analysis.

## Overview

The Metric Correlation Engine detects when multiple metrics spike together, correlating symptoms with likely root causes. This accelerates incident response by providing immediate hypotheses about what's broken.

## Correlation Patterns

### Latency Spike Correlations

When API latency increases, we correlate with:

1. **CPU Usage** (Strong correlation = CPU-bound issue)
   - High CPU + High latency → Optimize algorithm or scale
   - Example: O(n²) algorithm in critical path

2. **Memory Usage** (Strong correlation = Memory pressure)
   - High memory + High latency → GC pauses or memory leak
   - Example: Cache unlimited growth

3. **Database Query Time** (Strong correlation = Database bottleneck)
   - High DB latency + High API latency → Query optimization or indexing
   - Example: N+1 query pattern

4. **Disk I/O** (Strong correlation = I/O bottleneck)
   - High disk I/O + High latency → Reduce disk access or add caching
   - Example: Logging to disk synchronously

5. **Lock Contention** (Strong correlation = Synchronization bottleneck)
   - High lock contention + High latency → Reduce lock scope or use lock-free data structures
   - Example: Global locks in concurrent system

6. **Network Latency** (Strong correlation = External dependency issue)
   - High network latency + High API latency → External API is slow
   - Example: Third-party API degradation

### Error Rate Correlations

When error rate increases, we correlate with:

1. **Recent Deployment**
   - Error spike timestamp matches deployment time → Rollback
   - Example: Code bug introduced in release

2. **Resource Exhaustion**
   - High error rate + High CPU/Memory → Scale resources
   - Example: Out of memory → segfault

3. **Service Dependency Failure**
   - Error spike in downstream service before upstream → Dependency is down
   - Example: Database connection failure propagates to API

4. **Configuration Change**
   - Error spike matches configuration deployment → Rollback config
   - Example: Wrong timeout value causes timeouts

## Correlation Rules

### Rule Format

```python
if metric_A_spike AND metric_B_spike:
    confidence = calculate_correlation_strength(metric_A, metric_B, time_window)
    if confidence > threshold:
        root_cause = find_root_cause(metric_A, metric_B)
        remediation = get_remediation_steps(root_cause)
```

### Example Rules

#### CPU-Bound Latency Issue

```python
Rule: LatencySpike + HighCPU → Algorithm Optimization or Horizontal Scaling
- Detect: P99 latency > baseline * 1.5
- Correlate: CPU usage > 80%
- Time alignment: < 2 minute drift
- Confidence formula: correlation_coefficient(latency, cpu_usage) > 0.8
- Remediation: "Profile CPU, identify hot path, scale or optimize"
```

#### Memory Leak Issue

```python
Rule: LatencySpike + MemoryGrowth + HighGCPause → Memory Leak
- Detect: Heap size growing over time (linear trend)
- Correlate: GC pause times increasing
- Time alignment: Memory growth precedes latency increase
- Confidence formula: memory_leak_probability() > 0.7
- Remediation: "Profile memory allocations, find leak, fix allocation pattern"
```

#### Dependency Failure

```python
Rule: ErrorRateSpike + DownstreamLatency → Dependency Failure
- Detect: Error rate > baseline * 2
- Correlate: Downstream service latency increased
- Time alignment: Downstream latency increase precedes error spike
- Confidence formula: depends_on_service(error_source, slow_service) > 0.9
- Remediation: "Check downstream service health, add circuit breaker, retry logic"
```

#### Database Bottleneck

```python
Rule: LatencySpike + DatabaseSlowdown → Query Optimization
- Detect: P99 latency > threshold
- Correlate: Database query time > 80% of API latency
- Time alignment: Database slow first, then API slow
- Confidence formula: correlation(db_time, api_latency) > 0.85
- Remediation: "Review slow query log, add indexes, optimize queries"
```

## Root Cause Decision Tree

```
Symptom: Latency Spike Detected
│
├─→ [Correlate with CPU]
│   ├─→ High CPU (>80%) + Latency
│   │   └─→ Root Cause: CPU-Bound Algorithm
│   │       Remediation: Profile, Optimize, or Scale
│   │
│   └─→ Normal CPU
│       └─→ [Check Memory]
│
├─→ [Correlate with Memory]
│   ├─→ Memory growing + GC pauses increasing
│   │   └─→ Root Cause: Memory Leak
│   │       Remediation: Find leak, fix allocation
│   │
│   └─→ Normal Memory
│       └─→ [Check Database]
│
├─→ [Correlate with Database]
│   ├─→ Database slow + Query time high
│   │   └─→ Root Cause: Query Bottleneck
│   │       Remediation: Optimize queries, add indexes
│   │
│   └─→ Database normal
│       └─→ [Check External Dependencies]
│
├─→ [Correlate with External Dependencies]
│   ├─→ External API slow
│   │   └─→ Root Cause: Dependency Issue
│   │       Remediation: Add circuit breaker, increase timeout
│   │
│   └─→ All dependencies normal
│       └─→ [Check for Lock Contention]
│
└─→ [Check Lock Contention]
    └─→ High lock waits
        └─→ Root Cause: Lock Contention
            Remediation: Reduce scope, use lock-free structures
```

## Metric Collection

### Required Metrics

For effective correlation, we need:

1. **Application Metrics**
   - Request latency (histogram)
   - Error rate
   - Throughput
   - Processing time by component

2. **Resource Metrics**
   - CPU usage (%)
   - Memory usage (bytes, %)
   - Disk I/O (read/write rate)
   - Network I/O (bytes/sec)

3. **System Metrics**
   - GC pause times (if applicable)
   - Lock contention stats
   - Thread/goroutine counts
   - File descriptor count

4. **Dependency Metrics**
   - Database query latency
   - External API response time
   - Cache hit rate
   - Queue depth

5. **Infrastructure Metrics**
   - Deployment events (timestamp, service, version)
   - Configuration changes (timestamp, what changed)
   - Auto-scaling events
   - Service restart events

### Metric Storage

All metrics stored in Prometheus with labels:

```
http_request_duration_seconds{service="api", endpoint="/users", method="GET", status="200"}
gauge_process_cpu_usage_percent{service="api", instance="pod-123"}
gauge_process_heap_size_bytes{service="api", instance="pod-123"}
gauge_database_query_time_ms{service="api", query_type="select"}
```

## Correlation Algorithms

### Pearson Correlation Coefficient

Measures linear correlation between two metrics:

```
correlation = covariance(X, Y) / (stddev(X) * stddev(Y))
range: -1.0 (inverse) to 1.0 (perfect positive)
threshold: > 0.7 for strong correlation
```

### Time-Aligned Correlation

Accounts for lag between cause and effect:

```
for lag in [0s, 30s, 60s, 120s]:
    correlation = compute_correlation(metric_A, lag(metric_B, lag))
    if correlation > threshold:
        return (metric_A causes metric_B with lag)
```

### Multi-Metric Clustering

Groups correlated metrics together:

```
cluster = {metric_A, metric_B, metric_C} if all pairs have correlation > 0.7
root_cause = metric that spiked first (highest confidence)
```

## Configuration

### alerting/correlation-engine.yml

```yaml
correlation_engine:
  enabled: true
  update_interval: 30s
  lookback_window: 15m

  # Correlation thresholds
  strong_correlation_threshold: 0.8
  moderate_correlation_threshold: 0.6

  # Time alignment settings
  max_lag_seconds: 120
  lag_check_interval_seconds: 30

  # Dashboard update
  dashboard_update_interval: 60s

  # Logging
  log_correlations: true
  log_threshold: 0.6  # Only log if confidence > 60%
```

## Dashboard Integration

### Metric Correlation Dashboard

Shows top correlated metric pairs when alerts fire:

1. **Top Correlations**
   - Metric pairs with highest correlation strength
   - Time lag between spike and effect
   - Confidence score

2. **Root Cause Likelihood Score**
   - Ranked list of suspected root causes
   - Confidence % for each
   - Recommended remediation steps

3. **Cascade Impact**
   - Services impacted by primary failure
   - Dependency chain visualization
   - Impact severity estimation

4. **Historical Patterns**
   - Common correlation patterns (learned from past incidents)
   - Recurring issues (memory leak patterns, etc.)
   - Seasonal trends (traffic spikes)

## Alert Integration

When alert fires, correlation engine automatically:

1. Collects metric data for past 15 minutes
2. Computes correlations with all relevant metrics
3. Identifies top root cause candidates
4. Adds to alert details:
   - "Most likely cause: CPU bottleneck (80% confidence)"
   - "Recommended actions: Profile CPU hot path"
5. Updates Metric Correlation dashboard
6. Includes in runbook suggestions

## Examples

### Example 1: Latency Spike → CPU Bottleneck

```
Time: 2026-01-25 10:15:30
Event: P95 latency increased from 200ms to 400ms

Correlation Analysis:
  CPU usage: 42% → 88% (+46%, aligned at +2s)
  Memory: 65% → 68% (no correlation)
  DB time: 20ms → 22ms (no correlation)
  Lock waits: 0.1ms → 0.15ms (no correlation)

Result: Strong correlation with CPU (0.92)
Root Cause: CPU-bound algorithm or insufficient resources
Confidence: 92%
Recommended Action: Profile CPU, identify hot path, scale or optimize
```

### Example 2: Error Spike → Deployment

```
Time: 2026-01-25 11:30:00
Event: Error rate increased from 0.1% to 5%

Correlation Analysis:
  CPU: normal
  Memory: normal
  Recent deployment: v2.3.1 deployed at 11:29:45 (15s ago)
  Database: normal
  Downstream latency: normal

Result: Error spike timestamp matches deployment
Root Cause: Buggy code change in v2.3.1
Confidence: 95%
Recommended Action: Rollback to v2.3.0, investigate commit diff
```

### Example 3: Latency → Memory Leak

```
Time: 2026-01-25 14:00:00
Event: P99 latency increased from 500ms to 2500ms

Correlation Analysis:
  Heap size: 500MB → 850MB (growing trend)
  GC pause times: 20ms → 150ms (strong increase)
  CPU: 45% → 52% (slight increase)
  Memory allocation rate: 10MB/s → 15MB/s (increasing)

Result: Strong correlation with memory metrics (0.87)
Root Cause: Memory leak causing frequent GC pauses
Confidence: 87%
Recommended Action: Profile memory allocations, identify leak source
Time to resolution: 1-2 hours
```

## Machine Learning (Future)

Future enhancements:

1. **Pattern Recognition**
   - Learn common issue patterns from historical data
   - Improve confidence scoring over time
   - Suggest preventive actions

2. **Anomaly Detection**
   - Unsupervised learning for unexpected patterns
   - Zero-shot anomaly detection
   - Predict incidents before they happen

3. **Causal Inference**
   - Not just correlation but causation
   - DAG (Directed Acyclic Graph) of metric dependencies
   - True root cause identification

## Limitations

1. **Correlation ≠ Causation**
   - Multiple metrics might spike together but one might not cause other
   - Always verify root cause in code/logs

2. **Lag Detection**
   - Metrics might not align perfectly (0-2 minute drift)
   - Requires buffering and lag analysis

3. **Confounding Variables**
   - Third variable might cause both spikes
   - Need to account for load, time of day, etc.

## Validation

Validate correlation accuracy:

1. **Historical Analysis**
   - Apply correlation rules to past incidents
   - Measure accuracy of root cause identification

2. **User Feedback**
   - Ask on-call engineers if suggestions were helpful
   - Track incident resolution time improvement

3. **A/B Testing**
   - Compare with/without suggestions
   - Measure MTTR (Mean Time To Resolution)
