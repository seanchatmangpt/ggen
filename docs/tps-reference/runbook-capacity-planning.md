# Capacity Planning Runbook for TPS Systems

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: Platform engineers, ops leads
**Frequency**: Review quarterly, plan annually

---

## Capacity Planning Process

### Step 1: Gather Historical Data

**Metrics to Collect** (minimum last 3-6 months):
- Daily request volume (peak and average)
- Daily worker scaling events
- Daily resource usage (CPU, memory, disk)
- Error rates and anomalies
- Deployment schedule and changes

**Where to Get Data**:
- Prometheus: Metrics retention (usually 15 days)
- CloudWatch/GCP Monitoring: Longer retention (usually 1 year)
- Billing system: Resource usage and costs
- Git log: Deployments and changes

**Data Collection Spreadsheet**:
```
Date      | Avg RPS | Peak RPS | Workers | CPU % | Memory % | Errors | Notes
----------|---------|----------|---------|-------|----------|--------|--------
Jan 1     | 100     | 150      | 10      | 45    | 60       | 0.05%  | Quiet day
Jan 2     | 120     | 200      | 12      | 55    | 65       | 0.08%  | Slightly busy
Jan 3     | 150     | 280      | 15      | 65    | 70       | 0.15%  | Marketing event
...
```

---

### Step 2: Identify Growth Trends

**Calculate Growth Rate**:

```
Current Load:     100 req/sec
Load 3 months ago: 60 req/sec
Growth:           (100 - 60) / 60 = 66% in 3 months
Monthly growth:   ~18% per month
```

**Adjust for Seasonality**:
- Summer: Might be 20% higher
- Holidays: Might be 50% higher
- Post-launch: Might be 100%+ higher
- Recession: Might be 10% lower

**Conservative Forecast**:
- Use highest monthly growth from last 3 months
- Plan for 150% of normal peak (not 100%)
- Don't assume linear growth (might accelerate or decelerate)

---

### Step 3: Project Future Demand

**Projection Spreadsheet**:
```
Period     | Avg Load | Peak Load | Growth Rate | Notes
-----------|----------|-----------|-------------|---------------
Today      | 100      | 150       | -           | Current
+3 months  | 118      | 177       | 18% MoM     | Q1 forecast
+6 months  | 139      | 209       | 18% MoM     | Q2 forecast
+12 months | 196      | 294       | 18% MoM     | Q3 forecast
+24 months | 385      | 578       | 18% MoM     | Q4 forecast
```

**Projection Formula**:
```
Future Load = Current Load × (1 + Growth Rate) ^ Months
```

---

### Step 4: Right-Size Infrastructure

**For Each Forecast Point**, calculate resources needed:

#### Workers Calculation

```
Target worker utilization: 70% (so system can handle 30% spikes)
Workers needed = Peak Load / (Throughput per Worker × Target Utilization)

Example:
- Peak Load = 294 req/sec (12 months from now)
- Throughput per Worker = 10 req/sec
- Target Utilization = 0.70
- Workers = 294 / (10 × 0.70) = 42 workers needed
```

#### Queue Buffer Calculation

```
Queue should be sized to absorb burst load
Queue Max = Peak Load × Acceptable Queue Time

Example:
- Peak Load = 294 req/sec
- Acceptable Queue Time = 2 seconds (don't wait more than 2s)
- Queue Max = 294 × 2 = 588 items
```

#### Memory Calculation

```
Memory per Worker = [Your system specific, measure it]
Total Memory = Workers × Memory per Worker + Fixed Memory (queue, cache)

Example:
- Workers = 42
- Memory per Worker = 512 MB
- Queue Memory = 2 GB (your queue backend)
- Cache Memory = 1 GB
- Total = 42 × 512 MB + 2 GB + 1 GB = 24 GB total
```

#### CPU Calculation

```
CPU Cores = Workers × CPU per Worker (threads, concurrency)
Most systems: 1-4 cores per worker depending on task type

Example:
- Workers = 42
- CPU per Worker = 2 cores
- Total CPU Cores = 42 × 2 = 84 cores
```

#### Disk Calculation

```
Disk needed for:
- Queue persistence (if using disk-based queue)
- Logs (if storing locally)
- Cache (if using local cache)

Example:
- Queue persistence: 10 GB
- Logs (7 days): 5 GB
- Local cache: 2 GB
- Total: 17 GB (add 20% buffer) = 20 GB needed
```

---

### Step 5: Cost Modeling

**Determine Unit Costs** (in your hosting provider):

```
Pricing Breakdown:
- per Worker/Instance: $100/month
- per GB Memory (on top of instance): $5/month
- per 100 GB Disk: $10/month
- per TB network egress: $100/month
- Fixed overhead (load balancer, monitoring, etc.): $500/month
```

**Calculate Total Cost for Each Forecast**:

```
Period      | Workers | Memory | Disk  | Network | Fixed  | Total
------------|---------|--------|-------|---------|--------|--------
Today       | 15 × $100 = $1,500 | $500 | $100 | $200 | $500 = $2,800/mo
+3 months   | 18 × $100 = $1,800 | $600 | $100 | $250 | $500 = $3,250/mo
+6 months   | 21 × $100 = $2,100 | $700 | $100 | $300 | $500 = $3,700/mo
+12 months  | 42 × $100 = $4,200 | $1,400 | $150 | $400 | $500 = $6,650/mo
```

**Budget Planning**:
- Current cost: $2,800/month = $33,600/year
- 12-month projection: $6,650/month = $79,800/year
- Additional cost: $46,200/year (138% increase over current)
- Budget approved? Get approval from finance before scaling

---

### Step 6: Scaling Strategy

**Option A: Scale On-Demand (Reactive)**

Approach: Wait until you hit capacity, then scale
- Advantage: Cheapest (don't over-provision)
- Disadvantage: Risk of hitting capacity limits during peak
- When to use: Non-critical systems, good headroom

```
Scaling Trigger: When utilization hits 80%
Scaling Action: Add 50% more capacity
Frequency: As needed (might be multiple times/month)
```

---

**Option B: Scale Proactively (Planned)**

Approach: Scale on a schedule based on forecasts
- Advantage: Never risk hitting capacity during peak
- Disadvantage: Might over-provision slightly
- When to use: Critical systems, unpredictable load

```
Scaling Timeline:
- Q1 (Jan-Mar): Current capacity (15 workers)
- Q2 (Apr-Jun): Add 3 workers → 18 workers
- Q3 (Jul-Sep): Add 3 workers → 21 workers
- Q4 (Oct-Dec): Add 21 workers → 42 workers (for holiday peak)
```

---

**Option C: Scale Gradually (Balanced)**

Approach: Mix of reactive and planned scaling
- Advantage: Balance cost and risk
- Disadvantage: Requires monitoring
- When to use: Most systems (recommended)

```
Plan: Scale 25% quarterly + react when needed
- Q1: 15 workers (current)
- Q2: 19 workers (+4 planned + any reactive)
- Q3: 24 workers (+5 planned + any reactive)
- Q4: 30 workers (+6 planned + react for holiday peak)
```

---

### Step 7: Capacity Monitoring

**Daily Checks** (during business hours):
- Current utilization: Are we at capacity?
- Utilization trend: Growing or shrinking?
- Performance: Meeting SLOs?
- Cost: On track with budget?

**Weekly Reviews**:
- Scaling events: Did autoscaling trigger?
- Bottlenecks: Any resource hitting limits?
- Anomalies: Unusual load patterns?

**Monthly Forecast Update**:
- Actual vs forecast: Were we right?
- Adjust forecast: Update based on new data
- Adjust timeline: Move scaling dates if needed

---

## Right-Sizing: Current vs Optimal

### Identifying Over-Provisioning

**Signs**:
- Worker utilization < 40%
- Queue depth < 20% of max
- Memory usage < 50%
- CPU < 40%

**Solution**:
- Reduce worker count by 20%
- Monitor for impact
- Continue reducing until utilization hits 60-70%

**Savings**:
```
Current: 20 workers × $100/month = $2,000/month
Optimal: 12 workers × $100/month = $1,200/month
Savings: $800/month = $9,600/year
```

---

### Identifying Under-Provisioning

**Signs**:
- Worker utilization > 85%
- Queue depth > 70% of max
- Memory usage > 85%
- CPU > 85%
- Latency p99 increasing
- Error rate increasing

**Solution**:
- Increase worker count by 30%
- Monitor for improvement
- Continue increasing until metrics improve

**Cost**:
```
Current: 12 workers × $100/month = $1,200/month
Adequate: 16 workers × $100/month = $1,600/month
Cost: $400/month = $4,800/year
```

---

## Scaling Decision Tree

```
Are we approaching capacity?

┌─ Yes: Utilization > 75%
│  ├─ Growth rate high (> 15% MoM)?
│  │  └─ Scale proactively (+30% capacity immediately)
│  └─ Growth rate normal (5-15% MoM)?
│     └─ Scale on-demand (wait for 85% utilization)
│
└─ No: Utilization < 60%
   ├─ Consistent low utilization (> 1 month)?
   │  └─ Reduce capacity (-20% to save cost)
   └─ Temporarily low (spike over)?
      └─ Keep current capacity (might need again)
```

---

## Example: Full Capacity Plan

### Current State (January 2026)

```
Load: 100 req/sec (avg), 150 req/sec (peak)
Workers: 15
Memory: 10 GB
CPU: 8 cores
Disk: 15 GB
Monthly Cost: $2,800
Utilization: 70% (good)
```

### Forecast (One Year)

```
Historical growth: 18% per month
Seasonality: +50% for Q4 (Nov-Dec holidays)
Planned capacity: Maintain 70-80% utilization
```

### Scaling Plan

```
Q1 (Jan-Mar):
  - Avg load: 118 req/sec
  - Peak load: 177 req/sec
  - Workers: 15 → 17 (add 2 in Feb)
  - Cost: $2,800 → $3,100/month

Q2 (Apr-Jun):
  - Avg load: 139 req/sec
  - Peak load: 209 req/sec
  - Workers: 17 → 20 (add 3 in May)
  - Cost: $3,100 → $3,500/month

Q3 (Jul-Sep):
  - Avg load: 164 req/sec
  - Peak load: 246 req/sec
  - Workers: 20 → 24 (add 4 in Aug)
  - Cost: $3,500 → $4,200/month

Q4 (Oct-Dec):
  - Avg load: 194 req/sec
  - Peak load: 441 req/sec (holidays 3x normal)
  - Workers: 24 → 42 (add 18 for holiday)
  - Cost: $4,200 → $6,800/month (temporary)
  - Jan: Scale back to 25 workers ($3,500/month)
```

### Budget Approval

```
2024 Cost: $33,600
2025 Cost: $79,800 (with holiday peak)
Increase: $46,200 (138%)
ROI: Increased capacity = increased revenue
Approval: CFO approved Jan 2025
```

### Monitoring Plan

```
Daily:
- Check worker utilization
- Verify no errors from over-provisioning
- Monitor cost

Weekly:
- Review scaling events
- Check if actual matches plan
- Adjust if needed

Monthly:
- Update forecast with new data
- Present to stakeholders
- Adjust plan if growth rate changes
```

---

## Optimization Strategies

### Strategy 1: More Efficient Workers

Instead of adding more workers, make workers faster:
- Optimize code (reduce latency per item)
- Add caching (avoid redundant work)
- Use faster algorithms
- Parallelize operations

**Impact**: Same capacity with fewer workers = lower cost

### Strategy 2: Queue Optimization

Use dead-letter queue strategy:
- Fast-path items (80%): Process normally
- Slow-path items (15%): Queue separately, process slower
- Failed items (5%): Route to DLQ, manual review

**Impact**: Better throughput, lower latency for fast items

### Strategy 3: Traffic Smoothing

Instead of handling load spikes, smooth the load:
- Rate limiting (don't accept requests faster than can process)
- Request prioritization (priority queue)
- User feedback (tell users to "try again later")

**Impact**: Prevent cascading failure, reduce peak load spikes

### Strategy 4: Regional Distribution

Distribute load across regions:
- Primary region: Handles 80% of traffic
- Secondary region: Handles 20% of traffic
- During peak: Overflow to secondary

**Impact**: Reduce cost per region, increase redundancy

---

## Cost Optimization Checklist

- [ ] Identified over-provisioned resources?
- [ ] Considered spot/preemptible instances for non-critical capacity?
- [ ] Have reserved instances for baseline capacity (cheaper than on-demand)?
- [ ] Implemented auto-scaling (scale down when not needed)?
- [ ] Optimized code to improve throughput/worker?
- [ ] Use CDN for static content (reduce data transfer)?
- [ ] Archive old logs to cold storage (reduce disk cost)?
- [ ] Considered managed services vs self-hosted (might be cheaper)?

---

**End of Capacity Planning Runbook**
