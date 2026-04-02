# Loyalty Analytics Guide

## Overview

Comprehensive analytics for measuring loyalty program performance and member engagement.

## Key Metrics

### Program Health Metrics

```python
from loyalty_analytics import LoyaltyAnalytics

analytics = LoyaltyAnalytics(members)
metrics = analytics.calculate_program_metrics()

# Metrics:
# - total_members: Total enrolled members
# - active_members: Members active in last 90 days
# - total_points_issued: All points earned
# - total_points_redeemed: All points spent
# - redemption_rate: Points redeemed / points issued
# - average_points_per_member: Mean points balance
# - tier_distribution: Members per tier
```

### Engagement Score

Calculate member engagement:

```python
engagement_score = (
    lifetime_points / 100 +
    transaction_count * 10 +
    referral_count * 50 +
    badge_count * 25 +
    streak_count * 5
)
```

## Member Segmentation

### RFM Analysis

Segment by Recency, Frequency, Monetary value:

```python
segments = analytics.segment_members_by_engagement()

# Segments:
# - Champions: High engagement, high value
# - Loyal Customers: Regular activity, good value
# - Potential Loyalists: Growing engagement
# - New Customers: Recent enrollees
# - At Risk: Low engagement, churn risk
```

### Segment Characteristics

| Segment            | Engagement | Lifetime Points | Behavior                |
|--------------------|------------|-----------------|-------------------------|
| Champions          | 1000+      | 10,000+         | Frequent, high spend    |
| Loyal Customers    | 500-999    | 5,000-9,999     | Regular, moderate spend |
| Potential Loyalists| 200-499    | 1,000-4,999     | Growing activity        |
| New Customers      | 50-199     | 0-999           | Just enrolled           |
| At Risk            | <50        | Any             | Inactive, churn risk    |

## Tier Analytics

### Tier Distribution

```python
tier_stats = analytics.analyze_tier_progression()

# Returns:
# - memberCount: Members in each tier
# - avgPoints: Average points per tier
# - medianPoints: Median points per tier
# - avgLifetimePoints: Average lifetime points
```

### Tier Migration

Track tier upgrades and downgrades:

```python
# Query tier changes
SELECT ?member ?oldTier ?newTier ?changeDate
WHERE {
    ?member lr:hasTierHistory ?history .
    ?history lr:hasOldTier ?oldTier ;
             lr:hasNewTier ?newTier ;
             lr:hasChangeDate ?changeDate .
}
ORDER BY DESC(?changeDate)
```

## Redemption Analytics

### Redemption Patterns

```python
patterns = analytics.calculate_redemption_patterns(transactions)

# Returns per reward type:
# - redemptionCount: Number of redemptions
# - totalPointsRedeemed: Total points spent
# - avgPointsPerRedemption: Average redemption size
```

### Popular Rewards

```python
# Most redeemed rewards
SELECT ?reward ?rewardName (COUNT(?redemption) AS ?count)
WHERE {
    ?redemption lr:hasReward ?reward .
    ?reward lr:hasRewardName ?rewardName .
}
GROUP BY ?reward ?rewardName
ORDER BY DESC(?count)
LIMIT 10
```

## Churn Analysis

### Identify At-Risk Members

```python
at_risk = analytics.identify_churn_risk(inactive_days=90)

# Returns:
# - memberId: Member identifier
# - tier: Current tier
# - pointsBalance: Available points
# - daysSinceActivity: Days inactive
```

### Churn Prediction

```python
def predict_churn(member):
    """Predict churn probability"""
    score = 0

    # Inactivity
    if member.days_since_activity > 90:
        score += 40
    elif member.days_since_activity > 60:
        score += 20

    # Low engagement
    if member.lifetime_points < 500:
        score += 20

    # No redemptions
    if member.redemption_count == 0:
        score += 20

    # Low tier
    if member.tier == Tier.Bronze:
        score += 20

    return score  # 0-100, higher = more likely to churn
```

## ROI Calculation

### Tier-Level ROI

```python
tier_roi = analytics.calculate_tier_roi(revenue_data)

# Returns per tier:
# - totalRevenue: Revenue from tier
# - avgRevenue: Average revenue per member
# - memberCount: Members in tier
# - pointsCost: Cost of points issued
# - roi: (revenue - cost) / cost
```

### Program ROI

```python
# Calculate overall program ROI
total_revenue = sum(tier_roi.totalRevenue)
total_cost = sum(tier_roi.pointsCost)
program_roi = (total_revenue - total_cost) / total_cost

# Example: 350% ROI
# $1M in costs → $4.5M in revenue
```

## Referral Analytics

### Referral Performance

```python
referral_stats = analytics.analyze_referral_effectiveness(referrals)

# Returns per referrer:
# - successfulReferrals: Number of referrals
# - totalPointsEarned: Points from referrals
```

### Viral Coefficient

```python
# Measure program virality
viral_coefficient = (
    total_referrals /
    total_active_members
)

# > 1.0 = viral growth
# 0.5-1.0 = strong referral program
# < 0.5 = low virality
```

## Cohort Analysis

### Enrollment Cohorts

Track retention by enrollment month:

```python
cohorts = analytics.analyze_cohorts()

# Month 0: 100% (enrollment)
# Month 1: 85% active
# Month 3: 70% active
# Month 6: 60% active
# Month 12: 50% active
```

### Cohort Metrics

```python
for cohort in cohorts:
    print(f"Cohort {cohort.month}:")
    print(f"  Retention: {cohort.retention_rate}%")
    print(f"  Avg Points: {cohort.avg_points}")
    print(f"  Avg Revenue: ${cohort.avg_revenue}")
```

## Revenue Attribution

### Points-Driven Revenue

Calculate revenue from loyalty members:

```python
# Revenue from loyalty members
loyalty_revenue = df[df.is_member == True]['revenue'].sum()

# Revenue from non-members
non_member_revenue = df[df.is_member == False]['revenue'].sum()

# Lift from loyalty program
lift = (loyalty_revenue / member_count) / (non_member_revenue / non_member_count)
# Example: 2.5x - Loyalty members spend 2.5x more
```

### Incremental Revenue

```python
# A/B test loyalty program impact
control_group = members[members.enrolled_in_loyalty == False]
treatment_group = members[members.enrolled_in_loyalty == True]

incremental_revenue = (
    treatment_group.revenue.mean() -
    control_group.revenue.mean()
)

# Example: $125 incremental revenue per member
```

## Reporting

### Daily Dashboard

```python
report = analytics.generate_daily_report(datetime.now())

# Returns:
# - new_members: Enrolled today
# - points_issued: Points earned today
# - points_redeemed: Points spent today
# - active_members: Logged in today
# - top_earners: Highest points earned
```

### Monthly Executive Report

```python
monthly = analytics.generate_monthly_report(year=2025, month=1)

# Includes:
# - Member growth
# - Tier distribution changes
# - Redemption trends
# - Revenue impact
# - ROI metrics
# - Churn analysis
```

## Visualization

### Points Flow Diagram

```
Earned Points
    ↓
  [Bronze] → (1,000 pts) → [Silver]
    ↓                         ↓
  [Silver] → (5,000 pts) → [Gold]
    ↓                         ↓
  [Gold] → (10,000 pts) → [Platinum]
    ↓
Redeemed Points
```

### Member Journey

```
Enrollment → First Purchase → First Review → First Redemption
    ↓             ↓              ↓               ↓
 100 pts      +150 pts       +50 pts         -500 pts
    ↓             ↓              ↓               ↓
  Bronze       Bronze          Silver          Silver
```

## Benchmarks

### Industry Standards

| Metric                | Target  | Best-in-Class |
|-----------------------|---------|---------------|
| Redemption Rate       | 20-30%  | 40%+          |
| Active Member Rate    | 40-50%  | 70%+          |
| Referral Rate         | 10-15%  | 25%+          |
| Tier Upgrade Rate     | 15-20%  | 30%+          |
| 12-Month Retention    | 50-60%  | 80%+          |
| Program ROI           | 200%+   | 400%+         |

## Export Formats

### CSV Export

```python
# Export member data
members_df.to_csv('loyalty_members.csv', index=False)

# Export transactions
transactions_df.to_csv('points_transactions.csv', index=False)
```

### JSON API

```python
# API endpoint for analytics
@app.get('/api/analytics/summary')
def get_analytics_summary():
    metrics = analytics.calculate_program_metrics()
    return {
        'total_members': metrics.total_members,
        'redemption_rate': metrics.redemption_rate,
        'tier_distribution': metrics.tier_distribution
    }
```

### Data Warehouse Integration

```python
# Push to BigQuery/Snowflake
warehouse.push('loyalty_metrics', {
    'date': datetime.now(),
    'metrics': metrics,
    'segments': segments,
    'tier_stats': tier_stats
})
```
