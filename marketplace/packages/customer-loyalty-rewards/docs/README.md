# Customer Loyalty & Rewards

Production-ready loyalty program with points, tiers, and gamification.

## Overview

This package provides a comprehensive loyalty solution including:

- **Tier System**: Bronze, Silver, Gold, Platinum with multipliers
- **Points Earning**: Purchase, referral, review, birthday, social sharing
- **Rewards Catalog**: Discounts, free shipping, products, experiences
- **Gamification**: Badges, achievements, streaks, leaderboards
- **Analytics**: Member segmentation, engagement metrics, ROI

## Components

### 1. RDF Ontology (270+ lines)

**File**: `ontology/loyalty-rewards.ttl`

Defines comprehensive loyalty semantics:

- Loyalty program and membership
- Tier levels with benefits
- Points earning and redemption
- Rewards catalog
- Gamification elements
- Referral programs

### 2. SPARQL Templates (10+ queries)

**File**: `sparql/queries.rq`

Production queries for:

- Points calculation
- Available rewards
- Tier progression
- Expiration tracking
- Referral analytics
- Leaderboards

### 3. Multi-Language Code

**Rust**: `templates/rust/points_engine.rs`
- High-performance points calculation
- Tier multipliers
- Redemption validation

**TypeScript**: `templates/typescript/loyalty-ui.ts`
- React UI components
- Member dashboard
- Rewards catalog

**Python**: `templates/python/loyalty_analytics.py`
- Member segmentation
- Engagement analysis
- ROI calculation

### 4. Chicago TDD Tests (550+ lines)

**File**: `tests/chicago_tdd/loyalty_tests.rs`

Test coverage:
- Points calculation (8 tests)
- Tier progression (11 tests)
- Redemption workflow (4 tests)
- Expiration handling (3 tests)
- Reward catalog (3 tests)
- Gamification (3 tests)

## Tier System

### Tier Levels

| Tier     | Min Points | Multiplier | Benefits                           |
|----------|------------|------------|------------------------------------|
| Bronze   | 0          | 1.0x       | Standard points earning            |
| Silver   | 1,000      | 1.25x      | 25% bonus points                   |
| Gold     | 5,000      | 1.5x       | 50% bonus points, exclusive access |
| Platinum | 10,000     | 2.0x       | Double points, priority support    |

### Tier Progression

Members automatically upgrade based on lifetime points:

```rust
pub fn from_points(points: i32) -> Tier {
    if points >= 10000 { Tier::Platinum }
    else if points >= 5000 { Tier::Gold }
    else if points >= 1000 { Tier::Silver }
    else { Tier::Bronze }
}
```

## Points Earning

### Purchase Points

```
points = purchase_amount × points_per_dollar × tier_multiplier
```

Example (Gold tier, $100 purchase):
```
points = 100 × 1.0 × 1.5 = 150 points
```

### Bonus Points

| Activity       | Points |
|----------------|--------|
| Referral       | 500    |
| Product Review | 50     |
| Birthday Bonus | 100    |
| Social Share   | 25     |

## Rewards Catalog

### Discount Rewards

- $10 off: 1,000 points
- $25 off: 2,000 points
- $50 off: 4,000 points

### Percentage Discounts

- 10% off: 1,500 points
- 20% off: 3,000 points
- 30% off: 5,000 points

### Special Rewards

- Free shipping: 500 points
- Free product: 2,500 points
- VIP experience: 10,000 points

## Usage Examples

### Calculate Purchase Points

```rust
let engine = PointsEngine::new();
let points = engine.calculate_purchase_points(100.0, Tier::Gold);
// points = 150
```

### Redeem Reward

```typescript
const service = new LoyaltyService();
const result = await service.redeemReward(memberId, rewardId);
// { success: true, voucher: "VOUCHER-123" }
```

### Check Tier Progress

```sparql
SELECT ?member ?currentTier ?recommendedTier ?pointsBalance
WHERE {
    ?member a lr:Member ;
            lr:hasTier ?currentTier ;
            lr:hasPointsBalance ?pointsBalance .

    ?recommendedTier a lr:Tier ;
                     lr:hasMinimumPoints ?minPoints .

    FILTER(?pointsBalance >= ?minPoints)
}
```

## Gamification

### Badges

Earn badges for achievements:

- First Purchase
- 10 Reviews Written
- 5 Successful Referrals
- 30-Day Login Streak
- Gold Tier Achieved

### Streaks

Track consecutive activities:

```rust
let mut streak = Streak::new("daily_login");
streak.increment(); // count = 1
```

### Leaderboards

Compete with other members:

```sparql
SELECT ?member ?memberId ?pointsBalance ?tier
WHERE {
    ?member a lr:Member ;
            lr:hasMemberId ?memberId ;
            lr:hasPointsBalance ?pointsBalance ;
            lr:hasTier ?tier .
}
ORDER BY DESC(?pointsBalance)
LIMIT 10
```

## Analytics

### Member Segmentation

| Segment            | Engagement Score | Characteristics                    |
|--------------------|------------------|------------------------------------|
| Champions          | 1000+            | High spend, frequent engagement    |
| Loyal Customers    | 500-999          | Regular purchases, moderate spend  |
| Potential Loyalists| 200-499          | Growing engagement                 |
| New Customers      | 50-199           | Recent enrollees                   |
| At Risk            | <50              | Low engagement, churn risk         |

### ROI Calculation

```python
tier_roi = analytics.calculate_tier_roi(revenue_data)
# Returns: totalRevenue, pointsCost, roi by tier
```

## Performance

- **Points calculation**: O(1) constant time
- **Tier lookup**: O(1) with if-else chain
- **Redemption validation**: O(1) balance check
- **SPARQL queries**: Optimized with tier and points indexes

## Testing

Run Chicago TDD tests:

```bash
cargo test --test loyalty_tests
```

All tests use Chicago-style testing with clear ARRANGE-ACT-ASSERT structure.

## License

MIT License - See LICENSE file for details
