// ============================================================================
// Loyalty Points Calculation Engine - Rust
// ============================================================================
// High-performance points calculation with tier multipliers
// ============================================================================

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};

// ============================================================================
// Loyalty Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Tier {
    Bronze,
    Silver,
    Gold,
    Platinum,
}

impl Tier {
    pub fn multiplier(&self) -> f64 {
        match self {
            Tier::Bronze => 1.0,
            Tier::Silver => 1.25,
            Tier::Gold => 1.5,
            Tier::Platinum => 2.0,
        }
    }

    pub fn minimum_points(&self) -> i32 {
        match self {
            Tier::Bronze => 0,
            Tier::Silver => 1000,
            Tier::Gold => 5000,
            Tier::Platinum => 10000,
        }
    }

    pub fn from_points(points: i32) -> Self {
        if points >= 10000 {
            Tier::Platinum
        } else if points >= 5000 {
            Tier::Gold
        } else if points >= 1000 {
            Tier::Silver
        } else {
            Tier::Bronze
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Member {
    pub member_id: String,
    pub tier: Tier,
    pub points_balance: i32,
    pub lifetime_points: i32,
    pub enrollment_date: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PointsTransaction {
    pub transaction_id: String,
    pub member_id: String,
    pub points: i32,
    pub transaction_type: TransactionType,
    pub timestamp: DateTime<Utc>,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransactionType {
    Earned,
    Redeemed,
    Expired,
    Adjusted,
}

// ============================================================================
// Points Calculation Engine
// ============================================================================

pub struct PointsEngine {
    points_per_dollar: f64,
    referral_points: i32,
    review_points: i32,
    birthday_bonus: i32,
}

impl PointsEngine {
    pub fn new() -> Self {
        Self {
            points_per_dollar: 1.0,
            referral_points: 500,
            review_points: 50,
            birthday_bonus: 100,
        }
    }

    /// Calculate points earned from purchase
    pub fn calculate_purchase_points(
        &self,
        amount: f64,
        tier: Tier,
    ) -> i32 {
        let base_points = (amount * self.points_per_dollar) as i32;
        let multiplier = tier.multiplier();
        (base_points as f64 * multiplier) as i32
    }

    /// Award referral points
    pub fn award_referral_points(&self) -> i32 {
        self.referral_points
    }

    /// Award review points
    pub fn award_review_points(&self) -> i32 {
        self.review_points
    }

    /// Award birthday bonus
    pub fn award_birthday_bonus(&self) -> i32 {
        self.birthday_bonus
    }

    /// Update member tier based on points
    pub fn update_tier(&self, member: &mut Member) {
        let new_tier = Tier::from_points(member.lifetime_points);
        if new_tier != member.tier {
            member.tier = new_tier;
        }
    }

    /// Redeem points
    pub fn redeem_points(
        &self,
        member: &mut Member,
        points_cost: i32,
    ) -> Result<(), String> {
        if member.points_balance < points_cost {
            return Err(format!(
                "Insufficient points: {} available, {} required",
                member.points_balance, points_cost
            ));
        }

        member.points_balance -= points_cost;
        Ok(())
    }

    /// Expire points older than expiration date
    pub fn expire_points(
        &self,
        member: &mut Member,
        expiring_points: i32,
    ) {
        member.points_balance = member.points_balance.saturating_sub(expiring_points);
    }
}

// ============================================================================
// Rewards Catalog
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reward {
    pub reward_id: String,
    pub name: String,
    pub points_cost: i32,
    pub reward_type: RewardType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RewardType {
    Discount { amount: f64 },
    PercentageDiscount { percentage: u8 },
    FreeShipping,
    Product { product_id: String },
}

pub struct RewardsCatalog {
    rewards: HashMap<String, Reward>,
}

impl RewardsCatalog {
    pub fn new() -> Self {
        Self {
            rewards: HashMap::new(),
        }
    }

    pub fn add_reward(&mut self, reward: Reward) {
        self.rewards.insert(reward.reward_id.clone(), reward);
    }

    pub fn get_affordable_rewards(&self, points_balance: i32) -> Vec<&Reward> {
        self.rewards
            .values()
            .filter(|r| r.points_cost <= points_balance)
            .collect()
    }

    pub fn get_reward(&self, reward_id: &str) -> Option<&Reward> {
        self.rewards.get(reward_id)
    }
}

// ============================================================================
// Gamification System
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Badge {
    pub badge_id: String,
    pub name: String,
    pub description: String,
    pub icon: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Streak {
    pub streak_type: String,
    pub count: i32,
    pub last_activity: DateTime<Utc>,
}

impl Streak {
    pub fn new(streak_type: String) -> Self {
        Self {
            streak_type,
            count: 0,
            last_activity: Utc::now(),
        }
    }

    pub fn increment(&mut self) -> bool {
        let now = Utc::now();
        let days_diff = (now - self.last_activity).num_days();

        if days_diff == 1 {
            self.count += 1;
            self.last_activity = now;
            true
        } else if days_diff > 1 {
            self.count = 1;
            self.last_activity = now;
            false
        } else {
            true
        }
    }

    pub fn is_active(&self) -> bool {
        let days_diff = (Utc::now() - self.last_activity).num_days();
        days_diff <= 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tier_multipliers() {
        assert_eq!(Tier::Bronze.multiplier(), 1.0);
        assert_eq!(Tier::Silver.multiplier(), 1.25);
        assert_eq!(Tier::Gold.multiplier(), 1.5);
        assert_eq!(Tier::Platinum.multiplier(), 2.0);
    }

    #[test]
    fn test_tier_from_points() {
        assert_eq!(Tier::from_points(0), Tier::Bronze);
        assert_eq!(Tier::from_points(1000), Tier::Silver);
        assert_eq!(Tier::from_points(5000), Tier::Gold);
        assert_eq!(Tier::from_points(10000), Tier::Platinum);
    }

    #[test]
    fn test_purchase_points_calculation() {
        let engine = PointsEngine::new();

        // Bronze: 100 * 1.0 * 1.0 = 100
        assert_eq!(engine.calculate_purchase_points(100.0, Tier::Bronze), 100);

        // Silver: 100 * 1.0 * 1.25 = 125
        assert_eq!(engine.calculate_purchase_points(100.0, Tier::Silver), 125);

        // Gold: 100 * 1.0 * 1.5 = 150
        assert_eq!(engine.calculate_purchase_points(100.0, Tier::Gold), 150);

        // Platinum: 100 * 1.0 * 2.0 = 200
        assert_eq!(engine.calculate_purchase_points(100.0, Tier::Platinum), 200);
    }

    #[test]
    fn test_redeem_points() {
        let engine = PointsEngine::new();
        let mut member = create_test_member();

        // Successful redemption
        assert!(engine.redeem_points(&mut member, 50).is_ok());
        assert_eq!(member.points_balance, 50);

        // Insufficient points
        assert!(engine.redeem_points(&mut member, 100).is_err());
    }

    #[test]
    fn test_streak_increment() {
        let mut streak = Streak::new("daily_login".to_string());

        assert!(streak.increment());
        assert_eq!(streak.count, 1);
    }

    fn create_test_member() -> Member {
        Member {
            member_id: "MEM-001".to_string(),
            tier: Tier::Bronze,
            points_balance: 100,
            lifetime_points: 100,
            enrollment_date: Utc::now(),
        }
    }
}
