//! Evidence Integration Demonstration
//!
//! Shows how Byzantine evidence integrates with replication for region health scoring.

use std::collections::HashMap;

// Simplified types for demonstration
#[derive(Debug, Clone, Copy)]
pub struct HealthScore(pub i32);

impl HealthScore {
    pub const MAX: i32 = 100;
    pub const MIN: i32 = 0;
    pub const ISOLATION_THRESHOLD: i32 = 30;

    pub fn new(score: i32) -> Self {
        Self(score.clamp(Self::MIN, Self::MAX))
    }

    pub fn should_isolate(&self) -> bool {
        self.0 < Self::ISOLATION_THRESHOLD
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegionHealth {
    Healthy,
    Degraded,
    Unhealthy,
}

impl From<HealthScore> for RegionHealth {
    fn from(score: HealthScore) -> Self {
        match score.0 {
            s if s >= 70 => RegionHealth::Healthy,
            s if s >= 40 => RegionHealth::Degraded,
            _ => RegionHealth::Unhealthy,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EvidenceSeverity {
    Low,
    Medium,
    High,
    Critical,
}

// Demonstration of evidence-based health scoring
fn main() {
    println!("=== Byzantine Evidence Integration Demo ===\n");

    // Scenario 1: Gradual degradation
    println!("Scenario 1: Gradual Degradation from Timeouts");
    println!("-----------------------------------------------");
    let mut score = HealthScore::new(100);
    println!("Initial health: {:?} (score: {})", RegionHealth::from(score), score.0);

    // Simulate 5 timeout events (-5 points each)
    for i in 1..=5 {
        score = HealthScore::new(score.0 - 5);
        let health: RegionHealth = score.into();
        println!("After timeout #{}: {:?} (score: {})", i, health, score.0);
    }
    assert_eq!(score.0, 75);
    println!("✅ Region degraded but not isolated\n");

    // Scenario 2: Data conflict
    println!("Scenario 2: Data Conflict Event");
    println!("-------------------------------");
    score = HealthScore::new(100);
    score = HealthScore::new(score.0 - 25); // Data conflict penalty
    let health: RegionHealth = score.into();
    println!("After conflict: {:?} (score: {})", health, score.0);
    assert_eq!(score.0, 75);
    println!("✅ Significant penalty, still healthy\n");

    // Scenario 3: Critical Byzantine evidence
    println!("Scenario 3: Critical Byzantine Evidence");
    println!("---------------------------------------");
    score = HealthScore::new(100);
    println!("Initial health: {:?} (score: {})", RegionHealth::from(score), score.0);

    // First critical evidence
    score = HealthScore::new(score.0 - 50);
    let health: RegionHealth = score.into();
    println!("After 1st critical evidence: {:?} (score: {})", health, score.0);
    assert!(!score.should_isolate());

    // Second critical evidence
    score = HealthScore::new(score.0 - 50);
    let health: RegionHealth = score.into();
    println!("After 2nd critical evidence: {:?} (score: {})", health, score.0);
    assert!(score.should_isolate());
    println!("✅ Region isolated after 2 critical violations\n");

    // Scenario 4: Recovery
    println!("Scenario 4: Recovery After Failure");
    println!("----------------------------------");
    score = HealthScore::new(40);
    println!("After failure: {:?} (score: {})", RegionHealth::from(score), score.0);

    // Successful operations (+2 points each)
    for i in 1..=5 {
        score = HealthScore::new(score.0 + 2);
        let health: RegionHealth = score.into();
        println!("After successful op #{}: {:?} (score: {})", i, health, score.0);
    }
    assert_eq!(score.0, 50);
    println!("✅ Gradual recovery to degraded state\n");

    // Scenario 5: Evidence-based isolation decision
    println!("Scenario 5: Evidence-Based Isolation Decision");
    println!("---------------------------------------------");
    let mut region_scores: HashMap<&str, HealthScore> = HashMap::new();
    region_scores.insert("us-east", HealthScore::new(95));
    region_scores.insert("us-west", HealthScore::new(25));
    region_scores.insert("eu", HealthScore::new(60));

    println!("Region Health Scores:");
    for (region, score) in &region_scores {
        let health: RegionHealth = (*score).into();
        println!("  {}: {:?} (score: {})", region, health, score.0);
    }

    let to_isolate: Vec<_> = region_scores
        .iter()
        .filter(|(_, score)| score.should_isolate())
        .map(|(region, _)| *region)
        .collect();

    println!("\nRegions to isolate: {:?}", to_isolate);
    assert_eq!(to_isolate, vec!["us-west"]);
    println!("✅ Only us-west isolated (score < 30)\n");

    // Summary
    println!("=== Summary ===");
    println!("Evidence integration provides:");
    println!("  • Type-safe health scoring (0-100)");
    println!("  • Severity-based penalties (5-50 points)");
    println!("  • Automatic isolation at score < 30");
    println!("  • Gradual recovery (+2 points per success)");
    println!("  • Evidence-based audit trail");
    println!("\n✅ All scenarios passed!");
}
