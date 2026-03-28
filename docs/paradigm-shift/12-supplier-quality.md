<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Supplier Quality Scoring: Defect-Driven Rate Limiting](#supplier-quality-scoring-defect-driven-rate-limiting)
  - [TL;DR](#tldr)
  - [Table of Contents](#table-of-contents)
  - [The Problem: Coordination Cost Externalization](#the-problem-coordination-cost-externalization)
    - [Traditional Workflow](#traditional-workflow)
    - [The Coordination Cost Problem](#the-coordination-cost-problem)
  - [The Solution: Supplier Quality Scoring](#the-solution-supplier-quality-scoring)
    - [Core Principle: Defects Have Consequences](#core-principle-defects-have-consequences)
    - [The Feedback Loop](#the-feedback-loop)
  - [Defect Taxonomy](#defect-taxonomy)
    - [Type 1: Incomplete Packets](#type-1-incomplete-packets)
    - [Type 2: Requirement Churn](#type-2-requirement-churn)
    - [Type 3: Urgency Inflation](#type-3-urgency-inflation)
    - [Type 4: Coordination Dumping](#type-4-coordination-dumping)
  - [Scoring Algorithms](#scoring-algorithms)
    - [Bayesian Score Update](#bayesian-score-update)
    - [Score Computation with Defect Weighting](#score-computation-with-defect-weighting)
  - [Rate Limiting Implementation](#rate-limiting-implementation)
    - [Rate Limit Strategies](#rate-limit-strategies)
    - [Rate Limit Enforcement](#rate-limit-enforcement)
  - [Automated Quality Gates](#automated-quality-gates)
    - [Quality Gate Implementation](#quality-gate-implementation)
  - [Supplier Feedback Loops](#supplier-feedback-loops)
    - [Feedback Report Generation](#feedback-report-generation)
  - [Upstream Pays Principle](#upstream-pays-principle)
    - [Cost Accounting](#cost-accounting)
  - [Real-World Examples](#real-world-examples)
    - [Example 1: Human Supplier (Product Manager)](#example-1-human-supplier-product-manager)
    - [Example 2: AI Agent Supplier](#example-2-ai-agent-supplier)
    - [Example 3: Team Supplier Comparison](#example-3-team-supplier-comparison)
  - [Integration with ggen](#integration-with-ggen)
    - [ggen Quality Gate CLI](#ggen-quality-gate-cli)
    - [Usage Examples](#usage-examples)
  - [Summary](#summary)
    - [Key Principles](#key-principles)
    - [Benefits](#benefits)
    - [Integration Points](#integration-points)
  - [Further Reading](#further-reading)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Supplier Quality Scoring: Defect-Driven Rate Limiting

**Reading Time**: 35-40 minutes | **Difficulty**: Advanced | **Prerequisites**: [Packet Discipline](./10-packet-discipline.md), [Regime Split](./01-regime-split.md)

---

## TL;DR

**Supplier quality scoring treats all work sources (humans, agents, systems) as suppliers in a manufacturing pipeline, scoring them by defect rate and rate-limiting high-defect suppliers.**

In manufacturing, defective suppliers get rate-limited or blocked. Same principle applies to work packets. Incomplete packets, requirement churn, urgency inflation, and coordination dumping are measurable defects. The upstream supplier pays the coordination cost, not the downstream system.

Once you adopt supplier quality scoring, you cannot accept work from high-defect sources without rate limiting. Like manufacturing tolerances prevent defective parts, quality scores prevent coordination waste.

---

## Table of Contents

1. [The Problem: Coordination Cost Externalization](#the-problem-coordination-cost-externalization)
2. [The Solution: Supplier Quality Scoring](#the-solution-supplier-quality-scoring)
3. [Defect Taxonomy](#defect-taxonomy)
4. [Scoring Algorithms](#scoring-algorithms)
5. [Rate Limiting Implementation](#rate-limiting-implementation)
6. [Automated Quality Gates](#automated-quality-gates)
7. [Supplier Feedback Loops](#supplier-feedback-loops)
8. [Upstream Pays Principle](#upstream-pays-principle)
9. [Real-World Examples](#real-world-examples)
10. [Integration with ggen](#integration-with-ggen)

---

## The Problem: Coordination Cost Externalization

### Traditional Workflow

```
Bad Supplier                  Downstream System
────────────                  ─────────────────
"Make it faster" ────────────> Parse vague request
                               Ask clarifying questions
"ASAP!" ─────────────────────> Negotiate deadline
                               Estimate scope
"No budget" ─────────────────> Assess feasibility
                               Push back on constraints
"Changed my mind" ───────────> Throw away work
                               Start over

RESULT: Downstream pays ALL coordination costs
        Supplier has no incentive to improve
        Defects are rewarded (get immediate attention)
```

### The Coordination Cost Problem

Every defective work packet imposes costs:

```rust
struct CoordinationCost {
    // Time costs
    clarification_rounds: u32,          // "What did you mean?"
    rework_cycles: u32,                 // "You changed requirements"
    validation_failures: u32,           // "This packet is malformed"

    // Resource costs
    wasted_computation: Duration,       // Partial work discarded
    context_switching: u32,             // Interruptions from supplier
    downstream_blocking: Duration,      // Waiting for clarification

    // Quality costs
    defect_propagation: u32,            // Bad specs → bad code
    technical_debt_accrued: f64,        // Quick fixes to work around defects
    reliability_degradation: f64,       // System confidence erodes
}

impl CoordinationCost {
    fn total_cost_usd(&self) -> f64 {
        // Conservative estimate: $200/hour engineer time
        let hourly_rate = 200.0;

        let clarification_hours = self.clarification_rounds as f64 * 0.5;
        let rework_hours = self.rework_cycles as f64 * 2.0;
        let validation_hours = self.validation_failures as f64 * 0.25;

        (clarification_hours + rework_hours + validation_hours) * hourly_rate
    }
}
```

**Example**:
- 5 clarification rounds = 2.5 hours = $500
- 3 rework cycles = 6 hours = $1,200
- 10 validation failures = 2.5 hours = $500
- **Total**: 11 hours = **$2,200 coordination cost**

**Who pays?** Traditionally, the downstream system.

**Who should pay?** The defective supplier (via rate limiting).

---

## The Solution: Supplier Quality Scoring

### Core Principle: Defects Have Consequences

```rust
/// Supplier quality score based on defect rate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplierQualityScore {
    pub supplier_id: SupplierId,
    pub score: f64,  // 0.0 (worst) to 100.0 (perfect)
    pub defect_rate: f64,  // Defects per work packet
    pub reputation: SupplierReputation,
    pub rate_limit: RateLimit,
    pub total_packets_submitted: u64,
    pub total_defects: u64,
    pub last_updated: DateTime<Utc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SupplierReputation {
    Trusted,      // Score ≥ 90: No rate limiting
    Verified,     // Score ≥ 70: Minimal rate limiting
    Provisional,  // Score ≥ 50: Moderate rate limiting
    Restricted,   // Score ≥ 30: Heavy rate limiting
    Blocked,      // Score < 30: No work accepted
}

impl SupplierQualityScore {
    pub fn compute_score(&self) -> f64 {
        if self.total_packets_submitted == 0 {
            return 50.0;  // Neutral starting score
        }

        // Base score: inverse of defect rate
        let base_score = 100.0 * (1.0 - self.defect_rate);

        // Apply confidence factor (more data = more confidence)
        let confidence = confidence_factor(self.total_packets_submitted);

        // Weight towards 50.0 for low-confidence scores
        50.0 + (base_score - 50.0) * confidence
    }

    pub fn reputation(&self) -> SupplierReputation {
        match self.score {
            s if s >= 90.0 => SupplierReputation::Trusted,
            s if s >= 70.0 => SupplierReputation::Verified,
            s if s >= 50.0 => SupplierReputation::Provisional,
            s if s >= 30.0 => SupplierReputation::Restricted,
            _ => SupplierReputation::Blocked,
        }
    }
}

fn confidence_factor(num_packets: u64) -> f64 {
    // Asymptotic confidence: approaches 1.0 as data increases
    // Uses inverse exponential: 1 - e^(-k*n)
    let k = 0.1;  // Confidence growth rate
    1.0 - (-k * num_packets as f64).exp()
}
```

### The Feedback Loop

```
Work Packet Submission
        ↓
   Validation
        ↓
    ┌───────────────────┐
    │ Defect Detection  │
    └───────────────────┘
        ↓
    ┌───────────────────┐
    │ Score Update      │
    │ (Bayesian)        │
    └───────────────────┘
        ↓
    ┌───────────────────┐
    │ Rate Limit Adjust │
    └───────────────────┘
        ↓
    ┌───────────────────┐
    │ Supplier Feedback │
    │ (Defect Report)   │
    └───────────────────┘
        ↓
    Improved Packets
```

---

## Defect Taxonomy

### Type 1: Incomplete Packets

**Definition**: Work packet missing required fields or validation criteria.

**Detection**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DefectType {
    IncompletePacket(IncompleteDefect),
    RequirementChurn(ChurnDefect),
    UrgencyInflation(UrgencyDefect),
    CoordinationDumping(CoordinationDefect),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IncompleteDefect {
    pub missing_fields: Vec<String>,
    pub severity: DefectSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefectSeverity {
    Critical,   // Cannot proceed (weight: 1.0)
    High,       // Major rework needed (weight: 0.75)
    Medium,     // Clarification needed (weight: 0.5)
    Low,        // Minor issue (weight: 0.25)
}

impl DefectSeverity {
    pub fn weight(&self) -> f64 {
        match self {
            Self::Critical => 1.0,
            Self::High => 0.75,
            Self::Medium => 0.5,
            Self::Low => 0.25,
        }
    }
}
```

**Examples**:

```rust
// ❌ DEFECT: Incomplete Packet (missing objective)
let defective_packet = WorkPacket {
    id: WorkPacketId::new("WP", 500),
    objective: Objective {
        description: String::new(),  // MISSING
        success_criteria: vec![],    // MISSING
    },
    constraints: Constraints::default(),
    acceptance_test: AcceptanceTest::default(),
    reversibility: ReversibilityPlan::default(),
    dependencies: Dependencies::default(),
    owner: Owner::default(),
    status: WorkPacketStatus::Proposed,
    created_at: Utc::now(),
    updated_at: Utc::now(),
};

// Defect detection
let defect = IncompleteDefect {
    missing_fields: vec![
        "objective.description".to_string(),
        "objective.success_criteria".to_string(),
    ],
    severity: DefectSeverity::Critical,
};

// ✅ NO DEFECT: Complete Packet
let complete_packet = WorkPacket {
    objective: Objective {
        description: "Reduce API p95 latency to <200ms".to_string(),
        success_criteria: vec![
            SuccessCriterion { /* ... */ },
        ],
    },
    // ... all required fields populated
};
```

**Defect Weight**: Missing critical fields = 1.0 defect

---

### Type 2: Requirement Churn

**Definition**: Supplier changes requirements after work has started, invalidating downstream effort.

**Detection**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChurnDefect {
    pub packet_id: WorkPacketId,
    pub change_requests: Vec<ChangeRequest>,
    pub wasted_effort_hours: f64,
    pub severity: DefectSeverity,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeRequest {
    pub timestamp: DateTime<Utc>,
    pub field_changed: String,
    pub old_value: String,
    pub new_value: String,
    pub work_invalidated: bool,
}

impl ChurnDefect {
    pub fn calculate_severity(&self) -> DefectSeverity {
        let invalidating_changes = self.change_requests
            .iter()
            .filter(|cr| cr.work_invalidated)
            .count();

        match invalidating_changes {
            0 => DefectSeverity::Low,
            1 => DefectSeverity::Medium,
            2 => DefectSeverity::High,
            _ => DefectSeverity::Critical,
        }
    }

    pub fn churn_rate(&self) -> f64 {
        // Changes per day
        if self.change_requests.is_empty() {
            return 0.0;
        }

        let first = self.change_requests.first().unwrap().timestamp;
        let last = self.change_requests.last().unwrap().timestamp;
        let duration_days = (last - first).num_days() as f64;

        if duration_days < 1.0 {
            self.change_requests.len() as f64
        } else {
            self.change_requests.len() as f64 / duration_days
        }
    }
}
```

**Examples**:

```rust
// ❌ DEFECT: High Churn
let churn_defect = ChurnDefect {
    packet_id: WorkPacketId::new("WP", 501),
    change_requests: vec![
        ChangeRequest {
            timestamp: Utc::now() - Duration::days(2),
            field_changed: "objective.description".to_string(),
            old_value: "Add user authentication".to_string(),
            new_value: "Add OAuth2 with Google and GitHub".to_string(),
            work_invalidated: true,  // ← Wasted 8 hours of work
        },
        ChangeRequest {
            timestamp: Utc::now() - Duration::days(1),
            field_changed: "constraints.deadline".to_string(),
            old_value: "2026-02-15".to_string(),
            new_value: "2026-02-10".to_string(),
            work_invalidated: true,  // ← New approach needed
        },
        ChangeRequest {
            timestamp: Utc::now(),
            field_changed: "objective.description".to_string(),
            old_value: "Add OAuth2 with Google and GitHub".to_string(),
            new_value: "Just add basic username/password for now".to_string(),
            work_invalidated: true,  // ← All OAuth work wasted
        },
    ],
    wasted_effort_hours: 16.0,
    severity: DefectSeverity::Critical,
};

// Churn rate: 3 changes / 2 days = 1.5 changes/day (HIGH)
```

**Defect Weight**:
- 1-2 changes = 0.25 defect
- 3-5 changes = 0.5 defect
- 6+ changes = 1.0 defect

---

### Type 3: Urgency Inflation

**Definition**: Supplier marks packets as urgent when they're not, creating false priority signals.

**Detection**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UrgencyDefect {
    pub packet_id: WorkPacketId,
    pub claimed_urgency: UrgencyLevel,
    pub actual_urgency: UrgencyLevel,
    pub inflation_ratio: f64,
    pub severity: DefectSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum UrgencyLevel {
    Low,       // Days to weeks
    Medium,    // Hours to days
    High,      // Minutes to hours
    Critical,  // Seconds to minutes (production down)
}

impl UrgencyDefect {
    pub fn detect(packet: &WorkPacket, completion_time: DateTime<Utc>) -> Option<Self> {
        let claimed = Self::parse_claimed_urgency(packet);
        let actual = Self::calculate_actual_urgency(packet, completion_time);

        if claimed > actual {
            let inflation = (claimed as u8 - actual as u8) as f64;
            Some(UrgencyDefect {
                packet_id: packet.id.clone(),
                claimed_urgency: claimed,
                actual_urgency: actual,
                inflation_ratio: inflation,
                severity: match inflation as u8 {
                    1 => DefectSeverity::Medium,
                    2 => DefectSeverity::High,
                    3 => DefectSeverity::Critical,
                    _ => DefectSeverity::Low,
                },
            })
        } else {
            None
        }
    }

    fn parse_claimed_urgency(packet: &WorkPacket) -> UrgencyLevel {
        // Check for urgency markers in description
        let desc = packet.objective.description.to_lowercase();
        if desc.contains("critical") || desc.contains("emergency") {
            UrgencyLevel::Critical
        } else if desc.contains("urgent") || desc.contains("asap") {
            UrgencyLevel::High
        } else if desc.contains("soon") {
            UrgencyLevel::Medium
        } else {
            UrgencyLevel::Low
        }
    }

    fn calculate_actual_urgency(packet: &WorkPacket, completion: DateTime<Utc>) -> UrgencyLevel {
        // Actual urgency based on observed behavior
        let time_to_completion = completion - packet.created_at;

        match time_to_completion.num_hours() {
            0..=1 => UrgencyLevel::Critical,
            2..=24 => UrgencyLevel::High,
            25..=168 => UrgencyLevel::Medium,
            _ => UrgencyLevel::Low,
        }
    }
}
```

**Examples**:

```rust
// ❌ DEFECT: Urgency Inflation
let packet = WorkPacket {
    objective: Objective {
        description: "URGENT CRITICAL: Add dark mode toggle".to_string(),
        //              ^^^^^^^^^^^^^^^^  False urgency
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::days(7)),  // 7 days? Not urgent.
    },
    // ...
};

// Reality: Dark mode is nice-to-have, not business-critical
// Actual completion: 5 days later (Low urgency)
// Claimed urgency: Critical
// Inflation ratio: 3 levels (Critical → Low)
// Severity: Critical defect

let defect = UrgencyDefect {
    packet_id: packet.id.clone(),
    claimed_urgency: UrgencyLevel::Critical,
    actual_urgency: UrgencyLevel::Low,
    inflation_ratio: 3.0,
    severity: DefectSeverity::Critical,
};
```

**Defect Weight**: Inflation ratio × 0.33 (max 1.0 defect)

---

### Type 4: Coordination Dumping

**Definition**: Supplier offloads coordination work to downstream by providing under-specified packets.

**Detection**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoordinationDefect {
    pub packet_id: WorkPacketId,
    pub clarification_rounds: u32,
    pub questions_asked: Vec<String>,
    pub coordination_hours: f64,
    pub severity: DefectSeverity,
}

impl CoordinationDefect {
    pub fn detect_from_validation(
        packet: &WorkPacket,
        validation_report: &ValidationReport,
    ) -> Option<Self> {
        let questions = Self::extract_clarification_questions(validation_report);

        if questions.is_empty() {
            return None;
        }

        let coordination_hours = questions.len() as f64 * 0.5;  // 30 min per question

        Some(CoordinationDefect {
            packet_id: packet.id.clone(),
            clarification_rounds: questions.len() as u32,
            questions_asked: questions,
            coordination_hours,
            severity: match coordination_hours as u32 {
                0..=1 => DefectSeverity::Low,
                2..=4 => DefectSeverity::Medium,
                5..=8 => DefectSeverity::High,
                _ => DefectSeverity::Critical,
            },
        })
    }

    fn extract_clarification_questions(report: &ValidationReport) -> Vec<String> {
        let mut questions = Vec::new();

        for warning in &report.warnings {
            match warning {
                ValidationWarning::VagueObjective { suggestion, .. } => {
                    questions.push(suggestion.clone());
                }
                _ => {}
            }
        }

        for error in &report.errors {
            match error {
                ValidationError::MissingSuccessCriteria => {
                    questions.push("What defines success for this work?".to_string());
                }
                ValidationError::NonExecutableAcceptanceTest => {
                    questions.push("How do we verify completion?".to_string());
                }
                _ => {}
            }
        }

        questions
    }
}
```

**Examples**:

```rust
// ❌ DEFECT: Coordination Dumping
let packet = WorkPacket {
    objective: Objective {
        description: "Make it better".to_string(),  // Vague
        success_criteria: vec![],  // Missing
    },
    constraints: Constraints::default(),  // Underspecified
    acceptance_test: AcceptanceTest::default(),  // Not defined
    // ...
};

// Downstream must ask:
// 1. "What does 'better' mean?"
// 2. "Better by what metric?"
// 3. "What's the success threshold?"
// 4. "How do we test this?"
// 5. "What's the deadline?"
// 6. "What's the budget?"
// 7. "What constraints exist?"
// 8. "Who's the owner?"

let defect = CoordinationDefect {
    packet_id: packet.id.clone(),
    clarification_rounds: 8,
    questions_asked: vec![
        "What does 'better' mean?".to_string(),
        "Better by what metric?".to_string(),
        // ... 6 more questions
    ],
    coordination_hours: 4.0,  // 8 questions × 30 min
    severity: DefectSeverity::High,
};
```

**Defect Weight**: Clarification rounds / 4.0 (capped at 1.0)

---

## Scoring Algorithms

### Bayesian Score Update

```rust
use rand_distr::{Beta, Distribution};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BayesianScorer {
    // Beta distribution parameters
    pub alpha: f64,  // Successes (well-formed packets)
    pub beta: f64,   // Failures (defective packets)
}

impl BayesianScorer {
    pub fn new() -> Self {
        // Neutral prior: Beta(1, 1) = Uniform(0, 1)
        Self {
            alpha: 1.0,
            beta: 1.0,
        }
    }

    pub fn update(&mut self, packet_valid: bool, defect_weight: f64) {
        if packet_valid {
            // No defects: increase alpha
            self.alpha += 1.0;
        } else {
            // Defects found: increase beta weighted by severity
            self.beta += defect_weight;
        }
    }

    pub fn score(&self) -> f64 {
        // Expected value of Beta(alpha, beta)
        // E[X] = alpha / (alpha + beta)
        let expected_value = self.alpha / (self.alpha + self.beta);

        // Scale to 0-100
        expected_value * 100.0
    }

    pub fn confidence(&self) -> f64 {
        // Higher alpha + beta = more data = higher confidence
        let n = self.alpha + self.beta;
        1.0 - (-0.1 * n).exp()
    }

    pub fn defect_rate(&self) -> f64 {
        // Probability of defect
        self.beta / (self.alpha + self.beta)
    }

    pub fn sample_distribution(&self) -> Vec<f64> {
        // Sample from Beta distribution for visualization
        let beta_dist = Beta::new(self.alpha, self.beta).unwrap();
        let mut rng = rand::thread_rng();

        (0..1000)
            .map(|_| beta_dist.sample(&mut rng) * 100.0)
            .collect()
    }
}

impl Default for BayesianScorer {
    fn default() -> Self {
        Self::new()
    }
}
```

### Score Computation with Defect Weighting

```rust
pub struct SupplierScoreCalculator;

impl SupplierScoreCalculator {
    pub fn calculate_score(
        supplier_id: &SupplierId,
        packets: &[WorkPacket],
        defects: &[Defect],
    ) -> SupplierQualityScore {
        let mut scorer = BayesianScorer::new();

        for packet in packets {
            let packet_defects: Vec<_> = defects
                .iter()
                .filter(|d| d.packet_id == packet.id)
                .collect();

            if packet_defects.is_empty() {
                // Perfect packet
                scorer.update(true, 0.0);
            } else {
                // Calculate total defect weight
                let total_weight = packet_defects
                    .iter()
                    .map(|d| d.weight())
                    .sum::<f64>();

                scorer.update(false, total_weight);
            }
        }

        let score = scorer.score();
        let defect_rate = scorer.defect_rate();

        SupplierQualityScore {
            supplier_id: supplier_id.clone(),
            score,
            defect_rate,
            reputation: Self::score_to_reputation(score),
            rate_limit: Self::score_to_rate_limit(score, defect_rate),
            total_packets_submitted: packets.len() as u64,
            total_defects: defects.len() as u64,
            last_updated: Utc::now(),
        }
    }

    fn score_to_reputation(score: f64) -> SupplierReputation {
        match score {
            s if s >= 90.0 => SupplierReputation::Trusted,
            s if s >= 70.0 => SupplierReputation::Verified,
            s if s >= 50.0 => SupplierReputation::Provisional,
            s if s >= 30.0 => SupplierReputation::Restricted,
            _ => SupplierReputation::Blocked,
        }
    }

    fn score_to_rate_limit(score: f64, defect_rate: f64) -> RateLimit {
        match score {
            s if s >= 90.0 => RateLimit::none(),
            s if s >= 70.0 => RateLimit::light(defect_rate),
            s if s >= 50.0 => RateLimit::moderate(defect_rate),
            s if s >= 30.0 => RateLimit::heavy(defect_rate),
            _ => RateLimit::blocked(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Defect {
    pub packet_id: WorkPacketId,
    pub defect_type: DefectType,
    pub detected_at: DateTime<Utc>,
}

impl Defect {
    pub fn weight(&self) -> f64 {
        match &self.defect_type {
            DefectType::IncompletePacket(d) => d.severity.weight(),
            DefectType::RequirementChurn(d) => {
                // Churn weight: min(changes / 4, 1.0)
                (d.change_requests.len() as f64 / 4.0).min(1.0)
            }
            DefectType::UrgencyInflation(d) => {
                // Inflation weight: inflation_ratio / 3
                (d.inflation_ratio / 3.0).min(1.0)
            }
            DefectType::CoordinationDumping(d) => {
                // Coordination weight: rounds / 4
                (d.clarification_rounds as f64 / 4.0).min(1.0)
            }
        }
    }
}
```

---

## Rate Limiting Implementation

### Rate Limit Strategies

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimit {
    pub strategy: RateLimitStrategy,
    pub max_packets_per_hour: u32,
    pub max_packets_per_day: u32,
    pub max_concurrent_packets: u32,
    pub cooldown_period: Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RateLimitStrategy {
    None,                          // No limits (trusted supplier)
    TokenBucket(TokenBucketConfig), // Standard rate limiting
    LeakyBucket(LeakyBucketConfig), // Smooth rate limiting
    FixedWindow(FixedWindowConfig), // Simple windowed limiting
    Blocked,                        // No packets accepted
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenBucketConfig {
    pub capacity: u32,      // Max tokens
    pub refill_rate: f64,   // Tokens per second
    pub current_tokens: f64,
    pub last_refill: DateTime<Utc>,
}

impl RateLimit {
    pub fn none() -> Self {
        Self {
            strategy: RateLimitStrategy::None,
            max_packets_per_hour: u32::MAX,
            max_packets_per_day: u32::MAX,
            max_concurrent_packets: u32::MAX,
            cooldown_period: Duration::zero(),
        }
    }

    pub fn light(defect_rate: f64) -> Self {
        // Light rate limiting for Verified suppliers
        Self {
            strategy: RateLimitStrategy::TokenBucket(TokenBucketConfig {
                capacity: 20,
                refill_rate: 5.0,  // 5 tokens/second
                current_tokens: 20.0,
                last_refill: Utc::now(),
            }),
            max_packets_per_hour: 20,
            max_packets_per_day: 100,
            max_concurrent_packets: 5,
            cooldown_period: Duration::seconds(12),  // 12s between packets
        }
    }

    pub fn moderate(defect_rate: f64) -> Self {
        // Moderate rate limiting for Provisional suppliers
        Self {
            strategy: RateLimitStrategy::TokenBucket(TokenBucketConfig {
                capacity: 10,
                refill_rate: 2.0,  // 2 tokens/second
                current_tokens: 10.0,
                last_refill: Utc::now(),
            }),
            max_packets_per_hour: 10,
            max_packets_per_day: 50,
            max_concurrent_packets: 3,
            cooldown_period: Duration::seconds(30),
        }
    }

    pub fn heavy(defect_rate: f64) -> Self {
        // Heavy rate limiting for Restricted suppliers
        Self {
            strategy: RateLimitStrategy::TokenBucket(TokenBucketConfig {
                capacity: 3,
                refill_rate: 0.5,  // 0.5 tokens/second = 1 every 2 seconds
                current_tokens: 3.0,
                last_refill: Utc::now(),
            }),
            max_packets_per_hour: 3,
            max_packets_per_day: 10,
            max_concurrent_packets: 1,
            cooldown_period: Duration::minutes(2),
        }
    }

    pub fn blocked() -> Self {
        Self {
            strategy: RateLimitStrategy::Blocked,
            max_packets_per_hour: 0,
            max_packets_per_day: 0,
            max_concurrent_packets: 0,
            cooldown_period: Duration::max_value(),
        }
    }
}
```

### Rate Limit Enforcement

```rust
pub struct RateLimiter {
    supplier_states: HashMap<SupplierId, SupplierState>,
}

#[derive(Debug, Clone)]
struct SupplierState {
    last_packet_time: DateTime<Utc>,
    packets_this_hour: u32,
    packets_this_day: u32,
    concurrent_packets: u32,
    tokens: f64,
    last_refill: DateTime<Utc>,
}

impl RateLimiter {
    pub fn new() -> Self {
        Self {
            supplier_states: HashMap::new(),
        }
    }

    pub fn can_submit(
        &mut self,
        supplier_id: &SupplierId,
        rate_limit: &RateLimit,
    ) -> Result<(), RateLimitViolation> {
        match &rate_limit.strategy {
            RateLimitStrategy::None => Ok(()),
            RateLimitStrategy::Blocked => {
                Err(RateLimitViolation::SupplierBlocked {
                    supplier_id: supplier_id.clone(),
                })
            }
            RateLimitStrategy::TokenBucket(config) => {
                self.check_token_bucket(supplier_id, rate_limit, config)
            }
            _ => todo!("Other rate limit strategies"),
        }
    }

    fn check_token_bucket(
        &mut self,
        supplier_id: &SupplierId,
        rate_limit: &RateLimit,
        config: &TokenBucketConfig,
    ) -> Result<(), RateLimitViolation> {
        let now = Utc::now();

        let state = self.supplier_states
            .entry(supplier_id.clone())
            .or_insert_with(|| SupplierState {
                last_packet_time: now,
                packets_this_hour: 0,
                packets_this_day: 0,
                concurrent_packets: 0,
                tokens: config.capacity as f64,
                last_refill: now,
            });

        // Refill tokens based on time elapsed
        let elapsed = (now - state.last_refill).num_milliseconds() as f64 / 1000.0;
        let tokens_to_add = elapsed * config.refill_rate;
        state.tokens = (state.tokens + tokens_to_add).min(config.capacity as f64);
        state.last_refill = now;

        // Check if we have tokens available
        if state.tokens < 1.0 {
            return Err(RateLimitViolation::RateLimitExceeded {
                supplier_id: supplier_id.clone(),
                retry_after: Duration::seconds(
                    ((1.0 - state.tokens) / config.refill_rate) as i64
                ),
            });
        }

        // Check hourly limit
        if (now - state.last_packet_time).num_hours() >= 1 {
            state.packets_this_hour = 0;
        }
        if state.packets_this_hour >= rate_limit.max_packets_per_hour {
            return Err(RateLimitViolation::HourlyLimitExceeded {
                supplier_id: supplier_id.clone(),
                limit: rate_limit.max_packets_per_hour,
            });
        }

        // Check daily limit
        if (now - state.last_packet_time).num_days() >= 1 {
            state.packets_this_day = 0;
        }
        if state.packets_this_day >= rate_limit.max_packets_per_day {
            return Err(RateLimitViolation::DailyLimitExceeded {
                supplier_id: supplier_id.clone(),
                limit: rate_limit.max_packets_per_day,
            });
        }

        // Check concurrent limit
        if state.concurrent_packets >= rate_limit.max_concurrent_packets {
            return Err(RateLimitViolation::ConcurrentLimitExceeded {
                supplier_id: supplier_id.clone(),
                limit: rate_limit.max_concurrent_packets,
            });
        }

        // All checks passed - consume token
        state.tokens -= 1.0;
        state.packets_this_hour += 1;
        state.packets_this_day += 1;
        state.concurrent_packets += 1;
        state.last_packet_time = now;

        Ok(())
    }

    pub fn packet_completed(&mut self, supplier_id: &SupplierId) {
        if let Some(state) = self.supplier_states.get_mut(supplier_id) {
            state.concurrent_packets = state.concurrent_packets.saturating_sub(1);
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum RateLimitViolation {
    #[error("Supplier {supplier_id:?} is blocked")]
    SupplierBlocked { supplier_id: SupplierId },

    #[error("Rate limit exceeded for {supplier_id:?}, retry after {retry_after}")]
    RateLimitExceeded {
        supplier_id: SupplierId,
        retry_after: Duration,
    },

    #[error("Hourly limit ({limit}) exceeded for {supplier_id:?}")]
    HourlyLimitExceeded {
        supplier_id: SupplierId,
        limit: u32,
    },

    #[error("Daily limit ({limit}) exceeded for {supplier_id:?}")]
    DailyLimitExceeded {
        supplier_id: SupplierId,
        limit: u32,
    },

    #[error("Concurrent limit ({limit}) exceeded for {supplier_id:?}")]
    ConcurrentLimitExceeded {
        supplier_id: SupplierId,
        limit: u32,
    },
}
```

---

## Automated Quality Gates

### Quality Gate Implementation

```rust
pub struct QualityGate {
    validators: Vec<Box<dyn PacketValidator>>,
    defect_detectors: Vec<Box<dyn DefectDetector>>,
    rate_limiter: RateLimiter,
    supplier_scores: HashMap<SupplierId, SupplierQualityScore>,
}

pub trait PacketValidator: Send + Sync {
    fn validate(&self, packet: &WorkPacket) -> Result<ValidationReport, ValidationError>;
}

pub trait DefectDetector: Send + Sync {
    fn detect(&self, packet: &WorkPacket) -> Vec<Defect>;
}

impl QualityGate {
    pub fn new() -> Self {
        Self {
            validators: vec![
                Box::new(StructuralValidator),
                Box::new(SemanticValidator),
                Box::new(DependencyValidator),
            ],
            defect_detectors: vec![
                Box::new(IncompletePacketDetector),
                Box::new(ChurnDetector),
                Box::new(UrgencyInflationDetector),
                Box::new(CoordinationDumpingDetector),
            ],
            rate_limiter: RateLimiter::new(),
            supplier_scores: HashMap::new(),
        }
    }

    pub fn admit_packet(
        &mut self,
        packet: WorkPacket,
    ) -> Result<AdmissionReceipt, AdmissionRejection> {
        let supplier_id = SupplierId::from_owner(&packet.owner);

        // Step 1: Check rate limits
        let supplier_score = self.supplier_scores
            .get(&supplier_id)
            .cloned()
            .unwrap_or_else(|| Self::default_score(&supplier_id));

        self.rate_limiter
            .can_submit(&supplier_id, &supplier_score.rate_limit)
            .map_err(|e| AdmissionRejection::RateLimitViolation(e))?;

        // Step 2: Validate packet structure
        let mut all_errors = Vec::new();
        let mut all_warnings = Vec::new();

        for validator in &self.validators {
            match validator.validate(&packet) {
                Ok(report) => {
                    all_errors.extend(report.errors);
                    all_warnings.extend(report.warnings);
                }
                Err(e) => {
                    return Err(AdmissionRejection::ValidationFailed(e));
                }
            }
        }

        // Step 3: Detect defects
        let mut defects = Vec::new();
        for detector in &self.defect_detectors {
            defects.extend(detector.detect(&packet));
        }

        // Step 4: Calculate defect weight
        let total_defect_weight: f64 = defects.iter().map(|d| d.weight()).sum();

        // Step 5: Update supplier score
        self.update_supplier_score(
            &supplier_id,
            &packet,
            &defects,
            total_defect_weight,
        );

        // Step 6: Decision
        if !all_errors.is_empty() || total_defect_weight >= 1.0 {
            // Critical defects: reject
            Err(AdmissionRejection::DefectsDetected {
                packet_id: packet.id.clone(),
                defects,
                defect_weight: total_defect_weight,
                supplier_score: self.supplier_scores[&supplier_id].clone(),
            })
        } else if total_defect_weight > 0.0 {
            // Minor defects: accept with warnings
            Ok(AdmissionReceipt {
                packet_id: packet.id.clone(),
                admitted_at: Utc::now(),
                supplier_score: self.supplier_scores[&supplier_id].clone(),
                defects_detected: defects,
                defect_weight: total_defect_weight,
                warnings: all_warnings,
                status: AdmissionStatus::AcceptedWithWarnings,
            })
        } else {
            // Perfect packet: accept
            Ok(AdmissionReceipt {
                packet_id: packet.id.clone(),
                admitted_at: Utc::now(),
                supplier_score: self.supplier_scores[&supplier_id].clone(),
                defects_detected: vec![],
                defect_weight: 0.0,
                warnings: vec![],
                status: AdmissionStatus::Accepted,
            })
        }
    }

    fn update_supplier_score(
        &mut self,
        supplier_id: &SupplierId,
        packet: &WorkPacket,
        defects: &[Defect],
        defect_weight: f64,
    ) {
        let score = self.supplier_scores
            .entry(supplier_id.clone())
            .or_insert_with(|| Self::default_score(supplier_id));

        // Update Bayesian scorer
        let packet_valid = defect_weight < 0.5;  // Threshold for "valid"

        let mut scorer = BayesianScorer {
            alpha: score.score,  // Reuse previous score as alpha
            beta: 100.0 - score.score,  // Complement as beta
        };

        scorer.update(packet_valid, defect_weight);

        // Update score
        score.score = scorer.score();
        score.defect_rate = scorer.defect_rate();
        score.reputation = SupplierScoreCalculator::score_to_reputation(score.score);
        score.rate_limit = SupplierScoreCalculator::score_to_rate_limit(
            score.score,
            score.defect_rate,
        );
        score.total_packets_submitted += 1;
        score.total_defects += defects.len() as u64;
        score.last_updated = Utc::now();
    }

    fn default_score(supplier_id: &SupplierId) -> SupplierQualityScore {
        SupplierQualityScore {
            supplier_id: supplier_id.clone(),
            score: 50.0,  // Neutral starting score
            defect_rate: 0.5,
            reputation: SupplierReputation::Provisional,
            rate_limit: RateLimit::moderate(0.5),
            total_packets_submitted: 0,
            total_defects: 0,
            last_updated: Utc::now(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdmissionReceipt {
    pub packet_id: WorkPacketId,
    pub admitted_at: DateTime<Utc>,
    pub supplier_score: SupplierQualityScore,
    pub defects_detected: Vec<Defect>,
    pub defect_weight: f64,
    pub warnings: Vec<ValidationWarning>,
    pub status: AdmissionStatus,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AdmissionStatus {
    Accepted,
    AcceptedWithWarnings,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum AdmissionRejection {
    #[error("Rate limit violation: {0}")]
    RateLimitViolation(#[from] RateLimitViolation),

    #[error("Validation failed: {0}")]
    ValidationFailed(#[from] ValidationError),

    #[error("Defects detected in packet {packet_id:?}: {defects:?}")]
    DefectsDetected {
        packet_id: WorkPacketId,
        defects: Vec<Defect>,
        defect_weight: f64,
        supplier_score: SupplierQualityScore,
    },
}
```

---

## Supplier Feedback Loops

### Feedback Report Generation

```rust
pub struct FeedbackGenerator;

impl FeedbackGenerator {
    pub fn generate_feedback(
        supplier_id: &SupplierId,
        score: &SupplierQualityScore,
        recent_defects: &[Defect],
    ) -> SupplierFeedbackReport {
        let defect_summary = Self::summarize_defects(recent_defects);
        let improvement_suggestions = Self::generate_suggestions(score, recent_defects);
        let quality_trend = Self::calculate_trend(supplier_id);

        SupplierFeedbackReport {
            supplier_id: supplier_id.clone(),
            current_score: score.score,
            reputation: score.reputation,
            defect_rate: score.defect_rate,
            rate_limit_status: score.rate_limit.clone(),
            defect_summary,
            improvement_suggestions,
            quality_trend,
            generated_at: Utc::now(),
        }
    }

    fn summarize_defects(defects: &[Defect]) -> DefectSummary {
        let mut incomplete_count = 0;
        let mut churn_count = 0;
        let mut urgency_inflation_count = 0;
        let mut coordination_dumping_count = 0;

        for defect in defects {
            match &defect.defect_type {
                DefectType::IncompletePacket(_) => incomplete_count += 1,
                DefectType::RequirementChurn(_) => churn_count += 1,
                DefectType::UrgencyInflation(_) => urgency_inflation_count += 1,
                DefectType::CoordinationDumping(_) => coordination_dumping_count += 1,
            }
        }

        DefectSummary {
            total_defects: defects.len(),
            incomplete_packets: incomplete_count,
            requirement_churn: churn_count,
            urgency_inflation: urgency_inflation_count,
            coordination_dumping: coordination_dumping_count,
        }
    }

    fn generate_suggestions(
        score: &SupplierQualityScore,
        defects: &[Defect],
    ) -> Vec<ImprovementSuggestion> {
        let mut suggestions = Vec::new();

        // Analyze defect patterns
        let defect_counts = Self::count_defect_types(defects);

        if defect_counts.incomplete > defects.len() / 4 {
            suggestions.push(ImprovementSuggestion {
                priority: SuggestionPriority::High,
                category: DefectCategory::IncompletePackets,
                suggestion: "Complete all required fields before submission. \
                            Use WorkPacketBuilder to ensure no fields are missing.".to_string(),
                example_fix: Some("let packet = WorkPacketBuilder::new(id)\
                    .objective(\"Clear objective\")\
                    .add_success_criterion(...)\
                    .acceptance_test(...)\
                    .build()?;".to_string()),
            });
        }

        if defect_counts.churn > defects.len() / 4 {
            suggestions.push(ImprovementSuggestion {
                priority: SuggestionPriority::High,
                category: DefectCategory::RequirementChurn,
                suggestion: "Stabilize requirements before submission. \
                            Create a draft specification and review internally \
                            before submitting to downstream.".to_string(),
                example_fix: Some("Draft spec → Internal review → Finalize → Submit".to_string()),
            });
        }

        if defect_counts.urgency_inflation > defects.len() / 4 {
            suggestions.push(ImprovementSuggestion {
                priority: SuggestionPriority::Medium,
                category: DefectCategory::UrgencyInflation,
                suggestion: "Use urgency markers only for genuine emergencies. \
                            'Urgent' should mean production down, not 'I want this soon'.".to_string(),
                example_fix: Some("High urgency = production impact. \
                                  Medium urgency = deadline within 48h. \
                                  Low urgency = normal priority.".to_string()),
            });
        }

        if defect_counts.coordination_dumping > defects.len() / 4 {
            suggestions.push(ImprovementSuggestion {
                priority: SuggestionPriority::High,
                category: DefectCategory::CoordinationDumping,
                suggestion: "Provide complete specifications. Answer these questions \
                            in the packet: What? Why? How to verify? How to undo?".to_string(),
                example_fix: Some("Use the 4 W's: What (objective), Why (constraints), \
                                  When (deadline), hoW (acceptance test)".to_string()),
            });
        }

        suggestions
    }

    fn count_defect_types(defects: &[Defect]) -> DefectCounts {
        let mut counts = DefectCounts::default();

        for defect in defects {
            match &defect.defect_type {
                DefectType::IncompletePacket(_) => counts.incomplete += 1,
                DefectType::RequirementChurn(_) => counts.churn += 1,
                DefectType::UrgencyInflation(_) => counts.urgency_inflation += 1,
                DefectType::CoordinationDumping(_) => counts.coordination_dumping += 1,
            }
        }

        counts
    }

    fn calculate_trend(supplier_id: &SupplierId) -> QualityTrend {
        // Calculate score trend over last 30 days
        // Simplified for example
        QualityTrend::Improving
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplierFeedbackReport {
    pub supplier_id: SupplierId,
    pub current_score: f64,
    pub reputation: SupplierReputation,
    pub defect_rate: f64,
    pub rate_limit_status: RateLimit,
    pub defect_summary: DefectSummary,
    pub improvement_suggestions: Vec<ImprovementSuggestion>,
    pub quality_trend: QualityTrend,
    pub generated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectSummary {
    pub total_defects: usize,
    pub incomplete_packets: usize,
    pub requirement_churn: usize,
    pub urgency_inflation: usize,
    pub coordination_dumping: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementSuggestion {
    pub priority: SuggestionPriority,
    pub category: DefectCategory,
    pub suggestion: String,
    pub example_fix: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SuggestionPriority {
    High,
    Medium,
    Low,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefectCategory {
    IncompletePackets,
    RequirementChurn,
    UrgencyInflation,
    CoordinationDumping,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum QualityTrend {
    Improving,
    Stable,
    Declining,
}

#[derive(Debug, Default)]
struct DefectCounts {
    incomplete: usize,
    churn: usize,
    urgency_inflation: usize,
    coordination_dumping: usize,
}
```

---

## Upstream Pays Principle

### Cost Accounting

```rust
pub struct CoordinationCostAccounting {
    costs_by_supplier: HashMap<SupplierId, AccumulatedCosts>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AccumulatedCosts {
    pub total_coordination_hours: f64,
    pub total_cost_usd: f64,
    pub clarification_rounds: u32,
    pub rework_cycles: u32,
    pub validation_failures: u32,
    pub packets_processed: u32,
}

impl CoordinationCostAccounting {
    pub fn new() -> Self {
        Self {
            costs_by_supplier: HashMap::new(),
        }
    }

    pub fn charge_defect_cost(
        &mut self,
        supplier_id: &SupplierId,
        defect: &Defect,
    ) {
        let cost_hours = match &defect.defect_type {
            DefectType::IncompletePacket(d) => {
                // Time to identify missing fields and request clarification
                0.5 * d.missing_fields.len() as f64
            }
            DefectType::RequirementChurn(d) => {
                // Direct waste measurement
                d.wasted_effort_hours
            }
            DefectType::UrgencyInflation(d) => {
                // Cost of context switching to "urgent" work
                0.25 * d.inflation_ratio
            }
            DefectType::CoordinationDumping(d) => {
                // Direct coordination cost
                d.coordination_hours
            }
        };

        let cost_usd = cost_hours * 200.0;  // $200/hour

        let costs = self.costs_by_supplier
            .entry(supplier_id.clone())
            .or_default();

        costs.total_coordination_hours += cost_hours;
        costs.total_cost_usd += cost_usd;
        costs.packets_processed += 1;

        match &defect.defect_type {
            DefectType::IncompletePacket(_) => costs.validation_failures += 1,
            DefectType::RequirementChurn(_) => costs.rework_cycles += 1,
            DefectType::CoordinationDumping(_) => costs.clarification_rounds += 1,
            _ => {}
        }
    }

    pub fn generate_invoice(
        &self,
        supplier_id: &SupplierId,
        period_start: DateTime<Utc>,
        period_end: DateTime<Utc>,
    ) -> CoordinationInvoice {
        let costs = self.costs_by_supplier
            .get(supplier_id)
            .cloned()
            .unwrap_or_default();

        CoordinationInvoice {
            supplier_id: supplier_id.clone(),
            period_start,
            period_end,
            line_items: vec![
                InvoiceLineItem {
                    description: format!("{} clarification rounds", costs.clarification_rounds),
                    hours: costs.clarification_rounds as f64 * 0.5,
                    rate_usd: 200.0,
                    total_usd: costs.clarification_rounds as f64 * 0.5 * 200.0,
                },
                InvoiceLineItem {
                    description: format!("{} rework cycles", costs.rework_cycles),
                    hours: costs.rework_cycles as f64 * 2.0,
                    rate_usd: 200.0,
                    total_usd: costs.rework_cycles as f64 * 2.0 * 200.0,
                },
                InvoiceLineItem {
                    description: format!("{} validation failures", costs.validation_failures),
                    hours: costs.validation_failures as f64 * 0.25,
                    rate_usd: 200.0,
                    total_usd: costs.validation_failures as f64 * 0.25 * 200.0,
                },
            ],
            total_hours: costs.total_coordination_hours,
            total_cost_usd: costs.total_cost_usd,
            generated_at: Utc::now(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoordinationInvoice {
    pub supplier_id: SupplierId,
    pub period_start: DateTime<Utc>,
    pub period_end: DateTime<Utc>,
    pub line_items: Vec<InvoiceLineItem>,
    pub total_hours: f64,
    pub total_cost_usd: f64,
    pub generated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvoiceLineItem {
    pub description: String,
    pub hours: f64,
    pub rate_usd: f64,
    pub total_usd: f64,
}

impl CoordinationInvoice {
    pub fn format_markdown(&self) -> String {
        format!(
            r#"# Coordination Cost Invoice

**Supplier**: {:?}
**Period**: {} to {}
**Generated**: {}

## Line Items

{}

## Summary

**Total Hours**: {:.2}
**Total Cost**: ${:.2}

---

*Note: These costs represent coordination overhead imposed on downstream systems.
Improve packet quality to reduce these costs.*
"#,
            self.supplier_id,
            self.period_start.format("%Y-%m-%d"),
            self.period_end.format("%Y-%m-%d"),
            self.generated_at.format("%Y-%m-%d %H:%M:%S UTC"),
            self.line_items.iter()
                .map(|item| format!(
                    "- {} ({:.2}h × ${:.2}/h = ${:.2})",
                    item.description,
                    item.hours,
                    item.rate_usd,
                    item.total_usd
                ))
                .collect::<Vec<_>>()
                .join("\n"),
            self.total_hours,
            self.total_cost_usd
        )
    }
}
```

---

## Real-World Examples

### Example 1: Human Supplier (Product Manager)

```rust
// Scenario: Product manager submits feature requests

let pm_supplier = SupplierId::new("pm-alice");

// Week 1: High-quality packet
let packet_1 = WorkPacket {
    id: WorkPacketId::new("WP", 600),
    objective: Objective {
        description: "Add export to CSV functionality for user reports".to_string(),
        success_criteria: vec![
            SuccessCriterion {
                metric: "export_completion_time".to_string(),
                operator: ComparisonOperator::LessThan,
                threshold: 5.0,
                unit: "seconds".to_string(),
            },
        ],
    },
    // ... complete specification
};

// Result: No defects detected
// Score: 50.0 → 65.0 (improving)
// Rate limit: Provisional → Verified

// Week 2: Defective packet (incomplete)
let packet_2 = WorkPacket {
    id: WorkPacketId::new("WP", 601),
    objective: Objective {
        description: "Make dashboards better".to_string(),  // Vague
        success_criteria: vec![],  // Missing
    },
    // ... missing acceptance test
};

// Result: Coordination dumping defect detected
// Defect weight: 0.75 (High severity)
// Score: 65.0 → 58.0 (declining)
// Rate limit: Verified → Provisional (rate limited)
// Invoice: $300 coordination cost

// Week 3: Churn packet
let packet_3_v1 = WorkPacket {
    objective: Objective {
        description: "Add OAuth2 authentication".to_string(),
    },
    // ...
};

// 2 days later: Changed requirements
let packet_3_v2 = WorkPacket {
    objective: Objective {
        description: "Add simple username/password authentication".to_string(),
    },
    // ...
};

// Result: Requirement churn defect
// Defect weight: 1.0 (Critical - wasted 12 hours)
// Score: 58.0 → 45.0 (declining rapidly)
// Rate limit: Provisional → Restricted
// Invoice: $2,400 coordination cost
// Feedback: "Stabilize requirements before submission"
```

### Example 2: AI Agent Supplier

```rust
// Scenario: AI agent generates work packets automatically

let agent_supplier = SupplierId::new("agent-codegen-v1");

// Initial packets: High defect rate (agent learning)
for i in 0..10 {
    let packet = generate_packet_v1();  // Buggy generator
    // Defects: Missing acceptance tests, vague objectives
    // Average defect weight: 0.8
}

// Result after 10 packets:
// Score: 50.0 → 25.0 (blocked)
// Rate limit: Blocked (no new packets accepted)
// Feedback: "40% of packets missing acceptance tests"

// Improvement: Fix generator based on feedback
for i in 10..20 {
    let packet = generate_packet_v2();  // Improved generator
    // Defects: Mostly complete, occasional coordination dumping
    // Average defect weight: 0.3
}

// Result after 20 packets:
// Score: 25.0 → 42.0 (recovering)
// Rate limit: Restricted → Provisional
// Invoice: $1,200 → $400 (costs declining)

// Further improvement
for i in 20..50 {
    let packet = generate_packet_v3();  // Production-ready generator
    // Defects: Rare, mostly minor
    // Average defect weight: 0.1
}

// Result after 50 packets:
// Score: 42.0 → 78.0 (trusted)
// Rate limit: Provisional → Verified → Trusted
// Invoice: $400 → $100 → $20
// Outcome: Agent learns to generate high-quality packets
```

### Example 3: Team Supplier Comparison

```rust
// Team A: Well-disciplined team
let team_a = SupplierId::new("team-backend");

// Stats over 3 months:
// - 50 packets submitted
// - 3 defects detected (6% defect rate)
// - Total coordination cost: $600
// - Average defect weight: 0.12

// Score: 92.0 (Trusted)
// Rate limit: None
// Reputation: Trusted supplier

// Team B: Undisciplined team
let team_b = SupplierId::new("team-frontend");

// Stats over 3 months:
// - 50 packets submitted
// - 25 defects detected (50% defect rate)
// - Total coordination cost: $5,000
// - Average defect weight: 0.7

// Score: 38.0 (Restricted)
// Rate limit: Heavy (3/hour, 10/day)
// Reputation: Restricted supplier
// Invoice: $5,000
// Feedback: "Improve packet completeness and reduce churn"

// Outcome: Team B receives feedback and training
// After improvement:
// - Defect rate: 50% → 15%
// - Score: 38.0 → 68.0
// - Rate limit: Restricted → Verified
// - Coordination cost: $5,000 → $1,200
```

---

## Integration with ggen

### ggen Quality Gate CLI

```rust
// src/cli/quality_gate.rs

use clap::{Args, Subcommand};

#[derive(Debug, Args)]
pub struct QualityGateArgs {
    #[command(subcommand)]
    pub command: QualityGateCommand,
}

#[derive(Debug, Subcommand)]
pub enum QualityGateCommand {
    /// Check supplier quality score
    Check {
        #[arg(long)]
        supplier_id: String,
    },

    /// Submit work packet for admission
    Submit {
        #[arg(long)]
        packet_file: PathBuf,
    },

    /// Generate supplier feedback report
    Feedback {
        #[arg(long)]
        supplier_id: String,

        #[arg(long, default_value = "30")]
        days: u32,
    },

    /// Generate coordination cost invoice
    Invoice {
        #[arg(long)]
        supplier_id: String,

        #[arg(long)]
        start_date: String,

        #[arg(long)]
        end_date: String,
    },

    /// List all suppliers with scores
    List {
        #[arg(long)]
        min_score: Option<f64>,

        #[arg(long)]
        reputation: Option<String>,
    },
}

impl QualityGateCommand {
    pub async fn execute(&self) -> Result<()> {
        match self {
            Self::Check { supplier_id } => {
                self.check_supplier(supplier_id).await
            }
            Self::Submit { packet_file } => {
                self.submit_packet(packet_file).await
            }
            Self::Feedback { supplier_id, days } => {
                self.generate_feedback(supplier_id, *days).await
            }
            Self::Invoice { supplier_id, start_date, end_date } => {
                self.generate_invoice(supplier_id, start_date, end_date).await
            }
            Self::List { min_score, reputation } => {
                self.list_suppliers(min_score, reputation).await
            }
        }
    }

    async fn check_supplier(&self, supplier_id: &str) -> Result<()> {
        let gate = QualityGate::new();
        let sid = SupplierId::new(supplier_id);

        if let Some(score) = gate.get_supplier_score(&sid) {
            println!("Supplier Quality Report");
            println!("======================");
            println!("Supplier ID: {:?}", score.supplier_id);
            println!("Score: {:.2}/100", score.score);
            println!("Reputation: {:?}", score.reputation);
            println!("Defect Rate: {:.1}%", score.defect_rate * 100.0);
            println!("Packets Submitted: {}", score.total_packets_submitted);
            println!("Total Defects: {}", score.total_defects);
            println!("\nRate Limits:");
            println!("  Max/hour: {}", score.rate_limit.max_packets_per_hour);
            println!("  Max/day: {}", score.rate_limit.max_packets_per_day);
            println!("  Max concurrent: {}", score.rate_limit.max_concurrent_packets);
        } else {
            println!("No score found for supplier: {}", supplier_id);
        }

        Ok(())
    }

    async fn submit_packet(&self, packet_file: &Path) -> Result<()> {
        let packet: WorkPacket = serde_json::from_str(
            &fs::read_to_string(packet_file)?
        )?;

        let mut gate = QualityGate::new();

        match gate.admit_packet(packet) {
            Ok(receipt) => {
                println!("✓ Packet admitted");
                println!("Status: {:?}", receipt.status);
                println!("Supplier score: {:.2}", receipt.supplier_score.score);

                if !receipt.defects_detected.is_empty() {
                    println!("\n⚠ Defects detected:");
                    for defect in &receipt.defects_detected {
                        println!("  - {:?} (weight: {:.2})", defect.defect_type, defect.weight());
                    }
                }

                if !receipt.warnings.is_empty() {
                    println!("\n⚠ Warnings:");
                    for warning in &receipt.warnings {
                        println!("  - {:?}", warning);
                    }
                }
            }
            Err(e) => {
                println!("✗ Packet rejected");
                println!("Reason: {:?}", e);
                return Err(e.into());
            }
        }

        Ok(())
    }
}
```

### Usage Examples

```bash
# Check supplier quality score
$ ggen quality-gate check --supplier-id pm-alice
Supplier Quality Report
======================
Supplier ID: "pm-alice"
Score: 72.50/100
Reputation: Verified
Defect Rate: 18.2%
Packets Submitted: 44
Total Defects: 8

Rate Limits:
  Max/hour: 20
  Max/day: 100
  Max concurrent: 5

# Submit work packet
$ ggen quality-gate submit --packet-file packet.json
✓ Packet admitted
Status: Accepted
Supplier score: 85.30

# Generate feedback report
$ ggen quality-gate feedback --supplier-id pm-alice --days 30
Supplier Feedback Report
========================
Period: Last 30 days
Current Score: 72.50
Trend: Improving ↑

Defect Summary:
- Total defects: 8
- Incomplete packets: 3 (37%)
- Requirement churn: 2 (25%)
- Urgency inflation: 1 (13%)
- Coordination dumping: 2 (25%)

Improvement Suggestions:
1. [HIGH] Complete all required fields before submission
   Example: Use WorkPacketBuilder to ensure completeness

2. [MEDIUM] Reduce requirement changes after submission
   Suggestion: Review spec internally before submitting

# Generate cost invoice
$ ggen quality-gate invoice \
    --supplier-id pm-alice \
    --start-date 2026-01-01 \
    --end-date 2026-01-31

Coordination Cost Invoice
=========================
Supplier: "pm-alice"
Period: 2026-01-01 to 2026-01-31

Line Items:
- 6 clarification rounds (3.00h × $200/h = $600.00)
- 2 rework cycles (4.00h × $200/h = $800.00)
- 5 validation failures (1.25h × $200/h = $250.00)

Summary:
Total Hours: 8.25
Total Cost: $1,650.00

# List all suppliers
$ ggen quality-gate list --min-score 70
Suppliers (score ≥ 70):
1. team-backend (score: 92.0, reputation: Trusted)
2. pm-bob (score: 81.5, reputation: Verified)
3. pm-alice (score: 72.5, reputation: Verified)

$ ggen quality-gate list --reputation Restricted
Restricted Suppliers:
1. team-frontend (score: 38.0, defect rate: 52%)
2. agent-v1 (score: 42.0, defect rate: 45%)
```

---

## Summary

### Key Principles

1. **Defects Have Consequences**: Every malformed work packet imposes coordination costs on downstream systems.

2. **Upstream Pays**: The supplier who creates defects pays the coordination cost, not the downstream consumer.

3. **Measurable Quality**: Defect rate, churn, urgency inflation, and coordination dumping are quantifiable.

4. **Bayesian Scoring**: Supplier scores update continuously based on packet quality, with confidence growing over time.

5. **Automatic Rate Limiting**: High-defect suppliers get rate-limited automatically; quality improves, limits relax.

6. **Feedback Loops**: Suppliers receive actionable feedback to improve packet quality.

7. **Manufacturing Discipline**: Like physical manufacturing, defective suppliers cannot flood the system with low-quality work.

### Benefits

- **Reduced Coordination Waste**: High-quality packets reduce clarification rounds and rework.
- **Incentive Alignment**: Suppliers improve quality to avoid rate limiting.
- **System Protection**: Low-quality suppliers cannot overwhelm downstream capacity.
- **Transparency**: Clear metrics and feedback make quality visible.
- **Continuous Improvement**: Feedback loops drive quality improvement over time.

### Integration Points

```rust
// Quality gate integration in ggen pipeline
WorkPacket Submission
         ↓
    Quality Gate
    (rate limit check)
         ↓
    Defect Detection
    (validation + analysis)
         ↓
    Score Update
    (Bayesian update)
         ↓
    Admission Decision
    (accept/reject/warn)
         ↓
    Feedback Generation
    (improvement suggestions)
         ↓
    Cost Accounting
    (invoice generation)
```

---

## Further Reading

- [Packet Discipline](./10-packet-discipline.md) - Work packet structure and validation
- [Regime Split](./01-regime-split.md) - SELECT/DO vs CONSTRUCT paradigms
- [No Moving Parts](./04-no-moving-parts.md) - Deterministic systems design
- [Autonomy vs Automation](./06-autonomy-vs-automation.md) - Agent coordination patterns

---

## References

- ggen Source: `/home/user/ggen/crates/`
- CLAUDE.md: `/home/user/ggen/CLAUDE.md`
- Rules: `/home/user/ggen/.claude/rules/`

---

**Document Status**: Production-Ready
**Last Updated**: 2026-02-09
**Reading Time**: 38 minutes
**Next**: [Agent Coordination Topology](./13-agent-coordination.md)
**Feedback**: [Open an issue](https://github.com/seanchatmangpt/ggen/issues) or [discuss](https://github.com/seanchatmangpt/ggen/discussions)
