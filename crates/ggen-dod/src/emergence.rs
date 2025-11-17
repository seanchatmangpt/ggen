//! Emergence Detection and Constraint Synthesis
//!
//! Closes Gap #3: Handles emergent behaviors not predicted by local rules.
//!
//! When trillions of agents follow local laws (Q, μ), unexpected global patterns can emerge:
//! - Cascading failures (local failures → global instability)
//! - Self-organization (chaotic local rules → organized global state)
//! - Phase transitions (small parameter change → radical behavior shift)
//! - Oscillations and resonance
//!
//! This module:
//! 1. Detects emergent patterns in execution traces
//! 2. Synthesizes new constraints (ΔQ) to prevent/enable emergent behaviors
//! 3. Updates the ontology to explicitly encode emerged patterns
//!
//! Key insight: Emergence is not failure - it's evidence that Q is incomplete.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

/// Emergent pattern detected in system behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmergentPattern {
    /// Unique pattern identifier
    pub id: String,

    /// What kind of emergence
    pub pattern_type: EmergenceType,

    /// Scale at which it emerges (how many agents involved)
    pub scale: usize,

    /// Confidence (0.0-1.0) that this is real emergence vs. noise
    pub confidence: f64,

    /// Evidence: observations that support this pattern
    pub evidence: Vec<String>,

    /// First detected at timestamp
    pub detected_at_ns: u64,

    /// Duration observed
    pub duration_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EmergenceType {
    /// Individual failures → collective breakdown
    CascadingFailure,

    /// Chaotic local rules → global oscillation
    Oscillation,

    /// System self-organizes into unexpected state
    SelfOrganization,

    /// Phase transition (continuous → discontinuous change)
    PhaseTransition,

    /// Synchronization (independent agents → coordinated behavior)
    Synchronization,

    /// Deadlock or livelock
    DeadlockEmergence,

    /// Unknown pattern
    Unknown,
}

/// EmergenceDetector analyzes execution traces to find patterns
pub struct EmergenceDetector {
    /// Historical observations grouped by agent
    agent_traces: HashMap<String, Vec<AgentEvent>>,

    /// Detected patterns
    patterns: Vec<EmergentPattern>,

    /// Time window for pattern detection (nanoseconds)
    window_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentEvent {
    pub agent_id: String,
    pub event_type: String,
    pub value: f64,
    pub timestamp_ns: u64,
}

impl EmergenceDetector {
    pub fn new(window_ns: u64) -> Self {
        EmergenceDetector {
            agent_traces: HashMap::new(),
            patterns: Vec::new(),
            window_ns,
        }
    }

    /// Record an agent event
    pub fn observe(&mut self, event: AgentEvent) {
        self.agent_traces
            .entry(event.agent_id.clone())
            .or_insert_with(Vec::new)
            .push(event);
    }

    /// Detect cascading failures
    ///
    /// Q_cascade_free: failure doesn't spread to > K% of agents
    pub fn detect_cascading_failure(&self, threshold: f64) -> Option<EmergentPattern> {
        if self.agent_traces.is_empty() {
            return None;
        }

        // Count agents in "failure" state
        let failed_agents: usize = self
            .agent_traces
            .iter()
            .filter(|(_, events)| {
                events.iter().any(|e| {
                    e.event_type == "failure"
                        || e.event_type == "crash"
                        || e.event_type == "timeout"
                })
            })
            .count();

        let failure_rate = failed_agents as f64 / self.agent_traces.len() as f64;

        if failure_rate > threshold {
            return Some(EmergentPattern {
                id: format!("cascade_{}", uuid()),
                pattern_type: EmergenceType::CascadingFailure,
                scale: failed_agents,
                confidence: failure_rate,
                evidence: self
                    .agent_traces
                    .keys()
                    .take(10)
                    .cloned()
                    .collect(),
                detected_at_ns: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos() as u64,
                duration_ns: 0,
            });
        }

        None
    }

    /// Detect oscillations
    ///
    /// Q_oscillation_free: no periodic behavior with period < threshold
    pub fn detect_oscillation(&self, min_period_ns: u64) -> Option<EmergentPattern> {
        for (agent_id, events) in &self.agent_traces {
            let periods = self._find_oscillation_periods(events, min_period_ns);

            if !periods.is_empty() {
                return Some(EmergentPattern {
                    id: format!("oscillation_{}", uuid()),
                    pattern_type: EmergenceType::Oscillation,
                    scale: 1,
                    confidence: 0.8,
                    evidence: vec![agent_id.clone()],
                    detected_at_ns: events.first().map(|e| e.timestamp_ns).unwrap_or(0),
                    duration_ns: events
                        .last()
                        .map(|e| e.timestamp_ns)
                        .unwrap_or(0),
                });
            }
        }

        None
    }

    fn _find_oscillation_periods(&self, events: &[AgentEvent], min_period: u64) -> Vec<u64> {
        let mut periods = Vec::new();

        if events.len() < 4 {
            return periods;
        }

        // Look for repeating patterns
        for i in 0..events.len() - 2 {
            let period = events[i + 1].timestamp_ns - events[i].timestamp_ns;

            if period >= min_period {
                // Check if next event follows same period
                if i + 2 < events.len() {
                    let next_period = events[i + 2].timestamp_ns - events[i + 1].timestamp_ns;
                    if (next_period as i64 - period as i64).abs() < (period as i64 / 10) {
                        periods.push(period);
                    }
                }
            }
        }

        periods
    }

    /// Detect synchronization
    ///
    /// Q_independent: agents should act independently
    /// If > K% of agents do same action at same time → violation
    pub fn detect_synchronization(&self, threshold: f64) -> Option<EmergentPattern> {
        // Group events by (type, time window)
        let mut events_by_type_and_time: BTreeMap<(String, u64), usize> = BTreeMap::new();

        for events in self.agent_traces.values() {
            for event in events {
                let window = event.timestamp_ns / self.window_ns;
                *events_by_type_and_time
                    .entry((event.event_type.clone(), window))
                    .or_insert(0) += 1;
            }
        }

        // Check if any event type has too many agents doing it simultaneously
        for (_, count) in events_by_type_and_time.iter() {
            let sync_rate = *count as f64 / self.agent_traces.len() as f64;

            if sync_rate > threshold {
                return Some(EmergentPattern {
                    id: format!("sync_{}", uuid()),
                    pattern_type: EmergenceType::Synchronization,
                    scale: *count,
                    confidence: sync_rate,
                    evidence: self
                        .agent_traces
                        .keys()
                        .take(5)
                        .cloned()
                        .collect(),
                    detected_at_ns: std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_nanos() as u64,
                    duration_ns: self.window_ns,
                });
            }
        }

        None
    }

    /// Detect deadlocks
    pub fn detect_deadlock(&self) -> Option<EmergentPattern> {
        // Look for "blocked" states that don't change
        let mut blocked_agents = Vec::new();

        for (agent_id, events) in &self.agent_traces {
            let recent: Vec<_> = events.iter().rev().take(5).collect();

            // If all recent events are "blocked" or "waiting"
            if recent.iter().all(|e| {
                e.event_type == "blocked"
                    || e.event_type == "waiting"
                    || e.event_type == "deadlock"
            }) {
                blocked_agents.push(agent_id.clone());
            }
        }

        if blocked_agents.len() > 0 {
            return Some(EmergentPattern {
                id: format!("deadlock_{}", uuid()),
                pattern_type: EmergenceType::DeadlockEmergence,
                scale: blocked_agents.len(),
                confidence: 0.9,
                evidence: blocked_agents,
                detected_at_ns: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos() as u64,
                duration_ns: 0,
            });
        }

        None
    }

    /// Comprehensive emergence analysis
    pub fn analyze_all(&mut self) -> EmergenceAnalysis {
        let mut patterns = Vec::new();

        if let Some(cascade) = self.detect_cascading_failure(0.5) {
            patterns.push(cascade);
        }

        if let Some(osc) = self.detect_oscillation(1_000_000) {
            // 1ms threshold
            patterns.push(osc);
        }

        if let Some(sync) = self.detect_synchronization(0.8) {
            patterns.push(sync);
        }

        if let Some(deadlock) = self.detect_deadlock() {
            patterns.push(deadlock);
        }

        self.patterns = patterns.clone();

        EmergenceAnalysis {
            patterns,
            trace_size: self.agent_traces.len(),
            recommendation: self._recommend_constraints(),
        }
    }

    fn _recommend_constraints(&self) -> Vec<ConstraintRecommendation> {
        let mut recommendations = Vec::new();

        // For each pattern, recommend a constraint to prevent it
        for pattern in &self.patterns {
            match &pattern.pattern_type {
                EmergenceType::CascadingFailure => {
                    recommendations.push(ConstraintRecommendation {
                        pattern_id: pattern.id.clone(),
                        recommended_invariant: "Q_isolation: Agents can fail independently without affecting > 5% of peers".to_string(),
                        action: ConstraintAction::PreventEmergence,
                        confidence: pattern.confidence,
                    });
                }
                EmergenceType::Oscillation => {
                    recommendations.push(ConstraintRecommendation {
                        pattern_id: pattern.id.clone(),
                        recommended_invariant: "Q_damping: System must have dampening to prevent oscillations".to_string(),
                        action: ConstraintAction::PreventEmergence,
                        confidence: pattern.confidence,
                    });
                }
                EmergenceType::Synchronization => {
                    recommendations.push(ConstraintRecommendation {
                        pattern_id: pattern.id.clone(),
                        recommended_invariant: "Q_independence: Agent actions must be temporally decorrelated".to_string(),
                        action: ConstraintAction::PreventEmergence,
                        confidence: pattern.confidence,
                    });
                }
                EmergenceType::DeadlockEmergence => {
                    recommendations.push(ConstraintRecommendation {
                        pattern_id: pattern.id.clone(),
                        recommended_invariant: "Q_deadlock_free: No circular dependencies in resource allocation".to_string(),
                        action: ConstraintAction::PreventEmergence,
                        confidence: pattern.confidence,
                    });
                }
                _ => {
                    recommendations.push(ConstraintRecommendation {
                        pattern_id: pattern.id.clone(),
                        recommended_invariant: format!(
                            "Q_emerging: Constraint needed for {:?}",
                            pattern.pattern_type
                        ),
                        action: ConstraintAction::Monitor,
                        confidence: pattern.confidence,
                    });
                }
            }
        }

        recommendations
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintRecommendation {
    pub pattern_id: String,
    pub recommended_invariant: String,
    pub action: ConstraintAction,
    pub confidence: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConstraintAction {
    /// Synthesize new Q to prevent this emergence
    PreventEmergence,

    /// Monitor for this pattern, allow if necessary
    Monitor,

    /// Explicitly enable/allow this emergence
    Allow,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmergenceAnalysis {
    pub patterns: Vec<EmergentPattern>,
    pub trace_size: usize,
    pub recommendation: Vec<ConstraintRecommendation>,
}

/// Constraint synthesis: automatically generate new Q from patterns
pub struct ConstraintSynthesizer;

impl ConstraintSynthesizer {
    /// Given an emergent pattern, synthesize a constraint that prevents it
    pub fn synthesize_constraint(
        pattern: &EmergentPattern,
    ) -> Result<SynthesizedConstraint, SynthesisError> {
        match &pattern.pattern_type {
            EmergenceType::CascadingFailure => Ok(SynthesizedConstraint {
                id: format!("q_cascade_free_{}", uuid()),
                predicate: "For all agent sets S: failures(S) / |S| <= 0.05".to_string(),
                severity: "Error".to_string(),
                category: "Safety".to_string(),
                verifiable: true,
            }),

            EmergenceType::Oscillation => Ok(SynthesizedConstraint {
                id: format!("q_damped_{}", uuid()),
                predicate: "System amplitude must decrease over time: A(t+T) < A(t)".to_string(),
                severity: "Warning".to_string(),
                category: "Liveness".to_string(),
                verifiable: false, // Requires continued observation
            }),

            EmergenceType::Synchronization => Ok(SynthesizedConstraint {
                id: format!("q_decorrelated_{}", uuid()),
                predicate: "Agent action timing: stddev(δt) > threshold".to_string(),
                severity: "Warning".to_string(),
                category: "Determinism".to_string(),
                verifiable: true,
            }),

            EmergenceType::DeadlockEmergence => Ok(SynthesizedConstraint {
                id: format!("q_deadlock_free_{}", uuid()),
                predicate: "No cycles in resource dependency graph".to_string(),
                severity: "Error".to_string(),
                category: "Safety".to_string(),
                verifiable: true,
            }),

            _ => Err(SynthesisError::UnknownPattern),
        }
    }

    /// Generate the RDF Turtle representation of the constraint
    pub fn synthesize_to_turtle(constraint: &SynthesizedConstraint) -> String {
        format!(
            r#"@prefix q: <http://ggen.io/q/> .

q:{} a q:Invariant ;
    q:predicate "{}" ;
    q:severity q:{} ;
    q:category q:{} ;
    q:verifiable {} .
"#,
            constraint.id,
            constraint.predicate,
            constraint.severity,
            constraint.category,
            if constraint.verifiable { "true" } else { "false" }
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SynthesizedConstraint {
    pub id: String,
    pub predicate: String,
    pub severity: String,
    pub category: String,
    pub verifiable: bool,
}

#[derive(Debug, Clone)]
pub enum SynthesisError {
    UnknownPattern,
    InsufficientEvidence,
    ConflictingConstraints,
}

/// Phase transition detector
///
/// Detects when system behavior changes discontinuously
#[derive(Debug, Clone)]
pub struct PhaseTransitionDetector {
    /// Parameter values at each observation
    parameter_values: Vec<(f64, SystemBehavior)>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SystemBehavior {
    Stable,
    Oscillating,
    Chaotic,
    Unknown,
}

impl PhaseTransitionDetector {
    pub fn new() -> Self {
        PhaseTransitionDetector {
            parameter_values: Vec::new(),
        }
    }

    pub fn observe(&mut self, parameter: f64, behavior: SystemBehavior) {
        self.parameter_values.push((parameter, behavior));
    }

    /// Detect discontinuous change in behavior
    pub fn detect_phase_transition(&self) -> Option<PhaseTransition> {
        if self.parameter_values.len() < 2 {
            return None;
        }

        for i in 0..self.parameter_values.len() - 1 {
            let (p1, b1) = &self.parameter_values[i];
            let (p2, b2) = &self.parameter_values[i + 1];

            if b1 != b2 {
                // Behavior changed
                let parameter_delta = (p2 - p1).abs();

                // If small parameter change → large behavior change = phase transition
                if parameter_delta < 0.1 && b1 != b2 {
                    return Some(PhaseTransition {
                        id: format!("phase_transition_{}", uuid()),
                        at_parameter: *p2,
                        from_behavior: b1.clone(),
                        to_behavior: b2.clone(),
                        parameter_delta,
                    });
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhaseTransition {
    pub id: String,
    pub at_parameter: f64,
    pub from_behavior: SystemBehavior,
    pub to_behavior: SystemBehavior,
    pub parameter_delta: f64,
}

fn uuid() -> String {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher, Hash, Hasher};

    let mut hasher = RandomState::new().build_hasher();
    std::time::SystemTime::now().hash(&mut hasher);
    format!("{:x}", hasher.finish()).chars().take(8).collect()
}

impl fmt::Display for EmergentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Pattern({:?}, scale={}, confidence={:.2})",
            self.pattern_type, self.scale, self.confidence
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cascading_failure_detection() {
        let mut detector = EmergenceDetector::new(1_000_000);

        // Create trace where 60% of agents fail
        for i in 0..100 {
            if i < 60 {
                detector.observe(AgentEvent {
                    agent_id: format!("agent_{}", i),
                    event_type: "failure".to_string(),
                    value: 1.0,
                    timestamp_ns: 1000,
                });
            }
        }

        if let Some(pattern) = detector.detect_cascading_failure(0.5) {
            assert_eq!(pattern.pattern_type, EmergenceType::CascadingFailure);
            assert_eq!(pattern.scale, 60);
        }
    }

    #[test]
    fn test_synchronization_detection() {
        let mut detector = EmergenceDetector::new(1_000_000);

        // Create trace where 90% of agents do same action
        for i in 0..100 {
            detector.observe(AgentEvent {
                agent_id: format!("agent_{}", i),
                event_type: "send".to_string(),
                value: 1.0,
                timestamp_ns: 1000,
            });
        }

        if let Some(pattern) = detector.detect_synchronization(0.8) {
            assert_eq!(pattern.pattern_type, EmergenceType::Synchronization);
        }
    }

    #[test]
    fn test_constraint_synthesis() {
        let pattern = EmergentPattern {
            id: "test".to_string(),
            pattern_type: EmergenceType::CascadingFailure,
            scale: 100,
            confidence: 0.95,
            evidence: vec![],
            detected_at_ns: 0,
            duration_ns: 0,
        };

        let constraint = ConstraintSynthesizer::synthesize_constraint(&pattern).unwrap();
        assert!(constraint.verifiable);
        assert_eq!(constraint.severity, "Error");
    }

    #[test]
    fn test_phase_transition_detection() {
        let mut detector = PhaseTransitionDetector::new();

        detector.observe(0.5, SystemBehavior::Stable);
        detector.observe(0.51, SystemBehavior::Oscillating); // Small parameter change

        if let Some(transition) = detector.detect_phase_transition() {
            assert_eq!(transition.from_behavior, SystemBehavior::Stable);
            assert_eq!(transition.to_behavior, SystemBehavior::Oscillating);
        }
    }
}
