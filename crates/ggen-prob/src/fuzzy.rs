//! Fuzzy logic integration for ambiguous domain modeling
//!
//! This module provides fuzzy sets, membership functions, and fuzzy inference
//! for handling ambiguous and imprecise domain concepts.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Fuzzy set with membership function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuzzySet {
    /// Name of the fuzzy set
    pub name: String,
    /// Membership function
    pub membership: MembershipFunction,
    /// Universe of discourse (min, max)
    pub universe: (f64, f64),
}

impl FuzzySet {
    /// Create a new fuzzy set
    pub fn new(name: impl Into<String>, membership: MembershipFunction, universe: (f64, f64)) -> Self {
        Self {
            name: name.into(),
            membership,
            universe,
        }
    }

    /// Get membership degree for a value
    pub fn membership_degree(&self, value: f64) -> f64 {
        if value < self.universe.0 || value > self.universe.1 {
            return 0.0;
        }
        self.membership.compute(value)
    }

    /// Check if value is in the core (membership = 1.0)
    pub fn in_core(&self, value: f64) -> bool {
        (self.membership_degree(value) - 1.0).abs() < 1e-6
    }

    /// Check if value is in the support (membership > 0)
    pub fn in_support(&self, value: f64) -> bool {
        self.membership_degree(value) > 1e-6
    }

    /// Get alpha-cut (values with membership >= alpha)
    pub fn alpha_cut(&self, alpha: f64, samples: usize) -> Vec<f64> {
        let step = (self.universe.1 - self.universe.0) / samples as f64;
        let mut result = Vec::new();

        let mut x = self.universe.0;
        while x <= self.universe.1 {
            if self.membership_degree(x) >= alpha {
                result.push(x);
            }
            x += step;
        }

        result
    }
}

/// Membership function for fuzzy sets
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MembershipFunction {
    /// Triangular membership function (a, b, c)
    Triangular { a: f64, b: f64, c: f64 },
    /// Trapezoidal membership function (a, b, c, d)
    Trapezoidal { a: f64, b: f64, c: f64, d: f64 },
    /// Gaussian membership function (mean, stddev)
    Gaussian { mean: f64, stddev: f64 },
    /// Sigmoid membership function (center, slope)
    Sigmoid { center: f64, slope: f64 },
    /// Custom piecewise linear function
    Piecewise { points: Vec<(f64, f64)> },
}

impl MembershipFunction {
    /// Compute membership degree
    pub fn compute(&self, x: f64) -> f64 {
        match self {
            MembershipFunction::Triangular { a, b, c } => {
                if x <= *a || x >= *c {
                    0.0
                } else if x == *b {
                    1.0
                } else if x < *b {
                    (x - a) / (b - a)
                } else {
                    (c - x) / (c - b)
                }
            }
            MembershipFunction::Trapezoidal { a, b, c, d } => {
                if x <= *a || x >= *d {
                    0.0
                } else if x >= *b && x <= *c {
                    1.0
                } else if x < *b {
                    (x - a) / (b - a)
                } else {
                    (d - x) / (d - c)
                }
            }
            MembershipFunction::Gaussian { mean, stddev } => {
                let exponent = -0.5 * ((x - mean) / stddev).powi(2);
                exponent.exp()
            }
            MembershipFunction::Sigmoid { center, slope } => {
                1.0 / (1.0 + (-slope * (x - center)).exp())
            }
            MembershipFunction::Piecewise { points } => {
                if points.is_empty() {
                    return 0.0;
                }

                // Find surrounding points
                for i in 0..points.len() - 1 {
                    let (x1, y1) = points[i];
                    let (x2, y2) = points[i + 1];

                    if x >= x1 && x <= x2 {
                        // Linear interpolation
                        if (x2 - x1).abs() < 1e-10 {
                            return y1;
                        }
                        return y1 + (y2 - y1) * (x - x1) / (x2 - x1);
                    }
                }

                0.0
            }
        }
    }
}

/// Fuzzy logic system for inference
pub struct FuzzyLogic {
    /// Fuzzy sets
    sets: HashMap<String, FuzzySet>,
    /// Fuzzy rules
    rules: Vec<FuzzyRule>,
}

impl FuzzyLogic {
    /// Create a new fuzzy logic system
    pub fn new() -> Self {
        Self {
            sets: HashMap::new(),
            rules: Vec::new(),
        }
    }

    /// Add a fuzzy set
    pub fn add_set(&mut self, set: FuzzySet) {
        self.sets.insert(set.name.clone(), set);
    }

    /// Add a fuzzy rule
    pub fn add_rule(&mut self, rule: FuzzyRule) {
        self.rules.push(rule);
    }

    /// Get a fuzzy set
    pub fn get_set(&self, name: &str) -> Option<&FuzzySet> {
        self.sets.get(name)
    }

    /// Perform fuzzy inference
    pub fn infer(&self, inputs: &HashMap<String, f64>) -> HashMap<String, f64> {
        let mut outputs = HashMap::new();

        for rule in &self.rules {
            // Evaluate antecedent (minimum of all conditions)
            let mut min_membership = 1.0;

            for (var, set_name) in &rule.antecedents {
                if let Some(&value) = inputs.get(var) {
                    if let Some(set) = self.sets.get(set_name) {
                        min_membership = min_membership.min(set.membership_degree(value));
                    }
                }
            }

            // Apply to consequent
            for (var, set_name) in &rule.consequents {
                if let Some(set) = self.sets.get(set_name) {
                    let current = outputs.get(var).copied().unwrap_or(0.0);
                    outputs.insert(var.clone(), current.max(min_membership));
                }
            }
        }

        outputs
    }

    /// Defuzzify output using centroid method
    pub fn defuzzify(&self, set_name: &str, membership: f64, samples: usize) -> Option<f64> {
        self.sets.get(set_name).map(|set| {
            let step = (set.universe.1 - set.universe.0) / samples as f64;
            let mut numerator = 0.0;
            let mut denominator = 0.0;

            let mut x = set.universe.0;
            while x <= set.universe.1 {
                let mu = set.membership_degree(x).min(membership);
                numerator += x * mu;
                denominator += mu;
                x += step;
            }

            if denominator > 1e-10 {
                numerator / denominator
            } else {
                (set.universe.0 + set.universe.1) / 2.0
            }
        })
    }
}

impl Default for FuzzyLogic {
    fn default() -> Self {
        Self::new()
    }
}

/// Fuzzy rule (IF-THEN)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuzzyRule {
    /// Antecedents (IF part): variable -> fuzzy set
    pub antecedents: Vec<(String, String)>,
    /// Consequents (THEN part): variable -> fuzzy set
    pub consequents: Vec<(String, String)>,
    /// Rule weight [0.0, 1.0]
    pub weight: f64,
}

impl FuzzyRule {
    /// Create a new fuzzy rule
    pub fn new() -> Self {
        Self {
            antecedents: Vec::new(),
            consequents: Vec::new(),
            weight: 1.0,
        }
    }

    /// Add an antecedent condition
    pub fn if_condition(mut self, variable: impl Into<String>, fuzzy_set: impl Into<String>) -> Self {
        self.antecedents.push((variable.into(), fuzzy_set.into()));
        self
    }

    /// Add a consequent action
    pub fn then_action(mut self, variable: impl Into<String>, fuzzy_set: impl Into<String>) -> Self {
        self.consequents.push((variable.into(), fuzzy_set.into()));
        self
    }

    /// Set rule weight
    pub fn with_weight(mut self, weight: f64) -> Self {
        self.weight = weight.clamp(0.0, 1.0);
        self
    }
}

impl Default for FuzzyRule {
    fn default() -> Self {
        Self::new()
    }
}

/// Fuzzy type classifier for ambiguous type inference
pub struct FuzzyTypeClassifier {
    /// Fuzzy logic system
    logic: FuzzyLogic,
}

impl FuzzyTypeClassifier {
    /// Create a new fuzzy type classifier
    pub fn new() -> Self {
        let mut logic = FuzzyLogic::new();

        // Define fuzzy sets for type features
        logic.add_set(FuzzySet::new(
            "has_quotes",
            MembershipFunction::Sigmoid {
                center: 0.5,
                slope: 10.0,
            },
            (0.0, 1.0),
        ));

        logic.add_set(FuzzySet::new(
            "has_numbers",
            MembershipFunction::Sigmoid {
                center: 0.5,
                slope: 10.0,
            },
            (0.0, 1.0),
        ));

        logic.add_set(FuzzySet::new(
            "is_string",
            MembershipFunction::Trapezoidal {
                a: 0.0,
                b: 0.3,
                c: 0.7,
                d: 1.0,
            },
            (0.0, 1.0),
        ));

        logic.add_set(FuzzySet::new(
            "is_numeric",
            MembershipFunction::Trapezoidal {
                a: 0.0,
                b: 0.3,
                c: 0.7,
                d: 1.0,
            },
            (0.0, 1.0),
        ));

        // Add fuzzy rules
        logic.add_rule(
            FuzzyRule::new()
                .if_condition("has_quotes", "has_quotes")
                .then_action("type", "is_string")
                .with_weight(0.9),
        );

        logic.add_rule(
            FuzzyRule::new()
                .if_condition("has_numbers", "has_numbers")
                .then_action("type", "is_numeric")
                .with_weight(0.8),
        );

        Self { logic }
    }

    /// Classify type based on features
    pub fn classify(&self, features: &HashMap<String, f64>) -> HashMap<String, f64> {
        self.logic.infer(features)
    }

    /// Get the underlying fuzzy logic system
    pub fn logic(&self) -> &FuzzyLogic {
        &self.logic
    }
}

impl Default for FuzzyTypeClassifier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_triangular_membership() {
        let mf = MembershipFunction::Triangular {
            a: 0.0,
            b: 5.0,
            c: 10.0,
        };

        assert_eq!(mf.compute(0.0), 0.0);
        assert_eq!(mf.compute(5.0), 1.0);
        assert_eq!(mf.compute(10.0), 0.0);
        assert!((mf.compute(2.5) - 0.5).abs() < 0.01);
    }

    #[test]
    fn test_trapezoidal_membership() {
        let mf = MembershipFunction::Trapezoidal {
            a: 0.0,
            b: 2.0,
            c: 8.0,
            d: 10.0,
        };

        assert_eq!(mf.compute(0.0), 0.0);
        assert_eq!(mf.compute(5.0), 1.0);
        assert_eq!(mf.compute(10.0), 0.0);
    }

    #[test]
    fn test_gaussian_membership() {
        let mf = MembershipFunction::Gaussian {
            mean: 5.0,
            stddev: 1.0,
        };

        assert_eq!(mf.compute(5.0), 1.0);
        assert!(mf.compute(4.0) < 1.0);
        assert!(mf.compute(6.0) < 1.0);
    }

    #[test]
    fn test_fuzzy_set() {
        let set = FuzzySet::new(
            "medium",
            MembershipFunction::Triangular {
                a: 0.0,
                b: 5.0,
                c: 10.0,
            },
            (0.0, 10.0),
        );

        assert!(set.in_core(5.0));
        assert!(set.in_support(2.5));
        assert!(!set.in_support(15.0));
    }

    #[test]
    fn test_alpha_cut() {
        let set = FuzzySet::new(
            "medium",
            MembershipFunction::Triangular {
                a: 0.0,
                b: 5.0,
                c: 10.0,
            },
            (0.0, 10.0),
        );

        let cut = set.alpha_cut(0.5, 100);
        assert!(!cut.is_empty());
        assert!(cut.iter().all(|&x| set.membership_degree(x) >= 0.49));
    }

    #[test]
    fn test_fuzzy_logic() {
        let mut logic = FuzzyLogic::new();

        logic.add_set(FuzzySet::new(
            "high",
            MembershipFunction::Sigmoid {
                center: 7.0,
                slope: 1.0,
            },
            (0.0, 10.0),
        ));

        logic.add_set(FuzzySet::new(
            "positive",
            MembershipFunction::Sigmoid {
                center: 0.5,
                slope: 5.0,
            },
            (0.0, 1.0),
        ));

        logic.add_rule(
            FuzzyRule::new()
                .if_condition("confidence", "high")
                .then_action("decision", "positive"),
        );

        let mut inputs = HashMap::new();
        inputs.insert("confidence".to_string(), 8.0);

        let outputs = logic.infer(&inputs);
        assert!(outputs.contains_key("decision"));
    }

    #[test]
    fn test_fuzzy_type_classifier() {
        let classifier = FuzzyTypeClassifier::new();

        let mut features = HashMap::new();
        features.insert("has_quotes".to_string(), 0.9);
        features.insert("has_numbers".to_string(), 0.1);

        let result = classifier.classify(&features);
        assert!(result.contains_key("type"));
    }
}
