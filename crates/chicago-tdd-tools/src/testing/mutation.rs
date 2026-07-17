//! Mutation Testing Framework
//!
//! Validates test quality by introducing mutations and checking if tests catch them.

use std::collections::HashMap;

/// Mutation operator
#[derive(Debug, Clone)]
pub enum MutationOperator {
    /// Remove a key
    RemoveKey(String),
    /// Add a key-value pair
    AddKey(String, String),
    /// Change a value
    ChangeValue(String, String),
    /// Swap values between two keys (v1.3.0)
    SwapValues(String, String),
    /// Toggle boolean value (flip true/false) (v1.3.0)
    ToggleBoolean(String),
    /// Apply numeric delta (add/subtract from numeric value) (v1.3.0)
    NumericDelta(String, i32),
    /// Change string case (uppercase/lowercase/title) (v1.3.0)
    StringCase(String, CaseMode),
}

/// String case transformation mode (v1.3.0)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseMode {
    /// Convert to uppercase
    Upper,
    /// Convert to lowercase
    Lower,
    /// Convert to title case (first letter uppercase)
    Title,
}

/// Mutation tester
pub struct MutationTester {
    /// Original data
    original: HashMap<String, String>,
    /// Mutations applied
    mutations: Vec<MutationOperator>,
}

impl MutationTester {
    /// Create new mutation tester
    #[must_use]
    pub const fn new(original: HashMap<String, String>) -> Self {
        Self { original, mutations: vec![] }
    }

    /// Apply mutation operator
    pub fn apply_mutation(&mut self, mutation: MutationOperator) -> HashMap<String, String> {
        self.mutations.push(mutation.clone());
        self.mutate_data(&self.original.clone(), mutation)
    }

    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    fn mutate_data(
        &self,
        data: &HashMap<String, String>,
        mutation: MutationOperator,
    ) -> HashMap<String, String> {
        let mut mutated = data.clone();

        match mutation {
            MutationOperator::RemoveKey(key) => {
                mutated.remove(&key);
            }
            MutationOperator::AddKey(key, value) => {
                mutated.insert(key, value);
            }
            MutationOperator::ChangeValue(key, new_value) => {
                if let Some(v) = mutated.get_mut(&key) {
                    *v = new_value;
                }
            }
            // === v1.3.0 Phase 4: New Mutation Operators ===
            MutationOperator::SwapValues(key1, key2) => {
                // Swap values between two keys
                if let (Some(val1), Some(val2)) =
                    (mutated.get(&key1).cloned(), mutated.get(&key2).cloned())
                {
                    mutated.insert(key1, val2);
                    mutated.insert(key2, val1);
                }
            }
            MutationOperator::ToggleBoolean(key) => {
                // Toggle boolean value (flip true/false)
                if let Some(val) = mutated.get_mut(&key) {
                    *val = match val.to_lowercase().as_str() {
                        "true" => "false".to_string(),
                        "false" => "true".to_string(),
                        _ => val.clone(), // Not a boolean, no change
                    };
                }
            }
            MutationOperator::NumericDelta(key, offset) => {
                // Apply numeric delta (add/subtract from numeric value)
                if let Some(val) = mutated.get_mut(&key) {
                    if let Ok(num) = val.parse::<i32>() {
                        *val = (num + offset).to_string();
                    }
                    // If not a valid number, no change
                }
            }
            MutationOperator::StringCase(key, case_mode) => {
                // Change string case
                if let Some(val) = mutated.get_mut(&key) {
                    *val = match case_mode {
                        CaseMode::Upper => val.to_uppercase(),
                        CaseMode::Lower => val.to_lowercase(),
                        CaseMode::Title => {
                            // Title case: first character uppercase, rest lowercase
                            let mut chars = val.chars();
                            chars.next().map_or_else(String::new, |first| {
                                first.to_uppercase().collect::<String>()
                                    + &chars.as_str().to_lowercase()
                            })
                        }
                    };
                }
            }
        }

        mutated
    }

    /// Test if mutation is caught by tests
    pub fn test_mutation_detection<F>(&mut self, test_fn: F) -> bool
    where
        F: Fn(&HashMap<String, String>) -> bool,
    {
        // Test original (should pass)
        if !test_fn(&self.original) {
            return false; // Original test fails - invalid test
        }

        // Apply each mutation and test
        let mutations = self.mutations.clone();
        for mutation in mutations {
            let mutated = self.apply_mutation(mutation);
            if test_fn(&mutated) {
                // Mutation not detected - test quality issue
                return false;
            }
        }

        true
    }
}

/// Default threshold for an acceptable mutation score (80%).
///
/// A mutation score at or above this value indicates the test suite catches
/// enough mutations to be considered adequate. Callers that need a different
/// threshold should compare `MutationScore::score()` directly.
pub const DEFAULT_ACCEPTABLE_MUTATION_SCORE: f64 = 80.0;

/// Mutation score (percentage of mutations caught)
pub struct MutationScore {
    /// Total mutations tested
    #[allow(dead_code)] // Used in tests and future analysis
    pub total: usize,
    /// Mutations caught by tests
    #[allow(dead_code)] // Used in tests and future analysis
    pub caught: usize,
    /// Score percentage
    pub score: f64,
}

impl MutationScore {
    /// Calculate mutation score
    #[must_use]
    pub fn calculate(caught: usize, total: usize) -> Self {
        #[allow(clippy::cast_precision_loss)]
        // Percentage calculation - precision loss acceptable for mutation scores
        let score = if total > 0 { (caught as f64 / total as f64) * 100.0 } else { 0.0 };

        Self { total, caught, score }
    }

    /// Get score percentage
    #[must_use]
    pub const fn score(&self) -> f64 {
        self.score
    }

    /// Is score acceptable? (>= [`DEFAULT_ACCEPTABLE_MUTATION_SCORE`])
    #[must_use]
    pub fn is_acceptable(&self) -> bool {
        self.score >= DEFAULT_ACCEPTABLE_MUTATION_SCORE
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    // ========================================================================
    // MutationOperator Tests
    // ========================================================================

    #[test]
    fn test_mutation_operator_remove_key() {
        let op = MutationOperator::RemoveKey("key1".to_string());
        match op {
            MutationOperator::RemoveKey(key) => assert_eq!(key, "key1"),
            _ => panic!("Expected RemoveKey variant"),
        }
    }

    #[test]
    fn test_mutation_operator_add_key() {
        let op = MutationOperator::AddKey("key1".to_string(), "value1".to_string());
        match op {
            MutationOperator::AddKey(key, value) => {
                assert_eq!(key, "key1");
                assert_eq!(value, "value1");
            }
            _ => panic!("Expected AddKey variant"),
        }
    }

    #[test]
    fn test_mutation_operator_change_value() {
        let op = MutationOperator::ChangeValue("key1".to_string(), "new_value".to_string());
        match op {
            MutationOperator::ChangeValue(key, value) => {
                assert_eq!(key, "key1");
                assert_eq!(value, "new_value");
            }
            _ => panic!("Expected ChangeValue variant"),
        }
    }

    #[test]
    fn test_mutation_operator_clone() {
        let op1 = MutationOperator::RemoveKey("key1".to_string());
        let op2 = op1.clone();
        match (op1, op2) {
            (MutationOperator::RemoveKey(k1), MutationOperator::RemoveKey(k2)) => {
                assert_eq!(k1, k2);
            }
            _ => panic!("Expected RemoveKey variants"),
        }
    }

    // ========================================================================
    // MutationTester Tests
    // ========================================================================

    #[test]
    fn test_mutation_tester_new() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());
        let tester = MutationTester::new(data);
        assert_eq!(tester.original.len(), 1, "Original data should have 1 entry");
        assert_eq!(tester.mutations.len(), 0, "Should start with no mutations");
    }

    #[test]
    fn test_mutation_tester_apply_remove_key() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());
        data.insert("key2".to_string(), "value2".to_string());

        let mut tester = MutationTester::new(data);
        let mutated = tester.apply_mutation(MutationOperator::RemoveKey("key1".to_string()));

        assert_eq!(mutated.len(), 1, "Mutated data should have 1 entry");
        assert!(!mutated.contains_key("key1"), "key1 should be removed");
        assert!(mutated.contains_key("key2"), "key2 should remain");
        assert_eq!(tester.mutations.len(), 1, "Should track 1 mutation");
    }

    #[test]
    fn test_mutation_tester_apply_add_key() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);
        let mutated = tester
            .apply_mutation(MutationOperator::AddKey("key2".to_string(), "value2".to_string()));

        assert_eq!(mutated.len(), 2, "Mutated data should have 2 entries");
        assert_eq!(mutated.get("key2"), Some(&"value2".to_string()), "key2 should be added");
    }

    #[test]
    fn test_mutation_tester_apply_change_value() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);
        let mutated = tester.apply_mutation(MutationOperator::ChangeValue(
            "key1".to_string(),
            "new_value".to_string(),
        ));

        assert_eq!(mutated.get("key1"), Some(&"new_value".to_string()), "value should be changed");
    }

    #[test]
    fn test_mutation_tester_apply_change_value_nonexistent() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);
        let mutated = tester.apply_mutation(MutationOperator::ChangeValue(
            "nonexistent".to_string(),
            "value".to_string(),
        ));

        assert_eq!(mutated.len(), 1, "Data should remain unchanged");
        assert_eq!(
            mutated.get("key1"),
            Some(&"value1".to_string()),
            "Original value should remain"
        );
    }

    #[test]
    fn test_mutation_tester_test_mutation_detection_all_caught() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());
        data.insert("key2".to_string(), "value2".to_string());

        let mut tester = MutationTester::new(data);
        tester.apply_mutation(MutationOperator::RemoveKey("key1".to_string()));
        tester.apply_mutation(MutationOperator::AddKey("key3".to_string(), "value3".to_string()));

        // Test function that checks for both keys
        let test_fn = |d: &HashMap<String, String>| {
            d.contains_key("key1") && d.contains_key("key2") && !d.contains_key("key3")
        };

        let result = tester.test_mutation_detection(test_fn);
        assert!(result, "All mutations should be caught");
    }

    #[test]
    fn test_mutation_tester_test_mutation_detection_not_caught() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);
        tester.apply_mutation(MutationOperator::AddKey("key2".to_string(), "value2".to_string()));

        // Test function that doesn't check for key2
        let test_fn = |d: &HashMap<String, String>| d.contains_key("key1");

        let result = tester.test_mutation_detection(test_fn);
        assert!(!result, "Mutation should not be caught");
    }

    #[test]
    fn test_mutation_tester_test_mutation_detection_original_fails() {
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);
        tester.apply_mutation(MutationOperator::RemoveKey("key1".to_string()));

        // Test function that fails on original
        let test_fn = |d: &HashMap<String, String>| d.contains_key("nonexistent");

        let result = tester.test_mutation_detection(test_fn);
        assert!(!result, "Should return false if original test fails");
    }

    // ========================================================================
    // v1.3.0 Phase 4: New Mutation Operators Tests
    // ========================================================================

    #[test]
    fn test_mutation_operator_swap_values() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());
        data.insert("key2".to_string(), "value2".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester
            .apply_mutation(MutationOperator::SwapValues("key1".to_string(), "key2".to_string()));

        // Assert
        assert_eq!(mutated.get("key1"), Some(&"value2".to_string()), "key1 should have value2");
        assert_eq!(mutated.get("key2"), Some(&"value1".to_string()), "key2 should have value1");
    }

    #[test]
    fn test_mutation_operator_swap_values_nonexistent() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("key1".to_string(), "value1".to_string());

        let mut tester = MutationTester::new(data);

        // Act: Swap with nonexistent key
        let mutated = tester.apply_mutation(MutationOperator::SwapValues(
            "key1".to_string(),
            "nonexistent".to_string(),
        ));

        // Assert: No change when one key doesn't exist
        assert_eq!(
            mutated.get("key1"),
            Some(&"value1".to_string()),
            "key1 should remain unchanged"
        );
    }

    #[test]
    fn test_mutation_operator_toggle_boolean_true_to_false() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("flag".to_string(), "true".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::ToggleBoolean("flag".to_string()));

        // Assert
        assert_eq!(mutated.get("flag"), Some(&"false".to_string()), "true should toggle to false");
    }

    #[test]
    fn test_mutation_operator_toggle_boolean_false_to_true() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("flag".to_string(), "false".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::ToggleBoolean("flag".to_string()));

        // Assert
        assert_eq!(mutated.get("flag"), Some(&"true".to_string()), "false should toggle to true");
    }

    #[test]
    fn test_mutation_operator_toggle_boolean_case_insensitive() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("flag".to_string(), "TRUE".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::ToggleBoolean("flag".to_string()));

        // Assert: Should handle uppercase
        assert_eq!(mutated.get("flag"), Some(&"false".to_string()), "TRUE should toggle to false");
    }

    #[test]
    fn test_mutation_operator_toggle_boolean_non_boolean() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("key".to_string(), "not_a_bool".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::ToggleBoolean("key".to_string()));

        // Assert: Should remain unchanged for non-boolean values
        assert_eq!(
            mutated.get("key"),
            Some(&"not_a_bool".to_string()),
            "non-boolean should remain unchanged"
        );
    }

    #[test]
    fn test_mutation_operator_numeric_delta_positive() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("count".to_string(), "10".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::NumericDelta("count".to_string(), 5));

        // Assert
        assert_eq!(mutated.get("count"), Some(&"15".to_string()), "10 + 5 should be 15");
    }

    #[test]
    fn test_mutation_operator_numeric_delta_negative() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("count".to_string(), "10".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated =
            tester.apply_mutation(MutationOperator::NumericDelta("count".to_string(), -3));

        // Assert
        assert_eq!(mutated.get("count"), Some(&"7".to_string()), "10 - 3 should be 7");
    }

    #[test]
    fn test_mutation_operator_numeric_delta_non_numeric() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("key".to_string(), "not_a_number".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester.apply_mutation(MutationOperator::NumericDelta("key".to_string(), 5));

        // Assert: Should remain unchanged for non-numeric values
        assert_eq!(
            mutated.get("key"),
            Some(&"not_a_number".to_string()),
            "non-numeric should remain unchanged"
        );
    }

    #[test]
    fn test_mutation_operator_string_case_upper() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("text".to_string(), "hello".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester
            .apply_mutation(MutationOperator::StringCase("text".to_string(), CaseMode::Upper));

        // Assert
        assert_eq!(mutated.get("text"), Some(&"HELLO".to_string()), "should convert to uppercase");
    }

    #[test]
    fn test_mutation_operator_string_case_lower() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("text".to_string(), "HELLO".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester
            .apply_mutation(MutationOperator::StringCase("text".to_string(), CaseMode::Lower));

        // Assert
        assert_eq!(mutated.get("text"), Some(&"hello".to_string()), "should convert to lowercase");
    }

    #[test]
    fn test_mutation_operator_string_case_title() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("text".to_string(), "hello world".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester
            .apply_mutation(MutationOperator::StringCase("text".to_string(), CaseMode::Title));

        // Assert
        assert_eq!(
            mutated.get("text"),
            Some(&"Hello world".to_string()),
            "should convert to title case"
        );
    }

    #[test]
    fn test_mutation_operator_string_case_empty() {
        // Arrange
        let mut data = HashMap::new();
        data.insert("text".to_string(), "".to_string());

        let mut tester = MutationTester::new(data);

        // Act
        let mutated = tester
            .apply_mutation(MutationOperator::StringCase("text".to_string(), CaseMode::Title));

        // Assert: Empty string should remain empty
        assert_eq!(mutated.get("text"), Some(&"".to_string()), "empty string should remain empty");
    }

    #[test]
    fn test_case_mode_debug() {
        // Arrange & Act
        let upper = format!("{:?}", CaseMode::Upper);
        let lower = format!("{:?}", CaseMode::Lower);
        let title = format!("{:?}", CaseMode::Title);

        // Assert
        assert_eq!(upper, "Upper");
        assert_eq!(lower, "Lower");
        assert_eq!(title, "Title");
    }

    #[test]
    fn test_case_mode_clone() {
        // Arrange
        let mode1 = CaseMode::Upper;

        // Act
        let mode2 = mode1;

        // Assert: Copy trait allows this
        assert_eq!(mode1, mode2);
    }

    #[test]
    fn test_case_mode_equality() {
        // Arrange & Act & Assert
        assert_eq!(CaseMode::Upper, CaseMode::Upper);
        assert_eq!(CaseMode::Lower, CaseMode::Lower);
        assert_eq!(CaseMode::Title, CaseMode::Title);
        assert_ne!(CaseMode::Upper, CaseMode::Lower);
    }

    // ========================================================================
    // MutationScore Tests
    // ========================================================================

    #[test]
    fn test_mutation_score_calculate() {
        let score = MutationScore::calculate(8, 10);
        assert_eq!(score.total, 10, "Total should be 10");
        assert_eq!(score.caught, 8, "Caught should be 8");
        assert_eq!(score.score(), 80.0, "Score should be 80%");
    }

    #[test]
    fn test_mutation_score_calculate_zero_total() {
        let score = MutationScore::calculate(0, 0);
        assert_eq!(score.total, 0, "Total should be 0");
        assert_eq!(score.caught, 0, "Caught should be 0");
        assert_eq!(score.score(), 0.0, "Score should be 0% when total is 0");
    }

    #[test]
    fn test_mutation_score_calculate_perfect() {
        let score = MutationScore::calculate(10, 10);
        assert_eq!(score.score(), 100.0, "Perfect score should be 100%");
    }

    #[test]
    fn test_mutation_score_calculate_partial() {
        let score = MutationScore::calculate(5, 10);
        assert_eq!(score.score(), 50.0, "Partial score should be 50%");
    }

    #[test]
    fn test_mutation_score_is_acceptable() {
        let score_80 = MutationScore::calculate(8, 10);
        assert!(score_80.is_acceptable(), "80% should be acceptable");

        let score_100 = MutationScore::calculate(10, 10);
        assert!(score_100.is_acceptable(), "100% should be acceptable");

        let score_79 = MutationScore::calculate(79, 100);
        assert!(!score_79.is_acceptable(), "79% should not be acceptable");
    }
}
