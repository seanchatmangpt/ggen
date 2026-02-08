//! Built-in Metrics for DSPy Evaluation
//!
//! Provides standard metrics for common evaluation scenarios.

use super::types::{MetricError, SimpleMetricFn};
use crate::dspy::Example;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// Calculate F1 score from precision and recall
///
/// # Arguments
/// * `precision` - Precision value (0.0-1.0)
/// * `recall` - Recall value (0.0-1.0)
///
/// # Returns
/// F1 score (0.0-1.0)
pub fn f1_score(precision: f64, recall: f64) -> f64 {
    let precision = precision.clamp(0.0, 1.0);
    let recall = recall.clamp(0.0, 1.0);

    if precision + recall == 0.0 {
        0.0
    } else {
        2.0 * (precision * recall) / (precision + recall)
    }
}

/// Exact match metric - checks if output exactly matches expected value
///
/// # Arguments
/// * `field` - Name of the field to compare
///
/// # Returns
/// Metric function that returns 1.0 for exact match, 0.0 otherwise
///
/// # Example
/// ```ignore
/// use ggen_ai::dspy::evaluation::metrics::exact_match;
///
/// let metric = exact_match("answer");
/// ```
pub fn exact_match(field: &str) -> SimpleMetricFn {
    let field = field.to_string();
    Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
        let expected = example.outputs.get(&field);
        let actual = pred.get(&field);

        match (expected, actual) {
            (Some(exp), Some(act)) => Ok(if exp == act { 1.0 } else { 0.0 }),
            _ => Ok(0.0),
        }
    })
}

/// Case-insensitive exact match metric
///
/// # Arguments
/// * `field` - Name of the field to compare
///
/// # Returns
/// Metric function that returns 1.0 for case-insensitive match, 0.0 otherwise
pub fn exact_match_ci(field: &str) -> SimpleMetricFn {
    let field = field.to_string();
    Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
        let expected = example.outputs.get(&field).and_then(|v| v.as_str());
        let actual = pred.get(&field).and_then(|v| v.as_str());

        match (expected, actual) {
            (Some(exp), Some(act)) => Ok(if exp.to_lowercase() == act.to_lowercase() {
                1.0
            } else {
                0.0
            }),
            _ => Ok(0.0),
        }
    })
}

/// Passage match metric - checks if answer appears in retrieved context
///
/// # Arguments
/// * `answer_field` - Name of the answer field
/// * `passage_field` - Name of the passage/context field (should be array)
///
/// # Returns
/// Metric function that returns 1.0 if answer found in any passage, 0.0 otherwise
pub fn passage_match(answer_field: &str, passage_field: &str) -> SimpleMetricFn {
    let answer_field = answer_field.to_string();
    let passage_field = passage_field.to_string();

    Arc::new(move |_example: &Example, pred: &HashMap<String, Value>| {
        let answer = pred
            .get(&answer_field)
            .and_then(|v| v.as_str())
            .ok_or_else(|| MetricError::MissingField(answer_field.clone()))?;

        let passages = pred
            .get(&passage_field)
            .and_then(|v| v.as_array())
            .ok_or_else(|| MetricError::MissingField(passage_field.clone()))?;

        let found = passages
            .iter()
            .any(|p| p.as_str().map(|s| s.contains(answer)).unwrap_or(false));

        Ok(if found { 1.0 } else { 0.0 })
    })
}

/// Substring match metric - checks if answer is substring of expected or vice versa
///
/// # Arguments
/// * `field` - Name of the field to compare
///
/// # Returns
/// Metric function that returns 1.0 if strings overlap, 0.0 otherwise
pub fn substring_match(field: &str) -> SimpleMetricFn {
    let field = field.to_string();
    Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
        let expected = example.outputs.get(&field).and_then(|v| v.as_str());
        let actual = pred.get(&field).and_then(|v| v.as_str());

        match (expected, actual) {
            (Some(exp), Some(act)) => {
                let exp_lower = exp.to_lowercase();
                let act_lower = act.to_lowercase();

                Ok(
                    if exp_lower.contains(&act_lower) || act_lower.contains(&exp_lower) {
                        1.0
                    } else {
                        0.0
                    },
                )
            }
            _ => Ok(0.0),
        }
    })
}

/// Token overlap metric - calculates Jaccard similarity between token sets
///
/// # Arguments
/// * `field` - Name of the field to compare
///
/// # Returns
/// Metric function that returns Jaccard similarity (0.0-1.0)
pub fn token_overlap(field: &str) -> SimpleMetricFn {
    let field = field.to_string();
    Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
        let expected = example.outputs.get(&field).and_then(|v| v.as_str());
        let actual = pred.get(&field).and_then(|v| v.as_str());

        match (expected, actual) {
            (Some(exp), Some(act)) => {
                let exp_lower = exp.to_lowercase();
                let act_lower = act.to_lowercase();

                let exp_tokens: std::collections::HashSet<_> =
                    exp_lower.split_whitespace().collect();
                let act_tokens: std::collections::HashSet<_> =
                    act_lower.split_whitespace().collect();

                let intersection = exp_tokens.intersection(&act_tokens).count();
                let union = exp_tokens.union(&act_tokens).count();

                if union == 0 {
                    Ok(0.0)
                } else {
                    Ok(intersection as f64 / union as f64)
                }
            }
            _ => Ok(0.0),
        }
    })
}

/// Length within range metric - checks if output length is within acceptable range
///
/// # Arguments
/// * `field` - Name of the field to check
/// * `min_words` - Minimum word count
/// * `max_words` - Maximum word count
///
/// # Returns
/// Metric function that returns 1.0 if length in range, 0.0 otherwise
pub fn length_within_range(field: &str, min_words: usize, max_words: usize) -> SimpleMetricFn {
    let field = field.to_string();
    Arc::new(move |_example: &Example, pred: &HashMap<String, Value>| {
        let text = pred
            .get(&field)
            .and_then(|v| v.as_str())
            .ok_or_else(|| MetricError::MissingField(field.clone()))?;

        let word_count = text.split_whitespace().count();

        Ok(if word_count >= min_words && word_count <= max_words {
            1.0
        } else {
            0.0
        })
    })
}

/// Composite metric - combines multiple metrics with weights
///
/// # Arguments
/// * `metrics` - Vector of (metric, weight) pairs
///
/// # Returns
/// Metric function that returns weighted average of all metrics
pub fn composite(metrics: Vec<(SimpleMetricFn, f64)>) -> SimpleMetricFn {
    Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
        let mut total_score = 0.0;
        let mut total_weight = 0.0;

        for (metric, weight) in &metrics {
            let score = metric(example, pred)?;
            total_score += score * weight;
            total_weight += weight;
        }

        if total_weight == 0.0 {
            Ok(0.0)
        } else {
            Ok(total_score / total_weight)
        }
    })
}

/// ExactMatchMetric struct for object-oriented style
pub struct ExactMatchMetric {
    field: String,
    case_sensitive: bool,
}

impl ExactMatchMetric {
    /// Create new exact match metric
    pub fn new(field: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            case_sensitive: true,
        }
    }

    /// Set case sensitivity
    pub fn case_sensitive(mut self, sensitive: bool) -> Self {
        self.case_sensitive = sensitive;
        self
    }

    /// Convert to metric function
    pub fn as_metric(&self) -> SimpleMetricFn {
        if self.case_sensitive {
            exact_match(&self.field)
        } else {
            exact_match_ci(&self.field)
        }
    }
}

/// F1Metric struct for precision/recall-based evaluation
pub struct F1Metric {
    answer_field: String,
    expected_field: String,
}

impl F1Metric {
    /// Create new F1 metric
    pub fn new(answer_field: impl Into<String>, expected_field: impl Into<String>) -> Self {
        Self {
            answer_field: answer_field.into(),
            expected_field: expected_field.into(),
        }
    }

    /// Convert to metric function
    pub fn as_metric(&self) -> SimpleMetricFn {
        let answer_field = self.answer_field.clone();
        let expected_field = self.expected_field.clone();

        Arc::new(move |example: &Example, pred: &HashMap<String, Value>| {
            let predicted = pred
                .get(&answer_field)
                .and_then(|v| v.as_str())
                .ok_or_else(|| MetricError::MissingField(answer_field.clone()))?;

            let expected = example
                .outputs
                .get(&expected_field)
                .and_then(|v| v.as_str())
                .ok_or_else(|| MetricError::MissingField(expected_field.clone()))?;

            // Tokenize and calculate precision/recall
            let predicted_lower = predicted.to_lowercase();
            let expected_lower = expected.to_lowercase();

            let pred_tokens: std::collections::HashSet<_> =
                predicted_lower.split_whitespace().collect();
            let exp_tokens: std::collections::HashSet<_> =
                expected_lower.split_whitespace().collect();

            if exp_tokens.is_empty() || pred_tokens.is_empty() {
                return Ok(0.0);
            }

            let true_positives = pred_tokens.intersection(&exp_tokens).count() as f64;
            let precision = true_positives / pred_tokens.len() as f64;
            let recall = true_positives / exp_tokens.len() as f64;

            Ok(f1_score(precision, recall))
        })
    }
}

/// PassageMatchMetric struct for context-grounded evaluation
pub struct PassageMatchMetric {
    answer_field: String,
    passage_field: String,
}

impl PassageMatchMetric {
    /// Create new passage match metric
    pub fn new(answer_field: impl Into<String>, passage_field: impl Into<String>) -> Self {
        Self {
            answer_field: answer_field.into(),
            passage_field: passage_field.into(),
        }
    }

    /// Convert to metric function
    pub fn as_metric(&self) -> SimpleMetricFn {
        passage_match(&self.answer_field, &self.passage_field)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn create_example(expected: &str) -> Example {
        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), json!(expected));
        Example::new(HashMap::new(), outputs)
    }

    fn create_prediction(answer: &str) -> HashMap<String, Value> {
        let mut pred = HashMap::new();
        pred.insert("answer".to_string(), json!(answer));
        pred
    }

    // ===== F1 Score Tests =====

    #[test]
    fn test_f1_score_perfect() {
        assert_eq!(f1_score(1.0, 1.0), 1.0);
    }

    #[test]
    fn test_f1_score_zero() {
        assert_eq!(f1_score(0.0, 0.0), 0.0);
    }

    #[test]
    fn test_f1_score_balanced() {
        let score = f1_score(0.5, 0.5);
        assert!((score - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_f1_score_precision_recall() {
        let score = f1_score(0.8, 0.6);
        let expected = 2.0 * (0.8 * 0.6) / (0.8 + 0.6);
        assert!((score - expected).abs() < 0.001);
    }

    #[test]
    fn test_f1_score_clamping() {
        // Values outside 0-1 should be clamped
        assert_eq!(f1_score(-0.5, 1.5), 0.0);
    }

    // ===== Exact Match Tests =====

    #[test]
    fn test_exact_match_success() {
        let metric = exact_match("answer");
        let example = create_example("42");
        let pred = create_prediction("42");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_exact_match_failure() {
        let metric = exact_match("answer");
        let example = create_example("42");
        let pred = create_prediction("43");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    #[test]
    fn test_exact_match_missing_field() {
        let metric = exact_match("answer");
        let example = create_example("42");
        let pred = HashMap::new();

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    // ===== Case-Insensitive Exact Match Tests =====

    #[test]
    fn test_exact_match_ci_success() {
        let metric = exact_match_ci("answer");
        let example = create_example("HELLO");
        let pred = create_prediction("hello");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_exact_match_ci_failure() {
        let metric = exact_match_ci("answer");
        let example = create_example("hello");
        let pred = create_prediction("world");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    // ===== Passage Match Tests =====

    #[test]
    fn test_passage_match_success() {
        let metric = passage_match("answer", "passages");

        let example = Example::new(HashMap::new(), HashMap::new());

        let mut pred = HashMap::new();
        pred.insert("answer".to_string(), json!("Paris"));
        pred.insert(
            "passages".to_string(),
            json!(["The capital is Paris", "Other text"]),
        );

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_passage_match_failure() {
        let metric = passage_match("answer", "passages");

        let example = Example::new(HashMap::new(), HashMap::new());

        let mut pred = HashMap::new();
        pred.insert("answer".to_string(), json!("London"));
        pred.insert("passages".to_string(), json!(["The capital is Paris"]));

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    #[test]
    fn test_passage_match_missing_answer() {
        let metric = passage_match("answer", "passages");

        let example = Example::new(HashMap::new(), HashMap::new());

        let mut pred = HashMap::new();
        pred.insert("passages".to_string(), json!(["Some text"]));

        let result = metric(&example, &pred);
        assert!(result.is_err());
    }

    // ===== Substring Match Tests =====

    #[test]
    fn test_substring_match_exact() {
        let metric = substring_match("answer");
        let example = create_example("hello world");
        let pred = create_prediction("hello world");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_substring_match_substring() {
        let metric = substring_match("answer");
        let example = create_example("hello world");
        let pred = create_prediction("hello");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_substring_match_failure() {
        let metric = substring_match("answer");
        let example = create_example("hello");
        let pred = create_prediction("world");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    // ===== Token Overlap Tests =====

    #[test]
    fn test_token_overlap_perfect() {
        let metric = token_overlap("answer");
        let example = create_example("the quick brown fox");
        let pred = create_prediction("the quick brown fox");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_token_overlap_partial() {
        let metric = token_overlap("answer");
        let example = create_example("the quick brown fox");
        let pred = create_prediction("the quick");

        let score = metric(&example, &pred).unwrap();
        assert!((score - 0.5).abs() < 0.001); // 2 tokens overlap, 4 total
    }

    #[test]
    fn test_token_overlap_none() {
        let metric = token_overlap("answer");
        let example = create_example("hello world");
        let pred = create_prediction("foo bar");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    // ===== Length Within Range Tests =====

    #[test]
    fn test_length_within_range_success() {
        let metric = length_within_range("answer", 2, 5);

        let example = Example::new(HashMap::new(), HashMap::new());
        let pred = create_prediction("hello world test");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_length_within_range_too_short() {
        let metric = length_within_range("answer", 5, 10);

        let example = Example::new(HashMap::new(), HashMap::new());
        let pred = create_prediction("hello");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    #[test]
    fn test_length_within_range_too_long() {
        let metric = length_within_range("answer", 1, 3);

        let example = Example::new(HashMap::new(), HashMap::new());
        let pred = create_prediction("one two three four");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 0.0);
    }

    // ===== Composite Metric Tests =====

    #[test]
    fn test_composite_metric_single() {
        let metric1 = exact_match("answer");
        let composite_metric = composite(vec![(metric1, 1.0)]);

        let example = create_example("42");
        let pred = create_prediction("42");

        let score = composite_metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_composite_metric_weighted() {
        let metric1 = exact_match("answer");
        let metric2 = substring_match("answer");

        let composite_metric = composite(vec![(metric1, 0.7), (metric2, 0.3)]);

        let example = create_example("hello");
        let pred = create_prediction("hello");

        let score = composite_metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_composite_metric_partial_match() {
        let metric1 = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(1.0));
        let metric2 = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(0.0));

        let composite_metric = composite(vec![(metric1, 0.5), (metric2, 0.5)]);

        let example = Example::new(HashMap::new(), HashMap::new());
        let pred = HashMap::new();

        let score = composite_metric(&example, &pred).unwrap();
        assert_eq!(score, 0.5);
    }

    // ===== ExactMatchMetric Struct Tests =====

    #[test]
    fn test_exact_match_metric_struct() {
        let metric = ExactMatchMetric::new("answer").as_metric();

        let example = create_example("test");
        let pred = create_prediction("test");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_exact_match_metric_case_insensitive() {
        let metric = ExactMatchMetric::new("answer")
            .case_sensitive(false)
            .as_metric();

        let example = create_example("TEST");
        let pred = create_prediction("test");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    // ===== F1Metric Struct Tests =====

    #[test]
    fn test_f1_metric_struct_perfect() {
        let metric = F1Metric::new("answer", "answer").as_metric();

        let example = create_example("the quick brown fox");
        let pred = create_prediction("the quick brown fox");

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }

    #[test]
    fn test_f1_metric_struct_partial() {
        let metric = F1Metric::new("answer", "answer").as_metric();

        let example = create_example("the quick brown fox");
        let pred = create_prediction("the quick");

        let score = metric(&example, &pred).unwrap();
        assert!(score > 0.0 && score < 1.0);
    }

    // ===== PassageMatchMetric Struct Tests =====

    #[test]
    fn test_passage_match_metric_struct() {
        let metric = PassageMatchMetric::new("answer", "passages").as_metric();

        let example = Example::new(HashMap::new(), HashMap::new());

        let mut pred = HashMap::new();
        pred.insert("answer".to_string(), json!("Berlin"));
        pred.insert(
            "passages".to_string(),
            json!(["Berlin is the capital", "Other text"]),
        );

        let score = metric(&example, &pred).unwrap();
        assert_eq!(score, 1.0);
    }
}
