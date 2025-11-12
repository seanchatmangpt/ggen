//! Permutation generator for comprehensive edge case testing
//!
//! Generates all possible combinations of test parameters to ensure
//! complete coverage of edge cases and boundary conditions.

use std::collections::HashSet;

/// Configuration for permutation generation
#[derive(Debug, Clone)]
pub struct PermutationConfig {
    /// Maximum number of permutations to generate
    pub max_permutations: usize,
    /// Include edge cases (empty, null, extreme values)
    pub include_edge_cases: bool,
    /// Include invalid inputs for error testing
    pub include_invalid: bool,
}

impl Default for PermutationConfig {
    fn default() -> Self {
        Self {
            max_permutations: 1000,
            include_edge_cases: true,
            include_invalid: true,
        }
    }
}

/// Generates permutations of string inputs
pub struct StringPermutations {
    values: Vec<String>,
    config: PermutationConfig,
}

impl StringPermutations {
    pub fn new(config: PermutationConfig) -> Self {
        let mut values = Vec::new();

        // Normal values
        values.push("test".to_string());
        values.push("example".to_string());
        values.push("sample-package".to_string());

        if config.include_edge_cases {
            // Edge cases
            values.push(String::new()); // Empty
            values.push(" ".to_string()); // Whitespace
            values.push("a".to_string()); // Single character
            values.push("a".repeat(256)); // Very long
            values.push("special-chars-!@#$%".to_string());
            values.push("unicode-测试-🦀".to_string());
        }

        if config.include_invalid {
            // Invalid inputs
            values.push("../../../etc/passwd".to_string()); // Path traversal
            values.push("<script>alert('xss')</script>".to_string()); // XSS
            values.push("'; DROP TABLE packages; --".to_string()); // SQL injection
            values.push("\0null\0byte".to_string()); // Null bytes
        }

        Self { values, config }
    }

    /// Get all permutations
    pub fn all(&self) -> Vec<String> {
        self.values
            .iter()
            .take(self.config.max_permutations)
            .cloned()
            .collect()
    }

    /// Get pairs of permutations
    pub fn pairs(&self) -> Vec<(String, String)> {
        let mut pairs = Vec::new();
        let limit = self.config.max_permutations;

        for (i, a) in self.values.iter().enumerate() {
            for b in self.values.iter().skip(i) {
                if pairs.len() >= limit {
                    return pairs;
                }
                pairs.push((a.clone(), b.clone()));
            }
        }

        pairs
    }
}

/// Generates permutations of numeric inputs
pub struct NumericPermutations {
    values: Vec<u64>,
    config: PermutationConfig,
}

impl NumericPermutations {
    pub fn new(config: PermutationConfig) -> Self {
        let mut values = Vec::new();

        // Normal values
        values.extend([1, 10, 100, 1000]);

        if config.include_edge_cases {
            // Edge cases
            values.push(0);
            values.push(u64::MAX);
            values.push(1);
            values.push(u64::MAX - 1);
        }

        Self { values, config }
    }

    pub fn all(&self) -> Vec<u64> {
        self.values
            .iter()
            .take(self.config.max_permutations)
            .copied()
            .collect()
    }
}

/// Generates permutations of search queries
pub struct SearchQueryPermutations {
    queries: Vec<String>,
    filters: Vec<Vec<String>>,
    config: PermutationConfig,
}

impl SearchQueryPermutations {
    pub fn new(config: PermutationConfig) -> Self {
        let mut queries = vec![
            "rust".to_string(),
            "web framework".to_string(),
            "database".to_string(),
        ];

        let mut filters = vec![
            vec![],
            vec!["rust".to_string()],
            vec!["rust".to_string(), "web".to_string()],
        ];

        if config.include_edge_cases {
            queries.push(String::new());
            queries.push("a".to_string());
            queries.push("a".repeat(1000));
            filters.push(vec!["category".to_string(); 100]);
        }

        Self {
            queries,
            filters,
            config,
        }
    }

    /// Generate all query + filter combinations
    pub fn all_combinations(&self) -> Vec<(String, Vec<String>)> {
        let mut combinations = Vec::new();
        let limit = self.config.max_permutations;

        for query in &self.queries {
            for filter in &self.filters {
                if combinations.len() >= limit {
                    return combinations;
                }
                combinations.push((query.clone(), filter.clone()));
            }
        }

        combinations
    }
}

/// Generates permutations of package operations
#[derive(Debug, Clone)]
pub enum PackageOperation {
    Add(String),
    Remove(String),
    Search(String),
    List,
    Update(String),
}

pub struct OperationPermutations {
    operations: Vec<PackageOperation>,
    config: PermutationConfig,
}

impl OperationPermutations {
    pub fn new(config: PermutationConfig) -> Self {
        let mut operations = vec![
            PackageOperation::Add("test-package".to_string()),
            PackageOperation::Search("rust".to_string()),
            PackageOperation::List,
            PackageOperation::Remove("test-package".to_string()),
        ];

        if config.include_edge_cases {
            operations.push(PackageOperation::Add(String::new()));
            operations.push(PackageOperation::Search("a".repeat(1000)));
            operations.push(PackageOperation::Remove("nonexistent".to_string()));
        }

        Self { operations, config }
    }

    /// Generate sequential operation sequences
    pub fn sequences(&self, length: usize) -> Vec<Vec<PackageOperation>> {
        let mut sequences = Vec::new();
        let mut current_sequence = Vec::new();

        self.generate_sequences_recursive(
            &mut sequences,
            &mut current_sequence,
            length,
            self.config.max_permutations,
        );

        sequences
    }

    fn generate_sequences_recursive(
        &self, sequences: &mut Vec<Vec<PackageOperation>>, current: &mut Vec<PackageOperation>,
        remaining: usize, max_sequences: usize,
    ) {
        if sequences.len() >= max_sequences {
            return;
        }

        if remaining == 0 {
            sequences.push(current.clone());
            return;
        }

        for op in &self.operations {
            current.push(op.clone());
            self.generate_sequences_recursive(sequences, current, remaining - 1, max_sequences);
            current.pop();
        }
    }

    pub fn all(&self) -> Vec<PackageOperation> {
        self.operations.clone()
    }
}

/// Generate concurrent operation patterns
pub struct ConcurrentPatterns {
    config: PermutationConfig,
}

impl ConcurrentPatterns {
    pub fn new(config: PermutationConfig) -> Self {
        Self { config }
    }

    /// Generate patterns of concurrent operations
    pub fn generate(
        &self, num_threads: usize, ops_per_thread: usize,
    ) -> Vec<Vec<PackageOperation>> {
        let op_gen = OperationPermutations::new(self.config.clone());
        let all_ops = op_gen.all();

        let mut patterns = Vec::new();

        for _ in 0..num_threads.min(self.config.max_permutations) {
            let mut thread_ops = Vec::new();
            for (i, op) in all_ops.iter().enumerate() {
                if i >= ops_per_thread {
                    break;
                }
                thread_ops.push(op.clone());
            }
            patterns.push(thread_ops);
        }

        patterns
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_permutations() {
        let config = PermutationConfig::default();
        let perms = StringPermutations::new(config);
        let all = perms.all();

        assert!(!all.is_empty());
        assert!(all.contains(&"test".to_string()));
    }

    #[test]
    fn test_string_pairs() {
        let config = PermutationConfig {
            max_permutations: 10,
            ..Default::default()
        };
        let perms = StringPermutations::new(config);
        let pairs = perms.pairs();

        assert!(!pairs.is_empty());
        assert!(pairs.len() <= 10);
    }

    #[test]
    fn test_numeric_permutations() {
        let config = PermutationConfig::default();
        let perms = NumericPermutations::new(config);
        let all = perms.all();

        assert!(!all.is_empty());
        assert!(all.contains(&0) || all.contains(&1));
    }

    #[test]
    fn test_search_query_combinations() {
        let config = PermutationConfig {
            max_permutations: 20,
            ..Default::default()
        };
        let perms = SearchQueryPermutations::new(config);
        let combos = perms.all_combinations();

        assert!(!combos.is_empty());
        assert!(combos.len() <= 20);
    }

    #[test]
    fn test_operation_sequences() {
        let config = PermutationConfig {
            max_permutations: 10,
            ..Default::default()
        };
        let perms = OperationPermutations::new(config);
        let sequences = perms.sequences(2);

        assert!(!sequences.is_empty());
        assert!(sequences.iter().all(|seq| seq.len() == 2));
    }

    #[test]
    fn test_concurrent_patterns() {
        let config = PermutationConfig {
            max_permutations: 10,
            ..Default::default()
        };
        let patterns = ConcurrentPatterns::new(config);
        let concurrent = patterns.generate(4, 3);

        assert_eq!(concurrent.len(), 4);
        assert!(concurrent.iter().all(|ops| ops.len() <= 3));
    }

    #[test]
    fn test_no_invalid_when_disabled() {
        let config = PermutationConfig {
            include_invalid: false,
            include_edge_cases: false,
            ..Default::default()
        };
        let perms = StringPermutations::new(config);
        let all = perms.all();

        // Should only contain normal values
        assert!(!all.iter().any(|s| s.contains("DROP TABLE")));
        assert!(!all.iter().any(|s| s.contains("<script>")));
    }
}
