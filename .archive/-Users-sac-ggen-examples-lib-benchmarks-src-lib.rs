//! High-performance library demonstrating optimization techniques

use ahash::AHashMap;
use rayon::prelude::*;

/// Fast hash-based deduplication
pub fn deduplicate_parallel(items: &[String]) -> Vec<String> {
    let mut seen = AHashMap::new();
    items
        .par_iter()
        .enumerate()
        .filter_map(|(idx, item)| {
            if seen.insert(item.clone(), idx).is_none() {
                Some(item.clone())
            } else {
                None
            }
        })
        .collect()
}

/// Sequential version for comparison
pub fn deduplicate_sequential(items: &[String]) -> Vec<String> {
    let mut seen = AHashMap::new();
    let mut result = Vec::new();

    for item in items {
        if !seen.contains_key(item) {
            seen.insert(item.clone(), ());
            result.push(item.clone());
        }
    }

    result
}

/// Optimized string processing
pub fn process_batch(inputs: &[String]) -> Vec<String> {
    inputs
        .par_iter()
        .map(|s| s.to_uppercase())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deduplicate_parallel() {
        let items = vec!["a".to_string(), "b".to_string(), "a".to_string()];
        let result = deduplicate_parallel(&items);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_deduplicate_sequential() {
        let items = vec!["a".to_string(), "b".to_string(), "a".to_string()];
        let result = deduplicate_sequential(&items);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_process_batch() {
        let items = vec!["hello".to_string(), "world".to_string()];
        let result = process_batch(&items);
        assert_eq!(result, vec!["HELLO", "WORLD"]);
    }
}
