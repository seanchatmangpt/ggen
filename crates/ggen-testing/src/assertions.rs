//! State Assertion Helpers
//!
//! Chicago TDD focuses on state-based verification rather than behavior mocking.
//! These assertions help verify observable state changes.
//!
//! # Examples
//!
//! ```
//! use ggen_testing::assertions::*;
//!
//! // Assert state changed
//! let before = vec![1, 2, 3];
//! let after = vec![1, 2, 3, 4];
//! assert_state_changed(&before, &after);
//!
//! // Assert collection contains
//! assert_contains(&vec![1, 2, 3], &2);
//! ```

use std::fmt::Debug;

/// Assert that state has changed
///
/// # Panics
///
/// Panics if the states are equal
pub fn assert_state_changed<T: PartialEq + Debug>(before: &T, after: &T) {
    assert_ne!(
        before, after,
        "Expected state to change, but it remained the same: {:?}",
        before
    );
}

/// Assert that state has not changed
///
/// # Panics
///
/// Panics if the states are not equal
pub fn assert_state_unchanged<T: PartialEq + Debug>(before: &T, after: &T) {
    assert_eq!(
        before, after,
        "Expected state to remain unchanged, but it changed from {:?} to {:?}",
        before, after
    );
}

/// Assert collection contains an item
///
/// # Panics
///
/// Panics if the collection does not contain the item
pub fn assert_contains<T: PartialEq + Debug>(collection: &[T], item: &T) {
    assert!(
        collection.contains(item),
        "Expected collection to contain {:?}, but it did not. Collection: {:?}",
        item, collection
    );
}

/// Assert collection does not contain an item
///
/// # Panics
///
/// Panics if the collection contains the item
pub fn assert_not_contains<T: PartialEq + Debug>(collection: &[T], item: &T) {
    assert!(
        !collection.contains(item),
        "Expected collection to not contain {:?}, but it did. Collection: {:?}",
        item, collection
    );
}

/// Assert collection has expected length
///
/// # Panics
///
/// Panics if the collection length does not match expected
pub fn assert_length<T>(collection: &[T], expected: usize) {
    assert_eq!(
        collection.len(),
        expected,
        "Expected collection length to be {expected}, but was {}",
        collection.len()
    );
}

/// Assert collection is empty
///
/// # Panics
///
/// Panics if the collection is not empty
pub fn assert_empty<T: Debug>(collection: &[T]) {
    assert!(
        collection.is_empty(),
        "Expected collection to be empty, but it contained {} items: {:?}",
        collection.len(),
        collection
    );
}

/// Assert collection is not empty
///
/// # Panics
///
/// Panics if the collection is empty
pub fn assert_not_empty<T>(collection: &[T]) {
    assert!(
        !collection.is_empty(),
        "Expected collection to not be empty, but it was"
    );
}

/// Assert value is within range
///
/// # Panics
///
/// Panics if the value is outside the range
pub fn assert_in_range<T: PartialOrd + Debug>(value: &T, min: &T, max: &T) {
    assert!(
        value >= min && value <= max,
        "Expected {:?} to be in range [{:?}, {:?}]",
        value, min, max
    );
}

/// Assert value is greater than threshold
///
/// # Panics
///
/// Panics if the value is not greater than threshold
pub fn assert_greater_than<T: PartialOrd + Debug>(value: &T, threshold: &T) {
    assert!(
        value > threshold,
        "Expected {:?} to be greater than {:?}",
        value, threshold
    );
}

/// Assert value is less than threshold
///
/// # Panics
///
/// Panics if the value is not less than threshold
pub fn assert_less_than<T: PartialOrd + Debug>(value: &T, threshold: &T) {
    assert!(
        value < threshold,
        "Expected {:?} to be less than {:?}",
        value, threshold
    );
}

/// Assert that a result is Ok
///
/// # Panics
///
/// Panics if the result is Err
pub fn assert_ok<T: Debug, E: Debug>(result: &Result<T, E>) {
    assert!(result.is_ok(), "Expected Ok, but got Err: {:?}", result);
}

/// Assert that a result is Err
///
/// # Panics
///
/// Panics if the result is Ok
pub fn assert_err<T: Debug, E: Debug>(result: &Result<T, E>) {
    assert!(result.is_err(), "Expected Err, but got Ok: {:?}", result);
}

/// Assert that an option is Some
///
/// # Panics
///
/// Panics if the option is None
pub fn assert_some<T>(option: &Option<T>) {
    assert!(option.is_some(), "Expected Some, but got None");
}

/// Assert that an option is None
///
/// # Panics
///
/// Panics if the option is Some
pub fn assert_none<T: Debug>(option: &Option<T>) {
    assert!(option.is_none(), "Expected None, but got Some: {:?}", option);
}

/// Assert that two collections have the same elements (order-independent)
///
/// # Panics
///
/// Panics if the collections do not have the same elements
pub fn assert_same_elements<T: PartialEq + Debug>(a: &[T], b: &[T]) {
    assert_eq!(
        a.len(),
        b.len(),
        "Collections have different lengths: {} vs {}",
        a.len(),
        b.len()
    );
    for item in a {
        assert!(
            b.contains(item),
            "Collection b does not contain item from a: {:?}",
            item
        );
    }
}

/// Assert that a string contains a substring
///
/// # Panics
///
/// Panics if the string does not contain the substring
pub fn assert_str_contains(haystack: &str, needle: &str) {
    assert!(
        haystack.contains(needle),
        "Expected string to contain '{needle}', but it did not. String: {haystack}"
    );
}

/// Assert that a string does not contain a substring
///
/// # Panics
///
/// Panics if the string contains the substring
pub fn assert_str_not_contains(haystack: &str, needle: &str) {
    assert!(
        !haystack.contains(needle),
        "Expected string to not contain '{needle}', but it did. String: {haystack}"
    );
}

/// Assert that a string starts with a prefix
///
/// # Panics
///
/// Panics if the string does not start with the prefix
pub fn assert_str_starts_with(s: &str, prefix: &str) {
    assert!(
        s.starts_with(prefix),
        "Expected string to start with '{prefix}', but it did not. String: {s}"
    );
}

/// Assert that a string ends with a suffix
///
/// # Panics
///
/// Panics if the string does not end with the suffix
pub fn assert_str_ends_with(s: &str, suffix: &str) {
    assert!(
        s.ends_with(suffix),
        "Expected string to end with '{suffix}', but it did not. String: {s}"
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assert_state_changed() {
        let before = vec![1, 2, 3];
        let after = vec![1, 2, 3, 4];
        assert_state_changed(&before, &after);
    }

    #[test]
    fn test_assert_contains() {
        let collection = vec![1, 2, 3];
        assert_contains(&collection, &2);
    }

    #[test]
    fn test_assert_length() {
        let collection = vec![1, 2, 3];
        assert_length(&collection, 3);
    }

    #[test]
    fn test_assert_in_range() {
        assert_in_range(&5, &0, &10);
    }

    #[test]
    fn test_assert_ok() {
        let result: Result<i32, String> = Ok(42);
        assert_ok(&result);
    }

    #[test]
    fn test_assert_some() {
        let option = Some(42);
        assert_some(&option);
    }

    #[test]
    fn test_assert_str_contains() {
        assert_str_contains("hello world", "world");
    }
}
