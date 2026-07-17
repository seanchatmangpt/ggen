//! Collection Assertion Macros
//!
//! **New in v1.3.0**: Assertions for testing collection membership and relationships.

/// Assert that a collection contains an item
///
/// **New in v1.3.0**: Simplifies common collection testing scenarios.
///
/// Provides better error messages than manual iteration checks.
/// Works with any type that implements `IntoIterator` where items implement `PartialEq + Debug`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_contains;
///
/// let numbers = vec![1, 2, 3, 4, 5];
/// assert_contains!(numbers, 3);
///
/// // With custom message
/// let users = vec!["alice", "bob", "charlie"];
/// assert_contains!(users, "bob", "User should exist");
/// ```
#[macro_export]
macro_rules! assert_contains {
    ($collection:expr, $item:expr) => {{
        let collection_ref = &$collection;
        let item_ref = &$item;
        let found = collection_ref.into_iter().any(|x| x == item_ref);
        if !found {
            panic!(
                "Collection does not contain item.\n  collection: {:?}\n  missing item: {:?}",
                collection_ref, item_ref
            );
        }
    }};
    ($collection:expr, $item:expr, $msg:expr) => {{
        let collection_ref = &$collection;
        let item_ref = &$item;
        let found = collection_ref.into_iter().any(|x| x == item_ref);
        if !found {
            panic!(
                "{}: Collection does not contain item.\n  collection: {:?}\n  missing item: {:?}",
                $msg, collection_ref, item_ref
            );
        }
    }};
}

/// Assert that a collection does not contain an item
///
/// **New in v1.3.0**: Inverse of `assert_contains!`.
///
/// Provides better error messages for negative collection checks.
/// Works with any type that implements `IntoIterator` where items implement `PartialEq + Debug`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_not_contains;
///
/// let numbers = vec![1, 2, 3, 4, 5];
/// assert_not_contains!(numbers, 6);
///
/// // With custom message
/// let banned_users = vec!["alice", "bob"];
/// assert_not_contains!(banned_users, "charlie", "User should not be banned");
/// ```
#[macro_export]
macro_rules! assert_not_contains {
    ($collection:expr, $item:expr) => {{
        let collection_ref = &$collection;
        let item_ref = &$item;
        let found = collection_ref.into_iter().any(|x| x == item_ref);
        if found {
            panic!(
                "Collection contains item that should not be present.\n  collection: {:?}\n  unexpected item: {:?}",
                collection_ref, item_ref
            );
        }
    }};
    ($collection:expr, $item:expr, $msg:expr) => {{
        let collection_ref = &$collection;
        let item_ref = &$item;
        let found = collection_ref.into_iter().any(|x| x == item_ref);
        if found {
            panic!(
                "{}: Collection contains item that should not be present.\n  collection: {:?}\n  unexpected item: {:?}",
                $msg, collection_ref, item_ref
            );
        }
    }};
}

/// Assert that one collection is a subset of another
///
/// **New in v1.3.0**: Validates subset relationships between collections.
///
/// Checks that all items in `subset` are present in `superset`.
/// Works with any type that implements `IntoIterator` where items implement `PartialEq + Debug`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_subset;
///
/// let all_features = vec!["feature_a", "feature_b", "feature_c"];
/// let enabled_features = vec!["feature_a", "feature_c"];
/// assert_subset!(enabled_features, all_features);
///
/// // With custom message
/// let allowed_roles = vec!["admin", "user", "guest"];
/// let user_roles = vec!["user"];
/// assert_subset!(user_roles, allowed_roles, "User roles must be allowed");
/// ```
#[macro_export]
macro_rules! assert_subset {
    ($subset:expr, $superset:expr) => {{
        let subset_ref = &$subset;
        let superset_ref = &$superset;
        let subset_vec: Vec<_> = subset_ref.into_iter().collect();
        let superset_vec: Vec<_> = superset_ref.into_iter().collect();

        let missing: Vec<_> = subset_vec
            .iter()
            .filter(|item| !superset_vec.contains(item))
            .collect();

        if !missing.is_empty() {
            panic!(
                "Subset contains items not in superset.\n  subset: {:?}\n  superset: {:?}\n  missing from superset: {:?}",
                subset_vec, superset_vec, missing
            );
        }
    }};
    ($subset:expr, $superset:expr, $msg:expr) => {{
        let subset_ref = &$subset;
        let superset_ref = &$superset;
        let subset_vec: Vec<_> = subset_ref.into_iter().collect();
        let superset_vec: Vec<_> = superset_ref.into_iter().collect();

        let missing: Vec<_> = subset_vec
            .iter()
            .filter(|item| !superset_vec.contains(item))
            .collect();

        if !missing.is_empty() {
            panic!(
                "{}: Subset contains items not in superset.\n  subset: {:?}\n  superset: {:?}\n  missing from superset: {:?}",
                $msg, subset_vec, superset_vec, missing
            );
        }
    }};
}

/// Assert that one collection is a superset of another
///
/// **New in v1.3.0**: Validates superset relationships between collections.
///
/// Checks that all items in `subset` are present in `superset`.
/// This is the inverse of `assert_subset!` for better readability in some contexts.
/// Works with any type that implements `IntoIterator` where items implement `PartialEq + Debug`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_superset;
///
/// let all_features = vec!["feature_a", "feature_b", "feature_c"];
/// let enabled_features = vec!["feature_a", "feature_c"];
/// assert_superset!(all_features, enabled_features);
///
/// // With custom message
/// let all_permissions = vec!["read", "write", "execute"];
/// let user_permissions = vec!["read"];
/// assert_superset!(all_permissions, user_permissions, "All permissions must include user permissions");
/// ```
#[macro_export]
macro_rules! assert_superset {
    ($superset:expr, $subset:expr) => {{
        $crate::assert_subset!($subset, $superset);
    }};
    ($superset:expr, $subset:expr, $msg:expr) => {{
        $crate::assert_subset!($subset, $superset, $msg);
    }};
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    test!(test_assert_contains_macro, {
        // Arrange: Collection with items
        let numbers = vec![1, 2, 3, 4, 5];
        let users = vec!["alice", "bob", "charlie"];

        // Act & Assert: Verify assert_contains! macro works
        assert_contains!(numbers, 3);
        assert_contains!(users, "bob");
        assert_contains!(users, "bob", "User should exist");
    });

    #[test]
    #[should_panic(expected = "Collection does not contain item")]
    fn test_assert_contains_macro_fails() {
        // Arrange: Collection without target item
        let numbers = vec![1, 2, 3];

        // Act & Assert: Should panic
        assert_contains!(numbers, 5);
    }

    test!(test_assert_not_contains_macro, {
        // Arrange: Collection without certain items
        let numbers = vec![1, 2, 3, 4, 5];
        let banned_users = vec!["alice", "bob"];

        // Act & Assert: Verify assert_not_contains! macro works
        assert_not_contains!(numbers, 6);
        assert_not_contains!(banned_users, "charlie");
        assert_not_contains!(banned_users, "charlie", "User should not be banned");
    });

    #[test]
    #[should_panic(expected = "Collection contains item that should not be present")]
    fn test_assert_not_contains_macro_fails() {
        // Arrange: Collection with item
        let numbers = vec![1, 2, 3];

        // Act & Assert: Should panic
        assert_not_contains!(numbers, 2);
    }

    test!(test_assert_subset_macro, {
        // Arrange: Subset and superset collections
        let all_features = vec!["feature_a", "feature_b", "feature_c"];
        let enabled_features = vec!["feature_a", "feature_c"];
        let all_roles = vec!["admin", "user", "guest"];
        let user_roles = vec!["user"];

        // Act & Assert: Verify assert_subset! macro works
        assert_subset!(enabled_features, all_features);
        assert_subset!(user_roles, all_roles);
        assert_subset!(user_roles, all_roles, "User roles must be allowed");
    });

    #[test]
    #[should_panic(expected = "Subset contains items not in superset")]
    fn test_assert_subset_macro_fails() {
        // Arrange: Invalid subset (contains items not in superset)
        let superset = vec![1, 2, 3];
        let subset = vec![2, 3, 4]; // 4 is not in superset

        // Act & Assert: Should panic
        assert_subset!(subset, superset);
    }

    test!(test_assert_superset_macro, {
        // Arrange: Superset and subset collections
        let all_features = vec!["feature_a", "feature_b", "feature_c"];
        let enabled_features = vec!["feature_a", "feature_c"];
        let all_permissions = vec!["read", "write", "execute"];
        let user_permissions = vec!["read"];

        // Act & Assert: Verify assert_superset! macro works
        assert_superset!(all_features, enabled_features);
        assert_superset!(all_permissions, user_permissions);
        assert_superset!(
            all_permissions,
            user_permissions,
            "All permissions must include user permissions"
        );
    });

    #[test]
    #[should_panic(expected = "Subset contains items not in superset")]
    fn test_assert_superset_macro_fails() {
        // Arrange: Invalid relationship (subset has items not in superset)
        let superset = vec![1, 2, 3];
        let subset = vec![2, 3, 4]; // 4 is not in superset

        // Act & Assert: Should panic
        assert_superset!(superset, subset);
    }
}
