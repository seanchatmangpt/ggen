#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Unit tests for search parameters and filtering

use chicago_tdd_tools::prelude::*;
use ggen_core::registry::SearchParams;

test!(test_search_params_creation, {
    // Arrange
    let params = SearchParams {
        query: "rust",
        category: Some("cli"),
        keyword: Some("clap"),
        author: Some("test-author"),
        stable_only: true,
        limit: 10,
    };

    // Assert
    assert_eq!(params.query, "rust");
    assert_eq!(params.category, Some("cli"));
    assert_eq!(params.keyword, Some("clap"));
    assert_eq!(params.author, Some("test-author"));
    assert!(params.stable_only);
    assert_eq!(params.limit, 10);
});

test!(test_search_params_minimal, {
    // Arrange
    let params = SearchParams {
        query: "test",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 50,
    };

    // Assert
    assert_eq!(params.query, "test");
    assert!(params.category.is_none());
    assert!(params.keyword.is_none());
    assert!(params.author.is_none());
    assert!(!params.stable_only);
    assert_eq!(params.limit, 50);
});

test!(test_search_params_empty_query, {
    // Arrange
    let params = SearchParams {
        query: "",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Assert
    assert_eq!(params.query, "");
});

test!(test_search_params_special_characters, {
    // Arrange
    let params = SearchParams {
        query: "rust-cli/tool@1.0",
        category: Some("cli/tools"),
        keyword: Some("command-line"),
        author: Some("user@example.com"),
        stable_only: false,
        limit: 10,
    };

    // Assert
    assert!(params.query.contains('/'));
    assert!(params.query.contains('@'));
    assert!(params.category.unwrap().contains('/'));
});

test!(test_search_params_case_sensitivity, {
    // Arrange
    let params1 = SearchParams {
        query: "RUST",
        category: Some("CLI"),
        keyword: Some("CLAP"),
        author: Some("TEST-AUTHOR"),
        stable_only: false,
        limit: 10,
    };

    let params2 = SearchParams {
        query: "rust",
        category: Some("cli"),
        keyword: Some("clap"),
        author: Some("test-author"),
        stable_only: false,
        limit: 10,
    };

    // Assert
    assert_ne!(params1.query, params2.query);
    assert_ne!(params1.category, params2.category);
});

test!(test_search_params_unicode, {
    // Arrange
    let params = SearchParams {
        query: "Rust 🦀",
        category: Some("CLI ⚡"),
        keyword: Some("命令行"),
        author: Some("Håkon"),
        stable_only: false,
        limit: 10,
    };

    // Assert
    assert!(params.query.contains("🦀"));
    assert!(params.category.unwrap().contains("⚡"));
    assert!(params.keyword.unwrap().contains("命令行"));
});

test!(test_search_params_limit_boundaries, {
    // Arrange
    let params_zero = SearchParams {
        query: "test",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 0,
    };

    let params_large = SearchParams {
        query: "test",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 1000,
    };

    // Assert
    assert_eq!(params_zero.limit, 0);
    assert_eq!(params_large.limit, 1000);
});

test!(test_search_params_whitespace, {
    // Arrange
    let params = SearchParams {
        query: "  rust cli  ",
        category: Some("  tools  "),
        keyword: Some("  command-line  "),
        author: Some("  author  "),
        stable_only: false,
        limit: 10,
    };

    // Assert
    assert_eq!(params.query, "  rust cli  ");
    assert_eq!(params.category, Some("  tools  "));
});
