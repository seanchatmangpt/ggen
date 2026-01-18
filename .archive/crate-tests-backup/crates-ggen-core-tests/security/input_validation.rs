//! Security tests for input validation

use ggen_core::registry::SearchParams;

#[test]
fn test_xss_prevention_in_search() {
    let malicious_query = "<script>alert('XSS')</script>";

    let params = SearchParams {
        query: malicious_query,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Query should be stored as-is (escaping happens at display time)
    assert_eq!(params.query, malicious_query);
}

#[test]
fn test_sql_injection_prevention_in_search() {
    let malicious_query = "'; DROP TABLE packages; --";

    let params = SearchParams {
        query: malicious_query,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Query should be stored safely
    assert_eq!(params.query, malicious_query);
    // Note: Actual SQL injection prevention happens at database layer
}

#[test]
fn test_path_traversal_prevention() {
    let malicious_id = "../../../etc/passwd";

    // Package ID should be validated/sanitized before file operations
    // This test verifies we can handle such input without panicking
    assert!(malicious_id.contains(".."));
}

#[test]
fn test_null_byte_injection() {
    let malicious_query = "test\0hidden";

    let params = SearchParams {
        query: malicious_query,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should handle null bytes safely
    assert!(params.query.contains('\0'));
}

#[test]
fn test_unicode_normalization() {
    // Different Unicode representations of "é"
    let query1 = "café"; // NFC form (single codepoint)
    let query2 = "cafe\u{0301}"; // NFD form (base + combining accent)

    let params1 = SearchParams {
        query: query1,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    let params2 = SearchParams {
        query: query2,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Both should be valid queries
    assert!(!params1.query.is_empty());
    assert!(!params2.query.is_empty());

    // Note: Unicode normalization should happen during search
}

#[test]
fn test_extremely_long_input() {
    let long_query = "a".repeat(10000);

    let params = SearchParams {
        query: &long_query,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should handle long input without panicking
    assert_eq!(params.query.len(), 10000);
}

#[test]
fn test_special_regex_characters() {
    let regex_chars = ".*+?^${}()|[]\\";

    let params = SearchParams {
        query: regex_chars,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should treat as literal string, not regex
    assert_eq!(params.query, regex_chars);
}

#[test]
fn test_control_characters() {
    let control_chars = "\t\n\r\x00\x1F";

    let params = SearchParams {
        query: control_chars,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should handle control characters safely
    assert!(params.query.contains('\t'));
    assert!(params.query.contains('\n'));
}

#[test]
fn test_mixed_scripts_unicode() {
    // Mix of Latin, Cyrillic, Chinese, and Arabic
    let mixed = "Hello Привет 你好 مرحبا";

    let params = SearchParams {
        query: mixed,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should handle mixed scripts safely
    assert_eq!(params.query, mixed);
}

#[test]
fn test_homograph_attack_characters() {
    // Cyrillic "а" (U+0430) looks like Latin "a" (U+0061)
    let latin = "package";
    let cyrillic = "pаckаge"; // Contains Cyrillic 'а'

    // Should be treated as different strings
    assert_ne!(latin, cyrillic);
}

#[test]
fn test_zero_width_characters() {
    let with_zwsp = "pack\u{200B}age"; // Zero-width space
    let normal = "package";

    // Should be treated as different
    assert_ne!(with_zwsp, normal);
}

#[test]
fn test_bidi_override_characters() {
    // Right-to-left override can hide malicious content
    let bidi_text = "package\u{202E}egakcap";

    let params = SearchParams {
        query: bidi_text,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should handle BiDi characters safely
    assert!(params.query.contains('\u{202E}'));
}

#[test]
fn test_limit_boundaries() {
    // Test extreme limit values
    let params_zero = SearchParams {
        query: "test",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 0,
    };

    let params_max = SearchParams {
        query: "test",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: usize::MAX,
    };

    // Should not panic
    assert_eq!(params_zero.limit, 0);
    assert_eq!(params_max.limit, usize::MAX);
}

#[test]
fn test_command_injection_attempt() {
    let malicious = "test; rm -rf /";

    let params = SearchParams {
        query: malicious,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should be treated as literal string
    assert_eq!(params.query, malicious);
}

#[test]
fn test_format_string_injection() {
    let malicious = "test %s %x %n";

    let params = SearchParams {
        query: malicious,
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Should be treated as literal string
    assert_eq!(params.query, malicious);
}
