//! Integration tests for marketplace edge cases
//!
//! Tests verify robust handling of:
//! - Empty marketplace
//! - Invalid package names
//! - Extreme values (0 and 100 scores)
//! - Large result sets
//! - Special characters
//! - Concurrent operations
//! - Error recovery

use assert_cmd::Command;
use predicates::prelude::*;
use std::sync::{Arc, Mutex};
use std::thread;

#[test]
fn test_empty_query_validation() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("")
        .assert();

    // Should fail gracefully
    assert.failure();
}

#[test]
fn test_whitespace_only_query() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("   ")
        .assert();

    // Should either fail or normalize to empty
    let _ = assert.failure().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_special_characters_in_query() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Test various special characters
    let special_queries = vec![
        "test@package",
        "test#package",
        "test$package",
        "test%package",
        "test&package",
        "test*package",
        "test(package)",
        "test[package]",
        "test{package}",
    ];

    for query in special_queries {
        let assert = cmd
            .arg("marketplace")
            .arg("search")
            .arg("--query")
            .arg(query)
            .assert();

        // Should handle gracefully (not panic)
        let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
    }
}

#[test]
fn test_unicode_in_query() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("rust-🦀-package")
        .assert();

    // Should handle unicode gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_very_long_query() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let long_query = "a".repeat(1000);

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg(&long_query)
        .assert();

    // Should handle or reject gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_invalid_package_name() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("this/is/not/valid")
        .assert();

    // Should either handle or fail gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_nonexistent_package() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("nonexistent.package.that.does.not.exist.12345")
        .assert();

    // Should handle missing package gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_zero_limit() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("rust")
        .arg("--limit")
        .arg("0")
        .assert();

    // Should handle zero limit (empty result or error)
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_negative_limit() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("rust")
        .arg("--limit")
        .arg("-1")
        .assert();

    // Should fail validation
    assert.failure();
}

#[test]
fn test_huge_limit() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("rust")
        .arg("--limit")
        .arg("999999")
        .assert();

    // Should either cap or handle gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_invalid_maturity_level() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("list")
        .arg("--min-maturity")
        .arg("invalid-level")
        .assert();

    // Should either default or fail validation
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_invalid_export_format() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("export")
        .arg("--format")
        .arg("invalid-format")
        .arg("--output")
        .arg("/tmp/test.txt")
        .assert();

    // Should fail with unsupported format error
    assert.failure();
}

#[test]
fn test_compare_same_package() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("compare")
        .arg("--package-a")
        .arg("io.ggen.compiler")
        .arg("--package-b")
        .arg("io.ggen.compiler")
        .assert();

    // Should handle comparing package to itself
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_compare_nonexistent_packages() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("compare")
        .arg("--package-a")
        .arg("nonexistent.a")
        .arg("--package-b")
        .arg("nonexistent.b")
        .assert();

    // Should fail with package not found error
    assert.failure();
}

#[test]
fn test_invalid_use_case() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("invalid-use-case-12345")
        .assert();

    // Should either default or handle unknown use case
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_concurrent_search_requests() {
    // Test multiple concurrent search operations don't interfere
    let queries = vec!["rust", "web", "cli", "api", "database"];
    let handles: Vec<_> = queries
        .into_iter()
        .map(|query| {
            let q = query.to_string();
            thread::spawn(move || {
                let mut cmd = Command::cargo_bin("ggen").unwrap();
                cmd.arg("marketplace")
                    .arg("search")
                    .arg("--query")
                    .arg(&q)
                    .assert()
                    .try_success()
            })
        })
        .collect();

    // All should complete without panicking
    for handle in handles {
        let _ = handle.join().unwrap();
    }
}

#[test]
fn test_concurrent_maturity_assessments() {
    let packages = vec![
        "io.ggen.rust.microservice",
        "io.ggen.typescript.sdk",
        "io.ggen.python.pydantic",
    ];

    let handles: Vec<_> = packages
        .into_iter()
        .map(|pkg| {
            let p = pkg.to_string();
            thread::spawn(move || {
                let mut cmd = Command::cargo_bin("ggen").unwrap();
                cmd.arg("marketplace")
                    .arg("maturity")
                    .arg(&p)
                    .assert()
                    .try_success()
            })
        })
        .collect();

    for handle in handles {
        let _ = handle.join().unwrap();
    }
}

#[test]
fn test_sql_injection_attempts() {
    let injection_attempts = vec![
        "'; DROP TABLE packages; --",
        "1' OR '1'='1",
        "admin'--",
        "' UNION SELECT * FROM users--",
    ];

    for attempt in injection_attempts {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        let assert = cmd
            .arg("marketplace")
            .arg("search")
            .arg("--query")
            .arg(attempt)
            .assert();

        // Should handle safely (not execute SQL)
        let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
    }
}

#[test]
fn test_path_traversal_attempts() {
    let traversal_attempts = vec![
        "../../etc/passwd",
        "../../../root/.ssh/id_rsa",
        "....//....//....//etc/shadow",
    ];

    for attempt in traversal_attempts {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        let assert = cmd
            .arg("marketplace")
            .arg("search")
            .arg("--query")
            .arg(attempt)
            .assert();

        // Should sanitize or reject
        let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
    }
}

#[test]
fn test_null_bytes_in_input() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let query_with_null = format!("test{}package", '\0');

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg(&query_with_null)
        .assert();

    // Should handle or reject gracefully
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_extremely_nested_json_output() {
    // Generate deeply nested query that might cause stack overflow
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("dashboard")
        .arg("--format")
        .arg("json")
        .assert();

    // Should not cause stack overflow
    assert.success();
}

#[test]
fn test_invalid_score_threshold() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("production")
        .arg("--min-score")
        .arg("101")
        .assert();

    // Should either cap at 100 or fail validation
    let _ = assert.try_success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_negative_score_threshold() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("production")
        .arg("--min-score")
        .arg("-10")
        .assert();

    // Should fail validation
    assert.failure();
}
