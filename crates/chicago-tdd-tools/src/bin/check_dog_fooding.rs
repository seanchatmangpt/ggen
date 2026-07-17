#![allow(
    warnings,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::todo,
    clippy::unimplemented
)]
#![allow(missing_docs)] // Binary crate - documentation not required

//! Check for dog fooding violations in test files
//! Detects:
//! 1. Standard assertions (assert!, assert_eq!, assert_ne!) in test files
//! 2. #[test] and #[tokio::test] attributes in test files
//!
//! Exit code: 0 if no violations, 1 if violations found

use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let violations = check_dog_fooding();
    if violations > 0 {
        std::process::exit(1);
    }
}

fn check_dog_fooding() -> u32 {
    let mut violations = 0;

    println!("🔍 Checking for dog fooding violations...");

    // Check for standard assertions in test files
    println!("Checking for standard assertions (assert!, assert_eq!, assert_ne!)...");
    let standard_assertions = find_standard_assertions();
    if standard_assertions > 0 {
        println!("❌ Found {standard_assertions} instances of standard assertions in test files");
        println!("   Use library assertion macros instead: assert_ok!, assert_err!, assert_eq_msg!, assert_that!");
        violations += 1;
    } else {
        println!("✅ No standard assertions found");
    }

    // Check for #[test] attributes in test files
    println!();
    println!("Checking for #[test] attributes...");
    let test_attributes = find_test_attributes();
    if test_attributes > 0 {
        println!("❌ Found {test_attributes} instances of #[test] in test files");
        println!("   Use test! macro instead: test!(test_name, {{ /* AAA */ }})");
        violations += 1;
    } else {
        println!("✅ No #[test] attributes found");
    }

    // Check for #[tokio::test] attributes in test files
    println!();
    println!("Checking for #[tokio::test] attributes...");
    let tokio_test_attributes = find_tokio_test_attributes();
    if tokio_test_attributes > 0 {
        println!("❌ Found {tokio_test_attributes} instances of #[tokio::test] in test files");
        println!("   Use async_test! or fixture_test! macro instead: async_test!(test_name, {{ /* AAA */ }})");
        violations += 1;
    } else {
        println!("✅ No #[tokio::test] attributes found");
    }

    println!();
    if violations == 0 {
        println!("✅ All dog fooding checks passed!");
    } else {
        println!("❌ Found {violations} violation(s)");
        println!("   See above for details");
    }

    violations
}

fn find_standard_assertions() -> u32 {
    let mut count = 0;
    let patterns = [
        (r"assert!\(", "assert!"),
        (r"assert_eq!\(", "assert_eq!"),
        (r"assert_ne!\(", "assert_ne!"),
    ];

    for file in find_rust_files(&["tests/", "src/"]) {
        if let Ok(content) = fs::read_to_string(&file) {
            for (line_num, line) in content.lines().enumerate() {
                // Skip comments and stringify! macros (matches bash grep -v behavior)
                let trimmed = line.trim();
                if trimmed.starts_with("//!")
                    || trimmed.starts_with("//")
                    || line.contains("stringify!")
                    || line.contains("Binary")
                    || is_in_string_literal(line)
                {
                    continue;
                }

                for (pattern, _name) in &patterns {
                    if line.contains(pattern) {
                        println!("   {}:{}: {}", file.display(), line_num + 1, line.trim());
                        count += 1;
                    }
                }
            }
        }
    }

    count
}

fn find_test_attributes() -> u32 {
    let mut count = 0;

    for file in find_rust_files(&["tests/", "src/"]) {
        if let Ok(content) = fs::read_to_string(&file) {
            for (line_num, line) in content.lines().enumerate() {
                // Skip comments (matches bash grep -v behavior)
                if line.trim().starts_with("//!") || line.contains("Binary") {
                    continue;
                }

                // Match patterns: ^#\[test\] or ^\s+#\[test\] (matches bash regex)
                let trimmed = line.trim();
                if trimmed == "#[test]" || trimmed.starts_with("#[test]") {
                    println!("   {}:{}: {}", file.display(), line_num + 1, line.trim());
                    count += 1;
                }
            }
        }
    }

    count
}

fn find_tokio_test_attributes() -> u32 {
    let mut count = 0;

    for file in find_rust_files(&["tests/", "src/"]) {
        if let Ok(content) = fs::read_to_string(&file) {
            for (line_num, line) in content.lines().enumerate() {
                // Skip comments (matches bash grep -v behavior)
                if line.trim().starts_with("//!") || line.contains("Binary") {
                    continue;
                }

                // Match patterns: ^#\[tokio::test\] or ^\s+#\[tokio::test\] (matches bash regex)
                let trimmed = line.trim();
                if trimmed == "#[tokio::test]" || trimmed.starts_with("#[tokio::test]") {
                    println!("   {}:{}: {}", file.display(), line_num + 1, line.trim());
                    count += 1;
                }
            }
        }
    }

    count
}

fn find_rust_files(dirs: &[&str]) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for dir in dirs {
        let path = Path::new(dir);
        if path.exists() {
            walk_dir(path, &mut files);
        }
    }

    // Exclude check_dog_fooding.rs itself (it contains patterns that would be false positives)
    files.retain(|f| !f.to_string_lossy().contains("check_dog_fooding.rs"));

    files
}

/// Determines whether an `assert` pattern found on `line` occurs inside a string literal.
///
/// Strategy: scan the characters of the line left-to-right, tracking whether the current
/// position is inside a regular (`"…"`) or raw (`r#"…"#`, `r"…"`) string literal.
/// When the first occurrence of `assert` is found, return the current in-string state.
///
/// Known limitations (documented trade-offs, not hidden):
/// - Multi-line string literals are not detected; only the portion on this line is inspected.
/// - Byte strings (`b"…"`) are treated identically to `"…"`.
/// - String literals that begin on a previous line will not be detected as in-string context.
///   The caller already filters out comment lines (`//`, `//!`) and `stringify!` before calling
///   this function, so those cases are handled upstream.
fn is_in_string_literal(line: &str) -> bool {
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;
    let mut in_string = false;
    let mut raw_hashes: usize = 0; // number of `#` in the current raw-string delimiter

    while i < len {
        if in_string {
            if raw_hashes > 0 {
                // Inside a raw string: look for closing `"` followed by `raw_hashes` `#` chars.
                if chars[i] == '"' {
                    let closing_start = i + 1;
                    let available = len.saturating_sub(closing_start);
                    let actual_hashes = available.min(raw_hashes);
                    if actual_hashes == raw_hashes
                        && chars[closing_start..closing_start + raw_hashes]
                            .iter()
                            .all(|&c| c == '#')
                    {
                        // Closed raw string.
                        in_string = false;
                        i = closing_start + raw_hashes;
                        continue;
                    }
                }
                // Check if we're at the `assert` keyword inside this raw string.
                if chars[i..].iter().collect::<String>().starts_with("assert") {
                    return true;
                }
                i += 1;
            } else {
                // Inside a regular string.
                if chars[i] == '\\' {
                    // Escape sequence — skip both characters.
                    i += 2;
                    continue;
                }
                if chars[i] == '"' {
                    in_string = false;
                    i += 1;
                    continue;
                }
                // Check if we're at the `assert` keyword inside this string.
                if chars[i..].iter().collect::<String>().starts_with("assert") {
                    return true;
                }
                i += 1;
            }
        } else {
            // Outside a string.
            // Detect raw string: optional `b` then `r` then zero-or-more `#` then `"`.
            let rest: String = chars[i..].iter().collect();
            // Strip optional leading `b`.
            let scan_start = if rest.starts_with('b') { 1 } else { 0 };
            let after_b: &str = &rest[scan_start..];
            if after_b.starts_with('r') {
                let after_r = &after_b[1..];
                let hashes = after_r.chars().take_while(|&c| c == '#').count();
                let after_hashes = &after_r[hashes..];
                if after_hashes.starts_with('"') {
                    // Entering a raw string.
                    in_string = true;
                    raw_hashes = hashes;
                    i += scan_start + 1 + hashes + 1; // b? + r + #* + "
                    continue;
                }
            }
            // Detect regular (or byte) string.
            if rest.starts_with("b\"") || chars[i] == '"' {
                in_string = true;
                raw_hashes = 0;
                i += if rest.starts_with("b\"") { 2 } else { 1 };
                continue;
            }
            // Single-quoted char literals: skip `'x'` or `'\n'` etc.
            if chars[i] == '\'' {
                i += 1;
                if i < len && chars[i] == '\\' {
                    i += 1; // escape char
                }
                // Skip until closing `'`.
                while i < len && chars[i] != '\'' {
                    i += 1;
                }
                if i < len {
                    i += 1; // consume closing `'`
                }
                continue;
            }
            i += 1;
        }
    }
    false
}

fn walk_dir(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk_dir(&path, files);
            } else if path.extension() == Some(std::ffi::OsStr::new("rs")) {
                files.push(path);
            }
        }
    }
}
