#!/usr/bin/env rust-script
//! Bulk fix unwrap()/expect() violations in production code
//!
//! ```cargo
//! [dependencies]
//! regex = "1.10"
//! walkdir = "2.4"
//! ```

use regex::Regex;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

struct UnwrapFixer {
    // Pattern: .lock().unwrap()
    mutex_lock_pattern: Regex,
    // Pattern: Regex::new("pattern").unwrap()
    regex_new_pattern: Regex,
    // Pattern: NonZeroUsize::new(N).unwrap()
    nonzero_pattern: Regex,
    // Pattern: SystemTime::now().duration_since(UNIX_EPOCH).unwrap()
    systemtime_pattern: Regex,
    // Pattern: .partial_cmp(&other).unwrap()
    partial_cmp_pattern: Regex,
}

impl UnwrapFixer {
    fn new() -> Self {
        Self {
            mutex_lock_pattern: Regex::new(r"\.lock\(\)\.unwrap\(\)").unwrap(),
            regex_new_pattern: Regex::new(r#"Regex::new\((r#?"[^"]+)"#?\)\.unwrap\(\)"#).unwrap(),
            nonzero_pattern: Regex::new(r"NonZeroUsize::new\((\d+)\)\.unwrap\(\)").unwrap(),
            systemtime_pattern: Regex::new(
                r"SystemTime::now\(\)\.duration_since\(UNIX_EPOCH\)\.unwrap\(\)"
            ).unwrap(),
            partial_cmp_pattern: Regex::new(r"\.partial_cmp\(([^)]+)\)\.unwrap\(\)").unwrap(),
        }
    }

    fn fix_mutex_locks(&self, content: &str) -> String {
        // Replace: .lock().unwrap()
        // With: .lock().expect("Mutex poisoned")
        self.mutex_lock_pattern.replace_all(
            content,
            ".lock().expect(\"Mutex lock failed (poisoned)\")"
        ).to_string()
    }

    fn fix_regex_compilation(&self, content: &str) -> String {
        // Replace: Regex::new("pattern").unwrap()
        // With: Regex::new("pattern").expect("Invalid regex pattern")
        self.regex_new_pattern.replace_all(
            content,
            "Regex::new($1).expect(\"Invalid regex pattern\")"
        ).to_string()
    }

    fn fix_nonzero(&self, content: &str) -> String {
        // Replace: NonZeroUsize::new(1000).unwrap()
        // With: NonZeroUsize::new(1000).expect("1000 is non-zero")
        self.nonzero_pattern.replace_all(
            content,
            "NonZeroUsize::new($1).expect(\"$1 is non-zero\")"
        ).to_string()
    }

    fn fix_systemtime(&self, content: &str) -> String {
        // Replace: SystemTime::now().duration_since(UNIX_EPOCH).unwrap()
        // With: SystemTime::now().duration_since(UNIX_EPOCH).expect("System time before UNIX epoch")
        self.systemtime_pattern.replace_all(
            content,
            "SystemTime::now().duration_since(UNIX_EPOCH).expect(\"System time before UNIX epoch\")"
        ).to_string()
    }

    fn fix_partial_cmp(&self, content: &str) -> String {
        // Replace: .partial_cmp(&other).unwrap()
        // With: .partial_cmp(&other).expect("NaN comparison")
        self.partial_cmp_pattern.replace_all(
            content,
            ".partial_cmp($1).expect(\"NaN in comparison\")"
        ).to_string()
    }

    fn fix_file(&self, path: &Path) -> Result<(usize, String), String> {
        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

        // Skip if in test module or benchmark
        if content.contains("#[cfg(test)]") || content.contains("#[bench]") {
            return Ok((0, "Skipped (test/bench)".to_string()));
        }

        let original_content = content.clone();
        let mut fixed_content = content;
        let mut fixes = Vec::new();

        // Apply all fixes
        let temp = self.fix_mutex_locks(&fixed_content);
        if temp != fixed_content {
            fixes.push("mutex_locks");
            fixed_content = temp;
        }

        let temp = self.fix_regex_compilation(&fixed_content);
        if temp != fixed_content {
            fixes.push("regex_new");
            fixed_content = temp;
        }

        let temp = self.fix_nonzero(&fixed_content);
        if temp != fixed_content {
            fixes.push("nonzero");
            fixed_content = temp;
        }

        let temp = self.fix_systemtime(&fixed_content);
        if temp != fixed_content {
            fixes.push("systemtime");
            fixed_content = temp;
        }

        let temp = self.fix_partial_cmp(&fixed_content);
        if temp != fixed_content {
            fixes.push("partial_cmp");
            fixed_content = temp;
        }

        if fixed_content != original_content {
            // Write back
            fs::write(path, &fixed_content)
                .map_err(|e| format!("Failed to write {}: {}", path.display(), e))?;

            Ok((fixes.len(), fixes.join(", ")))
        } else {
            Ok((0, "No changes".to_string()))
        }
    }
}

fn should_process_file(path: &Path) -> bool {
    // Only process Rust source files, not tests/benches/examples
    if !path.extension().map_or(false, |e| e == "rs") {
        return false;
    }

    let path_str = path.to_string_lossy();

    // Skip test/bench/example directories
    if path_str.contains("/tests/")
        || path_str.contains("/benches/")
        || path_str.contains("/examples/")
        || path_str.contains("tests-archive")
        || path_str.contains("/target/")
    {
        return false;
    }

    // Only process src/ directories
    path_str.contains("/src/")
}

fn main() {
    let fixer = UnwrapFixer::new();
    let root = PathBuf::from(".");

    println!("🔧 Bulk Unwrap/Expect Fixer");
    println!("============================\n");
    println!("Scanning production Rust files...\n");

    let mut total_files = 0;
    let mut fixed_files = 0;
    let mut total_fixes = 0;

    for entry in WalkDir::new(&root)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();

        if !should_process_file(path) {
            continue;
        }

        total_files += 1;

        match fixer.fix_file(path) {
            Ok((count, details)) => {
                if count > 0 {
                    fixed_files += 1;
                    total_fixes += count;
                    println!("✅ {}: {} fix(es) [{}]",
                        path.display(),
                        count,
                        details
                    );
                }
            }
            Err(e) => {
                eprintln!("❌ {}: {}", path.display(), e);
            }
        }
    }

    println!("\n============================");
    println!("Summary:");
    println!("  Files scanned: {}", total_files);
    println!("  Files fixed:   {}", fixed_files);
    println!("  Total fixes:   {}", total_fixes);
    println!("\n✅ Run 'cargo make check' to verify!");
}
