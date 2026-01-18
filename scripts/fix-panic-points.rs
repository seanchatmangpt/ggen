#!/usr/bin/env rust-script
//! # Panic Point Auto-Fixer
//!
//! Automatically fixes .expect() and .unwrap() calls in Rust code.
//! Uses ggen's own template system for safe error handling patterns.
//!
//! ```cargo
//! [dependencies]
//! anyhow = "1.0"
//! regex = "1.10"
//! walkdir = "2.4"
//! syn = "2.0"
//! quote = "1.0"
//! ```

use anyhow::{Context, Result};
use regex::Regex;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(Debug)]
struct PanicPoint {
    file: PathBuf,
    line: usize,
    column: usize,
    pattern: PanicPattern,
    context: String,
}

#[derive(Debug, Clone)]
enum PanicPattern {
    Expect(String), // The error message in expect()
    Unwrap,
    UnwrapOr,      // Already safe, just log
    UnwrapOrElse,  // Already safe, just log
}

struct Fixer {
    dry_run: bool,
    verbose: bool,
    stats: FixStats,
}

#[derive(Debug, Default)]
struct FixStats {
    files_scanned: usize,
    panic_points_found: usize,
    panic_points_fixed: usize,
    already_safe: usize,
}

impl Fixer {
    fn new(dry_run: bool, verbose: bool) -> Self {
        Self {
            dry_run,
            verbose,
            stats: FixStats::default(),
        }
    }

    /// Scan a directory for Rust files with panic points
    fn scan_directory(&mut self, dir: &Path) -> Result<Vec<PanicPoint>> {
        let mut panic_points = Vec::new();

        for entry in WalkDir::new(dir)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            if !entry.file_type().is_file() {
                continue;
            }

            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) != Some("rs") {
                continue;
            }

            // Skip test files
            if path.to_string_lossy().contains("/tests/")
                || path.to_string_lossy().contains("_test.rs")
                || path.to_string_lossy().contains("/benches/") {
                continue;
            }

            self.stats.files_scanned += 1;

            if let Ok(points) = self.scan_file(path) {
                panic_points.extend(points);
            }
        }

        Ok(panic_points)
    }

    /// Scan a single file for panic points
    fn scan_file(&self, path: &Path) -> Result<Vec<PanicPoint>> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {:?}", path))?;

        let mut panic_points = Vec::new();

        // Regex patterns for different panic points
        let expect_re = Regex::new(r#"\.expect\("([^"]*)"\)"#)?;
        let unwrap_re = Regex::new(r"\.unwrap\(\)")?;
        let unwrap_or_re = Regex::new(r"\.unwrap_or\(")?;
        let unwrap_or_else_re = Regex::new(r"\.unwrap_or_else\(")?;

        for (line_num, line) in content.lines().enumerate() {
            let line_num = line_num + 1;

            // Skip lines with SAFE comment
            if line.contains("// SAFE:") {
                continue;
            }

            // Skip #[cfg(test)]
            if line.contains("#[cfg(test)]") {
                continue;
            }

            // Check for .expect()
            if let Some(cap) = expect_re.captures(line) {
                let message = cap.get(1).map(|m| m.as_str()).unwrap_or("");
                panic_points.push(PanicPoint {
                    file: path.to_path_buf(),
                    line: line_num,
                    column: line.find(".expect(").unwrap_or(0),
                    pattern: PanicPattern::Expect(message.to_string()),
                    context: line.trim().to_string(),
                });
            }

            // Check for .unwrap()
            if unwrap_re.is_match(line) {
                // Check if it's already safe (unwrap_or or unwrap_or_else)
                if unwrap_or_re.is_match(line) || unwrap_or_else_re.is_match(line) {
                    panic_points.push(PanicPoint {
                        file: path.to_path_buf(),
                        line: line_num,
                        column: line.find(".unwrap_or").unwrap_or(0),
                        pattern: PanicPattern::UnwrapOr,
                        context: line.trim().to_string(),
                    });
                } else {
                    panic_points.push(PanicPoint {
                        file: path.to_path_buf(),
                        line: line_num,
                        column: line.find(".unwrap()").unwrap_or(0),
                        pattern: PanicPattern::Unwrap,
                        context: line.trim().to_string(),
                    });
                }
            }
        }

        Ok(panic_points)
    }

    /// Fix panic points in a file
    fn fix_file(&mut self, path: &Path, points: &[PanicPoint]) -> Result<()> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {:?}", path))?;

        let mut lines: Vec<String> = content.lines().map(String::from).collect();
        let mut fixed_count = 0;

        // Sort points by line number in reverse order to avoid index shifting
        let mut sorted_points = points.to_vec();
        sorted_points.sort_by(|a, b| b.line.cmp(&a.line));

        for point in sorted_points {
            let line_idx = point.line - 1;
            if line_idx >= lines.len() {
                continue;
            }

            let line = &lines[line_idx];
            let fixed_line = match &point.pattern {
                PanicPattern::Expect(msg) => {
                    self.fix_expect(line, msg)
                }
                PanicPattern::Unwrap => {
                    self.fix_unwrap(line)
                }
                PanicPattern::UnwrapOr | PanicPattern::UnwrapOrElse => {
                    self.stats.already_safe += 1;
                    continue; // Already safe
                }
            };

            if let Some(fixed) = fixed_line {
                lines[line_idx] = fixed;
                fixed_count += 1;
            }
        }

        if fixed_count > 0 {
            let fixed_content = lines.join("\n") + "\n";

            if !self.dry_run {
                fs::write(path, fixed_content)
                    .with_context(|| format!("Failed to write file: {:?}", path))?;

                if self.verbose {
                    println!("✅ Fixed {} panic points in {:?}", fixed_count, path);
                }
            } else {
                println!("🔍 Would fix {} panic points in {:?}", fixed_count, path);
            }

            self.stats.panic_points_fixed += fixed_count;
        }

        Ok(())
    }

    /// Fix .expect() call
    fn fix_expect(&self, line: &str, _msg: &str) -> Option<String> {
        // Replace .expect("msg") with .map_err(|e| anyhow!("msg: {}", e))?
        if let Some(pos) = line.find(".expect(") {
            let before = &line[..pos];
            let after_start = line[pos..].find(')').map(|p| pos + p + 1)?;
            let after = &line[after_start..];

            // Extract the error message
            let expect_part = &line[pos..after_start];
            let msg = expect_part
                .trim_start_matches(".expect(\"")
                .trim_end_matches("\")")
                .to_string();

            // Generate safe replacement
            let replacement = format!(
                "{}.map_err(|e| anyhow::anyhow!(\"{}: {{}}\", e))?{}",
                before.trim_end(),
                msg,
                after
            );

            Some(replacement)
        } else {
            None
        }
    }

    /// Fix .unwrap() call
    fn fix_unwrap(&self, line: &str) -> Option<String> {
        // Replace .unwrap() with .unwrap_or_default() or proper error handling
        if let Some(pos) = line.find(".unwrap()") {
            let before = &line[..pos];
            let after = &line[pos + 9..]; // ".unwrap()" is 9 chars

            // Heuristic: If it looks like an Option, use unwrap_or_default()
            // Otherwise, suggest using ? operator
            let replacement = if before.contains("Option") || before.contains("Some(") {
                format!("{}.unwrap_or_default(){}", before.trim_end(), after)
            } else {
                // For Result types, use proper error handling
                format!("{}?{}", before.trim_end(), after)
            };

            Some(replacement)
        } else {
            None
        }
    }

    /// Run the fixer
    fn run(&mut self, dirs: &[PathBuf]) -> Result<()> {
        println!("🔍 Scanning for panic points...\n");

        let mut all_panic_points = Vec::new();

        for dir in dirs {
            let points = self.scan_directory(dir)?;
            all_panic_points.extend(points);
        }

        self.stats.panic_points_found = all_panic_points.len();

        println!("📊 Scan Results:");
        println!("  Files scanned: {}", self.stats.files_scanned);
        println!("  Panic points found: {}", self.stats.panic_points_found);
        println!("  Already safe: {}", self.stats.already_safe);
        println!();

        if all_panic_points.is_empty() {
            println!("✅ No panic points found!");
            return Ok(());
        }

        // Group by file
        let mut by_file: std::collections::HashMap<PathBuf, Vec<PanicPoint>> =
            std::collections::HashMap::new();

        for point in all_panic_points {
            by_file.entry(point.file.clone()).or_default().push(point);
        }

        println!("🔧 Fixing panic points...\n");

        for (file, points) in by_file {
            self.fix_file(&file, &points)?;
        }

        println!("\n📊 Fix Summary:");
        println!("  Panic points fixed: {}", self.stats.panic_points_fixed);
        println!("  Already safe: {}", self.stats.already_safe);

        if self.dry_run {
            println!("\n⚠️  DRY RUN: No files were modified");
            println!("   Run without --dry-run to apply fixes");
        }

        Ok(())
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let dry_run = args.contains(&"--dry-run".to_string());
    let verbose = args.contains(&"--verbose".to_string()) || args.contains(&"-v".to_string());

    let dirs: Vec<PathBuf> = if args.len() > 1 {
        args.iter()
            .skip(1)
            .filter(|a| !a.starts_with("--") && !a.starts_with("-"))
            .map(PathBuf::from)
            .collect()
    } else {
        vec![
            PathBuf::from("cli/src"),
            PathBuf::from("ggen-core/src"),
            PathBuf::from("ggen-ai/src"),
        ]
    };

    println!("🚀 Ggen Panic Point Auto-Fixer\n");

    let mut fixer = Fixer::new(dry_run, verbose);
    fixer.run(&dirs)?;

    Ok(())
}
