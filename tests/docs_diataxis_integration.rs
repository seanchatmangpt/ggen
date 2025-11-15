//! Comprehensive integration tests for Diataxis documentation refactoring
//!
//! This test suite validates that the ggen documentation:
//! 1. Follows Diataxis framework structure (Tutorials, How-to Guides, Reference, Explanations)
//! 2. Contains all required documentation files
//! 3. Has valid cross-references and links
//! 4. Meets content quality standards
//! 5. Maintains navigation consistency
//!
//! Enterprise-grade validation suitable for Fortune 500 deployments.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Represents a documentation file with metadata
#[derive(Debug, Clone)]
struct DocFile {
    path: PathBuf,
    category: DocCategory,
    title: String,
    links: Vec<String>,
    word_count: usize,
}

/// Diataxis documentation categories
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DocCategory {
    Tutorial,
    HowToGuide,
    Reference,
    Explanation,
    Index,
}

impl DocCategory {
    fn from_path(path: &Path) -> Option<Self> {
        let path_str = path.to_string_lossy();
        if path_str.contains("tutorials/") {
            Some(DocCategory::Tutorial)
        } else if path_str.contains("how-to-guides/") {
            Some(DocCategory::HowToGuide)
        } else if path_str.contains("reference/") {
            Some(DocCategory::Reference)
        } else if path_str.contains("explanations/") {
            Some(DocCategory::Explanation)
        } else if path_str.ends_with("README.md") {
            Some(DocCategory::Index)
        } else {
            None
        }
    }

    fn name(&self) -> &'static str {
        match self {
            DocCategory::Tutorial => "Tutorial",
            DocCategory::HowToGuide => "How-to Guide",
            DocCategory::Reference => "Reference",
            DocCategory::Explanation => "Explanation",
            DocCategory::Index => "Index",
        }
    }

    fn expected_characteristics(&self) -> DocumentCharacteristics {
        match self {
            DocCategory::Tutorial => DocumentCharacteristics {
                min_word_count: 800,
                max_word_count: 5000,
                must_contain_headings: vec!["Prerequisites", "Step"],
                should_have_examples: true,
                orientation: "Learning-oriented",
            },
            DocCategory::HowToGuide => DocumentCharacteristics {
                min_word_count: 600,
                max_word_count: 4000,
                must_contain_headings: vec!["Problem", "Step"],
                should_have_examples: true,
                orientation: "Task-oriented",
            },
            DocCategory::Reference => DocumentCharacteristics {
                min_word_count: 500,
                max_word_count: 10000,
                must_contain_headings: vec![],
                should_have_examples: true,
                orientation: "Information-oriented",
            },
            DocCategory::Explanation => DocumentCharacteristics {
                min_word_count: 800,
                max_word_count: 6000,
                must_contain_headings: vec!["What", "Why"],
                should_have_examples: false,
                orientation: "Understanding-oriented",
            },
            DocCategory::Index => DocumentCharacteristics {
                min_word_count: 400,
                max_word_count: 3000,
                must_contain_headings: vec!["Documentation"],
                should_have_examples: false,
                orientation: "Navigation",
            },
        }
    }
}

/// Expected characteristics for documents in each category
#[derive(Debug)]
struct DocumentCharacteristics {
    min_word_count: usize,
    max_word_count: usize,
    must_contain_headings: Vec<&'static str>,
    should_have_examples: bool,
    orientation: &'static str,
}

/// Test result with detailed failure information
struct TestResult {
    passed: bool,
    category: String,
    checks: Vec<CheckResult>,
}

#[derive(Debug)]
struct CheckResult {
    name: String,
    passed: bool,
    message: String,
}

impl TestResult {
    fn new(category: String) -> Self {
        TestResult {
            passed: true,
            category,
            checks: Vec::new(),
        }
    }

    fn add_check(&mut self, name: String, passed: bool, message: String) {
        if !passed {
            self.passed = false;
        }
        self.checks.push(CheckResult {
            name,
            passed,
            message,
        });
    }

    fn summary(&self) -> String {
        let passed_count = self.checks.iter().filter(|c| c.passed).count();
        let total_count = self.checks.len();
        format!(
            "{}: {}/{} checks passed",
            self.category, passed_count, total_count
        )
    }

    fn details(&self) -> Vec<String> {
        self.checks
            .iter()
            .filter(|c| !c.passed)
            .map(|c| format!("  ✗ {}: {}", c.name, c.message))
            .collect()
    }
}

/// Load all documentation files from docs directory
fn load_documentation_files(docs_dir: &Path) -> Vec<DocFile> {
    let mut files = Vec::new();

    fn walk_dir(dir: &Path, files: &mut Vec<DocFile>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().map_or(false, |ext| ext == "md") {
                    if let Some(doc) = parse_doc_file(&path) {
                        files.push(doc);
                    }
                } else if path.is_dir() && !path.file_name().map_or(false, |n| n == "wip" || n == "src") {
                    walk_dir(&path, files);
                }
            }
        }
    }

    walk_dir(docs_dir, &mut files);
    files.sort_by(|a, b| a.path.cmp(&b.path));
    files
}

/// Parse a single markdown file
fn parse_doc_file(path: &Path) -> Option<DocFile> {
    let content = fs::read_to_string(path).ok()?;
    let category = DocCategory::from_path(path)?;

    // Extract title from first heading
    let title = content
        .lines()
        .find(|l| l.starts_with('#'))
        .map(|l| l.trim_start_matches('#').trim().to_string())
        .unwrap_or_else(|| path.file_name().unwrap().to_string_lossy().to_string());

    // Extract links
    let mut links = Vec::new();
    for line in content.lines() {
        if let Some(start) = line.find("[") {
            if let Some(end) = line.find("]") {
                if let Some(link_start) = line[end..].find("(") {
                    if let Some(link_end) = line[link_start..].find(")") {
                        let link = &line[end + 1 + link_start + 1..end + 1 + link_start + link_end];
                        if !link.is_empty() && !link.starts_with("http") {
                            links.push(link.to_string());
                        }
                    }
                }
            }
        }
    }

    // Count words
    let word_count = content.split_whitespace().count();

    Some(DocFile {
        path: path.to_path_buf(),
        category,
        title,
        links,
        word_count,
    })
}

/// Verify Diataxis structure
fn test_diataxis_structure(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Diataxis Structure".to_string());

    let categories = vec![
        (DocCategory::Tutorial, 4..=5),
        (DocCategory::HowToGuide, 9..=12),
        (DocCategory::Reference, 4..=5),
        (DocCategory::Explanation, 5..=7),
    ];

    for (category, expected_range) in categories {
        let count = files.iter().filter(|f| f.category == category).count();
        let passed = expected_range.contains(&count);
        result.add_check(
            format!("{} Files", category.name()),
            passed,
            format!(
                "Found {} files, expected {}-{}",
                count,
                expected_range.start(),
                expected_range.end()
            ),
        );
    }

    result
}

/// Verify required documentation files exist
fn test_required_files(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Required Files".to_string());

    let required_files = vec![
        ("docs/README.md", DocCategory::Index),
        ("docs/tutorials/getting-started.md", DocCategory::Tutorial),
        ("docs/how-to-guides/installation.md", DocCategory::HowToGuide),
        ("docs/how-to-guides/validate-ontologies-shacl.md", DocCategory::HowToGuide),
        ("docs/how-to-guides/migrate-existing-code.md", DocCategory::HowToGuide),
        ("docs/how-to-guides/DOGFOODING_QUICKSTART.md", DocCategory::HowToGuide),
        ("docs/reference/cli.md", DocCategory::Reference),
        ("docs/explanations/architecture.md", DocCategory::Explanation),
        ("docs/explanations/lifecycle-and-hooks.md", DocCategory::Explanation),
    ];

    for (required_path, expected_category) in required_files {
        let found = files
            .iter()
            .find(|f| f.path.to_string_lossy().ends_with(required_path));

        let exists = found.is_some();
        let correct_category = found.map_or(false, |f| f.category == expected_category);

        result.add_check(
            format!("File: {}", required_path),
            exists,
            if exists {
                if correct_category {
                    "✓ Found in correct category".to_string()
                } else {
                    format!(
                        "Wrong category: expected {}, got {}",
                        expected_category.name(),
                        found.unwrap().category.name()
                    )
                }
            } else {
                "File not found".to_string()
            },
        );
    }

    result
}

/// Verify content quality for each document
fn test_content_quality(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Content Quality".to_string());

    for file in files {
        let characteristics = file.category.expected_characteristics();

        // Word count check
        let word_count_ok = file.word_count >= characteristics.min_word_count
            && file.word_count <= characteristics.max_word_count;

        result.add_check(
            format!("Word count: {}", file.path.file_name().unwrap().to_string_lossy()),
            word_count_ok,
            format!(
                "{} words (expected {}-{})",
                file.word_count, characteristics.min_word_count, characteristics.max_word_count
            ),
        );

        // Check for required headings (sample check)
        let content = fs::read_to_string(&file.path).unwrap_or_default();
        for required_heading in &characteristics.must_contain_headings {
            let has_heading = content
                .lines()
                .any(|l| l.starts_with('#') && l.contains(required_heading));
            result.add_check(
                format!("Has '{}' heading: {}", required_heading, file.title),
                has_heading || characteristics.must_contain_headings.is_empty(),
                if has_heading {
                    "✓ Found".to_string()
                } else {
                    format!("Missing recommended heading: {}", required_heading)
                },
            );
        }
    }

    result
}

/// Verify cross-references and links
fn test_cross_references(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Cross-References".to_string());

    // Build a map of available files
    let available_files: HashSet<String> = files
        .iter()
        .flat_map(|f| {
            let path_str = f.path.to_string_lossy().to_string();
            vec![
                path_str.clone(),
                path_str.replace("docs/", ""),
                path_str
                    .replace("docs/", "")
                    .split('/')
                    .last()
                    .unwrap_or(&"")
                    .to_string(),
            ]
        })
        .collect();

    for file in files {
        for link in &file.links {
            // Skip external links and anchors
            if link.starts_with("http") || link.starts_with("#") {
                continue;
            }

            // Normalize link for comparison
            let normalized_link = link.replace("../", "").replace("./", "");
            let link_exists = available_files.iter().any(|f| {
                f.ends_with(&normalized_link) || f.ends_with(&link.as_str())
            });

            result.add_check(
                format!("Link from {}: {}", file.title, link),
                link_exists,
                if link_exists {
                    "✓ Valid".to_string()
                } else {
                    format!("Link target not found: {}", link)
                },
            );
        }
    }

    result
}

/// Verify Diataxis principles are followed
fn test_diataxis_principles(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Diataxis Principles".to_string());

    let mut categories = HashMap::new();
    for file in files {
        categories
            .entry(file.category)
            .or_insert_with(Vec::new)
            .push(file);
    }

    // Check Tutorial: Learning-oriented
    let tutorials = categories.get(&DocCategory::Tutorial).unwrap_or(&vec![]);
    for tutorial in tutorials {
        let content = fs::read_to_string(&tutorial.path).unwrap_or_default();
        let has_objectives = content.contains("learn") || content.contains("objective")
            || content.contains("goal");
        let has_steps = content.lines().any(|l| l.starts_with("##") || l.starts_with("###"));
        result.add_check(
            format!("Tutorial Learning Path: {}", tutorial.title),
            has_objectives && has_steps,
            "Has clear learning progression".to_string(),
        );
    }

    // Check How-to: Task-oriented
    let howtos = categories.get(&DocCategory::HowToGuide).unwrap_or(&vec![]);
    for howto in howtos {
        let content = fs::read_to_string(&howto.path).unwrap_or_default();
        let task_oriented = content.contains("How to")
            || content.contains("Problem")
            || content.contains("Step");
        result.add_check(
            format!("How-to Task Focus: {}", howto.title),
            task_oriented,
            "Has task-oriented structure".to_string(),
        );
    }

    // Check Reference: Information-oriented
    let refs = categories.get(&DocCategory::Reference).unwrap_or(&vec![]);
    for reference in refs {
        let content = fs::read_to_string(&reference.path).unwrap_or_default();
        let is_comprehensive = reference.word_count > 500 && content.lines().count() > 30;
        result.add_check(
            format!("Reference Completeness: {}", reference.title),
            is_comprehensive,
            "Has comprehensive content".to_string(),
        );
    }

    // Check Explanation: Understanding-oriented
    let explanations = categories.get(&DocCategory::Explanation).unwrap_or(&vec![]);
    for explanation in explanations {
        let content = fs::read_to_string(&explanation.path).unwrap_or_default();
        let conceptual = content.contains("Why") || content.contains("concept")
            || content.contains("understand");
        result.add_check(
            format!("Explanation Conceptual Depth: {}", explanation.title),
            conceptual,
            "Has conceptual focus".to_string(),
        );
    }

    result
}

/// Verify navigation consistency
fn test_navigation_consistency(files: &[DocFile]) -> TestResult {
    let mut result = TestResult::new("Navigation Consistency".to_string());

    // Check that index files reference all major documents
    let index_files: Vec<_> = files
        .iter()
        .filter(|f| f.path.ends_with("README.md"))
        .collect();

    for index in index_files {
        let mut has_tutorials = false;
        let mut has_howtos = false;
        let mut has_references = false;
        let mut has_explanations = false;

        for link in &index.links {
            if link.contains("tutorial") {
                has_tutorials = true;
            }
            if link.contains("how-to") {
                has_howtos = true;
            }
            if link.contains("reference") {
                has_references = true;
            }
            if link.contains("explanation") {
                has_explanations = true;
            }
        }

        result.add_check(
            "Index references tutorials".to_string(),
            has_tutorials,
            "Links to tutorial section".to_string(),
        );
        result.add_check(
            "Index references how-to guides".to_string(),
            has_howtos,
            "Links to how-to section".to_string(),
        );
        result.add_check(
            "Index references reference docs".to_string(),
            has_references,
            "Links to reference section".to_string(),
        );
        result.add_check(
            "Index references explanations".to_string(),
            has_explanations,
            "Links to explanation section".to_string(),
        );
    }

    result
}

#[test]
fn test_documentation_diataxis_compliance() {
    // Setup
    let docs_dir = Path::new("docs");
    assert!(
        docs_dir.exists(),
        "docs directory not found. Please run from repository root."
    );

    let files = load_documentation_files(docs_dir);
    assert!(!files.is_empty(), "No documentation files found");

    // Run all test suites
    let test_suites = vec![
        test_diataxis_structure(&files),
        test_required_files(&files),
        test_content_quality(&files),
        test_cross_references(&files),
        test_diataxis_principles(&files),
        test_navigation_consistency(&files),
    ];

    // Collect results
    let all_passed = test_suites.iter().all(|r| r.passed);
    let total_checks: usize = test_suites.iter().map(|r| r.checks.len()).sum();
    let passed_checks: usize = test_suites.iter().map(|r| r.checks.iter().filter(|c| c.passed).count()).sum();

    // Print summary
    println!("\n╔══════════════════════════════════════════════════════════╗");
    println!("║    Diataxis Documentation Integration Test Suite        ║");
    println!("╚══════════════════════════════════════════════════════════╝\n");

    println!("Documentation Files: {}", files.len());
    for suite in &test_suites {
        println!("  • {}", suite.summary());
    }

    println!("\n Overall Results: {}/{} checks passed", passed_checks, total_checks);
    println!("  Status: {}\n", if all_passed { "✓ PASS" } else { "✗ FAIL" });

    // Print detailed failures
    let failures: Vec<_> = test_suites
        .iter()
        .flat_map(|s| {
            let mut details = s.details();
            if !details.is_empty() {
                details.insert(0, format!("{}:", s.category));
            }
            details
        })
        .collect();

    if !failures.is_empty() {
        println!("Failed Checks:");
        for failure in failures {
            println!("{}", failure);
        }
        println!();
    }

    // Final assertion
    assert!(all_passed, "Documentation Diataxis compliance check failed");
}

/// Unit test: Verify specific files
#[test]
fn test_critical_documentation_files_exist() {
    let critical_files = vec![
        "docs/README.md",
        "docs/tutorials/getting-started.md",
        "docs/how-to-guides/installation.md",
        "docs/how-to-guides/validate-ontologies-shacl.md",
        "docs/how-to-guides/migrate-existing-code.md",
        "docs/reference/cli.md",
        "docs/explanations/architecture.md",
        "docs/explanations/lifecycle-and-hooks.md",
    ];

    for file in critical_files {
        let path = Path::new(file);
        assert!(path.exists(), "Critical file missing: {}", file);
        assert!(
            path.is_file(),
            "Expected file but found directory: {}",
            file
        );
    }
}

/// Unit test: Verify README.md has Diataxis section
#[test]
fn test_root_readme_diataxis_section() {
    let readme = fs::read_to_string("README.md").expect("Could not read README.md");
    assert!(
        readme.contains("Documentation Map (Diataxis"),
        "README.md missing Diataxis documentation section"
    );
    assert!(
        readme.contains("Tutorials"),
        "README.md missing Tutorial reference"
    );
    assert!(
        readme.contains("How-to Guides") || readme.contains("How-to-Guides"),
        "README.md missing How-to Guide reference"
    );
    assert!(
        readme.contains("Reference"),
        "README.md missing Reference section"
    );
    assert!(
        readme.contains("Explanation"),
        "README.md missing Explanation section"
    );
}

/// Unit test: Verify no broken cross-references in docs/README.md
#[test]
fn test_docs_readme_links() {
    let docs_readme = fs::read_to_string("docs/README.md").expect("Could not read docs/README.md");

    // Extract all markdown links
    let mut link_count = 0;
    let mut broken_links = Vec::new();

    for line in docs_readme.lines() {
        if line.contains("[") && line.contains("]") {
            if let Some(start) = line.find("[") {
                if let Some(end) = line.find("]") {
                    if let Some(link_start) = line[end..].find("(") {
                        if let Some(link_end) = line[end + 1 + link_start..].find(")") {
                            link_count += 1;
                            let link = &line
                                [end + 1 + link_start + 1..end + 1 + link_start + link_end];

                            // Skip external links and anchors
                            if !link.starts_with("http") && !link.starts_with("#") {
                                let link_path = link.replace("../", "");
                                if !Path::new(&format!("docs/{}", link_path)).exists() {
                                    broken_links.push(link.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    println!("docs/README.md: Found {} links", link_count);
    if !broken_links.is_empty() {
        println!("Broken links: {:?}", broken_links);
    }

    assert!(
        broken_links.is_empty(),
        "docs/README.md contains broken links: {:?}",
        broken_links
    );
}
