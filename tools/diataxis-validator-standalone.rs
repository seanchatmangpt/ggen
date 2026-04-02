#!/usr/bin/env rust-script
use std::fs;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

/// Diataxis category types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Category {
    Tutorial,
    HowTo,
    Explanation,
    Reference,
    Unknown,
}

impl Category {
    fn from_path(path: &Path) -> Self {
        let path_str = path.to_string_lossy();
        if path_str.contains("/tutorials/") {
            Category::Tutorial
        } else if path_str.contains("/how-to/") {
            Category::HowTo
        } else if path_str.contains("/explanations/") {
            Category::Explanation
        } else if path_str.contains("/reference/") {
            Category::Reference
        } else {
            Category::Unknown
        }
    }

    fn name(&self) -> String {
        match self {
            Category::Tutorial => "Tutorial".to_string(),
            Category::HowTo => "How-To".to_string(),
            Category::Explanation => "Explanation".to_string(),
            Category::Reference => "Reference".to_string(),
            Category::Unknown => "Unknown".to_string(),
        }
    }

    fn min_words(&self) -> usize {
        match self {
            Category::Tutorial => 800,
            Category::HowTo => 600,
            Category::Explanation => 700,
            Category::Reference => 400,
            Category::Unknown => 200,
        }
    }
}

/// Document metadata
#[derive(Debug, Clone)]
struct Document {
    path: PathBuf,
    category: Category,
    title: String,
    word_count: usize,
    section_count: usize,
    has_learning_objectives: bool,
    has_time_estimate: bool,
    has_difficulty: bool,
    has_next_steps: bool,
    references: Vec<String>,
    headers: Vec<String>,
}

/// Validation result
#[derive(Debug)]
struct ValidationResult {
    total_files: usize,
    valid_files: usize,
    errors: Vec<String>,
    warnings: Vec<String>,
    category_counts: HashMap<String, usize>,
}

impl ValidationResult {
    fn new() -> Self {
        ValidationResult {
            total_files: 0,
            valid_files: 0,
            errors: Vec::new(),
            warnings: Vec::new(),
            category_counts: HashMap::new(),
        }
    }

    fn print_report(&self) {
        println!("\n{}", "=".repeat(80));
        println!("  DIATAXIS DOCUMENTATION VALIDATION REPORT");
        println!("{}", "=".repeat(80));

        println!("\n📊 SUMMARY");
        println!("  ├─ Total files found:       {}", self.total_files);
        println!("  ├─ Valid files:            {}", self.valid_files);
        println!("  ├─ Errors:                 {}", self.errors.len());
        println!("  └─ Warnings:               {}", self.warnings.len());

        println!("\n📈 DOCUMENT COUNTS BY CATEGORY");
        let mut categories: Vec<_> = self.category_counts.iter().collect();
        categories.sort_by_key(|a| a.0);
        for (category, count) in categories {
            println!("  ├─ {}:              {}", category, count);
        }

        if !self.errors.is_empty() {
            println!("\n❌ ERRORS ({})", self.errors.len());
            for (i, error) in self.errors.iter().enumerate() {
                println!("  ├─ [{}] {}", i + 1, error);
            }
        }

        if !self.warnings.is_empty() {
            println!("\n⚠️  WARNINGS ({})", self.warnings.len());
            for (i, warning) in self.warnings.iter().enumerate() {
                println!("  ├─ [{}] {}", i + 1, warning);
            }
        }

        let status = if self.errors.is_empty() {
            "✅ VALIDATION PASSED"
        } else {
            "❌ VALIDATION FAILED"
        };

        println!("\n{}", "=".repeat(80));
        println!("  STATUS: {}", status);
        println!("{}", "=".repeat(80));
    }
}

/// Walk directory and collect markdown files
fn collect_markdown_files(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    fn walk(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
        if !dir.is_dir() {
            return Ok(());
        }

        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                walk(&path, files)?;
            } else if path.extension().map_or(false, |ext| ext == "md") {
                files.push(path);
            }
        }
        Ok(())
    }

    walk(dir, &mut files)?;
    files.sort();
    Ok(files)
}

/// Extract word count from content
fn count_words(content: &str) -> usize {
    content
        .split_whitespace()
        .filter(|word| !word.is_empty())
        .count()
}

/// Extract title from markdown (first H1)
fn extract_title(content: &str) -> Option<String> {
    for line in content.lines() {
        if line.starts_with("# ") {
            return Some(line[2..].trim().to_string());
        }
    }
    None
}

/// Count markdown sections
fn count_sections(content: &str) -> usize {
    let mut count = 0;
    for line in content.lines() {
        if line.starts_with("## ") || line.starts_with("### ") {
            count += 1;
        }
    }
    count
}

/// Extract all markdown headers
fn extract_headers(content: &str) -> Vec<String> {
    let mut headers = Vec::new();
    for line in content.lines() {
        if line.starts_with("# ") || line.starts_with("## ") || line.starts_with("### ") {
            let header = line.trim_start_matches('#').trim().to_string();
            headers.push(header);
        }
    }
    headers
}

/// Extract referenced files from markdown links
fn extract_references(content: &str) -> Vec<String> {
    let mut references = Vec::new();
    for line in content.lines() {
        if let Some(start) = line.find("](") {
            if let Some(end) = line[start + 2..].find(')') {
                let reference = line[start + 2..start + 2 + end].to_string();
                if reference.ends_with(".md") {
                    references.push(reference);
                }
            }
        }
    }
    references
}

/// Validate a single document
fn validate_document(path: &Path, content: &str) -> Result<Document, String> {
    let category = Category::from_path(path);

    if category == Category::Unknown {
        return Err("File not in diataxis structure (tutorials/how-to/explanations/reference)".to_string());
    }

    let title = extract_title(content)
        .ok_or_else(|| "Missing H1 title (# Title)".to_string())?;

    let word_count = count_words(content);
    let section_count = count_sections(content);
    let headers = extract_headers(content);
    let references = extract_references(content);

    let has_learning_objectives =
        content.contains("Learning Objectives") ||
        content.contains("Objective") ||
        headers.iter().any(|h| h.to_lowercase().contains("objective"));

    let has_time_estimate =
        content.contains("Estimated Time") ||
        content.contains("Time:") ||
        headers.iter().any(|h| h.to_lowercase().contains("time"));

    let has_difficulty =
        content.contains("Difficulty") ||
        content.contains("difficulty:") ||
        headers.iter().any(|h| h.to_lowercase().contains("difficulty"));

    let has_next_steps =
        content.contains("Next Steps") ||
        content.contains("Next steps") ||
        headers.iter().any(|h| h.to_lowercase().contains("next step"));

    Ok(Document {
        path: path.to_path_buf(),
        category,
        title,
        word_count,
        section_count,
        has_learning_objectives,
        has_time_estimate,
        has_difficulty,
        has_next_steps,
        references,
        headers,
    })
}

/// Validate all documents
fn validate_all(docs_dir: &Path) -> Result<ValidationResult, Box<dyn std::error::Error>> {
    let mut result = ValidationResult::new();
    let mut documents = Vec::new();

    // Collect all markdown files
    let files = collect_markdown_files(docs_dir)?;

    for path in files {
        result.total_files += 1;

        match fs::read_to_string(&path) {
            Ok(content) => {
                match validate_document(&path, &content) {
                    Ok(doc) => {
                        let category_name = doc.category.name();

                        // Check word count
                        if doc.word_count < doc.category.min_words() {
                            result.warnings.push(format!(
                                "{}: Only {} words (minimum {} for {})",
                                path.display(),
                                doc.word_count,
                                doc.category.min_words(),
                                category_name
                            ));
                        }

                        // Check required metadata for tutorials
                        if doc.category == Category::Tutorial {
                            if !doc.has_learning_objectives {
                                result.warnings.push(format!(
                                    "{}: Tutorial missing 'Learning Objectives' section",
                                    path.display()
                                ));
                            }
                            if !doc.has_time_estimate {
                                result.warnings.push(format!(
                                    "{}: Tutorial missing 'Estimated Time'",
                                    path.display()
                                ));
                            }
                            if !doc.has_difficulty {
                                result.warnings.push(format!(
                                    "{}: Tutorial missing 'Difficulty' level",
                                    path.display()
                                ));
                            }
                        }

                        // Check required metadata for how-to
                        if doc.category == Category::HowTo {
                            if !doc.has_time_estimate {
                                result.warnings.push(format!(
                                    "{}: How-To missing 'Estimated Time'",
                                    path.display()
                                ));
                            }
                            if !doc.has_difficulty {
                                result.warnings.push(format!(
                                    "{}: How-To missing 'Difficulty' level",
                                    path.display()
                                ));
                            }
                        }

                        // Check structure
                        if doc.section_count < 2 {
                            result.warnings.push(format!(
                                "{}: Only {} sections (expected at least 3)",
                                path.display(),
                                doc.section_count
                            ));
                        }

                        // Count by category
                        *result
                            .category_counts
                            .entry(category_name)
                            .or_insert(0) += 1;

                        result.valid_files += 1;
                        documents.push(doc);
                    }
                    Err(e) => {
                        result.errors.push(format!("{}: {}", path.display(), e));
                    }
                }
            }
            Err(e) => {
                result.errors.push(format!("{}: Cannot read file: {}", path.display(), e));
            }
        }
    }

    // Validate cross-references
    validate_cross_references(&documents, &mut result);

    // Validate index
    validate_index_file(&documents, &mut result)?;

    Ok(result)
}

/// Validate cross-references between documents
fn validate_cross_references(documents: &[Document], result: &mut ValidationResult) {
    let doc_map: HashMap<String, bool> = documents
        .iter()
        .map(|doc| {
            let filename = doc
                .path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            (filename, true)
        })
        .collect();

    for doc in documents {
        for reference in &doc.references {
            let filename = reference.split('/').last().unwrap_or(reference);

            // Skip external URLs
            if reference.contains("http") {
                continue;
            }

            if !doc_map.contains_key(filename) {
                result.errors.push(format!(
                    "{}: Broken reference to '{}'",
                    doc.path.display(),
                    reference
                ));
            }
        }
    }
}

/// Validate the diataxis-index.md file
fn validate_index_file(documents: &[Document], result: &mut ValidationResult) -> Result<(), Box<dyn std::error::Error>> {
    let index_path = PathBuf::from("docs/diataxis-index.md");

    if !index_path.exists() {
        result.errors.push("Missing docs/diataxis-index.md".to_string());
        return Ok(());
    }

    let index_content = fs::read_to_string(&index_path)?;

    // Expected counts
    let tutorials = documents.iter().filter(|d| d.category == Category::Tutorial).count();
    let howtos = documents.iter().filter(|d| d.category == Category::HowTo).count();
    let explanations = documents.iter().filter(|d| d.category == Category::Explanation).count();
    let references = documents.iter().filter(|d| d.category == Category::Reference).count();

    // Check totals
    let expected_tutorials = 5;
    let expected_howtos = 5;
    let expected_explanations = 5;
    let expected_references = 7;

    if tutorials != expected_tutorials {
        result.errors.push(format!(
            "Tutorial count mismatch: expected {}, found {}",
            expected_tutorials, tutorials
        ));
    }

    if howtos != expected_howtos {
        result.errors.push(format!(
            "How-To count mismatch: expected {}, found {}",
            expected_howtos, howtos
        ));
    }

    if explanations != expected_explanations {
        result.errors.push(format!(
            "Explanation count mismatch: expected {}, found {}",
            expected_explanations, explanations
        ));
    }

    if references != expected_references {
        result.errors.push(format!(
            "Reference count mismatch: expected {}, found {}",
            expected_references, references
        ));
    }

    // Check that all documents are referenced in index
    for doc in documents {
        if let Some(filename) = doc.path.file_name() {
            let filename_str = filename.to_string_lossy();
            if !index_content.contains(&filename_str.to_string()) {
                result.warnings.push(format!(
                    "Document {} not referenced in diataxis-index.md",
                    filename_str
                ));
            }
        }
    }

    Ok(())
}

/// Main entry point
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let docs_dir = PathBuf::from("docs/diataxis");

    if !docs_dir.exists() {
        eprintln!("❌ Error: Directory not found: {}", docs_dir.display());
        eprintln!("\nUsage: Run from ggen root directory");
        std::process::exit(1);
    }

    let result = validate_all(&docs_dir)?;
    result.print_report();

    if !result.errors.is_empty() {
        std::process::exit(1);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_detection() {
        assert_eq!(
            Category::from_path(Path::new("docs/diataxis/tutorials/test.md")),
            Category::Tutorial
        );
        assert_eq!(
            Category::from_path(Path::new("docs/diataxis/how-to/test.md")),
            Category::HowTo
        );
        assert_eq!(
            Category::from_path(Path::new("docs/diataxis/explanations/test.md")),
            Category::Explanation
        );
        assert_eq!(
            Category::from_path(Path::new("docs/diataxis/reference/test.md")),
            Category::Reference
        );
    }

    #[test]
    fn test_min_words() {
        assert_eq!(Category::Tutorial.min_words(), 800);
        assert_eq!(Category::HowTo.min_words(), 600);
        assert_eq!(Category::Explanation.min_words(), 700);
        assert_eq!(Category::Reference.min_words(), 400);
    }

    #[test]
    fn test_word_count() {
        assert_eq!(count_words("hello world"), 2);
        assert_eq!(count_words("one  two   three"), 3);
        assert_eq!(count_words(""), 0);
    }

    #[test]
    fn test_extract_title() {
        let content = "# My Title\n\nSome content";
        assert_eq!(extract_title(content), Some("My Title".to_string()));

        let no_title = "Some content\n\nMore content";
        assert_eq!(extract_title(no_title), None);
    }

    #[test]
    fn test_extract_references() {
        let content = "Check [this](file.md) and [that](other.md) but not http://example.com";
        let refs = extract_references(content);
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().any(|r| r.ends_with("file.md")));
        assert!(refs.iter().any(|r| r.ends_with("other.md")));
    }
}
