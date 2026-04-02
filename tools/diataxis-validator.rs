use std::fs;
use std::path::PathBuf;
use std::collections::HashMap;
use regex::Regex;

#[derive(Debug, Clone)]
struct DiataxisDoc {
    path: PathBuf,
    category: DiataxisCategory,
    title: String,
    has_objectives: bool,
    has_time_estimate: bool,
    has_difficulty: bool,
    has_next_steps: bool,
    cross_references: Vec<String>,
    word_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiataxisCategory {
    Tutorial,
    HowTo,
    Explanation,
    Reference,
}

impl DiataxisCategory {
    fn from_path(path: &PathBuf) -> Option<Self> {
        let path_str = path.to_string_lossy();
        if path_str.contains("/tutorials/") {
            Some(DiataxisCategory::Tutorial)
        } else if path_str.contains("/how-to/") {
            Some(DiataxisCategory::HowTo)
        } else if path_str.contains("/explanations/") {
            Some(DiataxisCategory::Explanation)
        } else if path_str.contains("/reference/") {
            Some(DiataxisCategory::Reference)
        } else {
            None
        }
    }

    fn name(&self) -> &'static str {
        match self {
            DiataxisCategory::Tutorial => "Tutorial",
            DiataxisCategory::HowTo => "How-To",
            DiataxisCategory::Explanation => "Explanation",
            DiataxisCategory::Reference => "Reference",
        }
    }

    fn min_words(&self) -> usize {
        match self {
            DiataxisCategory::Tutorial => 800,
            DiataxisCategory::HowTo => 600,
            DiataxisCategory::Explanation => 700,
            DiataxisCategory::Reference => 400,
        }
    }

    fn required_metadata(&self) -> Vec<&'static str> {
        match self {
            DiataxisCategory::Tutorial => vec!["Learning Objectives", "Estimated Time", "Difficulty"],
            DiataxisCategory::HowTo => vec!["Estimated Time", "Difficulty"],
            DiataxisCategory::Explanation => vec!["none"],
            DiataxisCategory::Reference => vec!["none"],
        }
    }
}

#[derive(Debug)]
struct ValidationReport {
    total_files: usize,
    valid_files: usize,
    errors: Vec<String>,
    warnings: Vec<String>,
    doc_counts: HashMap<String, usize>,
}

impl ValidationReport {
    fn new() -> Self {
        ValidationReport {
            total_files: 0,
            valid_files: 0,
            errors: Vec::new(),
            warnings: Vec::new(),
            doc_counts: HashMap::new(),
        }
    }

    fn print(&self) {
        println!("\n{}", "=".repeat(70));
        println!("DIATAXIS DOCUMENTATION VALIDATION REPORT");
        println!("{}", "=".repeat(70));

        println!("\n📊 SUMMARY:");
        println!("  Total files: {}", self.total_files);
        println!("  Valid files: {}", self.valid_files);
        println!("  Error count: {}", self.errors.len());
        println!("  Warning count: {}", self.warnings.len());

        println!("\n📈 DOCUMENT COUNTS:");
        for (category, count) in &self.doc_counts {
            println!("  {}: {}", category, count);
        }

        if !self.errors.is_empty() {
            println!("\n❌ ERRORS ({}):", self.errors.len());
            for (i, error) in self.errors.iter().enumerate() {
                println!("  {}. {}", i + 1, error);
            }
        }

        if !self.warnings.is_empty() {
            println!("\n⚠️  WARNINGS ({}):", self.warnings.len());
            for (i, warning) in self.warnings.iter().enumerate() {
                println!("  {}. {}", i + 1, warning);
            }
        }

        let status = if self.errors.is_empty() { "✅ PASS" } else { "❌ FAIL" };
        println!("\n{}", "=".repeat(70));
        println!("VALIDATION STATUS: {}", status);
        println!("{}", "=".repeat(70));
    }
}

fn main() {
    let docs_dir = PathBuf::from("docs/diataxis");

    if !docs_dir.exists() {
        eprintln!("❌ Directory not found: {}", docs_dir.display());
        std::process::exit(1);
    }

    let mut report = ValidationReport::new();
    let mut all_docs = Vec::new();

    // Scan all markdown files
    for entry in walkdir::WalkDir::new(&docs_dir)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "md"))
    {
        let path = entry.path().to_path_buf();
        report.total_files += 1;

        match validate_file(&path, &mut report) {
            Ok(doc) => {
                report.valid_files += 1;
                *report
                    .doc_counts
                    .entry(doc.category.name().to_string())
                    .or_insert(0) += 1;
                all_docs.push(doc);
            }
            Err(e) => {
                report.errors.push(format!("{}: {}", path.display(), e));
            }
        }
    }

    // Validate cross-references
    validate_cross_references(&all_docs, &mut report);

    // Validate index completeness
    validate_index(&all_docs, &mut report);

    // Print report
    report.print();

    // Exit with error if validation failed
    if !report.errors.is_empty() {
        std::process::exit(1);
    }
}

fn validate_file(path: &PathBuf, report: &mut ValidationReport) -> Result<DiataxisDoc, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read file: {}", e))?;

    let category = DiataxisCategory::from_path(path)
        .ok_or_else(|| "File not in diataxis category (tutorials/how-to/explanations/reference)".to_string())?;

    // Extract title from first H1
    let title_regex = Regex::new(r"^# (.+)$").unwrap();
    let title = title_regex
        .captures(&content)
        .and_then(|c| c.get(1))
        .map(|m| m.as_str().to_string())
        .ok_or_else(|| "Missing H1 title".to_string())?;

    // Check metadata
    let has_objectives = content.contains("Learning Objectives") || content.contains("Objective");
    let has_time_estimate = content.contains("Estimated Time") || content.contains("Time:");
    let has_difficulty = content.contains("Difficulty") || content.contains("difficulty:");
    let has_next_steps = content.contains("Next Steps") || content.contains("Next steps");

    // Word count
    let word_count = content.split_whitespace().count();
    let min_words = category.min_words();

    if word_count < min_words {
        report.warnings.push(format!(
            "{}: Only {} words (minimum {})",
            path.display(),
            word_count,
            min_words
        ));
    }

    // Check required metadata
    match category {
        DiataxisCategory::Tutorial => {
            if !has_objectives {
                report.warnings.push(format!("{}: Missing Learning Objectives section", path.display()));
            }
            if !has_time_estimate {
                report.warnings.push(format!("{}: Missing Estimated Time", path.display()));
            }
            if !has_difficulty {
                report.warnings.push(format!("{}: Missing Difficulty level", path.display()));
            }
        }
        DiataxisCategory::HowTo => {
            if !has_time_estimate {
                report.warnings.push(format!("{}: Missing Estimated Time", path.display()));
            }
            if !has_difficulty {
                report.warnings.push(format!("{}: Missing Difficulty level", path.display()));
            }
        }
        _ => {}
    }

    // Extract cross-references (markdown links to other docs)
    let link_regex = Regex::new(r"\]\(([^)]+\.md)\)").unwrap();
    let cross_references: Vec<String> = link_regex
        .captures_iter(&content)
        .filter_map(|c| c.get(1).map(|m| m.as_str().to_string()))
        .collect();

    // Validate structure (should have multiple sections)
    let section_count = content.matches("\n## ").count() + content.matches("\n### ").count();
    if section_count < 2 {
        report.warnings.push(format!(
            "{}: Only {} sections (expected at least 3)",
            path.display(),
            section_count
        ));
    }

    Ok(DiataxisDoc {
        path: path.clone(),
        category,
        title,
        has_objectives,
        has_time_estimate,
        has_difficulty,
        has_next_steps,
        cross_references,
        word_count,
    })
}

fn validate_cross_references(docs: &[DiataxisDoc], report: &mut ValidationReport) {
    let doc_map: HashMap<String, &DiataxisDoc> = docs
        .iter()
        .map(|doc| {
            let filename = doc
                .path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            (filename, doc)
        })
        .collect();

    for doc in docs {
        for reference in &doc.cross_references {
            // Extract just the filename
            let filename = reference.split('/').last().unwrap_or(reference);

            if !doc_map.contains_key(filename) && !reference.contains("http") {
                report.errors.push(format!(
                    "{}: Broken reference to {}",
                    doc.path.display(),
                    reference
                ));
            }
        }
    }
}

fn validate_index(docs: &[DiataxisDoc], report: &mut ValidationReport) {
    let diataxis_index = PathBuf::from("docs/diataxis-index.md");

    if !diataxis_index.exists() {
        report.errors.push("Missing docs/diataxis-index.md".to_string());
        return;
    }

    let content = fs::read_to_string(&diataxis_index)
        .expect("Cannot read diataxis-index.md");

    // Expected counts
    let expected_counts = vec![
        ("Tutorial", 5),
        ("How-to", 5),
        ("Explanation", 5),
        ("Reference", 7),
    ];

    for (category_name, expected_count) in expected_counts {
        let actual_count = docs.iter().filter(|d| {
            match d.category {
                DiataxisCategory::Tutorial => category_name == "Tutorial",
                DiataxisCategory::HowTo => category_name == "How-to",
                DiataxisCategory::Explanation => category_name == "Explanation",
                DiataxisCategory::Reference => category_name == "Reference",
            }
        }).count();

        if actual_count != expected_count {
            report.errors.push(format!(
                "Index mismatch: {} documents expected {}, found {}",
                category_name, expected_count, actual_count
            ));
        }

        // Check that index mentions the count
        if !content.contains(&expected_count.to_string()) {
            report.warnings.push(format!(
                "Index may not list correct count for {}",
                category_name
            ));
        }
    }

    // Verify all documents are referenced in index
    for doc in docs {
        let filename = doc
            .path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        if !content.contains(&filename) {
            report.warnings.push(format!(
                "Document {} not referenced in index",
                filename
            ));
        }
    }
}

// For running without external dependencies
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_detection() {
        let tutorial_path = PathBuf::from("docs/diataxis/tutorials/test.md");
        assert_eq!(
            DiataxisCategory::from_path(&tutorial_path),
            Some(DiataxisCategory::Tutorial)
        );

        let howto_path = PathBuf::from("docs/diataxis/how-to/test.md");
        assert_eq!(
            DiataxisCategory::from_path(&howto_path),
            Some(DiataxisCategory::HowTo)
        );
    }

    #[test]
    fn test_min_words() {
        assert_eq!(DiataxisCategory::Tutorial.min_words(), 800);
        assert_eq!(DiataxisCategory::HowTo.min_words(), 600);
        assert_eq!(DiataxisCategory::Explanation.min_words(), 700);
        assert_eq!(DiataxisCategory::Reference.min_words(), 400);
    }
}
