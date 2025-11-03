//! Domain logic for RDF-based CLI generation
//!
//! This module provides business logic for generating CLI projects from RDF/TTL files
//! using the ggen-ai RDF generator.

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

/// Options for RDF-based CLI generation
#[derive(Debug, Clone)]
pub struct GenerateFromRdfOptions {
    /// Path to the TTL file containing CLI definition
    pub ttl_file: PathBuf,
    /// Output directory for generated project
    pub output_dir: PathBuf,
    /// Template directory containing rendering templates
    pub template_dir: PathBuf,
}

impl GenerateFromRdfOptions {
    pub fn new(ttl_file: PathBuf, output_dir: PathBuf, template_dir: PathBuf) -> Self {
        Self {
            ttl_file,
            output_dir,
            template_dir,
        }
    }
}

/// Result of RDF-based generation
#[derive(Debug, Clone)]
pub struct GenerateFromRdfResult {
    /// Output directory where project was generated
    pub output_dir: PathBuf,
    /// Number of files generated
    pub files_generated: usize,
    /// Project name extracted from RDF
    pub project_name: String,
}

/// Generate CLI project from RDF/TTL file
///
/// This function orchestrates the following steps:
/// 1. Parse the RDF/TTL file to extract CLI structure
/// 2. Execute SPARQL queries to extract project metadata, nouns, verbs, arguments
/// 3. Validate the extracted data
/// 4. Render templates to generate project files
/// 5. Run post-generation hooks (format, check)
pub fn generate_cli_from_rdf(options: &GenerateFromRdfOptions) -> Result<GenerateFromRdfResult> {
    // Validate input file exists
    if !options.ttl_file.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "TTL file not found: {}",
            options.ttl_file.display()
        )));
    }

    // Validate template directory exists
    if !options.template_dir.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template directory not found: {}",
            options.template_dir.display()
        )));
    }

    // Create output directory if needed
    std::fs::create_dir_all(&options.output_dir).map_err(|e| {
        ggen_utils::error::Error::new(&format!(
            "Failed to create output directory: {}",
            e
        ))
    })?;

    // Use ggen-ai RDF generator
    let generator = ggen_ai::rdf::CliGenerator::new(options.template_dir.clone());

    generator.generate_from_ttl(&options.ttl_file, &options.output_dir).map_err(|e| {
        ggen_utils::error::Error::new(&format!(
            "RDF generation failed: {}",
            e
        ))
    })?;

    // Count generated files (approximate)
    let files_generated = count_files_in_directory(&options.output_dir)?;

    // Extract project name from output directory
    let project_name = options
        .output_dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("generated-project")
        .to_string();

    Ok(GenerateFromRdfResult {
        output_dir: options.output_dir.clone(),
        files_generated,
        project_name,
    })
}

/// Count files recursively in directory
fn count_files_in_directory(dir: &Path) -> Result<usize> {
    let mut count = 0;

    if !dir.exists() {
        return Ok(0);
    }

    for entry in std::fs::read_dir(dir).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read directory: {}", e))
    })? {
        let entry = entry.map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read entry: {}", e))
        })?;

        let path = entry.path();
        if path.is_file() {
            count += 1;
        } else if path.is_dir() {
            count += count_files_in_directory(&path)?;
        }
    }

    Ok(count)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;

    #[test]
    fn test_generate_from_rdf_options_creation() {
        let options = GenerateFromRdfOptions::new(
            PathBuf::from("test.ttl"),
            PathBuf::from("output"),
            PathBuf::from("templates"),
        );

        assert_eq!(options.ttl_file, PathBuf::from("test.ttl"));
        assert_eq!(options.output_dir, PathBuf::from("output"));
        assert_eq!(options.template_dir, PathBuf::from("templates"));
    }

    #[test]
    fn test_count_files_in_directory() {
        let temp_dir = TempDir::new().unwrap();

        // Create test structure
        fs::write(temp_dir.path().join("file1.txt"), "content").unwrap();
        fs::write(temp_dir.path().join("file2.txt"), "content").unwrap();
        fs::create_dir(temp_dir.path().join("subdir")).unwrap();
        fs::write(temp_dir.path().join("subdir/file3.txt"), "content").unwrap();

        let count = count_files_in_directory(temp_dir.path()).unwrap();
        assert_eq!(count, 3);
    }

    #[test]
    fn test_count_files_empty_directory() {
        let temp_dir = TempDir::new().unwrap();
        let count = count_files_in_directory(temp_dir.path()).unwrap();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_count_files_nonexistent_directory() {
        let count = count_files_in_directory(Path::new("/nonexistent/path")).unwrap();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_generate_cli_from_rdf_missing_ttl() {
        let temp_dir = TempDir::new().unwrap();
        let options = GenerateFromRdfOptions::new(
            PathBuf::from("/nonexistent/test.ttl"),
            temp_dir.path().to_path_buf(),
            PathBuf::from("templates"),
        );

        let result = generate_cli_from_rdf(&options);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("TTL file not found"));
    }

    #[test]
    fn test_generate_cli_from_rdf_missing_template_dir() {
        let temp_dir = TempDir::new().unwrap();
        let ttl_file = temp_dir.path().join("test.ttl");
        fs::write(&ttl_file, "@prefix cnv: <http://example.com#> .").unwrap();

        let options = GenerateFromRdfOptions::new(
            ttl_file,
            temp_dir.path().join("output"),
            PathBuf::from("/nonexistent/templates"),
        );

        let result = generate_cli_from_rdf(&options);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Template directory not found"));
    }
}
