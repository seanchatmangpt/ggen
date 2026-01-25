//! Example: Secure Path Validation for Template and RDF Loading
//!
//! This example demonstrates how to use PathValidator to prevent
//! path traversal, symlink attacks, and other security vulnerabilities.

use ggen_utils::error::Result;
use ggen_utils::path_validator::{PathValidator, SafePath};
use std::path::Path;

fn main() -> Result<()> {
    println!("Path Validation Security Examples\n");

    // Example 1: Template Loading with Extension Validation
    template_loading_example()?;

    // Example 2: RDF Ontology Loading
    rdf_loading_example()?;

    // Example 3: Output File Validation
    output_file_example()?;

    // Example 4: Batch Validation
    batch_validation_example()?;

    // Example 5: Attack Prevention Demonstrations
    attack_prevention_examples()?;

    Ok(())
}

/// Example 1: Secure Template Loading
fn template_loading_example() -> Result<()> {
    println!("=== Example 1: Template Loading ===\n");

    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"])
        .with_max_depth(5);

    // Valid template paths
    let valid_paths = vec![
        "templates/example.tera",
        "templates/rust/struct.tmpl",
        "src/templates/component.tera",
    ];

    for path in valid_paths {
        match validator.validate(path) {
            Ok(safe_path) => {
                println!("✓ Valid template: {}", safe_path);
                println!("  Absolute path: {}", safe_path.absolute().display());
                println!("  Extension: {:?}", safe_path.extension());
            }
            Err(e) => println!("✗ Invalid: {}", e),
        }
    }

    println!();
    Ok(())
}

/// Example 2: RDF Ontology Loading
fn rdf_loading_example() -> Result<()> {
    println!("=== Example 2: RDF Ontology Loading ===\n");

    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
        .with_max_depth(10);

    let ontology_paths = vec![
        "schema/domain.ttl",
        "ontologies/core.rdf",
        "data/entities.n3",
    ];

    for path in ontology_paths {
        match validator.validate(path) {
            Ok(safe_path) => {
                println!("✓ Valid ontology: {}", safe_path);
                // Safe to load RDF
                // let content = std::fs::read_to_string(safe_path.as_path())?;
            }
            Err(e) => println!("✗ Invalid: {}", e),
        }
    }

    println!();
    Ok(())
}

/// Example 3: Output File Validation
fn output_file_example() -> Result<()> {
    println!("=== Example 3: Output File Validation ===\n");

    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
        .with_max_depth(10);

    let output_paths = vec![
        "src/generated/models.rs",
        "dist/components/Button.ts",
        "build/api/handlers.py",
    ];

    for path in output_paths {
        match validator.validate(path) {
            Ok(safe_path) => {
                println!("✓ Valid output path: {}", safe_path);
                // Safe to write generated code
                // std::fs::write(safe_path.as_path(), generated_code)?;
            }
            Err(e) => println!("✗ Invalid: {}", e),
        }
    }

    println!();
    Ok(())
}

/// Example 4: Batch Validation
fn batch_validation_example() -> Result<()> {
    println!("=== Example 4: Batch Validation ===\n");

    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera"])
        .with_max_depth(5);

    let template_paths = vec![
        "templates/struct.tera",
        "templates/enum.tera",
        "templates/trait.tera",
    ];

    match validator.validate_batch(&template_paths) {
        Ok(safe_paths) => {
            println!(
                "✓ All {} templates validated successfully:",
                safe_paths.len()
            );
            for (i, safe_path) in safe_paths.iter().enumerate() {
                println!("  {}. {}", i + 1, safe_path);
            }
        }
        Err(e) => println!("✗ Batch validation failed: {}", e),
    }

    println!();
    Ok(())
}

/// Example 5: Attack Prevention Demonstrations
fn attack_prevention_examples() -> Result<()> {
    println!("=== Example 5: Attack Prevention ===\n");

    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace);

    // Attack 1: Path Traversal
    println!("Attack 1: Path Traversal");
    let traversal_attacks = vec![
        "../../../etc/passwd",
        "../../secrets/api_keys.txt",
        "subdir/../../outside/file.txt",
    ];

    for attack in traversal_attacks {
        match validator.validate(attack) {
            Ok(_) => println!("  ✗ FAILED TO BLOCK: {}", attack),
            Err(e) => println!("  ✓ Blocked: {} - {}", attack, e),
        }
    }

    println!();

    // Attack 2: Null Byte Injection
    println!("Attack 2: Null Byte Injection");
    let null_byte_attacks = vec!["safe.txt\0../../etc/passwd", "file\0.evil"];

    for attack in null_byte_attacks {
        match validator.validate(attack) {
            Ok(_) => println!("  ✗ FAILED TO BLOCK: {}", attack),
            Err(e) => println!("  ✓ Blocked: null byte injection - {}", e),
        }
    }

    println!();

    // Attack 3: Absolute Path Escape
    println!("Attack 3: Absolute Path Escape");
    let absolute_attacks = vec!["/etc/passwd", "/var/log/secrets.txt"];

    for attack in absolute_attacks {
        match validator.validate(attack) {
            Ok(_) => println!("  ✗ FAILED TO BLOCK: {}", attack),
            Err(e) => println!("  ✓ Blocked: {} - {}", attack, e),
        }
    }

    println!();

    // Attack 4: Extension Bypass
    println!("Attack 4: Extension Bypass");
    let extension_validator =
        PathValidator::new(workspace).with_allowed_extensions(vec!["tera", "tmpl"]);

    let extension_attacks = vec!["malware.exe", "script.sh", "backdoor.so"];

    for attack in extension_attacks {
        match extension_validator.validate(attack) {
            Ok(_) => println!("  ✗ FAILED TO BLOCK: {}", attack),
            Err(e) => println!("  ✓ Blocked: {} - {}", attack, e),
        }
    }

    println!();

    // Attack 5: Depth Limit Bypass
    println!("Attack 5: Depth Limit Bypass");
    let depth_validator = PathValidator::new(workspace).with_max_depth(5);

    let depth_attack = "a/b/c/d/e/f/g/h/i/j/file.txt"; // Depth > 5

    match depth_validator.validate(depth_attack) {
        Ok(_) => println!("  ✗ FAILED TO BLOCK: deep path"),
        Err(e) => println!("  ✓ Blocked: depth limit exceeded - {}", e),
    }

    println!();
    Ok(())
}

/// Real-world usage: Template engine integration
pub struct TemplateEngine {
    validator: PathValidator,
    template_dir: std::path::PathBuf,
}

impl TemplateEngine {
    pub fn new(workspace: &Path) -> Self {
        let validator = PathValidator::new(workspace)
            .with_allowed_extensions(vec!["tera", "tmpl"])
            .with_max_depth(5);

        Self {
            validator,
            template_dir: workspace.join("templates"),
        }
    }

    pub fn load_template(&self, template_name: &str) -> Result<String> {
        // Validate path first
        let safe_path = self.validator.validate(template_name)?;

        // Safe to load - path has been validated
        let content = std::fs::read_to_string(safe_path.as_path())?;

        Ok(content)
    }

    pub fn render(&self, template_name: &str, context: &str) -> Result<String> {
        let template_content = self.load_template(template_name)?;

        // Render logic here
        Ok(format!(
            "Rendered: {} with context: {}",
            template_content, context
        ))
    }
}

/// Real-world usage: RDF loader
pub struct RdfLoader {
    validator: PathValidator,
}

impl RdfLoader {
    pub fn new(workspace: &Path) -> Self {
        let validator = PathValidator::new(workspace)
            .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
            .with_max_depth(10);

        Self { validator }
    }

    pub fn load_ontology(&self, ontology_path: &str) -> Result<String> {
        // Validate path
        let safe_path = self.validator.validate(ontology_path)?;

        // Safe to load RDF
        let content = std::fs::read_to_string(safe_path.as_path())?;

        Ok(content)
    }

    pub fn load_multiple(&self, paths: &[&str]) -> Result<Vec<String>> {
        // Batch validation
        let safe_paths = self.validator.validate_batch(paths)?;

        // Load all ontologies
        let mut contents = Vec::new();
        for safe_path in safe_paths {
            let content = std::fs::read_to_string(safe_path.as_path())?;
            contents.push(content);
        }

        Ok(contents)
    }
}

/// Real-world usage: Code generator
pub struct CodeGenerator {
    validator: PathValidator,
    output_dir: std::path::PathBuf,
}

impl CodeGenerator {
    pub fn new(workspace: &Path) -> Self {
        let validator = PathValidator::new(workspace)
            .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
            .with_max_depth(10);

        Self {
            validator,
            output_dir: workspace.join("src/generated"),
        }
    }

    pub fn generate_file(&self, relative_path: &str, content: &str) -> Result<()> {
        // Validate output path
        let safe_path = self.validator.validate(relative_path)?;

        // Create parent directories if needed
        if let Some(parent) = safe_path.as_path().parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Safe to write - path has been validated
        std::fs::write(safe_path.as_path(), content)?;

        Ok(())
    }

    pub fn generate_multiple(&self, files: Vec<(&str, &str)>) -> Result<()> {
        for (path, content) in files {
            self.generate_file(path, content)?;
        }
        Ok(())
    }
}
