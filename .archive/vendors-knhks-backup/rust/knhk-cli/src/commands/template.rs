// rust/knhk-cli/src/commands/template.rs
// Template commands - Template management and code generation

use std::fs;
use std::path::{Path, PathBuf};

/// Get the templates directory path
fn get_templates_dir() -> Result<PathBuf, String> {
    // Navigate from current executable to the templates directory
    // This assumes templates are in the project root's templates/ directory
    let current_dir = std::env::current_dir()
        .map_err(|e| format!("Failed to get current directory: {}", e))?;

    // Try multiple possible locations for templates
    let possible_paths = [
        current_dir.join("templates").join("clap-noun-verb-360"),
        current_dir.parent().and_then(|p| Some(p.join("templates").join("clap-noun-verb-360"))).unwrap_or_default(),
        PathBuf::from("/Users/sac/ggen/templates/clap-noun-verb-360"),
    ];

    for path in &possible_paths {
        if path.exists() && path.is_dir() {
            return Ok(path.clone());
        }
    }

    Err("Templates directory not found. Expected at templates/clap-noun-verb-360/".to_string())
}

/// List available templates
pub fn list() -> Result<(), String> {
    let templates_dir = get_templates_dir()?;

    println!("Available templates in {}:", templates_dir.display());

    // Read directory entries
    let entries = fs::read_dir(&templates_dir)
        .map_err(|e| format!("Failed to read templates directory: {}", e))?;

    // Categorize templates
    let mut noun_templates = Vec::new();
    let mut verb_templates = Vec::new();
    let mut test_templates = Vec::new();
    let mut async_templates = Vec::new();
    let mut middleware_templates = Vec::new();
    let mut error_templates = Vec::new();

    for entry in entries {
        let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();

        if name.ends_with(".tmpl") {
            if name.starts_with("noun-") {
                noun_templates.push(name.to_string());
            } else if name.starts_with("verb-") {
                verb_templates.push(name.to_string());
            } else if name.starts_with("test-") {
                test_templates.push(name.to_string());
            } else if name.starts_with("async-pattern-") {
                async_templates.push(name.to_string());
            } else if name.starts_with("middleware-pattern-") {
                middleware_templates.push(name.to_string());
            } else if name.starts_with("error-") {
                error_templates.push(name.to_string());
            }
        }
    }

    // Sort templates
    noun_templates.sort();
    verb_templates.sort();
    test_templates.sort();
    async_templates.sort();
    middleware_templates.sort();
    error_templates.sort();

    // Display categorized templates
    if !noun_templates.is_empty() {
        println!("\nNoun Templates ({}):", noun_templates.len());
        for template in &noun_templates {
            println!("  - {}", template);
        }
    }

    if !verb_templates.is_empty() {
        println!("\nVerb Templates ({}):", verb_templates.len());
        for template in &verb_templates {
            println!("  - {}", template);
        }
    }

    if !test_templates.is_empty() {
        println!("\nTest Templates ({}):", test_templates.len());
        for template in &test_templates {
            println!("  - {}", template);
        }
    }

    if !async_templates.is_empty() {
        println!("\nAsync Pattern Templates ({}):", async_templates.len());
        for template in &async_templates {
            println!("  - {}", template);
        }
    }

    if !middleware_templates.is_empty() {
        println!("\nMiddleware Pattern Templates ({}):", middleware_templates.len());
        for template in &middleware_templates {
            println!("  - {}", template);
        }
    }

    if !error_templates.is_empty() {
        println!("\nError Templates ({}):", error_templates.len());
        for template in &error_templates {
            println!("  - {}", template);
        }
    }

    let total = noun_templates.len() + verb_templates.len() + test_templates.len()
        + async_templates.len() + middleware_templates.len() + error_templates.len();
    println!("\nTotal templates: {}", total);

    Ok(())
}

/// Create a new template
pub fn new(template_name: String, template_type: String) -> Result<(), String> {
    let templates_dir = get_templates_dir()?;

    // Validate template type
    let valid_types = ["noun", "verb", "test", "async-pattern", "middleware-pattern", "error"];
    if !valid_types.contains(&template_type.as_str()) {
        return Err(format!(
            "Invalid template type '{}'. Must be one of: {:?}",
            template_type, valid_types
        ));
    }

    // Construct file name
    let file_name = format!("{}-{}.tmpl", template_type, template_name);
    let file_path = templates_dir.join(&file_name);

    // Check if template already exists
    if file_path.exists() {
        return Err(format!("Template '{}' already exists", file_name));
    }

    // Create basic template content based on type
    let content = match template_type.as_str() {
        "noun" => format!(
            "// {}\n// Noun command template\n\nuse crate::Result;\n\npub fn {}() -> Result<()> {{\n    todo!(\"Implement {} noun\")\n}}\n",
            file_name, template_name, template_name
        ),
        "verb" => format!(
            "// {}\n// Verb action template\n\nuse crate::Result;\n\npub fn {}() -> Result<()> {{\n    todo!(\"Implement {} verb\")\n}}\n",
            file_name, template_name, template_name
        ),
        "test" => format!(
            "// {}\n// Test template\n\n#[cfg(test)]\nmod tests {{\n    use super::*;\n\n    #[test]\n    fn test_{}() {{\n        // Arrange\n        \n        // Act\n        \n        // Assert\n        todo!(\"Implement test\")\n    }}\n}}\n",
            file_name, template_name
        ),
        "async-pattern" => format!(
            "// {}\n// Async pattern template\n\nuse tokio::{{task, sync::mpsc}};\nuse crate::Result;\n\npub async fn {}() -> Result<()> {{\n    todo!(\"Implement async pattern\")\n}}\n",
            file_name, template_name
        ),
        "middleware-pattern" => format!(
            "// {}\n// Middleware pattern template\n\nuse crate::Result;\n\npub fn {}() -> Result<()> {{\n    todo!(\"Implement middleware pattern\")\n}}\n",
            file_name, template_name
        ),
        "error" => format!(
            "// {}\n// Error type template\n\nuse thiserror::Error;\n\n#[derive(Debug, Error)]\npub enum {}Error {{\n    #[error(\"TODO: Add error variants\")]\n    Placeholder,\n}}\n",
            file_name, template_name
        ),
        _ => unreachable!(),
    };

    // Write template file
    fs::write(&file_path, content)
        .map_err(|e| format!("Failed to write template file: {}", e))?;

    println!("✓ Template created: {}", file_path.display());

    Ok(())
}

/// Show template content
pub fn show(template_name: String) -> Result<(), String> {
    let templates_dir = get_templates_dir()?;

    // Find template file (try with and without .tmpl extension)
    let possible_names = [
        template_name.clone(),
        format!("{}.tmpl", template_name),
    ];

    let mut template_path = None;
    for name in &possible_names {
        let path = templates_dir.join(name);
        if path.exists() {
            template_path = Some(path);
            break;
        }
    }

    let template_path = template_path
        .ok_or_else(|| format!("Template '{}' not found", template_name))?;

    // Read and display template content
    let content = fs::read_to_string(&template_path)
        .map_err(|e| format!("Failed to read template: {}", e))?;

    println!("Template: {}", template_path.display());
    println!("----------------------------------------");
    println!("{}", content);
    println!("----------------------------------------");
    println!("Lines: {}", content.lines().count());

    Ok(())
}

/// Generate code from template
pub fn generate(template_name: String, output_path: String) -> Result<(), String> {
    let templates_dir = get_templates_dir()?;

    // Find template file
    let possible_names = [
        template_name.clone(),
        format!("{}.tmpl", template_name),
    ];

    let mut template_path = None;
    for name in &possible_names {
        let path = templates_dir.join(name);
        if path.exists() {
            template_path = Some(path);
            break;
        }
    }

    let template_path = template_path
        .ok_or_else(|| format!("Template '{}' not found", template_name))?;

    // Read template content
    let content = fs::read_to_string(&template_path)
        .map_err(|e| format!("Failed to read template: {}", e))?;

    // Create output directory if it doesn't exist
    if let Some(parent) = Path::new(&output_path).parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create output directory: {}", e))?;
        }
    }

    // Write generated code
    fs::write(&output_path, &content)
        .map_err(|e| format!("Failed to write output file: {}", e))?;

    println!("✓ Code generated from template '{}' to '{}'", template_name, output_path);
    println!("  Lines: {}", content.lines().count());

    Ok(())
}

/// Lint template for syntax errors
pub fn lint(template_name: String) -> Result<(), String> {
    let templates_dir = get_templates_dir()?;

    // Find template file
    let possible_names = [
        template_name.clone(),
        format!("{}.tmpl", template_name),
    ];

    let mut template_path = None;
    for name in &possible_names {
        let path = templates_dir.join(name);
        if path.exists() {
            template_path = Some(path);
            break;
        }
    }

    let template_path = template_path
        .ok_or_else(|| format!("Template '{}' not found", template_name))?;

    // Read template content
    let content = fs::read_to_string(&template_path)
        .map_err(|e| format!("Failed to read template: {}", e))?;

    // Basic validation checks
    let mut issues = Vec::new();

    // Check for empty file
    if content.trim().is_empty() {
        issues.push("Template is empty".to_string());
    }

    // Check for balanced braces
    let open_braces = content.chars().filter(|&c| c == '{').count();
    let close_braces = content.chars().filter(|&c| c == '}').count();
    if open_braces != close_braces {
        issues.push(format!(
            "Unbalanced braces: {} open, {} close",
            open_braces, close_braces
        ));
    }

    // Check for balanced parentheses
    let open_parens = content.chars().filter(|&c| c == '(').count();
    let close_parens = content.chars().filter(|&c| c == ')').count();
    if open_parens != close_parens {
        issues.push(format!(
            "Unbalanced parentheses: {} open, {} close",
            open_parens, close_parens
        ));
    }

    // Check for balanced brackets
    let open_brackets = content.chars().filter(|&c| c == '[').count();
    let close_brackets = content.chars().filter(|&c| c == ']').count();
    if open_brackets != close_brackets {
        issues.push(format!(
            "Unbalanced brackets: {} open, {} close",
            open_brackets, close_brackets
        ));
    }

    // Check for common Rust syntax issues
    if content.contains("unwrap()") {
        issues.push("Contains unwrap() - consider using Result instead".to_string());
    }

    if content.contains("expect(") {
        issues.push("Contains expect() - consider using Result instead".to_string());
    }

    // Display results
    println!("Linting template: {}", template_path.display());

    if issues.is_empty() {
        println!("✓ No issues found");
        println!("  Lines: {}", content.lines().count());
        println!("  Characters: {}", content.len());
    } else {
        println!("⚠ Issues found:");
        for (i, issue) in issues.iter().enumerate() {
            println!("  {}. {}", i + 1, issue);
        }
        return Err(format!("Template has {} issues", issues.len()));
    }

    Ok(())
}
