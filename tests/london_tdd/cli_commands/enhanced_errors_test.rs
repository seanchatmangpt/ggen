#![cfg(feature = "london_tdd")]
//! London TDD tests for enhanced error messages
//!
//! README.md §User-Friendly Features - Enhanced Error Messages
//!
//! Tests verify:
//! - Clear error context
//! - "Did you mean?" suggestions
//! - Step-by-step fix instructions
//! - Platform-specific guidance

#[test]
fn test_template_not_found_suggests_alternatives() {
    let start = std::time::Instant::now();

    // Arrange: User types "rust-servce" instead of "rust-service"
    let available_templates = vec!["rust-service", "rust-server", "rust-svc", "python-service"];

    // Act
    let error = generate_template_not_found_error("rust-servce", &available_templates);

    // Assert: Contains suggestions
    assert!(error.message.contains("Template 'rust-servce' not found"));
    assert!(error.suggestions.contains(&"rust-service".to_string()));
    assert!(error.suggestions.contains(&"rust-server".to_string()));
    assert!(!error.fix_steps.is_empty());
    assert!(error.fix_steps[0].contains("ggen list"));

    // Performance
    assert!(start.elapsed().as_millis() < 100);
}

#[test]
fn test_error_includes_documentation_links() {
    // Arrange
    let error = generate_marketplace_error("Package not found", "io.ggen.missing");

    // Assert
    assert!(error.documentation_link.is_some());
    assert!(error
        .documentation_link
        .unwrap()
        .contains("https://seanchatmangpt.github.io/ggen"));
}

#[test]
fn test_error_provides_platform_specific_fixes() {
    // Arrange: Missing Docker on different platforms
    let macos_error = generate_dependency_error("docker", Platform::MacOS);
    let linux_error = generate_dependency_error("docker", Platform::Linux);

    // Assert: Platform-specific instructions
    assert!(macos_error
        .fix_steps
        .iter()
        .any(|s| s.contains("Docker Desktop for Mac")));
    assert!(linux_error
        .fix_steps
        .iter()
        .any(|s| s.contains("Docker Engine")));
}

#[test]
fn test_error_calculates_edit_distance_for_suggestions() {
    // Arrange
    let available = vec!["rust-web-service", "python-api", "typescript-cli"];

    // Act
    let suggestions = find_similar_templates("rust-web-servce", &available); // typo: servce

    // Assert: Most similar suggestions first
    assert!(!suggestions.is_empty());
    assert_eq!(suggestions[0], "rust-web-service");
}

#[test]
fn test_error_limits_suggestions_to_top_3() {
    // Arrange
    let available = vec!["opt1", "opt2", "opt3", "opt4", "opt5"];

    // Act
    let error = generate_template_not_found_error("test", &available);

    // Assert: Max 3 suggestions
    assert!(error.suggestions.len() <= 3);
}

// Helper types and functions

#[derive(Debug)]
struct EnhancedError {
    message: String,
    #[allow(dead_code)]
    context: String,
    suggestions: Vec<String>,
    fix_steps: Vec<String>,
    documentation_link: Option<String>,
}

#[derive(Debug, Clone, Copy)]
enum Platform {
    MacOS,
    Linux,
    #[allow(dead_code)]
    Windows,
}

fn generate_template_not_found_error(template: &str, available: &[&str]) -> EnhancedError {
    let suggestions = find_similar_templates(template, available);

    EnhancedError {
        message: format!("Template '{}' not found", template),
        context: "The specified template does not exist in the registry".to_string(),
        suggestions: suggestions.into_iter().take(3).collect(),
        fix_steps: vec![
            "Run 'ggen list' to see available templates".to_string(),
            format!("Use 'ggen search {}' to find templates", template),
            "Install packages with 'ggen add <package>'".to_string(),
        ],
        documentation_link: Some("https://seanchatmangpt.github.io/ggen/templates".to_string()),
    }
}

fn generate_marketplace_error(message: &str, _package_id: &str) -> EnhancedError {
    EnhancedError {
        message: message.to_string(),
        context: "Marketplace operation failed".to_string(),
        suggestions: vec![],
        fix_steps: vec!["Check your internet connection".to_string()],
        documentation_link: Some("https://seanchatmangpt.github.io/ggen/marketplace".to_string()),
    }
}

fn generate_dependency_error(tool: &str, platform: Platform) -> EnhancedError {
    let fix_steps = match (tool, platform) {
        ("docker", Platform::MacOS) => vec![
            "Install Docker Desktop for Mac: https://docs.docker.com/desktop/install/mac-install/"
                .to_string(),
        ],
        ("docker", Platform::Linux) => {
            vec!["Install Docker Engine: https://docs.docker.com/engine/install/".to_string()]
        }
        _ => vec![format!("Install {}", tool)],
    };

    EnhancedError {
        message: format!("{} not found", tool),
        context: format!("{} is required for this operation", tool),
        suggestions: vec![],
        fix_steps,
        documentation_link: Some("https://seanchatmangpt.github.io/ggen/installation".to_string()),
    }
}

fn find_similar_templates(query: &str, available: &[&str]) -> Vec<String> {
    let mut similarities: Vec<(String, usize)> = available
        .iter()
        .map(|&template| {
            let distance = levenshtein_distance(query, template);
            (template.to_string(), distance)
        })
        .filter(|(_, dist)| *dist <= 3) // Max edit distance of 3
        .collect();

    similarities.sort_by_key(|(_, dist)| *dist);
    similarities.into_iter().map(|(name, _)| name).collect()
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    #[allow(clippy::needless_range_loop)]
    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = std::cmp::min(
                std::cmp::min(matrix[i][j + 1] + 1, matrix[i + 1][j] + 1),
                matrix[i][j] + cost,
            );
        }
    }

    matrix[len1][len2]
}
