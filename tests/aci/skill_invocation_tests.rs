//! Skill Invocation Tests (User Story 3)
//!
//! Tests verify that ggen constitution is packaged as Claude Code skill
//! with keyword-triggered auto-loading for 80% automatic invocation rate.
//!
//! SUCCESS CRITERIA: 80% auto-invocation rate (SC-005)
//!
//! NOTE: These tests follow Chicago TDD - they test observable file structure
//! and YAML parsing, not runtime invocation (which requires Claude Code context).

use std::fs;
use std::path::Path;

/// Test: Constitution skill file exists in .claude/skills/
///
/// EXPECTED: .claude/skills/ggen-constitution.md exists
/// RATIONALE: Claude Code auto-discovers skills in .claude/skills/ directory
#[test]
fn test_skill_file_exists() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");

    assert!(
        skill_path.exists(),
        "Constitution skill MUST exist at .claude/skills/ggen-constitution.md for auto-discovery"
    );

    // Verify it's a file, not directory
    assert!(
        skill_path.is_file(),
        "ggen-constitution.md MUST be a file, not directory"
    );
}

/// Test: Skill file has valid YAML frontmatter
///
/// EXPECTED: File starts with `---` delimiter and contains WHEN/WHEN_NOT sections
/// RATIONALE: Claude Code requires YAML frontmatter for skill metadata and triggers
#[test]
fn test_yaml_frontmatter_valid() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Check YAML frontmatter delimiters
    assert!(
        content.starts_with("---\n"),
        "Skill file MUST start with YAML frontmatter delimiter '---'"
    );

    let has_closing_delimiter = content[4..].contains("\n---\n");
    assert!(
        has_closing_delimiter,
        "Skill file MUST have closing YAML frontmatter delimiter '---'"
    );

    // Check required WHEN section
    assert!(
        content.contains("WHEN:") || content.contains("when:"),
        "Skill frontmatter MUST contain WHEN: section for trigger keywords"
    );

    // Check required WHEN_NOT section
    assert!(
        content.contains("WHEN_NOT:") || content.contains("when_not:"),
        "Skill frontmatter MUST contain WHEN_NOT: section for exclusion keywords"
    );
}

/// Test: trigger_keywords array has >=10 keywords
///
/// EXPECTED: WHEN section lists 10+ trigger keywords
/// RATIONALE: 80% auto-invocation requires comprehensive keyword coverage
#[test]
fn test_trigger_keywords_count() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Extract YAML frontmatter
    let yaml_end = content[4..]
        .find("\n---\n")
        .expect("Could not find YAML closing delimiter");
    let yaml_content = &content[4..4 + yaml_end];

    // Count trigger keywords (heuristic: look for bullet points or array elements)
    // Expected format:
    //   WHEN:
    //     - cargo make
    //     - unwrap
    //     ...

    let when_section = yaml_content
        .split("WHEN:")
        .nth(1)
        .or_else(|| yaml_content.split("when:").nth(1))
        .expect("WHEN: section not found in frontmatter");

    // Stop at next section (WHEN_NOT or end)
    let when_content = when_section
        .split("WHEN_NOT:")
        .next()
        .and_then(|s| s.split("when_not:").next())
        .unwrap_or(when_section);

    // Count bullet points (lines starting with -)
    let trigger_count = when_content
        .lines()
        .filter(|line| line.trim().starts_with('-'))
        .count();

    assert!(
        trigger_count >= 10,
        "WHEN: section MUST have >=10 trigger keywords for 80% auto-invocation (found: {})",
        trigger_count
    );
}

/// Test: exclusion_keywords array has >=3 keywords
///
/// EXPECTED: WHEN_NOT section lists 3+ exclusion keywords
/// RATIONALE: Prevents loading on non-ggen Rust projects
#[test]
fn test_exclusion_keywords_count() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Extract YAML frontmatter
    let yaml_end = content[4..]
        .find("\n---\n")
        .expect("Could not find YAML closing delimiter");
    let yaml_content = &content[4..4 + yaml_end];

    // Find WHEN_NOT section
    let when_not_section = yaml_content
        .split("WHEN_NOT:")
        .nth(1)
        .or_else(|| yaml_content.split("when_not:").nth(1))
        .expect("WHEN_NOT: section not found in frontmatter");

    // Count bullet points (lines starting with -)
    let exclusion_count = when_not_section
        .lines()
        .filter(|line| line.trim().starts_with('-'))
        .count();

    assert!(
        exclusion_count >= 3,
        "WHEN_NOT: section MUST have >=3 exclusion keywords to prevent false loading (found: {})",
        exclusion_count
    );
}

/// Test: version field matches constitution
///
/// EXPECTED: Skill frontmatter contains version: "1.0.0"
/// RATIONALE: Version tracking ensures skill matches constitution
#[test]
fn test_version_field_matches() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Extract YAML frontmatter
    let yaml_end = content[4..]
        .find("\n---\n")
        .expect("Could not find YAML closing delimiter");
    let yaml_content = &content[4..4 + yaml_end];

    // Check for version field
    assert!(
        yaml_content.contains("version:") || yaml_content.contains("VERSION:"),
        "Skill frontmatter MUST contain version: field for tracking"
    );

    // Verify version is 1.0.0 (current constitution version)
    let has_correct_version = yaml_content.contains("version: \"1.0.0\"")
        || yaml_content.contains("version: '1.0.0'")
        || yaml_content.contains("version: 1.0.0");

    assert!(
        has_correct_version,
        "Skill version MUST match constitution version: 1.0.0"
    );
}

/// Test: Skill content includes constitution principles
///
/// EXPECTED: Content section includes core principles from constitution
/// RATIONALE: Skill should provide same guidance as full constitution
#[test]
fn test_skill_content_includes_principles() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Content should include key principles
    let expected_principles = vec![
        "Chicago TDD",
        "cargo make",
        "Andon Signal",
        "Error Handling",
        "Type-First",
    ];

    let mut missing_principles = Vec::new();

    for principle in expected_principles {
        if !content.contains(principle) {
            missing_principles.push(principle);
        }
    }

    assert!(
        missing_principles.is_empty(),
        "Skill content MUST include core principles: {:?}",
        missing_principles
    );
}

/// Test: Skill file has reasonable size
///
/// EXPECTED: File size between 5KB and 50KB
/// RATIONALE: Too small = missing content, too large = bloated/unusable
#[test]
fn test_skill_file_size_reasonable() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let metadata = fs::metadata(skill_path).expect("Failed to get skill file metadata");

    let file_size = metadata.len();

    assert!(
        file_size >= 5_000,
        "Skill file too small ({} bytes) - likely missing content (min: 5KB)",
        file_size
    );

    assert!(
        file_size <= 50_000,
        "Skill file too large ({} bytes) - should be concise summary (max: 50KB)",
        file_size
    );
}

/// Test: Skill describes WHEN to use it
///
/// EXPECTED: Frontmatter or content explains when skill should be invoked
/// RATIONALE: Clear guidance helps Claude Code decide when to load skill
#[test]
fn test_skill_describes_usage() {
    let skill_path = Path::new(".claude/skills/ggen-constitution.md");
    let content = fs::read_to_string(skill_path).expect("Failed to read skill file");

    // Check for usage description keywords
    let has_usage_description = content.to_lowercase().contains("use this")
        || content.to_lowercase().contains("when to")
        || content.to_lowercase().contains("working on ggen")
        || content.to_lowercase().contains("developing ggen");

    assert!(
        has_usage_description,
        "Skill SHOULD describe when to use it (helps agent decision-making)"
    );
}
