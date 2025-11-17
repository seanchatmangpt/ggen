//! Quality Autopilot for Marketplace Packages
//!
//! Automatically suggests improvements to packages based on failing guards.

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Improvement suggestion for a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementSuggestion {
    pub guard_name: String,
    pub guard_failed: bool,
    pub suggestion: String,
    pub effort_level: String, // "low", "medium", "high"
    pub potential_score_gain: f64,
    pub template_available: bool,
}

/// Improvement plan for a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementPlan {
    pub package_id: String,
    pub current_score: f64,
    pub target_score: f64,
    pub suggestions: Vec<ImprovementSuggestion>,
    pub estimated_effort_hours: f64,
    pub projected_new_score: f64,
}

impl ImprovementPlan {
    pub fn new(package_id: String, current_score: f64) -> Self {
        Self {
            package_id,
            current_score,
            target_score: 95.0, // Production ready
            suggestions: Vec::new(),
            estimated_effort_hours: 0.0,
            projected_new_score: current_score,
        }
    }

    pub fn add_suggestion(&mut self, suggestion: ImprovementSuggestion) {
        self.estimated_effort_hours += match suggestion.effort_level.as_str() {
            "low" => 0.5,
            "medium" => 2.0,
            "high" => 4.0,
            _ => 1.0,
        };
        self.projected_new_score += suggestion.potential_score_gain;
        self.suggestions.push(suggestion);
    }
}

/// Generate improvement plan for a package based on its receipt
pub fn generate_improvement_plan(
    package_id: &str, marketplace_root: &Path,
) -> Result<ImprovementPlan, String> {
    // Get latest receipt first
    let receipt =
        crate::marketplace::ValidationReceipt::latest_for_package(marketplace_root, package_id)
            .map_err(|e| format!("Failed to read receipt: {}", e))?
            .ok_or_else(|| {
                "No receipt found for package. Run marketplace-emit-receipts first.".to_string()
            })?;

    let mut plan = ImprovementPlan::new(package_id.to_string(), receipt.overall_score);
    plan.projected_new_score = receipt.overall_score; // Ensure projected score starts at current

    // Analyze guard results and suggest improvements
    for result in &receipt.guard_results {
        if !result.passed {
            let suggestion = match result.guard_type.as_str() {
                "metadata" => ImprovementSuggestion {
                    guard_name: "Metadata Guard".to_string(),
                    guard_failed: true,
                    suggestion: "Add or complete package.toml with id, version, and description fields"
                        .to_string(),
                    effort_level: "low".to_string(),
                    potential_score_gain: 5.0,
                    template_available: true,
                },
                "license" => ImprovementSuggestion {
                    guard_name: "License Guard".to_string(),
                    guard_failed: true,
                    suggestion: "Add LICENSE file (MIT or Apache 2.0 recommended)".to_string(),
                    effort_level: "low".to_string(),
                    potential_score_gain: 4.0,
                    template_available: true,
                },
                "readme" => ImprovementSuggestion {
                    guard_name: "README Guard".to_string(),
                    guard_failed: true,
                    suggestion: "Expand README.md with usage examples, features, and installation instructions (minimum 500 characters)"
                        .to_string(),
                    effort_level: "medium".to_string(),
                    potential_score_gain: 4.0,
                    template_available: true,
                },
                "tests" => ImprovementSuggestion {
                    guard_name: "Tests Guard".to_string(),
                    guard_failed: true,
                    suggestion: "Create tests/ directory with unit and integration tests".to_string(),
                    effort_level: "high".to_string(),
                    potential_score_gain: 5.0,
                    template_available: true,
                },
                "chicago_compliance" => ImprovementSuggestion {
                    guard_name: "Chicago Compliance Guard".to_string(),
                    guard_failed: true,
                    suggestion: "Remove unwrap(), expect(), and panic!() from production code. Use proper error handling and Result types."
                        .to_string(),
                    effort_level: "high".to_string(),
                    potential_score_gain: 8.0,
                    template_available: false,
                },
                _ => ImprovementSuggestion {
                    guard_name: result.guard_name.clone(),
                    guard_failed: true,
                    suggestion: format!("Address the following issue: {}", result.message),
                    effort_level: "medium".to_string(),
                    potential_score_gain: 3.0,
                    template_available: false,
                },
            };

            plan.add_suggestion(suggestion);
        }
    }

    // Additional suggestions for bonus guards
    if receipt.bonus_passed < receipt.bonus_total {
        let missing_bonus = receipt.bonus_total - receipt.bonus_passed;
        if missing_bonus > 0 {
            plan.add_suggestion(ImprovementSuggestion {
                guard_name: "Documentation".to_string(),
                guard_failed: false,
                suggestion:
                    "Add API documentation and examples to boost score (optional but valuable)"
                        .to_string(),
                effort_level: "medium".to_string(),
                potential_score_gain: 2.5,
                template_available: true,
            });
        }
    }

    Ok(plan)
}

/// Apply improvement template files to a package
pub fn apply_template_improvements(
    package_path: &Path, template_type: &str,
) -> Result<String, String> {
    match template_type {
        "license-mit" => create_license_file(package_path, "MIT"),
        "license-apache" => create_license_file(package_path, "Apache-2.0"),
        "readme" => create_expanded_readme(package_path),
        "tests" => create_test_structure(package_path),
        _ => Err(format!("Unknown template type: {}", template_type)),
    }
}

/// Create LICENSE file
fn create_license_file(package_path: &Path, license_type: &str) -> Result<String, String> {
    let license_content = match license_type {
        "MIT" => {
            r#"MIT License

Copyright (c) 2025 ggen contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"#
        }
        "Apache-2.0" => {
            r#"Apache License
Version 2.0, January 2004

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
"#
        }
        _ => return Err(format!("Unknown license type: {}", license_type)),
    };

    let license_path = package_path.join(format!("LICENSE-{}", license_type));
    std::fs::write(&license_path, license_content)
        .map_err(|e| format!("Failed to create LICENSE file: {}", e))?;

    Ok(format!("Created: {}", license_path.display()))
}

/// Create expanded README template
fn create_expanded_readme(package_path: &Path) -> Result<String, String> {
    let pkg_name = package_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("package");

    let readme_content = format!(
        r#"# {}

Brief description of the package.

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

```bash
ggen market install {}
```

## Usage

```bash
# Basic usage example
example-command --option value
```

## Examples

### Example 1

```bash
example command output
```

### Example 2

```bash
another example
```

## API Reference

### Main Function

```rust
pub fn main_function(param: Type) -> Result<Output>
```

Describe what the function does.

## Contributing

Contributions welcome! Please follow the contribution guidelines.

## License

MIT OR Apache-2.0

## See Also

- Related packages
- Documentation
"#,
        pkg_name, pkg_name
    );

    let readme_path = package_path.join("README.md");
    std::fs::write(&readme_path, readme_content)
        .map_err(|e| format!("Failed to create README: {}", e))?;

    Ok(format!("Created: {}", readme_path.display()))
}

/// Create test directory structure
fn create_test_structure(package_path: &Path) -> Result<String, String> {
    let tests_dir = package_path.join("tests");
    std::fs::create_dir_all(&tests_dir)
        .map_err(|e| format!("Failed to create tests directory: {}", e))?;

    let test_file_content = r#"#[test]
fn test_basic_functionality() {
    // Arrange
    let input = "test";

    // Act
    let result = run_function(input);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_error_handling() {
    // Test error cases
}
"#;

    let test_file = tests_dir.join("basic_tests.rs");
    std::fs::write(&test_file, test_file_content)
        .map_err(|e| format!("Failed to create test file: {}", e))?;

    Ok(format!(
        "Created test structure in: {}",
        tests_dir.display()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_improvement_plan_creation() {
        let plan = ImprovementPlan::new("test-pkg".to_string(), 75.0);
        assert_eq!(plan.current_score, 75.0);
        assert_eq!(plan.target_score, 95.0);
    }

    #[test]
    fn test_suggestion_effort() {
        let mut plan = ImprovementPlan::new("test-pkg".to_string(), 75.0);
        plan.add_suggestion(ImprovementSuggestion {
            guard_name: "Test".to_string(),
            guard_failed: true,
            suggestion: "Test suggestion".to_string(),
            effort_level: "low".to_string(),
            potential_score_gain: 2.5,
            template_available: true,
        });

        assert!(plan.estimated_effort_hours > 0.0);
    }
}
