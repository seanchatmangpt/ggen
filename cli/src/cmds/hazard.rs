use colored::*;
use std::path::Path;
use walkdir::WalkDir;

pub fn run() -> rgen_utils::error::Result<()> {
    println!("🔍 RGen Hazard Report");
    println!("====================");

    let mut hazards = Vec::new();

    // Check for templates directory
    check_templates_directory(&mut hazards);

    // Check for RDF files
    check_rdf_files(&mut hazards);

    // Check for configuration files
    check_configuration(&mut hazards);

    // Check for potential security issues
    check_security_hazards(&mut hazards);

    // Check for performance issues
    check_performance_hazards(&mut hazards);

    // Display results
    if hazards.is_empty() {
        println!("✅ No hazards detected!");
    } else {
        println!("\n⚠️  Found {} potential hazard(s):", hazards.len());
        for (i, hazard) in hazards.iter().enumerate() {
            println!(
                "\n{}. {} - {}",
                i + 1,
                hazard.severity.colorize(),
                hazard.description
            );
            if let Some(recommendation) = &hazard.recommendation {
                println!("   💡 Recommendation: {}", recommendation);
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
struct Hazard {
    severity: Severity,
    description: String,
    recommendation: Option<String>,
}

#[derive(Debug)]
#[allow(dead_code)]
enum Severity {
    Low,
    Medium,
    High,
    Critical,
}

impl Severity {
    fn colorize(&self) -> colored::ColoredString {
        match self {
            Severity::Low => "LOW".yellow(),
            Severity::Medium => "MEDIUM".yellow(),
            Severity::High => "HIGH".red(),
            Severity::Critical => "CRITICAL".red().bold(),
        }
    }
}

fn check_templates_directory(hazards: &mut Vec<Hazard>) {
    let templates_dirs = ["templates", "examples"];

    for dir in &templates_dirs {
        if !Path::new(dir).exists() {
            hazards.push(Hazard {
                severity: Severity::Low,
                description: format!("Templates directory '{}' not found", dir),
                recommendation: Some(
                    "Create a templates directory to organize your templates".to_string(),
                ),
            });
        } else {
            // Check for .tmpl files
            let mut template_count = 0;
            for entry in WalkDir::new(dir).into_iter().filter_map(|e| e.ok()) {
                if let Some(ext) = entry.path().extension() {
                    if ext == "tmpl" {
                        template_count += 1;
                    }
                }
            }

            if template_count == 0 {
                hazards.push(Hazard {
                    severity: Severity::Medium,
                    description: format!("No .tmpl files found in '{}' directory", dir),
                    recommendation: Some("Add template files with .tmpl extension".to_string()),
                });
            }
        }
    }
}

fn check_rdf_files(hazards: &mut Vec<Hazard>) {
    let rdf_dirs = ["graphs", "rdf", "data"];
    let mut rdf_count = 0;

    for dir in &rdf_dirs {
        if Path::new(dir).exists() {
            for entry in WalkDir::new(dir).into_iter().filter_map(|e| e.ok()) {
                if let Some(ext) = entry.path().extension() {
                    if matches!(ext.to_str(), Some("ttl" | "rdf" | "xml" | "jsonld")) {
                        rdf_count += 1;
                    }
                }
            }
        }
    }

    if rdf_count == 0 {
        hazards.push(Hazard {
            severity: Severity::Medium,
            description: "No RDF files found in project".to_string(),
            recommendation: Some("Add RDF files to enable graph-based code generation".to_string()),
        });
    }
}

fn check_configuration(hazards: &mut Vec<Hazard>) {
    let config_files = ["rgen.toml", "Cargo.toml", "pyproject.toml"];

    for config_file in &config_files {
        if !Path::new(config_file).exists() {
            hazards.push(Hazard {
                severity: Severity::Low,
                description: format!("Configuration file '{}' not found", config_file),
                recommendation: Some(
                    "Consider adding configuration for better project management".to_string(),
                ),
            });
        }
    }
}

fn check_security_hazards(hazards: &mut Vec<Hazard>) {
    // Check for shell hooks in templates
    for entry in WalkDir::new(".").into_iter().filter_map(|e| e.ok()) {
        if let Some(ext) = entry.path().extension() {
            if ext == "tmpl" {
                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    if content.contains("sh_before:") || content.contains("sh_after:") {
                        hazards.push(Hazard {
                            severity: Severity::High,
                            description: format!(
                                "Template '{}' contains shell hooks",
                                entry.path().display()
                            ),
                            recommendation: Some(
                                "Review shell hooks for security implications".to_string(),
                            ),
                        });
                    }
                }
            }
        }
    }
}

fn check_performance_hazards(hazards: &mut Vec<Hazard>) {
    // Check for large RDF files
    for entry in WalkDir::new(".").into_iter().filter_map(|e| e.ok()) {
        if let Some(ext) = entry.path().extension() {
            if matches!(ext.to_str(), Some("ttl" | "rdf" | "xml" | "jsonld")) {
                if let Ok(metadata) = std::fs::metadata(entry.path()) {
                    let size_mb = metadata.len() as f64 / 1_048_576.0;
                    if size_mb > 10.0 {
                        hazards.push(Hazard {
                            severity: Severity::Medium,
                            description: format!(
                                "Large RDF file '{}' ({:.1} MB)",
                                entry.path().display(),
                                size_mb
                            ),
                            recommendation: Some(
                                "Consider splitting large RDF files for better performance"
                                    .to_string(),
                            ),
                        });
                    }
                }
            }
        }
    }
}
