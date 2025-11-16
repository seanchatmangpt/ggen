//! Marketplace Artifact Generation
//!
//! Generates JSON registry and Markdown documentation from validation receipts.
//! Implements the μ_market projection layer: ontology → JSON/Markdown artifacts.

use serde_json::{json, Value};
use std::path::Path;
use chrono::Utc;

/// Generate registry index.json from marketplace receipts and ontology
pub fn generate_registry_index(marketplace_root: &Path) -> Result<Value, String> {
    let receipts_dir = marketplace_root.join("marketplace").join("receipts");
    let mut packages = Vec::new();

    if !receipts_dir.exists() {
        return Err("No receipts directory found. Run marketplace-emit-receipts first.".to_string());
    }

    // Collect all package receipts
    if let Ok(entries) = std::fs::read_dir(&receipts_dir) {
        for entry in entries.flatten() {
            let pkg_dir = entry.path();
            if pkg_dir.is_dir() {
                if let Some(pkg_id) = pkg_dir.file_name().and_then(|n| n.to_str()) {
                    // Get latest receipt for this package
                    if let Ok(Some(receipt)) =
                        crate::marketplace::ValidationReceipt::latest_for_package(marketplace_root, pkg_id)
                    {
                        packages.push(json!({
                            "name": pkg_id,
                            "version": receipt.version,
                            "description": format!("{} package (v{})", pkg_id, receipt.version),
                            "category": categorize_package(pkg_id),
                            "author": "ggen-team",
                            "license": "MIT",
                            "tags": extract_tags(pkg_id),
                            "keywords": extract_keywords(pkg_id),
                            "production_ready": receipt.production_ready,
                            "validation_score": format!("{:.1}", receipt.overall_score),
                            "last_validated": receipt.validated_at,
                            "critical_guards_passed": receipt.critical_passed,
                            "critical_guards_total": receipt.critical_total,
                            "bonus_guards_passed": receipt.bonus_passed,
                            "bonus_guards_total": receipt.bonus_total,
                            "downloads": 0,
                            "stars": 0,
                            "path": format!("marketplace/packages/{}", pkg_id),
                            "download_url": "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip",
                            "checksum": receipt.checksum.unwrap_or_default()
                        }));
                    }
                }
            }
        }
    }

    // Sort packages by name
    packages.sort_by(|a, b| {
        let name_a = a.get("name").and_then(|n| n.as_str()).unwrap_or("");
        let name_b = b.get("name").and_then(|n| n.as_str()).unwrap_or("");
        name_a.cmp(name_b)
    });

    // Build categories
    let mut categories = std::collections::HashMap::new();
    for pkg in &packages {
        if let Some(cat) = pkg.get("category").and_then(|c| c.as_str()) {
            *categories.entry(cat.to_string()).or_insert(0) += 1;
        }
    }

    Ok(json!({
        "version": "1.0.0",
        "registry_url": "https://github.com/seanchatmangpt/ggen",
        "updated_at": Utc::now().to_rfc3339(),
        "package_count": packages.len(),
        "categories": categories,
        "packages": packages
    }))
}

/// Generate PACKAGES.md documentation from receipts
pub fn generate_packages_markdown(marketplace_root: &Path) -> Result<String, String> {
    let receipts_dir = marketplace_root.join("marketplace").join("receipts");
    let mut packages_by_category: std::collections::BTreeMap<String, Vec<(String, f64, bool)>> =
        std::collections::BTreeMap::new();

    if !receipts_dir.exists() {
        return Err("No receipts directory found. Run marketplace-emit-receipts first.".to_string());
    }

    // Collect packages by category
    if let Ok(entries) = std::fs::read_dir(&receipts_dir) {
        for entry in entries.flatten() {
            let pkg_dir = entry.path();
            if pkg_dir.is_dir() {
                if let Some(pkg_id) = pkg_dir.file_name().and_then(|n| n.to_str()) {
                    if let Ok(Some(receipt)) =
                        crate::marketplace::ValidationReceipt::latest_for_package(marketplace_root, pkg_id)
                    {
                        let category = categorize_package(pkg_id).to_string();
                        packages_by_category
                            .entry(category)
                            .or_insert_with(Vec::new)
                            .push((pkg_id.to_string(), receipt.overall_score, receipt.production_ready));
                    }
                }
            }
        }
    }

    let mut markdown = String::new();
    markdown.push_str("# Complete Package Directory\n\n");
    markdown.push_str("Comprehensive documentation of all packages available in the ggen marketplace.\n\n");
    markdown.push_str(&format!(
        "**Last Updated**: {}\n",
        chrono::Local::now().format("%Y-%m-%d")
    ));
    markdown.push_str(&format!(
        "**Total Packages**: {}\n",
        packages_by_category.values().map(|v| v.len()).sum::<usize>()
    ));
    markdown.push_str("**Version**: 1.0.0\n\n");

    markdown.push_str("---\n\n## 📑 Quick Navigation\n\n");

    // Navigation links
    for category in packages_by_category.keys() {
        let safe_category = category.replace(" ", "-").to_lowercase();
        markdown.push_str(&format!("- [{}](#-{})\n", category, safe_category));
    }

    markdown.push_str("\n---\n\n");

    // Package sections by category
    for (category, packages) in packages_by_category {
        let _safe_category = category.replace(" ", "-").to_lowercase();
        markdown.push_str(&format!("## {} {}\n\n", category_emoji(&category), category));

        for (pkg_id, score, prod_ready) in packages {
            let status_badge = if prod_ready {
                "✅ Production Ready"
            } else if score >= 80.0 {
                "⚠️ Beta"
            } else {
                "🔶 Experimental"
            };

            markdown.push_str(&format!("### {}\n", pkg_id));
            markdown.push_str(&format!(
                "**Score**: {:.1}% | **Status**: {}\n\n",
                score, status_badge
            ));
            markdown.push_str(&format!(
                "**Install**:\n```bash\nggen marketplace install \"{}\"\n```\n\n",
                pkg_id
            ));
            markdown.push_str("---\n\n");
        }
    }

    Ok(markdown)
}

/// Write registry index.json to file
pub fn write_registry_index(index_json: &Value, output_path: &Path) -> Result<(), String> {
    let json_str = serde_json::to_string_pretty(index_json)
        .map_err(|e| format!("Failed to serialize registry: {}", e))?;

    std::fs::write(output_path, json_str)
        .map_err(|e| format!("Failed to write registry: {}", e))?;

    Ok(())
}

/// Write packages markdown to file
pub fn write_packages_markdown(markdown: &str, output_path: &Path) -> Result<(), String> {
    std::fs::write(output_path, markdown)
        .map_err(|e| format!("Failed to write PACKAGES.md: {}", e))
}

/// Categorize a package based on its name
fn categorize_package(pkg_id: &str) -> &'static str {
    match pkg_id {
        p if p.contains("academic") => "academic",
        p if p.contains("paper") => "academic",
        p if p.contains("rust") => "rust",
        p if p.contains("microservice") => "architecture",
        p if p.contains("saas") => "enterprise",
        p if p.contains("erp") => "enterprise",
        p if p.contains("crm") => "enterprise",
        p if p.contains("healthcare") | p.contains("medical") | p.contains("ehr") => "healthcare",
        p if p.contains("banking") | p.contains("payment") | p.contains("fintech") => "finance",
        p if p.contains("data") | p.contains("pipeline") => "data",
        p if p.contains("ai") | p.contains("llm") => "ai",
        p if p.contains("template") => "templates",
        _ => "uncategorized",
    }
}

/// Extract tags for a package
fn extract_tags(pkg_id: &str) -> Vec<&'static str> {
    let mut tags = vec![];

    if pkg_id.contains("rust") {
        tags.push("rust");
    }
    if pkg_id.contains("template") {
        tags.push("template");
    }
    if pkg_id.contains("microservice") {
        tags.push("microservices");
    }
    if pkg_id.contains("ai") {
        tags.push("ai");
    }
    if pkg_id.contains("test") {
        tags.push("testing");
    }

    if tags.is_empty() {
        tags.push("package");
    }

    tags
}

/// Extract keywords for a package
fn extract_keywords(pkg_id: &str) -> Vec<&'static str> {
    extract_tags(pkg_id) // Simplified - in production could be more sophisticated
}

/// Get emoji for category
fn category_emoji(category: &str) -> &'static str {
    match category.to_lowercase().as_str() {
        "academic" => "🎓",
        "rust" => "🦀",
        "architecture" => "🏗️",
        "enterprise" => "💼",
        "healthcare" => "🏥",
        "finance" => "💰",
        "data" => "📊",
        "ai" => "🤖",
        "templates" => "📝",
        _ => "📦",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_categorize_package() {
        assert_eq!(categorize_package("rust-project"), "rust");
        assert_eq!(categorize_package("academic-paper"), "academic");
        assert_eq!(categorize_package("microservice-template"), "architecture");
    }

    #[test]
    fn test_extract_tags() {
        let tags = extract_tags("rust-template");
        assert!(tags.contains(&"rust"));
        assert!(tags.contains(&"template"));
    }

    #[test]
    fn test_category_emoji() {
        assert_eq!(category_emoji("academic"), "🎓");
        assert_eq!(category_emoji("finance"), "💰");
    }
}
