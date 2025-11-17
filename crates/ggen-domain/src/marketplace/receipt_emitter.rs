//! Marketplace Validation Receipt Emission
//!
//! Extends the validation system to emit immutable, checksummed ValidationReceipt JSON files
//! for audit trail and downstream artifact generation.

use crate::marketplace::guards::factories::{
    GuardChicagoCompliance, GuardLicense, GuardMetadata, GuardReadme, GuardTests,
};
use crate::marketplace::guards::{Guard, ValidationReceipt};
use chrono::Utc;
use std::path::{Path, PathBuf};

/// Emit validation receipts for all packages in the marketplace
pub fn emit_receipts_for_marketplace(
    marketplace_root: &Path,
) -> Vec<(String, Result<PathBuf, String>)> {
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let mut results = Vec::new();

    if !packages_dir.exists() {
        return results;
    }

    // Iterate all package directories
    if let Ok(entries) = std::fs::read_dir(&packages_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if let Some(pkg_name) = path.file_name().and_then(|n| n.to_str()) {
                    // Try to extract version from package.toml
                    if let Ok(version) = extract_version_from_package(&path) {
                        match emit_receipt_for_package(&path, pkg_name, &version, marketplace_root)
                        {
                            Ok(receipt_path) => {
                                results.push((pkg_name.to_string(), Ok(receipt_path)));
                            }
                            Err(e) => {
                                results.push((pkg_name.to_string(), Err(e)));
                            }
                        }
                    } else {
                        results.push((
                            pkg_name.to_string(),
                            Err("Could not extract version from package".to_string()),
                        ));
                    }
                }
            }
        }
    }

    results
}

/// Extract package version from package.toml
fn extract_version_from_package(package_path: &Path) -> Result<String, String> {
    let package_toml = package_path.join("package.toml");
    if !package_toml.exists() {
        return Err("No package.toml found".to_string());
    }

    let content = std::fs::read_to_string(&package_toml)
        .map_err(|e| format!("Cannot read package.toml: {}", e))?;

    // Try to parse as TOML first
    if let Ok(value) = content.parse::<toml::Value>() {
        // Check for version field
        if let Some(version_val) = value
            .get("package")
            .and_then(|p| p.get("version"))
            .or_else(|| value.get("version"))
        {
            if let Some(v) = version_val.as_str() {
                return Ok(v.to_string());
            }
        }
    }

    // Fallback: regex search
    use regex::Regex;
    let re = Regex::new(r#"version\s*=\s*"([^"]+)""#).unwrap();
    if let Some(caps) = re.captures(&content) {
        return Ok(caps[1].to_string());
    }

    Err("Could not find version in package.toml".to_string())
}

/// Emit a validation receipt for a single package
pub fn emit_receipt_for_package(
    package_path: &Path, package_id: &str, version: &str, marketplace_root: &Path,
) -> Result<PathBuf, String> {
    // Create validation receipt
    let mut receipt = ValidationReceipt::new(
        package_id.to_string(),
        version.to_string(),
        "3.0.0".to_string(), // ggen version
    );

    // Instantiate and execute standard guards
    let guards: Vec<Box<dyn Guard>> = vec![
        Box::new(GuardMetadata),
        Box::new(GuardLicense),
        Box::new(GuardReadme),
        Box::new(GuardTests),
        Box::new(GuardChicagoCompliance),
    ];

    for guard in guards {
        match guard.execute(package_path) {
            Ok(result) => {
                receipt.add_guard_result(result);
            }
            Err(e) => {
                // Log error but continue
                eprintln!(
                    "Failed to execute guard {} for {}: {}",
                    guard.name(),
                    package_id,
                    e
                );
            }
        }
    }

    // Calculate score and production-ready flag
    receipt.calculate_score();

    // Compute checksum for immutability
    receipt.compute_checksum();

    // Write receipt to disk
    receipt
        .write_to_file(marketplace_root)
        .map_err(|e| format!("Failed to write receipt: {}", e))
}

/// Update production_ready flags in index.json based on receipts
pub fn update_production_flags(
    registry_index_path: &Path, receipts_root: &Path,
) -> Result<(), String> {
    // Read current index.json
    let index_json = std::fs::read_to_string(registry_index_path)
        .map_err(|e| format!("Cannot read index.json: {}", e))?;

    let mut index: serde_json::Value = serde_json::from_str(&index_json)
        .map_err(|e| format!("Invalid JSON in index.json: {}", e))?;

    // Update each package's production_ready flag based on latest receipt
    if let Some(packages) = index.get_mut("packages").and_then(|p| p.as_array_mut()) {
        for package in packages.iter_mut() {
            if let Some(name) = package.get("name").and_then(|n| n.as_str()) {
                if let Ok(Some(receipt)) =
                    ValidationReceipt::latest_for_package(receipts_root, name)
                {
                    package["production_ready"] = serde_json::json!(receipt.production_ready);
                    package["validation_score"] = serde_json::json!(receipt.overall_score);
                    package["last_validated"] = serde_json::json!(receipt.validated_at);
                }
            }
        }
    }

    // Write updated index.json
    let updated_json = serde_json::to_string_pretty(&index)
        .map_err(|e| format!("Failed to serialize updated index: {}", e))?;

    std::fs::write(registry_index_path, updated_json)
        .map_err(|e| format!("Failed to write updated index.json: {}", e))
}

/// Generate validation report showing receipt statistics
pub fn generate_validation_report(marketplace_root: &Path) -> Result<ValidationReport, String> {
    let receipts_dir = marketplace_root.join("marketplace").join("receipts");

    if !receipts_dir.exists() {
        return Ok(ValidationReport::default());
    }

    let mut report = ValidationReport::default();
    report.generated_at = Utc::now().to_rfc3339();

    // Collect all receipts
    let mut all_scores = Vec::new();
    if let Ok(entries) = std::fs::read_dir(&receipts_dir) {
        for entry in entries.flatten() {
            let pkg_dir = entry.path();
            if pkg_dir.is_dir() {
                if let Some(pkg_id) = pkg_dir.file_name().and_then(|n| n.to_str()) {
                    if let Ok(Some(receipt)) =
                        ValidationReceipt::latest_for_package(marketplace_root, pkg_id)
                    {
                        all_scores.push(receipt.overall_score);

                        // Bucket by score
                        match receipt.overall_score {
                            s if s >= 95.0 => report.score_95_plus += 1,
                            s if s >= 80.0 => report.score_80_94 += 1,
                            _ => report.score_below_80 += 1,
                        }

                        // Track production ready
                        if receipt.production_ready {
                            report.production_ready_count += 1;
                        }

                        report.total_packages += 1;
                        report.packages.push(PackageReport {
                            id: pkg_id.to_string(),
                            score: receipt.overall_score,
                            production_ready: receipt.production_ready,
                            critical_passed: receipt.critical_passed,
                            critical_total: receipt.critical_total,
                            bonus_passed: receipt.bonus_passed,
                            bonus_total: receipt.bonus_total,
                        });
                    }
                }
            }
        }
    }

    // Calculate statistics
    if !all_scores.is_empty() {
        report.average_score = all_scores.iter().sum::<f64>() / all_scores.len() as f64;
        all_scores.sort_by(|a, b| a.partial_cmp(b).unwrap());
        report.median_score = all_scores[all_scores.len() / 2];
    }

    Ok(report)
}

/// Validation report with ecosystem statistics
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ValidationReport {
    pub generated_at: String,
    pub total_packages: usize,
    pub production_ready_count: usize,
    pub average_score: f64,
    pub median_score: f64,
    pub score_95_plus: usize,
    pub score_80_94: usize,
    pub score_below_80: usize,
    pub packages: Vec<PackageReport>,
}

impl Default for ValidationReport {
    fn default() -> Self {
        Self {
            generated_at: String::new(),
            total_packages: 0,
            production_ready_count: 0,
            average_score: 0.0,
            median_score: 0.0,
            score_95_plus: 0,
            score_80_94: 0,
            score_below_80: 0,
            packages: Vec::new(),
        }
    }
}

/// Per-package validation report
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackageReport {
    pub id: String,
    pub score: f64,
    pub production_ready: bool,
    pub critical_passed: usize,
    pub critical_total: usize,
    pub bonus_passed: usize,
    pub bonus_total: usize,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_extract_version() {
        // Will be tested with real packages
    }
}
