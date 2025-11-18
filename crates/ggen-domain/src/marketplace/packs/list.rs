//! List Packages Command
//!
//! Lists all installed gpack packages on the system with support for filtering,
//! sorting, and formatting in human-readable or machine-readable formats.
//!
//! # Examples
//!
//! List all packages:
//! ```bash
//! ggen packs list
//! ```
//!
//! List production-ready packages only:
//! ```bash
//! ggen packs list --production-only
//! ```
//!
//! List packages with quality score >= 80, sorted by quality:
//! ```bash
//! ggen packs list --quality-score 80 --sort quality
//! ```
//!
//! List as JSON for scripting:
//! ```bash
//! ggen packs list --format json | jq '.packages[] | select(.quality_score > 80)'
//! ```

use crate::marketplace::adapter::PackageInfo;
use crate::marketplace::packs_services::discovery::{
    DiscoveryFilter, PackageDiscoveryService, SortField,
};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Output format for list command
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable table format
    Human,
    /// JSON format
    Json,
    /// YAML format
    Yaml,
    /// CSV format
    Csv,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "human" => Ok(OutputFormat::Human),
            "json" => Ok(OutputFormat::Json),
            "yaml" => Ok(OutputFormat::Yaml),
            "csv" => Ok(OutputFormat::Csv),
            _ => Err(format!("Unknown format: {}", s)),
        }
    }
}

/// Options for the list command
#[derive(Debug, Clone)]
pub struct ListOptions {
    /// Output format (default: human)
    pub format: OutputFormat,
    /// Minimum quality score filter (0-100)
    pub quality_score: Option<u32>,
    /// Only include production-ready packages
    pub production_only: bool,
    /// Filter by author name
    pub author: Option<String>,
    /// Filter by name pattern
    pub name_pattern: Option<String>,
    /// Field to sort by (default: name)
    pub sort_by: SortField,
    /// Sort in reverse order
    pub sort_reverse: bool,
    /// Maximum number of results
    pub limit: Option<usize>,
}

impl Default for ListOptions {
    fn default() -> Self {
        Self {
            format: OutputFormat::Human,
            quality_score: None,
            production_only: false,
            author: None,
            name_pattern: None,
            sort_by: SortField::Name,
            sort_reverse: false,
            limit: None,
        }
    }
}

/// A single package in the list output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListedPackage {
    /// Package ID
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Current version
    pub version: String,
    /// Package author
    pub author: String,
    /// Quality score (0-100)
    pub quality_score: u32,
    /// Whether production-ready
    pub is_production_ready: bool,
    /// Disk size in bytes
    pub size_bytes: u64,
    /// Size in human-readable format
    pub size: String,
    /// Short description
    pub description: String,
}

impl ListedPackage {
    /// Create from PackageInfo
    fn from_package_info(pkg: &PackageInfo) -> Self {
        Self {
            id: pkg.id.clone(),
            name: pkg.name.clone(),
            version: pkg.version.clone(),
            author: pkg.author.clone(),
            quality_score: pkg.quality_score,
            is_production_ready: pkg.is_production_ready,
            size_bytes: pkg.size_bytes,
            size: format_size(pkg.size_bytes),
            description: pkg.description.clone(),
        }
    }
}

/// Output structure for list command
#[derive(Debug, Serialize, Deserialize)]
pub struct ListPackagesOutput {
    /// List of packages
    pub packages: Vec<ListedPackage>,
    /// Total count
    pub total: usize,
    /// Filters applied
    pub filters_applied: HashMap<String, String>,
    /// Sort order
    pub sort_info: SortInfo,
    /// Timestamp
    pub timestamp: String,
}

/// Information about how results were sorted
#[derive(Debug, Serialize, Deserialize)]
pub struct SortInfo {
    /// Field results were sorted by
    pub sort_by: String,
    /// Sort direction
    pub direction: String,
}

/// List all packages with filtering and sorting
///
/// # Arguments
/// * `registry` - The marketplace registry to query
/// * `options` - Listing options (filters, sort, format)
///
/// # Returns
/// Formatted output as JSON/YAML/Human/CSV
///
/// # Example
/// ```ignore
/// let options = ListOptions {
///     quality_score: Some(80),
///     production_only: true,
///     ..Default::default()
/// };
/// let output = list_packages(&registry, &options).await?;
/// println!("{}", output);
/// ```
pub async fn list_packages(
    registry: &dyn crate::marketplace::MarketplaceRegistry, options: &ListOptions,
) -> Result<String> {
    // Initialize discovery service
    let mut discovery = PackageDiscoveryService::new();

    // Discover all packages
    let packages = discovery.discover_all(registry).await?;

    // Build filter
    let mut filter = DiscoveryFilter::new();
    if let Some(quality) = options.quality_score {
        filter.min_quality_score = Some(quality);
    }
    if options.production_only {
        filter.production_ready_only = Some(true);
    }
    if let Some(ref author) = options.author {
        filter.author = Some(author.clone());
    }
    if let Some(ref pattern) = options.name_pattern {
        filter.name_pattern = Some(pattern.clone());
    }

    // Apply filter
    let filtered = discovery.filter(&filter)?;

    // Sort
    let sorted = discovery.sort(filtered, options.sort_by, options.sort_reverse);

    // Apply limit
    let limited = if let Some(limit) = options.limit {
        discovery.limit(sorted, limit)
    } else {
        sorted
    };

    // Convert to output format
    let listed_packages: Vec<ListedPackage> = limited
        .iter()
        .map(ListedPackage::from_package_info)
        .collect();

    // Build filters applied map
    let mut filters_applied = HashMap::new();
    if let Some(quality) = options.quality_score {
        filters_applied.insert("quality_score".to_string(), format!(">= {}", quality));
    }
    if options.production_only {
        filters_applied.insert("production_ready".to_string(), "true".to_string());
    }
    if let Some(ref author) = options.author {
        filters_applied.insert("author".to_string(), author.clone());
    }
    if let Some(ref pattern) = options.name_pattern {
        filters_applied.insert("name_pattern".to_string(), pattern.clone());
    }

    // Build output
    let output = ListPackagesOutput {
        packages: listed_packages.clone(),
        total: listed_packages.len(),
        filters_applied,
        sort_info: SortInfo {
            sort_by: format!("{:?}", options.sort_by),
            direction: if options.sort_reverse {
                "descending".to_string()
            } else {
                "ascending".to_string()
            },
        },
        timestamp: chrono::Utc::now().to_rfc3339(),
    };

    // Format output
    match options.format {
        OutputFormat::Json => serde_json::to_string_pretty(&output)
            .map_err(|e| Error::with_source("Failed to render packages as JSON", Box::new(e))),
        OutputFormat::Yaml => serde_yaml::to_string(&output)
            .map_err(|e| Error::with_source("Failed to render packages as YAML", Box::new(e))),
        OutputFormat::Csv => format_as_csv(&listed_packages),
        OutputFormat::Human => format_as_human(&listed_packages, &output),
    }
}

/// Format packages as human-readable table
fn format_as_human(packages: &[ListedPackage], output: &ListPackagesOutput) -> Result<String> {
    let mut result = String::new();

    // Header
    result.push_str("Package Management - Installed Packages\n");
    result.push_str("═══════════════════════════════════════════════════════════════════════════════════════════════════════\n");

    // Filters info
    if !output.filters_applied.is_empty() {
        result.push_str("Filters Applied:\n");
        for (key, value) in &output.filters_applied {
            result.push_str(&format!("  • {}: {}\n", key, value));
        }
        result.push('\n');
    }

    // Table header
    result.push_str(&format!(
        "{:<20} {:<15} {:<8} {:<8} {:<30}\n",
        "Package", "Version", "Quality", "Size", "Description"
    ));
    result.push_str("─────────────────────────────────────────────────────────────────────────────────────────────────────\n");

    // Table rows
    for pkg in packages {
        let prod_indicator = if pkg.is_production_ready { "✓" } else { " " };
        result.push_str(&format!(
            "{:<20} {:<15} {:<8} {:<8} {:<30}\n",
            format!("{} [{}]", pkg.name, prod_indicator),
            pkg.version,
            format!("{}/100", pkg.quality_score),
            pkg.size,
            &pkg.description[..std::cmp::min(30, pkg.description.len())]
        ));
    }

    // Footer
    result.push_str("─────────────────────────────────────────────────────────────────────────────────────────────────────\n");
    result.push_str(&format!(
        "Total: {} packages | Sorted by: {} ({})\n",
        output.total, output.sort_info.sort_by, output.sort_info.direction
    ));

    Ok(result)
}

/// Format packages as CSV
fn format_as_csv(packages: &[ListedPackage]) -> Result<String> {
    let mut result =
        String::from("id,name,version,author,quality_score,is_production_ready,size_bytes,size\n");

    for pkg in packages {
        result.push_str(&format!(
            "\"{}\",\"{}\",\"{}\",\"{}\",{},{},{},\"{}\"\n",
            pkg.id,
            pkg.name,
            pkg.version,
            pkg.author,
            pkg.quality_score,
            pkg.is_production_ready,
            pkg.size_bytes,
            pkg.size
        ));
    }

    Ok(result)
}

/// Format bytes as human-readable size
fn format_size(bytes: u64) -> String {
    const UNITS: &[&str] = &["B", "KB", "MB", "GB", "TB"];
    let mut size = bytes as f64;
    let mut unit_idx = 0;

    while size >= 1024.0 && unit_idx < UNITS.len() - 1 {
        size /= 1024.0;
        unit_idx += 1;
    }

    if unit_idx == 0 {
        format!("{} {}", size as u64, UNITS[unit_idx])
    } else {
        format!("{:.2} {}", size, UNITS[unit_idx])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_size_bytes() {
        assert_eq!(format_size(512), "512 B");
        assert_eq!(format_size(1024), "1.00 KB");
        assert_eq!(format_size(1_048_576), "1.00 MB");
        assert_eq!(format_size(1_073_741_824), "1.00 GB");
    }

    #[test]
    fn test_output_format_parsing() {
        assert_eq!(
            "human".parse::<OutputFormat>().unwrap(),
            OutputFormat::Human
        );
        assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
        assert_eq!("yaml".parse::<OutputFormat>().unwrap(), OutputFormat::Yaml);
        assert_eq!("csv".parse::<OutputFormat>().unwrap(), OutputFormat::Csv);
        assert!("invalid".parse::<OutputFormat>().is_err());
    }

    #[test]
    fn test_listed_package_creation() {
        let pkg = PackageInfo {
            id: "test-pkg".to_string(),
            name: "Test Package".to_string(),
            version: "1.0.0".to_string(),
            author: "Test Author".to_string(),
            quality_score: 85,
            is_production_ready: true,
            downloads: 0,
            description: "A test package".to_string(),
            repository: None,
            license: None,
            homepage: None,
            location: std::path::PathBuf::from("/test"),
            size_bytes: 1_024_000,
            published_at: chrono::Utc::now(),
            last_used: chrono::Utc::now(),
            created_at: chrono::Utc::now(),
            is_deprecated: false,
            deprecation_notice: None,
            metadata: std::collections::HashMap::new(),
        };

        let listed = ListedPackage::from_package_info(&pkg);
        assert_eq!(listed.id, "test-pkg");
        assert_eq!(listed.quality_score, 85);
        assert!(listed.is_production_ready);
    }
}
