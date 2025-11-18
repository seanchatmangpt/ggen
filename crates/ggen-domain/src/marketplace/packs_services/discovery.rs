//! Package Discovery Service
//!
//! Discovers and indexes all gpack packages on a system, providing filtering,
//! sorting, and searching capabilities. Uses the MarketplaceRegistry adapter
//! to support both v1 (legacy) and v2 (RDF-backed) marketplace implementations.

use crate::marketplace::adapter::{PackageInfo, SearchMatch};
use crate::error::Result;
use std::cmp::Ordering;
use std::collections::HashMap;

/// Sorting field for package listings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortField {
    /// Sort by package name (A-Z)
    Name,
    /// Sort by version number
    Version,
    /// Sort by disk size (largest first)
    Size,
    /// Sort by quality score (highest first)
    Quality,
    /// Sort by last used timestamp (most recent first)
    LastUsed,
}

impl std::str::FromStr for SortField {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "name" => Ok(SortField::Name),
            "version" => Ok(SortField::Version),
            "size" => Ok(SortField::Size),
            "quality" => Ok(SortField::Quality),
            "last-used" => Ok(SortField::LastUsed),
            _ => Err(format!("Unknown sort field: {}", s)),
        }
    }
}

/// Filter criteria for package discovery
#[derive(Debug, Clone, Default)]
pub struct DiscoveryFilter {
    /// Minimum quality score (0-100)
    pub min_quality_score: Option<u32>,
    /// Only include production-ready packages
    pub production_ready_only: Option<bool>,
    /// Filter by author
    pub author: Option<String>,
    /// Filter by name pattern (substring match)
    pub name_pattern: Option<String>,
}

impl DiscoveryFilter {
    /// Create a new empty filter
    pub fn new() -> Self {
        Self::default()
    }

    /// Set minimum quality score
    pub fn with_quality_score(mut self, min_score: u32) -> Self {
        self.min_quality_score = Some(min_score);
        self
    }

    /// Set production-ready filter
    pub fn production_ready(mut self) -> Self {
        self.production_ready_only = Some(true);
        self
    }

    /// Set author filter
    pub fn with_author(mut self, author: String) -> Self {
        self.author = Some(author);
        self
    }

    /// Set name pattern filter
    pub fn with_name_pattern(mut self, pattern: String) -> Self {
        self.name_pattern = Some(pattern);
        self
    }

    /// Check if a package matches all filter criteria
    fn matches(&self, package: &PackageInfo) -> bool {
        // Check quality score
        if let Some(min_score) = self.min_quality_score {
            if package.quality_score < min_score {
                return false;
            }
        }

        // Check production-ready flag
        if let Some(prod_only) = self.production_ready_only {
            if prod_only && !package.is_production_ready {
                return false;
            }
        }

        // Check author
        if let Some(ref author_filter) = self.author {
            if !package.author.to_lowercase().contains(&author_filter.to_lowercase()) {
                return false;
            }
        }

        // Check name pattern
        if let Some(ref pattern) = self.name_pattern {
            if !package.name.to_lowercase().contains(&pattern.to_lowercase()) {
                return false;
            }
        }

        true
    }
}

/// Package Discovery Service
///
/// Discovers all packages on a system and provides filtering, sorting,
/// and search capabilities. Works with both v1 and v2 marketplace implementations
/// through the MarketplaceRegistry adapter.
pub struct PackageDiscoveryService {
    /// Cache of discovered packages
    packages_cache: HashMap<String, PackageInfo>,
    /// Whether cache is current
    cache_valid: bool,
}

impl PackageDiscoveryService {
    /// Create a new discovery service
    pub fn new() -> Self {
        Self {
            packages_cache: HashMap::new(),
            cache_valid: false,
        }
    }

    /// Discover all packages on the system
    ///
    /// # Arguments
    /// * `registry` - The marketplace registry adapter
    ///
    /// # Returns
    /// A list of all discovered packages
    ///
    /// # Example
    /// ```ignore
    /// let service = PackageDiscoveryService::new();
    /// let packages = service.discover_all(&registry).await?;
    /// println!("Found {} packages", packages.len());
    /// ```
    pub async fn discover_all(
        &mut self,
        registry: &dyn crate::marketplace::MarketplaceRegistry,
    ) -> Result<Vec<PackageInfo>> {
        // List all packages from the registry
        let packages = registry.list_all().await?;

        // Update cache
        self.packages_cache = packages.iter().map(|p| (p.id.clone(), p.clone())).collect();
        self.cache_valid = true;

        Ok(packages)
    }

    /// Filter packages based on criteria
    ///
    /// # Arguments
    /// * `filter` - Filter criteria to apply
    ///
    /// # Returns
    /// Filtered packages (must call discover_all first)
    ///
    /// # Example
    /// ```ignore
    /// let service = PackageDiscoveryService::new();
    /// service.discover_all(&registry).await?;
    ///
    /// let filter = DiscoveryFilter::new().with_quality_score(80);
    /// let filtered = service.filter(&filter)?;
    /// ```
    pub fn filter(&self, filter: &DiscoveryFilter) -> Result<Vec<PackageInfo>> {
        if !self.cache_valid {
            return Err(crate::error::Error::Other(
                "Cache not valid. Call discover_all first.".into(),
            ));
        }

        let filtered: Vec<PackageInfo> = self
            .packages_cache
            .values()
            .filter(|p| filter.matches(p))
            .cloned()
            .collect();

        Ok(filtered)
    }

    /// Sort packages by specified field
    ///
    /// # Arguments
    /// * `packages` - Packages to sort
    /// * `sort_field` - Field to sort by
    /// * `reverse` - If true, sort in descending order
    ///
    /// # Returns
    /// Sorted packages
    pub fn sort(
        &self,
        mut packages: Vec<PackageInfo>,
        sort_field: SortField,
        reverse: bool,
    ) -> Vec<PackageInfo> {
        packages.sort_by(|a, b| {
            let cmp = match sort_field {
                SortField::Name => a.name.cmp(&b.name),
                SortField::Version => a.version.cmp(&b.version),
                SortField::Size => a.size_bytes.cmp(&b.size_bytes),
                SortField::Quality => b.quality_score.cmp(&a.quality_score), // Higher scores first
                SortField::LastUsed => b.last_used.cmp(&a.last_used), // More recent first
            };

            if reverse {
                cmp.reverse()
            } else {
                cmp
            }
        });

        packages
    }

    /// Limit results to a maximum count
    pub fn limit(&self, packages: Vec<PackageInfo>, limit: usize) -> Vec<PackageInfo> {
        packages.into_iter().take(limit).collect()
    }

    /// Get total package count
    pub fn total_count(&self) -> usize {
        self.packages_cache.len()
    }

    /// Get total size of all packages in bytes
    pub fn total_size_bytes(&self) -> u64 {
        self.packages_cache
            .values()
            .map(|p| p.size_bytes)
            .sum()
    }

    /// Clear the cache
    pub fn clear_cache(&mut self) {
        self.packages_cache.clear();
        self.cache_valid = false;
    }
}

impl Default for PackageDiscoveryService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_package(id: &str, quality: u32, production: bool) -> PackageInfo {
        PackageInfo {
            id: id.to_string(),
            name: format!("Package {}", id),
            version: "1.0.0".to_string(),
            author: "Test Author".to_string(),
            quality_score: quality,
            is_production_ready: production,
            description: "Test package".to_string(),
            repository: None,
            license: None,
            homepage: None,
            location: std::path::PathBuf::from("/test"),
            size_bytes: 1000,
            published_at: chrono::Utc::now(),
            last_used: chrono::Utc::now(),
            created_at: chrono::Utc::now(),
            is_deprecated: false,
            deprecation_notice: None,
            metadata: std::collections::HashMap::new(),
        }
    }

    #[test]
    fn test_discovery_filter_quality() {
        let filter = DiscoveryFilter::new().with_quality_score(80);

        let pkg_good = create_test_package("good", 85, true);
        let pkg_bad = create_test_package("bad", 70, true);

        assert!(filter.matches(&pkg_good));
        assert!(!filter.matches(&pkg_bad));
    }

    #[test]
    fn test_discovery_filter_production_ready() {
        let filter = DiscoveryFilter::new().production_ready();

        let pkg_prod = create_test_package("prod", 80, true);
        let pkg_dev = create_test_package("dev", 80, false);

        assert!(filter.matches(&pkg_prod));
        assert!(!filter.matches(&pkg_dev));
    }

    #[test]
    fn test_discovery_filter_author() {
        let mut filter = DiscoveryFilter::new();
        filter.author = Some("React".to_string());

        let mut pkg = create_test_package("react", 90, true);
        pkg.author = "React Core Team".to_string();

        assert!(filter.matches(&pkg));
    }

    #[test]
    fn test_discovery_filter_name_pattern() {
        let filter = DiscoveryFilter::new().with_name_pattern("database");

        let mut pkg = create_test_package("db", 80, true);
        pkg.name = "Database ORM".to_string();

        assert!(filter.matches(&pkg));
    }

    #[test]
    fn test_sort_by_name() {
        let service = PackageDiscoveryService::new();

        let mut packages = vec![
            create_test_package("zebra", 80, true),
            create_test_package("apple", 85, true),
            create_test_package("mango", 90, true),
        ];

        let sorted = service.sort(packages, SortField::Name, false);
        assert_eq!(sorted[0].id, "apple");
        assert_eq!(sorted[1].id, "mango");
        assert_eq!(sorted[2].id, "zebra");
    }

    #[test]
    fn test_sort_by_quality() {
        let service = PackageDiscoveryService::new();

        let packages = vec![
            create_test_package("low", 60, true),
            create_test_package("high", 95, true),
            create_test_package("mid", 75, true),
        ];

        let sorted = service.sort(packages, SortField::Quality, false);
        assert_eq!(sorted[0].quality_score, 95);
        assert_eq!(sorted[1].quality_score, 75);
        assert_eq!(sorted[2].quality_score, 60);
    }

    #[test]
    fn test_limit() {
        let service = PackageDiscoveryService::new();

        let packages = vec![
            create_test_package("pkg1", 80, true),
            create_test_package("pkg2", 80, true),
            create_test_package("pkg3", 80, true),
        ];

        let limited = service.limit(packages, 2);
        assert_eq!(limited.len(), 2);
    }
}
