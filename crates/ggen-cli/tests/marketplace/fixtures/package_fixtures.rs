//! Test package fixtures for marketplace testing
//!
//! Provides realistic package data for various test scenarios including
//! normal cases, edge cases, and invalid data.

use chrono::Utc;
use ggen_marketplace::Package as V1Package;
use ggen_marketplace::{MaturityLevel, PackageMetadata as V1Metadata};
use ggen_marketplace_v2::models::{
    Package as V2Package, PackageId, PackageMetadata as V2Metadata, PackageVersion,
};

/// Package fixtures for comprehensive testing
pub struct PackageFixtures {
    pub minimal: Vec<V1Package>,
    pub standard: Vec<V1Package>,
    pub complex: Vec<V1Package>,
    pub unicode: Vec<V1Package>,
    pub edge_cases: Vec<V1Package>,
}

impl PackageFixtures {
    /// Create complete fixture set with 100 packages
    pub fn new() -> Self {
        Self {
            minimal: Self::create_minimal_packages(),
            standard: Self::create_standard_packages(),
            complex: Self::create_complex_packages(),
            unicode: Self::create_unicode_packages(),
            edge_cases: Self::create_edge_case_packages(),
        }
    }

    /// Create 10 minimal packages (bare minimum fields)
    fn create_minimal_packages() -> Vec<V1Package> {
        (0..10)
            .map(|i| V1Package {
                metadata: V1Metadata {
                    id: format!("minimal-{}", i),
                    name: format!("Minimal Package {}", i),
                    version: "1.0.0".to_string(),
                    description: "Minimal description".to_string(),
                    author: "test-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec![],
                },
            })
            .collect()
    }

    /// Create 50 standard packages (typical real-world packages)
    fn create_standard_packages() -> Vec<V1Package> {
        let categories = vec!["database", "web", "cli", "data", "network"];
        let licenses = vec!["MIT", "Apache-2.0", "GPL-3.0", "BSD-3-Clause"];

        (0..50)
            .map(|i| {
                let category = categories[i % categories.len()];
                let license = licenses[i % licenses.len()];

                V1Package {
                    metadata: V1Metadata {
                        id: format!("{}-package-{}", category, i),
                        name: format!("{} Package {}", category, i),
                        version: format!("{}.{}.{}", 1 + i / 10, i % 10, 0),
                        description: format!("A {} package for testing", category),
                        author: format!("author-{}", i),
                        license: license.to_string(),
                        repository_url: Some(format!("https://github.com/test/{}-{}", category, i)),
                        created_at: Utc::now(),
                        updated_at: Utc::now(),
                        maturity: MaturityLevel::Stable,
                        tags: vec![category.to_string(), "rust".to_string()],
                    },
                }
            })
            .collect()
    }

    /// Create 20 complex packages (multi-version, many dependencies)
    fn create_complex_packages() -> Vec<V1Package> {
        (0..20)
            .map(|i| V1Package {
                metadata: V1Metadata {
                    id: format!("complex-{}", i),
                    name: format!("Complex Package {}", i),
                    version: format!("{}.{}.{}", 2 + i / 5, i % 5, i % 3),
                    description: format!(
                        "Complex package with many features: {}. {}",
                        "feature1, feature2, feature3, feature4, feature5",
                        "Long description".repeat(10)
                    ),
                    author: format!("Complex Author {} <author{}@example.com>", i, i),
                    license: "MIT OR Apache-2.0".to_string(),
                    repository_url: Some(format!("https://github.com/complex/pkg-{}", i)),
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: if i % 3 == 0 {
                        MaturityLevel::Stable
                    } else {
                        MaturityLevel::Mature
                    },
                    tags: (0..10).map(|t| format!("tag-{}", t)).collect(),
                },
            })
            .collect()
    }

    /// Create 10 unicode packages (international characters, emojis)
    fn create_unicode_packages() -> Vec<V1Package> {
        let names = vec![
            "测试包 🚀",
            "Тестовый пакет 📦",
            "パッケージテスト 🎌",
            "패키지 테스트 🇰🇷",
            "حزمة اختبار 🌍",
            "Δοκιμαστικό πακέτο 🏛️",
            "पैकेज परीक्षण 🇮🇳",
            "Paket ujian 🇮🇩",
            "Pacote de teste 🇧🇷",
            "Paquete de prueba 🇪🇸",
        ];

        names
            .iter()
            .enumerate()
            .map(|(i, name)| V1Package {
                metadata: V1Metadata {
                    id: format!("unicode-{}", i),
                    name: (*name).to_string(),
                    version: "1.0.0".to_string(),
                    description: format!("Unicode test package: {}", name),
                    author: format!("作者-{}", i),
                    license: "MIT".to_string(),
                    repository_url: Some(format!("https://github.com/unicode/pkg-{}", i)),
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec!["unicode".to_string(), "test".to_string()],
                },
            })
            .collect()
    }

    /// Create 10 edge case packages (special chars, extremes)
    fn create_edge_case_packages() -> Vec<V1Package> {
        vec![
            // Long name
            V1Package {
                metadata: V1Metadata {
                    id: "edge-long-name".to_string(),
                    name: "x".repeat(100),
                    version: "1.0.0".to_string(),
                    description: "Edge case: very long name".to_string(),
                    author: "edge-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec![],
                },
            },
            // Long description
            V1Package {
                metadata: V1Metadata {
                    id: "edge-long-desc".to_string(),
                    name: "Long Description".to_string(),
                    version: "1.0.0".to_string(),
                    description: "x".repeat(10000),
                    author: "edge-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec![],
                },
            },
            // Many tags
            V1Package {
                metadata: V1Metadata {
                    id: "edge-many-tags".to_string(),
                    name: "Many Tags".to_string(),
                    version: "1.0.0".to_string(),
                    description: "Edge case: 100 tags".to_string(),
                    author: "edge-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: (0..100).map(|i| format!("tag-{}", i)).collect(),
                },
            },
            // Special characters in name
            V1Package {
                metadata: V1Metadata {
                    id: "edge-special-chars".to_string(),
                    name: r#"Test "Package" <Name> & More"#.to_string(),
                    version: "1.0.0".to_string(),
                    description: "Edge case: special characters".to_string(),
                    author: "edge-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec![],
                },
            },
            // Version extremes
            V1Package {
                metadata: V1Metadata {
                    id: "edge-version-extreme".to_string(),
                    name: "Version Extreme".to_string(),
                    version: "999.999.999".to_string(),
                    description: "Edge case: extreme version number".to_string(),
                    author: "edge-author".to_string(),
                    license: "MIT".to_string(),
                    repository_url: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    maturity: MaturityLevel::Stable,
                    tags: vec![],
                },
            },
        ]
    }

    /// Get all packages (total: 100+)
    pub fn all(&self) -> Vec<V1Package> {
        let mut all = Vec::new();
        all.extend(self.minimal.clone());
        all.extend(self.standard.clone());
        all.extend(self.complex.clone());
        all.extend(self.unicode.clone());
        all.extend(self.edge_cases.clone());
        all
    }

    /// Get count of all packages
    pub fn count(&self) -> usize {
        self.minimal.len()
            + self.standard.len()
            + self.complex.len()
            + self.unicode.len()
            + self.edge_cases.len()
    }
}

impl Default for PackageFixtures {
    fn default() -> Self {
        Self::new()
    }
}

/// Invalid package IDs for error testing
pub fn invalid_package_ids() -> Vec<&'static str> {
    vec![
        "",                       // Empty
        "-invalid",               // Starts with hyphen
        "invalid-",               // Ends with hyphen
        "invalid package",        // Contains space
        "invalid!@#",             // Special characters
        "INVALID",                // Uppercase (depending on validation rules)
        "in",                     // Too short
        "a".repeat(100).as_str(), // Too long (if stored)
    ]
}

/// Invalid version strings for error testing
pub fn invalid_versions() -> Vec<&'static str> {
    vec![
        "",
        "1",
        "1.0",
        "v1.0.0",
        "1.0.0-",
        "1.0.0.0",
        "a.b.c",
        "1.0.0+invalid",
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixture_counts() {
        let fixtures = PackageFixtures::new();

        assert_eq!(fixtures.minimal.len(), 10);
        assert_eq!(fixtures.standard.len(), 50);
        assert_eq!(fixtures.complex.len(), 20);
        assert_eq!(fixtures.unicode.len(), 10);
        assert_eq!(fixtures.edge_cases.len(), 5);

        assert_eq!(fixtures.count(), 95);
    }

    #[test]
    fn test_all_packages_unique_ids() {
        let fixtures = PackageFixtures::new();
        let all = fixtures.all();

        let mut ids: std::collections::HashSet<String> = std::collections::HashSet::new();

        for pkg in &all {
            assert!(
                ids.insert(pkg.metadata.id.clone()),
                "Duplicate ID: {}",
                pkg.metadata.id
            );
        }
    }

    #[test]
    fn test_minimal_packages_valid() {
        let fixtures = PackageFixtures::new();

        for pkg in &fixtures.minimal {
            assert!(!pkg.metadata.id.is_empty());
            assert!(!pkg.metadata.name.is_empty());
            assert!(!pkg.metadata.version.is_empty());
        }
    }

    #[test]
    fn test_unicode_packages_contain_unicode() {
        let fixtures = PackageFixtures::new();

        for pkg in &fixtures.unicode {
            // Should contain non-ASCII characters
            assert!(pkg.metadata.name.chars().any(|c| !c.is_ascii()));
        }
    }
}
