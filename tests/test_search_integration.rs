use anyhow::Result;
use core::registry::{RegistryClient, SearchParams, PackMetadata, VersionMetadata, SearchResult};
use std::collections::HashMap;
use tempfile::TempDir;
use std::fs;
use url::Url;

/// Mock registry client for testing
struct MockRegistryClient {
    packs: HashMap<String, PackMetadata>,
}

impl MockRegistryClient {
    fn new() -> Self {
        let mut packs = HashMap::new();
        
        // Add test packs with comprehensive metadata
        packs.insert("io.rgen.rust.cli-subcommand".to_string(), PackMetadata {
            id: "io.rgen.rust.cli-subcommand".to_string(),
            name: "Rust CLI Subcommand Generator".to_string(),
            description: "Generate clap subcommands for Rust CLI applications with full argument parsing".to_string(),
            tags: vec!["rust".to_string(), "cli".to_string(), "clap".to_string(), "subcommand".to_string()],
            keywords: vec!["command-line".to_string(), "argument-parsing".to_string(), "interactive".to_string(), "help".to_string()],
            category: Some("rust".to_string()),
            author: Some("rgen-team".to_string()),
            latest_version: "1.2.0".to_string(),
            versions: {
                let mut versions = HashMap::new();
                versions.insert("1.2.0".to_string(), VersionMetadata {
                    version: "1.2.0".to_string(),
                    git_url: "https://github.com/rgen-team/rust-cli-templates.git".to_string(),
                    git_rev: "abc123".to_string(),
                    manifest_url: None,
                    sha256: "def456".to_string(),
                });
                versions
            },
            downloads: Some(15420),
            updated: Some(chrono::Utc::now()),
            license: Some("MIT".to_string()),
            homepage: Some("https://rgen.dev/templates/rust-cli".to_string()),
            repository: Some("https://github.com/rgen-team/rust-cli-templates".to_string()),
            documentation: Some("https://docs.rgen.dev/rust-cli".to_string()),
        });
        
        packs.insert("io.rgen.python.web-api".to_string(), PackMetadata {
            id: "io.rgen.python.web-api".to_string(),
            name: "Python Web API Generator".to_string(),
            description: "Generate FastAPI web APIs with database models and authentication".to_string(),
            tags: vec!["python".to_string(), "web".to_string(), "api".to_string(), "fastapi".to_string()],
            keywords: vec!["rest-api".to_string(), "database".to_string(), "auth".to_string(), "swagger".to_string(), "async".to_string()],
            category: Some("python".to_string()),
            author: Some("python-dev".to_string()),
            latest_version: "2.1.0-beta.1".to_string(),
            versions: {
                let mut versions = HashMap::new();
                versions.insert("2.1.0-beta.1".to_string(), VersionMetadata {
                    version: "2.1.0-beta.1".to_string(),
                    git_url: "https://github.com/python-dev/web-api-templates.git".to_string(),
                    git_rev: "xyz789".to_string(),
                    manifest_url: None,
                    sha256: "ghi012".to_string(),
                });
                versions
            },
            downloads: Some(8750),
            updated: Some(chrono::Utc::now()),
            license: Some("Apache-2.0".to_string()),
            homepage: Some("https://rgen.dev/templates/python-api".to_string()),
            repository: Some("https://github.com/python-dev/web-api-templates".to_string()),
            documentation: Some("https://docs.rgen.dev/python-api".to_string()),
        });
        
        packs.insert("io.rgen.web.react-component".to_string(), PackMetadata {
            id: "io.rgen.web.react-component".to_string(),
            name: "React Component Generator".to_string(),
            description: "Generate React components with TypeScript, testing, and styling".to_string(),
            tags: vec!["web".to_string(), "react".to_string(), "typescript".to_string(), "component".to_string()],
            keywords: vec!["frontend".to_string(), "ui".to_string(), "testing".to_string(), "styled-components".to_string(), "hooks".to_string()],
            category: Some("web".to_string()),
            author: Some("frontend-team".to_string()),
            latest_version: "3.0.0".to_string(),
            versions: {
                let mut versions = HashMap::new();
                versions.insert("3.0.0".to_string(), VersionMetadata {
                    version: "3.0.0".to_string(),
                    git_url: "https://github.com/frontend-team/react-templates.git".to_string(),
                    git_rev: "mno345".to_string(),
                    manifest_url: None,
                    sha256: "pqr678".to_string(),
                });
                versions
            },
            downloads: Some(23400),
            updated: Some(chrono::Utc::now()),
            license: Some("MIT".to_string()),
            homepage: Some("https://rgen.dev/templates/react".to_string()),
            repository: Some("https://github.com/frontend-team/react-templates".to_string()),
            documentation: Some("https://docs.rgen.dev/react".to_string()),
        });
        
        Self { packs }
    }
    
    fn mock_advanced_search(&self, params: &SearchParams<'_>) -> Result<Vec<SearchResult>> {
        let query_lower = params.query.to_lowercase();
        let mut results = Vec::new();
        
        for (id, pack) in &self.packs {
            // Apply filters
            if !self.matches_filters(pack, params) {
                continue;
            }
            
            // Search in name, description, tags, and keywords
            let matches = pack.name.to_lowercase().contains(&query_lower)
                || pack.description.to_lowercase().contains(&query_lower)
                || pack.tags.iter().any(|tag| tag.to_lowercase().contains(&query_lower))
                || pack.keywords.iter().any(|keyword| keyword.to_lowercase().contains(&query_lower));
            
            if matches {
                let search_result = self.convert_to_search_result(id.clone(), pack.clone())?;
                results.push(search_result);
            }
        }
        
        // Sort by relevance and apply limit
        results.sort_by(|a, b| self.compare_relevance(a, b, &query_lower));
        results.truncate(params.limit);
        
        Ok(results)
    }
    
    fn matches_filters(&self, pack: &PackMetadata, params: &SearchParams<'_>) -> bool {
        // Category filter
        if let Some(category) = params.category {
            if !pack.category.as_ref().map_or(false, |c| c.to_lowercase() == category.to_lowercase()) {
                return false;
            }
        }
        
        // Keyword filter
        if let Some(keyword) = params.keyword {
            if !pack.keywords.iter().any(|k| k.to_lowercase() == keyword.to_lowercase()) {
                return false;
            }
        }
        
        // Author filter
        if let Some(author) = params.author {
            if !pack.author.as_ref().map_or(false, |a| a.to_lowercase().contains(&author.to_lowercase())) {
                return false;
            }
        }
        
        // Stable version filter
        if params.stable_only {
            if let Ok(version) = semver::Version::parse(&pack.latest_version) {
                if !version.pre.is_empty() {
                    return false; // Pre-release versions are not stable
                }
            }
        }
        
        true
    }
    
    fn convert_to_search_result(&self, id: String, pack: PackMetadata) -> Result<SearchResult> {
        Ok(SearchResult {
            id,
            name: pack.name,
            description: pack.description,
            tags: pack.tags,
            keywords: pack.keywords,
            category: pack.category,
            author: pack.author,
            latest_version: pack.latest_version,
            downloads: pack.downloads,
            updated: pack.updated,
            license: pack.license,
            homepage: pack.homepage,
            repository: pack.repository,
            documentation: pack.documentation,
        })
    }
    
    fn compare_relevance(&self, a: &SearchResult, b: &SearchResult, query: &str) -> std::cmp::Ordering {
        // Exact matches first
        let a_exact = a.id.to_lowercase() == query || a.name.to_lowercase() == query;
        let b_exact = b.id.to_lowercase() == query || b.name.to_lowercase() == query;
        
        match (a_exact, b_exact) {
            (true, false) => return std::cmp::Ordering::Less,
            (false, true) => return std::cmp::Ordering::Greater,
            _ => {}
        }
        
        // Then by downloads (popularity)
        let download_ordering = match (a.downloads, b.downloads) {
            (Some(a_dl), Some(b_dl)) => b_dl.cmp(&a_dl), // Higher downloads first
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        };
        
        if download_ordering != std::cmp::Ordering::Equal {
            return download_ordering;
        }
        
        // Finally by name
        a.name.cmp(&b.name)
    }
    
    fn get_popular_categories(&self) -> Result<Vec<(String, u64)>> {
        let mut category_counts: std::collections::HashMap<String, u64> = std::collections::HashMap::new();
        
        for (_, pack) in &self.packs {
            if let Some(category) = &pack.category {
                *category_counts.entry(category.clone()).or_insert(0) += 1;
            }
        }
        
        let mut categories: Vec<(String, u64)> = category_counts.into_iter().collect();
        categories.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending
        
        Ok(categories)
    }
    
    fn get_popular_keywords(&self) -> Result<Vec<(String, u64)>> {
        let mut keyword_counts: std::collections::HashMap<String, u64> = std::collections::HashMap::new();
        
        for (_, pack) in &self.packs {
            for keyword in &pack.keywords {
                *keyword_counts.entry(keyword.clone()).or_insert(0) += 1;
            }
        }
        
        let mut keywords: Vec<(String, u64)> = keyword_counts.into_iter().collect();
        keywords.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending
        
        Ok(keywords)
    }
}

#[test]
fn test_basic_search() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "rust",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find the Rust CLI template
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.rust.cli-subcommand");
    assert_eq!(results[0].name, "Rust CLI Subcommand Generator");
    
    Ok(())
}

#[test]
fn test_category_filtering() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "generator",
        category: Some("python"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find only the Python template
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.python.web-api");
    assert_eq!(results[0].category, Some("python".to_string()));
    
    Ok(())
}

#[test]
fn test_keyword_filtering() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "component",
        category: None,
        keyword: Some("testing"),
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find the React component template
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.web.react-component");
    assert!(results[0].keywords.contains(&"testing".to_string()));
    
    Ok(())
}

#[test]
fn test_author_filtering() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "generator",
        category: None,
        keyword: None,
        author: Some("rgen-team"),
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find only the rgen-team template
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.rust.cli-subcommand");
    assert_eq!(results[0].author, Some("rgen-team".to_string()));
    
    Ok(())
}

#[test]
fn test_stable_version_filtering() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "api",
        category: None,
        keyword: None,
        author: None,
        stable_only: true,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find only stable versions (exclude beta versions)
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.web.react-component");
    assert!(!results[0].latest_version.contains("beta"));
    
    Ok(())
}

#[test]
fn test_limit_functionality() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "generator",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 1,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should be limited to 1 result
    assert_eq!(results.len(), 1);
    
    Ok(())
}

#[test]
fn test_relevance_sorting() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "generator",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should be sorted by downloads (popularity)
    assert_eq!(results.len(), 3);
    
    // React component should be first (highest downloads: 23400)
    assert_eq!(results[0].id, "io.rgen.web.react-component");
    assert_eq!(results[0].downloads, Some(23400));
    
    // Rust CLI should be second (15420 downloads)
    assert_eq!(results[1].id, "io.rgen.rust.cli-subcommand");
    assert_eq!(results[1].downloads, Some(15420));
    
    // Python API should be third (8750 downloads)
    assert_eq!(results[2].id, "io.rgen.python.web-api");
    assert_eq!(results[2].downloads, Some(8750));
    
    Ok(())
}

#[test]
fn test_popular_categories() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let categories = client.get_popular_categories()?;
    
    // Should have 3 categories
    assert_eq!(categories.len(), 3);
    
    // Check that categories are sorted by count
    assert_eq!(categories[0].0, "web");
    assert_eq!(categories[0].1, 1);
    
    assert_eq!(categories[1].0, "rust");
    assert_eq!(categories[1].1, 1);
    
    assert_eq!(categories[2].0, "python");
    assert_eq!(categories[2].1, 1);
    
    Ok(())
}

#[test]
fn test_popular_keywords() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let keywords = client.get_popular_keywords()?;
    
    // Should have multiple keywords
    assert!(keywords.len() > 0);
    
    // Check that keywords are sorted by count
    for i in 1..keywords.len() {
        assert!(keywords[i-1].1 >= keywords[i].1);
    }
    
    // Check for specific keywords
    let keyword_names: Vec<&String> = keywords.iter().map(|(name, _)| name).collect();
    assert!(keyword_names.contains(&&"testing".to_string()));
    assert!(keyword_names.contains(&&"api".to_string()));
    assert!(keyword_names.contains(&&"database".to_string()));
    
    Ok(())
}

#[test]
fn test_no_results() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "nonexistent",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should return no results
    assert_eq!(results.len(), 0);
    
    Ok(())
}

#[test]
fn test_case_insensitive_search() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "RUST",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find the Rust template (case insensitive)
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.rust.cli-subcommand");
    
    Ok(())
}

#[test]
fn test_partial_matching() -> Result<()> {
    let client = MockRegistryClient::new();
    
    let params = SearchParams {
        query: "react",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };
    
    let results = client.mock_advanced_search(&params)?;
    
    // Should find the React template
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id, "io.rgen.web.react-component");
    
    Ok(())
}
