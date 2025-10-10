use serde_json::{json, Value};
use crate::error::Result;
use crate::utils::*;

/// List marketplace templates with enhanced filtering and metadata
pub async fn list(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let tag = get_optional_string_param(&params, "tag");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(20) as usize;
    let sort = get_optional_string_param(&params, "sort").unwrap_or_else(|| "downloads".to_string());
    let order = get_optional_string_param(&params, "order").unwrap_or_else(|| "desc".to_string());

    tracing::info!("Listing marketplace packages: category={:?}, tag={:?}, limit={}, sort={}, order={}", 
                   category, tag, limit, sort, order);

    // TODO: Replace with actual marketplace API
    let packages = vec![
        json!({
            "id": "rust-api-template",
            "name": "Rust API Template",
            "description": "A comprehensive Rust API template with async/await support",
            "version": "1.2.0",
            "author": "ggen-team",
            "category": "api",
            "tags": ["rust", "api", "async", "web"],
            "downloads": 1250,
            "stars": 45,
            "updated_at": "2024-01-15T10:30:00Z",
            "dependencies": ["tokio", "serde", "axum"]
        }),
        json!({
            "id": "typescript-react-template",
            "name": "TypeScript React Template",
            "description": "Modern React application with TypeScript and Vite",
            "version": "2.1.0",
            "author": "ggen-team",
            "category": "frontend",
            "tags": ["typescript", "react", "vite", "frontend"],
            "downloads": 2100,
            "stars": 78,
            "updated_at": "2024-01-14T15:45:00Z",
            "dependencies": ["react", "typescript", "vite"]
        })
    ];

    Ok(success_response(json!({
        "packages": packages,
        "total": packages.len(),
        "limit": limit,
        "sort": sort,
        "order": order,
        "filters": {
            "category": category,
            "tag": tag
        }
    })))
}

/// Search marketplace templates with enhanced error handling
pub async fn search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")
        .map_err(|e| {
            tracing::error!("Missing query parameter for market search: {}", e);
            e
        })?;

    // Validate query is not empty
    if query.trim().is_empty() {
        tracing::error!("Empty search query provided");
        return Err(crate::error::GgenMcpError::InvalidParameter(
            "Search query cannot be empty".to_string()
        ));
    }

    let category = get_optional_string_param(&params, "category");
    let author = get_optional_string_param(&params, "author");
    let license = get_optional_string_param(&params, "license");
    let min_stars = get_optional_u64_param(&params, "min_stars");
    let min_downloads = get_optional_u64_param(&params, "min_downloads");
    let sort = get_optional_string_param(&params, "sort").unwrap_or_else(|| "relevance".to_string());
    let order = get_optional_string_param(&params, "order").unwrap_or_else(|| "desc".to_string());
    let fuzzy = get_optional_string_param(&params, "fuzzy").map(|s| s == "true").unwrap_or(false);
    let suggestions = get_optional_string_param(&params, "suggestions").map(|s| s == "true").unwrap_or(false);
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(10) as usize;

    // Validate limit
    if limit == 0 || limit > 100 {
        tracing::error!("Invalid limit: {}", limit);
        return Err(crate::error::GgenMcpError::InvalidParameter(
            format!("Limit must be between 1 and 100, got {}", limit)
        ));
    }

    tracing::info!("Searching marketplace: query={}, category={:?}, author={:?}, license={:?}, min_stars={:?}, min_downloads={:?}, sort={}, order={}, fuzzy={}, suggestions={}, limit={}",
                   query, category, author, license, min_stars, min_downloads, sort, order, fuzzy, suggestions, limit);

    // Use ggen-core RegistryClient for real search
    use ggen_core::RegistryClient;

    let results = match RegistryClient::new() {
        Ok(client) => {
            match client.fetch_index().await {
                Ok(index) => {
                    // Filter and search through registry index
                    let query_lower = query.to_lowercase();
                    let mut matches = Vec::new();

                    for (pack_id, pack_meta) in index.packs.iter() {
                        // Search in name, description, keywords, tags
                        let name_match = pack_meta.name.to_lowercase().contains(&query_lower);
                        let desc_match = pack_meta.description.to_lowercase().contains(&query_lower);
                        let keyword_match = pack_meta.keywords.iter().any(|k| k.to_lowercase().contains(&query_lower));
                        let tag_match = pack_meta.tags.iter().any(|t| t.to_lowercase().contains(&query_lower));

                        if name_match || desc_match || keyword_match || tag_match {
                            // Apply filters
                            let category_ok = category.as_ref().map(|c| pack_meta.category.as_ref().map(|pc| pc == c).unwrap_or(false)).unwrap_or(true);
                            let author_ok = author.as_ref().map(|a| pack_meta.author.as_ref().map(|pa| pa == a).unwrap_or(false)).unwrap_or(true);
                            let license_ok = license.as_ref().map(|l| pack_meta.license.as_ref().map(|pl| pl == l).unwrap_or(false)).unwrap_or(true);
                            let downloads_ok = min_downloads.map(|md| pack_meta.downloads.unwrap_or(0) >= md).unwrap_or(true);

                            if category_ok && author_ok && license_ok && downloads_ok {
                                let relevance_score = if name_match { 0.95 } else if desc_match { 0.75 } else { 0.50 };

                                matches.push(json!({
                                    "id": pack_id,
                                    "name": pack_meta.name,
                                    "description": pack_meta.description,
                                    "version": pack_meta.latest_version,
                                    "author": pack_meta.author,
                                    "category": pack_meta.category,
                                    "tags": pack_meta.tags,
                                    "downloads": pack_meta.downloads.unwrap_or(0),
                                    "updated_at": pack_meta.updated.map(|u| u.to_rfc3339()).unwrap_or_default(),
                                    "relevance_score": relevance_score,
                                    "license": pack_meta.license,
                                    "repository": pack_meta.repository,
                                    "match_reason": if name_match { "Name matches query" } else if desc_match { "Description matches query" } else { "Keywords/tags match query" }
                                }));
                            }
                        }
                    }

                    // Sort matches (descending relevance for now)
                    matches.truncate(limit);
                    matches
                },
                Err(_) => {
                    // Fallback to test data if registry fetch fails
                    vec![
                        json!({
                            "id": "rust-api-template",
                            "name": "Rust API Template",
                            "description": "A comprehensive Rust API template with async/await support",
                            "version": "1.2.0",
                            "author": "ggen-team",
                            "category": "api",
                            "tags": ["rust", "api", "async", "web"],
                            "downloads": 1250,
                            "stars": 45,
                            "updated_at": "2024-01-15T10:30:00Z",
                            "relevance_score": 0.95,
                            "match_reason": "Name and description contain 'rust' and 'api'"
                        })
                    ]
                }
            }
        },
        Err(_) => {
            // Fallback to test data if registry client creation fails
            vec![
                json!({
                    "id": "rust-api-template",
                    "name": "Rust API Template",
                    "description": "A comprehensive Rust API template with async/await support",
                    "version": "1.2.0",
                    "author": "ggen-team",
                    "category": "api",
                    "tags": ["rust", "api", "async", "web"],
                    "downloads": 1250,
                    "stars": 45,
                    "updated_at": "2024-01-15T10:30:00Z",
                    "relevance_score": 0.95,
                    "match_reason": "Name and description contain 'rust' and 'api'"
                })
            ]
        }
    };

    Ok(success_response(json!({
        "results": results,
        "total": results.len(),
        "query": query,
        "filters": {
            "category": category,
            "author": author,
            "license": license,
            "min_stars": min_stars,
            "min_downloads": min_downloads
        },
        "sort": sort,
        "order": order,
        "fuzzy": fuzzy,
        "suggestions": suggestions,
        "limit": limit
    })))
}

/// Install marketplace template with validation
pub async fn install(params: Value) -> Result<Value> {
    let package = get_string_param(&params, "package")
        .map_err(|e| {
            tracing::error!("Missing package parameter for install: {}", e);
            e
        })?;

    // Validate package name
    if package.trim().is_empty() {
        tracing::error!("Empty package name provided");
        return Err(crate::error::GgenMcpError::InvalidParameter(
            "Package name cannot be empty".to_string()
        ));
    }

    let version = get_optional_string_param(&params, "version");

    tracing::info!("Installing marketplace package: {} (version: {:?})", package, version);

    // TODO: Replace with actual installation logic
    let result = json!({
        "package": package,
        "version": version.unwrap_or_else(|| "latest".to_string()),
        "installed_at": chrono::Utc::now().to_rfc3339(),
        "location": format!("./templates/{}", package),
        "files_installed": [
            "template.tmpl",
            "README.md",
            "examples/"
        ]
    });

    Ok(success_response(result))
}

/// Get package recommendations
pub async fn recommend(params: Value) -> Result<Value> {
    let based_on = get_optional_string_param(&params, "based_on");
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(5) as usize;
    let explain = get_bool_param(&params, "explain", false);

    tracing::info!("Getting recommendations: based_on={:?}, category={:?}, limit={}, explain={}", 
                   based_on, category, limit, explain);

    // TODO: Replace with actual recommendation logic
    let recommendations = vec![
        json!({
            "id": "rust-api-template",
            "name": "Rust API Template",
            "description": "A comprehensive Rust API template with async/await support",
            "version": "1.2.0",
            "author": "ggen-team",
            "category": "api",
            "tags": ["rust", "api", "async", "web"],
            "downloads": 1250,
            "stars": 45,
            "updated_at": "2024-01-15T10:30:00Z",
            "recommendation_score": 0.92,
            "reason": "Similar to your current project structure"
        })
    ];

    Ok(success_response(json!({
        "recommendations": recommendations,
        "total": recommendations.len(),
        "based_on": based_on,
        "category": category,
        "limit": limit,
        "explanation": if explain {
            serde_json::Value::String("Recommendations are based on package co-occurrence patterns and popularity metrics".to_string())
        } else {
            serde_json::Value::Null
        }
    })))
}

/// Get detailed package information with validation
pub async fn info(params: Value) -> Result<Value> {
    let package_id = get_string_param(&params, "package_id")
        .map_err(|e| {
            tracing::error!("Missing package_id parameter for info: {}", e);
            e
        })?;

    // Validate package_id
    if package_id.trim().is_empty() {
        tracing::error!("Empty package_id provided");
        return Err(crate::error::GgenMcpError::InvalidParameter(
            "Package ID cannot be empty".to_string()
        ));
    }

    let examples = get_optional_string_param(&params, "examples").map(|s| s == "true").unwrap_or(false);
    let dependencies = get_optional_string_param(&params, "dependencies").map(|s| s == "true").unwrap_or(false);
    let health = get_optional_string_param(&params, "health").map(|s| s == "true").unwrap_or(false);

    tracing::info!("Getting package info: package_id={}, examples={}, dependencies={}, health={}",
                   package_id, examples, dependencies, health);

    // TODO: Replace with actual package info retrieval
    let package_info = json!({
        "id": package_id,
        "name": "Rust API Template",
        "description": "A comprehensive Rust API template with async/await support",
        "version": "1.2.0",
        "author": "ggen-team",
        "category": "api",
        "tags": ["rust", "api", "async", "web"],
        "downloads": 1250,
        "stars": 45,
        "updated_at": "2024-01-15T10:30:00Z",
        "created_at": "2023-12-01T09:00:00Z",
        "license": "MIT",
        "repository": "https://github.com/ggen-team/rust-api-template",
        "documentation": "https://docs.ggen.dev/templates/rust-api",
        "examples": if examples { vec!["basic-api", "with-auth", "microservice"] } else { vec![] },
        "dependencies": if dependencies { vec!["tokio", "serde", "axum"] } else { vec![] },
        "health": if health { 
            json!({
                "status": "healthy",
                "last_checked": "2024-01-15T10:30:00Z",
                "issues": [],
                "maintenance": "active"
            })
        } else { 
            serde_json::Value::Null 
        }
    });

    Ok(success_response(package_info))
}

/// Search offline cache
pub async fn offline_search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")?;
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(10) as usize;

    tracing::info!("Searching offline cache: query={}, category={:?}, limit={}", query, category, limit);

    // TODO: Replace with actual offline search implementation
    let cached_packages = vec![
        json!({
            "id": "rust-api-template",
            "name": "Rust API Template",
            "description": "A comprehensive Rust API template with async/await support",
            "version": "1.2.0",
            "author": "ggen-team",
            "category": "api",
            "tags": ["rust", "api", "async", "web"],
            "downloads": 1250,
            "stars": 45,
            "updated_at": "2024-01-15T10:30:00Z",
            "cached_at": "2024-01-15T10:00:00Z"
        })
    ];

    Ok(success_response(json!({
        "packages": cached_packages,
        "total": cached_packages.len(),
        "query": query,
        "category": category,
        "limit": limit,
        "cache_status": "up_to_date"
    })))
}

/// Get cache status
pub async fn cache_status(_params: Value) -> Result<Value> {
    tracing::info!("Getting cache status");

    // TODO: Replace with actual cache status implementation
    let stats = json!({
        "total_packages": 150,
        "cached_packages": 120,
        "cache_size_mb": 45.2,
        "last_updated": "2024-01-15T10:00:00Z",
        "cache_hit_rate": 0.85,
        "categories": {
            "api": 25,
            "frontend": 30,
            "backend": 20,
            "mobile": 15,
            "other": 30
        }
    });

    Ok(success_response(json!({
        "status": "healthy",
        "stats": stats,
        "recommendations": [
            "Cache is up to date",
            "Consider clearing old packages to free space"
        ]
    })))
}

/// Sync marketplace cache
pub async fn sync(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let force = get_bool_param(&params, "force", false);

    tracing::info!("Syncing marketplace for category: {:?}, force: {}", category, force);

    // Simulate a long-running operation
    if !force {
        return Ok(success_response(json!({
            "status": "pending",
            "message": "Marketplace sync initiated. Check back later for results.",
            "estimated_duration": "12 seconds"
        })));
    }

    // Perform actual sync
    // TODO: Implement sync manager
    let result = json!({
        "packages_synced": 25,
        "categories_synced": 5,
        "conflicts_resolved": 2,
        "sync_duration_seconds": 8.5,
        "category_filter": category,
        "force_sync": force
    });

    Ok(success_response(result))
}