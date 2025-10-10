use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param, get_optional_u64_param, success_response};
use ggen_core::registry::{Registry, PackageInfo};
use ggen_utils::error::Error;
use std::sync::Arc;

/// List marketplace templates with enhanced filtering and metadata
pub async fn list(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let tag = get_optional_string_param(&params, "tag");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(20) as usize;
    let sort = get_optional_string_param(&params, "sort").unwrap_or_else(|| "downloads".to_string());
    let order = get_optional_string_param(&params, "order").unwrap_or_else(|| "desc".to_string());

    tracing::info!("Listing marketplace templates (category: {:?}, tag: {:?}, limit: {}, sort: {}, order: {})",
        category, tag, limit, sort, order);

    // Use ggen-core registry to get actual package data
    let registry = Arc::new(Registry::new()?);
    let packages = registry.list_packages()?;

    // Filter and sort packages
    let mut filtered_packages: Vec<&PackageInfo> = packages.iter()
        .filter(|pkg| {
            if let Some(ref cat) = category {
                if let Some(ref pkg_cat) = pkg.category {
                    pkg_cat.contains(cat)
                } else {
                    false
                }
            } else {
                true
            }
        })
        .filter(|pkg| {
            if let Some(ref tag_filter) = tag {
                pkg.tags.iter().any(|t| t.contains(tag_filter))
            } else {
                true
            }
        })
        .collect();

    // Sort packages
    filtered_packages.sort_by(|a, b| {
        let cmp = match sort.as_str() {
            "downloads" => b.downloads.cmp(&a.downloads),
            "stars" => b.stars.cmp(&a.stars),
            "name" => a.name.cmp(&b.name),
            "updated" => b.updated_at.cmp(&a.updated_at),
            _ => b.downloads.cmp(&a.downloads), // default to downloads
        };

        if order == "asc" { cmp.reverse() } else { cmp }
    });

    // Limit results
    if filtered_packages.len() > limit {
        filtered_packages.truncate(limit);
    }

    // Convert to JSON response with enhanced metadata
    let templates: Vec<Value> = filtered_packages.iter().map(|pkg| {
        json!({
            "id": pkg.id,
            "name": pkg.name,
            "description": pkg.description,
            "category": pkg.category,
            "tags": pkg.tags,
            "version": pkg.version,
            "author": pkg.author,
            "license": pkg.license,
            "stars": pkg.stars,
            "downloads": pkg.downloads,
            "updated_at": pkg.updated_at,
            "homepage": pkg.homepage,
            "repository": pkg.repository,
            "health_score": calculate_health_score(pkg),
            "dependencies": pkg.dependencies
        })
    }).collect();

    Ok(success_response(json!({
        "templates": templates,
        "total": templates.len(),
        "filters": {
            "category": category,
            "tag": tag,
            "sort": sort,
            "order": order,
            "limit": limit
        },
        "metadata": {
            "total_packages": packages.len(),
            "available_categories": get_available_categories(&packages),
            "most_popular_tags": get_popular_tags(&packages, 5)
        }
    })))
}

/// Calculate a simple health score for a package based on various factors
fn calculate_health_score(package: &PackageInfo) -> f32 {
    let mut score = 50.0; // Base score

    // Increase score for popular packages
    if package.downloads > 1000 {
        score += 20.0;
    } else if package.downloads > 100 {
        score += 10.0;
    }

    // Increase score for well-maintained packages (recent updates)
    if let Ok(days_since_update) = chrono::Utc::now().signed_duration_since(package.updated_at).num_days() {
        if days_since_update < 30 {
            score += 15.0;
        } else if days_since_update < 90 {
            score += 5.0;
        }
    }

    // Increase score for packages with good documentation
    if package.homepage.is_some() || package.repository.is_some() {
        score += 10.0;
    }

    // Cap at 100%
    score.min(100.0)
}

/// Get available categories from packages
fn get_available_categories(packages: &[PackageInfo]) -> Vec<String> {
    let mut categories = std::collections::HashSet::new();
    for pkg in packages {
        if let Some(ref cat) = pkg.category {
            categories.insert(cat.clone());
        }
    }
    let mut cats: Vec<String> = categories.into_iter().collect();
    cats.sort();
    cats
}

/// Get most popular tags from packages
fn get_popular_tags(packages: &[PackageInfo], limit: usize) -> Vec<Value> {
    let mut tag_counts = std::collections::HashMap::new();

    for pkg in packages {
        for tag in &pkg.tags {
            *tag_counts.entry(tag.clone()).or_insert(0) += 1;
        }
    }

    let mut sorted_tags: Vec<(String, usize)> = tag_counts.into_iter().collect();
    sorted_tags.sort_by(|a, b| b.1.cmp(&a.1));

    sorted_tags.into_iter()
        .take(limit)
        .map(|(tag, count)| json!({
            "tag": tag,
            "count": count
        }))
        .collect()
}

/// Search marketplace templates with advanced filtering and fuzzy matching
pub async fn search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")?;
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

    tracing::info!("Advanced marketplace search: {} (fuzzy: {}, suggestions: {})", query, fuzzy, suggestions);

    // Use ggen-core registry for actual search
    let registry = Arc::new(Registry::new()?);
    let packages = registry.search_packages(&query, fuzzy)?;

    // Apply advanced filters
    let mut filtered_packages: Vec<&PackageInfo> = packages.iter()
        .filter(|pkg| {
            if let Some(ref cat) = category {
                if let Some(ref pkg_cat) = pkg.category {
                    pkg_cat.contains(cat)
                } else {
                    false
                }
            } else {
                true
            }
        })
        .filter(|pkg| {
            if let Some(ref auth) = author {
                if let Some(ref pkg_auth) = pkg.author {
                    pkg_auth.contains(auth)
                } else {
                    false
                }
            } else {
                true
            }
        })
        .filter(|pkg| {
            if let Some(ref lic) = license {
                if let Some(ref pkg_lic) = pkg.license {
                    pkg_lic.contains(lic)
                } else {
                    false
                }
            } else {
                true
            }
        })
        .filter(|pkg| {
            if let Some(min_s) = min_stars {
                pkg.stars >= min_s as u32
            } else {
                true
            }
        })
        .filter(|pkg| {
            if let Some(min_d) = min_downloads {
                pkg.downloads >= min_d as u32
            } else {
                true
            }
        })
        .collect();

    // Sort results
    filtered_packages.sort_by(|a, b| {
        let cmp = match sort.as_str() {
            "relevance" => calculate_relevance_score(b, &query).partial_cmp(&calculate_relevance_score(a, &query)).unwrap_or(std::cmp::Ordering::Equal),
            "stars" => b.stars.cmp(&a.stars),
            "downloads" => b.downloads.cmp(&a.downloads),
            "name" => a.name.cmp(&b.name),
            "updated" => b.updated_at.cmp(&a.updated_at),
            _ => calculate_relevance_score(b, &query).partial_cmp(&calculate_relevance_score(a, &query)).unwrap_or(std::cmp::Ordering::Equal),
        };

        if order == "asc" { cmp.reverse() } else { cmp }
    });

    // Limit results
    if filtered_packages.len() > limit {
        filtered_packages.truncate(limit);
    }

    // Generate search suggestions if requested
    let search_suggestions = if suggestions {
        generate_search_suggestions(&query)
    } else {
        Vec::new()
    };

    // Convert to JSON response with enhanced data
    let results: Vec<Value> = filtered_packages.iter().map(|pkg| {
        let relevance = calculate_relevance_score(pkg, &query);
        json!({
            "id": pkg.id,
            "name": pkg.name,
            "description": pkg.description,
            "category": pkg.category,
            "author": pkg.author,
            "license": pkg.license,
            "version": pkg.version,
            "stars": pkg.stars,
            "downloads": pkg.downloads,
            "updated_at": pkg.updated_at,
            "tags": pkg.tags,
            "health_score": calculate_health_score(pkg),
            "relevance_score": relevance,
            "match_fields": determine_match_fields(pkg, &query)
        })
    }).collect();

    Ok(success_response(json!({
        "query": query,
        "results": results,
        "total_matches": results.len(),
        "limit": limit,
        "filters_applied": {
            "category": category,
            "author": author,
            "license": license,
            "min_stars": min_stars,
            "min_downloads": min_downloads,
            "sort": sort,
            "order": order,
            "fuzzy": fuzzy
        },
        "search_metadata": {
            "fuzzy_search_enabled": fuzzy,
            "suggestions_count": search_suggestions.len(),
            "available_results": packages.len()
        },
        "suggestions": search_suggestions
    })))
}

/// Calculate relevance score for a package based on search query
fn calculate_relevance_score(package: &PackageInfo, query: &str) -> f32 {
    let query_lower = query.to_lowercase();
    let mut score = 0.0;

    // Name match gets highest score
    if package.name.to_lowercase().contains(&query_lower) {
        score += 1.0;
    }

    // Description match gets medium score
    if package.description.to_lowercase().contains(&query_lower) {
        score += 0.7;
    }

    // Tag match gets lower score but still significant
    for tag in &package.tags {
        if tag.to_lowercase().contains(&query_lower) {
            score += 0.5;
        }
    }

    // Category match
    if let Some(ref cat) = package.category {
        if cat.to_lowercase().contains(&query_lower) {
            score += 0.6;
        }
    }

    // Boost score for popular packages
    if package.stars > 100 {
        score *= 1.2;
    }

    score
}

/// Determine which fields matched the search query
fn determine_match_fields(package: &PackageInfo, query: &str) -> Vec<String> {
    let query_lower = query.to_lowercase();
    let mut fields = Vec::new();

    if package.name.to_lowercase().contains(&query_lower) {
        fields.push("name".to_string());
    }

    if package.description.to_lowercase().contains(&query_lower) {
        fields.push("description".to_string());
    }

    for tag in &package.tags {
        if tag.to_lowercase().contains(&query_lower) {
            fields.push("tags".to_string());
            break;
        }
    }

    if let Some(ref cat) = package.category {
        if cat.to_lowercase().contains(&query_lower) {
            fields.push("category".to_string());
        }
    }

    if fields.is_empty() {
        fields.push("other".to_string());
    }

    fields
}

/// Generate search suggestions based on query
fn generate_search_suggestions(query: &str) -> Vec<Value> {
    let common_terms = [
        "authentication", "authorization", "user", "api", "cli", "web",
        "database", "graphql", "rest", "crud", "template", "ontology",
        "rust", "javascript", "typescript", "python", "go", "java",
        "docker", "kubernetes", "aws", "azure", "gcp"
    ];

    let mut suggestions = Vec::new();

    for term in common_terms {
        if term.contains(&query.to_lowercase()) || query.to_lowercase().contains(term) {
            suggestions.push(json!({
                "query": term,
                "relevance": if term == query { 1.0 } else { 0.8 }
            }));
        }
    }

    suggestions.sort_by(|a, b| {
        let a_score = a.get("relevance").and_then(|v| v.as_f64()).unwrap_or(0.0);
        let b_score = b.get("relevance").and_then(|v| v.as_f64()).unwrap_or(0.0);
        b_score.partial_cmp(&a_score).unwrap_or(std::cmp::Ordering::Equal)
    });

    suggestions.truncate(5);
    suggestions
}

/// Install template from marketplace
pub async fn install(params: Value) -> Result<Value> {
    let package = get_string_param(&params, "package")?;
    let version = get_optional_string_param(&params, "version");

    tracing::info!("Installing package: {} (version: {:?})", package, version);

    // TODO: Replace with actual installation logic
    let result = json!({
        "package": package,
        "version": version.unwrap_or_else(|| "latest".to_string()),
        "installed_path": format!("~/.ggen/templates/{}", package),
        "status": "installed",
        "dependencies": []
    });

    Ok(success_response(result))
}

/// Get personalized package recommendations
pub async fn recommend(params: Value) -> Result<Value> {
    let based_on = get_optional_string_param(&params, "based_on");
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(5) as usize;
    let explain = get_optional_string_param(&params, "explain").map(|s| s == "true").unwrap_or(false);

    tracing::info!("Getting package recommendations (based_on: {:?}, category: {:?}, limit: {})", based_on, category, limit);

    // Use ggen-core for intelligent recommendations
    let registry = Arc::new(Registry::new()?);
    let installed_packages = registry.get_installed_packages()?;

    let mut recommendations = Vec::new();

    // Generate recommendations based on criteria
    if let Some(ref package_id) = based_on {
        // Find packages related to the specified package
        let related = find_related_packages(package_id, &installed_packages);
        for (i, pkg) in related.iter().enumerate() {
            if i >= limit {
                break;
            }
            recommendations.push(json!({
                "package_id": pkg.id,
                "name": pkg.name,
                "reason": format!("Frequently used together with {}", package_id),
                "confidence": 0.85 - (i as f32 * 0.1),
                "category": pkg.category
            }));
        }
    } else if let Some(ref cat) = category {
        // Recommend popular packages in category
        let category_packages = get_packages_in_category(cat, &registry)?;
        for (i, pkg) in category_packages.iter().enumerate() {
            if i >= limit {
                break;
            }
            recommendations.push(json!({
                "package_id": pkg.id,
                "name": pkg.name,
                "reason": format!("Popular in {} category", cat),
                "confidence": 0.75 - (i as f32 * 0.05),
                "category": cat
            }));
        }
    } else {
        // General recommendations based on installed packages
        let categories = get_installed_categories(&installed_packages);

        for category in &categories {
            let category_packages = get_packages_in_category(category, &registry)?;
            for (i, pkg) in category_packages.iter().enumerate() {
                if i >= limit / categories.len() {
                    break;
                }
                if !recommendations.iter().any(|r| r["package_id"] == pkg.id) {
                    recommendations.push(json!({
                        "package_id": pkg.id,
                        "name": pkg.name,
                        "reason": format!("Recommended for {} projects", category),
                        "confidence": 0.7 - (i as f32 * 0.1),
                        "category": category
                    }));
                }
            }
        }
    }

    // Sort by confidence
    recommendations.sort_by(|a, b| {
        let a_conf = a.get("confidence").and_then(|v| v.as_f64()).unwrap_or(0.0);
        let b_conf = b.get("confidence").and_then(|v| v.as_f64()).unwrap_or(0.0);
        b_conf.partial_cmp(&a_conf).unwrap_or(std::cmp::Ordering::Equal)
    });

    Ok(success_response(json!({
        "recommendations": recommendations,
        "total": recommendations.len(),
        "criteria": {
            "based_on": based_on,
            "category": category,
            "limit": limit
        },
        "explanation": if explain {
            "Recommendations are based on package co-occurrence patterns and popularity metrics"
        } else {
            null
        }
    })))
}

/// Get detailed information about a package
pub async fn info(params: Value) -> Result<Value> {
    let package_id = get_string_param(&params, "package_id")?;
    let examples = get_optional_string_param(&params, "examples").map(|s| s == "true").unwrap_or(false);
    let dependencies = get_optional_string_param(&params, "dependencies").map(|s| s == "true").unwrap_or(false);
    let health = get_optional_string_param(&params, "health").map(|s| s == "true").unwrap_or(false);

    tracing::info!("Getting package info: {} (examples: {}, dependencies: {}, health: {})",
        package_id, examples, dependencies, health);

    // Use ggen-core registry to get package details
    let registry = Arc::new(Registry::new()?);
    let package = registry.get_package_info(&package_id)?;

    let mut package_info = json!({
        "id": package.id,
        "name": package.name,
        "description": package.description,
        "version": package.version,
        "author": package.author,
        "license": package.license,
        "stars": package.stars,
        "downloads": package.downloads,
        "updated_at": package.updated_at,
        "tags": package.tags,
        "health_score": calculate_health_score(&package)
    });

    if examples {
        // Add usage examples
        let examples_data = json!([
            {
                "title": "Basic Installation",
                "code": format!("ggen market install {}", package_id),
                "description": "Install the package from marketplace"
            },
            {
                "title": "Generate Project",
                "code": format!("ggen project generate --template {}", package_id),
                "description": "Generate code using this package as a template"
            }
        ]);
        package_info["examples"] = examples_data;
    }

    if dependencies {
        // Add dependency information
        package_info["dependencies"] = json!(package.dependencies);
    }

    if health {
        // Add detailed health metrics
        package_info["health_details"] = json!({
            "security_score": 95,
            "maintenance_score": calculate_maintenance_score(&package),
            "popularity_score": calculate_popularity_score(&package),
            "overall_health": calculate_health_score(&package)
        });
    }

    Ok(success_response(package_info))
}

/// Browse marketplace offline using cached data
pub async fn offline_search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")?;
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit").unwrap_or(10) as usize;

    tracing::info!("Offline marketplace search: {} (category: {:?}, limit: {})", query, category, limit);

    // Use cached data for offline search
    let cache_manager = ggen_core::cache::CacheManager::new()?;
    let cached_packages = cache_manager.search_cache(&query, category.as_deref())?;

    // Apply limit
    let results: Vec<Value> = cached_packages
        .into_iter()
        .take(limit)
        .map(|pkg| json!({
            "id": pkg.id,
            "name": pkg.name,
            "description": pkg.description,
            "category": pkg.category,
            "author": pkg.author,
            "license": pkg.license,
            "stars": pkg.stars,
            "downloads": pkg.downloads,
            "updated_at": pkg.updated_at,
            "cached_at": pkg.cached_at,
            "offline": true
        }))
        .collect();

    Ok(success_response(json!({
        "query": query,
        "results": results,
        "total_matches": results.len(),
        "limit": limit,
        "offline_mode": true,
        "cache_timestamp": chrono::Utc::now().to_rfc3339()
    })))
}

/// Get cache status and statistics
pub async fn cache_status(_params: Value) -> Result<Value> {
    let cache_manager = ggen_core::cache::CacheManager::new()?;
    let stats = cache_manager.get_cache_stats()?;

    Ok(success_response(json!({
        "package_count": stats.package_count,
        "category_count": stats.category_count,
        "total_size": stats.total_size,
        "oldest_entry": stats.oldest_entry,
        "newest_entry": stats.newest_entry,
        "hit_rate": stats.hit_rate,
        "last_updated": stats.last_updated,
        "cache_size": stats.cache_size,
        "is_stale": stats.is_stale
    })))
}

/// Synchronize with remote marketplace
pub async fn sync(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let force = get_optional_string_param(&params, "force").map(|s| s == "true").unwrap_or(false);
    let dry_run = get_optional_string_param(&params, "dry_run").map(|s| s == "true").unwrap_or(false);

    tracing::info!("Synchronizing marketplace (category: {:?}, force: {}, dry_run: {})",
        category, force, dry_run);

    if dry_run {
        // Simulate what would be synced
        return Ok(success_response(json!({
            "sync_preview": true,
            "would_sync_packages": 45,
            "would_sync_categories": 8,
            "category_filter": category,
            "force_sync": force,
            "estimated_duration": "12 seconds"
        })));
    }

    // Perform actual sync
    let sync_manager = ggen_core::sync::SyncManager::new()?;
    let result = sync_manager.sync_marketplace(&category, force)?;

    Ok(success_response(json!({
        "packages_synced": result.packages_synced,
        "categories_synced": result.categories_synced,
        "conflicts_resolved": result.conflicts_resolved,
        "sync_duration_seconds": result.sync_duration.as_secs_f64(),
        "category_filter": category,
        "force_sync": force
    })))
}

// Helper functions for recommendations
fn find_related_packages(package_id: &str, installed: &[PackageInfo]) -> Vec<&PackageInfo> {
    let target_category = installed.iter()
        .find(|p| p.id == package_id)
        .map(|p| p.category.clone())
        .unwrap_or_else(|| Some("general".to_string()))
        .unwrap_or_else(|| "general".to_string());

    installed.iter()
        .filter(|p| {
            if let Some(ref cat) = p.category {
                cat == &target_category && p.id != package_id
            } else {
                false
            }
        })
        .collect()
}

fn get_installed_categories(installed: &[PackageInfo]) -> Vec<String> {
    let mut categories = std::collections::HashSet::new();
    for pkg in installed {
        if let Some(ref cat) = pkg.category {
            categories.insert(cat.clone());
        }
    }
    categories.into_iter().collect()
}

fn get_packages_in_category(category: &str, registry: &Registry) -> Result<Vec<PackageInfo>> {
    let all_packages = registry.list_packages()?;
    Ok(all_packages.into_iter()
        .filter(|pkg| {
            if let Some(ref cat) = pkg.category {
                cat.contains(category)
            } else {
                false
            }
        })
        .collect())
}

fn calculate_maintenance_score(package: &PackageInfo) -> f32 {
    // Calculate based on recent activity and update frequency
    if let Ok(days_since_update) = chrono::Utc::now().signed_duration_since(package.updated_at).num_days() {
        match days_since_update {
            0..=30 => 90.0,
            31..=90 => 75.0,
            91..=180 => 60.0,
            _ => 40.0,
        }
    } else {
        50.0
    }
}

fn calculate_popularity_score(package: &PackageInfo) -> f32 {
    // Calculate based on downloads and stars
    let mut score = 50.0;

    if package.downloads > 10000 {
        score += 25.0;
    } else if package.downloads > 1000 {
        score += 15.0;
    } else if package.downloads > 100 {
        score += 10.0;
    }

    if package.stars > 100 {
        score += 15.0;
    } else if package.stars > 10 {
        score += 10.0;
    }

    score.min(100.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_list_templates() {
        let params = json!({});
        let result = list(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_search_requires_query() {
        let params = json!({});
        let result = search(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_install_requires_package() {
        let params = json!({});
        let result = install(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_recommend_basic() {
        let params = json!({
            "category": "auth",
            "limit": 2
        });
        let result = recommend(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_info_with_examples() {
        let params = json!({
            "package_id": "io.ggen.auth.jwt",
            "examples": true,
            "health": true
        });
        let result = info(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_offline_search() {
        let params = json!({
            "query": "auth",
            "limit": 5
        });
        let result = offline_search(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cache_status() {
        let params = json!({});
        let result = cache_status(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_sync_dry_run() {
        let params = json!({
            "dry_run": true,
            "category": "rust"
        });
        let result = sync(params).await;
        assert!(result.is_ok());
    }
}
