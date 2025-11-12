//! Marketplace Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements marketplace commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use crate::runtime_helper::execute_async_verb;
use ggen_domain::marketplace::{
    execute_install, execute_list, execute_publish, execute_search, validate_all_packages,
    validate_package, InstallInput, ListInput, PackageValidation, PublishInput, SearchInput,
};

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct SearchOutput {
    packages: Vec<PackageInfo>,
    total: usize,
}

#[derive(Serialize)]
struct PackageInfo {
    name: String,
    version: String,
    description: String,
    author: Option<String>,
    downloads: u32,
    stars: u32,
}

#[derive(Serialize)]
struct InstallOutput {
    package: String,
    version: String,
    path: String,
    dependencies: Vec<String>,
}

#[derive(Serialize)]
struct ListOutput {
    packages: Vec<InstalledPackage>,
    total: usize,
}

#[derive(Serialize)]
struct InstalledPackage {
    name: String,
    version: String,
    title: String,
    description: String,
}

#[derive(Serialize)]
struct PublishOutput {
    package: String,
    version: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    package: Option<String>,
    score: f64,
    production_ready: bool,
    total_packages: usize,
    ready_count: usize,
    needs_improvement_count: usize,
    not_ready_count: usize,
    details: Option<PackageValidation>,
    all_results: Option<Vec<PackageValidation>>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search for packages in the marketplace
#[verb]
fn search(query: String, limit: Option<usize>, category: Option<String>) -> Result<SearchOutput> {
    let input = SearchInput {
        query,
        limit: limit.unwrap_or(10),
        category,
        ..Default::default()
    };

    execute_async_verb(async move {
        let results = execute_search(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let packages = results
            .into_iter()
            .map(|p| PackageInfo {
                name: p.name,
                version: p.version,
                description: p.description,
                author: p.author,
                downloads: p.downloads,
                stars: p.stars,
            })
            .collect::<Vec<_>>();

        let total = packages.len();

        Ok(SearchOutput { packages, total })
    })
}

/// Install a package from the marketplace
#[verb]
fn install(
    package: String, target: Option<String>, force: bool, no_dependencies: bool, dry_run: bool,
) -> Result<InstallOutput> {
    let input = InstallInput {
        package: package.clone(),
        target,
        force,
        no_dependencies,
        dry_run,
    };

    execute_async_verb(async move {
        let result = execute_install(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(InstallOutput {
            package: result.package_name,
            version: result.version,
            path: result.install_path.display().to_string(),
            dependencies: result.dependencies_installed,
        })
    })
}

/// List installed packages
#[verb]
fn list(detailed: bool, json: bool) -> Result<ListOutput> {
    let input = ListInput { detailed, json };

    execute_async_verb(async move {
        let result = execute_list(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let packages = result
            .packages
            .into_iter()
            .map(|p| InstalledPackage {
                name: p.name,
                version: p.version,
                title: p.title,
                description: p.description,
            })
            .collect::<Vec<_>>();

        let total = result.packages_listed;

        Ok(ListOutput { packages, total })
    })
}

/// Publish a package to the marketplace
#[verb]
fn publish(
    path: PathBuf, tag: Option<String>, dry_run: bool, force: bool,
) -> Result<PublishOutput> {
    let input = PublishInput {
        path,
        tag,
        dry_run,
        force,
    };

    execute_async_verb(async move {
        let result = execute_publish(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(PublishOutput {
            package: result.package_name,
            version: result.version,
        })
    })
}

/// Validate package(s) for production readiness
#[verb]
fn validate(
    package: Option<String>, packages_dir: Option<PathBuf>, update: bool,
) -> Result<ValidateOutput> {
    execute_async_verb(async move {
        let packages_path = packages_dir.unwrap_or_else(|| PathBuf::from("marketplace/packages"));

        if let Some(pkg_name) = package {
            // Validate single package
            let package_path = packages_path.join(&pkg_name);
            let validation = validate_package(&package_path)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

            // Update production_ready flag if requested
            if update && validation.production_ready {
                update_package_toml_production_flag(&package_path, true)
                    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
            }

            Ok(ValidateOutput {
                package: Some(pkg_name),
                score: validation.score,
                production_ready: validation.production_ready,
                total_packages: 1,
                ready_count: if validation.production_ready { 1 } else { 0 },
                needs_improvement_count: if validation.score >= 80.0 && validation.score < 95.0 {
                    1
                } else {
                    0
                },
                not_ready_count: if validation.score < 80.0 { 1 } else { 0 },
                details: Some(validation),
                all_results: None,
            })
        } else {
            // Validate all packages
            let validations = validate_all_packages(&packages_path)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

            let ready_count = validations.iter().filter(|v| v.production_ready).count();
            let needs_improvement_count = validations
                .iter()
                .filter(|v| v.score >= 80.0 && v.score < 95.0)
                .count();
            let not_ready_count = validations.iter().filter(|v| v.score < 80.0).count();

            // Update production_ready flags if requested
            if update {
                for validation in &validations {
                    if validation.production_ready {
                        update_package_toml_production_flag(&validation.package_path, true)
                            .map_err(|e| {
                                clap_noun_verb::NounVerbError::execution_error(e.to_string())
                            })?;
                    }
                }
            }

            Ok(ValidateOutput {
                package: None,
                score: 0.0,
                production_ready: false,
                total_packages: validations.len(),
                ready_count,
                needs_improvement_count,
                not_ready_count,
                details: None,
                all_results: Some(validations),
            })
        }
    })
}

/// Update production_ready flag in package.toml
fn update_package_toml_production_flag(
    package_path: &PathBuf, production_ready: bool,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    use std::fs;
    use toml::Value;

    let package_toml_path = package_path.join("package.toml");
    if !package_toml_path.exists() {
        return Err("package.toml not found".into());
    }

    let content = fs::read_to_string(&package_toml_path)?;
    let mut toml_value: Value = content.parse()?;

    // Update or create [package.metadata] section
    if let Some(package) = toml_value.get_mut("package") {
        if let Some(metadata) = package.get_mut("metadata") {
            if let Some(metadata_table) = metadata.as_table_mut() {
                metadata_table.insert(
                    "production_ready".to_string(),
                    Value::Boolean(production_ready),
                );
            }
        } else {
            // Create metadata section
            if let Some(package_table) = package.as_table_mut() {
                let mut metadata = toml::map::Map::new();
                metadata.insert(
                    "production_ready".to_string(),
                    Value::Boolean(production_ready),
                );
                package_table.insert("metadata".to_string(), Value::Table(metadata));
            }
        }
    }

    // Write back to file
    let updated_content = toml::to_string_pretty(&toml_value)?;
    fs::write(&package_toml_path, updated_content)?;

    Ok(())
}
