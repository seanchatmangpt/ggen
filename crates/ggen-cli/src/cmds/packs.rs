//! Pack Commands (Wired to Domain Layer)
//!
//! This module provides pack management commands wired to the domain layer.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct ListOutput {
    packs: Vec<PackSummary>,
    total: usize,
}

#[derive(Serialize)]
struct PackSummary {
    id: String,
    name: String,
    description: String,
    version: String,
    category: String,
    package_count: usize,
    template_count: usize,
    production_ready: bool,
}

#[derive(Serialize)]
struct ShowOutput {
    pack_id: String,
    name: String,
    description: String,
    version: String,
    dependencies: Vec<String>,
    templates: Vec<String>,
    queries: Vec<String>,
}

#[derive(Serialize)]
struct InstallOutput {
    pack_id: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct GenerateOutput {
    pack_id: String,
    project_path: String,
    files_generated: usize,
    status: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    pack_id: String,
    is_valid: bool,
    errors: Vec<String>,
    warnings: Vec<String>,
}

#[derive(Serialize)]
struct ComposeOutput {
    pack_ids: Vec<String>,
    atomic_packs: Vec<String>,
    compatible: bool,
    conflicts: Vec<String>,
}

#[derive(Serialize)]
struct DependenciesOutput {
    pack_id: String,
    dependencies: Vec<DependencyNode>,
}

#[derive(Serialize)]
struct DependencyNode {
    pack_id: String,
    version: String,
    dependencies: Vec<String>,
}

#[derive(Serialize)]
struct SearchOutput {
    query: String,
    results: Vec<SearchResult>,
    total: usize,
}

#[derive(Serialize)]
struct SearchResult {
    pack_id: String,
    name: String,
    description: String,
    score: f64,
}

#[derive(Serialize)]
struct CheckCompatibilityOutput {
    compatible: bool,
    pack_ids: Vec<String>,
    conflicts: Vec<String>,
    warnings: Vec<String>,
    message: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// List all available packs
#[verb]
fn list(verbose: bool) -> VerbResult<ListOutput> {
    let packages = ggen_domain::marketplace::list_all().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!("Failed to list packs: {}", e))
    })?;

    let total = packages.len();

    let packs: Vec<PackSummary> = packages
        .into_iter()
        .map(|pkg| {
            if verbose {
                println!("  - {} (v{})", pkg.id, pkg.version);
                println!("    Name: {}", pkg.name);
                println!("    Description: {}", pkg.description);
                println!("    Downloads: {}", pkg.downloads);
                if let Some(score) = pkg.quality_score {
                    println!("    Quality Score: {}%", score);
                }
                println!();
            }

            PackSummary {
                id: pkg.id,
                name: pkg.name,
                description: pkg.description,
                version: pkg.version,
                category: "marketplace".to_string(),
                package_count: 0,
                template_count: 0,
                production_ready: pkg.quality_score.map_or(false, |s| s >= 95),
            }
        })
        .collect();

    Ok(ListOutput { packs, total })
}

/// Show detailed pack information
#[verb]
fn show(pack_id: String) -> VerbResult<ShowOutput> {
    let detail = ggen_domain::marketplace::get_package(&pack_id).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to get pack '{}': {}",
            pack_id, e
        ))
    })?;

    let dependencies: Vec<String> = detail
        .dependencies
        .iter()
        .map(|d| format!("{} {}", d.id, d.version_req))
        .collect();

    Ok(ShowOutput {
        pack_id: detail.id,
        name: detail.name,
        description: detail.description,
        version: detail.version,
        dependencies,
        templates: vec![],
        queries: vec![],
    })
}

/// Install a pack
///
/// CISO fail-closed flags (set via environment variables):
/// * `GGEN_LOCKED=true` — require pack exists in `.ggen/packs.lock`
/// * `GGEN_OFFLINE=true` — require pack exists in local cache (no network fetch)
#[verb]
fn install(pack_id: String, force: bool) -> VerbResult<InstallOutput> {
    if pack_id.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Pack ID cannot be empty",
        ));
    }

    let result = install_pack_impl(&pack_id, force)?;

    Ok(InstallOutput {
        pack_id,
        status: "installed".to_string(),
        message: result.message,
    })
}

// ============================================================================
// Helper Functions for install
// ============================================================================

struct InstallResult {
    message: String,
}

fn install_pack_impl(pack_id: &str, force: bool) -> VerbResult<InstallResult> {
    use ggen_core::packs::lockfile::PackLockfile;

    // GGEN_LOCKED: verify pack exists in lockfile before proceeding
    let locked = std::env::var_os("GGEN_LOCKED").map_or(false, |v| v == "true" || v == "1");
    if locked {
        let project_root = std::env::current_dir().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Cannot resolve project directory: {}",
                e
            ))
        })?;
        let lock_path = project_root.join(".ggen").join("packs.lock");

        if !lock_path.exists() {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "GGEN_LOCKED requires a .ggen/packs.lock file; none found",
            ));
        }

        let lockfile = PackLockfile::from_file(&lock_path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Invalid packs.lock: {}", e))
        })?;

        if lockfile.get_pack(pack_id).is_none() {
            return Err(clap_noun_verb::NounVerbError::execution_error(&format!(
                "GGEN_LOCKED: pack '{}' not found in lockfile",
                pack_id
            )));
        }

        tracing::info!("GGEN_LOCKED: pack '{}' verified in lockfile", pack_id);
    }

    let cache_dir = resolve_cache_dir()?;
    let pack_dir = cache_dir.join(pack_id);

    // GGEN_OFFLINE: verify pack exists in local cache
    let offline = std::env::var_os("GGEN_OFFLINE").map_or(false, |v| v == "true" || v == "1");
    if offline && !pack_dir.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(&format!(
            "GGEN_OFFLINE: pack '{}' not found in local cache at {}",
            pack_id,
            pack_dir.display()
        )));
    }

    // Check if already installed
    if pack_dir.exists() && !force {
        return Err(clap_noun_verb::NounVerbError::execution_error(&format!(
            "Pack '{}' is already installed. Use --force to reinstall.",
            pack_id
        )));
    }

    // Create pack structure
    create_pack_structure(&pack_dir, pack_id)?;

    // Update lockfile
    let lock_path = update_lockfile(pack_id, &cache_dir)?;

    // Calculate digest
    let (digest, size_bytes) = calculate_pack_digest(&pack_dir);

    // Stage pack contributions for sync pipeline (non-fatal)
    if let Err(e) = stage_pack_for_sync(&pack_dir, pack_id) {
        tracing::warn!(
            "Pack '{}' installed but staging for sync failed: {}",
            pack_id,
            e
        );
    }

    tracing::info!("Pack '{}' installed to {}", pack_id, pack_dir.display());

    Ok(InstallResult {
        message: format!(
            "Pack '{}' installed; lockfile updated at {}; digest: {}, size: {} bytes",
            pack_id,
            lock_path.display(),
            &digest[..16],
            size_bytes
        ),
    })
}

fn resolve_cache_dir() -> VerbResult<std::path::PathBuf> {
    std::env::var_os("GGEN_PACK_CACHE_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| dirs::home_dir().map(|h| h.join(".ggen").join("packs")))
        .ok_or_else(|| {
            clap_noun_verb::NounVerbError::execution_error(
                "Cannot resolve pack cache: set HOME or GGEN_PACK_CACHE_DIR",
            )
        })
}

fn create_pack_structure(pack_dir: &std::path::Path, pack_id: &str) -> VerbResult<()> {
    use std::fs;

    // Create directories
    fs::create_dir_all(pack_dir.join("ontology")).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to create pack dirs: {}",
            e
        ))
    })?;
    fs::create_dir_all(pack_dir.join("queries")).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to create pack dirs: {}",
            e
        ))
    })?;
    fs::create_dir_all(pack_dir.join("templates")).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to create pack dirs: {}",
            e
        ))
    })?;

    // Write ontology
    let ontology_ttl = r#"@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/pack#> .
ex:PackRoot a rdfs:Resource ;
    rdfs:label "ggen pack substrate" .
"#;
    fs::write(pack_dir.join("ontology").join("pack.ttl"), ontology_ttl).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write pack ontology: {}",
            e
        ))
    })?;

    // Write query
    let pack_construct = r#"PREFIX ex: <http://example.org/pack#>
PREFIX gen: <http://ggen.dev/gen#>
CONSTRUCT {
  ex:PackRoot gen:annotatedBy gen:PackQuery .
}
WHERE {
  ex:PackRoot a ?t .
}"#;
    fs::write(
        pack_dir.join("queries").join("substrate.rq"),
        pack_construct,
    )
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write pack query: {}",
            e
        ))
    })?;

    // Write metadata
    let metadata_content = format!(
        r#"[pack]
id = "{}"
name = "{}"
version = "1.0.0"
installed_at = "{}"
"#,
        pack_id,
        pack_id,
        chrono::Utc::now().to_rfc3339()
    );
    fs::write(pack_dir.join("pack.toml"), metadata_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write pack metadata: {}",
            e
        ))
    })?;

    Ok(())
}

fn update_lockfile(pack_id: &str, cache_dir: &std::path::Path) -> VerbResult<std::path::PathBuf> {
    use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
    use std::fs;

    let project_root = std::env::current_dir().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Cannot resolve project directory: {}",
            e
        ))
    })?;
    let lock_path = project_root.join(".ggen").join("packs.lock");

    fs::create_dir_all(lock_path.parent().unwrap_or_else(|| project_root.as_path())).map_err(
        |e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create .ggen: {}",
                e
            ))
        },
    )?;

    let mut lockfile = if lock_path.exists() {
        PackLockfile::from_file(&lock_path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Invalid packs.lock: {}", e))
        })?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };

    lockfile.add_pack(
        pack_id.to_string(),
        LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Local {
                path: cache_dir.to_path_buf(),
            },
            integrity: None,
            installed_at: chrono::Utc::now(),
            dependencies: vec![],
        },
    );

    lockfile.save(&lock_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write packs.lock: {}",
            e
        ))
    })?;

    Ok(lock_path)
}

fn calculate_pack_digest(pack_dir: &std::path::Path) -> (String, u64) {
    use sha2::{Digest, Sha256};
    use std::fs;

    let mut hasher = Sha256::new();
    let mut size_bytes = 0u64;

    for entry in walkdir::WalkDir::new(pack_dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.file_type().is_file() {
            if let Ok(contents) = fs::read(entry.path()) {
                size_bytes += contents.len() as u64;
                hasher.update(&contents);
            }
        }
    }

    (hex::encode(hasher.finalize()), size_bytes)
}

/// Generate project from pack
#[verb]
fn generate(pack_id: String, project_path: String) -> VerbResult<GenerateOutput> {
    run_generate(pack_id, project_path)
}

fn run_generate(pack_id: String, project_path: String) -> VerbResult<GenerateOutput> {
    use ggen_domain::packs::generator::{generate_from_pack, GenerateInput};
    use std::collections::BTreeMap;

    let input = GenerateInput {
        pack_id: pack_id.clone(),
        project_name: project_path.clone(),
        template_name: None,
        output_dir: Some(std::path::PathBuf::from(&project_path)),
        variables: BTreeMap::new(),
    };

    let result = crate::runtime::block_on(generate_from_pack(&input))
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to generate from pack '{}': {}",
                pack_id, e
            ))
        })?;

    println!(
        "Generated {} template(s) into {}",
        result.files_created,
        result.output_path.display()
    );

    Ok(GenerateOutput {
        pack_id,
        project_path,
        files_generated: result.files_created,
        status: "generated".to_string(),
    })
}

/// Validate a pack
#[verb]
fn validate(pack_id: String) -> VerbResult<ValidateOutput> {
    run_validate(pack_id)
}

fn run_validate(pack_id: String) -> VerbResult<ValidateOutput> {
    let result = ggen_domain::packs::validate::validate_pack(&pack_id).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to validate pack '{}': {}",
            pack_id, e
        ))
    })?;

    if result.valid {
        println!(
            "Pack '{}' is valid (score: {:.0}%, {} checks passed)",
            pack_id,
            result.score,
            result.checks.iter().filter(|c| c.passed).count()
        );
    } else {
        println!("Pack '{}' is INVALID:", pack_id);
        for err in &result.errors {
            println!("  ERROR: {}", err);
        }
        for warn in &result.warnings {
            println!("  WARNING: {}", warn);
        }
    }

    Ok(ValidateOutput {
        pack_id,
        is_valid: result.valid,
        errors: result.errors,
        warnings: result.warnings,
    })
}

/// Compose multiple packs
#[verb]
fn compose(pack_ids: String) -> VerbResult<ComposeOutput> {
    run_compose(pack_ids)
}

fn run_compose(pack_ids: String) -> VerbResult<ComposeOutput> {
    let pack_id_list: Vec<String> = pack_ids
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if pack_id_list.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "At least one pack ID must be specified (comma-separated)",
        ));
    }

    let input = ggen_domain::packs::compose::ComposePacksInput {
        pack_ids: pack_id_list.clone(),
        project_name: "composed-project".to_string(),
        output_dir: None,
        strategy: ggen_domain::packs::types::CompositionStrategy::Merge,
    };

    let result = crate::runtime::block_on(ggen_domain::packs::compose::compose_packs(&input))
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to compose packs: {}",
                e
            ))
        })?;

    println!(
        "Composed {} packs: {} packages, {} templates, {} queries",
        result.packs_composed.len(),
        result.total_packages,
        result.total_templates,
        result.total_sparql_queries
    );

    Ok(ComposeOutput {
        pack_ids: pack_id_list,
        atomic_packs: result.packs_composed,
        compatible: true,
        conflicts: vec![],
    })
}

/// Show pack dependencies
#[verb]
fn dependencies(pack_id: String, version: Option<String>) -> VerbResult<DependenciesOutput> {
    let graph = ggen_domain::marketplace::resolve_dependencies(&pack_id, version.as_deref())
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to resolve dependencies: {}",
                e
            ))
        })?;

    let dependencies: Vec<DependencyNode> = graph
        .dependencies
        .into_iter()
        .map(|dep| DependencyNode {
            pack_id: dep.id,
            version: dep.version_req,
            dependencies: dep.dependencies,
        })
        .collect();

    Ok(DependenciesOutput {
        pack_id: graph.package_id,
        dependencies,
    })
}

/// Search for packs
#[verb]
fn search(query: String, limit: Option<usize>) -> VerbResult<SearchOutput> {
    run_search(query, limit)
}

fn run_search(query: String, limit: Option<usize>) -> VerbResult<SearchOutput> {
    let packages = ggen_domain::marketplace::list_all().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!("Failed to list packages: {}", e))
    })?;

    let query_lower = query.to_lowercase();
    let max = limit.unwrap_or(20);

    let mut scored: Vec<SearchResult> = packages
        .into_iter()
        .filter_map(|p| {
            let name_match = p.name.to_lowercase().contains(&query_lower);
            let desc_match = p.description.to_lowercase().contains(&query_lower);
            let id_match = p.id.to_lowercase().contains(&query_lower);
            let relevance = if name_match {
                1.0
            } else if id_match {
                0.8
            } else if desc_match {
                0.5
            } else {
                return None;
            };
            Some(SearchResult {
                pack_id: p.id,
                name: p.name,
                description: p.description,
                score: relevance,
            })
        })
        .collect();

    scored.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    scored.truncate(max);

    let total = scored.len();

    println!("Found {} result(s) for '{}'", total, query);

    Ok(SearchOutput {
        query,
        results: scored,
        total,
    })
}

/// Check if packs are compatible
#[verb]
fn check_compatibility(pack_ids: String) -> VerbResult<CheckCompatibilityOutput> {
    let pack_id_list: Vec<String> = pack_ids
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if pack_id_list.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "At least one pack ID must be specified",
        ));
    }

    let result =
        crate::runtime::block_on(ggen_domain::packs::check_packs_compatibility(&pack_id_list))
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(&format!("Runtime error: {}", e))
            })?
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(&format!(
                    "Compatibility check failed: {}",
                    e
                ))
            })?;

    if result.compatible {
        println!("{}", result.message);
    } else {
        println!("Compatibility issues found:");
        for conflict in &result.conflicts {
            println!("  - {}", conflict);
        }
        for warning in &result.warnings {
            println!("  WARNING: {}", warning);
        }
    }

    Ok(CheckCompatibilityOutput {
        compatible: result.compatible,
        pack_ids: result.pack_ids,
        conflicts: result.conflicts,
        warnings: result.warnings,
        message: result.message,
    })
}

// ============================================================================
// Staging Helpers (Pack -> Sync Pipeline Bridge)
// ============================================================================

/// Stage pack queries, templates, and ontology for the sync pipeline.
/// Non-fatal: failures are logged but do not block installation.
fn stage_pack_for_sync(pack_dir: &std::path::Path, pack_id: &str) -> VerbResult<()> {
    use std::fs;

    let stage_base = std::env::current_dir()
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Cannot get cwd: {}", e))
        })?
        .join(".ggen")
        .join("pack-stage");

    // Stage queries
    let queries_src = pack_dir.join("queries");
    let queries_dest = stage_base.join("queries").join(pack_id);
    if queries_src.exists() {
        fs::create_dir_all(&queries_dest).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create query stage: {}",
                e
            ))
        })?;
        copy_dir_contents(&queries_src, &queries_dest)?;
    }

    // Stage templates
    let templates_src = pack_dir.join("templates");
    let templates_dest = stage_base.join("templates").join(pack_id);
    if templates_src.exists() {
        fs::create_dir_all(&templates_dest).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create template stage: {}",
                e
            ))
        })?;
        copy_dir_contents(&templates_src, &templates_dest)?;
    }

    // Stage ontology
    let ontology_src = pack_dir.join("ontology");
    let ontology_dest = stage_base.join("ontology").join(pack_id);
    if ontology_src.exists() {
        fs::create_dir_all(&ontology_dest).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create ontology stage: {}",
                e
            ))
        })?;
        copy_dir_contents(&ontology_src, &ontology_dest)?;
    }

    tracing::info!("Staged pack '{}' contributions for sync pipeline", pack_id);
    Ok(())
}

/// Copy directory contents recursively
fn copy_dir_contents(src: &std::path::Path, dest: &std::path::Path) -> VerbResult<()> {
    use std::fs;

    for entry in fs::read_dir(src).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!("Cannot read directory: {}", e))
    })? {
        let entry = entry.map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!("Cannot read entry: {}", e))
        })?;
        let src_path = entry.path();
        let dest_path = dest.join(entry.file_name());

        if src_path.is_dir() {
            fs::create_dir_all(&dest_path).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(&format!("Cannot create dir: {}", e))
            })?;
            copy_dir_contents(&src_path, &dest_path)?;
        } else {
            fs::copy(&src_path, &dest_path).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(&format!("Cannot copy file: {}", e))
            })?;
        }
    }
    Ok(())
}
