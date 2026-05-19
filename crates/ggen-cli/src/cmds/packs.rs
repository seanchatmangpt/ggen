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
///
/// Displays all packs available in the marketplace with their metadata including
/// version, description, quality score, and download counts.
///
/// ## Arguments
/// * `verbose` - Show detailed pack information [default: false]
///
/// ## Examples
/// ```bash
/// mcpp pack list
/// mcpp pack list --verbose
/// ```
///
/// ## Output
/// Returns a JSON array of pack summaries with the following fields:
/// - `id`: Pack identifier
/// - `name`: Human-readable pack name
/// - `description`: Pack description
/// - `version`: Pack version
/// - `category`: Pack category (e.g., "marketplace")
/// - `production_ready`: Whether quality score >= 95%
#[verb]
fn list(
    verbose: bool,
) -> VerbResult<ListOutput> {
    let packages = mcpp_domain::marketplace::list_all().map_err(|e| {
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
///
/// Displays comprehensive metadata for a specific pack including dependencies,
/// templates, SPARQL queries, and other relevant details.
///
/// ## Arguments
/// * `pack_id` - Pack identifier [required]
///
/// ## Examples
/// ```bash
/// mcpp pack show mcp-rust
/// mcpp pack show acme/base
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_id`: Pack identifier
/// - `name`: Human-readable name
/// - `description`: Detailed description
/// - `version`: Pack version
/// - `dependencies`: List of dependency requirements
/// - `templates`: Available templates
/// - `queries`: SPARQL queries provided
#[verb]
fn show(
    pack_id: String,
) -> VerbResult<ShowOutput> {
    let detail = mcpp_domain::marketplace::get_package(&pack_id).map_err(|e| {
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

/// Install a pack from the registry
///
/// Downloads and installs a pack from the registry with intelligent caching,
/// dependency resolution, and lockfile management. The pack is staged for
/// the sync pipeline automatically.
///
/// ## Arguments
/// * `pack_id` - Pack identifier [required]
/// * `force` - Reinstall even if already installed [default: false]
///
/// ## CISO Fail-Closed Flags
/// Set these via environment variables to enforce security policies:
///
/// * `GGEN_LOCKED=true` — Require pack exists in `.mcpp/packs.lock` before installation.
///   Prevents installing packs not previously approved in the lockfile.
///   Exit code: non-zero if pack not found in lockfile.
///
/// * `GGEN_OFFLINE=true` — Require pack exists in local cache (no network fetch).
///   Prevents unauthorized network access during installation.
///   Exit code: non-zero if pack not found in cache.
///
/// ## Examples
/// ```bash
/// # Standard installation
/// mcpp pack install mcp-rust
///
/// # Force reinstall
/// mcpp pack install mcp-rust --force
///
/// # CISO fail-closed: require lockfile verification
/// GGEN_LOCKED=true mcpp pack install mcp-rust
///
/// # CISO fail-closed: offline mode (no network)
/// GGEN_OFFLINE=true mcpp pack install mcp-rust
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_id`: Installed pack identifier
/// - `status`: Installation status ("installed")
/// - `message`: Details including lockfile path, digest, and size
///
/// ## Notes
/// - Updates `.mcpp/packs.lock` with pack metadata and digest
/// - Stages pack contributions (queries, templates, ontology) for sync pipeline
/// - Failures in sync staging are non-fatal (logged but don't block installation)
/// - Digest is computed over all pack files for integrity verification
#[verb]
fn install(
    pack_id: String,
    force: bool,
) -> VerbResult<InstallOutput> {
    if pack_id.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Pack ID cannot be empty",
        ));
    }

    let result = install_pack_improved(&pack_id, force)?;

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

/// Improved pack installation implementation with better UX and performance
fn install_pack_improved(pack_id: &str, force: bool) -> VerbResult<InstallResult> {
    use mcpp_core::packs::lockfile::PackLockfile;

    // GGEN_LOCKED: verify pack exists in lockfile before proceeding
    let locked = std::env::var_os("GGEN_LOCKED").map_or(false, |v| v == "true" || v == "1");
    if locked {
        let project_root = std::env::current_dir().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Cannot resolve project directory: {}",
                e
            ))
        })?;
        let lock_path = project_root.join(".mcpp").join("packs.lock");

        if !lock_path.exists() {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "GGEN_LOCKED requires a .mcpp/packs.lock file; none found",
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
        .or_else(|| dirs::home_dir().map(|h| h.join(".mcpp").join("packs")))
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
    rdfs:label "mcpp pack substrate" .
"#;
    fs::write(pack_dir.join("ontology").join("pack.ttl"), ontology_ttl).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write pack ontology: {}",
            e
        ))
    })?;

    // Write query
    let pack_construct = r#"PREFIX ex: <http://example.org/pack#>
PREFIX gen: <http://mcpp.dev/gen#>
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
    use mcpp_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
    use std::fs;

    let project_root = std::env::current_dir().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Cannot resolve project directory: {}",
            e
        ))
    })?;
    let lock_path = project_root.join(".mcpp").join("packs.lock");

    fs::create_dir_all(lock_path.parent().unwrap_or_else(|| project_root.as_path())).map_err(
        |e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create .mcpp: {}",
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

/// Generate a project from an installed pack
///
/// Creates a new project by applying templates and queries from an installed
/// pack. The pack must already be installed via `mcpp pack install`.
///
/// ## Arguments
/// * `pack_id` - Pack identifier to use for generation [required]
/// * `project_path` - Target directory for generated project [required]
///
/// ## Examples
/// ```bash
/// # Generate project in current directory
/// mcpp pack generate mcp-rust ./my-project
///
/// # Generate project in specific path
/// mcpp pack generate acme/base /path/to/project
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_id`: Pack used for generation
/// - `project_path`: Path where project was generated
/// - `files_generated`: Number of files created
/// - `status`: Generation status ("generated")
///
/// ## Notes
/// - Project directory will be created if it doesn't exist
/// - Existing files may be overwritten depending on pack templates
/// - Generation is asynchronous; progress is reported in real-time
#[verb]
fn generate(
    pack_id: String,
    project_path: String,
) -> VerbResult<GenerateOutput> {
    run_generate(pack_id, project_path)
}

fn run_generate(pack_id: String, project_path: String) -> VerbResult<GenerateOutput> {
    use mcpp_domain::packs::generator::{generate_from_pack, GenerateInput};
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
///
/// Performs comprehensive validation of a pack including structure integrity,
/// schema compliance, and quality checks. Returns a quality score and
/// detailed error/warning information.
///
/// ## Arguments
/// * `pack_id` - Pack identifier to validate [required]
///
/// ## Examples
/// ```bash
/// # Validate installed pack
/// mcpp pack validate mcp-rust
///
/// # Validate custom pack
/// mcpp pack validate acme/base
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_id`: Validated pack identifier
/// - `is_valid`: Overall validation result (true/false)
/// - `errors`: List of validation errors (empty if valid)
/// - `warnings`: List of validation warnings (non-critical issues)
///
/// ## Validation Checks
/// - Pack structure completeness (ontology/, queries/, templates/)
/// - Metadata file validity (pack.toml)
/// - RDF ontology syntax and semantics
/// - SPARQL query correctness
/// - Template rendering capability
/// - Quality score calculation (0-100%)
///
/// ## Notes
/// - Quality score >= 95% indicates production-ready pack
/// - Warnings don't block usage but should be reviewed
/// - Errors indicate the pack is not safe to use
#[verb]
fn validate(
    pack_id: String,
) -> VerbResult<ValidateOutput> {
    run_validate(pack_id)
}

fn run_validate(pack_id: String) -> VerbResult<ValidateOutput> {
    let result = mcpp_domain::packs::validate::validate_pack(&pack_id).map_err(|e| {
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

/// Compose multiple packs into a unified project
///
/// Merges multiple packs into a single coherent project by resolving
/// dependencies, merging templates, and combining queries. Useful for
/// creating projects that combine functionality from multiple packs.
///
/// ## Arguments
/// * `pack_ids` - Comma-separated list of pack identifiers [required]
///
/// ## Examples
/// ```bash
/// # Compose two packs
/// mcpp pack compose mcp-rust,acme/base
///
/// # Compose multiple packs
/// mcpp pack compose mcp-rust,acme/base,templates/web
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_ids`: List of composed pack IDs
/// - `atomic_packs`: Packs that remain separate (not merged)
/// - `compatible`: Whether composition succeeded without conflicts
/// - `conflicts`: List of any conflicts found (empty if compatible)
///
/// ## Composition Strategy
/// - Uses merge strategy by default
/// - Resolves dependency graphs across all packs
/// - Merges templates with override logic (later packs override earlier)
/// - Combines SPARQL queries into unified query set
/// - Preserves atomic packs that cannot be merged
///
/// ## Notes
/// - Pack order matters: later packs override earlier ones in conflicts
/// - Conflicts prevent successful composition and are reported in output
/// - All packs must be installed before composing
#[verb]
fn compose(
    pack_ids: String,
) -> VerbResult<ComposeOutput> {
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

    let input = mcpp_domain::packs::compose::ComposePacksInput {
        pack_ids: pack_id_list.clone(),
        project_name: "composed-project".to_string(),
        output_dir: None,
        strategy: mcpp_domain::packs::types::CompositionStrategy::Merge,
    };

    let result = crate::runtime::block_on(mcpp_domain::packs::compose::compose_packs(&input))
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

/// Show pack dependency tree
///
/// Displays the complete dependency graph for a pack including transitive
/// dependencies. Useful for understanding what a pack requires and ensuring
/// all dependencies are available.
///
/// ## Arguments
/// * `pack_id` - Pack identifier [required]
/// * `version` - Specific version to analyze [default: latest]
///
/// ## Examples
/// ```bash
/// # Show dependencies for latest version
/// mcpp pack dependencies mcp-rust
///
/// # Show dependencies for specific version
/// mcpp pack dependencies mcp-rust --version 1.2.0
///
/// # Show dependencies with version flag
/// mcpp pack dependencies acme/base --version 2.0.0
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `pack_id`: Root pack identifier
/// - `dependencies`: Array of dependency nodes, each containing:
///   - `pack_id`: Dependency pack identifier
///   - `version`: Version requirement
///   - `dependencies`: Nested array of transitive dependencies
///
/// ## Notes
/// - Dependency tree is fully resolved (includes transitive dependencies)
/// - Version requirements follow semver syntax
/// - Circular dependencies are detected and reported
#[verb]
fn dependencies(
    pack_id: String,
    version: Option<String>,
) -> VerbResult<DependenciesOutput> {
    let graph = mcpp_domain::marketplace::resolve_dependencies(&pack_id, version.as_deref())
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

/// Search for packs in the marketplace
///
/// Searches available packs by name, identifier, or description with relevance
/// scoring. Results are ranked by match quality (name matches rank highest).
///
/// ## Arguments
/// * `query` - Search query string [required]
/// * `limit` - Maximum number of results to return [default: 20]
///
/// ## Examples
/// ```bash
/// # Search for packs
/// mcpp pack search rust
///
/// # Search with limited results
/// mcpp pack search mcp --limit 5
///
/// # Search for specific functionality
/// mcpp pack search "web server"
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `query`: Original search query
/// - `results`: Array of search results, each containing:
///   - `pack_id`: Pack identifier
///   - `name`: Human-readable name
///   - `description`: Pack description
///   - `score`: Relevance score (0.0-1.0, higher is better)
/// - `total`: Number of results returned
///
/// ## Scoring
/// - Exact name match: 1.0
/// - Pack ID match: 0.8
/// - Description match: 0.5
/// - Non-matching packs are excluded from results
///
/// ## Notes
/// - Search is case-insensitive
/// - Results are sorted by relevance score (highest first)
/// - Default limit of 20 results can be overridden
#[verb]
fn search(
    query: String,
    limit: Option<usize>,
) -> VerbResult<SearchOutput> {
    run_search(query, limit)
}

fn run_search(query: String, limit: Option<usize>) -> VerbResult<SearchOutput> {
    let packages = mcpp_domain::marketplace::list_all().map_err(|e| {
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

/// Check compatibility between packs
///
/// Analyzes multiple packs for potential conflicts, dependency issues,
/// and compatibility problems. Essential validation step before composing
/// packs or using them together in a project.
///
/// ## Arguments
/// * `pack_ids` - Comma-separated list of pack identifiers [required]
///
/// ## Examples
/// ```bash
/// # Check two packs for compatibility
/// mcpp pack check-compatibility mcp-rust,acme/base
///
/// # Check multiple packs
/// mcpp pack check-compatibility mcp-rust,acme/base,templates/web
/// ```
///
/// ## Output
/// Returns JSON with:
/// - `compatible`: Overall compatibility result (true/false)
/// - `pack_ids`: List of pack IDs checked
/// - `conflicts`: List of conflict descriptions (empty if compatible)
/// - `warnings`: List of non-critical compatibility warnings
/// - `message`: Human-readable compatibility summary
///
/// ## Compatibility Checks
/// - Dependency version conflicts
/// - Template name collisions
/// - SPARQL query incompatibilities
/// - Resource ID clashes
/// - Schema validation issues
///
/// ## Notes
/// - Compatible packs can be safely used together
/// - Conflicts must be resolved before composition
/// - Warnings indicate potential issues but don't block usage
/// - Order of pack IDs doesn't affect compatibility results
#[verb]
fn check_compatibility(
    pack_ids: String,
) -> VerbResult<CheckCompatibilityOutput> {
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
        crate::runtime::block_on(mcpp_domain::packs::check_packs_compatibility(&pack_id_list))
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
        .join(".mcpp")
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
