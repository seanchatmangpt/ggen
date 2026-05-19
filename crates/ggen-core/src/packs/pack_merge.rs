//! Pack merge — merge installed pack contributions into pipeline template/query sets.
//!
//! This is where packs actually affect the sync pipeline. For each installed pack,
//! this module loads templates, queries, and SPARQL files from the pack's install
//! directory and provides them to the pipeline.
//!
//! This module lives in ggen-core (not ggen-domain) to avoid circular dependencies.
//! It accepts simple `PackInstallInfo` structs instead of domain types.

use crate::utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Minimal pack info needed for merging — designed to avoid circular deps.
/// Callers in ggen-domain can construct this from their richer types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackInstallInfo {
    pub id: String,
    pub install_path: PathBuf,
}

/// Merged pack contributions ready for pipeline consumption.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackContributions {
    /// Additional templates (filename -> template_content)
    pub templates: HashMap<String, String>,
    /// Additional SPARQL queries (filename -> query_content)
    pub sparql_queries: HashMap<String, String>,
    /// Additional .rq queries (filename -> query_content)
    pub queries: HashMap<String, String>,
    /// Template directory paths to register with Tera
    pub template_dirs: Vec<PathBuf>,
    /// Query file paths for SPARQL execution
    pub query_paths: Vec<PathBuf>,
    /// Total number of contributions
    pub total_contributions: usize,
}

impl Default for PackContributions {
    fn default() -> Self {
        Self {
            templates: HashMap::new(),
            sparql_queries: HashMap::new(),
            queries: HashMap::new(),
            template_dirs: vec![],
            query_paths: vec![],
            total_contributions: 0,
        }
    }
}

/// Merge pack contributions from all installed packs.
///
/// Scans each pack's install directory for:
/// - `templates/*.tera` — Tera templates
/// - `sparql/*.rq` — SPARQL queries
/// - `queries/*.rq` — Additional queries
pub fn merge_pack_contributions(
    installed_packs: &[PackInstallInfo],
) -> Result<PackContributions> {
    let mut contributions = PackContributions::default();

    for pack in installed_packs {
        let pack_dir = &pack.install_path;

        // Merge templates
        let templates_dir = pack_dir.join("templates");
        if templates_dir.exists() {
            contributions.template_dirs.push(templates_dir.clone());
            merge_files_into(&templates_dir, ".tera", &mut contributions.templates)?;
        }

        // Merge SPARQL queries
        let sparql_dir = pack_dir.join("sparql");
        if sparql_dir.exists() {
            contributions.query_paths.push(sparql_dir.clone());
            merge_files_into(&sparql_dir, ".rq", &mut contributions.sparql_queries)?;
        }

        // Merge regular queries
        let queries_dir = pack_dir.join("queries");
        if queries_dir.exists() {
            contributions.query_paths.push(queries_dir.clone());
            merge_files_into(&queries_dir, ".rq", &mut contributions.queries)?;
        }
    }

    contributions.total_contributions = contributions.templates.len()
        + contributions.sparql_queries.len()
        + contributions.queries.len();

    Ok(contributions)
}

/// Merge files from a directory into a map, keyed by filename.
fn merge_files_into(
    dir: &Path,
    extension: &str,
    map: &mut HashMap<String, String>,
) -> Result<()> {
    if !dir.exists() {
        return Ok(());
    }

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) == Some(extension) {
            let key = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("")
                .to_string();

            let content = std::fs::read_to_string(&path)?;
            map.insert(key, content);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn make_pack_info(id: &str, base_dir: &Path) -> PackInstallInfo {
        let pack_dir = base_dir.join("packs").join(id);
        std::fs::create_dir_all(pack_dir.join("templates")).unwrap();
        std::fs::create_dir_all(pack_dir.join("sparql")).unwrap();
        std::fs::create_dir_all(pack_dir.join("queries")).unwrap();

        std::fs::write(pack_dir.join("templates/hello.rs.tera"), "hello {{ name }}").unwrap();
        std::fs::write(pack_dir.join("sparql/construct.rq"), "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").unwrap();
        std::fs::write(pack_dir.join("queries/select.rq"), "SELECT ?x WHERE { ?x a ?type }").unwrap();

        PackInstallInfo {
            id: id.to_string(),
            install_path: pack_dir,
        }
    }

    #[test]
    fn test_merge_single_pack() {
        let tmp = TempDir::new().unwrap();
        let packs = vec![make_pack_info("mcp-rust", tmp.path())];

        let contributions = merge_pack_contributions(&packs).unwrap();

        assert_eq!(contributions.templates.len(), 1);
        assert_eq!(contributions.sparql_queries.len(), 1);
        assert_eq!(contributions.queries.len(), 1);
        assert_eq!(contributions.total_contributions, 3);
        assert!(contributions.templates.contains_key("hello.rs.tera"));
    }

    #[test]
    fn test_merge_multiple_packs() {
        let tmp = TempDir::new().unwrap();
        let packs = vec![
            make_pack_info("mcp-rust", tmp.path()),
            make_pack_info("a2a-rust", tmp.path()),
        ];

        let contributions = merge_pack_contributions(&packs).unwrap();

        assert_eq!(contributions.template_dirs.len(), 2);
        assert_eq!(contributions.query_paths.len(), 4);
    }

    #[test]
    fn test_merge_empty_packs() {
        let contributions = merge_pack_contributions(&[]).unwrap();
        assert_eq!(contributions.total_contributions, 0);
    }
}
