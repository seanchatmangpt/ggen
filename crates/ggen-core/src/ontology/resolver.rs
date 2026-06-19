use std::path::{Path, PathBuf};
use walkdir::WalkDir;

pub struct OntologyResolver;

impl OntologyResolver {
    /// Resolves an ontology source string (which may be a registry URI or a local path)
    /// into a list of absolute or relative `PathBuf`s that contain the Turtle files.
    pub fn resolve(source: &Path, base_path: &Path) -> Vec<PathBuf> {
        let source_str = source.to_string_lossy();
        if source_str.starts_with("registry://") {
            let crate_ref = source_str.trim_start_matches("registry://");
            let crate_name = crate_ref.split('@').next().unwrap_or(crate_ref);
            let home_dir = std::env::var("HOME").unwrap_or_else(|_| "".to_string());
            let local_registry = PathBuf::from(home_dir)
                .join("ggen")
                .join("ontology_catalogue")
                .join(crate_name);
            
            let mut paths = Vec::new();
            if local_registry.exists() && local_registry.is_dir() {
                for entry in WalkDir::new(&local_registry) {
                    if let Ok(entry) = entry {
                        if entry.path().extension().and_then(|e| e.to_str()) == Some("ttl") {
                            paths.push(entry.path().to_path_buf());
                        }
                    }
                }
            }
            // Sort to ensure determinism
            paths.sort();
            paths
        } else {
            vec![base_path.join(source)]
        }
    }
}
