use crate::codegen::{SyncExecutor, SyncOptions};
use crate::manifest::ManifestParser;
use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tokio::time::sleep;

pub struct WatchConfig {
    pub debounce_ms: u64,
    pub check_interval_ms: u64,
    pub max_retries: usize,
}

impl Default for WatchConfig {
    fn default() -> Self {
        Self {
            debounce_ms: 500,
            check_interval_ms: 1000,
            max_retries: 3,
        }
    }
}

pub struct WatchMode {
    options: SyncOptions,
    config: WatchConfig,
    watched_paths: Arc<RwLock<Vec<PathBuf>>>,
}

impl WatchMode {
    pub fn new(options: SyncOptions, config: WatchConfig) -> Self {
        let watched_paths = Arc::new(RwLock::new(vec![
            options.manifest_path.clone(),
        ]));

        Self {
            options,
            config,
            watched_paths,
        }
    }

    pub async fn start(&mut self) -> Result<()> {
        let base_path = self
            .options
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."));

        // Load initial manifest
        let mut manifest_data = ManifestParser::parse(&self.options.manifest_path)
            .map_err(|e| Error::new(&format!("Failed to parse manifest: {}", e)))?;

        // Add ontology to watched paths
        let ontology_path = base_path.join(&manifest_data.ontology.source);
        self.watched_paths.write().await.push(ontology_path);

        eprintln!("Watch mode started. Press Ctrl+C to exit.");
        eprintln!("Watching {} files for changes...", self.watched_paths.read().await.len());

        // Store initial file hashes
        let mut file_hashes = self.compute_file_hashes().await?;

        loop {
            sleep(Duration::from_millis(self.config.check_interval_ms)).await;

            // Compute current hashes
            let current_hashes = match self.compute_file_hashes().await {
                Ok(h) => h,
                Err(_) => continue,
            };

            // Check for changes
            let mut changed = false;
            for (path, new_hash) in &current_hashes {
                if let Some(old_hash) = file_hashes.get(path) {
                    if old_hash != new_hash {
                        eprintln!("Changed: {}", path.display());
                        changed = true;
                    }
                }
            }

            // Check for new files
            if current_hashes.len() != file_hashes.len() {
                changed = true;
            }

            if changed {
                eprintln!("Debouncing changes for {}ms...", self.config.debounce_ms);
                sleep(Duration::from_millis(self.config.debounce_ms)).await;

                // Reload manifest to detect new files
                if let Ok(new_manifest) = ManifestParser::parse(&self.options.manifest_path) {
                    manifest_data = new_manifest;

                    // Update watched paths
                    self.watched_paths.write().await.clear();
                    self.watched_paths.write().await.push(self.options.manifest_path.clone());

                    let ontology_path = base_path.join(&manifest_data.ontology.source);
                    self.watched_paths.write().await.push(ontology_path);
                }

                // Trigger sync
                eprintln!("Triggering sync...");
                let mut retry_count = 0;
                loop {
                    let executor = SyncExecutor::new(self.options.clone());
                    match executor.execute() {
                        Ok(result) => {
                            eprintln!(
                                "✓ Sync complete: {} files in {}ms",
                                result.files_synced, result.duration_ms
                            );
                            break;
                        }
                        Err(e) => {
                            retry_count += 1;
                            if retry_count >= self.config.max_retries {
                                eprintln!("✗ Sync failed after {} retries: {}", self.config.max_retries, e);
                                break;
                            }
                            eprintln!(
                                "⚠ Sync failed (retry {}/{}): {}",
                                retry_count, self.config.max_retries, e
                            );
                            sleep(Duration::from_millis(self.config.debounce_ms)).await;
                        }
                    }
                }

                file_hashes = self.compute_file_hashes().await?;
                eprintln!("Watching for more changes...");
            }
        }
    }

    async fn compute_file_hashes(&self) -> Result<std::collections::HashMap<PathBuf, String>> {
        let mut hashes = std::collections::HashMap::new();

        for path in self.watched_paths.read().await.iter() {
            if path.exists() {
                match std::fs::read_to_string(path) {
                    Ok(content) => {
                        let hash = Self::hash_file(&content);
                        hashes.insert(path.clone(), hash);
                    }
                    Err(_) => {
                        // Skip files that can't be read
                    }
                }
            }
        }

        Ok(hashes)
    }

    fn hash_file(content: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_watch_config_defaults() {
        let config = WatchConfig::default();
        assert_eq!(config.debounce_ms, 500);
        assert_eq!(config.check_interval_ms, 1000);
        assert_eq!(config.max_retries, 3);
    }

    #[tokio::test]
    async fn test_watch_mode_creation() {
        let options = SyncOptions::default();
        let config = WatchConfig::default();
        let watch = WatchMode::new(options.clone(), config);

        let paths = watch.watched_paths.read().await;
        assert!(paths.contains(&options.manifest_path));
    }

    #[test]
    fn test_hash_file_consistency() {
        let content1 = "test content";
        let content2 = "test content";
        let content3 = "different content";

        assert_eq!(WatchMode::hash_file(content1), WatchMode::hash_file(content2));
        assert_ne!(WatchMode::hash_file(content1), WatchMode::hash_file(content3));
    }
}
