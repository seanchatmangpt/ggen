//! Enhanced Watch Mode - Integration with FileWatcher, IncrementalCache, and graceful shutdown
//!
//! This module provides the enhanced watch mode implementation for the executor,
//! integrating all watch mode components for production use.

use super::{
    collect_watch_paths, install_shutdown_handler, FileWatcher, IncrementalCache, SyncExecutor,
    SyncOptions, SyncResult, WatchCacheIntegration, WatchEventKind,
};
use crate::manifest::ManifestParser;
use colored::Colorize;
use ggen_utils::error::{Error, Result};
use std::path::Path;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

/// Enhanced watch mode executor with cache integration and graceful shutdown
pub struct EnhancedWatchMode {
    options: SyncOptions,
    start_time: Instant,
}

impl EnhancedWatchMode {
    /// Create new enhanced watch mode instance
    pub fn new(options: SyncOptions) -> Self {
        Self {
            options,
            start_time: Instant::now(),
        }
    }

    /// Execute watch mode with all enhancements
    pub fn execute(&self, manifest_path: &Path) -> Result<SyncResult> {
        // Install SIGINT handler for graceful shutdown
        let shutdown = install_shutdown_handler()?;

        // Parse manifest to get watch paths
        let manifest_data = ManifestParser::parse(manifest_path).map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest parse error\n  --> {}\n  |\n  = error: {}\n  = help: Check ggen.toml syntax",
                manifest_path.display(),
                e
            ))
        })?;

        let base_path = manifest_path.parent().unwrap_or(Path::new("."));
        let watch_paths = collect_watch_paths(manifest_path, &manifest_data, base_path);

        // Display startup banner
        self.print_startup_banner(&watch_paths, base_path);

        // Initial sync
        let initial_result = self.run_initial_sync()?;

        // Initialize cache if enabled
        let mut cache = if self.options.use_cache {
            let cache_dir = self
                .options
                .cache_dir
                .clone()
                .unwrap_or_else(|| base_path.join(".ggen/cache"));
            Some(IncrementalCache::new(cache_dir))
        } else {
            None
        };

        // Start file watcher
        let watcher = FileWatcher::new(watch_paths.clone()).with_debounce_ms(500);
        let rx = watcher.start()?;

        eprintln!("{}\n", "Waiting for file changes...".dimmed());

        let mut regeneration_count = 0;
        let mut total_regeneration_time = 0u64;
        let mut cache_hits = 0;

        // Watch loop
        loop {
            // Check for shutdown signal
            if shutdown.load(Ordering::Relaxed) {
                return self.shutdown_gracefully(
                    regeneration_count,
                    total_regeneration_time,
                    cache_hits,
                );
            }

            match FileWatcher::wait_for_change(&rx, Duration::from_millis(500)) {
                Ok(Some(event)) => {
                    regeneration_count += 1;

                    // Display change detection
                    self.print_change_detected(&event, base_path);

                    // Analyze cache impact if enabled
                    if let Some(ref cache) = cache {
                        if let Ok(analysis) =
                            WatchCacheIntegration::detect_affected_rules(&manifest_data, base_path, cache)
                        {
                            if !analysis.rerun_all {
                                cache_hits += 1;
                                eprintln!(
                                    "  {} Incremental: {}/{} rules affected",
                                    "⚡".yellow(),
                                    analysis.affected_rule_count,
                                    analysis.affected_rule_count + analysis.unaffected_rule_count
                                );
                            }
                        }
                    }

                    // Re-run sync with timing
                    let (result, duration_ms) = self.run_regeneration()?;
                    total_regeneration_time += duration_ms;

                    self.print_regeneration_result(&result, duration_ms);
                }
                Ok(None) => {
                    // Timeout - continue watching
                }
                Err(e) => {
                    eprintln!("{} {}", "✗".red(), format!("Watch error: {}", e).red());
                    return Err(Error::new(&format!("Watch error: {}", e)));
                }
            }
        }
    }

    fn print_startup_banner(&self, watch_paths: &[std::path::PathBuf], base_path: &Path) {
        eprintln!("{}", "\n👀 Watch Mode Started".bold().cyan());
        eprintln!("{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        eprintln!("Monitoring {} path(s) for changes:", watch_paths.len());
        for (i, path) in watch_paths.iter().enumerate() {
            let display_path = path.strip_prefix(base_path).unwrap_or(path);
            eprintln!(
                "  {}. {}",
                i + 1,
                display_path.display().to_string().bright_white()
            );
        }
        eprintln!(
            "\n{} {} {}",
            "Press".dimmed(),
            "Ctrl+C".bold().yellow(),
            "to stop watching.".dimmed()
        );
        eprintln!();
    }

    fn run_initial_sync(&self) -> Result<SyncResult> {
        eprintln!("{}", "[Initial Sync] Running...".bright_blue());

        let executor = SyncExecutor::new(SyncOptions {
            watch: false,
            ..self.options.clone()
        });

        match executor.execute() {
            Ok(initial_result) => {
                let duration_s = initial_result.duration_ms as f64 / 1000.0;
                eprintln!(
                    "{} {} files in {:.2}s\n",
                    "[Initial Sync]".bright_blue(),
                    format!("✓ Synced {}", initial_result.files_synced).green(),
                    duration_s
                );

                if self.options.use_cache {
                    eprintln!("{} Incremental cache enabled\n", "ℹ".blue());
                }

                Ok(initial_result)
            }
            Err(e) => {
                eprintln!(
                    "{} {}\n",
                    "[Initial Sync]".bright_red(),
                    format!("✗ Error: {}", e).red()
                );
                Err(e)
            }
        }
    }

    fn print_change_detected(
        &self,
        event: &super::WatchEvent,
        base_path: &Path,
    ) {
        let display_path = event.path.strip_prefix(base_path).unwrap_or(&event.path);
        let event_kind_str = match event.kind {
            WatchEventKind::Created => "created".green(),
            WatchEventKind::Modified => "modified".yellow(),
            WatchEventKind::Removed => "removed".red(),
            WatchEventKind::Renamed => "renamed".blue(),
        };

        eprintln!(
            "{} {} {}",
            "🔄".cyan(),
            format!("Detected change in {}", display_path.display()).bright_white(),
            format!("({})", event_kind_str)
        );
    }

    fn run_regeneration(&self) -> Result<(SyncResult, u64)> {
        let regen_start = Instant::now();
        let executor = SyncExecutor::new(SyncOptions {
            watch: false,
            ..self.options.clone()
        });

        match executor.execute() {
            Ok(result) => {
                let duration_ms = regen_start.elapsed().as_millis() as u64;
                Ok((result, duration_ms))
            }
            Err(e) => {
                eprintln!(
                    "{} {}",
                    "✗".red(),
                    format!("Regeneration failed: {}", e).red()
                );
                eprintln!("{}\n", "  Fix errors and save to retry...".dimmed());
                Err(e)
            }
        }
    }

    fn print_regeneration_result(&self, result: &SyncResult, duration_ms: u64) {
        let duration_s = duration_ms as f64 / 1000.0;

        eprintln!(
            "{} {} files in {:.2}s",
            "✓".green(),
            format!("Regenerated {}", result.files_synced).green(),
            duration_s
        );

        // Show cache performance if enabled
        if self.options.use_cache && result.files_synced > 0 {
            eprintln!("  {} Using incremental cache", "⚡".yellow());
        }

        eprintln!("{}\n", "Watching for more changes...".dimmed());
    }

    fn shutdown_gracefully(
        &self,
        regeneration_count: usize,
        total_regeneration_time: u64,
        cache_hits: usize,
    ) -> Result<SyncResult> {
        eprintln!("\n{}", "Shutting down gracefully...".yellow());
        eprintln!("\n{}", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());
        eprintln!("{}", "Watch Mode Summary".bold().cyan());
        eprintln!("  Total regenerations: {}", regeneration_count);

        if regeneration_count > 0 {
            let avg_time =
                total_regeneration_time as f64 / regeneration_count as f64 / 1000.0;
            eprintln!("  Average time: {:.2}s", avg_time);
        }

        if cache_hits > 0 {
            let cache_hit_rate = (cache_hits as f64 / regeneration_count as f64) * 100.0;
            eprintln!(
                "  Cache hit rate: {:.1}% ({}/{})",
                cache_hit_rate, cache_hits, regeneration_count
            );
        }

        eprintln!("{}\n", "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".cyan());

        Ok(SyncResult {
            status: "shutdown".to_string(),
            files_synced: 0,
            duration_ms: self.start_time.elapsed().as_millis() as u64,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enhanced_watch_mode_creation() {
        let options = SyncOptions::default();
        let watch_mode = EnhancedWatchMode::new(options);
        assert!(watch_mode.start_time.elapsed().as_millis() < 100);
    }

    #[test]
    fn test_enhanced_watch_mode_with_cache() {
        let options = SyncOptions {
            use_cache: true,
            ..Default::default()
        };
        let watch_mode = EnhancedWatchMode::new(options);
        assert!(watch_mode.options.use_cache);
    }
}
