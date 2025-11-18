//! Developer Experience (DX) utilities for lifecycle operations
//!
//! This module provides quality-of-life improvements for developers:
//! - Progress indicators with colored output
//! - Verbose logging mode
//! - Execution summaries and metrics
//! - State visualization

use super::state::LifecycleState;
use colored::*;
use std::time::{Duration, Instant};

/// Execution mode configuration
#[derive(Debug, Clone)]
pub struct ExecutionMode {
    /// Enable verbose output (shows all commands before execution)
    pub verbose: bool,
    /// Dry run mode (show what would be done without executing)
    pub dry_run: bool,
    /// Show progress indicators
    pub show_progress: bool,
    /// Use colored output
    pub use_colors: bool,
}

impl Default for ExecutionMode {
    fn default() -> Self {
        Self {
            verbose: false,
            dry_run: false,
            show_progress: true,
            use_colors: true,
        }
    }
}

impl ExecutionMode {
    /// Create execution mode for CI/CD (no colors, no progress)
    pub fn ci() -> Self {
        Self {
            verbose: false,
            dry_run: false,
            show_progress: false,
            use_colors: false,
        }
    }

    /// Create verbose mode
    pub fn verbose() -> Self {
        Self {
            verbose: true,
            ..Default::default()
        }
    }

    /// Create dry-run mode
    pub fn dry_run() -> Self {
        Self {
            dry_run: true,
            verbose: true, // Always verbose in dry-run
            ..Default::default()
        }
    }
}

/// Execution metrics tracker
#[derive(Debug, Clone)]
pub struct ExecutionMetrics {
    /// Overall start time
    start_time: Instant,
    /// Phase execution times (phase_name -> duration_ms)
    phase_times: Vec<(String, u128)>,
    /// Commands executed count
    commands_executed: usize,
    /// Hooks executed count
    hooks_executed: usize,
    /// Cache hits
    cache_hits: usize,
}

impl ExecutionMetrics {
    /// Create new metrics tracker
    pub fn new() -> Self {
        Self {
            start_time: Instant::now(),
            phase_times: Vec::new(),
            commands_executed: 0,
            hooks_executed: 0,
            cache_hits: 0,
        }
    }

    /// Record a phase execution
    pub fn record_phase(&mut self, phase: String, duration_ms: u128) {
        self.phase_times.push((phase, duration_ms));
    }

    /// Increment commands executed
    pub fn record_command(&mut self) {
        self.commands_executed += 1;
    }

    /// Increment hooks executed
    pub fn record_hook(&mut self) {
        self.hooks_executed += 1;
    }

    /// Increment cache hits
    pub fn record_cache_hit(&mut self) {
        self.cache_hits += 1;
    }

    /// Get total elapsed time
    pub fn total_elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Get summary report
    pub fn summary(&self, mode: &ExecutionMode) -> String {
        let mut report = String::new();
        let total = self.total_elapsed();

        if mode.use_colors {
            report.push_str(&format!(
                "\n{}\n",
                "═══ Execution Summary ═══".bright_cyan().bold()
            ));

            // Total time
            report.push_str(&format!(
                "  {} {}\n",
                "⏱️  Total time:".bright_white(),
                format_duration(total).bright_green()
            ));

            // Phase breakdown
            if !self.phase_times.is_empty() {
                report.push_str(&format!(
                    "\n  {} {}\n",
                    "📊 Phase timing:".bright_white(),
                    ""
                ));
                for (phase, duration_ms) in &self.phase_times {
                    let percent = (*duration_ms as f64 / total.as_millis() as f64) * 100.0;
                    report.push_str(&format!(
                        "    {} {} {} {}\n",
                        "▸".bright_blue(),
                        phase.bright_yellow(),
                        format!("{}ms", duration_ms).bright_white(),
                        format!("({}%)", percent as u32).dimmed()
                    ));
                }
            }

            // Statistics
            report.push_str(&format!("\n  {} {}\n", "📈 Statistics:".bright_white(), ""));
            report.push_str(&format!(
                "    {} Phases executed: {}\n",
                "▸".bright_blue(),
                self.phase_times.len().to_string().bright_green()
            ));
            report.push_str(&format!(
                "    {} Commands run: {}\n",
                "▸".bright_blue(),
                self.commands_executed.to_string().bright_green()
            ));
            report.push_str(&format!(
                "    {} Hooks triggered: {}\n",
                "▸".bright_blue(),
                self.hooks_executed.to_string().bright_green()
            ));
            if self.cache_hits > 0 {
                report.push_str(&format!(
                    "    {} Cache hits: {} {}\n",
                    "▸".bright_blue(),
                    self.cache_hits.to_string().bright_green(),
                    "⚡".bright_yellow()
                ));
            }

            report.push_str(&format!(
                "\n{}\n",
                "═════════════════════════".bright_cyan()
            ));
        } else {
            // Plain text for CI/CD
            report.push_str("\n=== Execution Summary ===\n");
            report.push_str(&format!("Total time: {}\n", format_duration(total)));
            report.push_str(&format!("Phases: {}\n", self.phase_times.len()));
            report.push_str(&format!("Commands: {}\n", self.commands_executed));
            report.push_str(&format!("Hooks: {}\n", self.hooks_executed));
            if self.cache_hits > 0 {
                report.push_str(&format!("Cache hits: {}\n", self.cache_hits));
            }
            report.push_str("=========================\n");
        }

        report
    }
}

impl Default for ExecutionMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Output helper for consistent messaging
pub struct Output {
    mode: ExecutionMode,
}

impl Output {
    /// Create new output helper
    pub fn new(mode: ExecutionMode) -> Self {
        Self { mode }
    }

    /// Print info message
    pub fn info(&self, msg: &str) {
        if self.mode.use_colors {
            log::info!("{} {}", "ℹ".bright_blue(), msg);
        } else {
            log::info!("[INFO] {}", msg);
        }
    }

    /// Print success message
    pub fn success(&self, msg: &str) {
        if self.mode.use_colors {
            log::info!("{} {}", "✓".bright_green(), msg.bright_green());
        } else {
            log::info!("[SUCCESS] {}", msg);
        }
    }

    /// Print warning message
    pub fn warning(&self, msg: &str) {
        if self.mode.use_colors {
            log::warn!("{} {}", "⚠".bright_yellow(), msg.yellow());
        } else {
            log::warn!("[WARNING] {}", msg);
        }
    }

    /// Print error message
    pub fn error(&self, msg: &str) {
        if self.mode.use_colors {
            log::error!("{} {}", "✗".bright_red(), msg.red());
        } else {
            log::error!("[ERROR] {}", msg);
        }
    }

    /// Print phase start
    pub fn phase_start(&self, phase: &str) {
        if self.mode.use_colors {
            log::info!(
                "\n{} {}",
                "▶".bright_cyan().bold(),
                phase.bright_cyan().bold()
            );
        } else {
            log::info!("\n[PHASE] {}", phase);
        }
    }

    /// Print phase complete
    pub fn phase_complete(&self, phase: &str, duration_ms: u128) {
        if self.mode.use_colors {
            log::info!(
                "{} {} {} {}",
                "✓".bright_green(),
                phase.bright_green(),
                "completed in".dimmed(),
                format!("{}ms", duration_ms).bright_white()
            );
        } else {
            log::info!("[COMPLETE] {} ({}ms)", phase, duration_ms);
        }
    }

    /// Print command execution (verbose only)
    pub fn command(&self, cmd: &str) {
        if self.mode.verbose {
            if self.mode.use_colors {
                log::debug!("  {} {}", "$".bright_blue(), cmd.dimmed());
            } else {
                log::debug!("  $ {}", cmd);
            }
        }
    }

    /// Print dry-run command
    pub fn dry_run(&self, cmd: &str) {
        if self.mode.use_colors {
            log::info!("  {} {}", "[DRY-RUN]".bright_magenta().bold(), cmd.dimmed());
        } else {
            log::info!("  [DRY-RUN] {}", cmd);
        }
    }

    /// Print hook execution
    pub fn hook(&self, hook_type: &str, phase: &str) {
        if self.mode.verbose {
            if self.mode.use_colors {
                log::debug!(
                    "  {} {} {}",
                    "↪".bright_yellow(),
                    hook_type.yellow(),
                    phase.dimmed()
                );
            } else {
                log::debug!("  [HOOK] {} {}", hook_type, phase);
            }
        }
    }

    /// Print cache hit
    pub fn cache_hit(&self, phase: &str) {
        if self.mode.verbose {
            if self.mode.use_colors {
                log::debug!(
                    "  {} {} {}",
                    "⚡".bright_yellow(),
                    "Cache hit for".dimmed(),
                    phase.yellow()
                );
            } else {
                log::debug!("  [CACHE] Hit for {}", phase);
            }
        }
    }

    /// Print workspace
    pub fn workspace(&self, name: &str) {
        if self.mode.use_colors {
            log::info!(
                "\n{} {}",
                "📦".bright_magenta(),
                name.bright_magenta().bold()
            );
        } else {
            log::info!("\n[WORKSPACE] {}", name);
        }
    }
}

/// State visualization helper
pub struct StateVisualizer {
    use_colors: bool,
}

impl StateVisualizer {
    /// Create new state visualizer
    pub fn new(use_colors: bool) -> Self {
        Self { use_colors }
    }

    /// Pretty-print lifecycle state
    pub fn display(&self, state: &LifecycleState) -> String {
        let mut output = String::new();

        if self.use_colors {
            output.push_str(&format!(
                "\n{}\n",
                "━━━ Lifecycle State ━━━".bright_cyan().bold()
            ));

            // Last phase
            if let Some(last) = &state.last_phase {
                output.push_str(&format!(
                    "  {} {}\n",
                    "Last phase:".bright_white(),
                    last.bright_yellow()
                ));
            } else {
                output.push_str(&format!(
                    "  {} {}\n",
                    "Last phase:".bright_white(),
                    "None".dimmed()
                ));
            }

            // Phase history
            if !state.phase_history.is_empty() {
                output.push_str(&format!(
                    "\n  {} {}\n",
                    "Recent executions:".bright_white(),
                    ""
                ));
                let recent = state.phase_history.iter().rev().take(10);
                for record in recent {
                    let status = if record.success {
                        "✓".bright_green()
                    } else {
                        "✗".bright_red()
                    };
                    output.push_str(&format!(
                        "    {} {} {} {}\n",
                        status,
                        record.phase.bright_yellow(),
                        format!("({}ms)", record.duration_ms).dimmed(),
                        format_timestamp(record.started_ms).dimmed()
                    ));
                }
                if state.phase_history.len() > 10 {
                    output.push_str(&format!(
                        "    {} {} more...\n",
                        "...".dimmed(),
                        (state.phase_history.len() - 10).to_string().dimmed()
                    ));
                }
            }

            // Cache keys
            if !state.cache_keys.is_empty() {
                output.push_str(&format!("\n  {} {}\n", "Cache keys:".bright_white(), ""));
                for key in state.cache_keys.iter().rev().take(5) {
                    output.push_str(&format!(
                        "    {} {} {}\n",
                        "▸".bright_blue(),
                        key.phase.bright_yellow(),
                        key.key[..8].dimmed()
                    ));
                }
            }

            output.push_str(&format!("\n{}\n", "━━━━━━━━━━━━━━━━━━━━━━━".bright_cyan()));
        } else {
            output.push_str("\n=== Lifecycle State ===\n");
            if let Some(last) = &state.last_phase {
                output.push_str(&format!("Last phase: {}\n", last));
            }
            output.push_str(&format!(
                "Total executions: {}\n",
                state.phase_history.len()
            ));
            output.push_str(&format!("Cache keys: {}\n", state.cache_keys.len()));
            output.push_str("=======================\n");
        }

        output
    }
}

/// Format duration in human-readable form
fn format_duration(duration: Duration) -> String {
    let millis = duration.as_millis();
    if millis < 1000 {
        format!("{}ms", millis)
    } else if millis < 60_000 {
        format!("{:.2}s", millis as f64 / 1000.0)
    } else {
        let secs = millis / 1000;
        let mins = secs / 60;
        let secs = secs % 60;
        format!("{}m {}s", mins, secs)
    }
}

/// Format timestamp in human-readable form
fn format_timestamp(timestamp_ms: u128) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};

    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0); // Fallback to epoch if clock is invalid

    let diff_ms = now_ms.saturating_sub(timestamp_ms);

    if diff_ms < 1000 {
        "just now".to_string()
    } else if diff_ms < 60_000 {
        format!("{}s ago", diff_ms / 1000)
    } else if diff_ms < 3_600_000 {
        format!("{}m ago", diff_ms / 60_000)
    } else if diff_ms < 86_400_000 {
        format!("{}h ago", diff_ms / 3_600_000)
    } else {
        format!("{}d ago", diff_ms / 86_400_000)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    test!(test_execution_mode_defaults, {
        let mode = ExecutionMode::default();
        assert!(!mode.verbose);
        assert!(!mode.dry_run);
        assert!(mode.show_progress);
        assert!(mode.use_colors);
    });

    test!(test_ci_mode, {
        let mode = ExecutionMode::ci();
        assert!(!mode.show_progress);
        assert!(!mode.use_colors);
    });

    test!(test_metrics_tracking, {
        let mut metrics = ExecutionMetrics::new();
        metrics.record_phase("build".to_string(), 1000);
        metrics.record_command();
        metrics.record_command();
        metrics.record_hook();

        assert_eq!(metrics.phase_times.len(), 1);
        assert_eq!(metrics.commands_executed, 2);
        assert_eq!(metrics.hooks_executed, 1);
    });

    test!(test_format_duration, {
        assert_eq!(format_duration(Duration::from_millis(500)), "500ms");
        assert_eq!(format_duration(Duration::from_millis(1500)), "1.50s");
        assert_eq!(format_duration(Duration::from_secs(90)), "1m 30s");
    });

    test!(test_output_modes, {
        let output = Output::new(ExecutionMode::default());
        output.info("test");
        output.success("test");
        output.warning("test");
        output.phase_start("build");
        output.command("cargo build");
    });

    test!(test_state_visualizer, {
        let state = LifecycleState::default();
        let viz = StateVisualizer::new(false);
        let display = viz.display(&state);
        assert!(display.contains("Lifecycle State"));
    });
}
