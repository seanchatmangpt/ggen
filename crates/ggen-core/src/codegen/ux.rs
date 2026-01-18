//! UX Utilities - Progress indicators and user feedback
//!
//! This module provides user experience enhancements for the CLI:
//! - Progress spinners and bars
//! - Colored output formatting
//! - Confirmation prompts
//! - User-friendly messages

use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use std::io::{self, Write};
use std::time::Duration;

/// Progress indicator for long-running operations
pub struct ProgressIndicator {
    spinner: Option<ProgressBar>,
    enabled: bool,
}

impl ProgressIndicator {
    /// Create a new progress indicator
    pub fn new(enabled: bool) -> Self {
        Self {
            spinner: None,
            enabled,
        }
    }

    /// Start a spinner with a message
    pub fn start_spinner(&mut self, message: &str) {
        if !self.enabled {
            return;
        }

        let spinner = ProgressBar::new_spinner();
        spinner.set_style(
            ProgressStyle::default_spinner()
                .tick_chars("⠁⠂⠄⡀⢀⠠⠐⠈ ")
                .template("{spinner:.cyan} {msg}")
                .unwrap_or_else(|_| ProgressStyle::default_spinner()),
        );
        spinner.set_message(message.to_string());
        spinner.enable_steady_tick(Duration::from_millis(80));
        self.spinner = Some(spinner);
    }

    /// Update the spinner message
    pub fn update_message(&self, message: &str) {
        if let Some(ref spinner) = self.spinner {
            spinner.set_message(message.to_string());
        }
    }

    /// Finish the spinner with a success message
    pub fn finish_with_message(&mut self, message: &str) {
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_with_message(format!("{} {}", "✓".green(), message));
        }
    }

    /// Finish the spinner with an error message
    pub fn finish_with_error(&mut self, message: &str) {
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_with_message(format!("{} {}", "✗".red(), message));
        }
    }

    /// Clear the spinner
    pub fn clear(&mut self) {
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_and_clear();
        }
    }
}

/// Progress bar for file generation
pub struct FileProgressBar {
    bar: Option<ProgressBar>,
    #[allow(dead_code)]
    enabled: bool,
}

impl FileProgressBar {
    /// Create a new progress bar for file generation
    pub fn new(total: usize, enabled: bool) -> Self {
        if !enabled || total == 0 {
            return Self {
                bar: None,
                enabled: false,
            };
        }

        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} {msg}")
                .unwrap_or_else(|_| ProgressStyle::default_bar())
                .progress_chars("#>-"),
        );
        Self {
            bar: Some(bar),
            enabled: true,
        }
    }

    /// Increment the progress bar
    pub fn inc(&self, delta: u64) {
        if let Some(ref bar) = self.bar {
            bar.inc(delta);
        }
    }

    /// Set a message on the progress bar
    pub fn set_message(&self, message: &str) {
        if let Some(ref bar) = self.bar {
            bar.set_message(message.to_string());
        }
    }

    /// Finish the progress bar
    pub fn finish(&mut self) {
        if let Some(bar) = self.bar.take() {
            bar.finish_and_clear();
        }
    }
}

/// Format a success message
pub fn success_message(message: &str) -> String {
    format!("{} {}", "✓".green().bold(), message)
}

/// Format an error message
pub fn error_message(message: &str) -> String {
    format!("{} {}", "✗".red().bold(), message)
}

/// Format a warning message
pub fn warning_message(message: &str) -> String {
    format!("{} {}", "⚠".yellow().bold(), message)
}

/// Format an info message
pub fn info_message(message: &str) -> String {
    format!("{} {}", "ℹ".blue().bold(), message)
}

/// Prompt user for yes/no confirmation
///
/// Returns `Ok(true)` if user confirms, `Ok(false)` if user declines,
/// or `Err` if there's an I/O error.
pub fn confirm_prompt(message: &str, default: bool) -> io::Result<bool> {
    let default_text = if default { "[Y/n]" } else { "[y/N]" };
    print!("{} {} {} ", "?".yellow().bold(), message, default_text);
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let input = input.trim().to_lowercase();

    Ok(match input.as_str() {
        "" => default,
        "y" | "yes" => true,
        "n" | "no" => false,
        _ => default,
    })
}

/// Print a section header
pub fn print_section(title: &str) {
    eprintln!();
    eprintln!("{}", title.cyan().bold());
    eprintln!("{}", "─".repeat(title.len()).cyan());
}

/// Print a summary with statistics
pub fn print_summary(title: &str, items: &[(&str, String)]) {
    eprintln!();
    eprintln!("{}", title.green().bold());
    for (label, value) in items {
        eprintln!("  {}: {}", label.bold(), value);
    }
}

/// Format duration in human-readable format
pub fn format_duration(duration_ms: u64) -> String {
    let seconds = duration_ms as f64 / 1000.0;
    if seconds < 1.0 {
        format!("{}ms", duration_ms)
    } else if seconds < 60.0 {
        format!("{:.2}s", seconds)
    } else {
        let minutes = (seconds / 60.0).floor() as u64;
        let secs = (seconds % 60.0).floor() as u64;
        format!("{}m {}s", minutes, secs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_duration() {
        assert_eq!(format_duration(500), "500ms");
        assert_eq!(format_duration(1500), "1.50s");
        assert_eq!(format_duration(65000), "1m 5s");
    }

    #[test]
    fn test_message_formatting() {
        let msg = success_message("Test");
        assert!(msg.contains("Test"));

        let msg = error_message("Error");
        assert!(msg.contains("Error"));

        let msg = warning_message("Warning");
        assert!(msg.contains("Warning"));

        let msg = info_message("Info");
        assert!(msg.contains("Info"));
    }
}
