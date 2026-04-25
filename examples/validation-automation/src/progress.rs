//! Progress tracking with visual feedback
//!
//! Provides progress bars, spinners, and status updates for long-running validation operations.

use indicatif::{ProgressBar, ProgressStyle};
use std::time::Duration;
use colored::Colorize;

/// Progress tracker for validation operations
pub struct ProgressTracker {
    bar: ProgressBar,
    start_time: std::time::Instant,
}

impl ProgressTracker {
    /// Create a new progress tracker
    ///
    /// # Arguments
    /// * `message` - Description of what's being tracked
    /// * `total` - Total number of items to process
    pub fn new(message: &str, total: usize) -> Self {
        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template(&format!(
                    "{} {} {}",
                    "[{elapsed_precise}]".bold(),
                    "{bar:.40.cyan/blue}",
                    "{pos}/{len} {msg}"
                ))
                .expect("invalid template")
                .progress_chars("=>-")
        );
        bar.set_message(message.to_string());

        Self {
            bar,
            start_time: std::time::Instant::now(),
        }
    }

    /// Create a spinner for indeterminate operations
    pub fn spinner(message: &str) -> Self {
        let bar = ProgressBar::new_spinner();
        bar.set_style(
            ProgressStyle::default_spinner()
                .template(&format!("{} {} {{msg}}", "{spinner}", message))
                .expect("invalid template")
                .tick_chars("⠁⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
        );

        Self {
            bar,
            start_time: std::time::Instant::now(),
        }
    }

    /// Increment progress by 1
    pub fn inc(&self) {
        self.bar.inc(1);
    }

    /// Increment progress by custom amount
    pub fn inc_by(&self, amount: usize) {
        self.bar.inc(amount as u64);
    }

    /// Update progress message
    pub fn set_message(&self, msg: &str) {
        self.bar.set_message(msg.to_string());
    }

    /// Mark progress as complete
    pub fn finish(self, message: &str) {
        let msg = format!("✅ {}", message);
        self.bar.finish_with_message(msg);
    }

    /// Mark progress as failed
    pub fn fail(self, message: &str) {
        let msg = format!("❌ {}", message);
        self.bar.abandon_with_message(msg);
    }

    /// Get elapsed time
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Get elapsed time as formatted string
    pub fn elapsed_formatted(&self) -> String {
        let elapsed = self.elapsed();
        let secs = elapsed.as_secs();

        if secs < 60 {
            format!("{}s", secs)
        } else if secs < 3600 {
            format!("{}m {}s", secs / 60, secs % 60)
        } else {
            format!("{}h {}m", secs / 3600, (secs % 3600) / 60)
        }
    }
}

/// Status printer with colored output
pub struct StatusPrinter {
    verbose: bool,
}

impl StatusPrinter {
    /// Create a new status printer
    pub fn new(verbose: bool) -> Self {
        Self { verbose }
    }

    /// Print success message
    pub fn success(&self, message: &str) {
        println!("{} {}", "✅".green().bold(), message);
    }

    /// Print error message
    pub fn error(&self, message: &str) {
        eprintln!("{} {}", "❌".red().bold(), message);
    }

    /// Print warning message
    pub fn warning(&self, message: &str) {
        println!("{} {}", "⚠️ ".yellow().bold(), message);
    }

    /// Print info message
    pub fn info(&self, message: &str) {
        if self.verbose {
            println!("{} {}", "ℹ️ ".blue(), message);
        }
    }

    /// Print header
    pub fn header(&self, message: &str) {
        println!();
        println!("{}", message.bold().underline());
        println!();
    }

    /// Print section
    pub fn section(&self, message: &str) {
        println!("\n{}", message.bold());
        println!("{}", "─".repeat(message.len()));
    }

    /// Print sub-section
    pub fn subsection(&self, message: &str) {
        println!("\n{}", message.cyan());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elapsed_formatting() {
        let tracker = ProgressTracker::new("Test", 100);
        std::thread::sleep(std::time::Duration::from_millis(1100));
        assert!(tracker.elapsed_formatted().contains("1s"));
    }
}
