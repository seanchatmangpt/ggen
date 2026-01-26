//! Andon Signal System - Stop-the-Line Protocol
//!
//! Inspired by Toyota Production System's Andon (安頓) "stop the line" mechanism.
//! When a defect is detected, STOP IMMEDIATELY. Don't hide problems.
//!
//! # Signal Levels
//!
//! - 🔴 **RED** (STOP): Critical errors requiring immediate halt
//! - 🟡 **YELLOW** (CAUTION): Warnings requiring investigation
//! - 🟢 **GREEN** (GO): All checks passed, safe to proceed
//!
//! # Usage
//!
//! ```ignore
//! let signal = AndonSignal::Red(CriticalError {
//!     code: "MANIFEST_INVALID".to_string(),
//!     message: "Manifest validation failed".to_string(),
//!     context: "File: ggen.toml\nIssue: [ontology].source is required".to_string(),
//!     recovery_steps: vec![
//!         "Open ggen.toml in editor".to_string(),
//!         "Add [ontology] section".to_string(),
//!     ],
//!     documentation_link: "https://ggen.dev/docs/manifest".to_string(),
//! });
//!
//! signal.enforce()?;  // Displays error and returns Err
//! ```

use ggen_utils::error::{Error, Result};
use std::fmt;

/// Critical error information for RED signal
#[derive(Clone, Debug)]
pub struct CriticalError {
    /// Error code (e.g., "MANIFEST_INVALID", "CIRCULAR_DEPENDENCY")
    pub code: String,
    /// User-facing error message
    pub message: String,
    /// Detailed context (file path, specific issues)
    pub context: String,
    /// Numbered recovery steps for user
    pub recovery_steps: Vec<String>,
    /// Link to documentation
    pub documentation_link: String,
}

/// Warning information for YELLOW signal
#[derive(Clone, Debug)]
pub struct Warning {
    /// Warning code (e.g., "UNUSED_FILES", "PERFORMANCE")
    pub code: String,
    /// User-facing warning message
    pub message: String,
    /// Actionable suggestion
    pub suggestion: String,
}

/// Andon Signal - Stop-the-Line Protocol
#[derive(Clone, Debug)]
pub enum AndonSignal {
    /// 🔴 RED SIGNAL - STOP IMMEDIATELY
    /// Critical error, sync cannot proceed
    Red(CriticalError),

    /// 🟡 YELLOW SIGNAL - CAUTION
    /// Warning, but sync can continue (use --strict to block)
    Yellow(Warning),

    /// 🟢 GREEN SIGNAL - GO
    /// All checks passed, safe to proceed
    Green,
}

impl AndonSignal {
    /// Enforce the signal - display message and return error if RED
    ///
    /// - RED: Prints error box, recovery steps, and documentation link. Returns Err.
    /// - YELLOW: Prints warning but continues. Returns Ok.
    /// - GREEN: Prints success message. Returns Ok.
    pub fn enforce(&self) -> Result<()> {
        match self {
            AndonSignal::Red(error) => {
                eprint_red_signal(error);
                Err(Error::new(&format!(
                    "error[{}]: {}",
                    error.code, error.message
                )))
            }
            AndonSignal::Yellow(warning) => {
                eprint_yellow_signal(warning);
                Ok(())
            }
            AndonSignal::Green => {
                eprintln!("✓ 🟢 All Andon checks GREEN - proceeding");
                Ok(())
            }
        }
    }

    /// Create a RED signal for manifest validation failure
    pub fn manifest_invalid(missing_fields: Vec<String>) -> Self {
        AndonSignal::Red(CriticalError {
            code: "MANIFEST_INVALID".to_string(),
            message: format!(
                "Manifest validation failed - {} required field(s) missing",
                missing_fields.len()
            ),
            context: format!(
                "File: ggen.toml\nMissing fields:\n{}",
                missing_fields
                    .iter()
                    .enumerate()
                    .map(|(i, f)| format!("  {}. {}", i + 1, f))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            recovery_steps: vec![
                "Open ggen.toml in editor".to_string(),
                "Add missing fields with values".to_string(),
                format!("Or use `ggen init` to create valid template"),
            ],
            documentation_link: "https://ggen.dev/docs/manifest-format".to_string(),
        })
    }

    /// Create a RED signal for circular dependencies
    pub fn circular_dependency(cycles: Vec<Vec<String>>) -> Self {
        let cycle_display = cycles
            .iter()
            .map(|cycle| {
                cycle
                    .iter()
                    .map(|f| format!("{}", f))
                    .collect::<Vec<_>>()
                    .join(" → ")
            })
            .collect::<Vec<_>>()
            .join("\n    ");

        AndonSignal::Red(CriticalError {
            code: "CIRCULAR_DEPENDENCY".to_string(),
            message: "Circular dependency detected in ontology imports".to_string(),
            context: format!(
                "Cycle found:\n    {}\n\nThis creates infinite loop in dependency resolution",
                cycle_display
            ),
            recovery_steps: vec![
                "Review import statements in affected files".to_string(),
                "Restructure as directed acyclic graph (DAG)".to_string(),
                "Option A: Create root ontology that imports all".to_string(),
                "Option B: Move shared definitions to base ontology".to_string(),
                "Use `ggen sync --validate-only` to verify".to_string(),
            ],
            documentation_link: "https://ggen.dev/docs/ontology-structure".to_string(),
        })
    }

    /// Create a RED signal for permission denied
    pub fn permission_denied(path: &str, reason: &str) -> Self {
        AndonSignal::Red(CriticalError {
            code: "OUTPUT_DIR_NOT_WRITABLE".to_string(),
            message: "Cannot write to output directory".to_string(),
            context: format!(
                "Path: {}\nReason: {}\n\nCheck directory permissions",
                path, reason
            ),
            recovery_steps: vec![
                format!("Make directory writable: chmod u+w {}", path),
                "Or change output_dir in ggen.toml [generation] section".to_string(),
                "(Not recommended) Run with sudo".to_string(),
            ],
            documentation_link: "https://ggen.dev/docs/permissions".to_string(),
        })
    }

    /// Create a YELLOW signal for unused files
    pub fn unused_files(files: Vec<String>) -> Self {
        AndonSignal::Yellow(Warning {
            code: "UNUSED_FILES".to_string(),
            message: format!("Found {} unused ontology files:", files.len()),
            suggestion: if files.len() > 0 {
                format!(
                    "If these aren't needed, delete them.\nOtherwise add to imports: ontology.imports = [{}]",
                    files.iter()
                        .map(|f| format!("\"{}\"", f))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            } else {
                "No unused files".to_string()
            },
        })
    }

    /// Create a YELLOW signal for performance issues
    pub fn performance_warning(duration_ms: u64, target_ms: u64) -> Self {
        AndonSignal::Yellow(Warning {
            code: "PERFORMANCE_SLOW".to_string(),
            message: format!(
                "Performance: Generation took {}ms (target: <{}ms)",
                duration_ms, target_ms
            ),
            suggestion: "Consider optimizing SPARQL queries or enabling caching.".to_string(),
        })
    }

    /// Check if signal is RED (critical)
    pub fn is_red(&self) -> bool {
        matches!(self, AndonSignal::Red(_))
    }

    /// Check if signal is YELLOW (warning)
    pub fn is_yellow(&self) -> bool {
        matches!(self, AndonSignal::Yellow(_))
    }

    /// Check if signal is GREEN (safe)
    pub fn is_green(&self) -> bool {
        matches!(self, AndonSignal::Green)
    }
}

impl fmt::Display for AndonSignal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AndonSignal::Red(error) => {
                write!(f, "🔴 RED SIGNAL: {}", error.message)
            }
            AndonSignal::Yellow(warning) => {
                write!(f, "🟡 YELLOW SIGNAL: {}", warning.message)
            }
            AndonSignal::Green => {
                write!(f, "🟢 GREEN SIGNAL - All checks passed")
            }
        }
    }
}

/// Print RED signal with formatted error box
fn eprint_red_signal(error: &CriticalError) {
    eprintln!();
    eprintln!("╔════════════════════════════════════════════╗");
    eprintln!("║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║");
    eprintln!("╚════════════════════════════════════════════╝");
    eprintln!();
    eprintln!("Error Code: {}", error.code);
    eprintln!("Message: {}", error.message);
    eprintln!();
    eprintln!("Context:");
    for line in error.context.lines() {
        eprintln!("  {}", line);
    }
    eprintln!();
    eprintln!("Recovery Steps:");
    for (i, step) in error.recovery_steps.iter().enumerate() {
        eprintln!("  {}. {}", i + 1, step);
    }
    eprintln!();
    eprintln!("📖 Learn more: {}", error.documentation_link);
    eprintln!();
    eprintln!("Sync STOPPED. Fix error above and retry.");
    eprintln!();
}

/// Print YELLOW signal warning
fn eprint_yellow_signal(warning: &Warning) {
    eprintln!();
    eprintln!("⚠️  ANDON SIGNAL: YELLOW - CAUTION");
    eprintln!("{}", warning.message);
    eprintln!();
    eprintln!("  Suggestion: {}", warning.suggestion);
    eprintln!();
    eprintln!("Continuing (use --strict to error on warnings)");
    eprintln!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_red_signal_manifest_invalid() {
        let signal = AndonSignal::manifest_invalid(
            vec!["[ontology].source".to_string()],
        );
        assert!(signal.is_red());
    }

    #[test]
    fn test_yellow_signal() {
        let signal = AndonSignal::Yellow(Warning {
            code: "TEST".to_string(),
            message: "Test warning".to_string(),
            suggestion: "Do this".to_string(),
        });
        assert!(signal.is_yellow());
    }

    #[test]
    fn test_green_signal() {
        let signal = AndonSignal::Green;
        assert!(signal.is_green());
    }
}
