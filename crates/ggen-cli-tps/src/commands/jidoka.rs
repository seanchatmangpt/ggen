use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Subcommands for managing andon signals (stop-the-line protocol).
///
/// This enum provides commands to control the andon cord system, which implements
/// a "stop the line" protocol for halting operations when critical issues are detected.
/// Signals track problems by level, allowing teams to prioritize fixes and resume
/// operations only when issues are resolved.
///
/// # Variants
///
/// - `Pull`: Raise an andon signal to stop the line
/// - `Status`: Check current andon status and active signals
/// - `Clear`: Clear a resolved signal
/// - `List`: Display all active signals with optional filtering
/// - `Export`: Save signal history to a file for auditing
#[derive(Debug, Subcommand)]
pub enum JidokaCommands {
    /// Pull the andon cord (stop the line)
    Pull {
        /// Signal level (critical, high, medium, low)
        #[clap(short, long)]
        level: SignalLevel,

        /// Signal message
        #[clap(short, long)]
        message: String,

        /// Context data
        #[clap(short, long)]
        context: Option<String>,
    },

    /// Check current andon status
    Status {
        /// Output format (json, text)
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Clear an andon signal
    Clear {
        /// Signal ID to clear
        #[clap(short, long)]
        signal_id: String,

        /// Resolution notes
        #[clap(short, long)]
        notes: String,
    },

    /// List all active signals
    List {
        /// Filter by level
        #[clap(short, long)]
        level: Option<SignalLevel>,

        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Export signal history
    Export {
        /// Output file
        #[clap(short, long)]
        output: PathBuf,

        /// Include cleared signals
        #[clap(short, long)]
        include_cleared: bool,
    },
}

/// Severity levels for andon signals.
///
/// Defines the priority and severity of a signal raised to stop the line.
/// Higher severity levels require faster resolution.
#[derive(Debug, Clone, Serialize, Deserialize, clap::ValueEnum)]
#[serde(rename_all = "lowercase")]
pub enum SignalLevel {
    /// Critical issue that requires immediate halt
    Critical,
    /// High-priority issue affecting operations
    High,
    /// Medium-priority issue with noticeable impact
    Medium,
    /// Low-priority issue for tracking
    Low,
}

/// Output format specification for andon commands.
///
/// Controls how command output is rendered to the user.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    /// JSON format output
    Json,
    /// Human-readable text format
    Text,
}

/// Represents an andon signal that stops production.
///
/// A signal records when the andon cord is pulled, capturing the issue,
/// its severity level, and tracking whether it has been resolved.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonSignal {
    /// Unique signal identifier (format: "andon-{timestamp}")
    pub id: String,
    /// Severity level of this signal
    pub level: SignalLevel,
    /// Human-readable description of the issue
    pub message: String,
    /// Additional context data as JSON (e.g., error details, stack traces)
    pub context: serde_json::Value,
    /// RFC 3339 timestamp when signal was raised
    pub timestamp: String,
    /// Whether this signal has been cleared/resolved
    pub cleared: bool,
    /// Resolution notes explaining how the issue was fixed
    pub resolution: Option<String>,
}

/// Current state of the andon system.
///
/// Captures the overall status of all active signals and whether production
/// has been halted.
#[derive(Debug, Serialize, Deserialize)]
pub struct AndonStatus {
    /// List of all active andon signals
    pub active_signals: Vec<AndonSignal>,
    /// Whether the production line is currently stopped
    pub line_stopped: bool,
    /// RFC 3339 timestamp of the last status update
    pub last_update: String,
}

impl JidokaCommands {
    /// Execute the jidoka command asynchronously.
    ///
    /// Dispatches to the appropriate handler method based on the command variant.
    /// Each handler manages andon signals and the stop-the-line protocol.
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Pull {
                level,
                message,
                context,
            } => Self::pull_andon(level, message, context).await,
            Self::Status { format } => Self::check_status(format).await,
            Self::Clear { signal_id, notes } => Self::clear_signal(signal_id, notes).await,
            Self::List { level, format } => Self::list_signals(level, format).await,
            Self::Export {
                output,
                include_cleared,
            } => Self::export_history(output, include_cleared).await,
        }
    }

    /// Raise an andon signal to stop the line.
    ///
    /// Creates and persists a new andon signal with the specified level and message.
    /// Stops the production line by setting `line_stopped` to true.
    ///
    /// # Arguments
    ///
    /// * `level` - Severity level of the signal
    /// * `message` - Human-readable problem description
    /// * `context` - Optional JSON context data with additional problem details
    async fn pull_andon(
        level: SignalLevel, message: String, context: Option<String>,
    ) -> Result<()> {
        let signal_id = format!("andon-{}", chrono::Utc::now().timestamp());
        let timestamp = chrono::Utc::now().to_rfc3339();

        let context_value = if let Some(c) = context {
            serde_json::from_str(&c)?
        } else {
            serde_json::json!({})
        };

        let signal = AndonSignal {
            id: signal_id.clone(),
            level,
            message: message.clone(),
            context: context_value,
            timestamp,
            cleared: false,
            resolution: None,
        };

        Self::save_signal(&signal).await?;

        println!("🚨 ANDON CORD PULLED");
        println!("Signal ID: {}", signal_id);
        println!("Message: {}", message);
        println!("🛑 LINE STOPPED - Fix root cause before proceeding");

        Ok(())
    }

    /// Check current andon system status.
    ///
    /// Displays whether the line is stopped and lists all active signals.
    ///
    /// # Arguments
    ///
    /// * `format` - Output format (JSON or text)
    async fn check_status(format: OutputFormat) -> Result<()> {
        let status = Self::load_status().await?;

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&status)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Andon Status");
                println!("============");
                println!("Line Stopped: {}", status.line_stopped);
                println!("Active Signals: {}", status.active_signals.len());
                println!("Last Update: {}", status.last_update);

                if !status.active_signals.is_empty() {
                    println!("\nActive Signals:");
                    for signal in &status.active_signals {
                        println!("  - {} [{:?}] {}", signal.id, signal.level, signal.message);
                    }
                }
            }
        }

        Ok(())
    }

    /// Clear a resolved andon signal.
    ///
    /// Marks a signal as cleared and records resolution notes. When all signals
    /// are cleared, the production line can resume.
    ///
    /// # Arguments
    ///
    /// * `signal_id` - ID of the signal to clear
    /// * `notes` - Resolution notes explaining the fix
    async fn clear_signal(signal_id: String, notes: String) -> Result<()> {
        let mut status = Self::load_status().await?;

        let signal = status
            .active_signals
            .iter_mut()
            .find(|s| s.id == signal_id)
            .ok_or_else(|| CliError::AndonSignal(format!("Signal {} not found", signal_id)))?;

        signal.cleared = true;
        signal.resolution = Some(notes.clone());

        Self::save_status(&status).await?;

        println!("✓ Signal {} cleared", signal_id);
        println!("Resolution: {}", notes);

        if status.active_signals.iter().all(|s| s.cleared) {
            println!("✓ All signals cleared - Line can resume");
        }

        Ok(())
    }

    /// List all active andon signals.
    ///
    /// Displays all active signals, optionally filtered by severity level.
    ///
    /// # Arguments
    ///
    /// * `level` - Optional severity level to filter by
    /// * `format` - Output format (JSON or text)
    async fn list_signals(level: Option<SignalLevel>, format: OutputFormat) -> Result<()> {
        let status = Self::load_status().await?;

        let signals: Vec<_> = status
            .active_signals
            .iter()
            .filter(|s| {
                if let Some(ref filter_level) = level {
                    matches!(
                        (&s.level, filter_level),
                        (SignalLevel::Critical, SignalLevel::Critical)
                            | (SignalLevel::High, SignalLevel::High)
                            | (SignalLevel::Medium, SignalLevel::Medium)
                            | (SignalLevel::Low, SignalLevel::Low)
                    )
                } else {
                    true
                }
            })
            .collect();

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&signals)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Active Andon Signals ({})", signals.len());
                println!("=====================");
                for signal in signals {
                    println!(
                        "{} [{:?}] {} ({})",
                        signal.id,
                        signal.level,
                        signal.message,
                        if signal.cleared { "CLEARED" } else { "ACTIVE" }
                    );
                }
            }
        }

        Ok(())
    }

    /// Export signal history to a file for auditing.
    ///
    /// Writes all signals to a JSON file, optionally including cleared signals.
    ///
    /// # Arguments
    ///
    /// * `output` - Path where the history file will be written
    /// * `include_cleared` - Whether to include resolved signals
    async fn export_history(output: PathBuf, include_cleared: bool) -> Result<()> {
        let status = Self::load_status().await?;

        let signals: Vec<_> = if include_cleared {
            status.active_signals
        } else {
            status
                .active_signals
                .into_iter()
                .filter(|s| !s.cleared)
                .collect()
        };

        let json = serde_json::to_string_pretty(&signals)?;
        tokio::fs::write(&output, json).await?;

        println!(
            "✓ Exported {} signals to {}",
            signals.len(),
            output.display()
        );
        Ok(())
    }

    /// Save a new signal and update status.
    ///
    /// Persists the signal and marks the line as stopped.
    async fn save_signal(signal: &AndonSignal) -> Result<()> {
        let mut status = Self::load_status().await.unwrap_or_else(|_| AndonStatus {
            active_signals: Vec::new(),
            line_stopped: false,
            last_update: chrono::Utc::now().to_rfc3339(),
        });

        status.active_signals.push(signal.clone());
        status.line_stopped = true;
        status.last_update = chrono::Utc::now().to_rfc3339();

        Self::save_status(&status).await
    }

    /// Load the current andon status from storage.
    ///
    /// Returns a default empty status if no file exists yet.
    async fn load_status() -> Result<AndonStatus> {
        let path = Self::status_path();
        if !path.exists() {
            return Ok(AndonStatus {
                active_signals: Vec::new(),
                line_stopped: false,
                last_update: chrono::Utc::now().to_rfc3339(),
            });
        }

        let content = tokio::fs::read_to_string(&path).await?;
        Ok(serde_json::from_str(&content)?)
    }

    /// Persist andon status to storage.
    async fn save_status(status: &AndonStatus) -> Result<()> {
        let path = Self::status_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let json = serde_json::to_string_pretty(status)?;
        tokio::fs::write(&path, json).await?;
        Ok(())
    }

    /// Get the path to the andon status file.
    fn status_path() -> PathBuf {
        PathBuf::from(".ggen/andon-status.json")
    }
}
