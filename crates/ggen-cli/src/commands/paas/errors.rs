//! Error types for PaaS submodule operations
//! All errors implement Result<T, PaasError> for safe composition

use std::fmt;

/// PaaS operation errors
#[derive(Debug)]
pub enum PaasError {
    /// Specification closure incomplete
    ClosureIncomplete {
        current: f32,
        required: f32,
        missing_specs: Vec<String>,
    },

    /// Submodule not initialized
    SubmoduleNotInitialized(String),

    /// Submodule already exists
    SubmoduleExists(String),

    /// Git operation failed
    GitFailed(String),

    /// Specification validation failed
    SpecValidationFailed {
        file: String,
        reason: String,
        line: Option<usize>,
    },

    /// Code generation failed
    GenerationFailed {
        stage: String,
        reason: String,
    },

    /// Template not found
    TemplateNotFound(String),

    /// File I/O error
    IoError(String),

    /// Configuration error
    ConfigError(String),

    /// Deployment target invalid
    InvalidTarget(String),

    /// Timeout exceeded
    Timeout {
        operation: String,
        max_duration_ms: u64,
    },

    /// Missing required option
    MissingOption(String),

    /// Invalid noun-verb combination
    InvalidCommand {
        noun: String,
        verb: String,
        available_verbs: Vec<String>,
    },
}

impl fmt::Display for PaasError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClosureIncomplete {
                current,
                required,
                missing_specs,
            } => write!(
                f,
                "Specification closure incomplete: {:.1}% (required {:.1}%). Missing: {}",
                current,
                required,
                missing_specs.join(", ")
            ),
            Self::SubmoduleNotInitialized(name) => {
                write!(f, "Submodule '{}' not initialized. Run: git submodule update --init", name)
            }
            Self::SubmoduleExists(name) => {
                write!(f, "Submodule '{}' already exists", name)
            }
            Self::GitFailed(reason) => write!(f, "Git operation failed: {}", reason),
            Self::SpecValidationFailed {
                file,
                reason,
                line,
            } => {
                let line_info = line.map(|l| format!(" at line {}", l)).unwrap_or_default();
                write!(f, "Specification validation failed in {}{}: {}", file, line_info, reason)
            }
            Self::GenerationFailed { stage, reason } => {
                write!(f, "Code generation failed at stage '{}': {}", stage, reason)
            }
            Self::TemplateNotFound(name) => write!(f, "Template '{}' not found", name),
            Self::IoError(reason) => write!(f, "File I/O error: {}", reason),
            Self::ConfigError(reason) => write!(f, "Configuration error: {}", reason),
            Self::InvalidTarget(target) => {
                write!(f, "Invalid deployment target: {}. Valid: development, staging, production", target)
            }
            Self::Timeout {
                operation,
                max_duration_ms,
            } => {
                write!(f, "Operation '{}' exceeded timeout ({}ms)", operation, max_duration_ms)
            }
            Self::MissingOption(opt) => {
                write!(f, "Required option missing: {}", opt)
            }
            Self::InvalidCommand {
                noun,
                verb,
                available_verbs,
            } => {
                write!(
                    f,
                    "Invalid command: '{}' {}. Available verbs for {}: {}",
                    verb,
                    noun,
                    noun,
                    available_verbs.join(", ")
                )
            }
        }
    }
}

impl std::error::Error for PaasError {}

impl PaasError {
    /// Provide recovery suggestion for this error
    pub fn recovery_suggestion(&self) -> &'static str {
        match self {
            Self::ClosureIncomplete { .. } => {
                "Verify all .specify/*.ttl files exist and contain required definitions. Run 'ggen paas validate --spec .specify/'"
            }
            Self::SubmoduleNotInitialized(_) => {
                "Initialize submodules with: git submodule update --init --recursive"
            }
            Self::SubmoduleExists(_) => {
                "The submodule is already initialized. Use 'update' command instead."
            }
            Self::GitFailed(_) => {
                "Check git configuration and repository permissions. Verify origin remote is accessible."
            }
            Self::SpecValidationFailed { .. } => {
                "Check TTL syntax and ensure ontology is closed (all references defined)."
            }
            Self::GenerationFailed { .. } => {
                "Enable debug logging with GGEN_DEBUG=1 to see detailed error information."
            }
            Self::TemplateNotFound(_) => {
                "Verify templates/ directory exists and contains required .tera files."
            }
            Self::IoError(_) => {
                "Check file permissions and disk space. Verify output directory is writable."
            }
            Self::ConfigError(_) => {
                "Review ggen-paas.toml for syntax errors. All required sections must be defined."
            }
            Self::InvalidTarget(_) => {
                "Use one of: development, staging, production"
            }
            Self::Timeout { .. } => {
                "Operation timed out. Increase timeout or verify RDF store is responsive."
            }
            Self::MissingOption(_) => {
                "Provide the missing option. Use '--help' to see available options."
            }
            Self::InvalidCommand { .. } => {
                "Check command syntax. Use 'ggen paas --help' for available commands."
            }
        }
    }
}

/// Result type alias for PaaS operations
pub type Result<T> = std::result::Result<T, PaasError>;
