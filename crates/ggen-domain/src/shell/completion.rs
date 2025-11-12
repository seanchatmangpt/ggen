//! Shell completion generation and installation - Domain layer
//!
//! This module provides pure business logic for generating and installing shell
//! completion scripts. It's independent of CLI presentation concerns.

use ggen_utils::error::Result;
use std::path::PathBuf;

/// Supported shell types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShellType {
    Bash,
    Zsh,
    Fish,
    PowerShell,
}

impl ShellType {
    /// Parse shell type from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "bash" => Some(Self::Bash),
            "zsh" => Some(Self::Zsh),
            "fish" => Some(Self::Fish),
            "powershell" | "pwsh" => Some(Self::PowerShell),
            _ => None,
        }
    }

    /// Get shell name as string
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Bash => "bash",
            Self::Zsh => "zsh",
            Self::Fish => "fish",
            Self::PowerShell => "powershell",
        }
    }

    /// Get default completion directory for the shell
    pub fn default_completion_dir(&self) -> Option<PathBuf> {
        match self {
            Self::Bash => {
                // Try XDG_DATA_HOME first, then fallback to ~/.bash_completion.d
                if let Ok(xdg_data) = std::env::var("XDG_DATA_HOME") {
                    Some(PathBuf::from(xdg_data).join("bash-completion/completions"))
                } else if let Ok(home) = std::env::var("HOME") {
                    Some(PathBuf::from(home).join(".bash_completion.d"))
                } else {
                    None
                }
            }
            Self::Zsh => {
                // Try XDG_DATA_HOME first, then fallback to ~/.zfunc
                if let Ok(xdg_data) = std::env::var("XDG_DATA_HOME") {
                    Some(PathBuf::from(xdg_data).join("zsh/site-functions"))
                } else if let Ok(home) = std::env::var("HOME") {
                    Some(PathBuf::from(home).join(".zfunc"))
                } else {
                    None
                }
            }
            Self::Fish => {
                // Try XDG_CONFIG_HOME first, then fallback to ~/.config/fish
                if let Ok(xdg_config) = std::env::var("XDG_CONFIG_HOME") {
                    Some(PathBuf::from(xdg_config).join("fish").join("completions"))
                } else if let Ok(home) = std::env::var("HOME") {
                    Some(
                        PathBuf::from(home)
                            .join(".config")
                            .join("fish")
                            .join("completions"),
                    )
                } else {
                    None
                }
            }
            Self::PowerShell => {
                // PowerShell completion is typically in profile directory
                if let Ok(profile) = std::env::var("PROFILE") {
                    Some(PathBuf::from(profile).parent()?.to_path_buf())
                } else {
                    None
                }
            }
        }
    }
}

/// Result of completion generation
#[derive(Debug, Clone)]
pub struct CompletionResult {
    pub script: String,
    pub shell: ShellType,
}

/// Trait for generating shell completions
pub trait CompletionGenerator {
    /// Generate completion script for the specified shell
    fn generate(&self, shell: ShellType) -> Result<CompletionResult>;
}

/// Trait for installing shell completions
pub trait CompletionInstaller {
    /// Install completion script to default location
    fn install(&self, result: &CompletionResult, force: bool) -> Result<PathBuf>;

    /// Install completion script to custom location
    fn install_to(&self, result: &CompletionResult, path: PathBuf, force: bool) -> Result<PathBuf>;
}

/// Trait for listing available shells
pub trait ShellLister {
    /// List all supported shells
    fn list_supported(&self) -> Vec<ShellType>;

    /// Check if a shell is installed on the system
    fn is_installed(&self, shell: ShellType) -> bool;
}

// NOTE: Completion generation is delegated to the CLI layer (ggen-agent-editor)
// This domain layer only defines the contracts/traits.
// The actual implementation using clap_complete lives in ggen-agent-editor.

/// Default implementation for installing completions
pub struct FileSystemCompletionInstaller;

impl CompletionInstaller for FileSystemCompletionInstaller {
    fn install(&self, result: &CompletionResult, force: bool) -> Result<PathBuf> {
        let dir = result.shell.default_completion_dir().ok_or_else(|| {
            ggen_utils::error::Error::new("Could not determine completion directory")
        })?;

        let filename = format!("ggen.{}", result.shell.as_str());
        let path = dir.join(filename);

        self.install_to(result, path, force)
    }

    fn install_to(&self, result: &CompletionResult, path: PathBuf, force: bool) -> Result<PathBuf> {
        // Check if file exists and force is not set
        if path.exists() && !force {
            return Err(ggen_utils::error::Error::new(&format!(
                "Completion file already exists: {}. Use --force to overwrite",
                path.display()
            )));
        }

        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Write completion script
        std::fs::write(&path, &result.script)?;

        Ok(path)
    }
}

/// Default implementation for listing shells
pub struct SystemShellLister;

impl ShellLister for SystemShellLister {
    fn list_supported(&self) -> Vec<ShellType> {
        vec![
            ShellType::Bash,
            ShellType::Zsh,
            ShellType::Fish,
            ShellType::PowerShell,
        ]
    }

    fn is_installed(&self, shell: ShellType) -> bool {
        // Try to find shell executable
        let shell_cmd = match shell {
            ShellType::Bash => "bash",
            ShellType::Zsh => "zsh",
            ShellType::Fish => "fish",
            ShellType::PowerShell => {
                if cfg!(windows) {
                    "pwsh.exe"
                } else {
                    "pwsh"
                }
            }
        };

        // Use which/where to check if shell exists
        std::process::Command::new(if cfg!(windows) { "where" } else { "which" })
            .arg(shell_cmd)
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shell_type_from_str() {
        assert_eq!(ShellType::from_str("bash"), Some(ShellType::Bash));
        assert_eq!(ShellType::from_str("BASH"), Some(ShellType::Bash));
        assert_eq!(ShellType::from_str("zsh"), Some(ShellType::Zsh));
        assert_eq!(ShellType::from_str("fish"), Some(ShellType::Fish));
        assert_eq!(
            ShellType::from_str("powershell"),
            Some(ShellType::PowerShell)
        );
        assert_eq!(ShellType::from_str("pwsh"), Some(ShellType::PowerShell));
        assert_eq!(ShellType::from_str("invalid"), None);
    }

    #[test]
    fn test_shell_type_as_str() {
        assert_eq!(ShellType::Bash.as_str(), "bash");
        assert_eq!(ShellType::Zsh.as_str(), "zsh");
        assert_eq!(ShellType::Fish.as_str(), "fish");
        assert_eq!(ShellType::PowerShell.as_str(), "powershell");
    }

    #[test]
    fn test_system_shell_lister_lists_all() {
        let lister = SystemShellLister;
        let shells = lister.list_supported();
        assert_eq!(shells.len(), 4);
        assert!(shells.contains(&ShellType::Bash));
        assert!(shells.contains(&ShellType::Zsh));
        assert!(shells.contains(&ShellType::Fish));
        assert!(shells.contains(&ShellType::PowerShell));
    }
}
