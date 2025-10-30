//! Enhanced error handling with contextual help and platform-specific fixes

use colored::Colorize;
use std::fmt;

/// Error category for better error grouping
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorCategory {
    FileNotFound,
    PermissionDenied,
    InvalidInput,
    NetworkError,
    ConfigurationError,
    TemplateError,
    DependencyError,
    BuildError,
    Unknown,
}

/// Platform-specific fix suggestions
#[derive(Debug, Clone)]
pub struct PlatformFix {
    pub macos: Option<String>,
    pub linux: Option<String>,
    pub windows: Option<String>,
}

impl PlatformFix {
    pub fn new() -> Self {
        Self {
            macos: None,
            linux: None,
            windows: None,
        }
    }

    pub fn macos(mut self, fix: impl Into<String>) -> Self {
        self.macos = Some(fix.into());
        self
    }

    pub fn linux(mut self, fix: impl Into<String>) -> Self {
        self.linux = Some(fix.into());
        self
    }

    pub fn windows(mut self, fix: impl Into<String>) -> Self {
        self.windows = Some(fix.into());
        self
    }

    pub fn all_platforms(fix: impl Into<String>) -> Self {
        let fix_str = fix.into();
        Self {
            macos: Some(fix_str.clone()),
            linux: Some(fix_str.clone()),
            windows: Some(fix_str),
        }
    }

    #[cfg(target_os = "macos")]
    pub fn current_platform(&self) -> Option<&str> {
        self.macos.as_deref()
    }

    #[cfg(target_os = "linux")]
    pub fn current_platform(&self) -> Option<&str> {
        self.linux.as_deref()
    }

    #[cfg(target_os = "windows")]
    pub fn current_platform(&self) -> Option<&str> {
        self.windows.as_deref()
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    pub fn current_platform(&self) -> Option<&str> {
        None
    }
}

impl Default for PlatformFix {
    fn default() -> Self {
        Self::new()
    }
}

/// Enhanced error with contextual help
#[derive(Debug, Clone)]
pub struct EnhancedError {
    pub category: ErrorCategory,
    pub message: String,
    pub context: Option<String>,
    pub fix_suggestions: Vec<String>,
    pub platform_fixes: Option<PlatformFix>,
    pub did_you_mean: Option<Vec<String>>,
    pub related_docs: Option<String>,
}

impl EnhancedError {
    pub fn new(category: ErrorCategory, message: impl Into<String>) -> Self {
        Self {
            category,
            message: message.into(),
            context: None,
            fix_suggestions: Vec::new(),
            platform_fixes: None,
            did_you_mean: None,
            related_docs: None,
        }
    }

    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    pub fn with_fix(mut self, fix: impl Into<String>) -> Self {
        self.fix_suggestions.push(fix.into());
        self
    }

    pub fn with_fixes(mut self, fixes: Vec<String>) -> Self {
        self.fix_suggestions.extend(fixes);
        self
    }

    pub fn with_platform_fix(mut self, platform_fix: PlatformFix) -> Self {
        self.platform_fixes = Some(platform_fix);
        self
    }

    pub fn with_did_you_mean(mut self, suggestions: Vec<String>) -> Self {
        self.did_you_mean = Some(suggestions);
        self
    }

    pub fn with_docs(mut self, docs_url: impl Into<String>) -> Self {
        self.related_docs = Some(docs_url.into());
        self
    }

    /// Display the error with colored output and helpful context
    pub fn display_pretty(&self) {
        println!();
        println!("{} {}", "❌ Error:".red().bold(), self.message.red());

        if let Some(context) = &self.context {
            println!();
            println!("{} {}", "📝 Context:".yellow(), context.dimmed());
        }

        if let Some(suggestions) = &self.did_you_mean {
            println!();
            println!("{}", "💡 Did you mean:".cyan().bold());
            for suggestion in suggestions {
                println!("   • {}", suggestion.cyan());
            }
        }

        if !self.fix_suggestions.is_empty() {
            println!();
            println!("{}", "🔧 How to fix:".green().bold());
            for (i, fix) in self.fix_suggestions.iter().enumerate() {
                println!("   {}. {}", i + 1, fix);
            }
        }

        if let Some(platform_fixes) = &self.platform_fixes {
            if let Some(current_fix) = platform_fixes.current_platform() {
                println!();
                println!("{}", "🖥️  Platform-specific fix:".green().bold());
                println!("   {}", current_fix);
            }
        }

        if let Some(docs) = &self.related_docs {
            println!();
            println!("{} {}", "📚 Documentation:".blue(), docs);
        }

        println!();
    }
}

impl fmt::Display for EnhancedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(context) = &self.context {
            write!(f, " ({})", context)?;
        }
        Ok(())
    }
}

impl std::error::Error for EnhancedError {}

/// Common enhanced errors with contextual help
pub mod common_errors {
    use super::*;

    pub fn file_not_found(path: &str) -> EnhancedError {
        EnhancedError::new(
            ErrorCategory::FileNotFound,
            format!("File not found: {}", path),
        )
        .with_context("The specified file does not exist or is not accessible")
        .with_fix("Check that the file path is correct")
        .with_fix("Verify that you have permission to access the file")
        .with_fix("Try using an absolute path instead of a relative path")
        .with_platform_fix(
            PlatformFix::new()
                .macos("Use 'ls -la' to check file permissions")
                .linux("Use 'ls -la' to check file permissions")
                .windows("Use 'dir' or 'Get-ChildItem' to check file existence"),
        )
    }

    pub fn template_not_found(template_name: &str, available: Vec<String>) -> EnhancedError {
        let mut error = EnhancedError::new(
            ErrorCategory::TemplateError,
            format!("Template '{}' not found", template_name),
        )
        .with_context("The specified template does not exist in the registry")
        .with_fix("Run 'ggen list' to see available templates")
        .with_fix("Use 'ggen search <query>' to find templates")
        .with_docs("https://seanchatmangpt.github.io/ggen/templates");

        if !available.is_empty() {
            let suggestions = available
                .iter()
                .filter(|t| {
                    let name_lower = template_name.to_lowercase();
                    let t_lower = t.to_lowercase();
                    t_lower.contains(&name_lower) || name_lower.contains(&t_lower)
                })
                .take(3)
                .cloned()
                .collect::<Vec<_>>();

            if !suggestions.is_empty() {
                error = error.with_did_you_mean(suggestions);
            }
        }

        error
    }

    pub fn command_not_found(command: &str, available: Vec<String>) -> EnhancedError {
        let mut error = EnhancedError::new(
            ErrorCategory::InvalidInput,
            format!("Unknown command: {}", command),
        )
        .with_context("The command you entered is not recognized")
        .with_fix("Run 'ggen --help' to see all available commands")
        .with_fix("Check for typos in the command name");

        // Find similar commands using simple string distance
        let suggestions = available
            .iter()
            .filter(|cmd| {
                let cmd_lower = cmd.to_lowercase();
                let command_lower = command.to_lowercase();
                // Simple similarity check
                cmd_lower.starts_with(&command_lower)
                    || command_lower.starts_with(&cmd_lower)
                    || levenshtein_distance(&cmd_lower, &command_lower) <= 2
            })
            .take(3)
            .cloned()
            .collect::<Vec<_>>();

        if !suggestions.is_empty() {
            error = error.with_did_you_mean(suggestions);
        }

        error
    }

    pub fn missing_dependency(dependency: &str, install_cmd: &str) -> EnhancedError {
        EnhancedError::new(
            ErrorCategory::DependencyError,
            format!("Required dependency not found: {}", dependency),
        )
        .with_context("A required tool or library is not installed")
        .with_fix(format!("Install {} using: {}", dependency, install_cmd))
        .with_fix("Run 'ggen doctor' to check your environment")
        .with_platform_fix(
            PlatformFix::new()
                .macos(format!("brew install {}", dependency))
                .linux(format!("sudo apt install {}", dependency))
                .windows(format!("Use chocolatey: choco install {}", dependency)),
        )
    }

    pub fn permission_denied(path: &str) -> EnhancedError {
        EnhancedError::new(
            ErrorCategory::PermissionDenied,
            format!("Permission denied: {}", path),
        )
        .with_context("You don't have permission to access this file or directory")
        .with_fix("Check file permissions and ownership")
        .with_fix("Try running with appropriate permissions")
        .with_platform_fix(
            PlatformFix::new()
                .macos(format!("Use 'chmod +r {}' to add read permission", path))
                .linux(format!("Use 'chmod +r {}' to add read permission", path))
                .windows("Right-click the file and check security settings"),
        )
    }

    pub fn invalid_yaml(error_msg: &str) -> EnhancedError {
        EnhancedError::new(
            ErrorCategory::ConfigurationError,
            "Invalid YAML syntax in template frontmatter",
        )
        .with_context(error_msg)
        .with_fix("Check for indentation errors (YAML uses spaces, not tabs)")
        .with_fix("Ensure all strings with special characters are quoted")
        .with_fix("Verify that list items start with '- '")
        .with_fix("Use a YAML validator like yamllint")
        .with_docs("https://yaml.org/spec/1.2/spec.html")
    }

    pub fn network_error(url: &str) -> EnhancedError {
        EnhancedError::new(
            ErrorCategory::NetworkError,
            format!("Network request failed: {}", url),
        )
        .with_context("Could not reach the remote server")
        .with_fix("Check your internet connection")
        .with_fix("Verify the URL is correct")
        .with_fix("Check if you're behind a proxy or firewall")
        .with_fix("Try again in a few moments")
    }

    // Simple Levenshtein distance for "Did you mean?" suggestions
    fn levenshtein_distance(s1: &str, s2: &str) -> usize {
        let len1 = s1.len();
        let len2 = s2.len();
        let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

        #[allow(clippy::needless_range_loop)]
        for i in 0..=len1 {
            matrix[i][0] = i;
        }
        #[allow(clippy::needless_range_loop)]
        for j in 0..=len2 {
            matrix[0][j] = j;
        }

        for (i, c1) in s1.chars().enumerate() {
            for (j, c2) in s2.chars().enumerate() {
                let cost = if c1 == c2 { 0 } else { 1 };
                matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                    .min(matrix[i + 1][j] + 1)
                    .min(matrix[i][j] + cost);
            }
        }

        matrix[len1][len2]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enhanced_error_creation() {
        let error = EnhancedError::new(ErrorCategory::FileNotFound, "test.txt not found");
        assert_eq!(error.message, "test.txt not found");
        assert_eq!(error.category, ErrorCategory::FileNotFound);
    }

    #[test]
    fn test_error_with_context() {
        let error = EnhancedError::new(ErrorCategory::InvalidInput, "Invalid input")
            .with_context("Expected a number");
        assert_eq!(error.context, Some("Expected a number".to_string()));
    }

    #[test]
    fn test_error_with_fixes() {
        let error =
            EnhancedError::new(ErrorCategory::ConfigurationError, "Config error")
                .with_fix("Fix 1")
                .with_fix("Fix 2");
        assert_eq!(error.fix_suggestions.len(), 2);
    }

    #[test]
    fn test_platform_fix() {
        let fix = PlatformFix::new()
            .macos("brew install rust")
            .linux("apt install rust")
            .windows("choco install rust");

        assert!(fix.macos.is_some());
        assert!(fix.linux.is_some());
        assert!(fix.windows.is_some());
    }

    #[test]
    fn test_file_not_found_error() {
        let error = common_errors::file_not_found("test.txt");
        assert_eq!(error.category, ErrorCategory::FileNotFound);
        assert!(!error.fix_suggestions.is_empty());
    }

    #[test]
    fn test_command_not_found_with_suggestions() {
        let available = vec!["generate".to_string(), "list".to_string(), "search".to_string()];
        let error = common_errors::command_not_found("gen", available);
        assert!(error.did_you_mean.is_some());
    }
}
