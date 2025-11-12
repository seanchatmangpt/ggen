//! User experience level tracking for progressive help

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// User experience level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UserLevel {
    Newcomer,     // 0-5 commands run
    Intermediate, // 6-20 commands run
    Advanced,     // 21-50 commands run
    Expert,       // 50+ commands run
}

impl UserLevel {
    pub fn from_usage_count(count: usize) -> Self {
        match count {
            0..=5 => UserLevel::Newcomer,
            6..=20 => UserLevel::Intermediate,
            21..=50 => UserLevel::Advanced,
            _ => UserLevel::Expert,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            UserLevel::Newcomer => "newcomer",
            UserLevel::Intermediate => "intermediate",
            UserLevel::Advanced => "advanced",
            UserLevel::Expert => "expert",
        }
    }

    /// Get help text tailored to this user level
    pub fn get_help_focus(&self) -> &'static str {
        match self {
            UserLevel::Newcomer => "Getting Started",
            UserLevel::Intermediate => "Common Workflows",
            UserLevel::Advanced => "Advanced Features",
            UserLevel::Expert => "Power User Tips",
        }
    }
}

/// User activity tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserActivity {
    pub total_commands: usize,
    pub command_counts: HashMap<String, usize>,
    pub level: UserLevel,
    pub last_command: Option<String>,
}

impl Default for UserActivity {
    fn default() -> Self {
        Self {
            total_commands: 0,
            command_counts: HashMap::new(),
            level: UserLevel::Newcomer,
            last_command: None,
        }
    }
}

impl UserActivity {
    /// Load user activity from config file
    pub fn load() -> Result<Self, Box<dyn std::error::Error>> {
        let path = Self::config_path()?;
        if !path.exists() {
            return Ok(Self::default());
        }

        let contents = fs::read_to_string(&path)?;
        let activity: UserActivity = toml::from_str(&contents)?;
        Ok(activity)
    }

    /// Save user activity to config file
    pub fn save(&self) -> Result<(), Box<dyn std::error::Error>> {
        let path = Self::config_path()?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let contents = toml::to_string_pretty(self)?;
        fs::write(&path, contents)?;
        Ok(())
    }

    /// Record a command execution
    pub fn record_command(&mut self, command: &str) {
        self.total_commands += 1;
        *self.command_counts.entry(command.to_string()).or_insert(0) += 1;
        self.level = UserLevel::from_usage_count(self.total_commands);
        self.last_command = Some(command.to_string());
    }

    /// Get the current user level
    pub fn get_level(&self) -> UserLevel {
        self.level
    }

    /// Get most used commands
    pub fn get_top_commands(&self, limit: usize) -> Vec<(String, usize)> {
        let mut commands: Vec<_> = self
            .command_counts
            .iter()
            .map(|(k, v)| (k.clone(), *v))
            .collect();
        commands.sort_by(|a, b| b.1.cmp(&a.1));
        commands.into_iter().take(limit).collect()
    }

    /// Check if user has used a specific command
    pub fn has_used_command(&self, command: &str) -> bool {
        self.command_counts.contains_key(command)
    }

    /// Get config file path
    fn config_path() -> Result<PathBuf, Box<dyn std::error::Error>> {
        let home = dirs::home_dir().ok_or("Could not determine home directory")?;
        Ok(home.join(".ggen").join("user_activity.toml"))
    }
}

/// Progressive help text based on user level
pub struct ProgressiveHelp;

impl ProgressiveHelp {
    /// Get help text for a command based on user level
    pub fn get_command_help(command: &str, level: UserLevel) -> String {
        match (command, level) {
            // Generate command
            ("gen", UserLevel::Newcomer) => "Generate code from a template.\n\n\
                 Quickstart: ggen gen templates/example.tmpl\n\n\
                 This will read the template and generate the output file.\n\
                 Templates use YAML frontmatter and Tera syntax.\n\n\
                 💡 Tip: Try 'ggen list' to see available templates first!"
                .to_string(),
            ("gen", UserLevel::Intermediate) => "Generate code from templates with variables.\n\n\
                 Usage: ggen gen <template> [--vars key=value]\n\n\
                 Examples:\n\
                 • ggen gen rust-module.tmpl --vars name=auth\n\
                 • ggen gen service.tmpl --vars name=user service=api\n\n\
                 Variables are passed to the template engine for substitution."
                .to_string(),
            ("gen", UserLevel::Advanced | UserLevel::Expert) => {
                "Advanced template generation with RDF graphs and SPARQL.\n\n\
                 Usage: ggen gen <template> [--vars] [--graph] [--inject]\n\n\
                 Features:\n\
                 • Use --graph to load RDF knowledge graphs\n\
                 • SPARQL queries in templates for semantic data\n\
                 • Injection modes for idempotent updates\n\
                 • Deterministic generation with fixed seeds\n\n\
                 See: https://seanchatmangpt.github.io/ggen/advanced-templates"
                    .to_string()
            }

            // Doctor command
            ("doctor", UserLevel::Newcomer) => "Check if your environment is ready for ggen.\n\n\
                 This command verifies:\n\
                 • Rust and Cargo are installed\n\
                 • Git is available\n\
                 • Optional tools like Ollama and Docker\n\n\
                 Run 'ggen doctor' whenever you encounter setup issues!"
                .to_string(),
            ("doctor", _) => "Environment health check and diagnostics.\n\n\
                 Usage: ggen doctor [-v|--verbose]\n\n\
                 Checks for required and optional dependencies.\n\
                 Use --verbose for detailed fix instructions."
                .to_string(),

            // Default help
            _ => format!(
                "Help for '{}' command\n\nRun 'ggen {} --help' for details.",
                command, command
            ),
        }
    }

    /// Get contextual tips based on usage patterns
    pub fn get_contextual_tips(activity: &UserActivity) -> Vec<String> {
        let mut tips = Vec::new();
        let level = activity.get_level();

        match level {
            UserLevel::Newcomer => {
                tips.push("💡 Try 'ggen quickstart demo' for a quick tutorial".to_string());
                tips.push("📚 Run 'ggen --help' to see all available commands".to_string());
                tips.push("🔍 Use 'ggen search <query>' to find templates".to_string());
            }
            UserLevel::Intermediate => {
                if !activity.has_used_command("ai") {
                    tips.push("🤖 Try AI-powered generation with 'ggen ai generate'".to_string());
                }
                if !activity.has_used_command("market") {
                    tips.push("📦 Explore the marketplace with 'ggen market search'".to_string());
                }
            }
            UserLevel::Advanced => {
                tips.push("⚡ Tip: Use aliases for common workflows".to_string());
                tips.push("🔧 Check out lifecycle commands for project automation".to_string());
            }
            UserLevel::Expert => {
                tips.push("🚀 You're a power user! Consider contributing templates".to_string());
            }
        }

        tips
    }

    /// Get next suggested command based on current context
    pub fn suggest_next_command(last_command: Option<&str>, level: UserLevel) -> Option<String> {
        match (last_command, level) {
            (Some("doctor"), UserLevel::Newcomer) => Some("Try: ggen quickstart demo".to_string()),
            (Some("list"), _) => Some("Try: ggen gen <template-name>".to_string()),
            (Some("search"), _) => Some("Try: ggen add <package-name>".to_string()),
            (Some("gen"), UserLevel::Newcomer) => {
                Some("Next: ggen ai project \"your idea\"".to_string())
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_level_from_count() {
        assert_eq!(UserLevel::from_usage_count(0), UserLevel::Newcomer);
        assert_eq!(UserLevel::from_usage_count(5), UserLevel::Newcomer);
        assert_eq!(UserLevel::from_usage_count(10), UserLevel::Intermediate);
        assert_eq!(UserLevel::from_usage_count(25), UserLevel::Advanced);
        assert_eq!(UserLevel::from_usage_count(100), UserLevel::Expert);
    }

    #[test]
    fn test_user_activity_default() {
        let activity = UserActivity::default();
        assert_eq!(activity.total_commands, 0);
        assert_eq!(activity.level, UserLevel::Newcomer);
    }

    #[test]
    fn test_record_command() {
        let mut activity = UserActivity::default();
        activity.record_command("gen");
        assert_eq!(activity.total_commands, 1);
        assert_eq!(*activity.command_counts.get("gen").unwrap(), 1);
    }

    #[test]
    fn test_level_progression() {
        let mut activity = UserActivity::default();

        // Start as newcomer
        assert_eq!(activity.get_level(), UserLevel::Newcomer);

        // Progress to intermediate
        for _ in 0..10 {
            activity.record_command("test");
        }
        assert_eq!(activity.get_level(), UserLevel::Intermediate);

        // Progress to advanced
        for _ in 0..15 {
            activity.record_command("test");
        }
        assert_eq!(activity.get_level(), UserLevel::Advanced);

        // Progress to expert
        for _ in 0..30 {
            activity.record_command("test");
        }
        assert_eq!(activity.get_level(), UserLevel::Expert);
    }

    #[test]
    fn test_top_commands() {
        let mut activity = UserActivity::default();
        activity.record_command("gen");
        activity.record_command("gen");
        activity.record_command("list");
        activity.record_command("gen");

        let top = activity.get_top_commands(2);
        assert_eq!(top[0].0, "gen");
        assert_eq!(top[0].1, 3);
    }
}
