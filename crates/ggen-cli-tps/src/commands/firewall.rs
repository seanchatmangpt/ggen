use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use std::path::PathBuf;

#[derive(Debug, Subcommand)]
pub enum FirewallCommands {
    /// Add firewall rule
    Add {
        /// Rule type (allow, deny)
        #[clap(short = 't', long)]
        rule_type: RuleType,

        /// Source IP or CIDR
        #[clap(short, long)]
        source: String,

        /// Destination IP or CIDR
        #[clap(short, long)]
        destination: Option<String>,

        /// Port number
        #[clap(short, long)]
        port: Option<u16>,

        /// Protocol (tcp, udp, icmp)
        #[clap(short = 'r', long)]
        protocol: Option<Protocol>,

        /// Priority (lower is higher priority)
        #[clap(short = 'y', long, default_value = "100")]
        priority: u32,
    },

    /// Remove firewall rule
    Remove {
        /// Rule ID to remove
        #[clap(short, long)]
        id: String,
    },

    /// List all firewall rules
    List {
        /// Output format (json, text)
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,

        /// Filter by rule type
        #[clap(short = 't', long)]
        rule_type: Option<RuleType>,
    },

    /// Test if traffic would be allowed
    Test {
        /// Source IP
        #[clap(short, long)]
        source: IpAddr,

        /// Destination IP
        #[clap(short, long)]
        destination: IpAddr,

        /// Port number
        #[clap(short, long)]
        port: u16,

        /// Protocol
        #[clap(short = 'r', long)]
        protocol: Protocol,
    },

    /// Export firewall rules
    Export {
        /// Output file
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Import firewall rules
    Import {
        /// Input file
        #[clap(short, long)]
        input: PathBuf,

        /// Clear existing rules before import
        #[clap(short, long)]
        clear: bool,
    },

    /// Show firewall statistics
    Stats {
        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Clear all firewall rules
    Clear {
        /// Confirm clearing all rules
        #[clap(short, long)]
        confirm: bool,
    },
}

#[derive(Debug, Clone, clap::ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RuleType {
    Allow,
    Deny,
}

#[derive(Debug, Clone, clap::ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Protocol {
    Tcp,
    Udp,
    Icmp,
}

#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    Json,
    Text,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FirewallRule {
    pub id: String,
    pub rule_type: RuleType,
    pub source: String,
    pub destination: Option<String>,
    pub port: Option<u16>,
    pub protocol: Option<Protocol>,
    pub priority: u32,
    pub created_at: String,
    pub hits: u64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FirewallConfig {
    pub rules: Vec<FirewallRule>,
    pub default_action: RuleType,
    pub last_updated: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FirewallStats {
    pub total_rules: usize,
    pub allow_rules: usize,
    pub deny_rules: usize,
    pub total_hits: u64,
    pub last_updated: String,
}

impl FirewallCommands {
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Add {
                rule_type,
                source,
                destination,
                port,
                protocol,
                priority,
            } => Self::add_rule(rule_type, source, destination, port, protocol, priority).await,
            Self::Remove { id } => Self::remove_rule(id).await,
            Self::List { format, rule_type } => Self::list_rules(format, rule_type).await,
            Self::Test {
                source,
                destination,
                port,
                protocol,
            } => Self::test_traffic(source, destination, port, protocol).await,
            Self::Export { output } => Self::export_rules(output).await,
            Self::Import { input, clear } => Self::import_rules(input, clear).await,
            Self::Stats { format } => Self::show_stats(format).await,
            Self::Clear { confirm } => Self::clear_rules(confirm).await,
        }
    }

    async fn add_rule(
        rule_type: RuleType,
        source: String,
        destination: Option<String>,
        port: Option<u16>,
        protocol: Option<Protocol>,
        priority: u32,
    ) -> Result<()> {
        let mut config = Self::load_config().await?;

        let rule_id = format!("fw-{}", chrono::Utc::now().timestamp());
        let rule = FirewallRule {
            id: rule_id.clone(),
            rule_type: rule_type.clone(),
            source: source.clone(),
            destination: destination.clone(),
            port,
            protocol: protocol.clone(),
            priority,
            created_at: chrono::Utc::now().to_rfc3339(),
            hits: 0,
        };

        config.rules.push(rule);
        config.rules.sort_by_key(|r| r.priority);
        config.last_updated = chrono::Utc::now().to_rfc3339();

        Self::save_config(&config).await?;

        println!("✓ Firewall rule added");
        println!("  ID: {}", rule_id);
        println!("  Type: {:?}", rule_type);
        println!("  Source: {}", source);
        if let Some(dest) = destination {
            println!("  Destination: {}", dest);
        }
        if let Some(p) = port {
            println!("  Port: {}", p);
        }
        if let Some(proto) = protocol {
            println!("  Protocol: {:?}", proto);
        }
        println!("  Priority: {}", priority);

        Ok(())
    }

    async fn remove_rule(id: String) -> Result<()> {
        let mut config = Self::load_config().await?;

        let initial_len = config.rules.len();
        config.rules.retain(|r| r.id != id);

        if config.rules.len() == initial_len {
            return Err(CliError::FirewallBlocked(format!(
                "Rule {} not found",
                id
            )));
        }

        config.last_updated = chrono::Utc::now().to_rfc3339();
        Self::save_config(&config).await?;

        println!("✓ Firewall rule removed: {}", id);
        Ok(())
    }

    async fn list_rules(format: OutputFormat, rule_type_filter: Option<RuleType>) -> Result<()> {
        let config = Self::load_config().await?;

        let rules: Vec<_> = config
            .rules
            .iter()
            .filter(|r| {
                if let Some(ref filter) = rule_type_filter {
                    matches!(
                        (&r.rule_type, filter),
                        (RuleType::Allow, RuleType::Allow) | (RuleType::Deny, RuleType::Deny)
                    )
                } else {
                    true
                }
            })
            .collect();

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&rules)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Firewall Rules ({})", rules.len());
                println!("================");
                println!("Default Action: {:?}", config.default_action);
                println!();

                for rule in rules {
                    println!("{} [{:?}] Priority: {}", rule.id, rule.rule_type, rule.priority);
                    println!("  Source: {}", rule.source);
                    if let Some(ref dest) = rule.destination {
                        println!("  Destination: {}", dest);
                    }
                    if let Some(port) = rule.port {
                        println!("  Port: {}", port);
                    }
                    if let Some(ref proto) = rule.protocol {
                        println!("  Protocol: {:?}", proto);
                    }
                    println!("  Hits: {}", rule.hits);
                    println!();
                }
            }
        }

        Ok(())
    }

    async fn test_traffic(
        source: IpAddr,
        destination: IpAddr,
        port: u16,
        protocol: Protocol,
    ) -> Result<()> {
        let config = Self::load_config().await?;

        println!("Testing firewall rules:");
        println!("  Source: {}", source);
        println!("  Destination: {}", destination);
        println!("  Port: {}", port);
        println!("  Protocol: {:?}", protocol);
        println!();

        let mut matched_rule: Option<&FirewallRule> = None;

        for rule in &config.rules {
            if Self::rule_matches(rule, &source, &destination, port, &protocol) {
                matched_rule = Some(rule);
                break;
            }
        }

        if let Some(rule) = matched_rule {
            match rule.rule_type {
                RuleType::Allow => {
                    println!("✓ ALLOWED by rule: {}", rule.id);
                    println!("  Priority: {}", rule.priority);
                }
                RuleType::Deny => {
                    println!("✗ DENIED by rule: {}", rule.id);
                    println!("  Priority: {}", rule.priority);
                }
            }
        } else {
            match config.default_action {
                RuleType::Allow => println!("✓ ALLOWED by default policy"),
                RuleType::Deny => println!("✗ DENIED by default policy"),
            }
        }

        Ok(())
    }

    async fn export_rules(output: PathBuf) -> Result<()> {
        let config = Self::load_config().await?;
        let json = serde_json::to_string_pretty(&config)?;
        tokio::fs::write(&output, json).await?;

        println!("✓ Firewall rules exported to {}", output.display());
        println!("  Rules: {}", config.rules.len());
        Ok(())
    }

    async fn import_rules(input: PathBuf, clear: bool) -> Result<()> {
        let content = tokio::fs::read_to_string(&input).await?;
        let imported_config: FirewallConfig = serde_json::from_str(&content)?;

        let mut config = if clear {
            FirewallConfig {
                rules: Vec::new(),
                default_action: imported_config.default_action.clone(),
                last_updated: chrono::Utc::now().to_rfc3339(),
            }
        } else {
            Self::load_config().await?
        };

        let imported_count = imported_config.rules.len();
        config.rules.extend(imported_config.rules);
        config.rules.sort_by_key(|r| r.priority);
        config.last_updated = chrono::Utc::now().to_rfc3339();

        Self::save_config(&config).await?;

        println!("✓ Firewall rules imported");
        println!("  Imported: {}", imported_count);
        println!("  Total: {}", config.rules.len());

        Ok(())
    }

    async fn show_stats(format: OutputFormat) -> Result<()> {
        let config = Self::load_config().await?;

        let stats = FirewallStats {
            total_rules: config.rules.len(),
            allow_rules: config
                .rules
                .iter()
                .filter(|r| matches!(r.rule_type, RuleType::Allow))
                .count(),
            deny_rules: config
                .rules
                .iter()
                .filter(|r| matches!(r.rule_type, RuleType::Deny))
                .count(),
            total_hits: config.rules.iter().map(|r| r.hits).sum(),
            last_updated: config.last_updated,
        };

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&stats)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Firewall Statistics");
                println!("===================");
                println!("Total Rules: {}", stats.total_rules);
                println!("  Allow: {}", stats.allow_rules);
                println!("  Deny: {}", stats.deny_rules);
                println!("Total Hits: {}", stats.total_hits);
                println!("Last Updated: {}", stats.last_updated);
            }
        }

        Ok(())
    }

    async fn clear_rules(confirm: bool) -> Result<()> {
        if !confirm {
            return Err(CliError::Validation(
                "Must confirm with --confirm flag".to_string(),
            ));
        }

        let config = FirewallConfig {
            rules: Vec::new(),
            default_action: RuleType::Deny,
            last_updated: chrono::Utc::now().to_rfc3339(),
        };

        Self::save_config(&config).await?;

        println!("✓ All firewall rules cleared");
        println!("⚠ Default action: Deny");

        Ok(())
    }

    fn rule_matches(
        rule: &FirewallRule,
        source: &IpAddr,
        destination: &IpAddr,
        port: u16,
        protocol: &Protocol,
    ) -> bool {
        // Simple string matching for demonstration
        // In production, would use CIDR matching
        let source_match = rule.source == source.to_string() || rule.source == "0.0.0.0/0";

        let dest_match = rule
            .destination
            .as_ref()
            .map_or(true, |d| d == &destination.to_string() || d == "0.0.0.0/0");

        let port_match = rule.port.map_or(true, |p| p == port);

        let proto_match = rule.protocol.as_ref().map_or(true, |p| {
            matches!(
                (p, protocol),
                (Protocol::Tcp, Protocol::Tcp)
                    | (Protocol::Udp, Protocol::Udp)
                    | (Protocol::Icmp, Protocol::Icmp)
            )
        });

        source_match && dest_match && port_match && proto_match
    }

    async fn load_config() -> Result<FirewallConfig> {
        let path = Self::config_path();
        if !path.exists() {
            return Ok(FirewallConfig {
                rules: Vec::new(),
                default_action: RuleType::Deny,
                last_updated: chrono::Utc::now().to_rfc3339(),
            });
        }

        let content = tokio::fs::read_to_string(&path).await?;
        Ok(serde_json::from_str(&content)?)
    }

    async fn save_config(config: &FirewallConfig) -> Result<()> {
        let path = Self::config_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let json = serde_json::to_string_pretty(config)?;
        tokio::fs::write(&path, json).await?;
        Ok(())
    }

    fn config_path() -> PathBuf {
        PathBuf::from(".ggen/firewall-config.json")
    }
}
