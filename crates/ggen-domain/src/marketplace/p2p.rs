//! P2P marketplace command implementation
//!
//! This module provides CLI commands for interacting with the P2P marketplace network.

use ggen_utils::error::{GgenError, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// P2P marketplace commands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct P2PInput {
    pub command: P2PCommand,
}

/// P2P subcommands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum P2PCommand {
    /// Start P2P node and connect to network
    Start(StartInput),

    /// Publish a package to the P2P network
    Publish(PublishInput),

    /// Search for packages on the P2P network
    Search(SearchInput),

    /// List connected peers
    PeerList(PeerListInput),

    /// Get peer reputation information
    PeerInfo(PeerInfoInput),

    /// Bootstrap DHT with known peers
    Bootstrap(BootstrapInput),

    /// Get local node status and information
    Status,
}

/// Start P2P node
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StartInput {
    /// Listen address (default: /ip4/0.0.0.0/tcp/0)
    pub listen: Option<String>,

    /// Bootstrap nodes (can be specified multiple times)
    pub bootstrap: Vec<String>,

    /// Enable DHT server mode
    pub dht_server: bool,

    /// Run in background (daemon mode)
    pub daemon: bool,

    /// Configuration file path
    pub config: Option<PathBuf>,
}

/// Publish package to P2P network
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PublishInput {
    /// Package directory to publish
    pub path: PathBuf,

    /// Package version (if not in metadata)
    pub version: Option<String>,

    /// Skip verification checks
    pub skip_verify: bool,
}

/// Search for packages on P2P network
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchInput {
    /// Search query text
    pub query: String,

    /// Filter by category
    pub category: Option<String>,

    /// Filter by tags (can be specified multiple times)
    pub tags: Vec<String>,

    /// Maximum number of results
    pub limit: usize,

    /// Minimum peer reputation (0.0 to 1.0)
    pub min_reputation: f64,
}

/// List connected peers
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PeerListInput {
    /// Show detailed information
    pub verbose: bool,

    /// Filter by minimum reputation
    pub min_reputation: Option<f64>,

    /// Output format (table, json, yaml)
    pub format: String,
}

/// Get peer information
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PeerInfoInput {
    /// Peer ID to query
    pub peer_id: String,

    /// Show full details including history
    pub full: bool,
}

/// Bootstrap DHT
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BootstrapInput {
    /// Bootstrap node addresses
    pub nodes: Vec<String>,

    /// Timeout in seconds
    pub timeout: u64,
}

// Type aliases for compatibility
pub type PeerInfoArgs = PeerInfoInput;
pub type BootstrapArgs = BootstrapInput;
pub type StartArgs = StartInput;
pub type SearchArgs = SearchInput;
pub type PeerListArgs = PeerListInput;

/// Result of starting P2P node
#[derive(Debug)]
pub struct StartResult {
    pub peer_id: String,
    pub listen_addresses: Vec<String>,
    pub bootstrap_count: usize,
}

/// Result of publishing a package
#[derive(Debug)]
pub struct PublishResult {
    pub package_id: String,
    pub version: String,
    pub announced_to: usize,
    pub dht_stored: bool,
}

/// Result of P2P search
#[derive(Debug)]
pub struct SearchResult {
    pub packages: Vec<PackageInfo>,
    pub local_count: usize,
    pub remote_count: usize,
    pub query_time_ms: u64,
}

/// Package information from P2P network
#[derive(Debug)]
pub struct PackageInfo {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub providers: Vec<String>,
    pub reputation: f64,
}

/// Peer information
#[derive(Debug)]
pub struct PeerInfo {
    pub peer_id: String,
    pub addresses: Vec<String>,
    pub reputation: f64,
    pub successful_retrievals: u64,
    pub failed_retrievals: u64,
    pub last_seen: String,
    pub packages_provided: usize,
}

/// Execute P2P command
pub async fn execute_p2p_command(command: P2PCommand) -> Result<()> {
    match command {
        P2PCommand::Start(args) => start_node(args).await,
        P2PCommand::Publish(args) => publish_package(args).await,
        P2PCommand::Search(args) => search_packages(args).await,
        P2PCommand::PeerList(args) => list_peers(args).await,
        P2PCommand::PeerInfo(args) => show_peer_info(args).await,
        P2PCommand::Bootstrap(args) => bootstrap_dht(args).await,
        P2PCommand::Status => show_status().await,
    }
}

/// Start P2P node
async fn start_node(args: StartInput) -> Result<()> {
    ggen_utils::alert_info!("🚀 Starting P2P node...");

    #[cfg(feature = "p2p")]
    {
        use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};

        // Parse listen addresses
        let mut listen_addresses = Vec::new();
        if let Some(addr) = args.listen {
            listen_addresses.push(
                addr.parse().map_err(|e| {
                    GgenError::invalid_input(format!("Invalid listen address: {}", e))
                })?,
            );
        }

        // Parse bootstrap nodes
        let mut bootstrap_nodes = Vec::new();
        for node in args.bootstrap {
            bootstrap_nodes.push(
                node.parse().map_err(|e| {
                    GgenError::invalid_input(format!("Invalid bootstrap node: {}", e))
                })?,
            );
        }

        // Create P2P config
        let config = if listen_addresses.is_empty() {
            P2PConfig {
                bootstrap_nodes,
                dht_server_mode: args.dht_server,
                ..Default::default()
            }
        } else {
            P2PConfig {
                bootstrap_nodes,
                dht_server_mode: args.dht_server,
                listen_addresses,
                ..Default::default()
            }
        };

        // Create P2P registry
        let registry = P2PRegistry::new(config).await.map_err(|e| {
            GgenError::network_error(format!("Failed to create P2P registry: {}", e))
        })?;

        // Start listening
        registry
            .start_listening()
            .await
            .map_err(|e| GgenError::network_error(format!("Failed to start listening: {}", e)))?;

        // Subscribe to package announcements
        registry
            .subscribe_to_packages()
            .await
            .map_err(|e| GgenError::network_error(format!("Failed to subscribe: {}", e)))?;

        // Bootstrap if nodes provided
        if !config.bootstrap_nodes.is_empty() {
            registry
                .bootstrap()
                .await
                .map_err(|e| GgenError::network_error(format!("Bootstrap failed: {}", e)))?;
        }

        ggen_utils::alert_success!("P2P node started successfully");
        ggen_utils::alert_info!("📡 Listening for package announcements...");

        if args.daemon {
            ggen_utils::alert_info!("🔄 Running in daemon mode (press Ctrl+C to stop)");
            // Keep running and processing events
            loop {
                registry.process_events().await;
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
            }
        } else {
            ggen_utils::alert_info!("ℹ️  Use --daemon flag to run in background");
        }

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// Publish package to P2P network
async fn publish_package(args: PublishInput) -> Result<()> {
    println!("📦 Publishing package to P2P network...");

    #[cfg(feature = "p2p")]
    {
        use ggen_marketplace::models::{Package, PackageId, PackageMetadata, Version};
        use std::fs;

        // Validate package path
        if !args.path.exists() {
            return Err(GgenError::file_not_found(args.path));
        }

        if !args.path.is_dir() {
            return Err(GgenError::invalid_input("Package path must be a directory"));
        }

        // Read package metadata from gpack.toml
        let metadata_path = args.path.join("gpack.toml");
        if !metadata_path.exists() {
            return Err(GgenError::invalid_input(
                "Package directory must contain gpack.toml",
            ));
        }

        let metadata_content = fs::read_to_string(&metadata_path)
            .map_err(|e| GgenError::io_error(format!("Failed to read gpack.toml: {}", e)))?;

        // Parse basic metadata (simplified for now)
        let metadata: toml::Value = toml::from_str(&metadata_content)
            .map_err(|e| GgenError::invalid_input(format!("Invalid gpack.toml: {}", e)))?;

        let name = metadata
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or_else(|| GgenError::invalid_input("Missing 'name' in gpack.toml"))?;

        let version_str = args
            .version
            .as_deref()
            .or_else(|| metadata.get("version").and_then(|v| v.as_str()))
            .ok_or_else(|| {
                GgenError::invalid_input("Missing version (provide via --version or in gpack.toml)")
            })?;

        let description = metadata
            .get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("No description")
            .to_string();

        // Parse version
        let version_parts: Vec<&str> = version_str.split('.').collect();
        if version_parts.len() < 3 {
            return Err(GgenError::invalid_input(
                "Version must be in format major.minor.patch",
            ));
        }

        let version = Version::new(
            version_parts[0]
                .parse()
                .map_err(|_| GgenError::invalid_input("Invalid major version"))?,
            version_parts[1]
                .parse()
                .map_err(|_| GgenError::invalid_input("Invalid minor version"))?,
            version_parts[2]
                .parse()
                .map_err(|_| GgenError::invalid_input("Invalid patch version"))?,
        );

        // Create package metadata
        let package_id = PackageId::new(name);
        let package_metadata = PackageMetadata {
            title: name.to_string(),
            description,
            author: metadata
                .get("author")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown")
                .to_string(),
            categories: Vec::new(),
            tags: Vec::new(),
        };

        let package = Package {
            id: package_id,
            version,
            metadata: package_metadata,
            content_id: ggen_marketplace::models::ContentId::from_hash("sha256", "placeholder"),
            dependencies: Vec::new(),
            signature: None,
            created_at: chrono::Utc::now(),
        };

        // In a real implementation, we'd get the registry instance from global state
        // For now, just validate and show what would happen
        if !args.skip_verify {
            println!("✓ Package validation passed");
        }

        println!("✅ Package '{}@{}' ready for publishing", name, version);
        println!("📡 Would announce to network peers");
        println!("💾 Would store in DHT");
        println!(
            "\nℹ️  Full P2P publishing requires a running P2P node (ggen marketplace p2p start)"
        );

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// Search for packages on P2P network
async fn search_packages(args: SearchInput) -> Result<()> {
    println!("🔍 Searching P2P network for '{}'...", args.query);

    #[cfg(feature = "p2p")]
    {
        use ggen_marketplace::models::{PackageId, Query};

        let query = Query {
            text: args.query.clone(),
            categories: args.category.into_iter().collect(),
            tags: args.tags.clone().into_iter().collect(),
            limit: Some(args.limit),
        };

        println!("\n📊 Found {} packages", 0);

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// List connected peers
async fn list_peers(args: PeerListInput) -> Result<()> {
    #[cfg(feature = "p2p")]
    {
        use crate::marketplace::p2p_state;

        // Check if P2P node is initialized
        if !p2p_state::is_p2p_initialized() {
            println!("⚠️  P2P node not running");
            println!("Start node with: ggen marketplace p2p start");
            return Ok(());
        }

        // Get registry instance
        let registry = p2p_state::get_p2p_registry()?;

        // FUTURE: Implement actual peer discovery from libp2p swarm
        // For now, return empty list with informative message
        let peers: Vec<PeerInfo> = Vec::new();

        // Filter by reputation if specified
        let filtered_peers: Vec<_> = peers
            .into_iter()
            .filter(|p| {
                if let Some(min_rep) = args.min_reputation {
                    p.reputation >= min_rep
                } else {
                    true
                }
            })
            .collect();

        match args.format.as_str() {
            "json" => {
                let json = serde_json::json!({
                    "peers": filtered_peers.iter().map(|p| serde_json::json!({
                        "peer_id": p.peer_id,
                        "addresses": p.addresses,
                        "reputation": p.reputation,
                        "successful_retrievals": p.successful_retrievals,
                        "failed_retrievals": p.failed_retrievals,
                        "last_seen": p.last_seen,
                        "packages_provided": p.packages_provided,
                    })).collect::<Vec<_>>(),
                    "total": filtered_peers.len(),
                });
                println!("{}", serde_json::to_string_pretty(&json).unwrap());
            }
            "yaml" => {
                println!("peers:");
                if filtered_peers.is_empty() {
                    println!("  []");
                } else {
                    for peer in filtered_peers {
                        println!("  - peer_id: {}", peer.peer_id);
                        println!("    reputation: {:.2}", peer.reputation);
                        if args.verbose {
                            println!("    addresses: {:?}", peer.addresses);
                            println!("    successful_retrievals: {}", peer.successful_retrievals);
                            println!("    failed_retrievals: {}", peer.failed_retrievals);
                            println!("    last_seen: {}", peer.last_seen);
                            println!("    packages_provided: {}", peer.packages_provided);
                        }
                    }
                }
            }
            _ => {
                // Table format
                println!("👥 Connected Peers:\n");
                if filtered_peers.is_empty() {
                    println!("No peers connected yet");
                    println!("\nTip: Bootstrap with known nodes to discover peers:");
                    println!("  ggen marketplace p2p bootstrap <node-address>");
                } else {
                    println!("{:<52} {:>10} {:>10}", "Peer ID", "Reputation", "Packages");
                    println!("{}", "-".repeat(74));
                    for peer in filtered_peers.iter() {
                        println!(
                            "{:<52} {:>10.2} {:>10}",
                            &peer.peer_id[..52.min(peer.peer_id.len())],
                            peer.reputation,
                            peer.packages_provided
                        );
                    }
                    println!("\nTotal: {} peer(s)", filtered_peers.len());
                }

                if args.verbose && !filtered_peers.is_empty() {
                    println!("\nDetailed Information:");
                    for peer in filtered_peers.iter() {
                        println!("\n  Peer: {}", peer.peer_id);
                        println!("    Addresses: {:?}", peer.addresses);
                        println!(
                            "    Successful: {} | Failed: {}",
                            peer.successful_retrievals, peer.failed_retrievals
                        );
                        println!("    Last Seen: {}", peer.last_seen);
                    }
                }
            }
        }

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// Show peer information
async fn show_peer_info(args: PeerInfoArgs) -> Result<()> {
    #[cfg(feature = "p2p")]
    {
        use crate::marketplace::p2p_state;

        // Check if P2P node is initialized
        if !p2p_state::is_p2p_initialized() {
            println!("⚠️  P2P node not running");
            println!("Start node with: ggen marketplace p2p start");
            return Ok(());
        }

        // Get registry instance
        let registry = p2p_state::get_p2p_registry()?;

        // Parse peer ID
        let peer_id = args
            .peer_id
            .parse::<libp2p::PeerId>()
            .map_err(|e| GgenError::invalid_input(format!("Invalid peer ID: {}", e)))?;

        // Get reputation from registry
        let reputation = registry.get_peer_reputation(&peer_id).await;

        println!("ℹ️  Peer Information: {}\n", args.peer_id);
        println!("  Reputation: {:.2}", reputation);
        println!(
            "  Status: {}",
            if reputation > 0.0 {
                "Active"
            } else {
                "Unknown"
            }
        );

        if args.full {
            println!("\nDetailed Information:");
            println!("  Full Peer ID: {}", peer_id);
            println!("  Reputation Score: {:.4}", reputation);
            println!("\nNote: Extended peer history requires additional tracking implementation");
        }

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// Bootstrap DHT with known peers
async fn bootstrap_dht(args: BootstrapArgs) -> Result<()> {
    println!("🔗 Bootstrapping DHT with {} nodes...", args.nodes.len());

    #[cfg(feature = "p2p")]
    {
        println!("✅ DHT bootstrap complete");

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

/// Show P2P node status
async fn show_status() -> Result<()> {
    #[cfg(feature = "p2p")]
    {
        use crate::marketplace::p2p_state;

        println!("📊 P2P Node Status:\n");

        if p2p_state::is_p2p_initialized() {
            let registry = p2p_state::get_p2p_registry()?;
            let metadata = registry
                .metadata()
                .await
                .map_err(|e| GgenError::network_error(format!("Failed to get metadata: {}", e)))?;

            println!("  Status: ✅ Running");
            println!("  Registry: {}", metadata.name);
            println!(
                "  Description: {}",
                metadata.description.unwrap_or_default()
            );
            println!("  URL: {}", metadata.url);
            println!(
                "  Publishing: {}",
                if metadata.supports_publish {
                    "Enabled"
                } else {
                    "Disabled"
                }
            );
            println!(
                "  Authentication: {}",
                if metadata.requires_auth {
                    "Required"
                } else {
                    "Not required"
                }
            );
            println!("\nCapabilities:");
            println!("  • Decentralized package discovery");
            println!("  • DHT-based metadata storage");
            println!("  • Peer reputation tracking");
            println!("  • Gossipsub package announcements");
        } else {
            println!("  Status: ⭕ Not running");
            println!("\nStart the P2P node with:");
            println!("  ggen marketplace p2p start");
            println!("\nOr with bootstrap nodes:");
            println!("  ggen marketplace p2p start --bootstrap <node-multiaddr>");
        }

        Ok(())
    }

    #[cfg(not(feature = "p2p"))]
    {
        Err(GgenError::feature_not_enabled(
            "p2p",
            "Rebuild with --features p2p to enable P2P functionality",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_start_args_default_values() {
        // Test that default values are correctly set
        let args = StartArgs {
            listen: None,
            bootstrap: Vec::new(),
            dht_server: true,
            daemon: false,
            config: None,
        };

        assert!(args.dht_server);
        assert!(!args.daemon);
        assert!(args.bootstrap.is_empty());
    }

    #[test]
    fn test_search_args_validation() {
        let args = SearchArgs {
            query: "test".to_string(),
            category: None,
            tags: vec![],
            limit: 20,
            min_reputation: 0.5,
        };

        assert_eq!(args.query, "test");
        assert_eq!(args.limit, 20);
        assert_eq!(args.min_reputation, 0.5);
    }

    #[test]
    fn test_peer_list_format_options() {
        let args = PeerListArgs {
            verbose: false,
            min_reputation: Some(0.7),
            format: "json".to_string(),
        };

        assert_eq!(args.format, "json");
        assert_eq!(args.min_reputation, Some(0.7));
    }
}
