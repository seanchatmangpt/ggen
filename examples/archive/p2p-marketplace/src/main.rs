//! P2P Marketplace Example
//!
//! Demonstrates how to use the P2P registry for package discovery and distribution

use anyhow::Result;
use clap::{Parser, Subcommand};
use ggen::p2p::{
    P2PRegistryBuilder, Registry, Package, Query, BootstrapNode,
};

#[derive(Parser)]
#[command(name = "p2p-marketplace")]
#[command(about = "P2P Marketplace Example", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Start a P2P node
    Start {
        /// Listen address
        #[arg(short, long, default_value = "/ip4/0.0.0.0/tcp/4001")]
        listen: String,

        /// Bootstrap nodes (format: peer_id@address)
        #[arg(short, long)]
        bootstrap: Vec<String>,
    },

    /// Publish a package
    Publish {
        /// Package name
        #[arg(short, long)]
        name: String,

        /// Package version
        #[arg(short, long)]
        version: String,

        /// Package category
        #[arg(short, long, default_value = "general")]
        category: String,

        /// Package tags (comma-separated)
        #[arg(short, long)]
        tags: Option<String>,
    },

    /// Search for packages
    Search {
        /// Search keywords (space-separated)
        keywords: Vec<String>,

        /// Filter by category
        #[arg(short, long)]
        category: Option<String>,

        /// Filter by tags (comma-separated)
        #[arg(short, long)]
        tags: Option<String>,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Get package details
    Get {
        /// Package ID
        package_id: String,
    },

    /// Show network statistics
    Stats,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Start { listen, bootstrap } => {
            start_node(listen, bootstrap).await?;
        }
        Commands::Publish { name, version, category, tags } => {
            publish_package(name, version, category, tags).await?;
        }
        Commands::Search { keywords, category, tags, limit } => {
            search_packages(keywords, category, tags, limit).await?;
        }
        Commands::Get { package_id } => {
            get_package(package_id).await?;
        }
        Commands::Stats => {
            show_stats().await?;
        }
    }

    Ok(())
}

async fn start_node(listen: String, bootstrap: Vec<String>) -> Result<()> {
    println!("🚀 Starting P2P node...");
    println!("📡 Listening on: {}", listen);

    // Parse bootstrap nodes
    let bootstrap_nodes: Vec<BootstrapNode> = bootstrap
        .iter()
        .filter_map(|s| {
            let parts: Vec<&str> = s.split('@').collect();
            if parts.len() == 2 {
                Some(BootstrapNode {
                    peer_id: parts[0].to_string(),
                    address: parts[1].to_string(),
                })
            } else {
                None
            }
        })
        .collect();

    println!("🔗 Bootstrap nodes: {}", bootstrap_nodes.len());

    // Create registry
    let registry = P2PRegistryBuilder::new()
        .with_listen_addresses(vec![listen])
        .with_bootstrap_nodes(bootstrap_nodes)
        .build()?;

    // Start network
    registry.start().await?;

    println!("✅ Node started successfully!");
    println!("Press Ctrl+C to stop...");

    // Keep running
    tokio::signal::ctrl_c().await?;

    println!("\n🛑 Stopping node...");
    registry.stop().await?;

    Ok(())
}

async fn publish_package(
    name: String,
    version: String,
    category: String,
    tags: Option<String>,
) -> Result<()> {
    println!("📦 Publishing package: {} v{}", name, version);

    // Create registry
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    // Create package
    let mut package = Package::new(name.clone(), version.clone());
    package.category = category;
    if let Some(tags_str) = tags {
        package.tags = tags_str.split(',').map(|s| s.trim().to_string()).collect();
    }

    // Publish
    registry.publish(package).await?;

    println!("✅ Package published successfully!");
    println!("⏳ Waiting for propagation...");

    // Wait for DHT propagation
    tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;

    let stats = registry.stats().await;
    println!("📊 Network stats:");
    println!("   Connected peers: {}", stats.connected_peers);
    println!("   Cached packages: {}", stats.cached_packages);

    registry.stop().await?;

    Ok(())
}

async fn search_packages(
    keywords: Vec<String>,
    category: Option<String>,
    tags: Option<String>,
    limit: usize,
) -> Result<()> {
    println!("🔍 Searching for packages...");
    println!("   Keywords: {:?}", keywords);

    // Create registry
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    // Create query
    let mut query = Query::new(keywords).with_limit(limit);

    if let Some(cat) = category {
        query = query.with_category(cat);
    }

    if let Some(tags_str) = tags {
        let tag_vec: Vec<String> = tags_str.split(',').map(|s| s.trim().to_string()).collect();
        query = query.with_tags(tag_vec);
    }

    // Search
    let results = registry.search(&query).await?;

    println!("\n📋 Found {} package(s):", results.len());
    println!();

    for result in results {
        println!("📦 {} v{}", result.package.name, result.package.version);
        println!("   Category: {}", result.package.category);
        println!("   Score: {:.2}", result.score);
        println!("   Providers: {}", result.peer_count);
        if !result.package.tags.is_empty() {
            println!("   Tags: {}", result.package.tags.join(", "));
        }
        println!();
    }

    registry.stop().await?;

    Ok(())
}

async fn get_package(package_id: String) -> Result<()> {
    println!("📦 Fetching package: {}", package_id);

    // Create registry
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    // Get package
    if let Some(package) = registry.get(&package_id).await? {
        println!("\n✅ Package found:");
        println!("   Name: {}", package.name);
        println!("   Version: {}", package.version);
        println!("   Category: {}", package.category);
        println!("   Description: {}", package.description);
        println!("   Author: {}", package.author);
        println!("   License: {}", package.license);
        println!("   Downloads: {}", package.downloads);
        println!("   Tags: {}", package.tags.join(", "));
    } else {
        println!("❌ Package not found");
    }

    registry.stop().await?;

    Ok(())
}

async fn show_stats() -> Result<()> {
    println!("📊 Network Statistics");

    // Create registry
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    let stats = registry.stats().await;

    println!("\n🌐 Network:");
    println!("   Connected peers: {}", stats.connected_peers);
    println!("   Total peers: {}", stats.total_peers);
    println!();

    println!("📦 Packages:");
    println!("   Cached packages: {}", stats.cached_packages);
    println!("   Provider records: {}", stats.provider_records);
    println!();

    println!("📡 Messaging:");
    println!("   Gossip topics: {}", stats.gossip_topics);

    registry.stop().await?;

    Ok(())
}
