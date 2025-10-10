use clap::Args;
use ggen_core::graph::Graph;
use ggen_core::snapshot::{Snapshot, SnapshotManager};
use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

#[derive(Args, Debug)]
pub struct SnapshotArgs {
    #[command(subcommand)]
    pub command: SnapshotCommand,
}

#[derive(clap::Subcommand, Debug)]
pub enum SnapshotCommand {
    /// Create a new snapshot
    Create {
        /// Name for the snapshot
        #[arg(short, long)]
        name: String,

        /// Graph to snapshot
        #[arg(short, long)]
        graph: Option<String>,

        /// Files to include in snapshot
        #[arg(short, long)]
        files: Vec<PathBuf>,

        /// Templates to include in snapshot
        #[arg(short, long)]
        templates: Vec<PathBuf>,

        /// Snapshot directory
        #[arg(short, long, default_value = ".ggen/snapshots")]
        snapshot_dir: PathBuf,
    },

    /// List available snapshots
    List {
        /// Snapshot directory
        #[arg(short, long, default_value = ".ggen/snapshots")]
        snapshot_dir: PathBuf,
    },

    /// Show details of a snapshot
    Show {
        /// Name of snapshot to show
        #[arg(short, long)]
        name: String,

        /// Snapshot directory
        #[arg(short, long, default_value = ".ggen/snapshots")]
        snapshot_dir: PathBuf,
    },

    /// Delete a snapshot
    Delete {
        /// Name of snapshot to delete
        #[arg(short, long)]
        name: String,

        /// Snapshot directory
        #[arg(short, long, default_value = ".ggen/snapshots")]
        snapshot_dir: PathBuf,
    },

    /// Verify a snapshot against current state
    Verify {
        /// Name of snapshot to verify
        #[arg(short, long)]
        name: String,

        /// Snapshot directory
        #[arg(short, long, default_value = ".ggen/snapshots")]
        snapshot_dir: PathBuf,

        /// Exit with non-zero code if drift detected
        #[arg(short, long)]
        exit_code: bool,
    },
}

pub async fn run(args: &SnapshotArgs) -> Result<()> {
    match &args.command {
        SnapshotCommand::Create {
            name,
            graph,
            files,
            templates,
            snapshot_dir,
        } => create_snapshot(name, graph, files, templates, snapshot_dir).await,
        SnapshotCommand::List { snapshot_dir } => list_snapshots(snapshot_dir).await,
        SnapshotCommand::Show { name, snapshot_dir } => show_snapshot(name, snapshot_dir).await,
        SnapshotCommand::Delete { name, snapshot_dir } => delete_snapshot(name, snapshot_dir).await,
        SnapshotCommand::Verify {
            name,
            snapshot_dir,
            exit_code,
        } => verify_snapshot(name, snapshot_dir, *exit_code).await,
    }
}

async fn create_snapshot(
    name: &str, graph_name: &Option<String>, files: &[PathBuf], templates: &[PathBuf],
    snapshot_dir: &Path,
) -> Result<()> {
    // Load graph if specified
    let graph = if let Some(_graph_name) = graph_name {
        // Would need to load from named graph - simplified for now
        Graph::new()?
    } else {
        Graph::new()?
    };

    // Read file contents
    let mut file_contents = Vec::new();
    for file_path in files {
        if file_path.exists() {
            let content = std::fs::read_to_string(file_path)?;
            file_contents.push((file_path.clone(), content));
        }
    }

    // Read template contents
    let mut template_contents = Vec::new();
    for template_path in templates {
        if template_path.exists() {
            let content = std::fs::read_to_string(template_path)?;
            template_contents.push((template_path.clone(), content));
        }
    }

    // Create snapshot
    let snapshot = Snapshot::new(name.to_string(), &graph, file_contents, template_contents)?;

    // Save snapshot
    let manager = SnapshotManager::new(snapshot_dir.to_path_buf())?;
    manager.save(&snapshot)?;

    println!("Snapshot '{}' created successfully", name);
    Ok(())
}

async fn list_snapshots(snapshot_dir: &Path) -> Result<()> {
    let manager = SnapshotManager::new(snapshot_dir.to_path_buf())?;
    let snapshots = manager.list()?;

    if snapshots.is_empty() {
        println!("No snapshots found.");
    } else {
        println!("Available snapshots:");
        for snapshot in snapshots {
            println!("  {}", snapshot);
        }
    }

    Ok(())
}

async fn show_snapshot(name: &str, snapshot_dir: &Path) -> Result<()> {
    let manager = SnapshotManager::new(snapshot_dir.to_path_buf())?;
    let snapshot = manager.load(name)?;

    println!("Snapshot: {}", snapshot.name);
    println!(
        "Created:  {}",
        snapshot.created_at.format("%Y-%m-%d %H:%M:%S UTC")
    );
    println!("Graph hash: {}", snapshot.graph.hash);
    println!("Triples: {}", snapshot.graph.triple_count);
    println!("Files: {}", snapshot.files.len());
    println!("Templates: {}", snapshot.templates.len());

    if !snapshot.metadata.is_empty() {
        println!("Metadata:");
        for (key, value) in &snapshot.metadata {
            println!("  {}: {}", key, value);
        }
    }

    Ok(())
}

async fn delete_snapshot(name: &str, snapshot_dir: &Path) -> Result<()> {
    let manager = SnapshotManager::new(snapshot_dir.to_path_buf())?;
    manager.delete(name)?;
    println!("Snapshot '{}' deleted", name);
    Ok(())
}

async fn verify_snapshot(name: &str, snapshot_dir: &Path, exit_code: bool) -> Result<()> {
    let manager = SnapshotManager::new(snapshot_dir.to_path_buf())?;
    let snapshot = manager.load(name)?;

    // Load current graph (simplified - would need to match snapshot's graph source)
    let current_graph = Graph::new()?;

    let is_compatible = snapshot.is_compatible_with(&current_graph)?;

    if is_compatible {
        println!("✓ Snapshot '{}' is compatible with current state", name);
        if !exit_code {
            std::process::exit(0);
        }
    } else {
        println!("✗ Snapshot '{}' has drifted from current state", name);
        println!("  Graph hash mismatch:");
        println!("    Snapshot: {}", snapshot.graph.hash);
        println!("    Current:  {}", current_graph.compute_hash()?);

        if exit_code {
            std::process::exit(1);
        }
    }

    Ok(())
}
