use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Subcommand)]
pub enum PacketCommands {
    /// Validate a work order packet
    Validate {
        /// Packet file to validate
        #[clap(short, long)]
        packet: PathBuf,

        /// Strict validation mode
        #[clap(short, long)]
        strict: bool,
    },

    /// Route a packet to destination
    Route {
        /// Packet to route
        #[clap(short, long)]
        packet: PathBuf,

        /// Destination
        #[clap(short, long)]
        destination: String,

        /// Priority (1-10)
        #[clap(long, default_value = "5")]
        priority: u8,
    },

    /// Create new work order packet
    Create {
        /// Work order type
        #[clap(short = 't', long)]
        order_type: String,

        /// Payload JSON
        #[clap(short, long)]
        payload: String,

        /// Output file
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Inspect packet contents
    Inspect {
        /// Packet file
        #[clap(short, long)]
        packet: PathBuf,

        /// Show full payload
        #[clap(short, long)]
        full: bool,
    },

    /// Track packet status
    Track {
        /// Packet ID
        #[clap(short, long)]
        packet_id: String,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WorkPacket {
    pub packet_id: String,
    pub order_type: String,
    pub payload: serde_json::Value,
    pub priority: u8,
    pub timestamp: String,
    pub source: String,
    pub destination: Option<String>,
    pub status: PacketStatus,
    pub routing_history: Vec<RoutingEntry>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PacketStatus {
    Created,
    Routed,
    InProgress,
    Completed,
    Failed,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RoutingEntry {
    pub timestamp: String,
    pub from: String,
    pub to: String,
    pub status: String,
}

impl PacketCommands {
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Validate { packet, strict } => Self::validate_packet(&packet, strict).await,
            Self::Route {
                packet,
                destination,
                priority,
            } => Self::route_packet(&packet, destination, priority).await,
            Self::Create {
                order_type,
                payload,
                output,
            } => Self::create_packet(order_type, payload, output).await,
            Self::Inspect { packet, full } => Self::inspect_packet(&packet, full).await,
            Self::Track { packet_id } => Self::track_packet(packet_id).await,
        }
    }

    async fn validate_packet(path: &PathBuf, strict: bool) -> Result<()> {
        let content = tokio::fs::read_to_string(path).await?;
        let packet: WorkPacket = serde_json::from_str(&content)?;

        if packet.order_type.is_empty() {
            return Err(CliError::Validation("Order type is required".to_string()));
        }

        if packet.priority > 10 {
            return Err(CliError::Validation(
                "Priority must be 1-10".to_string(),
            ));
        }

        if strict {
            if packet.packet_id.is_empty() {
                return Err(CliError::Validation("Packet ID is required".to_string()));
            }

            if packet.source.is_empty() {
                return Err(CliError::Validation("Source is required".to_string()));
            }
        }

        println!("✓ Packet validation passed: {}", path.display());
        println!("  Type: {}", packet.order_type);
        println!("  Priority: {}", packet.priority);
        println!("  Status: {:?}", packet.status);

        Ok(())
    }

    async fn route_packet(path: &PathBuf, destination: String, priority: u8) -> Result<()> {
        let content = tokio::fs::read_to_string(path).await?;
        let mut packet: WorkPacket = serde_json::from_str(&content)?;

        if priority > 10 {
            return Err(CliError::Validation(
                "Priority must be 1-10".to_string(),
            ));
        }

        let routing_entry = RoutingEntry {
            timestamp: chrono::Utc::now().to_rfc3339(),
            from: packet.source.clone(),
            to: destination.clone(),
            status: "routed".to_string(),
        };

        packet.destination = Some(destination.clone());
        packet.priority = priority;
        packet.status = PacketStatus::Routed;
        packet.routing_history.push(routing_entry);

        let json = serde_json::to_string_pretty(&packet)?;
        tokio::fs::write(path, json).await?;

        println!("✓ Packet routed to: {}", destination);
        println!("  Packet ID: {}", packet.packet_id);
        println!("  Priority: {}", priority);

        Ok(())
    }

    async fn create_packet(order_type: String, payload: String, output: PathBuf) -> Result<()> {
        let packet_id = format!("pkt-{}", chrono::Utc::now().timestamp());
        let timestamp = chrono::Utc::now().to_rfc3339();

        let payload_value: serde_json::Value = serde_json::from_str(&payload)?;

        let packet = WorkPacket {
            packet_id: packet_id.clone(),
            order_type,
            payload: payload_value,
            priority: 5,
            timestamp,
            source: "cli".to_string(),
            destination: None,
            status: PacketStatus::Created,
            routing_history: Vec::new(),
        };

        let json = serde_json::to_string_pretty(&packet)?;
        tokio::fs::write(&output, json).await?;

        println!("✓ Work packet created: {}", output.display());
        println!("  Packet ID: {}", packet_id);

        Ok(())
    }

    async fn inspect_packet(path: &PathBuf, full: bool) -> Result<()> {
        let content = tokio::fs::read_to_string(path).await?;
        let packet: WorkPacket = serde_json::from_str(&content)?;

        println!("Work Packet: {}", packet.packet_id);
        println!("============");
        println!("Type: {}", packet.order_type);
        println!("Status: {:?}", packet.status);
        println!("Priority: {}", packet.priority);
        println!("Source: {}", packet.source);
        if let Some(ref dest) = packet.destination {
            println!("Destination: {}", dest);
        }
        println!("Timestamp: {}", packet.timestamp);

        if !packet.routing_history.is_empty() {
            println!("\nRouting History:");
            for entry in &packet.routing_history {
                println!("  {} -> {} [{}]", entry.from, entry.to, entry.status);
            }
        }

        if full {
            println!("\nPayload:");
            println!("{}", serde_json::to_string_pretty(&packet.payload)?);
        }

        Ok(())
    }

    async fn track_packet(packet_id: String) -> Result<()> {
        let tracking_path = PathBuf::from(".ggen/packets");

        if !tracking_path.exists() {
            return Err(CliError::PacketRouting(
                "Tracking directory does not exist".to_string(),
            ));
        }

        let mut found = false;
        let mut entries = tokio::fs::read_dir(&tracking_path).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();
            if let Some(filename) = path.file_name().and_then(|n| n.to_str()) {
                if filename.starts_with(&packet_id) && filename.ends_with(".json") {
                    let content = tokio::fs::read_to_string(&path).await?;
                    let packet: WorkPacket = serde_json::from_str(&content)?;

                    println!("Packet: {}", packet.packet_id);
                    println!("Status: {:?}", packet.status);
                    println!("Last Update: {}", packet.timestamp);

                    if !packet.routing_history.is_empty() {
                        println!("Route:");
                        for entry in &packet.routing_history {
                            println!("  {} → {} ({})", entry.from, entry.to, entry.timestamp);
                        }
                    }

                    found = true;
                    break;
                }
            }
        }

        if !found {
            return Err(CliError::PacketRouting(format!(
                "Packet {} not found",
                packet_id
            )));
        }

        Ok(())
    }
}
