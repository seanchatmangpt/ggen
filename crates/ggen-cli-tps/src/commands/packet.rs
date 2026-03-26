use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// CLI subcommands for managing work order packets in the TPS system.
///
/// This enum defines all packet-related operations including validation, routing,
/// creation, inspection, and status tracking. Each variant corresponds to a specific
/// command that can be executed through the CLI.
#[derive(Debug, Subcommand)]
pub enum PacketCommands {
    /// Validate a work order packet against schema and constraints.
    ///
    /// Performs validation of a packet file, optionally checking for required fields
    /// like packet ID and source in strict mode. Validates that order type is non-empty
    /// and priority is in the valid range (1-10).
    Validate {
        /// Path to the packet file to validate
        #[clap(short, long)]
        packet: PathBuf,

        /// Enable strict validation mode that enforces all required fields
        #[clap(short, long)]
        strict: bool,
    },

    /// Route a packet to a destination with specified priority.
    ///
    /// Updates a packet's destination, priority, and status to "Routed", while
    /// recording the routing action in the packet's routing history with a timestamp.
    Route {
        /// Path to the packet file to route
        #[clap(short, long)]
        packet: PathBuf,

        /// Destination for the packet
        #[clap(short, long)]
        destination: String,

        /// Priority level for the packet (1-10, default: 5)
        #[clap(long, default_value = "5")]
        priority: u8,
    },

    /// Create a new work order packet with the specified type and payload.
    ///
    /// Generates a new work packet with an auto-generated packet ID (based on current
    /// timestamp), initializes its status to "Created", and writes it to the output file
    /// in JSON format.
    Create {
        /// Type of work order to create
        #[clap(short = 't', long)]
        order_type: String,

        /// Payload data as a JSON string
        #[clap(short, long)]
        payload: String,

        /// Output file path where the packet will be saved
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Inspect and display packet contents.
    ///
    /// Reads and displays packet metadata including ID, type, status, priority, source,
    /// destination, and routing history. Optionally displays the full JSON payload.
    Inspect {
        /// Path to the packet file to inspect
        #[clap(short, long)]
        packet: PathBuf,

        /// Display the complete payload in JSON format
        #[clap(short, long)]
        full: bool,
    },

    /// Track the status and history of a packet by ID.
    ///
    /// Searches the packet tracking directory for a packet matching the given ID
    /// and displays its current status and routing history.
    Track {
        /// Packet ID to track
        #[clap(short, long)]
        packet_id: String,
    },
}

/// Represents a work order packet in the TPS system.
///
/// A work packet encapsulates a unit of work to be processed through the system,
/// including metadata for routing, tracking, and status management. Packets are
/// serialized to JSON for storage and transmission.
#[derive(Debug, Serialize, Deserialize)]
pub struct WorkPacket {
    /// Unique identifier for this packet (format: "pkt-{timestamp}")
    pub packet_id: String,

    /// Type of work order this packet contains
    pub order_type: String,

    /// Arbitrary payload data associated with this packet
    pub payload: serde_json::Value,

    /// Priority level for processing (1-10, where 10 is highest priority)
    pub priority: u8,

    /// RFC 3339 timestamp when the packet was created
    pub timestamp: String,

    /// Source system or component that created this packet
    pub source: String,

    /// Optional destination where this packet should be routed
    pub destination: Option<String>,

    /// Current processing status of the packet
    pub status: PacketStatus,

    /// Historical log of all routing actions applied to this packet
    pub routing_history: Vec<RoutingEntry>,
}

/// Enumeration of possible packet processing states.
///
/// Represents the lifecycle of a work packet from creation through completion or failure.
#[derive(Debug, Serialize, Deserialize)]
pub enum PacketStatus {
    /// Packet has been created but not yet routed
    Created,

    /// Packet has been routed to a destination
    Routed,

    /// Packet is currently being processed
    InProgress,

    /// Packet processing completed successfully
    Completed,

    /// Packet processing failed
    Failed,
}

/// Records a single routing operation applied to a packet.
///
/// Represents a historical entry in the packet's routing log, tracking how the packet
/// moved through the system and the status of that movement.
#[derive(Debug, Serialize, Deserialize)]
pub struct RoutingEntry {
    /// RFC 3339 timestamp of when this routing action occurred
    pub timestamp: String,

    /// Source of this routing step
    pub from: String,

    /// Destination of this routing step
    pub to: String,

    /// Status of the routing action (e.g., "routed", "delivered")
    pub status: String,
}

impl PacketCommands {
    /// Execute the packet command asynchronously.
    ///
    /// Dispatches to the appropriate handler method based on the command variant.
    /// Each handler performs the specific operation (validate, route, create, inspect, or track).
    ///
    /// # Errors
    ///
    /// Returns a [`CliError`] if the command execution fails, including:
    /// - IO errors when reading or writing files
    /// - JSON parsing errors
    /// - Validation errors for invalid packet data
    /// - File not found errors for tracking operations
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
            return Err(CliError::Validation("Priority must be 1-10".to_string()));
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
            return Err(CliError::Validation("Priority must be 1-10".to_string()));
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
