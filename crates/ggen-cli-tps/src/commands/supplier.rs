use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Subcommands for managing suppliers in the TPS (Trusted Provider System).
///
/// This enum defines the available operations for supplier lifecycle management,
/// including registration, quality scoring, rate limiting, status checking, and
/// delivery tracking. Each command is dispatched through the `execute()` method
/// to its corresponding handler.
///
/// # Variants
///
/// - `Register`: Create a new supplier with initial quality score
/// - `Score`: Update a supplier's quality score with audit trail
/// - `RateLimit`: Configure token bucket rate limiting
/// - `Status`: Retrieve detailed supplier status and metrics
/// - `List`: View all suppliers with optional filtering
/// - `Deliver`: Record a delivery attempt and update metrics
#[derive(Debug, Subcommand)]
pub enum SupplierCommands {
    /// Register a new supplier
    Register {
        /// Supplier ID
        #[clap(short, long)]
        id: String,

        /// Supplier name
        #[clap(short, long)]
        name: String,

        /// Initial quality score (0-100)
        #[clap(short, long, default_value = "100")]
        quality_score: u8,
    },

    /// Update supplier quality score
    Score {
        /// Supplier ID
        #[clap(short, long)]
        id: String,

        /// Quality score (0-100)
        #[clap(short, long)]
        score: u8,

        /// Reason for update
        #[clap(short, long)]
        reason: String,
    },

    /// Set rate limit for supplier
    RateLimit {
        /// Supplier ID
        #[clap(short, long)]
        id: String,

        /// Requests per second
        #[clap(short, long)]
        rps: f64,

        /// Burst size
        #[clap(short, long, default_value = "10")]
        burst: usize,
    },

    /// Check supplier status
    Status {
        /// Supplier ID
        #[clap(short, long)]
        id: String,

        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// List all suppliers
    List {
        /// Filter by quality threshold
        #[clap(short, long)]
        min_quality: Option<u8>,

        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Record supplier delivery
    Deliver {
        /// Supplier ID
        #[clap(short, long)]
        id: String,

        /// Success status
        #[clap(short, long)]
        success: bool,

        /// Notes
        #[clap(short, long)]
        notes: Option<String>,
    },
}

/// Specifies the output format for supplier commands.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    /// JSON format output.
    Json,
    /// Human-readable text format output.
    Text,
}

/// Represents a supplier with operational metrics and status.
#[derive(Debug, Serialize, Deserialize)]
pub struct Supplier {
    /// Unique identifier for the supplier.
    pub id: String,
    /// Display name of the supplier.
    pub name: String,
    /// Quality score from 0-100 reflecting supplier reliability.
    pub quality_score: u8,
    /// Rate limiting configuration for this supplier.
    pub rate_limit: RateLimit,
    /// Aggregated delivery statistics.
    pub deliveries: DeliveryStats,
    /// Timestamp of the last update (RFC 3339 format).
    pub last_update: String,
    /// Current operational status.
    pub status: SupplierStatus,
}

/// Token bucket rate limiting configuration for a supplier.
#[derive(Debug, Serialize, Deserialize)]
pub struct RateLimit {
    /// Sustained rate in requests per second.
    pub requests_per_second: f64,
    /// Burst capacity in number of requests.
    pub burst: usize,
    /// Current number of available tokens.
    pub current_tokens: f64,
    /// Timestamp of the last token refill (RFC 3339 format).
    pub last_refill: String,
}

/// Aggregated delivery performance metrics for a supplier.
#[derive(Debug, Serialize, Deserialize)]
pub struct DeliveryStats {
    /// Total number of delivery attempts.
    pub total: usize,
    /// Number of successful deliveries.
    pub successful: usize,
    /// Number of failed deliveries.
    pub failed: usize,
    /// Success rate as a fraction between 0.0 and 1.0.
    pub success_rate: f64,
}

/// Operational status of a supplier.
#[derive(Debug, Serialize, Deserialize)]
pub enum SupplierStatus {
    /// Supplier is operating normally.
    Active,
    /// Supplier has degraded performance; quality score 50-79.
    Warning,
    /// Supplier is suspended; quality score below 50.
    Suspended,
}

/// Registry that manages a collection of suppliers and their operational state.
#[derive(Debug, Serialize, Deserialize)]
pub struct SupplierRegistry {
    /// Collection of suppliers tracked in the registry.
    pub suppliers: Vec<Supplier>,
}

impl SupplierCommands {
    /// Executes the supplier command and dispatches to the appropriate handler based on the subcommand.
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Register {
                id,
                name,
                quality_score,
            } => Self::register_supplier(id, name, quality_score).await,
            Self::Score { id, score, reason } => Self::update_score(id, score, reason).await,
            Self::RateLimit { id, rps, burst } => Self::set_rate_limit(id, rps, burst).await,
            Self::Status { id, format } => Self::check_status(id, format).await,
            Self::List {
                min_quality,
                format,
            } => Self::list_suppliers(min_quality, format).await,
            Self::Deliver { id, success, notes } => Self::record_delivery(id, success, notes).await,
        }
    }

    async fn register_supplier(id: String, name: String, quality_score: u8) -> Result<()> {
        if quality_score > 100 {
            return Err(CliError::Validation(
                "Quality score must be 0-100".to_string(),
            ));
        }

        let mut registry = Self::load_registry().await?;

        if registry.suppliers.iter().any(|s| s.id == id) {
            return Err(CliError::Validation(format!(
                "Supplier {} already exists",
                id
            )));
        }

        let supplier = Supplier {
            id: id.clone(),
            name: name.clone(),
            quality_score,
            rate_limit: RateLimit {
                requests_per_second: 10.0,
                burst: 10,
                current_tokens: 10.0,
                last_refill: chrono::Utc::now().to_rfc3339(),
            },
            deliveries: DeliveryStats {
                total: 0,
                successful: 0,
                failed: 0,
                success_rate: 0.0,
            },
            last_update: chrono::Utc::now().to_rfc3339(),
            status: SupplierStatus::Active,
        };

        registry.suppliers.push(supplier);
        Self::save_registry(&registry).await?;

        println!("✓ Supplier registered: {}", name);
        println!("  ID: {}", id);
        println!("  Quality Score: {}", quality_score);

        Ok(())
    }

    async fn update_score(id: String, score: u8, reason: String) -> Result<()> {
        if score > 100 {
            return Err(CliError::Validation(
                "Quality score must be 0-100".to_string(),
            ));
        }

        let mut registry = Self::load_registry().await?;

        {
            let supplier = registry
                .suppliers
                .iter_mut()
                .find(|s| s.id == id)
                .ok_or_else(|| CliError::Validation(format!("Supplier {} not found", id)))?;

            supplier.quality_score = score;
            supplier.last_update = chrono::Utc::now().to_rfc3339();

            supplier.status = if score < 50 {
                SupplierStatus::Suspended
            } else if score < 80 {
                SupplierStatus::Warning
            } else {
                SupplierStatus::Active
            };
        }

        Self::save_registry(&registry).await?;

        println!("✓ Quality score updated for {}", id);
        println!("  New Score: {}", score);
        println!("  Reason: {}", reason);

        Ok(())
    }

    async fn set_rate_limit(id: String, rps: f64, burst: usize) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let supplier = registry
            .suppliers
            .iter_mut()
            .find(|s| s.id == id)
            .ok_or_else(|| CliError::Validation(format!("Supplier {} not found", id)))?;

        supplier.rate_limit.requests_per_second = rps;
        supplier.rate_limit.burst = burst;
        supplier.rate_limit.current_tokens = burst as f64;
        supplier.last_update = chrono::Utc::now().to_rfc3339();

        Self::save_registry(&registry).await?;

        println!("✓ Rate limit set for {}", id);
        println!("  RPS: {}", rps);
        println!("  Burst: {}", burst);

        Ok(())
    }

    async fn check_status(id: String, format: OutputFormat) -> Result<()> {
        let registry = Self::load_registry().await?;

        let supplier = registry
            .suppliers
            .iter()
            .find(|s| s.id == id)
            .ok_or_else(|| CliError::Validation(format!("Supplier {} not found", id)))?;

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&supplier)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Supplier: {} ({})", supplier.name, supplier.id);
                println!("========");
                println!("Quality Score: {}", supplier.quality_score);
                println!("Status: {:?}", supplier.status);
                println!(
                    "Rate Limit: {}/sec (burst: {})",
                    supplier.rate_limit.requests_per_second, supplier.rate_limit.burst
                );
                println!("Deliveries: {}", supplier.deliveries.total);
                println!(
                    "Success Rate: {:.1}%",
                    supplier.deliveries.success_rate * 100.0
                );
                println!("Last Update: {}", supplier.last_update);
            }
        }

        Ok(())
    }

    async fn list_suppliers(min_quality: Option<u8>, format: OutputFormat) -> Result<()> {
        let registry = Self::load_registry().await?;

        let suppliers: Vec<_> = registry
            .suppliers
            .iter()
            .filter(|s| {
                if let Some(min) = min_quality {
                    s.quality_score >= min
                } else {
                    true
                }
            })
            .collect();

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&suppliers)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Suppliers ({})", suppliers.len());
                println!("==========");
                for supplier in suppliers {
                    println!(
                        "{} ({}): Q={} {:?} Deliveries={}",
                        supplier.name,
                        supplier.id,
                        supplier.quality_score,
                        supplier.status,
                        supplier.deliveries.total
                    );
                }
            }
        }

        Ok(())
    }

    async fn record_delivery(id: String, success: bool, notes: Option<String>) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let (quality_score, success_rate) = {
            let supplier = registry
                .suppliers
                .iter_mut()
                .find(|s| s.id == id)
                .ok_or_else(|| CliError::Validation(format!("Supplier {} not found", id)))?;

            supplier.deliveries.total += 1;
            if success {
                supplier.deliveries.successful += 1;
            } else {
                supplier.deliveries.failed += 1;
            }

            supplier.deliveries.success_rate =
                supplier.deliveries.successful as f64 / supplier.deliveries.total as f64;

            supplier.last_update = chrono::Utc::now().to_rfc3339();

            let quality_adjustment = if success { 1 } else { -5 };
            supplier.quality_score =
                (supplier.quality_score as i16 + quality_adjustment).clamp(0, 100) as u8;

            supplier.status = if supplier.quality_score < 50 {
                SupplierStatus::Suspended
            } else if supplier.quality_score < 80 {
                SupplierStatus::Warning
            } else {
                SupplierStatus::Active
            };

            (supplier.quality_score, supplier.deliveries.success_rate)
        };

        Self::save_registry(&registry).await?;

        println!(
            "✓ Delivery recorded for {}: {}",
            id,
            if success { "SUCCESS" } else { "FAILED" }
        );
        println!("  Quality Score: {}", quality_score);
        println!("  Success Rate: {:.1}%", success_rate * 100.0);
        if let Some(n) = notes {
            println!("  Notes: {}", n);
        }

        Ok(())
    }

    async fn load_registry() -> Result<SupplierRegistry> {
        let path = Self::registry_path();
        if !path.exists() {
            return Ok(SupplierRegistry {
                suppliers: Vec::new(),
            });
        }

        let content = tokio::fs::read_to_string(&path).await?;
        Ok(serde_json::from_str(&content)?)
    }

    async fn save_registry(registry: &SupplierRegistry) -> Result<()> {
        let path = Self::registry_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let json = serde_json::to_string_pretty(registry)?;
        tokio::fs::write(&path, json).await?;
        Ok(())
    }

    fn registry_path() -> PathBuf {
        PathBuf::from(".ggen/suppliers.json")
    }
}
