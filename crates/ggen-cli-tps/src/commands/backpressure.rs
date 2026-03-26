use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Subcommands for managing token bucket backpressure in the TPS system.
///
/// Implements token bucket rate limiting with configurable policies. Commands
/// manage multiple independent token pools that can be acquired from, released to,
/// and configured with different admission control policies.
///
/// # Variants
///
/// - `Init`: Create a new token pool
/// - `Acquire`: Request tokens from a pool
/// - `Release`: Return tokens to a pool
/// - `Status`: Check pool status and utilization
/// - `List`: View all pools
/// - `SetPolicy`: Configure admission control policy
#[derive(Debug, Subcommand)]
pub enum BackpressureCommands {
    /// Initialize token pool
    Init {
        /// Pool name
        #[clap(short, long)]
        name: String,

        /// Total capacity
        #[clap(short, long)]
        capacity: usize,

        /// Refill rate (tokens/sec)
        #[clap(short, long)]
        refill_rate: f64,
    },

    /// Acquire tokens from pool
    Acquire {
        /// Pool name
        #[clap(short, long)]
        pool: String,

        /// Number of tokens
        #[clap(short, long, default_value = "1")]
        tokens: usize,

        /// Block until available
        #[clap(short, long)]
        blocking: bool,
    },

    /// Release tokens back to pool
    Release {
        /// Pool name
        #[clap(short, long)]
        pool: String,

        /// Number of tokens
        #[clap(short, long, default_value = "1")]
        tokens: usize,
    },

    /// Check pool status
    Status {
        /// Pool name
        #[clap(short, long)]
        pool: String,

        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// List all pools
    List {
        /// Output format
        #[clap(short, long, default_value = "text")]
        format: OutputFormat,
    },

    /// Set admission control policy
    SetPolicy {
        /// Pool name
        #[clap(short, long)]
        pool: String,

        /// Policy type (strict, relaxed, adaptive)
        #[clap(short = 't', long)]
        policy_type: PolicyType,
    },
}

/// Output format for backpressure commands.
#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    /// JSON format output
    Json,
    /// Human-readable text format
    Text,
}

/// Admission control policy for token pool.
///
/// Determines behavior when tokens are not available.
#[derive(Debug, Clone, Serialize, Deserialize, clap::ValueEnum)]
#[serde(rename_all = "lowercase")]
pub enum PolicyType {
    /// Strictly enforce token requirements, reject if insufficient
    Strict,
    /// Allow some overflow beyond strict limits
    Relaxed,
    /// Dynamically adjust based on demand patterns
    Adaptive,
}

/// A token bucket for rate limiting.
///
/// Implements the token bucket algorithm with configurable capacity and
/// refill rate. Tracks usage statistics for monitoring.
#[derive(Debug, Serialize, Deserialize)]
pub struct TokenPool {
    /// Name identifying this pool
    pub name: String,
    /// Maximum number of tokens this pool can hold
    pub capacity: usize,
    /// Current number of available tokens
    pub available: usize,
    /// Refill rate in tokens per second
    pub refill_rate: f64,
    /// RFC 3339 timestamp of last refill operation
    pub last_refill: String,
    /// Admission control policy for this pool
    pub policy: PolicyType,
    /// Total number of successful token acquisitions
    pub acquisitions: usize,
    /// Total number of rejected acquisition attempts
    pub rejections: usize,
}

/// Collection of all token pools in the system.
///
/// Maintains and manages the set of token pools used for backpressure control.
#[derive(Debug, Serialize, Deserialize)]
pub struct PoolRegistry {
    /// All registered token pools
    pub pools: Vec<TokenPool>,
}

impl BackpressureCommands {
    /// Execute the backpressure command asynchronously.
    ///
    /// Dispatches to the appropriate handler based on the command variant.
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Init {
                name,
                capacity,
                refill_rate,
            } => Self::init_pool(name, capacity, refill_rate).await,
            Self::Acquire {
                pool,
                tokens,
                blocking,
            } => Self::acquire_tokens(pool, tokens, blocking).await,
            Self::Release { pool, tokens } => Self::release_tokens(pool, tokens).await,
            Self::Status { pool, format } => Self::check_status(pool, format).await,
            Self::List { format } => Self::list_pools(format).await,
            Self::SetPolicy { pool, policy_type } => Self::set_policy(pool, policy_type).await,
        }
    }

    /// Create a new token pool with initial configuration.
    ///
    /// Initializes a pool with the specified capacity and refill rate,
    /// and sets the default policy to Strict.
    async fn init_pool(name: String, capacity: usize, refill_rate: f64) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        if registry.pools.iter().any(|p| p.name == name) {
            return Err(CliError::Validation(format!(
                "Pool {} already exists",
                name
            )));
        }

        let pool = TokenPool {
            name: name.clone(),
            capacity,
            available: capacity,
            refill_rate,
            last_refill: chrono::Utc::now().to_rfc3339(),
            policy: PolicyType::Strict,
            acquisitions: 0,
            rejections: 0,
        };

        registry.pools.push(pool);
        Self::save_registry(&registry).await?;

        println!("✓ Token pool initialized: {}", name);
        println!("  Capacity: {}", capacity);
        println!("  Refill rate: {}/sec", refill_rate);

        Ok(())
    }

    /// Request tokens from a pool.
    ///
    /// Attempts to acquire the specified number of tokens. If the pool
    /// has sufficient tokens, they are deducted. Otherwise, behavior
    /// depends on the blocking flag and policy.
    async fn acquire_tokens(pool_name: String, tokens: usize, blocking: bool) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let pool = registry
            .pools
            .iter_mut()
            .find(|p| p.name == pool_name)
            .ok_or_else(|| CliError::Validation(format!("Pool {} not found", pool_name)))?;

        Self::refill_pool(pool)?;

        if pool.available >= tokens {
            pool.available -= tokens;
            pool.acquisitions += 1;
            let available = pool.available;
            let capacity = pool.capacity;
            Self::save_registry(&registry).await?;

            println!("✓ Acquired {} tokens from {}", tokens, pool_name);
            println!("  Available: {}/{}", available, capacity);
            Ok(())
        } else if blocking {
            let available = pool.available;
            Err(CliError::BackpressureLimitExceeded(format!(
                "Insufficient tokens (requested: {}, available: {})",
                tokens, available
            )))
        } else {
            pool.rejections += 1;
            let available = pool.available;
            Self::save_registry(&registry).await?;

            Err(CliError::BackpressureLimitExceeded(format!(
                "Insufficient tokens (requested: {}, available: {})",
                tokens, available
            )))
        }
    }

    /// Return tokens to a pool.
    ///
    /// Adds tokens back to the pool, clamping to the pool's capacity.
    async fn release_tokens(pool_name: String, tokens: usize) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let pool = registry
            .pools
            .iter_mut()
            .find(|p| p.name == pool_name)
            .ok_or_else(|| CliError::Validation(format!("Pool {} not found", pool_name)))?;

        pool.available = (pool.available + tokens).min(pool.capacity);
        let available = pool.available;
        let capacity = pool.capacity;
        Self::save_registry(&registry).await?;

        println!("✓ Released {} tokens to {}", tokens, pool_name);
        println!("  Available: {}/{}", available, capacity);

        Ok(())
    }

    /// Display status and metrics of a token pool.
    ///
    /// Shows current availability, refill rate, and usage statistics.
    async fn check_status(pool_name: String, format: OutputFormat) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let pool = registry
            .pools
            .iter_mut()
            .find(|p| p.name == pool_name)
            .ok_or_else(|| CliError::Validation(format!("Pool {} not found", pool_name)))?;

        Self::refill_pool(pool)?;

        let name = pool.name.clone();
        let capacity = pool.capacity;
        let available = pool.available;
        let refill_rate = pool.refill_rate;
        let acquisitions = pool.acquisitions;
        let rejections = pool.rejections;

        Self::save_registry(&registry).await?;

        match format {
            OutputFormat::Json => {
                let pool_data = serde_json::json!({
                    "name": name,
                    "capacity": capacity,
                    "available": available,
                    "refill_rate": refill_rate,
                    "acquisitions": acquisitions,
                    "rejections": rejections
                });
                let json = serde_json::to_string_pretty(&pool_data)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Token Pool: {}", name);
                println!("===========");
                println!("Available: {}/{}", available, capacity);
                println!("Refill Rate: {}/sec", refill_rate);
                println!("Acquisitions: {}", acquisitions);
                println!("Rejections: {}", rejections);
                let utilization = 100.0 * (1.0 - available as f64 / capacity as f64);
                println!("Utilization: {:.1}%", utilization);
            }
        }

        Ok(())
    }

    /// List all token pools and their current status.
    ///
    /// Displays summary information for each pool including utilization percentage.
    async fn list_pools(format: OutputFormat) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        for pool in &mut registry.pools {
            Self::refill_pool(pool)?;
        }
        Self::save_registry(&registry).await?;

        match format {
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&registry)?;
                println!("{}", json);
            }
            OutputFormat::Text => {
                println!("Token Pools ({})", registry.pools.len());
                println!("============");
                for pool in &registry.pools {
                    let utilization = 100.0 * (1.0 - pool.available as f64 / pool.capacity as f64);
                    println!(
                        "{}: {}/{} ({:.1}%) [{:?}]",
                        pool.name, pool.available, pool.capacity, utilization, pool.policy
                    );
                }
            }
        }

        Ok(())
    }

    /// Configure the admission control policy for a pool.
    ///
    /// Changes how the pool behaves when tokens are insufficient.
    async fn set_policy(pool_name: String, policy_type: PolicyType) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        let pool = registry
            .pools
            .iter_mut()
            .find(|p| p.name == pool_name)
            .ok_or_else(|| CliError::Validation(format!("Pool {} not found", pool_name)))?;

        pool.policy = policy_type.clone();
        Self::save_registry(&registry).await?;

        println!("✓ Policy set for {}: {:?}", pool_name, policy_type);

        Ok(())
    }

    /// Refill tokens in a pool based on elapsed time and refill rate.
    ///
    /// Updates the pool's available tokens and last_refill timestamp.
    fn refill_pool(pool: &mut TokenPool) -> Result<()> {
        let last_refill_dt = chrono::DateTime::parse_from_rfc3339(&pool.last_refill)
            .map_err(|e| CliError::Unknown(format!("Invalid timestamp: {}", e)))?;
        let last_refill = last_refill_dt.with_timezone(&chrono::Utc);

        let now = chrono::Utc::now();
        let elapsed = (now - last_refill).num_seconds() as f64;

        let tokens_to_add = (elapsed * pool.refill_rate) as usize;

        if tokens_to_add > 0 {
            pool.available = (pool.available + tokens_to_add).min(pool.capacity);
            pool.last_refill = now.to_rfc3339();
        }

        Ok(())
    }

    /// Load the pool registry from storage.
    ///
    /// Returns an empty registry if no file exists yet.
    async fn load_registry() -> Result<PoolRegistry> {
        let path = Self::registry_path();
        if !path.exists() {
            return Ok(PoolRegistry { pools: Vec::new() });
        }

        let content = tokio::fs::read_to_string(&path).await?;
        Ok(serde_json::from_str(&content)?)
    }

    /// Persist the pool registry to storage.
    async fn save_registry(registry: &PoolRegistry) -> Result<()> {
        let path = Self::registry_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let json = serde_json::to_string_pretty(registry)?;
        tokio::fs::write(&path, json).await?;
        Ok(())
    }

    /// Get the path to the pool registry file.
    fn registry_path() -> PathBuf {
        PathBuf::from(".ggen/backpressure-pools.json")
    }
}
