use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

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

#[derive(Debug, Clone, clap::ValueEnum)]
pub enum OutputFormat {
    Json,
    Text,
}

#[derive(Debug, Clone, Serialize, Deserialize, clap::ValueEnum)]
#[serde(rename_all = "lowercase")]
pub enum PolicyType {
    Strict,
    Relaxed,
    Adaptive,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TokenPool {
    pub name: String,
    pub capacity: usize,
    pub available: usize,
    pub refill_rate: f64,
    pub last_refill: String,
    pub policy: PolicyType,
    pub acquisitions: usize,
    pub rejections: usize,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PoolRegistry {
    pub pools: Vec<TokenPool>,
}

impl BackpressureCommands {
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

    async fn init_pool(name: String, capacity: usize, refill_rate: f64) -> Result<()> {
        let mut registry = Self::load_registry().await?;

        if registry.pools.iter().any(|p| p.name == name) {
            return Err(CliError::Validation(format!("Pool {} already exists", name)));
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

    async fn load_registry() -> Result<PoolRegistry> {
        let path = Self::registry_path();
        if !path.exists() {
            return Ok(PoolRegistry { pools: Vec::new() });
        }

        let content = tokio::fs::read_to_string(&path).await?;
        Ok(serde_json::from_str(&content)?)
    }

    async fn save_registry(registry: &PoolRegistry) -> Result<()> {
        let path = Self::registry_path();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let json = serde_json::to_string_pretty(registry)?;
        tokio::fs::write(&path, json).await?;
        Ok(())
    }

    fn registry_path() -> PathBuf {
        PathBuf::from(".ggen/backpressure-pools.json")
    }
}
