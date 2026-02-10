use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::PathBuf;

#[derive(Debug, Subcommand)]
pub enum ReceiptCommands {
    /// Verify a receipt's cryptographic integrity
    Verify {
        /// Path to receipt file
        #[clap(short, long)]
        receipt: PathBuf,
    },

    /// Create a receipt chain
    Chain {
        /// Receipts to chain
        #[clap(short, long, num_args = 1..)]
        receipts: Vec<PathBuf>,

        /// Output file for chain
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Audit receipt chain
    Audit {
        /// Chain file to audit
        #[clap(short, long)]
        chain: PathBuf,

        /// Verbose output
        #[clap(short, long)]
        verbose: bool,
    },

    /// Generate new receipt
    Generate {
        /// Operation performed
        #[clap(short, long)]
        operation: String,

        /// Metadata JSON
        #[clap(short, long)]
        metadata: Option<String>,

        /// Output file
        #[clap(short = 'f', long)]
        output: PathBuf,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Receipt {
    pub operation: String,
    pub timestamp: String,
    pub hash: String,
    pub metadata: serde_json::Value,
    pub previous_hash: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ReceiptChain {
    pub receipts: Vec<Receipt>,
    pub chain_hash: String,
}

impl ReceiptCommands {
    pub async fn execute(self) -> Result<()> {
        match self {
            Self::Verify { receipt } => Self::verify_receipt(&receipt).await,
            Self::Chain { receipts, output } => Self::create_chain(receipts, output).await,
            Self::Audit { chain, verbose } => Self::audit_chain(&chain, verbose).await,
            Self::Generate {
                operation,
                metadata,
                output,
            } => Self::generate_receipt(operation, metadata, output).await,
        }
    }

    async fn verify_receipt(path: &PathBuf) -> Result<()> {
        let content = tokio::fs::read_to_string(path).await?;
        let receipt: Receipt = serde_json::from_str(&content)?;

        let computed_hash = Self::compute_hash(&receipt.operation, &receipt.timestamp)?;

        if computed_hash == receipt.hash {
            println!("✓ Receipt verified: {}", path.display());
            Ok(())
        } else {
            Err(CliError::ReceiptVerification(format!(
                "Hash mismatch for {}",
                path.display()
            )))
        }
    }

    async fn create_chain(receipts: Vec<PathBuf>, output: PathBuf) -> Result<()> {
        let mut chain_receipts = Vec::new();
        let mut previous_hash: Option<String> = None;

        for receipt_path in receipts {
            let content = tokio::fs::read_to_string(&receipt_path).await?;
            let mut receipt: Receipt = serde_json::from_str(&content)?;
            receipt.previous_hash = previous_hash.clone();
            previous_hash = Some(receipt.hash.clone());
            chain_receipts.push(receipt);
        }

        let chain_hash = Self::compute_chain_hash(&chain_receipts)?;
        let chain = ReceiptChain {
            receipts: chain_receipts,
            chain_hash,
        };

        let json = serde_json::to_string_pretty(&chain)?;
        tokio::fs::write(&output, json).await?;

        println!("✓ Receipt chain created: {}", output.display());
        Ok(())
    }

    async fn audit_chain(path: &PathBuf, verbose: bool) -> Result<()> {
        let content = tokio::fs::read_to_string(path).await?;
        let chain: ReceiptChain = serde_json::from_str(&content)?;

        let mut previous_hash: Option<String> = None;

        for (idx, receipt) in chain.receipts.iter().enumerate() {
            if receipt.previous_hash != previous_hash {
                return Err(CliError::ReceiptVerification(format!(
                    "Chain broken at receipt {}",
                    idx
                )));
            }

            let computed_hash = Self::compute_hash(&receipt.operation, &receipt.timestamp)?;
            if computed_hash != receipt.hash {
                return Err(CliError::ReceiptVerification(format!(
                    "Hash mismatch at receipt {}",
                    idx
                )));
            }

            if verbose {
                println!("Receipt {}: ✓ {}", idx, receipt.operation);
            }

            previous_hash = Some(receipt.hash.clone());
        }

        let computed_chain_hash = Self::compute_chain_hash(&chain.receipts)?;
        if computed_chain_hash != chain.chain_hash {
            return Err(CliError::ReceiptVerification(
                "Chain hash mismatch".to_string(),
            ));
        }

        println!("✓ Chain audit passed: {} receipts", chain.receipts.len());
        Ok(())
    }

    async fn generate_receipt(
        operation: String,
        metadata: Option<String>,
        output: PathBuf,
    ) -> Result<()> {
        let timestamp = chrono::Utc::now().to_rfc3339();
        let hash = Self::compute_hash(&operation, &timestamp)?;

        let metadata_value = if let Some(m) = metadata {
            serde_json::from_str(&m)?
        } else {
            serde_json::json!({})
        };

        let receipt = Receipt {
            operation,
            timestamp,
            hash,
            metadata: metadata_value,
            previous_hash: None,
        };

        let json = serde_json::to_string_pretty(&receipt)?;
        tokio::fs::write(&output, json).await?;

        println!("✓ Receipt generated: {}", output.display());
        Ok(())
    }

    fn compute_hash(operation: &str, timestamp: &str) -> Result<String> {
        let mut hasher = Sha256::new();
        hasher.update(operation.as_bytes());
        hasher.update(timestamp.as_bytes());
        let result = hasher.finalize();
        Ok(hex::encode(result))
    }

    fn compute_chain_hash(receipts: &[Receipt]) -> Result<String> {
        let mut hasher = Sha256::new();
        for receipt in receipts {
            hasher.update(receipt.hash.as_bytes());
        }
        let result = hasher.finalize();
        Ok(hex::encode(result))
    }
}
