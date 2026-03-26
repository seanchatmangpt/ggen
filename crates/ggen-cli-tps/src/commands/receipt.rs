use crate::error::{CliError, Result};
use clap::Subcommand;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::PathBuf;

/// Receipt management commands for cryptographic verification and auditing.
///
/// Provides subcommands for managing receipt chains that verify code generation operations.
/// Each receipt contains a SHA-256 hash of the operation and timestamp, enabling verification
/// of build integrity and auditability of generated code artifacts.
///
/// # Variants
///
/// - `Verify` - Check a single receipt's cryptographic integrity
/// - `Chain` - Link multiple receipts into a chain with hash linkage
/// - `Audit` - Verify the integrity of an entire receipt chain
/// - `Generate` - Create a new receipt for an operation
#[derive(Debug, Subcommand)]
pub enum ReceiptCommands {
    /// Verify a receipt's cryptographic integrity.
    ///
    /// Validates that a receipt's stored hash matches the computed hash of its operation
    /// and timestamp. This ensures the receipt has not been tampered with.
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - The receipt file cannot be read
    /// - The receipt JSON is malformed
    /// - The computed hash does not match the stored hash
    Verify {
        /// Path to receipt file to verify
        #[clap(short, long)]
        receipt: PathBuf,
    },

    /// Create a receipt chain linking multiple receipts.
    ///
    /// Takes an ordered sequence of individual receipts and chains them together,
    /// where each receipt stores the hash of the previous receipt. This creates
    /// an immutable chain that can be audited as a whole. The chain hash is
    /// computed from all individual receipt hashes.
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - Any receipt file cannot be read
    /// - Receipt JSON parsing fails
    /// - Chain output file cannot be written
    Chain {
        /// Paths to receipts to chain (in order)
        #[clap(short, long, num_args = 1..)]
        receipts: Vec<PathBuf>,

        /// Output file path for the receipt chain
        #[clap(short, long)]
        output: PathBuf,
    },

    /// Audit a receipt chain for integrity and consistency.
    ///
    /// Verifies the entire receipt chain by:
    /// 1. Checking each receipt's hash against its operation and timestamp
    /// 2. Verifying that each receipt's previous hash matches the prior receipt's hash
    /// 3. Computing and validating the chain's overall hash
    ///
    /// This detects any tampering or corruption in the chain.
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - The chain file cannot be read
    /// - Chain JSON parsing fails
    /// - The chain is broken (mismatched previous hashes)
    /// - Any receipt hash is invalid
    /// - The chain hash is invalid
    Audit {
        /// Path to the receipt chain file to audit
        #[clap(short, long)]
        chain: PathBuf,

        /// Enable verbose output showing each receipt verification
        #[clap(short, long)]
        verbose: bool,
    },

    /// Generate a new receipt for an operation.
    ///
    /// Creates a receipt containing:
    /// - Operation name or description
    /// - RFC3339 timestamp of generation
    /// - SHA-256 hash computed from operation and timestamp
    /// - Optional metadata as JSON
    ///
    /// Receipts are written as JSON files suitable for chaining.
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - Metadata JSON parsing fails
    /// - The output file cannot be written
    Generate {
        /// Name or description of the operation being receipted
        #[clap(short, long)]
        operation: String,

        /// Optional metadata as JSON string (e.g., `'{"test_count": 347}'`)
        #[clap(short, long)]
        metadata: Option<String>,

        /// Output file path for the receipt
        #[clap(short = 'f', long)]
        output: PathBuf,
    },
}

/// A cryptographic receipt for code generation operations.
///
/// A receipt provides tamper-proof evidence of an operation's execution with its
/// timestamp and metadata. The hash is computed from the operation name and timestamp,
/// enabling verification that the receipt has not been modified. When receipts are
/// chained, each receipt stores the hash of the previous receipt, creating an
/// immutable audit trail.
///
/// # Fields
///
/// * `operation` - Human-readable description of the operation being receipted
///   (e.g., `"cargo make test"`, `"ggen sync --audit"`)
/// * `timestamp` - RFC3339 timestamp of when the receipt was created
/// * `hash` - SHA-256 hash of the operation and timestamp (hex-encoded).
///   Computed as: `SHA256(operation || timestamp)`
/// * `metadata` - Optional operational metadata as JSON (e.g., test counts,
///   build times, code metrics)
/// * `previous_hash` - Hash of the prior receipt in a chain (None for standalone receipts).
///   Set when receipts are linked into a chain.
///
/// # Serialization
///
/// Receipts are serialized as JSON and can be written to files for storage
/// and later verification. See [`Receipt::verify`](ReceiptCommands::verify_receipt) and
/// [`ReceiptChain::audit`](ReceiptCommands::audit_chain).
///
/// # Example
///
/// ```json
/// {
///   "operation": "cargo make test",
///   "timestamp": "2026-03-24T10:30:45.123456Z",
///   "hash": "a1b2c3d4e5f6...",
///   "metadata": {"test_count": 347, "duration_ms": 5234},
///   "previous_hash": null
/// }
/// ```
#[derive(Debug, Serialize, Deserialize)]
pub struct Receipt {
    /// Name or description of the operation
    pub operation: String,

    /// RFC3339 timestamp of receipt creation
    pub timestamp: String,

    /// SHA-256 hash (hex-encoded) of operation and timestamp
    pub hash: String,

    /// Optional metadata as JSON
    pub metadata: serde_json::Value,

    /// Hash of the previous receipt in a chain (if chained)
    pub previous_hash: Option<String>,
}

/// A chain of linked receipts forming an immutable audit trail.
///
/// A receipt chain links multiple individual receipts such that each receipt
/// stores the hash of the previous one. This creates a cryptographic chain of
/// custody: if any receipt is modified, its hash changes, breaking the chain
/// and making tampering detectable. The chain itself is validated by computing
/// a single chain hash from all receipt hashes.
///
/// # Fields
///
/// * `receipts` - Ordered vector of receipts, where each receipt's `previous_hash`
///   field points to the prior receipt's hash
/// * `chain_hash` - SHA-256 hash (hex-encoded) computed from all receipt hashes in order.
///   Computed as: `SHA256(receipt[0].hash || receipt[1].hash || ... || receipt[n].hash)`
///
/// # Audit Integrity
///
/// A valid chain must satisfy:
/// 1. Each receipt's hash matches its operation and timestamp
/// 2. Each receipt's `previous_hash` matches the prior receipt's `hash`
///   (first receipt has `previous_hash == None`)
/// 3. The `chain_hash` matches the computed hash of all receipt hashes
///
/// # Example
///
/// ```json
/// {
///   "receipts": [
///     {
///       "operation": "cargo make check",
///       "timestamp": "2026-03-24T10:00:00Z",
///       "hash": "hash1",
///       "metadata": {},
///       "previous_hash": null
///     },
///     {
///       "operation": "cargo make test",
///       "timestamp": "2026-03-24T10:01:00Z",
///       "hash": "hash2",
///       "metadata": {"test_count": 347},
///       "previous_hash": "hash1"
///     }
///   ],
///   "chain_hash": "chain_hash_computed_from_hash1_and_hash2"
/// }
/// ```
#[derive(Debug, Serialize, Deserialize)]
pub struct ReceiptChain {
    /// Ordered vector of linked receipts
    pub receipts: Vec<Receipt>,

    /// SHA-256 hash (hex-encoded) computed from all receipt hashes
    pub chain_hash: String,
}

impl ReceiptCommands {
    /// Execute the receipt command asynchronously.
    ///
    /// Dispatches to the appropriate subcommand handler based on the variant:
    /// - `Verify` → verifies a receipt's cryptographic integrity
    /// - `Chain` → creates a linked receipt chain
    /// - `Audit` → audits a receipt chain for integrity
    /// - `Generate` → creates a new receipt
    ///
    /// # Errors
    ///
    /// Returns `Result<()>` with `CliError::ReceiptVerification` or I/O errors
    /// if the operation fails. See individual variant documentation for specific
    /// error conditions.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let cmd = ReceiptCommands::Generate {
    ///     operation: "cargo make test".to_string(),
    ///     metadata: None,
    ///     output: PathBuf::from("receipt.json"),
    /// };
    /// cmd.execute().await?;
    /// ```
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

    /// Verify a receipt's cryptographic integrity.
    ///
    /// Reads a receipt from the given path, computes the expected hash from its
    /// operation and timestamp, and compares it against the stored hash. If they
    /// match, the receipt has not been tampered with.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the receipt JSON file
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - The file cannot be read
    /// - Receipt JSON is malformed
    /// - Hash mismatch indicates tampering
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

    /// Create a receipt chain from multiple individual receipts.
    ///
    /// Reads receipts from the given paths in order and links them by setting each
    /// receipt's `previous_hash` to the prior receipt's hash. The chain hash is
    /// computed from all receipt hashes and written to the output file.
    ///
    /// # Arguments
    ///
    /// * `receipts` - Ordered vector of paths to receipt JSON files
    /// * `output` - Path where the receipt chain JSON will be written
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - Any receipt file cannot be read
    /// - Receipt JSON is malformed
    /// - Output file cannot be written
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

    /// Audit a receipt chain for integrity and consistency.
    ///
    /// Performs comprehensive validation of a receipt chain:
    /// 1. Verifies each receipt's hash against its operation and timestamp
    /// 2. Checks each receipt's `previous_hash` against the prior receipt's hash
    /// 3. Validates the chain's overall hash against all receipt hashes
    ///
    /// If `verbose` is true, prints verification status for each receipt.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the receipt chain JSON file
    /// * `verbose` - If true, print per-receipt verification results
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - The chain file cannot be read
    /// - Chain JSON is malformed
    /// - The chain is broken (mismatched `previous_hash`)
    /// - Any receipt hash is invalid
    /// - The chain hash is invalid
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

    /// Generate a new receipt for an operation.
    ///
    /// Creates a receipt containing the operation name, current UTC timestamp,
    /// and the SHA-256 hash of operation + timestamp. Optional metadata can be
    /// provided as a JSON string.
    ///
    /// # Arguments
    ///
    /// * `operation` - Description of the operation being receipted
    /// * `metadata` - Optional metadata as a JSON string
    /// * `output` - Path where the receipt JSON will be written
    ///
    /// # Errors
    ///
    /// Returns `CliError::ReceiptVerification` if:
    /// - Metadata JSON is malformed
    /// - Output file cannot be written
    ///
    /// # Example
    ///
    /// ```ignore
    /// Self::generate_receipt(
    ///     "cargo make test".to_string(),
    ///     Some(r#"{"test_count": 347}"#.to_string()),
    ///     PathBuf::from("receipt.json"),
    /// ).await?;
    /// ```
    async fn generate_receipt(
        operation: String, metadata: Option<String>, output: PathBuf,
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

    /// Compute the SHA-256 hash of an operation and timestamp.
    ///
    /// Creates a hex-encoded SHA-256 hash by concatenating operation and timestamp
    /// bytes. This is used to verify that a receipt has not been modified.
    ///
    /// # Arguments
    ///
    /// * `operation` - Operation name/description
    /// * `timestamp` - RFC3339 timestamp string
    ///
    /// # Returns
    ///
    /// Hex-encoded SHA-256 hash string
    fn compute_hash(operation: &str, timestamp: &str) -> Result<String> {
        let mut hasher = Sha256::new();
        hasher.update(operation.as_bytes());
        hasher.update(timestamp.as_bytes());
        let result = hasher.finalize();
        Ok(hex::encode(result))
    }

    /// Compute the SHA-256 hash of an entire receipt chain.
    ///
    /// Creates a hex-encoded SHA-256 hash by concatenating all receipt hashes
    /// in order. This provides a single checksum for the entire chain, enabling
    /// detection of any modification or reordering.
    ///
    /// # Arguments
    ///
    /// * `receipts` - Slice of receipts in the chain
    ///
    /// # Returns
    ///
    /// Hex-encoded SHA-256 hash string computed from all receipt hashes
    fn compute_chain_hash(receipts: &[Receipt]) -> Result<String> {
        let mut hasher = Sha256::new();
        for receipt in receipts {
            hasher.update(receipt.hash.as_bytes());
        }
        let result = hasher.finalize();
        Ok(hex::encode(result))
    }
}
