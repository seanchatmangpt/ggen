//! Validation Receipt system - Signed proofs of package validation
//!
//! A validation receipt is a cryptographically signed JSON artifact that proves
//! a package passed validation checks at a specific point in time. Receipts are
//! auditable, immutable, and can be used to establish trust in package quality.
//!
//! ## Structure
//!
//! A receipt includes:
//! - Package name and version
//! - Timestamp of validation
//! - Guards that passed/failed
//! - Quality score
//! - Signature (HMAC-SHA256)
//!
//! ## Usage
//!
//! ```ignore
//! let receipt = ValidationReceiptBuilder::new("my-package", "1.0.0")
//!     .with_guard("Guard8020Coverage", true, 85)
//!     .with_score(85)
//!     .sign("secret-key")
//!     .build()?;
//!
//! // Save receipt as JSON
//! let json = serde_json::to_string_pretty(&receipt)?;
//! ```

use serde::{Deserialize, Serialize};
use sha2::Sha256;

/// A validation receipt - signed proof of package validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReceipt {
    /// Package name
    pub package_name: String,

    /// Package version
    pub version: String,

    /// Validation timestamp (RFC3339)
    pub validated_at: String,

    /// Guards that passed
    pub guards_passed: Vec<GuardResult>,

    /// Guards that failed
    pub guards_failed: Vec<GuardResult>,

    /// Overall quality score (0-100)
    pub quality_score: u32,

    /// Is 8020 certified?
    pub is_8020_certified: bool,

    /// Dark matter reduction target (if provided)
    pub dark_matter_reduction_target: Option<String>,

    /// Sector classification (if provided)
    pub sector: Option<String>,

    /// HMAC-SHA256 signature of the receipt content
    pub signature: String,

    /// Signature algorithm
    pub signature_algorithm: String,

    /// Key identifier (for verification)
    pub key_id: String,
}

/// Single guard result in a receipt
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardResult {
    /// Guard name
    pub guard_name: String,

    /// Score contribution (0-100)
    pub score: u32,

    /// Details
    pub details: String,
}

/// Builder for ValidationReceipt
pub struct ValidationReceiptBuilder {
    package_name: String,
    version: String,
    validated_at: String,
    guards_passed: Vec<GuardResult>,
    guards_failed: Vec<GuardResult>,
    quality_score: u32,
    is_8020_certified: bool,
    dark_matter_reduction_target: Option<String>,
    sector: Option<String>,
}

impl ValidationReceiptBuilder {
    /// Create a new builder
    pub fn new(package_name: &str, version: &str) -> Self {
        Self {
            package_name: package_name.to_string(),
            version: version.to_string(),
            validated_at: chrono::Utc::now().to_rfc3339(),
            guards_passed: Vec::new(),
            guards_failed: Vec::new(),
            quality_score: 0,
            is_8020_certified: false,
            dark_matter_reduction_target: None,
            sector: None,
        }
    }

    /// Add a passing guard result
    pub fn with_passed_guard(mut self, guard_name: &str, score: u32, details: &str) -> Self {
        self.guards_passed.push(GuardResult {
            guard_name: guard_name.to_string(),
            score,
            details: details.to_string(),
        });
        self
    }

    /// Add a failing guard result
    pub fn with_failed_guard(mut self, guard_name: &str, details: &str) -> Self {
        self.guards_failed.push(GuardResult {
            guard_name: guard_name.to_string(),
            score: 0,
            details: details.to_string(),
        });
        self
    }

    /// Set quality score
    pub fn with_score(mut self, score: u32) -> Self {
        self.quality_score = score.min(100);
        self
    }

    /// Set 8020 certification status
    pub fn with_8020_certification(mut self, certified: bool) -> Self {
        self.is_8020_certified = certified;
        self
    }

    /// Set dark matter reduction target
    pub fn with_dark_matter_target(mut self, target: &str) -> Self {
        self.dark_matter_reduction_target = Some(target.to_string());
        self
    }

    /// Set sector
    pub fn with_sector(mut self, sector: &str) -> Self {
        self.sector = Some(sector.to_string());
        self
    }

    /// Build the receipt with signature
    pub fn build(self, signing_key: &str) -> Result<ValidationReceipt, String> {
        let receipt = ValidationReceipt {
            package_name: self.package_name,
            version: self.version,
            validated_at: self.validated_at,
            guards_passed: self.guards_passed,
            guards_failed: self.guards_failed,
            quality_score: self.quality_score,
            is_8020_certified: self.is_8020_certified,
            dark_matter_reduction_target: self.dark_matter_reduction_target,
            sector: self.sector,
            signature: String::new(), // Will be set below
            signature_algorithm: "HMAC-SHA256".to_string(),
            key_id: "ggen-marketplace-v1".to_string(),
        };

        // Calculate signature
        let signature = Self::calculate_signature(&receipt, signing_key)?;

        Ok(ValidationReceipt {
            signature,
            ..receipt
        })
    }

    /// Calculate HMAC-SHA256 signature
    fn calculate_signature(receipt: &ValidationReceipt, key: &str) -> Result<String, String> {
        // Create a canonical representation for signing
        let to_sign = format!(
            "{}|{}|{}|{}",
            receipt.package_name, receipt.version, receipt.validated_at, receipt.quality_score
        );

        // Use HMAC-SHA256
        use hmac::{Hmac, Mac};
        type HmacSha256 = Hmac<Sha256>;

        let mut mac = HmacSha256::new_from_slice(key.as_bytes())
            .map_err(|_| "Invalid key length".to_string())?;
        mac.update(to_sign.as_bytes());
        let result = mac.finalize();

        Ok(hex::encode(result.into_bytes()))
    }

    /// Verify receipt signature
    pub fn verify_signature(receipt: &ValidationReceipt, key: &str) -> Result<bool, String> {
        let to_sign = format!(
            "{}|{}|{}|{}",
            receipt.package_name, receipt.version, receipt.validated_at, receipt.quality_score
        );

        use hmac::{Hmac, Mac};
        type HmacSha256 = Hmac<Sha256>;

        let mut mac = HmacSha256::new_from_slice(key.as_bytes())
            .map_err(|_| "Invalid key length".to_string())?;
        mac.update(to_sign.as_bytes());

        let expected = hex::encode(mac.finalize().into_bytes());
        Ok(expected == receipt.signature)
    }
}

impl ValidationReceipt {
    /// Verify the receipt's signature
    pub fn verify(&self, signing_key: &str) -> Result<bool, String> {
        ValidationReceiptBuilder::verify_signature(self, signing_key)
    }

    /// Export as JSON string
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Export as JSON with minimal formatting
    pub fn to_json_compact(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_builder() {
        let receipt = ValidationReceiptBuilder::new("test-pkg", "1.0.0")
            .with_passed_guard("Guard8020Coverage", 85, "All checks passed")
            .with_score(85)
            .with_8020_certification(true)
            .with_sector("observability")
            .build("test-key")
            .unwrap();

        assert_eq!(receipt.package_name, "test-pkg");
        assert_eq!(receipt.version, "1.0.0");
        assert_eq!(receipt.quality_score, 85);
        assert!(receipt.is_8020_certified);
        assert_eq!(receipt.sector, Some("observability".to_string()));
    }

    #[test]
    fn test_signature_verification() {
        let signing_key = "my-secret-key";
        let receipt = ValidationReceiptBuilder::new("test-pkg", "1.0.0")
            .with_score(75)
            .build(signing_key)
            .unwrap();

        // Verify with correct key
        assert!(receipt.verify(signing_key).unwrap());

        // Verify with wrong key fails
        assert!(!receipt.verify("wrong-key").unwrap());
    }

    #[test]
    fn test_receipt_json_export() {
        let receipt = ValidationReceiptBuilder::new("test-pkg", "1.0.0")
            .with_score(80)
            .build("test-key")
            .unwrap();

        let json = receipt.to_json().unwrap();
        assert!(json.contains("test-pkg"));
        assert!(json.contains("1.0.0"));
        assert!(json.contains("80"));

        // Verify JSON is valid by round-tripping
        let deserialized: ValidationReceipt = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.package_name, "test-pkg");
    }
}
