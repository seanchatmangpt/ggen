//! Receipt chain implementation for maintaining hash-linked receipts.

use crate::error::{ReceiptError, Result};
use crate::receipt::Receipt;
use ed25519_dalek::VerifyingKey;
use serde::{Deserialize, Serialize};

/// A chain of cryptographically linked receipts.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptChain {
    /// All receipts in the chain, ordered from genesis to latest.
    receipts: Vec<Receipt>,
}

impl ReceiptChain {
    /// Creates a new empty receipt chain.
    #[must_use]
    pub fn new() -> Self {
        Self {
            receipts: Vec::new(),
        }
    }

    /// Creates a receipt chain from a genesis receipt.
    ///
    /// # Arguments
    ///
    /// * `genesis` - The first receipt in the chain
    ///
    /// # Returns
    ///
    /// A new chain containing the genesis receipt.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidReceipt` if the genesis receipt has a previous hash.
    pub fn from_genesis(genesis: Receipt) -> Result<Self> {
        if genesis.previous_receipt_hash.is_some() {
            return Err(ReceiptError::InvalidReceipt(
                "Genesis receipt must not have a previous hash".to_string(),
            ));
        }

        Ok(Self {
            receipts: vec![genesis],
        })
    }

    /// Appends a receipt to the chain.
    ///
    /// # Arguments
    ///
    /// * `receipt` - The receipt to append
    ///
    /// # Returns
    ///
    /// `Ok(())` if the receipt was successfully appended.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidChain` if:
    /// - The chain is empty and the receipt is not a genesis receipt
    /// - The receipt's previous hash doesn't match the last receipt's hash
    pub fn append(&mut self, receipt: Receipt) -> Result<()> {
        if self.receipts.is_empty() {
            if receipt.previous_receipt_hash.is_some() {
                return Err(ReceiptError::InvalidChain(
                    "First receipt must be a genesis receipt (no previous hash)".to_string(),
                ));
            }
        } else {
            let last_receipt = self.receipts.last()
                .ok_or_else(|| ReceiptError::InvalidChain("Chain is empty".to_string()))?;

            let last_hash = last_receipt.hash()?;

            match &receipt.previous_receipt_hash {
                Some(prev_hash) if prev_hash == &last_hash => {}
                Some(prev_hash) => {
                    return Err(ReceiptError::HashMismatch {
                        expected: last_hash,
                        actual: prev_hash.clone(),
                    });
                }
                None => {
                    return Err(ReceiptError::InvalidChain(
                        "Non-genesis receipt must have a previous hash".to_string(),
                    ));
                }
            }
        }

        self.receipts.push(receipt);
        Ok(())
    }

    /// Verifies the entire chain's integrity.
    ///
    /// # Arguments
    ///
    /// * `verifying_key` - The Ed25519 verifying key to verify signatures
    ///
    /// # Returns
    ///
    /// `Ok(())` if the entire chain is valid.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidChain` if any link in the chain is invalid.
    /// Returns `ReceiptError::InvalidSignature` if any signature verification fails.
    pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> {
        if self.receipts.is_empty() {
            return Ok(());
        }

        // Verify genesis receipt
        let genesis = &self.receipts[0];
        if genesis.previous_receipt_hash.is_some() {
            return Err(ReceiptError::InvalidChain(
                "Genesis receipt must not have a previous hash".to_string(),
            ));
        }
        genesis.verify(verifying_key)?;

        // Verify remaining receipts and chain links
        for i in 1..self.receipts.len() {
            let current = &self.receipts[i];
            let previous = &self.receipts[i - 1];

            // Verify signature
            current.verify(verifying_key)?;

            // Verify chain link
            let expected_hash = previous.hash()?;
            match &current.previous_receipt_hash {
                Some(prev_hash) if prev_hash == &expected_hash => {}
                Some(prev_hash) => {
                    return Err(ReceiptError::HashMismatch {
                        expected: expected_hash,
                        actual: prev_hash.clone(),
                    });
                }
                None => {
                    return Err(ReceiptError::InvalidChain(
                        format!("Receipt at index {} is missing previous hash", i),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Returns the number of receipts in the chain.
    #[must_use]
    pub fn len(&self) -> usize {
        self.receipts.len()
    }

    /// Returns true if the chain is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.receipts.is_empty()
    }

    /// Returns a reference to the receipts in the chain.
    #[must_use]
    pub fn receipts(&self) -> &[Receipt] {
        &self.receipts
    }

    /// Returns the last receipt in the chain, if any.
    #[must_use]
    pub fn last(&self) -> Option<&Receipt> {
        self.receipts.last()
    }

    /// Returns the genesis receipt, if any.
    #[must_use]
    pub fn genesis(&self) -> Option<&Receipt> {
        self.receipts.first()
    }
}

impl Default for ReceiptChain {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::receipt::generate_keypair;

    #[test]
    fn test_empty_chain() {
        let chain = ReceiptChain::new();
        assert!(chain.is_empty());
        assert_eq!(chain.len(), 0);
    }

    #[test]
    fn test_genesis_chain() {
        let (signing_key, _) = generate_keypair();

        let genesis = Receipt::new(
            "genesis".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

        assert!(!chain.is_empty());
        assert_eq!(chain.len(), 1);
    }

    #[test]
    fn test_genesis_with_previous_hash_fails() {
        let (signing_key, _) = generate_keypair();

        let invalid_genesis = Receipt::new(
            "genesis".to_string(),
            vec![],
            vec![],
            Some("previous".to_string()),
        )
        .sign(&signing_key)
        .expect("signing failed");

        let result = ReceiptChain::from_genesis(invalid_genesis);
        assert!(result.is_err());
    }

    #[test]
    fn test_chain_append() {
        let (signing_key, verifying_key) = generate_keypair();

        let genesis = Receipt::new(
            "op1".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis creation failed");

        let receipt2 = Receipt::new(
            "op2".to_string(),
            vec![],
            vec![],
            None,
        )
        .chain(&genesis)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

        chain.append(receipt2).expect("append failed");

        assert_eq!(chain.len(), 2);
        assert!(chain.verify(&verifying_key).is_ok());
    }

    #[test]
    fn test_chain_verify() {
        let (signing_key, verifying_key) = generate_keypair();

        let genesis = Receipt::new(
            "op1".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis creation failed");

        let receipt2 = Receipt::new(
            "op2".to_string(),
            vec![],
            vec![],
            None,
        )
        .chain(&genesis)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

        let receipt3 = Receipt::new(
            "op3".to_string(),
            vec![],
            vec![],
            None,
        )
        .chain(&receipt2)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

        chain.append(receipt2).expect("append failed");
        chain.append(receipt3).expect("append failed");

        assert_eq!(chain.len(), 3);
        assert!(chain.verify(&verifying_key).is_ok());
    }

    #[test]
    fn test_chain_verify_fails_with_wrong_key() {
        let (signing_key, _) = generate_keypair();
        let (_, wrong_key) = generate_keypair();

        let genesis = Receipt::new(
            "op1".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

        assert!(chain.verify(&wrong_key).is_err());
    }

    #[test]
    fn test_chain_append_with_wrong_hash_fails() {
        let (signing_key, _) = generate_keypair();

        let genesis = Receipt::new(
            "op1".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

        let wrong_receipt = Receipt::new(
            "op2".to_string(),
            vec![],
            vec![],
            Some("wrong_hash".to_string()),
        )
        .sign(&signing_key)
        .expect("signing failed");

        assert!(chain.append(wrong_receipt).is_err());
    }

    #[test]
    fn test_chain_accessors() {
        let (signing_key, _) = generate_keypair();

        let genesis = Receipt::new(
            "op1".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis creation failed");

        assert_eq!(chain.genesis().unwrap().operation_id, "op1");
        assert_eq!(chain.last().unwrap().operation_id, "op1");
        assert_eq!(chain.receipts().len(), 1);
    }
}
