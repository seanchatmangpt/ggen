use crate::crypto::Ed25519Verifier;
use crate::error::Result;
use crate::models::{PublicKey, Signature};
use crate::traits::CryptoVerifier;

/// Default cryptographic verifier
///
/// A convenience wrapper around Ed25519Verifier that can be extended
/// to support multiple signature algorithms in the future.
pub struct DefaultVerifier {
    ed25519: Ed25519Verifier,
}

impl DefaultVerifier {
    /// Create a new default verifier
    pub fn new() -> Self {
        Self {
            ed25519: Ed25519Verifier::new(),
        }
    }
}

impl Default for DefaultVerifier {
    fn default() -> Self {
        Self::new()
    }
}

impl CryptoVerifier for DefaultVerifier {
    fn sign(&self, content: &[u8]) -> Result<Signature> {
        self.ed25519.sign(content)
    }

    fn verify(&self, content: &[u8], signature: &Signature) -> Result<bool> {
        self.ed25519.verify(content, signature)
    }

    fn generate_keypair(&self) -> Result<crate::models::signature::KeyPair> {
        self.ed25519.generate_keypair()
    }

    fn import_public_key(&self, pem: &str) -> Result<PublicKey> {
        self.ed25519.import_public_key(pem)
    }

    fn export_public_key(&self, key: &PublicKey) -> Result<String> {
        self.ed25519.export_public_key(key)
    }

    fn hash_content(&self, content: &[u8]) -> Result<String> {
        self.ed25519.hash_content(content)
    }
}
