//! WebAssembly Cryptographic Operations Module
//!
//! Provides secure cryptographic primitives optimized for web environments:
//! - Password hashing with Argon2
//! - AES-256-GCM encryption/decryption
//! - Ed25519 digital signatures
//! - Secure random generation

mod crypto;
mod utils;

use wasm_bindgen::prelude::*;
use crypto::*;
use utils::*;

// Export panic hook for better error messages in browsers
#[wasm_bindgen(start)]
pub fn init() {
    utils::set_panic_hook();
}

/// Hash a password using Argon2id with secure defaults
///
/// # Arguments
/// * `password` - The password to hash
/// * `salt` - Optional salt (will be generated if not provided)
///
/// # Returns
/// Base64-encoded password hash with embedded salt and parameters
#[wasm_bindgen]
pub fn hash_password(password: &str, salt: Option<Vec<u8>>) -> Result<String, JsValue> {
    crypto::hash_password_argon2(password, salt)
        .map_err(|e| JsValue::from_str(&format!("Password hashing failed: {}", e)))
}

/// Verify a password against an Argon2 hash
///
/// # Arguments
/// * `password` - The password to verify
/// * `hash` - The base64-encoded hash to verify against
///
/// # Returns
/// `true` if the password matches, `false` otherwise
#[wasm_bindgen]
pub fn verify_password(password: &str, hash: &str) -> Result<bool, JsValue> {
    crypto::verify_password_argon2(password, hash)
        .map_err(|e| JsValue::from_str(&format!("Password verification failed: {}", e)))
}

/// Encrypt data using AES-256-GCM
///
/// # Arguments
/// * `data` - The data to encrypt
/// * `key` - 32-byte encryption key
/// * `nonce` - Optional 12-byte nonce (will be generated if not provided)
///
/// # Returns
/// Object containing: `ciphertext` (base64), `nonce` (base64), `tag` (base64)
#[wasm_bindgen]
pub fn encrypt_aes(data: &[u8], key: &[u8], nonce: Option<Vec<u8>>) -> Result<JsValue, JsValue> {
    let result = crypto::encrypt_aes_gcm(data, key, nonce)
        .map_err(|e| JsValue::from_str(&format!("Encryption failed: {}", e)))?;

    serde_wasm_bindgen::to_value(&result)
        .map_err(|e| JsValue::from_str(&format!("Serialization failed: {}", e)))
}

/// Decrypt data using AES-256-GCM
///
/// # Arguments
/// * `ciphertext` - Base64-encoded encrypted data
/// * `key` - 32-byte encryption key
/// * `nonce` - 12-byte nonce (base64)
///
/// # Returns
/// Decrypted plaintext as byte array
#[wasm_bindgen]
pub fn decrypt_aes(ciphertext: &str, key: &[u8], nonce: &str) -> Result<Vec<u8>, JsValue> {
    crypto::decrypt_aes_gcm(ciphertext, key, nonce)
        .map_err(|e| JsValue::from_str(&format!("Decryption failed: {}", e)))
}

/// Generate a new Ed25519 keypair
///
/// # Returns
/// Object containing: `publicKey` (base64), `secretKey` (base64)
#[wasm_bindgen]
pub fn generate_keypair() -> Result<JsValue, JsValue> {
    let keypair = crypto::generate_ed25519_keypair()
        .map_err(|e| JsValue::from_str(&format!("Keypair generation failed: {}", e)))?;

    serde_wasm_bindgen::to_value(&keypair)
        .map_err(|e| JsValue::from_str(&format!("Serialization failed: {}", e)))
}

/// Sign a message using Ed25519
///
/// # Arguments
/// * `message` - The message to sign
/// * `secret_key` - Base64-encoded secret key
///
/// # Returns
/// Base64-encoded signature
#[wasm_bindgen]
pub fn sign_message(message: &[u8], secret_key: &str) -> Result<String, JsValue> {
    crypto::sign_ed25519(message, secret_key)
        .map_err(|e| JsValue::from_str(&format!("Signing failed: {}", e)))
}

/// Verify an Ed25519 signature
///
/// # Arguments
/// * `message` - The original message
/// * `signature` - Base64-encoded signature
/// * `public_key` - Base64-encoded public key
///
/// # Returns
/// `true` if signature is valid, `false` otherwise
#[wasm_bindgen]
pub fn verify_signature(message: &[u8], signature: &str, public_key: &str) -> Result<bool, JsValue> {
    crypto::verify_ed25519(message, signature, public_key)
        .map_err(|e| JsValue::from_str(&format!("Verification failed: {}", e)))
}

/// Generate cryptographically secure random bytes
///
/// # Arguments
/// * `length` - Number of random bytes to generate
///
/// # Returns
/// Array of random bytes
#[wasm_bindgen]
pub fn random_bytes(length: usize) -> Result<Vec<u8>, JsValue> {
    crypto::generate_random_bytes(length)
        .map_err(|e| JsValue::from_str(&format!("Random generation failed: {}", e)))
}

/// Generate a secure random encryption key
///
/// # Returns
/// 32-byte random key suitable for AES-256
#[wasm_bindgen]
pub fn generate_key() -> Result<Vec<u8>, JsValue> {
    random_bytes(32)
}

/// Hash data using SHA-256
///
/// # Arguments
/// * `data` - The data to hash
///
/// # Returns
/// Base64-encoded hash
#[wasm_bindgen]
pub fn hash_sha256(data: &[u8]) -> String {
    crypto::hash_sha256(data)
}

/// Get module version and capabilities
#[wasm_bindgen]
pub fn get_info() -> JsValue {
    let info = serde_json::json!({
        "name": "wasm-crypto",
        "version": env!("CARGO_PKG_VERSION"),
        "capabilities": [
            "argon2-password-hashing",
            "aes-256-gcm-encryption",
            "ed25519-signatures",
            "sha256-hashing",
            "secure-random"
        ],
        "features": {
            "maxPasswordLength": 128,
            "keySize": 256,
            "nonceSize": 96,
            "signatureSize": 512
        }
    });

    serde_wasm_bindgen::to_value(&info).unwrap()
}
