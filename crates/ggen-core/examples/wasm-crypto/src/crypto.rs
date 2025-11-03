//! Core cryptographic operations implementation

use argon2::{Argon2, PasswordHasher, PasswordVerifier, PasswordHash};
use argon2::password_hash::SaltString;
use aes_gcm::{
    aead::{Aead, KeyInit, OsRng},
    Aes256Gcm, Nonce, Key
};
use ed25519_dalek::{Signer, SigningKey, Verifier, VerifyingKey, Signature};
use sha2::{Sha256, Digest};
use base64::{Engine as _, engine::general_purpose};
use serde::{Deserialize, Serialize};
use getrandom::getrandom;

#[derive(Debug)]
pub enum CryptoError {
    HashError(String),
    EncryptionError(String),
    DecryptionError(String),
    SignatureError(String),
    RandomError(String),
    InvalidInput(String),
}

impl std::fmt::Display for CryptoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CryptoError::HashError(e) => write!(f, "Hash error: {}", e),
            CryptoError::EncryptionError(e) => write!(f, "Encryption error: {}", e),
            CryptoError::DecryptionError(e) => write!(f, "Decryption error: {}", e),
            CryptoError::SignatureError(e) => write!(f, "Signature error: {}", e),
            CryptoError::RandomError(e) => write!(f, "Random generation error: {}", e),
            CryptoError::InvalidInput(e) => write!(f, "Invalid input: {}", e),
        }
    }
}

impl std::error::Error for CryptoError {}

type Result<T> = std::result::Result<T, CryptoError>;

/// Argon2 password hashing
pub fn hash_password_argon2(password: &str, salt: Option<Vec<u8>>) -> Result<String> {
    let argon2 = Argon2::default();

    let salt = match salt {
        Some(s) => SaltString::encode_b64(&s)
            .map_err(|e| CryptoError::HashError(format!("Invalid salt: {}", e)))?,
        None => {
            let mut salt_bytes = [0u8; 16];
            getrandom(&mut salt_bytes)
                .map_err(|e| CryptoError::RandomError(e.to_string()))?;
            SaltString::encode_b64(&salt_bytes)
                .map_err(|e| CryptoError::HashError(format!("Salt generation failed: {}", e)))?
        }
    };

    let password_hash = argon2.hash_password(password.as_bytes(), &salt)
        .map_err(|e| CryptoError::HashError(e.to_string()))?;

    Ok(password_hash.to_string())
}

pub fn verify_password_argon2(password: &str, hash: &str) -> Result<bool> {
    let parsed_hash = PasswordHash::new(hash)
        .map_err(|e| CryptoError::HashError(format!("Invalid hash format: {}", e)))?;

    let argon2 = Argon2::default();

    match argon2.verify_password(password.as_bytes(), &parsed_hash) {
        Ok(_) => Ok(true),
        Err(_) => Ok(false),
    }
}

/// AES-256-GCM encryption/decryption
#[derive(Serialize, Deserialize)]
pub struct EncryptedData {
    pub ciphertext: String,
    pub nonce: String,
}

pub fn encrypt_aes_gcm(data: &[u8], key: &[u8], nonce: Option<Vec<u8>>) -> Result<EncryptedData> {
    if key.len() != 32 {
        return Err(CryptoError::InvalidInput("Key must be 32 bytes".to_string()));
    }

    let key = Key::<Aes256Gcm>::from_slice(key);
    let cipher = Aes256Gcm::new(key);

    let nonce_bytes = match nonce {
        Some(n) => {
            if n.len() != 12 {
                return Err(CryptoError::InvalidInput("Nonce must be 12 bytes".to_string()));
            }
            n
        },
        None => {
            let mut n = vec![0u8; 12];
            getrandom(&mut n)
                .map_err(|e| CryptoError::RandomError(e.to_string()))?;
            n
        }
    };

    let nonce = Nonce::from_slice(&nonce_bytes);

    let ciphertext = cipher.encrypt(nonce, data)
        .map_err(|e| CryptoError::EncryptionError(e.to_string()))?;

    Ok(EncryptedData {
        ciphertext: general_purpose::STANDARD.encode(&ciphertext),
        nonce: general_purpose::STANDARD.encode(&nonce_bytes),
    })
}

pub fn decrypt_aes_gcm(ciphertext: &str, key: &[u8], nonce: &str) -> Result<Vec<u8>> {
    if key.len() != 32 {
        return Err(CryptoError::InvalidInput("Key must be 32 bytes".to_string()));
    }

    let key = Key::<Aes256Gcm>::from_slice(key);
    let cipher = Aes256Gcm::new(key);

    let ciphertext_bytes = general_purpose::STANDARD.decode(ciphertext)
        .map_err(|e| CryptoError::DecryptionError(format!("Invalid ciphertext encoding: {}", e)))?;

    let nonce_bytes = general_purpose::STANDARD.decode(nonce)
        .map_err(|e| CryptoError::DecryptionError(format!("Invalid nonce encoding: {}", e)))?;

    if nonce_bytes.len() != 12 {
        return Err(CryptoError::InvalidInput("Nonce must be 12 bytes".to_string()));
    }

    let nonce = Nonce::from_slice(&nonce_bytes);

    let plaintext = cipher.decrypt(nonce, ciphertext_bytes.as_ref())
        .map_err(|e| CryptoError::DecryptionError(e.to_string()))?;

    Ok(plaintext)
}

/// Ed25519 digital signatures
#[derive(Serialize, Deserialize)]
pub struct Keypair {
    #[serde(rename = "publicKey")]
    pub public_key: String,
    #[serde(rename = "secretKey")]
    pub secret_key: String,
}

pub fn generate_ed25519_keypair() -> Result<Keypair> {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();

    Ok(Keypair {
        public_key: general_purpose::STANDARD.encode(verifying_key.as_bytes()),
        secret_key: general_purpose::STANDARD.encode(signing_key.as_bytes()),
    })
}

pub fn sign_ed25519(message: &[u8], secret_key: &str) -> Result<String> {
    let key_bytes = general_purpose::STANDARD.decode(secret_key)
        .map_err(|e| CryptoError::SignatureError(format!("Invalid secret key: {}", e)))?;

    let key_array: [u8; 32] = key_bytes.try_into()
        .map_err(|_| CryptoError::SignatureError("Secret key must be 32 bytes".to_string()))?;

    let signing_key = SigningKey::from_bytes(&key_array);
    let signature = signing_key.sign(message);

    Ok(general_purpose::STANDARD.encode(signature.to_bytes()))
}

pub fn verify_ed25519(message: &[u8], signature: &str, public_key: &str) -> Result<bool> {
    let sig_bytes = general_purpose::STANDARD.decode(signature)
        .map_err(|e| CryptoError::SignatureError(format!("Invalid signature: {}", e)))?;

    let key_bytes = general_purpose::STANDARD.decode(public_key)
        .map_err(|e| CryptoError::SignatureError(format!("Invalid public key: {}", e)))?;

    let key_array: [u8; 32] = key_bytes.try_into()
        .map_err(|_| CryptoError::SignatureError("Public key must be 32 bytes".to_string()))?;

    let sig_array: [u8; 64] = sig_bytes.try_into()
        .map_err(|_| CryptoError::SignatureError("Signature must be 64 bytes".to_string()))?;

    let verifying_key = VerifyingKey::from_bytes(&key_array)
        .map_err(|e| CryptoError::SignatureError(format!("Invalid public key: {}", e)))?;

    let signature = Signature::from_bytes(&sig_array);

    match verifying_key.verify(message, &signature) {
        Ok(_) => Ok(true),
        Err(_) => Ok(false),
    }
}

/// Secure random generation
pub fn generate_random_bytes(length: usize) -> Result<Vec<u8>> {
    let mut bytes = vec![0u8; length];
    getrandom(&mut bytes)
        .map_err(|e| CryptoError::RandomError(e.to_string()))?;
    Ok(bytes)
}

/// SHA-256 hashing
pub fn hash_sha256(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    general_purpose::STANDARD.encode(result)
}
