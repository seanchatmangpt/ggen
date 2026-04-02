//! WebAssembly tests using wasm-bindgen-test

#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;
use wasm_crypto::*;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn test_password_hashing() {
    let password = "secure_password_123";
    let hash = hash_password(password, None).unwrap();

    assert!(!hash.is_empty());
    assert!(hash.starts_with("$argon2"));

    // Verify correct password
    assert!(verify_password(password, &hash).unwrap());

    // Verify incorrect password
    assert!(!verify_password("wrong_password", &hash).unwrap());
}

#[wasm_bindgen_test]
fn test_aes_encryption() {
    let key = generate_key().unwrap();
    let plaintext = b"Hello, WASM world!";

    // Encrypt
    let encrypted = encrypt_aes(plaintext, &key, None).unwrap();

    // Decrypt
    let ciphertext = js_sys::Reflect::get(&encrypted, &"ciphertext".into())
        .unwrap()
        .as_string()
        .unwrap();
    let nonce = js_sys::Reflect::get(&encrypted, &"nonce".into())
        .unwrap()
        .as_string()
        .unwrap();

    let decrypted = decrypt_aes(&ciphertext, &key, &nonce).unwrap();

    assert_eq!(plaintext, decrypted.as_slice());
}

#[wasm_bindgen_test]
fn test_digital_signatures() {
    let keypair = generate_keypair().unwrap();
    let message = b"Sign this message";

    let public_key = js_sys::Reflect::get(&keypair, &"publicKey".into())
        .unwrap()
        .as_string()
        .unwrap();
    let secret_key = js_sys::Reflect::get(&keypair, &"secretKey".into())
        .unwrap()
        .as_string()
        .unwrap();

    // Sign
    let signature = sign_message(message, &secret_key).unwrap();

    // Verify
    assert!(verify_signature(message, &signature, &public_key).unwrap());

    // Verify with wrong message
    assert!(!verify_signature(b"Wrong message", &signature, &public_key).unwrap());
}

#[wasm_bindgen_test]
fn test_random_generation() {
    let bytes1 = random_bytes(32).unwrap();
    let bytes2 = random_bytes(32).unwrap();

    assert_eq!(bytes1.len(), 32);
    assert_eq!(bytes2.len(), 32);
    assert_ne!(bytes1, bytes2); // Should be different
}

#[wasm_bindgen_test]
fn test_sha256_hashing() {
    let data = b"Hash this data";
    let hash1 = hash_sha256(data);
    let hash2 = hash_sha256(data);

    assert!(!hash1.is_empty());
    assert_eq!(hash1, hash2); // Deterministic

    let different_hash = hash_sha256(b"Different data");
    assert_ne!(hash1, different_hash);
}

#[wasm_bindgen_test]
fn test_module_info() {
    let info = get_info();

    let name = js_sys::Reflect::get(&info, &"name".into())
        .unwrap()
        .as_string()
        .unwrap();

    assert_eq!(name, "wasm-crypto");
}

#[wasm_bindgen_test]
fn test_error_handling() {
    // Test invalid key size
    let invalid_key = vec![0u8; 16]; // Should be 32
    let plaintext = b"test";

    assert!(encrypt_aes(plaintext, &invalid_key, None).is_err());
}
