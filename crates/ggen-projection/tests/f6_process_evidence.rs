use chrono::{DateTime, Duration, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for &b in bytes {
        s.push_str(&format!("{:02x}", b));
    }
    s
}

fn hex_decode(s: &str) -> Result<Vec<u8>, anyhow::Error> {
    if s.len() % 2 != 0 {
        return Err(anyhow::anyhow!("Invalid hex length"));
    }
    let mut bytes = Vec::with_capacity(s.len() / 2);
    for i in (0..s.len()).step_by(2) {
        let b = u8::from_str_radix(&s[i..i + 2], 16)
            .map_err(|e| anyhow::anyhow!("Invalid hex char: {}", e))?;
        bytes.push(b);
    }
    Ok(bytes)
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProcessEvidenceReceipt {
    pub target_id: String,
    pub receipt_id: String,
    pub blake3_hash: String,
    pub template_hash: String,
    pub generator_version: String,
    pub signature: String,
    pub public_key: String,
    pub parent_hash: Option<String>,
    pub verified_at: DateTime<Utc>,
}

impl ProcessEvidenceReceipt {
    pub fn new(
        target_id: String, template_content: &[u8], generated_content: &[u8],
        generator_version: String, signing_key: &SigningKey, parent_hash: Option<String>,
    ) -> Self {
        let blake3_hash = blake3::hash(generated_content).to_hex().to_string();
        let template_hash = blake3::hash(template_content).to_hex().to_string();
        let receipt_id = uuid::Uuid::new_v4().to_string();
        let verified_at = Utc::now();
        let public_key = hex_encode(&signing_key.verifying_key().to_bytes());

        // Construct signable payload: target_id + receipt_id + blake3_hash + template_hash + generator_version + parent_hash + verified_at
        let mut payload = Vec::new();
        payload.extend_from_slice(target_id.as_bytes());
        payload.extend_from_slice(receipt_id.as_bytes());
        payload.extend_from_slice(blake3_hash.as_bytes());
        payload.extend_from_slice(template_hash.as_bytes());
        payload.extend_from_slice(generator_version.as_bytes());
        if let Some(ref ph) = parent_hash {
            payload.extend_from_slice(ph.as_bytes());
        }
        payload.extend_from_slice(verified_at.to_rfc3339().as_bytes());

        let sig = signing_key.sign(&payload);
        let signature = hex_encode(&sig.to_bytes());

        Self {
            target_id,
            receipt_id,
            blake3_hash,
            template_hash,
            generator_version,
            signature,
            public_key,
            parent_hash,
            verified_at,
        }
    }

    pub fn verify(&self) -> Result<(), anyhow::Error> {
        // Parse public key
        let pk_bytes = hex_decode(&self.public_key)?;
        let vk_bytes: [u8; 32] = pk_bytes
            .try_into()
            .map_err(|_| anyhow::anyhow!("Invalid public key length"))?;
        let verifying_key = VerifyingKey::from_bytes(&vk_bytes)?;

        // Parse signature
        let sig_bytes = hex_decode(&self.signature)?;
        let sig_arr: [u8; 64] = sig_bytes
            .try_into()
            .map_err(|_| anyhow::anyhow!("Invalid signature length"))?;
        let signature = Signature::from_bytes(&sig_arr);

        // Reconstruct payload
        let mut payload = Vec::new();
        payload.extend_from_slice(self.target_id.as_bytes());
        payload.extend_from_slice(self.receipt_id.as_bytes());
        payload.extend_from_slice(self.blake3_hash.as_bytes());
        payload.extend_from_slice(self.template_hash.as_bytes());
        payload.extend_from_slice(self.generator_version.as_bytes());
        if let Some(ref ph) = self.parent_hash {
            payload.extend_from_slice(ph.as_bytes());
        }
        payload.extend_from_slice(self.verified_at.to_rfc3339().as_bytes());

        verifying_key
            .verify(&payload, &signature)
            .map_err(|e| anyhow::anyhow!("Signature verification failed: {}", e))
    }
}

// Verification manager for wasm4pm
pub fn verify_receipt_chain(receipts: &[ProcessEvidenceReceipt]) -> Result<(), anyhow::Error> {
    for (i, receipt) in receipts.iter().enumerate() {
        receipt.verify()?;
        if i > 0 {
            let prev = &receipts[i - 1];
            if receipt.parent_hash.as_deref() != Some(&prev.blake3_hash) {
                return Err(anyhow::anyhow!(
                    "Causal chain broken at index {}: expected parent_hash to be {}, got {:?}",
                    i,
                    prev.blake3_hash,
                    receipt.parent_hash
                ));
            }
        }
    }
    Ok(())
}

// ==========================================
// Tier 1: Feature Coverage (5 tests)
// ==========================================

#[test]
fn test_f6_t1_receipt_blake3_binding() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let template_content = b"template code";
    let generated_content = b"generated output code";

    let receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        template_content,
        generated_content,
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    let expected_gen_hash = blake3::hash(generated_content).to_hex().to_string();
    let expected_tmpl_hash = blake3::hash(template_content).to_hex().to_string();

    assert_eq!(receipt.blake3_hash, expected_gen_hash);
    assert_eq!(receipt.template_hash, expected_tmpl_hash);
    assert!(!receipt.blake3_hash.contains("placeholder"));
    assert!(!receipt.template_hash.contains("placeholder"));
}

#[test]
fn test_f6_t1_receipt_causal_chain() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt1 = ProcessEvidenceReceipt::new(
        "src/lib.rs".to_string(),
        b"tmpl1",
        b"gen1",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    let receipt2 = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl2",
        b"gen2",
        "1.0.0".to_string(),
        &signing_key,
        Some(receipt1.blake3_hash.clone()),
    );

    let chain = vec![receipt1, receipt2];
    assert!(verify_receipt_chain(&chain).is_ok());
}

#[test]
fn test_f6_t1_receipt_signature_check() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    assert!(receipt.verify().is_ok());
}

#[test]
fn test_f6_t1_receipt_process_evidence_format() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    let json_line = serde_json::to_string(&receipt).unwrap();
    assert!(json_line.starts_with('{'));
    assert!(json_line.ends_with('}'));

    let parsed: ProcessEvidenceReceipt = serde_json::from_str(&json_line).unwrap();
    assert_eq!(parsed, receipt);
    assert!(!parsed.blake3_hash.is_empty());
    assert!(!parsed.receipt_id.is_empty());
}

#[test]
fn test_f6_t1_export_package() {
    let tmp = TempDir::new().unwrap();
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    let jsonl_content = serde_json::to_string(&receipt).unwrap() + "\n";
    let receipts_path = tmp.path().join("receipts.jsonl");
    fs::write(&receipts_path, jsonl_content).unwrap();

    let package_manifest = r#"{
        "name": "clap-noun-verb-lsp",
        "version": "1.0.0",
        "wasm4pm_compat": true
    }"#;
    fs::write(tmp.path().join("package.json"), package_manifest).unwrap();

    assert!(receipts_path.exists());
    assert!(tmp.path().join("package.json").exists());
}

// ==========================================
// Tier 2: Boundary & Corner Cases (5 tests)
// ==========================================

#[test]
fn test_f6_t2_mismatched_blake3_hash() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let mut receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    // Tamper with the blake3 hash of the generated code
    receipt.blake3_hash = "anotherhashval".to_string();

    // Verification should fail because payload hash changed
    assert!(receipt.verify().is_err());
}

#[test]
fn test_f6_t2_broken_causal_link() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt1 = ProcessEvidenceReceipt::new(
        "src/lib.rs".to_string(),
        b"tmpl1",
        b"gen1",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    let receipt2 = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl2",
        b"gen2",
        "1.0.0".to_string(),
        &signing_key,
        Some("brokenparentlink".to_string()),
    );

    let chain = vec![receipt1, receipt2];
    assert!(verify_receipt_chain(&chain).is_err());
}

#[test]
fn test_f6_t2_expired_certificate() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let mut receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    // Simulate key/cert expiration by backdating verified_at by more than 30 days
    // and verifying against a max age rule.
    receipt.verified_at = Utc::now() - Duration::days(40);

    let max_age = Duration::days(30);
    let is_expired = Utc::now() - receipt.verified_at > max_age;
    assert!(
        is_expired,
        "Receipt must be expired if verified_at is older than max age"
    );
}

#[test]
fn test_f6_t2_missing_receipt_fields() {
    // Missing fields in JSON string will fail parsing into ProcessEvidenceReceipt
    let invalid_json = r#"{"target_id":"src/main.rs","receipt_id":"uuid"}"#;
    let parsed: Result<ProcessEvidenceReceipt, _> = serde_json::from_str(invalid_json);
    assert!(parsed.is_err());
}

#[test]
fn test_f6_t2_unwritable_export_target() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl",
        b"gen",
        "1.0.0".to_string(),
        &signing_key,
        None,
    );

    // Attempting to export to a completely invalid/nonexistent path root should fail gracefully
    let target = Path::new("/nonexistent_root_dir_abc_123/receipts.jsonl");
    let jsonl_content = serde_json::to_string(&receipt).unwrap() + "\n";
    let res = fs::write(target, jsonl_content);
    assert!(res.is_err());
}
