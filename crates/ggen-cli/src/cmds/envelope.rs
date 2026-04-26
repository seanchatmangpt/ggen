//! Envelope Commands
//!
//! `ggen envelope sign|verify|chain_verify` operate on the
//! `chatmangpt.receipt.envelope.v1` ReceiptEnvelope unifier. They are
//! intentionally separate from the `receipt` verbs so that the legacy
//! Receipt chain remains untouched.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ed25519_dalek::{SigningKey, VerifyingKey};
use ggen_receipt::{EnvelopeChain, ReceiptEnvelope};
use serde::Serialize;
use std::fs;
use std::path::PathBuf;

// ── Output Types ────────────────────────────────────────────────────────────

#[derive(Serialize)]
struct EnvelopeSignOutput {
    envelope_id: String,
    operation_id: String,
    output_file: String,
    own_hash: String,
    previous_envelope_hash: Option<String>,
    payload_hash: String,
    producer_system: String,
    producer_kind: String,
    chain_file: Option<String>,
    chain_length: Option<usize>,
}

#[derive(Serialize)]
struct EnvelopeVerifyOutput {
    envelope_file: String,
    is_valid: bool,
    message: String,
    envelope_id: Option<String>,
    operation_id: Option<String>,
    producer_system: Option<String>,
    producer_kind: Option<String>,
    payload_hash: Option<String>,
    own_hash: Option<String>,
    previous_envelope_hash: Option<String>,
}

#[derive(Serialize)]
struct EnvelopeChainVerifyOutput {
    chain_file: String,
    is_valid: bool,
    message: String,
    envelope_count: usize,
    genesis_operation: Option<String>,
    latest_operation: Option<String>,
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Read an Ed25519 signing key from `path` (hex-encoded, 32 raw bytes).
/// Mirrors the convention used by `read_signing_key` in `receipt.rs`.
fn read_signing_key(path: &PathBuf) -> Result<SigningKey, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read private key file: {}", e))?;
    let bytes = hex::decode(content.trim())
        .map_err(|e| format!("Failed to decode private key (expected hex): {}", e))?;
    if bytes.len() != 32 {
        return Err(format!(
            "Private key must be 32 bytes (got {})",
            bytes.len()
        ));
    }
    let arr: [u8; 32] = bytes
        .try_into()
        .map_err(|_| "Invalid signing key length".to_string())?;
    Ok(SigningKey::from_bytes(&arr))
}

/// Read an Ed25519 verifying key from `path` (hex- or base64-encoded, 32 bytes).
fn read_verifying_key(path: &PathBuf) -> Result<VerifyingKey, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read public key file: {}", e))?;
    let content = content.trim();
    if let Ok(bytes) = hex::decode(content) {
        if bytes.len() == 32 {
            return VerifyingKey::from_bytes(&bytes.try_into().unwrap())
                .map_err(|e| format!("Invalid verifying key: {}", e));
        }
    }
    use base64::Engine;
    if let Ok(bytes) = base64::engine::general_purpose::STANDARD.decode(content) {
        if bytes.len() == 32 {
            return VerifyingKey::from_bytes(&bytes.try_into().unwrap())
                .map_err(|e| format!("Invalid verifying key: {}", e));
        }
    }
    Err("Public key must be 32 bytes (hex or base64 encoded)".to_string())
}

// ── Domain helpers ─────────────────────────────────────────────────────────

/// Bundle of arguments for `do_sign`. Keeps the verb signature simple and
/// allows the verb to stay under the project's complexity-5 cap.
struct SignArgs {
    payload_path: String,
    payload_schema: String,
    producer_system: String,
    producer_kind: String,
    operation_id: String,
    envelope_id: String,
    private_key: String,
    public_key_ref: String,
    chain_file: Option<String>,
    output: Option<String>,
}

/// Read previous envelope hash (if any) from `chain_file`, returning the
/// chain so we can append later.
fn load_chain_for_link(
    chain_file: &Option<String>,
) -> Result<(Option<EnvelopeChain>, Option<String>), String> {
    match chain_file {
        None => Ok((None, None)),
        Some(p) => {
            let path = PathBuf::from(p);
            let chain = if path.exists() {
                let content = fs::read_to_string(&path)
                    .map_err(|e| format!("Failed to read chain file: {}", e))?;
                serde_json::from_str::<EnvelopeChain>(&content)
                    .map_err(|e| format!("Failed to parse envelope chain: {}", e))?
            } else {
                EnvelopeChain::new()
            };
            let prev = chain.last().map(|e| e.chain.own_hash.clone());
            Ok((Some(chain), prev))
        }
    }
}

/// Persist signed envelope to disk and (if `chain_file` is set) append it to
/// the chain. Returns `(output_path, Option<chain_length>)`.
fn persist_envelope(
    signed: &ReceiptEnvelope, output: Option<String>, chain_file: &Option<String>,
    chain: Option<EnvelopeChain>,
) -> Result<(String, Option<usize>), String> {
    let output_path = output
        .unwrap_or_else(|| format!(".portfolio/envelopes/{}.envelope.json", signed.envelope_id));
    if let Some(parent) = std::path::Path::new(&output_path).parent() {
        let _ = fs::create_dir_all(parent);
    }
    let envelope_json = serde_json::to_string_pretty(signed)
        .map_err(|e| format!("Failed to serialize envelope: {}", e))?;
    fs::write(&output_path, &envelope_json)
        .map_err(|e| format!("Failed to write envelope: {}", e))?;

    let chain_len = match (chain_file, chain) {
        (Some(p), Some(mut c)) => {
            c.append(signed.clone())
                .map_err(|e| format!("Failed to append envelope to chain: {}", e))?;
            let json = serde_json::to_string_pretty(&c)
                .map_err(|e| format!("Failed to serialize chain: {}", e))?;
            fs::write(p, &json).map_err(|e| format!("Failed to write chain file: {}", e))?;
            Some(c.len())
        }
        _ => None,
    };
    Ok((output_path, chain_len))
}

/// Domain-side: build, sign, persist. CLI verb stays thin.
fn do_sign(a: SignArgs) -> Result<EnvelopeSignOutput, String> {
    let payload_bytes =
        fs::read(&a.payload_path).map_err(|e| format!("Failed to read payload: {}", e))?;
    let phash = ggen_receipt::payload_hash(&payload_bytes);

    let signing_key = read_signing_key(&PathBuf::from(&a.private_key))?;
    let (chain, previous_envelope_hash) = load_chain_for_link(&a.chain_file)?;

    let env = ReceiptEnvelope::new(
        a.envelope_id.clone(),
        a.operation_id.clone(),
        ggen_receipt::Producer {
            system: a.producer_system.clone(),
            kind: a.producer_kind.clone(),
        },
        ggen_receipt::PayloadRef {
            schema: a.payload_schema,
            hash: phash.clone(),
            path: Some(a.payload_path.clone()),
        },
        previous_envelope_hash.clone(),
        a.public_key_ref,
    );

    let signed = env
        .sign(&signing_key)
        .map_err(|e| format!("Failed to sign envelope: {}", e))?;

    let (output_file, chain_length) = persist_envelope(&signed, a.output, &a.chain_file, chain)?;

    Ok(EnvelopeSignOutput {
        envelope_id: a.envelope_id,
        operation_id: a.operation_id,
        output_file,
        own_hash: signed.chain.own_hash.clone(),
        previous_envelope_hash,
        payload_hash: phash,
        producer_system: a.producer_system,
        producer_kind: a.producer_kind,
        chain_file: a.chain_file,
        chain_length,
    })
}

// ── Verbs ──────────────────────────────────────────────────────────────────

/// Sign a producer-native payload into a v1 ReceiptEnvelope (Ed25519, BLAKE3 chain)
#[verb]
fn sign(
    payload_path: String, payload_schema: String, producer_system: String, producer_kind: String,
    operation_id: String, envelope_id: String, private_key: String, public_key_ref: String,
    chain_file: Option<String>, output: Option<String>,
) -> VerbResult<EnvelopeSignOutput> {
    do_sign(SignArgs {
        payload_path,
        payload_schema,
        producer_system,
        producer_kind,
        operation_id,
        envelope_id,
        private_key,
        public_key_ref,
        chain_file,
        output,
    })
    .map_err(clap_noun_verb::NounVerbError::argument_error)
}

/// Verify a single ReceiptEnvelope's Ed25519 signature
#[verb]
fn verify(envelope_file: String, public_key: String) -> VerbResult<EnvelopeVerifyOutput> {
    let env_path = PathBuf::from(&envelope_file);
    if !env_path.exists() {
        return Ok(EnvelopeVerifyOutput {
            envelope_file: envelope_file.clone(),
            is_valid: false,
            message: format!("Envelope file not found: {}", envelope_file),
            envelope_id: None,
            operation_id: None,
            producer_system: None,
            producer_kind: None,
            payload_hash: None,
            own_hash: None,
            previous_envelope_hash: None,
        });
    }

    let content = fs::read_to_string(&env_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to read envelope file: {}",
            e
        ))
    })?;
    let env: ReceiptEnvelope = serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to parse envelope JSON: {}",
            e
        ))
    })?;

    let vk = read_verifying_key(&PathBuf::from(&public_key))
        .map_err(clap_noun_verb::NounVerbError::argument_error)?;

    let is_valid = env.verify(&vk).is_ok();
    let message = if is_valid {
        "Envelope signature verified successfully".to_string()
    } else {
        "Envelope signature verification failed".to_string()
    };

    Ok(EnvelopeVerifyOutput {
        envelope_file,
        is_valid,
        message,
        envelope_id: Some(env.envelope_id.clone()),
        operation_id: Some(env.operation_id.clone()),
        producer_system: Some(env.producer.system.clone()),
        producer_kind: Some(env.producer.kind.clone()),
        payload_hash: Some(env.payload.hash.clone()),
        own_hash: Some(env.chain.own_hash.clone()),
        previous_envelope_hash: env.chain.previous_envelope_hash.clone(),
    })
}

/// Verify an envelope chain end-to-end (signatures + BLAKE3 chain links)
#[verb]
fn chain_verify(chain_file: String, public_key: String) -> VerbResult<EnvelopeChainVerifyOutput> {
    let chain_path = PathBuf::from(&chain_file);
    if !chain_path.exists() {
        return Ok(EnvelopeChainVerifyOutput {
            chain_file: chain_file.clone(),
            is_valid: false,
            message: format!("Chain file not found: {}", chain_file),
            envelope_count: 0,
            genesis_operation: None,
            latest_operation: None,
        });
    }

    let content = fs::read_to_string(&chain_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to read chain file: {}", e))
    })?;
    let chain: EnvelopeChain = serde_json::from_str(&content).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to parse envelope chain: {}",
            e
        ))
    })?;

    let vk = read_verifying_key(&PathBuf::from(&public_key))
        .map_err(clap_noun_verb::NounVerbError::argument_error)?;

    let is_valid = chain.verify(&vk).is_ok();
    let message = if is_valid {
        format!(
            "Envelope chain verified successfully: {} envelopes",
            chain.len()
        )
    } else {
        format!(
            "Envelope chain verification failed: {} envelopes",
            chain.len()
        )
    };
    let genesis = chain.genesis();
    let latest = chain.last();

    Ok(EnvelopeChainVerifyOutput {
        chain_file,
        is_valid,
        message,
        envelope_count: chain.len(),
        genesis_operation: genesis.map(|e| e.operation_id.clone()),
        latest_operation: latest.map(|e| e.operation_id.clone()),
    })
}
