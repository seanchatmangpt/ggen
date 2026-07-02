# ggen-config

Configuration loader, parser, schema validator, and cryptographic receipt manager for the `ggen` framework.

This crate manages the load and validation cycles of `ggen.toml` workspace files, environment variable expansions, and the cryptographic chain-of-custody ledger for system configurations.

## Features

- **Type-Safe Configuration**: Deserializes `ggen.toml` configurations into structured models covering:
  - `AiConfig`: LLM provider, credentials, temperature, and models.
  - `McpConfig`: Stdio/TLS transports, tool registries, and server mappings.
  - `A2AConfig`: Peer messaging endpoints, transport ports, and retry policies.
  - `TelemetryConfig`: OTel logging, Jaeger exporters, and tracing configurations.
- **Environment Expansion**: Dynamically resolves `${VAR}` environment strings during configuration parsing.
- **Config Validator**: Rejects malformed configuration options early (checks URL forms, certificate paths, port ranges).
- **Cryptographic Receipt Envelopes**: Integrates a domain-agnostic `ReceiptEnvelope` ledger implementing Ed25519 signatures and BLAKE3 hash chains (`EnvelopeChain`) to verify the provenance of configurations and outputs.

## Usage

### 1. Loading and Validating a Project Configuration

```rust
use ggen_config::{ConfigLoader, ConfigValidator, GgenConfig};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config_path = Path::new("ggen.toml");

    // 1. Load the TOML file and expand environment variables
    let config: GgenConfig = ConfigLoader::load_from_file(config_path)?;

    // 2. Validate configuration constraints
    let validator = ConfigValidator::new();
    if let Err(e) = validator.validate(&config) {
        eprintln!("Configuration validation errors: {:?}", e);
    } else {
        println!("Configuration is valid. Project name: {}", config.project.name);
    }

    Ok(())
}
```

### 2. Creating and Signing a Receipt Envelope

```rust
use ggen_config::{
    ReceiptEnvelope, Producer, PayloadRef,
    generate_keypair, payload_hash
};
use ed25519_dalek::{SigningKey, VerifyingKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Generate Ed25519 keypair for signing
    let (signing_key, verifying_key): (SigningKey, VerifyingKey) = generate_keypair();

    // 2. Define the payload reference
    let config_toml_bytes = b"[project]\nname = \"my-project\"\n";
    let hash = payload_hash(config_toml_bytes);
    let payload = PayloadRef {
        schema: "chatmangpt.ggen.config.v1".to_string(),
        hash,
        path: Some("ggen.toml".to_string()),
    };

    // 3. Create the unsigned envelope
    let producer = Producer {
        system: "ggen".to_string(),
        kind: "config".to_string(),
    };
    let envelope = ReceiptEnvelope::new(
        "recenv-mcpp-001",    // envelope ID
        "op-construction-99", // operation ID
        producer,
        payload,
        None,                 // previous envelope hash (genesis)
        "key-ref-public-1",   // public key reference ID
    );

    // 4. Sign and finalize the envelope
    let signed_envelope = envelope.sign(&signing_key)?;
    assert!(signed_envelope.verify(&verifying_key).is_ok());

    println!("Signed Envelope Hash: {}", signed_envelope.chain.own_hash);
    Ok(())
}
```

## Architecture

- **`src/config_lib/`**: Type definitions (`schema.rs`), loaders (`parser.rs`), and validation criteria (`validator.rs`).
- **`src/receipt/`**: Implements the causal chain envelope primitives:
  - `envelope.rs`: `ReceiptEnvelope` formatting, Ed25519 signing, and validation.
  - `chain.rs`: Verification of the backward-linked BLAKE3 hash chains (`EnvelopeChain`).
