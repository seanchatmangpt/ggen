# ggen-firewall

Life Firewall: 3 Ingress Channels Only

## Overview

Controls all ingress through exactly 3 channels:
- **Batch**: Scheduled bulk operations with rate limits (100 max batch size, 10 requests/minute)
- **Scheduled**: Time-windowed planned operations
- **Emergency**: Circuit-breaker bypass for critical operations

Everything else refused with cryptographic receipts. No negotiation.

## Architecture

```
src/
├── lib.rs         - Core Firewall struct, IngressChannel enum, traits
├── channels.rs    - Channel implementations (Batch, Scheduled, Emergency)
├── admission.rs   - Admission control at boundary
└── refusal.rs     - Cryptographic refusal receipts

tests/
└── boundary_tests.rs - Boundary enforcement tests
```

## Usage

```rust
use ggen_firewall::{Firewall, IngressRequest, IngressChannel, AdmissionResponse};

#[tokio::main]
async fn main() {
    let mut firewall = Firewall::with_defaults();

    let request = IngressRequest::new(
        IngressChannel::Batch,
        vec![1, 2, 3]
    );

    match firewall.process(request).await {
        AdmissionResponse::Admitted { request_id, channel, admitted_at } => {
            println!("Admitted: {:?} via {}", request_id, channel);
        }
        AdmissionResponse::Refused { request_id, reason, receipt } => {
            println!("Refused: {:?} - {}", request_id, reason);
            println!("Receipt: {}", receipt.to_json().unwrap());
        }
    }
}
```

## Features

- **3 Channels Only**: Type system enforces only Batch/Scheduled/Emergency
- **Rate Limiting**: Batch channel enforces configurable rate limits
- **Time Windows**: Scheduled channel enforces time-based admission
- **Emergency Bypass**: Emergency channel for critical operations
- **Cryptographic Receipts**: SHA-256 signed refusal receipts
- **Zero Unwrap**: All operations return `Result<T, E>`
- **Async/Await**: Built on Tokio runtime

## Admission Rules

All requests validated at boundary:
- Payload must not be empty
- Payload size ≤ 10MB (configurable)
- Timestamp not in future
- Timestamp not older than 24 hours

## Refusal Receipts

Refused requests receive cryptographic receipts with:
- Request ID
- Channel
- Reason for refusal
- Timestamp
- SHA-256 payload hash
- SHA-256 signature
- Receipt ID

Receipts are:
- Verifiable via `receipt.verify()`
- Serializable to JSON
- Tamper-evident

## Testing

```bash
cargo test --package ggen-firewall
```

Tests verify:
- Only 3 channels exist (compile-time)
- Admission rules enforced
- Rate limits respected
- Refusal receipts generated
- Cryptographic signatures valid

## Dependencies

- tokio: Async runtime
- async-trait: Async trait definitions
- serde/serde_json: Serialization
- chrono: Timestamps
- thiserror: Error types
- uuid: Request IDs
- sha2: Cryptographic hashing
- hex: Hash encoding
