# `crates/ggen-config/src/receipt/envelope.rs`

Source SHA-256: `943822986fb831e02df9075a49a4cb713358aab0fb5f9e5bcfbfa379d4db6e1b`

```mermaid
classDiagram
    class struct_Producer {
      <<struct>>
      +"system: String"
      +"kind: String"
    }
    class struct_PayloadRef {
      <<struct>>
      +"schema: String"
      +"hash: String"
      +"path: Option~String~"
    }
    class struct_EnvelopeSignature {
      <<struct>>
      +"algorithm: String"
      +"public_key_ref: String"
      +"value: String"
    }
    class struct_EnvelopeChainLink {
      <<struct>>
      +"previous_envelope_hash: Option~String~"
      +"own_hash: String"
    }
    class struct_ReceiptEnvelope {
      <<struct>>
      +"schema: String"
      +"envelope_id: String"
      +"operation_id: String"
      +"timestamp: DateTime~Utc~"
      +"producer: Producer"
      +"payload: PayloadRef"
      +"signature: EnvelopeSignature"
      +"chain: EnvelopeChainLink"
    }
    class fn_payload_hash {
      <<fn>>
    }
    class struct_EnvelopeChain {
      <<struct>>
      +"envelopes: Vec~ReceiptEnvelope~"
    }
    class mod_tests {
      <<mod>>
    }
    note "EnvelopeChain"
    note "ReceiptEnvelope"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `crate::error::{ReceiptError, Result}`
- `crate::generate_keypair`
- `ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey}`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
