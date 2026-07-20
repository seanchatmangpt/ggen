# Security Model

## Trust boundaries

- `tcps-core` has no filesystem, network, clock, process, allocator, dynamic loading, or tool-execution capability.
- `tcps-std` may observe the host and atomically persist receipts.
- `tcps-ffi` and `tcps-wasm` are serialization and ABI boundaries, not authority boundaries.
- `青い川のダム` separates proposal, authorization, and actuation.
- No adapter may construct `許可済み選択` except through `仲介者::許可する`.

## Cryptography

Production source, policy, artifact, SBOM, and provenance identity uses SHA-256. `簡易要約` is non-cryptographic and retained only for source compatibility.

## Unsafe code

The universal core forbids unsafe code. FFI and WASM crates permit the Rust 2024 unsafe export attributes required to create external symbols, but contain no unsafe blocks or unsafe functions.

## Reporting

Do not publish vulnerabilities before a fix and negative fixture exist. A security repair must identify the originating abnormality, failed invariant, countermeasure, superseded standard, revised standard, and replay evidence.
