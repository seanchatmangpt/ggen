// STATUS: DORMANT — preserved per Non-Deletion Completion Protocol
// PATTERN: no_std + pure BLAKE3 genesis kernel
// ADMISSION_PATH: This crate provides the no_std / wasm32-unknown-unknown build
//   of the Genesis kernel. Once genesis-core-v2 gains no_std support, this
//   crate becomes a feature-gated compatibility surface.
//
// DO NOT DELETE. The no_std + pure blake3 feature combination is required for
// WASM targets where std is unavailable (wasm32-unknown-unknown, bare metal).
//
// See: crates/genesis-core-v2/ for the LIVE full-std implementation.
// See: docs/interop/DORMANT_CODE_REGISTER.md for policy.

#![no_std]
// This crate is intentionally minimal — a seed for the no_std genesis kernel.
// The full implementation lives in genesis-core-v2.
