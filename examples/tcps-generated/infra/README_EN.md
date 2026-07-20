# Toyota Code Production System — Production Edition v26.7.19

A production-engineered Rust workspace that maps the original Toyota Production System directly from its Japanese historical and production vocabulary into types, state transitions, conservation laws, and receipts. The canonical domain language remains Japanese; English is documentation and interoperability surface only.

> Independent research and implementation. This is not an official publication or product of Toyota Motor Corporation.

## Pinned compiler

The lifecycle pins Rust 1.97.1. This point release repairs an LLVM miscompilation reported in Rust 1.97.0; production builds must not silently fall back to 1.97.0.

## Product boundary

```text
Japanese TPS semantic system
    ↓
tcps-core          no_std, dependency-free universal core
    ├── tcps-std   environment observation and receipt persistence
    ├── tcps-ffi   stable C ABI, static and dynamic libraries
    ├── tcps-wasm  WebAssembly capability boundary
    └── tcps-cli   gemba inspection and benchmark executable
```

The governing laws are:

```text
Original problem → invention → invariant → standardized work → execution → abnormality → stop → root cause → countermeasure → revised standard
Selection ≠ authorization ≠ actuation
```

## Every Rust target, without false equivalence

The lifecycle discovers the current compiler's complete built-in target set using:

```bash
rustc --print target-list
```

It then applies Rust's official tier model:

- Tier 1: build and test are guaranteed by the Rust project.
- Tier 2: build is guaranteed; runtime testing is not universally guaranteed.
- Tier 3: the target exists in the compiler but carries no project guarantee.

`tcps-core` is attempted for every discovered target. CLI, C ABI, WebAssembly, Apple frameworks, Android archives, and native tests are added only where the platform has the required standard library, linker, SDK, and runner. Missing SDK, missing runtime, unavailable target, timeout, and compiler refusal remain distinct receipted outcomes.

## Start

Unix:

```bash
./scripts/bootstrap.sh
./scripts/full-lifecycle.sh
```

Windows PowerShell:

```powershell
./scripts/bootstrap.ps1
./scripts/full-lifecycle.ps1
```

Complete built-in-target core sweep:

```bash
./scripts/full-lifecycle-all-targets.sh
```

```powershell
./scripts/full-lifecycle-all-targets.ps1
```

Individual lifecycle stations:

```bash
python3 tools/lifecycle.py doctor
python3 tools/lifecycle.py verify
python3 tools/lifecycle.py matrix
python3 tools/lifecycle.py build --target x86_64-unknown-linux-gnu --product core
python3 tools/lifecycle.py test --target x86_64-unknown-linux-gnu
python3 tools/lifecycle.py benchmark --iterations 1000000
python3 tools/lifecycle.py package --target x86_64-unknown-linux-gnu
python3 tools/lifecycle.py sbom
python3 tools/lifecycle.py provenance
python3 tools/lifecycle.py checksums
python3 tools/lifecycle.py sign --method cosign
python3 tools/lifecycle.py verify-release
```

## Build matrices

All Tier 1 and Tier 2 core targets:

```bash
python3 tools/lifecycle.py build-all --tiers 1,2 --product core
```

One of sixteen Tier 3 best-effort shards:

```bash
python3 tools/lifecycle.py build-all --tiers 3 --product core --shard-index 0 --shard-count 16
```

For Tier 3, the lifecycle can fall back to an installed nightly toolchain with `rust-src` and `-Z build-std=core`. A failed build never becomes a successful receipt.

## Supply-chain outputs

Every lifecycle station emits JSON evidence under `receipts/`. Release output supports:

- versioned target matrix;
- per-target ZIP and tar.gz archives;
- SHA-256 manifest;
- CycloneDX 1.5 and SPDX 2.3 SBOMs;
- in-toto Statement with SLSA provenance predicate;
- optional GPG, minisign, or cosign signatures.

The core includes a dependency-free `no_std` SHA-256 implementation for production identity. The older `簡易要約` function remains a compatibility-only non-cryptographic digest and is not used at the release trust boundary.

## Current standing

The artifact-generation environment had no Rust toolchain and could not resolve the rustup download host. Structural verification, Python lifecycle execution, target classification, and archive integrity were completed. Compilation, Clippy, Rust tests, ABI generation, and per-target build standing must be established by the included CI or an approved Rust 1.97.1 environment.

## License

MIT OR Apache-2.0

