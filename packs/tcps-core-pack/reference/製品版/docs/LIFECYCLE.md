# Full Lifecycle

```text
observe → admit → verify → matrix → build → test → benchmark → package → SBOM → provenance → sign → publish → replay
```

## Observe

`doctor` records toolchain, host, SDK helpers, signing tools, and runners.

## Admit

The source tree is rejected for missing canonical modules, unsafe code in the universal core, placeholders, malformed delimiters, or missing Japanese semantic vocabulary.

## Verify

The host path runs formatting, checking, tests, Clippy correctness/suspicious/performance lints, and documentation.

## Matrix

The active compiler supplies the complete target list. Official tier facts and external SDK requirements are joined onto each target.

## Build

Core is always the first product. Other products are pulled only when the target supports them. Tier 3 may use `build-std=core` but remains a distinct standing.

## Test

Tests run natively, through Wasmtime, Wine, or QEMU when an approved runner is detected. Hardware-only targets return `not-applicable` or `runtime-unavailable`, never a fabricated pass.

## Benchmark

The benchmark receipt applies only to the exact source digest, compiler, host, cache condition, iteration count, and timer boundary recorded by the invocation.

## Package

Each target package contains libraries, C header, licenses, README files, artifact digests, and target metadata. ZIP and tar.gz are universal containers; platform packaging may wrap them further.

## Supply chain

CycloneDX, SPDX, in-toto/SLSA, SHA-256, and optional signatures are generated after artifacts exist. Release verification re-hashes every subject.
