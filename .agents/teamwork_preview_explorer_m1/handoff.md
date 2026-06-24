# Handoff Report: Ecosystem Cataloging & System Design for Praxis active self-healing & validation

## 1. Observation

During read-only exploration of the `/Users/sac/praxis` workspace, the following components and files were observed:

### A. Template and Script Baseline
* **Workspace root**: `/Users/sac/praxis` containing `crates/chatman-common/`, `template/`, `my-conforming-project/`, and `tools/`.
* **Monitored/Hygiene files**: In `/Users/sac/praxis/apply.sh` (lines 80-94), the following hygiene files are defined for project-agnostic copying:
  ```bash
  deny.toml
  typos.toml
  rustfmt.toml
  rust-toolchain.toml
  SECURITY.md
  .github/workflows/ci.yml
  .github/workflows/release.yml
  .github/dependabot.yml
  .editorconfig
  ```
* **Structural constraints (hollow-gate)**: In `/Users/sac/praxis/template/tools/hollow-gate/main.rs` (lines 6-12), the following stubs are blocked:
  ```rust
  const BLOCKING: &[(&str, &str)] = &[
      ("unimplemented!", "HOLLOW-001"),
      ("todo!",          "HOLLOW-002"),
      ("// TODO:",         "HOLLOW-004"),
      ("// FIXME:",        "HOLLOW-005"),
      ("// PLACEHOLDER",   "HOLLOW-006"),
  ];
  ```
  And lines 89-116 check for the existence of:
  * `PhantomData`
  * ZST marker `Raw` (`struct Raw;` or `struct Raw`)
  * ZST marker `Validated` (`struct Validated;` or `struct Validated`)
  * ZST marker `Admitted` (`struct Admitted;` or `struct Admitted`)
  * `Evidence` wrapper (`struct Evidence`)
  * `Admit` trait (`trait Admit`)
  * `RulePackServer` implementation (`impl RulePackServer for`)

### B. Shared Common Library
* **Crate `chatman-common`**: Located at `/Users/sac/praxis/crates/chatman-common`.
* **Dependencies**: `crates/chatman-common/Cargo.toml` specifies dependency version boundaries:
  - `blake3 = { version = "1", optional = true }` under `provenance` feature.
  - `tempfile = { version = "3", optional = true }` under `testkit` feature.
* **Rolling Chain Hash Utilities**: In `/Users/sac/praxis/crates/chatman-common/src/provenance.rs`, utility structures like `RollingChain` and `RollingHash` are defined:
  - `pub fn content_address(bytes: &[u8]) -> String`
  - `pub struct RollingChain { running: String, count: usize }`
  - `pub struct RollingHash { hasher: blake3::Hasher }`
  These are re-exported in `crates/chatman-common/src/chain.rs`.

---

## 2. Logic Chain

Based on the observations:
1. **Hygiene Restoration**: `praxis-reconciler` must monitor the files listed in `apply.sh`'s `HYGIENE_FILES` and `OPTIONAL_FILES` (e.g. `deny.toml`, `typos.toml`, `rustfmt.toml`, etc.) and prevent drift. Since some files (like `Cargo.toml` or `src/` modules) contain template variables (e.g. `{{project-name}}`), the reconciler can read the project's metadata from its local `Cargo.toml` on startup, perform dynamic placeholder substitutions, and then verify the file contents via BLAKE3 hashes.
2. **Subprocess Compilation & Structural Validation**: `praxis-guard` can spawn standard `cargo` commands (e.g., `cargo fmt --check`, `cargo clippy`, `cargo test`) to ensure the codebase is compilation-clean and matches standard format constraints. Additionally, by parsing files with regular expressions/AST checkers (mirroring `hollow-gate/main.rs`), it can programmatically assert ZST states and ensure no `todo!` or `unimplemented!` patterns are present.
3. **Receipt Authenticity**: Once compilation and tests pass, `praxis-guard` can generate a cryptographically valid JSON compliance receipt. The receipt will embed:
   - A deterministic BLAKE3 digest of all source files (collected by recursively walking `src/` using alphabetical sorting to guarantee path-content determinism).
   - An audit trace of completed operations.
   - An Ed25519 signature of the receipt JSON using a project private key to prevent tampering.
4. **Workspace Setup**: `/Users/sac/praxis/playground` should be structured as a Cargo workspace containing the tools as workspace members, along with a sample project that uses `chatman-common` as a dependency. This ensures that the self-healing and validation mechanisms can be tested in an integrated sandbox.

---

## 3. Caveats

* **Templated Files vs Static Files**: This design assumes that the files being self-healed by `praxis-reconciler` are either static hygiene files (identical to the template, like `deny.toml`) or are easily substituted template files (using `{{project-name}}`). If templates contain complex conditional sections (e.g., depending on interactive cargo-generate prompts), the reconciler would require a configuration file specifying those parameters, or it must compare against a local cached copy generated at project initialization.
* **Network limitations**: As we operate in CODE_ONLY network mode, all package updates or dependencies must compile using already-cached toolchains/crates in `/Users/sac/praxis` or local cargo cache.
* **Process Watcher Reliability**: The `notify` crate relies on OS-specific file system events which may occasionally be lost under extremely heavy I/O or system hibernation. A slow polling fallback thread (e.g. checking every 30-60 seconds) is proposed to make reconciliation robust.

---

## 4. Conclusion & Proposed Design

We propose the following concrete structure for the `/Users/sac/praxis/playground` workspace and the implementation details of the two tools.

### A. Playground Directory Structure
```
/Users/sac/praxis/playground/
├── Cargo.toml                  # Workspace manifest
├── Cargo.lock
├── README.md                   # Setup and usage instructions
├── tools/
│   ├── praxis-reconciler/      # Active Self-Healing File Daemon
│   │   ├── Cargo.toml
│   │   └── src/
│   │       └── main.rs
│   └── praxis-guard/           # Compilation, Test & Cryptographic Signer
│       ├── Cargo.toml
│       └── src/
│           └── main.rs
└── my-sample-project/          # Conforming project generated from template
    ├── Cargo.toml
    ├── deny.toml               # Monitored by reconciler
    ├── typos.toml              # Monitored by reconciler
    ├── rustfmt.toml            # Monitored by reconciler
    ├── rust-toolchain.toml     # Monitored by reconciler
    ├── SECURITY.md             # Monitored by reconciler
    ├── .editorconfig           # Monitored by reconciler
    ├── .github/
    │   └── workflows/
    │       ├── ci.yml          # Monitored by reconciler
    │       └── release.yml     # Monitored by reconciler
    └── src/
        ├── main.rs
        ├── cli.rs
        ├── lsp.rs
        └── types.rs            # Implements Evidence ZST transitions
```

#### Workspace `Cargo.toml` Definition
```toml
[workspace]
resolver = "2"
members = [
    "tools/praxis-reconciler",
    "tools/praxis-guard",
    "my-sample-project"
]

[workspace.dependencies]
chatman-common = { path = "../crates/chatman-common", features = ["full"] }
tokio = { version = "1", features = ["full"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
blake3 = "1"
walkdir = "2"
notify = "6.1"
ed25519-dalek = { version = "2.1", features = ["rand_core"] }
ignore = "0.4"
toml = "0.8"
```

---

### B. Library and Crate Selection & Tool Details

#### 1. `praxis-reconciler` (Active Self-Healing Watcher)
* **File Watcher**: `notify` crate (v6.1). Spawns an event-driven watcher mapping modifications on target files.
* **Polling Fallback**: `tokio::time::interval` running a low-priority thread walking the project directory every 30 seconds.
* **Dynamic Template Substitution**:
  - Parsed configuration: reads project name and description from the target's `Cargo.toml`.
  - Content check: for each template file, performs search-and-replace for `{{project-name}}` and `{{description}}`, computes the expected BLAKE3 hash, and compares it to the target file.
  - Active Self-healing: if the file is missing or its hash does not match, writes the substituted content. To prevent infinite loops, the reconciler ignores events generated by its own write actions.

#### 2. `praxis-guard` (Compilation & Receipt Verification)
* **Subprocess Execution**: `std::process::Command` to invoke `cargo clippy`, `cargo fmt`, and `cargo test`.
* **Source Digest Computation**:
  - Recursively crawls `src/` and configuration files using the `ignore` crate (respecting `.gitignore`).
  - Sorts file paths alphabetically to ensure stable hashes.
  - Feeds file relative paths and file contents into `chatman_common::chain::RollingChain` to produce a single, deterministic root hash of the source tree.
* **Receipt Generation & Signature**:
  - Generates a JSON document containing metadata, source hash, cargo status flags, and compliance state.
  - Signs the canonical, sorted JSON bytes using `ed25519-dalek` with a key pair stored in `.praxis/keys/`.
  - Embeds the public key and signature in the receipt file.
* **Receipt Verification**:
  - Reads the receipt file, validates the Ed25519 signature against the embedded public key, re-scans the project source directory to verify the current source hash matches `source_files_root_hash` in the receipt.

---

## 5. Verification Method

To verify these tools and workspace design once implemented, the following test cases must be run:

### Test Case 1: Active Self-Healing on Drift (`praxis-reconciler`)
1. Start `praxis-reconciler` targeting `my-sample-project/`.
2. Edit `my-sample-project/deny.toml` (e.g. delete a line or modify lint levels) or delete it entirely.
3. Observe that `praxis-reconciler` immediately detects the drift, logs the correction, and rewrites the file.
4. Verify that `my-sample-project/deny.toml` is restored to its exact template contents.

### Test Case 2: Compilation & Compliance Proof (`praxis-guard`)
1. Run `cargo run -p praxis-guard -- build /Users/sac/praxis/playground/my-sample-project --output /tmp/receipt.json`.
2. Verify that `cargo clippy`, `cargo fmt`, and `cargo test` run successfully and `/tmp/receipt.json` is generated.
3. Inspect `/tmp/receipt.json` to verify the presence of `source_files_root_hash`, `signature`, and `public_key`.
4. Run `cargo run -p praxis-guard -- verify --receipt /tmp/receipt.json --project /Users/sac/praxis/playground/my-sample-project`. Verify it prints `VERIFIED` and exits with status `0`.

### Test Case 3: Anti-Tampering Check (`praxis-guard`)
1. Modify a source file (e.g. `my-sample-project/src/main.rs`) by adding a space or a comment.
2. Run the verify command again: `cargo run -p praxis-guard -- verify --receipt /tmp/receipt.json --project /Users/sac/praxis/playground/my-sample-project`.
3. Verify that the verifier fails with a hash mismatch error and exits with status `1`.
