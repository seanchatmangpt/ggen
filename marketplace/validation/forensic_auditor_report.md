# Forensic Audit Report: Codebase Compliance with AGENTS.md & GEMINI.md

## Verdict
**INTEGRITY VIOLATION**

---

## Executive Summary
An exhaustive forensic audit was conducted on the codebase (specifically under the `/Users/sac/ggen` workspace) for compliance with the verification principles set forth in `AGENTS.md` and `GEMINI.md`.

While the codebase is clean of active `TODO` and `FIXME` comments representing deferred implementation tasks in the production and test source code, multiple violations were discovered in the codebase's architecture. These include:
1. **Mock Registries in Production Paths**: A fake registry database adapter ([TestRepository](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs#L524)) is utilized by default in production logic in [pack_install.rs](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs).
2. **Stub/Placeholder Cryptography in Production Paths**: Production code in [part_signer.rs](file:///Users/sac/ggen/crates/ggen-core/src/parts_foundry/part_signer.rs) bypasses cryptographic boundaries using placeholder keys (`"pk_stub_verifying_key"`) and signatures (`"sig_..."`).
3. **Legacy Mock Validators**: Production modules in [validators.rs](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs) expose mock validators that simulate test execution using hardcoded passes and simulated delays.
4. **Mock LLM Providers & Proposers**: An active [MockLLMProposer](file:///Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs#L155) generates fake proposals instead of crossing real LLM service boundaries.
5. **Fake Receipts & Telemetry in Tests**: Unit tests in [proof_gate.rs](file:///Users/sac/ggen/crates/ggen-core/src/pipeline_engine/proof_gate.rs) fabricate mock build receipts and pass executions via test fixtures.
6. **Mock Testing Clients**: Integration tests in [complete_marketplace_test.rs](file:///Users/sac/ggen/crates/ggen-cli/tests/integration/complete_marketplace_test.rs) and [marketplace_test.rs](file:///Users/sac/ggen/crates/ggen-cli/tests/integration/marketplace_test.rs) utilize manual mock client classes instead of crossing execution/filesystem boundaries.

---

## Detailed Audit Results

### 1. Scan for `TODO` and `FIXME` Comments
* **Status**: **CLEAN**
* **Findings**:
  All production and test Rust source code files (`*.rs`) were scanned case-insensitively for `todo` and `fixme`. No active comments indicating deferred implementation or unfinished tasks exist in the source tree.
  * **False Positives Inspected**:
    * String literals in Jinja/Tera templates in [llm_e2e_test.rs](file:///Users/sac/ggen/crates/ggen-core/tests/llm_e2e_test.rs#L393) and [v2_performance.rs](file:///Users/sac/ggen/benches/v2_performance.rs#L159) verify fallback behaviors.
    * Comments inside test checkers, e.g., in [llm_generation_test.rs](file:///Users/sac/ggen/crates/ggen-core/tests/llm_generation_test.rs#L88) or [anti_fake_implementation.rs](file:///Users/sac/ggen/crates/ggen-graph/tests/anti_fake_implementation.rs#L38) verify that code generators do not output TODOs or stubs.
    * Policy code and hooks (such as [git_hook_pre_commit.rs](file:///Users/sac/ggen/crates/ggen-core/src/utils/bin/git_hook_pre_commit.rs) and [git_hook_pre_push.rs](file:///Users/sac/ggen/crates/ggen-core/src/utils/bin/git_hook_pre_push.rs)) mention TODO/FIXME as forbidden patterns to block commits.

### 2. Scan for Mock Testing Helpers, Stub Implementations, Mockall Macros, Fake Receipt Generators, or Dummy Assertions
* **Status**: **INTEGRITY VIOLATION**
* **Findings**:
  Although `mockall` macros and `automock` attributes are absent from active Rust code (only referenced as forbidden list patterns in compliance checks), several custom/manual mock classes, fake data builders, and stubs exist in the codebase:
  * **Legacy Mock Validators** ([validators.rs](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs)):
    * [MockStaticValidator](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs#L335) yields a hardcoded all-pass `ValidationEvidence`.
    * [MockDynamicValidator](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs#L358) simulates test runs using `sleep()`.
    * [MockPerformanceValidator](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs#L380) returns hardcoded latencies and memory allocations.
  * **Mock Proposer** ([delta_proposer.rs](file:///Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs)):
    * [MockLLMProposer](file:///Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs#L155) yields mock proposals instead of calling real LLM boundaries.
  * **Fake Receipt Generators** ([proof_gate.rs](file:///Users/sac/ggen/crates/ggen-core/src/pipeline_engine/proof_gate.rs)):
    * [create_mock_receipt](file:///Users/sac/ggen/crates/ggen-core/src/pipeline_engine/proof_gate.rs#L311) constructs fake build receipt structures containing hardcoded dummy fields (`"test-receipt-id"`, `"test-epoch-id"`, `"test-ontology-hash"`, `"test-outputs-hash"`).
    * [create_mock_pass](file:///Users/sac/ggen/crates/ggen-core/src/pipeline_engine/proof_gate.rs#L331) fabricates simulated pass results.
  * **Placeholder Cryptographic Signatures** ([part_signer.rs](file:///Users/sac/ggen/crates/ggen-core/src/parts_foundry/part_signer.rs)):
    * [PartSigner::sign_part](file:///Users/sac/ggen/crates/ggen-core/src/parts_foundry/part_signer.rs#L27) uses the BLAKE3 hash string itself as a placeholder signature prefixed by `"sig_"` and returns a dummy verifying key (`"pk_stub_verifying_key"`).
  * **Mock Clients in CLI Integration Tests**:
    * [MockMarketplaceClient](file:///Users/sac/ggen/crates/ggen-cli/tests/integration/complete_marketplace_test.rs#L187) and [MockGpackInstaller](file:///Users/sac/ggen/crates/ggen-cli/tests/integration/complete_marketplace_test.rs#L220) bypass real registry endpoints in `complete_marketplace_test.rs` and `marketplace_test.rs`.

### 3. Verify `crates/ggen-cli/src/pack_install.rs` Contains No Stubs or Mocks
* **Status**: **INTEGRITY VIOLATION**
* **Findings**:
  [pack_install.rs](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs) violates `AGENTS.md` and `GEMINI.md` rules by embedding and using a mock registry adapter directly inside production workflows:
  * **Violation 1**: [TestRepository](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs#L524) implements the `AsyncRepository` trait, returning fabricated package models (`"Pack test-pack"`, `"Mock description"`, `"MIT"`) and versions (`"1.0.0"`) instead of performing real HTTP/registry requests.
  * **Violation 2**: The production installer constructor ([create_default_repository](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs#L52)) instantiates and returns [TestRepository](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs#L524) under normal production executions:
    ```rust
    async fn create_default_repository(
    ) -> Result<Box<dyn AsyncRepository<PackageIterator = std::vec::IntoIter<Package>>>, GgenError> {
        // For now, use a mock repository
        // In production, this would connect to the actual marketplace
        Ok(Box::new(TestRepository {}))
    }
    ```
    This redirects all production CLI pack installation calls to the fake registry backend.

---

## Remediation Requirements
To bring the codebase into alignment with the compliance rules, the following actions must be taken:
1. **Remove TestRepository**: Replace the mock implementation in [pack_install.rs](file:///Users/sac/ggen/crates/ggen-cli/src/pack_install.rs) with a real registry implementation (e.g., pulling from `ggen-marketplace`'s actual registry engines).
2. **Remove Cryptographic Stubs**: Replace the signature placeholder logic in [part_signer.rs](file:///Users/sac/ggen/crates/ggen-core/src/parts_foundry/part_signer.rs) with authentic Ed25519 signing and verification.
3. **Clean Legacy Mocks**: Remove mock validators from [validators.rs](file:///Users/sac/ggen/crates/ggen-core/src/ontology/validators.rs) and refactor dependent tests to cross real validation boundaries.
4. **Enforce Real Boundary Testing**: Rewrite proof gate, delta proposer, and CLI integration tests to execute across actual file, network, and subprocess boundaries rather than using fake structures, mock clients, or mock LLM providers.
