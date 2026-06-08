## 2026-06-06T20:27:51Z
You are teamwork_preview_worker_setup_1.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_worker_setup_1
Your task is to run `cargo check --all-targets` and `cargo test` in both /Users/sac/ggen and /Users/sac/tower-lsp-max to verify they build and pass all existing tests.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.

Please write your findings and verification results to a handoff report at /Users/sac/ggen/.agents/teamwork_preview_worker_setup_1/handoff.md and notify me when you are done.

## 2026-06-06T20:35:35Z
You are a developer worker. Your working directory is `/Users/sac/ggen/.agents/teamwork_preview_worker_setup_1`.
Your task is to implement Milestone 1: Test infrastructure and scaffolding.

Instructions:
1. Append the schema-complete skeleton models for the core projection engine to `crates/ggen-projection/src/lib.rs` (do not delete or break any existing code, just append). The models should include:
   - `PackDescriptor` with serialization/deserialization helper `from_toml(content: &str) -> Result<Self, anyhow::Error>`.
   - `PackPlan` with basic dependency resolution structure `resolve(descriptors: &[PackDescriptor]) -> Result<Self, anyhow::Error>`.
   - `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` with serialization/deserialization.
   - A skeleton `pub fn sync(output_dir: &std::path::Path) -> Result<(), anyhow::Error>`.
   Ensure this compiles perfectly.
2. Create the E2E test target folder `crates/ggen-projection/tests/` and create the shared test harness in `crates/ggen-projection/tests/common/mod.rs`.
   - The test harness must provide functions for setting up temporary directories, writing template files, starting LSP servers, and communicating with them via JSON-RPC.
   - It should NOT use mocks or stubs. It must interact with actual files and binaries.
3. Run `cargo check --all-targets` to verify that the library and tests compile successfully.
4. Write your handoff report to `/Users/sac/ggen/.agents/teamwork_preview_worker_setup_1/handoff.md` summarizing the changes and verification command results, and send a message back to the caller conversation ID.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
