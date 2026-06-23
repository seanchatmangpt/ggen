## 2026-06-22T17:20:26-07:00
Refine `check_path` in `crates/star-toml/src/validation.rs` to fix the backslash traversal security bug: check path traversal on Unix by ensuring that any backslash `\` character is treated as a separator for directory traversal checking (i.e. check if the path contains `..` components split by either `/` or `\`). For example:
```rust
let has_traversal = path.components().any(|c| c == std::path::Component::ParentDir)
    || value.split(|c| c == '/' || c == '\\').any(|s| s == "..");
```
Add a unit/integration test in `star-toml` (e.g. in `crates/star-toml/tests/adversarial.rs` or `crates/star-toml/src/validation.rs`) to verify this backslash traversal fix on Unix.

Next, implement `star_toml::Validate` for all configurations in `crates/ggen-config`:
- `GgenConfig` and all of its sub-configuration structures (`ProjectConfig`, `AiConfig`, `TelemetryConfig`, `TemplatesConfig`, `McpConfig`, `A2AConfig`, etc.) in `crates/ggen-config/src/config_lib/schema.rs` and other files.
- `OntologyConfig` in `crates/ggen-config/src/config/ontology_config.rs`.
Implement all 17 custom validation checks from `ConfigValidator` and the validation checks from `OntologyConfig` using the new `star-toml` validation helpers. Refer to the explorer's design for the exact trait implementation mappings.

Ensure the workspace compiles cleanly (`cargo check --all-targets`). Do NOT refactor the loader or validator of `ggen-config` yet (that is Milestone 4), just add the `Validate` trait implementations and ensure compilation.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.

Please write your changes to `changes.md` and `handoff.md` in your working directory and then send a message back to the parent (conversation ID: 2ad6d043-08f9-4408-b75e-dcdfbdedbc8c).
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_worker_m3_1/
Your identity is worker_m3_1 (archetype: teamwork_preview_worker).
