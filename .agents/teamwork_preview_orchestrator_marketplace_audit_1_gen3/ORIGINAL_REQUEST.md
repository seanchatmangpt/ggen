# Original User Request

## 2026-06-12T02:36:16Z

Implement the entire ggen pack and marketplace refactoring, safety, and correctness roadmap as documented in MARKETPLACE_AUDIT_REPORT.md.

### R1. Resolve Critical Bugs & Vulnerabilities
- Fix cache verification logic in `crates/ggen-marketplace/src/marketplace/cache.rs` to match the installer checksum generation.
- Implement path-traversal validation (Zip Slip prevention) and atomic temp extraction/rename in `crates/ggen-marketplace/src/marketplace/install.rs`.
- Fix the nested dotted key lookup (`package.metadata`) in `marketplace/scripts/generate_registry_index.py`.

### R2. Refactor Code Quality & Typestates
- Replace `HashMap` with `BTreeMap` in `crates/ggen-marketplace/src/marketplace/composition_receipt.rs` (and other serializable objects) to ensure deterministic JSON hashing.
- Fix the comparison ordering logic in `crates/ggen-marketplace/src/marketplace/trust.rs` so that `Blocked` packages never satisfy any requirements and `Quarantined` packages do not bypass `Experimental`.
- Map the registry classification property dynamically in `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` instead of hardcoding to public.
- Make the SPARQL query injection check case-insensitive in `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`.
- Fix `ReadmeValidator` in `crates/ggen-marketplace/src/marketplace/validation.rs` to check for actual file existence in the package directory rather than a metadata check.

### R3. Metadata & Build Configuration
- Repair the OWL violations (object vs datatype property, `owl:sameAs` usage) in `marketplace/ontology.ttl`.
- Remove duplicate entries and references to non-existent packages in the catalog/index (`marketplace/index.json`).
- Update `marketplace/validate-docs.sh` to only check for existing files.
- Add the missing tasks (`marketplace-validate`, `marketplace-report`, `marketplace-validate-update`) to `Makefile.toml`.
- Remove `#[ignore]` and correct mock/dummy assertions (`assert!(true)`) in the unit and integration tests.

### R4. Verification and Compiling
- The cargo project must compile cleanly under `--all-targets`.
- All tests (both existing and newly enabled/created ones) must pass successfully.

### Acceptance Criteria

#### Compilation & Build
- [ ] Codebase compiles successfully with `cargo build --all-targets`.
- [ ] Running `cargo test --all-targets` passes with no failures.
- [ ] Cargo make checks and registry generation tasks execute without errors.

#### Correctness Checks
- [ ] Cache verification successfully matches the downloaded compressed digest without purging on every request.
- [ ] Extracted archive filenames containing `..` or path traversal characters are blocked from extraction.
- [ ] Receipt IDs are deterministic and identical across repeated serializations.
- [ ] Dotted-key package metadata (like `production_ready`) is correctly indexed in `index.json`.

## Follow-up — 2026-06-12T03:36:42Z

Resume work at /Users/sac/ggen/.agents/teamwork_preview_orchestrator_marketplace_audit_1_gen3. Read handoff.md, BRIEFING.md, ORIGINAL_REQUEST.md, and progress.md for current state.
Your parent is 5f68347e-fea0-44d7-80f3-9d68ac835244 — use this ID for all escalation and status reporting (send_message).
