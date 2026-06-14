# Execution Plan - Marketplace Refactoring and Correctness

## Overview
We will implement the refactoring, safety, and correctness roadmap defined in `MARKETPLACE_AUDIT_REPORT.md` across four main milestones.

## Milestones

### Milestone 1: Resolve Critical Bugs & Vulnerabilities
- Cache verification: Match installer checksum generation (verify compressed digest).
- Path traversal Zip Slip: Add validation in extraction and use atomic temp extraction/rename.
- Nested dotted key TOML lookup: Fix python generator registry script nested package.metadata lookup.

### Milestone 2: Refactor Code Quality & Typestates
- Receipt determinism: Replace `HashMap` with `BTreeMap` in `composition_receipt.rs`.
- Trust tier ordering: Re-order priority/satisfies logic to block/quarantine correctly.
- RDF mapper registry class: Dynamically map `registry_class` instead of hardcoding.
- Case-insensitive SPARQL: Check queries case-insensitively.
- README file check: Validate existence of README.md file in the package directory.

### Milestone 3: Metadata & Build Configuration
- Repair OWL violations in `marketplace/ontology.ttl`.
- Remove duplicate entries in `marketplace/index.json`.
- Update `marketplace/validate-docs.sh` to check only for existing files.
- Add missing tasks to `Makefile.toml`.
- Enable ignored unit and integration tests and correct dummy assertions.

### Milestone 4: Verification and Compiling
- Compile all targets successfully (`cargo build --all-targets`).
- Pass all tests successfully (`cargo test --all-targets`).
