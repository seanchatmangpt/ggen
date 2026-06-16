<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Validation Gates — Definition of Done](#ggen-validation-gates--definition-of-done)
  - [Quick Reference: The 7 Validation Gates](#quick-reference-the-7-validation-gates)
  - [Gate-by-Gate Specification](#gate-by-gate-specification)
    - [Gate 1: SHACL Validation](#gate-1-shacl-validation)
    - [Gate 2: Profile Enforcement (Strict Mode)](#gate-2-profile-enforcement-strict-mode)
    - [Gate 3: SPARQL Conformance (Law Surfaces)](#gate-3-sparql-conformance-law-surfaces)
    - [Gate 4: Type-Level Validation (Rust Compiler)](#gate-4-type-level-validation-rust-compiler)
    - [Gate 5: Diagnostic Codes Resolution (LSP Diagnostics)](#gate-5-diagnostic-codes-resolution-lsp-diagnostics)
    - [Gate 6: Output Layer Validation (Yield Gate)](#gate-6-output-layer-validation-yield-gate)
    - [Gate 7: Determinism Validation (Hash Verification)](#gate-7-determinism-validation-hash-verification)
  - [Validation Command Sequences](#validation-command-sequences)
    - [Development Loop (Fast Feedback)](#development-loop-fast-feedback)
    - [Pre-Commit Hook](#pre-commit-hook)
    - [Full Validation (Before Sync)](#full-validation-before-sync)
    - [Sync and Emit (Production)](#sync-and-emit-production)
  - [Receipt Proof Requirements](#receipt-proof-requirements)
  - [Strictness Matrix](#strictness-matrix)
  - [Definition of Done Checklist](#definition-of-done-checklist)
  - [OTEL Validation Checklist](#otel-validation-checklist)
  - [Automation Triggers](#automation-triggers)
  - [File References](#file-references)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Validation Gates — Definition of Done

**Version**: 26.5.28 | **Last Updated**: 2026-06-14

Complete specification of the **7 validation gates** that must pass before artifacts are emitted. All gates are enforced across the μ₁–μ₅ pipeline.

---

## Quick Reference: The 7 Validation Gates

| # | Gate | Activation | Severity | Command | Evidence |
|---|------|-----------|----------|---------|----------|
| **1** | [SHACL Validation](#gate-1-shacl-validation) | μ₁ load | ERROR | `ggen validate spec.ttl` | SHACL report, OTEL span |
| **2** | [Profile Enforcement](#gate-2-profile-enforcement) | Post-render | ERROR (strict) | `ggen sync` | Profile report, GGEN-PROFILE-* codes |
| **3** | [SPARQL Conformance](#gate-3-sparql-conformance) | μ₂–μ₃ | ERROR (provision) | `ggen lsp check --all` | LSP diagnostics, GGEN-*/E-codes |
| **4** | [Type-Level Validation](#gate-4-type-level-validation) | Compile | ERROR | `just check` | Compiler output (zero errors) |
| **5** | [Diagnostic Resolution](#gate-5-diagnostic-codes-resolution) | Pre-emit | ERROR (strict) | `ggen lsp check --all` | LSP diagnostic report |
| **6** | [Output Layer Validation](#gate-6-output-layer-validation) | μ₅ emit | ERROR | `ggen sync` | Validated path list |
| **7** | [Determinism Validation](#gate-7-determinism-validation) | Post-emit | ERROR (strict) | `ggen sync --audit true` | Receipt with signature |

---

## Gate-by-Gate Specification

### Gate 1: SHACL Validation

**Activation**: μ₁ (load stage)

**Description**: RDF ontology shapes validation. Validates that the loaded RDF graph conforms to SHACL shape constraints defined in `.specify/shapes/`.

**Severity**: ERROR (halt on violation)

**Command**: 
```bash
ggen validate .specify/specs/feature.ttl
```

**Diagnostic Codes**: None (SHACL reports shape violations directly)

**Validation Rules**:
- All RDF triples must conform to declared SHACL shapes
- Required properties must be present on all instances
- Property value types must match SHACL datatype constraints

**Recovery**:
- Inspect SHACL error output and fix RDF triple to match shape constraint
- Add missing property with valid value
- Convert property value to correct datatype

**Evidence**:
- SHACL validation report with per-shape conformance results
- OTEL span `pipeline.validate_shacl` with violations count

---

### Gate 2: Profile Enforcement (Strict Mode)

**Activation**: Post-render (before emission)

**Description**: Validates output artifacts against marketplace profile constraints (Draft/Published state, immutability, trust requirements, receipt specifications). Prevents artifact emission if profile constraints are violated.

**Severity**: 
- Basic: WARNING (continue)
- Strict: ERROR (halt)

**Command**: 
```bash
ggen sync --profile-check  # or implicit in ggen sync
```

**Diagnostic Codes**:
| Code | Meaning | Recovery |
|------|---------|----------|
| GGEN-PROFILE-001 | Output violates profile constraints | Change profile to Draft or remove constraint |
| GGEN-PROFILE-002 | Pack immutability violated (published modified) | Bump semantic version or revert |
| GGEN-PROFILE-003 | Manifest schema version mismatch | Regenerate manifest or update version |

**Validation Rules**:
- Published packs cannot be modified without version bump
- Strict mode enforces all profile constraints
- Trust requirements must be satisfied
- Receipt specification must match emitted receipt structure

**Recovery**:
- Bump pack version in `ggen.toml` or `.ggen/packs.lock`
- Enable permissive profile or remove constraint
- Add signature/receipt or increase pack trust level
- Ensure receipt contains all required fields

**Evidence**:
- Profile enforcement report listing all constraints and pass/fail per constraint
- OTEL span `pipeline.profile_check` with violation count

---

### Gate 3: SPARQL Conformance (Law Surfaces)

**Activation**: μ₂–μ₃ (extract/generate stages)

**Description**: Validates SPARQL queries against law surfaces: (1) provision checks (GGEN-OUT-001), (2) SPARQL laws (E0010–E0015), (3) Tera template integration (GGEN-TPL-001, E0024).

**Severity**: 
- Basic: ERROR (provision checks), WARNING (SPARQL laws)
- Strict: ERROR (all codes)

**Command**: 
```bash
ggen lsp check --all
```

**Diagnostic Codes**:

| Code | Failure Class | Severity | Meaning | Recovery |
|------|---------------|----------|---------|----------|
| **GGEN-TPL-001** | unbound_projection | ERROR | Tera template consumes {{var}} not in SELECT | Add var to SELECT or remove from template |
| **GGEN-OUT-001** | unbound_output_path | ERROR | output_file pattern contains unbound variable | Bind all vars in output_file to SELECT |
| **GGEN-QUERY-002** | blindspot | WARNING | SELECT * used (disables provision checks) | Replace with explicit variable list |
| **E0010** | values_forbidden | ERROR | VALUES clause in external .rq file | Move to ggen.toml [[rule.data]] or inline |
| **E0011** | ordering_missing | WARNING (ERROR strict) | CONSTRUCT lacks ORDER BY | Add ORDER BY clause |
| **E0013** | ordering_missing | WARNING (ERROR strict) | SELECT lacks ORDER BY | Add ORDER BY clause |
| **E0015** | identity_construct | WARNING | Identity CONSTRUCT (no-op transformation) | Modify template or remove rule |
| **E0024** | template_syntax_error | ERROR | Tera template syntax error | Fix Tera syntax |

**Validation Rules**:
- All Tera template variables must be in SPARQL SELECT projection
- All output_file pattern variables must be in SPARQL SELECT projection
- SPARQL queries must use explicit SELECT when ORDER BY required for determinism
- VALUES clause only allowed inline in ggen.toml, not in external .rq files
- Tera template syntax must be valid

**Recovery**:
- Provision checks: Add missing variables to SELECT or template
- SPARQL laws: Add ORDER BY for determinism
- Syntax errors: Fix Tera template or SPARQL query

**Evidence**:
- LSP diagnostic report with all GGEN-*/E-codes
- OTEL span `pipeline.sparql_conformance_check` with violation counts
- Ability to link violation to line/char in ggen.toml or .rq file

---

### Gate 4: Type-Level Validation (Rust Compiler)

**Activation**: Compile time

**Description**: Rust compiler checks + clippy lints. Validates: (1) Zero compiler errors, (2) Zero unwrap/expect in production, (3) Zero unsafe blocks outside audit boundaries, (4) All fallible operations return Result<T, CrateError>, (5) No todo!() in committed code.

**Severity**: ERROR (halt)

**Command**: 
```bash
just check
```

**Diagnostic Codes**: None (Rust compiler E-codes)

**Validation Rules**:
- Zero compiler errors (E0xxx)
- No unwrap() or expect() in production code (crates/*/src/)
- No unsafe blocks outside audit boundaries
- All I/O, parsing, and FFI operations must return Result<T, CrateError>
- No todo!(), unimplemented!(), or panic!() in committed code
- No clippy warnings in production code

**Recovery**:
- Fix compiler error by addressing type mismatch, missing implementation, or borrow checker violation
- Replace unwrap with Result handling or match expression (unwrap only in tests)
- Remove unsafe or move to explicitly audited module with SAFETY comment
- Propagate error with ? operator or handle explicitly
- Implement function or mark with #[cfg(test)] or #[allow(dead_code)]
- Fix underlying issue or add justified #[allow(...)] with SAFETY comment

**Evidence**:
- Compiler output showing zero errors
- `cargo build --workspace 2>&1 | tail -5` showing 'Finished release'

---

### Gate 5: Diagnostic Codes Resolution (LSP Diagnostics)

**Activation**: Pre-emission (μ₄ validation stage)

**Description**: All active GGEN-* diagnostic codes must be resolved (no errors) before artifacts are emitted. 11 diagnostic species are registered; all active detectors must report zero violations.

**Severity**: 
- Basic: WARNING (all codes)
- Strict: ERROR (all ERROR-severity codes)

**Command**: 
```bash
ggen lsp check --all
```

**Active Diagnostic Species** (11 total):

| Code | Failure Class | Route | Surfaces |
|------|---------------|-------|----------|
| GGEN-TPL-001 | unbound_projection | source_law_repair | ggen.toml, SPARQL, Tera |
| GGEN-OUT-001 | unbound_output_path | source_law_repair | ggen.toml, SPARQL |
| GGEN-RULE-001 | unbound_rule_file | source_law_repair | ggen.toml |
| GGEN-YIELD-001 | layer_violation | security_repair | ggen.toml, OS |
| GGEN-YIELD-003 | orphaned_output | source_law_repair | ggen.toml |
| GGEN-YIELD-004 | competing_authority | source_law_repair | ggen.toml |
| GGEN-YIELD-005 | remote_fetch | source_law_repair | ggen.toml |
| GGEN-QUERY-002 | blindspot | advisory | SPARQL |
| GGEN-PACK-001 | pack_indirection | advisory | ggen.toml |
| GGEN-SRC-001 | source_caste | source_law_repair | ggen.toml |
| GGEN-HARNESS-001 | harness_mismatch | proof_topology_repair | Cargo.toml, tests/proof |

**Validation Rules**:
- All ERROR-severity codes must have zero violations before emission
- All WARNING-severity codes should be resolved (advisory)
- Diagnostic species table is authoritative (11 active species)
- Each diagnostic species has a repair route

**Recovery**:
- Run suggested repair routes or manually fix root cause
- Follow route guidance (source_law_repair, security_repair, proof_topology_repair, advisory)

**Evidence**:
- LSP diagnostic output showing zero active ERROR codes
- OTEL span `pipeline.diagnostic_resolution_check` with diagnostic counts

---

### Gate 6: Output Layer Validation (Yield Gate)

**Activation**: μ₅ (emit stage)

**Description**: Validates output file paths do not escape the project root. Prevents directory traversal attacks (../ escaping) and ensures all artifacts stay within boundaries.

**Severity**: ERROR (halt)

**Command**: 
```bash
ggen sync --validate-only
ggen sync  # implicit validation
```

**Diagnostic Codes**:

| Code | Failure Class | Meaning | Recovery |
|------|---------------|---------|----------|
| GGEN-YIELD-001 | layer_violation | output_file escapes root (../ or absolute path) | Remove ../ and use relative paths |
| GGEN-YIELD-003 | orphaned_output | output_file has no static filename base | Add static prefix: e.g., 'generated/{{name}}.rs' |
| GGEN-YIELD-004 | competing_authority | Multiple rules target same output_file | Consolidate rules or adjust paths |
| GGEN-YIELD-005 | remote_fetch | output_file is a URL | Use relative filesystem path |

**Validation Rules**:
- No ../ in output_file paths
- No absolute paths in output_file
- output_file must normalize to within project root
- No orphaned outputs (fully dynamic path with no static base)
- No multiple rules writing to same output_file
- No URLs in output_file

**Recovery**:
- Remove ../ and use relative paths within project
- Ensure path canonicalization shows output within project directory
- Add static prefix to dynamic paths
- Consolidate overlapping rules or adjust output file paths
- Use relative filesystem paths

**Evidence**:
- Validated path list showing all output paths and their canonical resolved form (all within project root)
- OTEL span `pipeline.yield_gate_check` confirming zero escapes

---

### Gate 7: Determinism Validation (Hash Verification)

**Activation**: μ₅ (post-emit) and receipt generation

**Description**: Validates reproducible builds via cryptographic hash verification. Ensures that identical inputs produce identical outputs. Validates input hashes, output hashes, receipt signatures, and detects non-deterministic sources (UUIDs, timestamps, random).

**Severity**: 
- Basic: WARNING (GGEN-DETERM-001/004)
- Strict: ERROR (all GGEN-DETERM-* codes)

**Command**: 
```bash
ggen sync --audit true
ggen receipt verify .ggen/receipts/latest.json
```

**Diagnostic Codes**:

| Code | Failure Class | Meaning | Recovery |
|------|---------------|---------|----------|
| GGEN-DETERM-001 | non_deterministic_output | Output hash changed with identical inputs | Audit for non-deterministic sources (UUID, timestamp, random) |
| GGEN-DETERM-002 | missing_input_hash | Input hash missing/empty in lockfile | Re-run pack install with digest computation |
| GGEN-DETERM-003 | unsigned_receipt | Receipt signature empty (not signed) | Verify Ed25519 key; re-run ggen sync |
| GGEN-DETERM-004 | non_deterministic_ordering | Output ordering non-deterministic | Add ORDER BY to SPARQL |

**Validation Rules**:
- Identical inputs must produce identical outputs (hash equality)
- All input files must have non-empty SHA-256 hashes in lockfile
- Receipt must be signed with Ed25519 private key
- Receipt signature must verify against Ed25519 public key
- SPARQL queries must use ORDER BY for deterministic result sets
- No non-deterministic values (UUIDs, timestamps, random) in templates

**Recovery**:
- Audit for non-deterministic sources: Uuid::new_v4(), SystemTime::now(), random(), thread-local state
- Re-compute hashes; verify pack digest computed at install time
- Ensure .ggen/keys/signing.key exists; run ggen sync to generate signed receipt
- Ensure receipt and key match; regenerate receipt if key was changed
- Add ORDER BY (?s, ?p, ?o) or other stable sort key
- Replace Uuid::new_v4() with fixed UUID or hash-based deterministic ID

**Evidence**:
- Receipt with non-empty input_hashes, output_hashes, and valid Ed25519 signature
- `ggen receipt verify` returning is_valid: true
- OTEL span `pipeline.determinism_audit` showing zero determinism violations

---

## Validation Command Sequences

### Development Loop (Fast Feedback)

```bash
# Fast iteration during feature development
just check                    # Gate 4: Type-level validation
ggen lsp check --all          # Gate 3, 5: SPARQL conformance + diagnostics
just test-unit                # Unit tests (Chicago TDD)
```

### Pre-Commit Hook

```bash
# Run before committing
just pre-commit               # check + lint + test-unit
```

### Full Validation (Before Sync)

```bash
# Full validation before artifact emission
ggen validate .specify/specs/*.ttl        # Gate 1: SHACL validation
ggen sync --validate-only                 # Gates 2, 6, 7: Profile, yield, determinism pre-check
just test                                 # All tests (unit + integration)
```

### Sync and Emit (Production)

```bash
# Complete validation with receipt generation
ggen sync --audit true                                        # Gates 1-7: Full validation + receipt
ggen receipt verify .ggen/receipts/latest.json              # Verify receipt signature
```

---

## Receipt Proof Requirements

**File**: `.ggen/receipts/latest.json`

**Required Fields**:

```json
{
  "operation_id": "550e8400-e29b-41d4-a716-446655440000",      // UUID v4, non-zero
  "timestamp": "2026-06-14T12:34:56.789Z",                     // RFC-3339 format
  "input_hashes": {                                             // SHA-256 hex (64 chars)
    "spec.ttl": "a1b2c3d4e5f6...",
    "ggen.toml": "f6e5d4c3b2a1..."
  },
  "output_hashes": {                                            // SHA-256 hex (64 chars)
    "src/generated.rs": "9z8y7x6w5v4u...",
    "docs/api.md": "u4v5w6x7y8z9..."
  },
  "signature": "2CPbF4NvWM3jQu8H9z7xK...",                       // Base64 Ed25519 signature (non-empty)
  "diagnostics_snapshot": [                                     // All active GGEN-* codes
    {"code": "GGEN-TPL-001", "severity": "error", "count": 0},
    {"code": "GGEN-OUT-001", "severity": "error", "count": 0}
  ]
}
```

**Verification**:
```bash
ggen receipt verify .ggen/receipts/latest.json
# Output: is_valid: true, signature_verified: true, hash_chain_intact: true
```

---

## Strictness Matrix

**Basic Mode** (default):
```
Gate 1: SHACL                    → ERROR
Gate 2: Profile Enforcement      → WARN
Gate 3: SPARQL Conformance       → ERROR (provision), WARN (E0011/E0013)
Gate 4: Type-Level              → ERROR
Gate 5: Diagnostic Resolution   → WARN
Gate 6: Output Layer            → ERROR
Gate 7: Determinism             → WARN
```

**Strict Mode** (`[validation] strict_mode = true`):
```
Gate 1: SHACL                    → ERROR
Gate 2: Profile Enforcement      → ERROR
Gate 3: SPARQL Conformance       → ERROR (all codes)
Gate 4: Type-Level              → ERROR
Gate 5: Diagnostic Resolution   → ERROR
Gate 6: Output Layer            → ERROR
Gate 7: Determinism             → ERROR
```

---

## Definition of Done Checklist

All gates must pass before artifact emission:

```bash
# 1. Type-level validation (Gate 4)
just check

# 2. RDF validation (Gate 1)
ggen validate .specify/specs/*.ttl

# 3. SPARQL & Diagnostic Resolution (Gates 3, 5)
ggen lsp check --all

# 4. Profile, Yield, Determinism pre-check (Gates 2, 6, 7)
ggen sync --validate-only

# 5. Full test suite (Chicago TDD)
just test

# 6. No clippy warnings
just lint

# 7. Full validation with receipt (Gates 1-7)
ggen sync --audit true

# 8. Receipt verification (Gate 7)
ggen receipt verify .ggen/receipts/latest.json
```

**Success Criteria**:
- ✅ `just check` returns 0
- ✅ `ggen validate` returns 0 (no SHACL violations)
- ✅ `ggen lsp check --all` shows zero ERROR codes
- ✅ `just test` passes all tests
- ✅ `just lint` shows zero warnings
- ✅ `.ggen/receipts/latest.json` exists with valid signature
- ✅ `ggen receipt verify` returns is_valid: true

---

## OTEL Validation Checklist

For features involving external services or LLM calls:

- [ ] All tests pass (`just test`)
- [ ] OTEL spans exist for the operation (RUST_LOG=trace)
- [ ] All required attributes are populated (non-zero tokens, real latency)
- [ ] Token counts and timing consistent with real calls (not synthetic)
- [ ] Error spans appear if operation failed (with error=true)
- [ ] Spans show network latency (2-3s for LLM, not instant)

**Verification**:
```bash
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel.txt
grep -E "pipeline\.|llm\." otel.txt
```

---

## Automation Triggers

| Trigger | Gates | Commands |
|---------|-------|----------|
| Pre-commit hook | 3, 4, 5 | `just pre-commit` |
| Pre-push hook | 3, 4, 5 | `just test`, `ggen lsp check --all` |
| CI: Build | 4 | `cargo build --workspace` |
| CI: Validation | 1, 3, 5 | `ggen validate`, `ggen lsp check --all` |
| CI: Tests | All | `cargo test --workspace` |
| Manual sync | 1-7 | `ggen sync --audit true` |

---

## File References

| File | Purpose | Lines |
|------|---------|-------|
| `docs/validation/VALIDATION_GATES_DEFINITION_OF_DONE.json` | Master validation spec (JSON) | 819 |
| `crates/ggen-lsp/src/route/diagnostic_species.rs` | Diagnostic species registry | 300 |
| `crates/ggen-lsp/src/analyzers/sparql_analyzer.rs` | SPARQL law checks (E0010–E0015) | 200+ |
| `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` | Template validation | 150+ |
| `crates/ggen-core/src/poka_yoke/quality_gates.rs` | Quality gate orchestration | 24KB |
| `crates/ggen-core/src/poka_yoke/validated_path.rs` | Path escaping checks | 5KB |
| `crates/ggen-marketplace/src/marketplace/profile.rs` | Profile enforcement | 150+ |
| `crates/ggen-core/src/domain/ontology/validate.rs` | SHACL validation | 100+ |

---

## Summary

ggen's validation system is **multi-layer and deterministic**:

1. **Compile-time** (Gate 4): Rust type system + clippy
2. **Author-time** (Gates 3, 5): LSP diagnostics with 11 active species
3. **Sync-time** (Gates 1, 2, 6, 7): Pipeline enforcement at load/render/emit stages
4. **Post-emit** (Gate 7): Cryptographic receipt + determinism audit

**All validation is evidence-based**: diagnostics map to law surfaces, receipts bind input→output hashes, and OTEL spans prove real execution. No fabrication is possible — the system enforces observability at every gate.
