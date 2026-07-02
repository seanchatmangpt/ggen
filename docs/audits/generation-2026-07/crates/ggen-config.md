# ggen-config — Generation-First Audit

**Purpose:** Configuration parser and validator for `ggen.toml` (serde schema, TOML loader, validator), ontology-pack/lockfile config types, and a cryptographic receipt subsystem (Ed25519 receipts, hash-linked chains, and the `chatmangpt.receipt.envelope.v1` envelope).

## LOC

`tokei crates/ggen-config/src --output json | jq '.Rust.code'` → **6,014** code LOC (tokei 14.0.0, per-file LOC below via `tokei <file> --output json`).

| Module | Code LOC |
|---|---|
| `config/` | 3,053 |
| `config_lib/` | 2,130 |
| `receipt/` | 816 |
| `lib.rs` | 15 |

## Class breakdown

| Class | LOC | % of total | % of (TOTAL − DEAD) |
|---|---|---|---|
| DEAD-DELETE | 2,089 | 34.7% | — |
| GENERATABLE-WITH-SPEC | 1,875 | 31.2% | 47.8% |
| IRREDUCIBLY-CUSTOM | 2,050 | 34.1% | 52.2% |
| GENERATED / GENERATABLE-NOW | 0 | 0% | 0% |

potential_coverage ≈ **48%**; custom_ratio ≈ **52%** (of which 775 LOC — the receipt/envelope crypto — is a proposed **E-ledger** entry; C-ledger custom glue is 1,275 LOC → C/(G+C) view ≈ 40%).

**Verdict on prior estimate (~70% generatable): NOT confirmed — actual ≈48% after DEAD subtraction.** The 70% figure was inflated by the dead QA/hive cluster (2,089 LOC of serde-heavy structs that *look* generatable but have no consumers — DEAD-DELETE is assigned first per METHODOLOGY.md, so it is not laundered as generatable). Sampling basis: full sweep — all 29 .rs files enumerated, 14 read at 20+ lines each.

## Per-module rationale

### DEAD-DELETE — QA/lean-manufacturing cluster (2,089 LOC)
`config/hive_coordinator.rs` (351), `config/andon_gemba.rs` (363), `config/system_health.rs` (415), `config/quality_assurance.rs` (403), `config/qa_cli.rs` (471), `config/qa_integration_test.rs` (86).
Evidence: `grep -rln "hive_coordinator\|HiveQueen\|andon_gemba\|quality_assurance\|system_health\|qa_cli" --include='*.rs' crates src` returns **zero hits outside `crates/ggen-config/src`** — no in-workspace consumers. `hive_coordinator.rs` opens with `#![allow(dead_code, ...)]` (self-declared dead). "Hive Queen Swarm Coordinator" / FMEA / POKA-YOKE frameworks are unrelated to config parsing; deletion is the cheapest path to coverage.

### GENERATABLE-WITH-SPEC (1,875 LOC)
- `config_lib/schema.rs` (1,053): pure serde struct definitions of the ggen.toml schema (`GgenConfig`, `ProjectConfig`, `AiConfig`, ...). Canonical spec-shaped type module; nearest template family: the serde-struct emitters used for `ggen-a2a-mcp/src/a2a_generated/`. Needs a `ggen.toml` schema TTL.
- `config_lib/error.rs` (259): thiserror enum + Display impls — error-enum pattern.
- `config/ontology_config.rs` (309), `config/template_config.rs` (137): serde config-section structs with defaults.
- `config/mod.rs` (37), `config_lib/mod.rs` (24), `receipt/mod.rs` (21), `receipt/error.rs` (20), `lib.rs` (15): re-export/module wiring.

### IRREDUCIBLY-CUSTOM (2,050 LOC)
- `config_lib/parser.rs` (363): TOML load, path validation, IO glue — behavior, not shape.
- `config_lib/validator.rs` (431): cross-field validation logic (`star_toml::Validate`, duplicate/constraint checks).
- `config/lock_manager.rs` (262): ggen.lock read/write + digest verification — authoritative-path code per `.claude/rules/coding-agent-mistakes.md` §4.1.
- `receipt/envelope.rs` (328), `receipt/receipt_impl.rs` (208), `receipt/chain.rs` (239): Ed25519 sign/verify, BLAKE3/SHA-256 hashing, chain-link verification. **Proposed E-ledger (crypto/receipts allowlist).** Note: roughly 40% of these files' lines are serde type/field definitions that a spec could emit, but signing/verification logic dominates and file-level classification follows the dominant content.
- `config/ontology_integration_test.rs` (219): hand-written Chicago test assertions living in `src/` (should move to `tests/`; counted here because tokei scope is src/).
