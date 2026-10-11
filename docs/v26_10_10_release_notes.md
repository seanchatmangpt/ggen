# ggen v26.10.10 Release Notes (2026-10-10)

## Highlights

- **crates.io release chain live**: `ggen-engine` is `publish = true`; `ggen-abb-sbb` publish lifted (dcbf333). CI pins `cargo-cicd` to crates.io 26.10.10 (was git `--rev 852a2d9`, 592fa43e8).
- **ABB/SBB admission gate in the sync pipeline**: `ggen-abb-sbb` is ggen-engine's Stage 2c gate — IO-free `admit()`/`plan()`/`depgraph` (transitive closure, cycle + unbound-port refusals); `canonical_digest` is feature-unification-stable via sorted-key JSON (12b096484).
- **Vendored praxis-core / praxis-graphlaw retired** (SJIRA-15): default graph backend is the sibling `graphlaw` crate, moved to registry pin 26.10.10 (5847a3d08); workspace drops to 14 crates.
- **Workspace clippy `-D warnings` gate green**: TECH-DEBT-002 closed (892d349c5); last 459 errors closed (68cddf958); `just lint` exits 0; rustdoc link warnings 37 → 0 (acaf8714f).
- **Build provenance attestation vector**: BLAKE3(commit+packs+ontology) via fail-soft `build.rs` (eec9fd05d).

## Breaking / Notable

- graphlaw path dep (`../graphlaw`) removed → registry 26.10.10. Downstream consumers pinning the path must switch to the crates.io version.
- ggen-mcp tool-surface parity: court-pinned exact set equality — **12 tools**, not nine (`ggen_pack_capabilities`/`ggen_pack_query`/`ggen_receipt_verify` added to docs).

## Internal

- Pack ecosystem: `ggen-release-pack` v0.3.0 (v0.2.0 release-discipline standards, a10ea7c2a); `cargo-cicd-pack` Round-6 re-audit — 24 nouns/56 verbs (9883a433f, PR #801).
- Non-fatal ggen.toml schema-drift advisory on mixed markers (12933b648); packCount guard 93 → 97 (5ebcbd29f); TPL-001 suppressed under `SELECT *` (ebaebc55f).

## Verification

- `just pre-commit` GREEN (a1c4a26f9 receipt, census adjudicated); workspace clippy `-D warnings` clean (3a731c6fb); final identity: ggen 919315ccb.

---

## Annotated tag message

```text
ggen v26.10.10 — crates.io release chain, ABB/SBB Stage 2c gate, lint closure

Highlights:
- ggen-engine publish = true; ggen-abb-sbb on crates.io (dcbf333); CI pins
  cargo-cicd to registry 26.10.10 (#800)
- ggen-abb-sbb wired as ggen-engine's Stage 2c admission gate
  (admit/plan/depgraph; cycle + unbound-port refusals)
- praxis-core/praxis-graphlaw vendored members retired (SJIRA-15);
  graphlaw -> registry pin 26.10.10; workspace = 14 crates
- Workspace clippy -D warnings gate green (TECH-DEBT-002); rustdoc
  warnings 37 -> 0
- Build provenance attestation: BLAKE3(commit+packs+ontology)
- ggen-mcp surface court-pinned at 12 tools
- cargo-cicd-pack Round-6 re-audit; ggen-release-pack v0.3.0 (#801)

Verification: just pre-commit GREEN; clippy -D warnings clean.
```
