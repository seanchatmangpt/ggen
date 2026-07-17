# Methodology — Generation-First Audit (2026-07)

Audit question: **what fraction of this repo's code and docs is (or could be) emitted by
ggen's μ-pipeline from TTL specs, and what is the irreducible custom core?**
Owner target: all code starts from ggen; ≤0.1% hand-written custom glue.

## Scope and roster

Workspace roster comes from `cargo metadata --no-deps` (NOT `ls crates/` — the crates/
directory contains dormant non-member dirs: genesis-construct8, genesis-lockchain,
genesis-wasm-shell, ggen-membrane, ggen-projection, plus 2 empty placeholders).
17 packages: 16 `crates/*` members + the root `ggen` binary package (`src/`).

LOC = Rust **code lines** (comments and blanks excluded) via `tokei <dir> --output json`,
scoped to each member's `src/` only (tests/, benches/, examples/ excluded).

Reproduce the baseline table:
```bash
for dir in src crates/*/src; do tokei "$dir" --output json | jq '.Rust.code'; done
```

## Classification taxonomy

Every workspace src file is assigned exactly one class in `classification.tsv`
(`path<TAB>class<TAB>evidence`). Classes, in the order they are assigned:

| Class | Definition | Required evidence |
|---|---|---|
| `DEAD-DELETE` | Deletion is the cheapest path to coverage. Classified FIRST so dead code is never laundered as "generatable". | Consolidation-analysis citation + dependent-grep showing no in-workspace consumers |
| `GENERATED` | Emitted today by a wired pipeline; regenerating from TTL reproduces it. A file with a generated header that a rule does NOT reproduce is not GENERATED (see the hollow `generated_commands.rs`). | Path + the rule/command that produces it |
| `GENERATABLE-NOW` | Template AND spec already exist; only wiring is missing. | Template path + TTL spec path + the missing wiring point |
| `GENERATABLE-WITH-SPEC` | Boilerplate-shaped (type defs, serde structs, error enums, CLI wiring, registries); needs a new TTL spec but no novel algorithm. Percentages are estimates — the sampling basis is stated in each crate file. | Pattern description + representative files + nearest existing template family |
| `IRREDUCIBLY-CUSTOM` | Algorithmic core TTL cannot express. This is the defended 0.1% budget — every entry is a claim. | Per-module justification |

## Metrics

```
generation_coverage = GENERATED_LOC / (TOTAL_LOC − DEAD_LOC)
potential_coverage  = (GENERATED + GENERATABLE_NOW + GENERATABLE_WITH_SPEC) / (TOTAL − DEAD)
custom_ratio        = IRREDUCIBLY_CUSTOM_LOC / (TOTAL − DEAD)     # target ratchet ≤ 0.001
```

Baseline (git 013bee436, tokei 14.0.0): TOTAL = 167,380 code LOC;
GENERATED = 5,184 (`crates/ggen-a2a-mcp/src/a2a_generated/`) → **≈3.1% before dead subtraction**.
(The previously-circulated 8,001 figure was `wc -l` including comments/blanks; this audit
standardizes on tokei code lines.)

## Target definition (three ledgers)

Literal 0.1% over all LOC is not the goal — README doctrine already protects human-owned
cores. The trackable definition:

- **E — Engine core**: reviewed allowlist (graph algorithms, RDF/SPARQL internals,
  crypto/receipts, LSP analyzers, the μ-pipeline itself). Excluded from the ratio.
- **G — Generated**: byte-identity-verified against regeneration (`ggen sync --check`, once built).
- **C — Custom glue**: everything else. **Goal: C/(G+C) ≤ 0.1%**, ratcheted through
  milestone gates 25% → 50% → 80% → 95%.

Anti-gaming rules: G requires reproducibility, not just an `@generated` header;
E-membership is a reviewed allowlist, never self-declared.

## Re-running

`scripts/generation-coverage.sh` joins `classification.tsv` against tokei output,
**fails if any workspace src file is unclassified**, and writes `coverage.json`
(with the git SHA it was computed at). CI ratchet rule: coverage may not decrease.

## Relationship to prior audits

See `supersessions.md`. In short: `AUDIT_DASHBOARD.md` superseded (stale roster, P0s
carried forward); `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` consumed (seeds DEAD-DELETE);
`DOCUMENTATION_AUDIT_REPORT.md` consumed as raw inventory for `docs-audit.md`.
