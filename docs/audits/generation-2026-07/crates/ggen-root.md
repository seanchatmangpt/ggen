# ggen root binary package (`src/`)

**Purpose:** Workspace-root `ggen` binary. `main.rs` is a 12-line delegation to
`ggen_cli_lib::cli_match()`; `lib.rs` is a 2-line re-export of `ggen_core`.

## LOC

Command: `tokei src --output json | jq '.Rust.code'` (tokei 14.0.0, 2026-07-02)

| File | Code LOC |
|---|---|
| `src/main.rs` | 12 |
| `src/lib.rs` | 2 |
| `src/scanner.rs` | 82 |
| `src/rdf.rs` | 119 |
| **Total** | **215** |

## Class breakdown

| Class | LOC | % of 215 | % of live (14) |
|---|---|---|---|
| DEAD-DELETE | 201 | 93.5% | — |
| GENERATABLE-NOW | 12 | 5.6% | 86% |
| GENERATABLE-WITH-SPEC | 2 | 0.9% | 14% |
| GENERATED / IRREDUCIBLY-CUSTOM | 0 | 0% | 0% |

## Per-file rationale

- `src/scanner.rs` (82), `src/rdf.rs` (119) — **DEAD-DELETE**. Orphaned: neither is
  declared as a module anywhere (`grep -rn 'scanner\|rdf' src/main.rs src/lib.rs` and
  `grep -rn 'include!' src/` both return nothing, exit 1), and they reference
  `crate::models`, `crate::db`, `crate::receipt`, `crate::projection`, `crate::symbol`,
  `crate::capability` — modules that do not exist in `src/`, so they could not compile
  if wired in. They are residue of the cpmp scanner (the `cpmp` crate owns
  scanner/capability/projection per CLAUDE.md). Deletion is the cheapest path.
- `src/main.rs` (12) — **GENERATABLE-NOW**. Pure delegation wrapper; template
  `.specify/templates/cli/main.rs.tera` + spec `.specify/cli-commands.ttl` exist; only
  wiring is missing.
- `src/lib.rs` (2) — **GENERATABLE-WITH-SPEC**. Trivial re-export + `VERSION` const;
  registry-shaped, needs a (tiny) spec entry.

**Summary:** 215-LOC thin wrapper package of which 93.5% (scanner.rs + rdf.rs) is dead
cpmp residue to delete; the live 14 LOC are generatable delegation/re-export.
