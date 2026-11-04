# ZD-46 Receipt — Zero-verb directory-module noun audit (CARRY-07, ds-8 FINDING-1)

Date: 2026-11-04 · Lane: ZD-46 · Repo: /Users/sac/ggen · Branch: v26-11-3-wave ·
Receipt: docs/jira/v26.11.3/receipts/zd-46-zero-verb-audit.md

## Charter

Enumerate every `crates/ggen-cli/src/cmds/*/` directory module, classify each
live-verbs vs zero-verb-dead, then fix or typed-refuse per dir. Bound: ≤12
files. Finding source: ds-8.md FINDING-1 — clap-noun-verb derives the noun
from `file!()`'s `file_stem` at runtime; a `mod.rs` file yields stem `"mod"`.

## Mechanism (re-derived, not cited)

`clap-noun-verb` 26.9.1 `#[verb]` with no positional args: verb name from fn
name, noun inferred at runtime from `Path::new(file!()).file_stem()` — for
`cmds/<noun>/mod.rs` that stem is `"mod"`. All bare-`#[verb]` fns across all
directory modules therefore bind to one shared phantom noun `"mod"`, while the
real nouns registered via `register_noun` in `cmds/mod.rs` are verbless
shells. Binary witness (pre-fix): `ggen mod --help` listed 14 verbs (the
union of sbb+maximalism+vision2030, `Commands:` section headed by
vision2030's module docs); `ggen sbb/vision2030/maximalism --help` showed a
bare usage line, no `Commands:`.

## Per-dir table (complete census: 4 dirs)

| dir | verbs | classification | disposition |
|---|---|---|---|
| `ea/` | plan, admit, manufacture, replay, verify-receipt (5) | live-verbs | no change — all 5 use explicit `#[verb("verb", "ea")]`; `ggen ea --help` lists all 5 pre- and post-fix. |
| `sbb/` | schema, inspect, validate, distribution, receipt, replay (6) | zero-verb-dead | fixed in place: 6 × `#[verb("<verb>", "sbb")]`. |
| `maximalism/` | schema, inspect, validate, combinations, outcomes, receipt, replay (7) | zero-verb-dead | fixed in place: 7 × `#[verb("<verb>", "maximalism")]`. |
| `vision2030/` | schema, inspect, validate, roadmap, blue_ocean, dx, qol, doctor, receipt, replay (10) | zero-verb-dead | fixed in place: 10 × `#[verb("<verb>", "vision2030")]`. |

Total: 23 dead verbs rewired, 5 live verbs untouched, 1 phantom noun (`mod`)
removed from the CLI surface.

## Disposition rationale (why wire-in-place, not move/delete)

- **Move verbs up a level** (ds-8's fix for sjira) worked there because
  `cmds/sjira/` contained ONLY `mod.rs`; moving to `cmds/sjira.rs` preserves
  the module tree. `sbb/`, `maximalism/`, `vision2030/` each carry sibling
  files (`evaluation.rs`, `receipts.rs`, `tests.rs`), so the same move is a
  bigger diff (relocate `//!` module docs to a new `cmds/<noun>.rs`). Adding
  the explicit noun argument is the minimal fix for the exact defect — noun
  inference — and its form is proven live by the `ea` noun (same explicit
  `#[verb("verb", "noun")]` form, lists all verbs at the binary).
- **Delete the noun** rejected: the verbs are real, tested surfaces.
- **Typed-refusal with receipt** unnecessary: a one-attribute-per-verb fix
  exists and is already proven in-tree.

## Diff

3 files, 23 attribute edits, zero behavioral-code changes:
- `crates/ggen-cli/src/cmds/sbb/mod.rs` — 6 bare `#[verb]` → explicit noun.
- `crates/ggen-cli/src/cmds/maximalism/mod.rs` — 7 bare `#[verb]` → explicit.
- `crates/ggen-cli/src/cmds/vision2030/mod.rs` — 10 bare `#[verb]` → explicit.

## Falsifier run and killed

After fix, `ggen <noun> --help` must list all verbs and `ggen mod --help` must
refuse. Witnessed at the binary (`./target/debug/ggen`, post-fix build):
- `ggen sbb --help` → Commands: schema, distribution, replay, receipt,
  validate, inspect (6/6).
- `ggen maximalism --help` → replay, inspect, receipt, validate, outcomes,
  combinations, schema (7/7).
- `ggen vision2030 --help` → replay, schema, inspect, receipt, roadmap,
  blue_ocean, doctor, qol, dx, validate (10/10).
- `ggen mod --help` → `error: unrecognized subcommand 'mod'` — the phantom
  noun is gone.
- Regression probes: `ggen ea --help` still 5 verbs; `ggen sjira --help` still
  lists the 5 ds-8 verbs (admit, epoch_check, frontier, execute, lease_admit);
  `ggen --help` noun census unchanged except `mod` gone.

No test or doc references the phantom noun (grep over `crates/*/tests/`,
`docs/jira/v26.11.3/`: zero hits for `"mod"` noun usage).

## Verification ladder

1. `cargo build -p ggen-cli-lib` — exit 0, 1m27s.
2. `cargo test -p ggen-cli-lib` — exit 0, **604 passed / 0 failed** across 62
   `test result: ok` lines, single serial run. (Transport note: an earlier
   lane-local concurrent-cargo collision on the shared `target/` — two
   `cargo test` invocations racing — corrupted dep artifacts; repaired by a
   targeted `cargo clean -p <crate>` loop, then one serial run. A
   pre-corruption run's sole failure,
   `unit::installation::extraction_test::test_large_archive_extraction`,
   passed on immediate re-run — pre-existing flake, not session-introduced.)
3. Binary witnesses listed under Falsifier above (all exits 0; `ggen mod`
   refuses with clap's unrecognized-subcommand error).
4. Runtime smoke, one real verb per fixed noun at the binary: `ggen sbb
   schema`, `ggen vision2030 schema`, `ggen maximalism schema` — all exit 0.
   `ggen --help` noun census post-fix: init init-self agent pack utils sbb ea
   doctor court sjira policy wizard sync bblock packs ontology vision2030
   maximalism telco law capability graph receipt help — `mod` absent.

## Standing

ALIVE for the noun-surface claim (binary witnesses this session, commands +
exits cited). The 23 verbs' runtime behavior is unchanged by construction
(attribute-only diff); no per-verb runtime execution was re-run — the verb
bodies are byte-identical, and the pre-existing tests over them are the
behavior gate (see Verification ladder).
