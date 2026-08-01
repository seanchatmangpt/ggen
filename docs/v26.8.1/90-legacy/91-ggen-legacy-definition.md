# ggen-legacy: definition and criteria

**Status:** Phase G2 working definition, grounded in real repository history (verified 2026-07-31 against `git log --all` in this worktree). Not a promotion of any capability's standing — standing stays `UNKNOWN` per capability until an equivalence verifier and negative falsifier are wired in a later phase.

## Definition

A **ggen-legacy capability** is any externally or operationally observable contract that existed in this repository's history before the current architecture superseded, absorbed, or deleted it, and that a caller (human or program) outside the crate could depend on. Concretely, an observable contract is one or more of:

- a CLI command, noun/verb, argument, flag, alias, or default;
- an environment variable or config field (including which of `ggen.toml`'s two schemas is selected — see `legacy_ggen_toml_dual_schema` below);
- a generated file, directory layout, or file format;
- an exit code, stdout/stderr shape, or error type;
- a diagnostic code (`GGEN-*`, `E00NN`);
- an ordering guarantee (pipeline stage order, SPARQL `ORDER BY` requirements);
- template (Tera) rendering behavior;
- graph/SPARQL/SHACL/ShEx semantics;
- a receipt, hash, or signature (BLAKE3 chain, `.ggen-v2/receipt.json`);
- cache or pack-resolution behavior;
- marketplace or LSP protocol behavior;
- telemetry or OCEL event emission;
- recovery, failure, or migration semantics;
- a documented performance assumption (SLO).

This definition deliberately excludes internal refactors that never crossed an observable boundary (e.g. a private function rename with no caller-visible effect) — those are not legacy capabilities, they are implementation detail. It also excludes anything found only by filename pattern-matching (a file merely named `*legacy*` or `*ggen_core*` is not itself evidence; the commit that removed, replaced, or absorbed it is the evidence).

## Criteria for inclusion in the inventory

A candidate is admitted to `ontology/v26.8.1/legacy-capabilities.ttl` only if this session (or a documented prior phase) found a **real commit** — via `git log --all`, `git log --diff-filter=D --summary`, or a currently-reproducible command failure — that either:

1. deleted the capability's implementation (e.g. `9cef6e40f` deleting `crates/ggen-core/`), or
2. absorbed/renamed it into a current crate (e.g. `bde78f7d5` folding `ggen-a2a-mcp`/`ggen-lsp-mcp`/`ggen-lsp-a2a` into `ggen-lsp`'s `mcp`/`a2a` features), or
3. left it in a currently-broken, currently-reproducible state that a caller could observe today (e.g. `just sync` failing with `error: unexpected argument '--audit' found`, confirmed by running the recipe).

Every catalog individual carries its `ggen:historicalSourceCommit` as a string; where no single commit could be found (three of the fifteen individuals mined this session), the field says `UNKNOWN` explicitly rather than guessing, and the individual's `ggen:hasDisposition` is `ggen:DISPOSITION_UNKNOWN`.

## How the inventory was built

`tools/v26.8.1/legacy_archaeology.py` runs the real `git log`/`git tag` commands specified for phase G2 against this worktree (see its `MINE_COMMANDS` list and `mine()` function) and prints raw evidence — commit counts, tag lists, deletion summaries. A hand-curated, evidence-checked `CATALOG` (15 individuals as of this run) turns a subset of that raw evidence into `ggen:LegacyCapability` Turtle individuals via `emit()`. The catalog is not an exhaustive automated NLP sweep of all ~6,090 commits in this repository's `--all` history; blind regex extraction over that volume risks fabricating contracts nobody actually observed, which the repository's Evidence-First Principle (`CLAUDE.md`) forbids. Extending the catalog is expected and is the intended next step for later phases: run `mine()`, inspect a candidate's real commit, verify it, then add a `CATALOG` entry.

## What this phase does not do

- It does not assign a non-`UNKNOWN` `ggen:hasStanding` to any capability — standing requires an external verifier per the Zero-information-loss mapping rules below, which is out of scope for G2.
- It does not fill `ggen:equivalenceVerifier` or `ggen:negativeFalsifier` — both are left `"UNASSIGNED"` per the phase brief, for a later phase to wire.
- It does not compute an `ggen:exactHeadReceipt` — no BLAKE3 binding exists yet for this disposition set.

## Zero-information-loss mapping (inherited from the ontology's `ggen:LegacyDisposition` enumeration)

Every discovered legacy capability receives one of six dispositions, matching `ontology/v26.8.1/ontology.ttl`'s existing `ggen:PRESERVED` / `ggen:SUBSUMED` / `ggen:REPLACED` / `ggen:ARCHIVED` / `ggen:REFUSED` / `ggen:DISPOSITION_UNKNOWN` individuals:

- `PRESERVED` — same observable behavior and recovery contract today as historically (no individual in this catalog currently qualifies — the phase found nothing simply carried over unchanged).
- `SUBSUMED` — behavior is now provided by a new shared owner (5 of 15 individuals: the three MCP/A2A crates folded into `ggen-lsp`, `genesis-schema-v2` absorbed into `genesis-types-v2`, and local process-intelligence analysis moved to `wasm4pm-compat`).
- `REPLACED` — behavior changed via an explicit migration (2 of 15: the `ggen-core` → `ggen-engine` pipeline swap, and `star-toml`'s move from workspace member to external published dependency).
- `ARCHIVED` — no longer active, restoration path exists but unconfirmed lineage (1 of 15: the original `genesis-core` crate, whose link to `genesis-core-v2` is a naming inference this session did not confirm with a commit).
- `REFUSED` — intentionally unsupported (4 of 15: `wizard`/`sigma`/`inverse_sync` CLI commands, and the dead `stpnt` crate).
- `UNKNOWN` (`ggen:DISPOSITION_UNKNOWN`) — incomplete mapping, blocks sunset (3 of 15: the broken `--audit`/`--dry_run true` justfile flags, and the `ggen.toml` dual-schema divergence — see `92-chestertons-fence-inventory.md` for why each remains open).

See `93-capability-equivalence-matrix.md` for the real per-subsystem counts projected from `ontology/v26.8.1/legacy-capabilities.ttl`.
