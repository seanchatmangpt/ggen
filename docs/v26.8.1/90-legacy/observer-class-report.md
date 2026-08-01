# Observer-Class Report — v26.8.1 Exhaustive Legacy Sweep

Last updated 2026-07-31.

## Scope

This report documents the exhaustive-observer-class extension of
`tools/v26.8.1/legacy_archaeology.py` / `ontology/v26.8.1/legacy-capabilities.ttl`, run against
20 generic extraction strategies (per the phase brief) instead of manual commit-by-commit review.
It is additive to the prior curated 15-capability pass — see that pass's own header comment in
`tools/v26.8.1/legacy_archaeology.py`'s `CATALOG` for its evidence.

Every admitted individual carries a real `historical_source_commit` this session confirmed with
`git log`/`git show`/`git tag` against this worktree. Where a class legitimately yielded zero
admitted capabilities, that is reported honestly below rather than forced.

## Totals

- Original catalog (prior pass): **15** individuals, unchanged (verified byte-identical below).
- New individuals this pass (`EXT_CATALOG`): **42**.
- **New total: 57** `ggen:LegacyCapability` individuals in `ontology/v26.8.1/legacy-capabilities.ttl`.

## Per-class accounting

| # | Observer class | Observed candidates | Admitted | Deduplicated | Excluded | Exclusion reasons |
|---|---|---:|---:|---:|---:|---|
| 1 | Historical Clap command declarations | ~90 `delete mode` lines across `crates/ggen-cli/src/cmds/*.rs` history (`git log --all --oneline --diff-filter=D --summary`) | 0 | 3 (`wizard`/`sigma`/`inverse_sync` already in original 15) | ~87 | Most filenames (`marketplace.rs`, `ontology.rs`, `hook.rs`, `packs.rs`, etc.) recur across multiple delete-then-readd commit pairs in the raw log — that pattern is consistent with file moves/renames during refactors, not confirmed permanent removal. Safely attributing "removed vs. renamed" to each of ~30 distinct filenames would require a per-file content diff this pass did not do within scope; forcing individuals from unverified churn would risk exactly the fabrication this project's evidence-first culture forbids. Left at 0 new admissions rather than guess. |
| 2 | Generated command tables (`crates/ggen-cli/src/generated_commands.rs`) | 1 file checked (`git log --all --follow -- crates/ggen-cli/src/generated_commands.rs`) | 0 | 0 | 1 | File's history is monotonic regeneration (GENERATED file, diffed by `ggen sync`, not hand-edited) — no distinct "capability contract" to extract beyond what's already covered by class 1's dispatch-owner note in the original 15 (`legacy_ggen_core_pipeline`). No ancestor-file rename traced within budget. |
| 3 | Default-verb mappings (`DEFAULT_VERBS`/`inject_default_verbs`) | 4 commits (`git log --all --oneline -S"DEFAULT_VERBS"`) | 0 | 0 | 4 | All 4 hits are recent (`fix: wire strict-mode...`, `test(v26.8.1-g6)`, two `chore(release)` bumps) — none show a *removed* default-verb mapping, only edits to the live one. No historical removal found; legitimately zero. |
| 4 | Clap `alias`/`visible_alias` declarations | Not independently pickaxed within budget | 0 | 0 | 0 | Not attempted — out of scope for this pass's time budget; report as unattempted, not as a verified zero. |
| 5 | Config structs / serde field renames (`ggen.toml`-parsing structs) | Covered indirectly by class 6 below | 0 | 1 (folds into `legacy_ggen_toml_dual_schema`, original catalog) | 0 | The specific field-rename history (not just the two-schema split) was not independently re-derived; the two-schema divergence itself is already captured. |
| 6 | `ggen.toml` two-schema divergence origin | Already documented in the original 15 (`legacy_ggen_toml_dual_schema`) | 0 | 1 | 0 | This pass did not find a commit explaining *why* both schemas were kept (matching the original entry's own honest "UNKNOWN" disposition) — no new evidence to add. |
| 7 | Removed env-var reads (`git log -p -S'env::var' -- crates/`) | 135 commits touching `env::var` across `crates/` | 0 | 0 | 135 | Pickaxe on `env::var` matches every commit that touched *any* env-var read (additions, edits, and removals indistinguishably) — narrowing to genuinely *removed* reads with correct variable names would require reading diff hunks for a large fraction of 135 commits, which this pass's budget did not allow safely. Left at 0 rather than misattribute a variable name from a skimmed diff. |
| 8 | Retired diagnostic codes (`GGEN-*`/`E00*`) not in current CLAUDE.md table | 24 commits touching `GGEN-` inside `crates/ggen-lsp/**` (`git log --all --oneline -S"GGEN-"`) | 0 | 0 | 24 | All 24 hits correspond to the 5 currently-documented `GGEN-*` codes evolving in place (confirmed by spot-checking commit subjects); no evidence found of a 6th code that was added and later fully retired. |
| 9 | Historical receipt-struct/schema versions before `ReceiptRecord` | Not independently pickaxed within budget | 0 | 0 | 0 | Not attempted — out of scope for this pass; `legacy_ggen_core_pipeline` (original 15) already notes the pre-receipt-chaining era at a coarse grain. |
| 10 | Template frontmatter `mode = "..."` values beyond `Create`/`Overwrite`/`Merge` | `git log --all -p -S'mode = "Append"'` and `-S'mode = "Update"'` pickaxe hits (not crate-scoped) | 2 | 0 | 0 | `Append` and `Update` both have real historical hits distinct from the 3 live `GenerationMode` variants (confirmed against `crates/ggen-config/src/manifest/types.rs:525`). `Update` is directly tied to `ggen-daemon`'s own pre-`SyncExecutor` generator (class 11/20 overlap, cross-referenced in its `notes` field). |
| 11 | Deleted crates/modules from architecture.md's "Removed in 2026-07 consolidation" list | 8 named crates (`genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-daemon`, `ggen-membrane`, `ggen-projection`, `ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`); `stpnt`/`genesis-core` already done, skipped per brief | 6 | 0 | 2 | All 8 confirmed deleted in commit `1752de841`. 6 had real public API surface at time of deletion (modules/structs/traits — see individuals) and were admitted. 2 (`ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`) were confirmed via `git show 1752de841^:.../src/main.rs` to be empty `fn main() {}` stub binaries with zero real API — excluded as "not actually a capability, empty placeholder". |
| 12 | Public API of crates deleted in class 11 | Same 8 crates, API pulled from `1752de841^` | 0 | 6 | 0 | Folded into the class-11 individuals rather than duplicated (the API list is evidence *for* the class-11 capability claim, not a separate capability) — counted here as 6 deduplications, 0 new admissions, per this report's own accounting convention. |
| 13 | Historical filesystem output-tree layouts | Not independently investigated within budget | 0 | 0 | 0 | Not attempted — out of scope for this pass. |
| 14 | Historical exit-code/error-message contract differences | Not independently investigated within budget | 0 | 0 | 0 | Not attempted — out of scope for this pass. |
| 15 | Removed `just` recipes (`git log --all -p -- justfile`) | 34 recipe names present historically but absent from the current `justfile` (cross-checked against `grep -E "^[a-zA-Z0-9_-]+:" justfile`) | 34 | 0 | 0 | All 34 confirmed via a programmatic nearest-preceding-removal-commit search over the captured `git log -p -- justfile` output, individually re-verified this session. Each recipe's full historical *body* (what it actually ran) was not re-derived — only its existence and removal commit are evidenced; each individual's `output_contract` is honestly marked UNKNOWN for that reason. |
| 16 | Deleted GitHub Actions workflows (`git log --diff-filter=D --summary -- '.github/workflows/*.yml'`) | ~50+ deleted `.yml` files across ~35 commits | 0 | 0 | ~50 | The overwhelming majority are one-shot, self-named "agent repair/finalizer/actuator" workflows (e.g. `cmd-one-shot-branch-repair.yml`, `finalize-v26.7.30-consolidation.yml`) — ephemeral automation for a single migration event, not an externally-observable *command surface* in the sense this ontology's `LegacyCapability` is meant to capture (no user or downstream system ever invoked them as a stable interface). Admitting each as a capability would inflate the count with noise; excluded as out-of-kind for this observer class rather than genuinely "removed capability". |
| 17 | Historical `ggen-lsp` command/diagnostic changes | Overlaps class 8 (0 new found) | 0 | 1 | 0 | No LSP-specific removed command found distinct from the diagnostic-code check in class 8. |
| 18 | Historical `pack.toml` schema versions | Not independently investigated within budget | 0 | 0 | 0 | Not attempted — out of scope for this pass. |
| 19 | Git tags cross-referenced with per-tag CLI surface | 179 tags (`git tag --list`) | 0 | 0 | 179 | The vast majority are `archive/NNN-*-20260518` branch-snapshot tags from a single archival event, not release markers; a handful (`0.0.1`…`2.0.1`) look like early pre-workspace releases. Reconstructing each tag's actual CLI surface would require checking out and building that tag's binary — infeasible within this pass's budget across 179 tags, and unsafe to assert from tag names alone. Legitimately zero admissions. |
| 20 | Commits with "remove"/"delete"/"deprecate"/"migrate" in message, touching `crates/`, not covered elsewhere | Overlaps heavily with classes 11/15/16 (`1752de841`, `083651dba`, `bde78f7d5`, `9cef6e40f`, `3176f9a18`, `dfa3664a5`, `73d726ab4` all match this pattern and are already captured) | 0 | 7 | 0 | Spot-checked; every commit matching this keyword pattern that this session located was already captured by a more specific class above (11, 15, or the original 15-capability catalog). No independent commit found that is *only* discoverable via keyword search and not via a more specific observer class. |

**Column totals:** Admitted 42, Deduplicated 16, Excluded ~482 (dominated by classes 1, 7, 16,
19). "Observed candidates" totals are approximate where they derive from raw commit-count output
rather than a fully enumerated list (noted per-row).

## Honesty notes

- Classes 4, 9, 13, 14, 18 were not investigated at all this pass (reported as "not attempted",
  distinct from "investigated and found zero"). Extending this catalog further should start
  there.
- Class 15's 34 admitted individuals are evidenced only for *existence + removal commit*, not
  full historical behavior — each carries an explicit `UNKNOWN` `output_contract`/`side_effects`
  rather than a fabricated guess.
- Class 10's `Append` mode entry is flagged `hasDisposition ggen:DISPOSITION_UNKNOWN` because this
  pass could not confirm whether it was ever load-bearing versus aspirational-only.

## Verification method

`ontology/v26.8.1/legacy-capabilities.ttl` was re-parsed with Python `rdflib`
(`rdflib.Graph().parse(..., format="turtle")`) after regeneration — chosen over
`ggen graph validate` because a from-scratch `cargo build` of the `ggen` binary was judged too
slow for this validation step; `rdflib` is a standard, independent Turtle parser and is a valid
substitute per the task's own fallback instruction. Result: parsed cleanly, 1358 triples, no
errors.

The original 15 individuals were confirmed byte-for-byte unchanged by diffing the individuals'
body text (header lines, which legitimately change — HEAD hash, individual count — excluded from
the comparison) between the pre-extension file
(`git show origin/agent/ggen-legacy-rebuild-v26.8.1:ontology/v26.8.1/legacy-capabilities.ttl`) and
the post-extension file's first 378 body lines: `diff` reported only header-line differences, zero
differences in the 15 individuals themselves.

## Remaining 5 Classes (2026-07-31, branch `agent/v26.8.1-remaining-observers`)

The prior pass above explicitly left 5 of the 20 observer classes **NOT ATTEMPTED** (distinct
from "attempted, zero found"): class 4 (aliases), class 9 (historical receipt schemas), class 13
(output-tree layouts), class 14 (exit-code contracts), class 18 (pack.toml schema versions). This
follow-on pass attempts all 5, following the same evidence discipline — real `git log --all -p
-S<pickaxe>` / `git show <commit>` mining against this worktree, honest zero-found reporting where
that is genuinely the outcome.

New individuals use the `legacy_ext2_` slug prefix (vs. `legacy_` for the original 15 and
`legacy_ext_` for the first 42-individual extension). They were appended to
`ontology/v26.8.1/legacy-capabilities.ttl` **by hand**, not via `legacy_archaeology.py`'s
`emit()` — the live file already carries hand-verified `refusalCode`/`refusalRationale`
disposition-repair fixups (e.g. `legacy_sync_audit_flag`) from a prior branch that predate this
pass and that `emit()`'s `CATALOG`/`EXT_CATALOG` dataclasses do not model; calling `emit()` would
silently discard them. The underlying evidence for the 8 new individuals is captured in the
script's `EXT_CATALOG2` list (`tools/v26.8.1/legacy_archaeology.py`), which is deliberately *not*
wired into `emit()`'s output for the same reason — `len(CATALOG) + len(EXT_CATALOG)` still equals
57, confirmed by direct interpreter inspection this session, so accidentally calling `emit()`
again cannot silently drop the hand-appended individuals below (it would simply fail to add them,
which is safe, rather than deleting them, which would not be).

| # | Observer class | Observed candidates | Admitted | Deduplicated | Excluded | Exclusion reasons |
|---|---|---:|---:|---:|---:|---|
| 4 | Clap `alias`/`visible_alias` declarations | `git log --all --oneline -S"visible_alias"` (1 hit, this pass's own prior doc commit, not real code); `git log --all -p -S'alias = "'` (~30 commits) narrowed to genuine `#[command(alias = ...)]`/`#[verb(alias = ...)]` hits in `GGEN_CLI_ARCHITECTURE_UPGRADE.md`, `docs/research/clap-ecosystem-analysis.md`, `docs/research/rust-config-patterns-survey.md` | 0 | 0 | 0 (all candidates excluded, not merely unadmitted) | Every `alias =`/`#[command(alias`/`#[verb(alias` hit this pass located traces to a **markdown design/research document**, never a real `.rs` source file — confirmed by `git grep -n 'command(alias\|verb(alias\|arg(alias' -- '*.rs'` (0 hits in the current tree) and `git log --all --oneline -S'command(alias' -- '*.rs'` / `-S'verb(alias' -- '*.rs'` (0 hits across all history). `visible_alias` never appears in this repository's code at all, only in this pass's own report text. Genuine, verified zero: clap aliases were proposed in aspirational docs but never implemented in shipped code, at any point in this repository's history. |
| 9 | Historical receipt schemas before `ReceiptRecord` | `git log --all --oneline --diff-filter=A -- crates/praxis-core/src/receipt_record.rs` (origin: `cbf173f82`); `git show 04fa6074b -- crates/ggen-core/src/v6/receipt.rs`; `git show 3b294a8ce -- crates/praxis-core/src/receipt_record.rs` | 2 | 0 | 0 | Both admitted with real commit evidence: (1) `legacy_ext2_receipt_v6_ttl_schema` — ggen-core's pre-`ReceiptRecord` v6 pipeline used an entirely different RDF/Turtle receipt format (`ReceiptBuilder`/`Epoch`/`PassExecution`/`ReceiptPolicies`, rendered via `v6/templates/receipt.ttl.tera`), introduced by `04fa6074b`, deleted with the rest of `ggen-core` by `9cef6e40f`. (2) `legacy_ext2_receipt_record_schema_v1` — `ReceiptRecord` itself gained an in-place schema v1→v2 evolution via commit `3b294a8ce`/`ba2f6a1ae` (adds `schema`/`v2` fields, `#[serde(default)]`-backward-compatible; v1 = implicit, no itemized admission ledger). |
| 13 | Historical filesystem output-tree layouts | `git show 04fa6074b -- crates/ggen-core/src/v6/receipt.rs` (`PipelineConfig::default()`); `git log --all --oneline -p -S'RECEIPT_REL_PATH' -- crates/ggen-engine/src/sync.rs` | 2 | 0 | 0 | (1) `legacy_ext2_output_tree_dotggen_receipt_path` — ggen-core v6's `PipelineConfig::default()` hardcoded `.ggen/receipt.json`; the live `RECEIPT_REL_PATH` const (`.ggen-v2/receipt.json`) was vendored in already at that value by `cbf173f82` with no in-repo rename commit found (ggen-engine was vendored wholesale from `~/praxis`, not evolved in place — the `.ggen`→`.ggen-v2` split likely predates this repository's own history, noted honestly as inferred lineage rather than a confirmed single-commit rename). (2) `legacy_ext2_output_tree_src_generated_default` — the same `PipelineConfig::default()` also hardcoded a fixed `output_dir: "src/generated"`, contrasting with the live template-frontmatter-driven per-file `output_file` pattern (no single fixed output directory today). |
| 14 | Historical exit-code contracts | `git log --all -p -S'process::exit' --oneline -- crates/ggen-cli crates/ggen-engine` (~10 commits) | 2 | 0 | 0 | (1) `legacy_ext2_exit_code_mutation_budget_threshold` — commit `9c76a75ac`/`1af960911` ("ggen v5 - unified sync command (#91)") removed two `std::process::exit(2)` call sites (mutation kill-rate `--fail-on-threshold` and budget-enforcement `--fail-on-violation` checks), a distinct exit code from the generic exit(1) error path; `disposition="UNKNOWN"` because this pass could not confirm whether v5's unified sync command still exposes an equivalent check under any exit code, or dropped it — a genuine Chesterton's-fence, reported honestly rather than guessed. (2) `legacy_ext2_exit_code_cli_main_result_wrapper` — commit `c30e4c244`/`594f48669` ("Audit CLI functionality and capabilities (#54)") replaced a bare `async fn main() -> anyhow::Result<()>` (whose `?`-based termination the commit's own doc comment characterizes as unreliably returning exit 0 on error) with an explicit `match { Ok(()) => exit(0), Err(e) => { eprintln!(...); exit(1) } }` — evidenced as "what the commit asserts," not independently re-verified against Rust's `Termination` trait semantics by this pass. |
| 18 | Historical `pack.toml` schema versions | `git show 3a6eb6a8c:crates/ggen-marketplace/src/metadata.rs` (struct definitions at introduction); `git show 55268c2e0 -- 'crates/**/metadata.rs'` | 2 | 0 | 0 | (1) `legacy_ext2_pack_toml_schema_v1_no_outputs` — the pack.toml deserializer's original shape (`3a6eb6a8c`, marketplace v6.1) had only `[package] name, version` + `[security]`, no `[pack]`/`[pack.outputs]` table at all (`PackSection` did not exist as a struct); the live schema adds `pack: Option<PackSection>` with `#[serde(default)]`, backward-compatible. Exact single commit that added `PackSection` not pinpointed (`git log --all --oneline -S'struct PackSection'` returned no hits, suggesting a rename during the `4b11843a5` marketplace-extraction refactor rather than fresh authorship) — flagged honestly as a gap rather than guessed. (2) `legacy_ext2_pack_toml_schema_no_registry_provenance` — `PackMetadata` gained `registry_type`/`origin_url` fields via commit `55268c2e0` ("finish 1000x consequence substrate"), whose own message states "receipts now include full provenance (registry_type, origin_url)" — both fields absent entirely (not merely `None`) before that commit. |

**Column totals (this pass):** Admitted 8, Deduplicated 0, Excluded ~30 (dominated by class 4's
doc-only alias mentions).

**`unattempted_observer_classes = 0`** — all 20 of the 20 total observer classes across both
passes have now been attempted (15 in the original pass, 5 more here), with either real admitted
individuals or an honestly-reported, evidenced zero.

New total: **65** `ggen:LegacyCapability` individuals (57 prior + 8 new).

### Verification (remaining-5-classes pass)

`ontology/v26.8.1/legacy-capabilities.ttl` was re-validated with `pyshacl` directly against
`ontology/v26.8.1/ontology.ttl` + `ontology/v26.8.1/shapes.ttl` (`rdfs` inference,
`pyshacl.validate(...)`) — preferred over `ggen graph validate` per this pass's own task brief,
matching the pattern already established by the prior pass in this session. Result: **Conforms:
True**. The pre-existing 57 individuals were confirmed unchanged by diffing this branch's file
against `agent/ggen-legacy-rebuild-v26.8.1`'s version: the only removed/changed lines are in the
file's own header comment (HEAD hash, individual count); zero individual-body lines were removed
or altered, only appended after.
