# ZD-27 — repo-facts crate parity: ggen-scip + ggen-sjira

## Subject

Repo `/Users/sac/ggen`, branch `v26-11-3-wave`. ZD-24 flagged
`system_crate_map_parity_test` red: `ggen-scip` and `ggen-sjira` were in root
`Cargo.toml`'s `[workspace] members` (lines 94–95) but missing as `rf:Crate`
individuals in `.specify/repo-facts.ttl` (gap introduced at HEAD 301849a9c).

## Diff

1. `.specify/repo-facts.ttl` — added exactly two `rf:Crate` individuals:
   - `rf:crate_ggen_scip` (order 022, dir `ggen-scip`, group `agent-lsp`,
     version 26.11.3) — purpose strings transcribed from
     `crates/ggen-scip/Cargo.toml` description + `src/lib.rs` docs
     (scip.json emitter, tree-sitter-rust extraction, byte-column ranges,
     generated/placeholder detection excluding `ggen-sjira/src/generated.rs`).
   - `rf:crate_ggen_sjira` (order 023, dir `ggen-sjira`, group
     `sovereign-admission`, version 26.11.3) — purpose strings transcribed
     from `crates/ggen-sjira/Cargo.toml` description + comments + `src/lib.rs`
     docs (WorkOrder/admission/digest core slice, receipts/authority/frontier/
     lease+epoch/execute/ledger/controller/edits_bridge/promote/descriptor,
     Slice 5 shacl/target_pack/pack_health, deferred slices listed).
   No other ttl content touched (existing `rf:crateMapIntro` prose untouched —
   see Drift note).
2. `CLAUDE.md` — parity law required it: `governance_precommit_gate_count_test`'s
   `real_workspace_crate_count()` = members(15) + root(1) = 16, and its
   `no_governance_doc_disagrees_with_real_workspace_crate_count` test scans
   `CLAUDE.md` for `<digits> workspace crates` claims. CLAUDE.md claimed
   "14 workspace crates" / "(13 entries) / 14 total" — stale. Updated:
   heading 14→16, intro entry counts 13→15 / 14→16 with a 2026-11-04
   correction note, and appended `ggen-scip` + `ggen-sjira` rows to the
   generated Crate Map table (content = the new ttl individuals'
   `summaryPurpose`, matching the table's source claim). `CLAUDE.md` was
   already ` M` (dirty from another lane's earlier work) before this session;
   only the count prose + two table rows were added.
   `.claude/rules/architecture.md` was checked for
   `-crate workspace`/`workspace crates`/`workspace members` claims — it makes
   none, so no edit needed (grep receipt below).

## Commands / exits

- `cargo test -p ggen-config --test system_crate_map_parity_test` (run 1)
  → exit 0, `2 passed; 0 failed`
- `cargo test -p ggen-config --test system_crate_map_parity_test` (run 2)
  → exit 0, `2 passed; 0 failed`
- `cargo test -p ggen-config --test governance_precommit_gate_count_test`
  → exit 0, `4 passed; 0 failed` (crate-count law green post-CLAUDE.md fix)
- `grep -c '^  "crates/' Cargo.toml` → `15`
- `grep -n 'crate workspace|workspace crates|workspace members'
  CLAUDE.md .claude/rules/architecture.md` → only CLAUDE.md:1 (now 16),
  architecture.md no matches

## Verification ladder

Parity test (real files off disk, no mocks) ×2 + governance crate-count court
+ grep receipts on both guarded docs. No git commands run (per lane law).

## Standing / residual drift (disclosed, not fixed — outside owned scope)

- `.specify/repo-facts.ttl`'s `rf:crateMapIntro` fact still says
  "(13 entries) ... = 14 total ... → 13" — now stale relative to the 15-member
  array. No test pins ttl prose (CRATE_COUNT_DOCS = CLAUDE.md +
  architecture.md only); left untouched because the lane contract was
  "add the 2 missing rf:Crate individuals ONLY". Coordinator may want a
  follow-up ttl-intro correction.
- CLAUDE.md retains pre-existing dirty state from another lane; my additions
  are limited to the count prose + two crate rows.
