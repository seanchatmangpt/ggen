# As-Found: Repository and Package Structure (Agent A)

Surveyed at commit `033e576f7` ("chore(release): bump version to 26.7.24 (#355)"), branch
`retrofit/ggen-self-g1`, worktree `/tmp/wt-selfretro`. All facts below are commands run
against that worktree, not recollection from CLAUDE.md prose. Unknowns are marked explicitly
where a check was not performed.

## 1. Workspace crates (Cargo.toml members)

`grep -c '^  "crates/' Cargo.toml` → **16** array entries, plus the root `ggen` package = 17
total workspace members. This matches CLAUDE.md's claimed count (see `#L50-71` of the root
`Cargo.toml`).

| # | Path | Notes |
|---|------|-------|
| 1 | `crates/ggen-config` | |
| 2 | `crates/ggen-marketplace` | |
| 3 | `crates/ggen-cli` | binary crate, see §6 |
| 4 | `crates/ggen-graph` | |
| 5 | `crates/ggen-lsp` | binary crate, see §6 |
| 6 | `crates/ggen-engine` | binary crate, see §6 |
| 7 | `crates/praxis-core` | |
| 8 | `crates/praxis-graphlaw` | |
| 9 | `crates/powl2-decompose` | |
| 10 | `crates/chicago-tdd-tools` | |
| 11 | `crates/bcinr-pddl` | |
| 12 | `crates/bcinr-mfw-ir` | |
| 13 | `crates/genesis-types-v2` | |
| 14 | `crates/genesis-core-v2` | |
| 15 | `crates/cpmp` | |
| 16 | `crates/ggen-cheat-scanner` | binary crate, see §6 |
| 17 | (root) `ggen` | package name `ggen`, `[package]` block at `Cargo.toml:8` |

`Cargo.toml:176` still carries a commented-out dependency line:
`# ggen-yawl = { path = "crates/ggen-yawl", version = "26.5.4" }  # crate not yet activated`.
`find crates -maxdepth 1 -iname "ggen-yawl*"` → empty. The comment is accurate — no such
directory exists on disk; this is a forward-looking placeholder, not drift.

Confirmed absent (checked directly, not inferred from doc prose):
- `crates/ggen-core` — `ls crates/ggen-core` → "No such file or directory". CLAUDE.md's claim
  that it is "fully deleted, not merely disconnected" is verified true at this commit.
- `find crates -maxdepth 1 -iname "stpnt*" -o -maxdepth 1 -iname "genesis-core"` → empty
  (no output). Both `stpnt` and non-v2 `genesis-core`, named in CLAUDE.md as removed dead
  code, are confirmed absent.

## 2. Packs — `packs/` (33 directories on `main`/this branch)

`ls packs/ | wc -l` → **33**. Full list (`ls packs/ | sort`):

```
affidavit-pack, anti-llm-cheat-lsp-pack, cargo-cicd-pack, chicago-tdd-tools-pack,
clap-noun-verb-pack, claude-code-pack, dogfood-lifecycle-pack, ggen-constitution-pack,
ggen-release-pack, ggen-verify-pack, gh-terraform-pack, github-actions-pack,
level-five-book-pack, lsp-max-pack, ma-case-study-pack, mcpp-pack, mfact-pack, mfw-pack,
osx-clnr-pack, praxis-core-pack, self-monitoring-pack, star-toml-pack, tcps-cli-pack,
tcps-core-pack, tcps-ffi-pack, tcps-release-pack, tcps-std-pack, tcps-wasm-pack,
wasm4pm-algorithms-pack, wasm4pm-cognition-pack, wasm4pm-compat-pack, wasm4pm-facts-pack,
wasm4pm-pack
```

This is 5 more than the 28-pack inventory `.claude/rules/architecture.md` cites as
"generated 2026-07-19" — `ggen-constitution-pack`, `ggen-release-pack`, `gh-terraform-pack`,
`github-actions-pack`, `level-five-book-pack` are present on disk but not yet reflected in
that generated table. That table is stale relative to disk state as of this survey
(2026-07-20); it should be regenerated via `ggen sync run` against `.specify/repo-facts.ttl`
per the file's own header, not hand-edited.

### 2a. Retrofit-relevant unmerged branches (checked via `gh pr list --state open`)

`gh pr list --state open --limit 50` returned exactly 4 open PRs:

| # | Title | Branch | State |
|---|-------|--------|-------|
| 356 | feat(retrofit): 5-pack structural-rehabilitation pipeline | `feat/repo-retrofit-pipeline-packs` | OPEN |
| 354 | feat(mermaid-pack): admit Mermaid-diagram-as-IR pack | `feat/mermaid-pack` | OPEN |
| 353 | chore(release): bump version to 26.7.23 | `release/bump-26.7.23` | OPEN |
| 210 | feat(ggen-lsp): add GGEN-SRC-* first-class source law diagnostic family | `feat/ggen-lsp-source-laws` | OPEN (since 2026-06-06, oldest open PR) |

Verified directly against the worktree filesystem (not the PR diff) that neither #354's nor
#356's packs exist on `main`:
- `ls packs/ | grep -i mermaid` → no match ("NOT PRESENT on main").
- `ls packs/ | grep -iE "as-found|load-path|intervention|temporary-works|reconciliation"` →
  no match ("NOT PRESENT on main"). PR #356's five packs (`repo-as-found-pack`,
  `repo-load-path-pack`, `repo-intervention-pack`, `temporary-works-pack`,
  `repo-reconciliation-pack`) are unmerged — the pipeline this retrofit effort is presumably
  meant to run *through* does not yet exist on the branch this survey is running on.

**`github-actions-pack` status** (explicitly requested check): already admitted on `main`,
not an open-PR artifact. `git log --oneline -3 -- packs/github-actions-pack`:
```
a96b9ae24 feat(github-actions-pack): production-machinery pack — 4-layer workflow ontology,
           permission derivation, first generated families (#347)
```
Merged via #347. Confirmed present in the `ls packs/` listing above.

`gh-terraform-pack` (also present on disk) has 3 merge commits touching it:
`e36710b88` (#341, fleet ontology Phase 2), `0dd441050` (#348, W3 fleet census),
`9a6efea99` (#334, release-binary bootstrap fallback) — all merged, not open-PR state.

## 3. Templates

Two separate template surfaces exist, at different levels of maturity:

1. **`templates/`** (repo root, ~90 loose files) — a large flat directory of `.tera`/`.tmpl`/
   `.template` files (`ai-client-wrapper.tmpl`, `c4-component-diagrams.tera`,
   `clap_cargo.template`, `terraform-main.tera`, `k8s-deployment.tera`,
   `mcp-{elixir,go,java,rust,typescript}.tera`, `a2a-{elixir,go,java,rust,typescript}.tera`,
   plus odd entries like `with custom prefixes.tmpl` and `with inline Turtle RDF.tmpl` whose
   filenames contain literal spaces — see full listing captured in this survey's raw command
   output). This directory is *not* pack-scoped and does not follow the
   `packs/*/templates/*.tmpl` + frontmatter (`to:`/`sparql:`) pattern documented as the house
   pattern for this session. Whether any of these ~90 files are still wired into a live
   generation path or are orphaned scaffolding from before the pack-based architecture is
   **Unknown** — this survey did not trace consumers of each file; that is a load-path
   question for a subsequent agent (see the retrofit pipeline's own
   `repo-load-path-pack`, currently unmerged per §2a).
2. **`packs/*/templates/*.tmpl`** — the current, pack-scoped, house-pattern template
   location (not separately enumerated here; each pack's own `pack.toml` and `ontology.ttl`
   govern its templates).

## 4. `scripts/` (top-level, ~180+ entries)

`ls scripts/` returns well over 150 files, dominated by one-off remediation scripts whose
names describe a specific historical fix rather than a durable tool: `fix_compilation_errors.sh`,
`fix_marketplace_batch.sh`, `fix_marketplace_errors.py`, `fix_ok_return.py`,
`fix_remaining_errors.sh`, `fix_swarm_agent.sh`, `fix_test_imports.py`,
`fix_test_result_final.py`, `fix_test_result_shadowing.py`, `fix_test_result_types.py`,
`fix_test_super_import.py`, `fix_unwrap.{py,rs,sh}`, `fix-async-steps.sh`,
`fix-bdd-compilation.sh`, `fix-command-compilation.sh`, `fix-doctest-format.sh`,
`fix-expect-violations.sh`, `fix-panic-points.rs`, `full_recovery.sh`, `full_recovery2.sh`
(two numbered variants of the same remediation), `patch-all-packages.sh`,
`remove_duplicate_result_aliases.py`. None of these were traced for current call sites in
this pass (Unknown — load-path question). Their presence at the top level of `scripts/`
alongside currently-load-bearing scripts (`ggen-receipt-gate.sh`, `ggen-receipt-generate.sh`,
`onto_shacl_check.py`) with no subdirectory separation is itself an observation: there is no
current/archived split inside `scripts/` the way there is inside `examples/` (§5).

## 5. Duplicated / abandoned subsystems

### 5a. `examples/` — five separate archive-named subdirectories

```
examples/
├── _archive
├── archive
├── archive_2025
├── archive_ggen_core
├── clap-noun-verb-cli
├── receiptctl
├── tcps-generated
└── tpot2-wasm4pm-autoconfig
```

Four distinctly-named archive directories (`_archive`, `archive`, `archive_2025`,
`archive_ggen_core`) coexist rather than one canonical archive location — this is itself a
minor structural-drift finding (three different naming conventions for "old examples we kept").
`examples/archive_ggen_core/` contains: `advanced-ai-usage/`, `advanced-pipeline/`,
`ai-microservice/`, `mcp-a2a-self-hosting/`, `stray-rs-files/` — all presumably referencing
the now-fully-deleted `ggen-core` crate (CLAUDE.md's "removed in 2026-07-17" claim); this
survey did not open individual files to confirm they still import `ggen_core::` symbols
(Unknown), but the directory name and CLAUDE.md's crate-removal history make that the
working hypothesis. `examples/receiptctl/` is cited as load-bearing elsewhere in this repo's
own CLAUDE.md (`just pre-commit`'s `guard-pack-proofs` gate "re-syncs + re-tests
`examples/receiptctl`") — so at least one `examples/` subdirectory is a live gate dependency,
not archive material.

### 5b. Superseded docs cited in CLAUDE.md as removed — confirmed still absent

- `docs/architecture/COMPRESSED_REFERENCE.md` — `ls` → "No such file or directory". CLAUDE.md
  says this was "removed from disk … verified 2026-07-19". Re-verified absent today
  (2026-07-20).
- `docs/crate-audits/` (whole directory, including `AUDIT_DASHBOARD.md`) — `ls` → "No such
  file or directory". Also re-verified absent, matching CLAUDE.md's claim.

Both of CLAUDE.md's "removed but maybe still present" flags are confirmed genuinely removed,
not drift.

### 5c. Root-level `.md` file sprawl — direct violation of the repo's own Rule 2

`.claude/rules/_core/absolute.md` Rule 2 states "No Root Files — NEVER save files to root -
use subdirectories." `ls *.md` at repo root returns 34 files, e.g.:
`audit_report.md`, `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md`,
`CRATE_CONSOLIDATION_PROPOSAL.md`, `DEFINITION_OF_DONE_DELIVERY.md`,
`DOCTEST_VALIDATION_PROGRESS.md`, `DOCUMENTATION_AUDIT_REPORT.md`, `EVIDENCE_SYNTHESIS.md`,
`FIXTURE_AUDIT_REPORT.md`, `FIXTURE_BLOCKING_ISSUES.md`,
`FIXTURE_SETUP_QUICK_REFERENCE.md`, `FUSION_THESIS.md`, `IMPLEMENTATION_SUMMARY.md`,
`LSP-ARD-PRD.md`, `MANIFESTO.md`, `MARKETPLACE_AUDIT_REPORT.md`, `MERGE_READINESS_AUDIT.md`,
`ORIGINAL_REQUEST.md`, `PATH_A_EVIDENCE_INDEX.md`, `PATH_A_MERGE_CHECKLIST.md`,
`PHASE4_IMPLEMENTATION.md`, `PHASE5_WAVE2_PLANNING_AUDIT.md`,
`PhD_THESIS_GGEN_ONTOLOGY_SYNTHESIS.md`, `POST_RELEASE_CHECKLIST.md`, `PROJECT.md`,
`RELEASE_STANDING.md`, `ROLLBACK.md`, `TCPS-PACK-ARD-PRD.md`, `TEST_INFRA.md`, plus the
conventional `README.md`, `CHANGELOG.md`, `CONTRIBUTING.md`, `CONTRIBUTORS.md`,
`CODE_OF_CONDUCT.md`, `SECURITY.md`, `SKILLS.md`, `AGENTS.md`, `CLAUDE.md`,
`CONSTITUTION.md`. `ls -la` on repo root shows 181 total directory entries. This is a
concrete, checkable instance of the repo violating its own committed rule; every
one-off-audit-named file above (`*_AUDIT_REPORT.md`, `*_ANALYSIS_*.md`,
`*_CHECKLIST.md`, `*_EVIDENCE_*.md`) is a candidate for relocation to `docs/` per the same
rule file's guidance, or archival — this survey does not judge which; it records the fact.

## 6. Build / binary entry points

`grep -rn "^\[\[bin\]\]" crates/*/Cargo.toml Cargo.toml` →

| Crate | `[[bin]]` |
|-------|-----------|
| `ggen-cheat-scanner` | 1 (`Cargo.toml:11`) |
| `ggen-cli` | 1 (`Cargo.toml:1`) |
| `ggen-engine` | 2 (`Cargo.toml:131`, `:135`) |
| `ggen-lsp` | 2 (`Cargo.toml:137`, `:141`) |

Root `ggen` package (`Cargo.toml:8` `[package] name = "ggen"`) has **no** `[[bin]]` target —
confirmed by its own in-file comment (`Cargo.toml`, immediately after the `include = [...]`
block): the former root-level `[[bin]] name = "ggen"` was removed 2026-07-16 as a
byte-identical duplicate of `ggen-cli`'s own `[[bin]] name = "ggen"` (both `main.rs` files
delegated to `ggen_cli_lib::cli_match()`); the duplicate caused a real bin-name collision
racing for `target/debug/ggen`, and also blocked `cargo publish --dry-run -p ggen` since
`ggen-cli-lib` depends on the never-publishable `ggen-engine`. This is documented in-repo,
in-Cargo.toml, as a fixed defect — not a currently-open issue.

## 7. `.specify/repo-facts.ttl` — generation source of truth

`.specify/repo-facts.ttl` exists (93,423 bytes, last modified 2026-07-20 22:39). Per
CLAUDE.md and `.claude/rules/architecture.md`, this is the ontology source that
`ggen sync run` uses to regenerate `.claude/rules/architecture.md`'s crate-map table and
CLAUDE.md's marked `<<<<<<< GENERATED … >>>>>>> MANUAL` block. This survey did not re-run
`ggen sync run` to check whether the currently-checked-in generated tables are byte-identical
to a fresh regeneration (Unknown — that is a direct check for whoever owns the
"generated products" reconciliation workstream; the drift already found in §2 — 33 packs on
disk vs. 28 in the last-generated table — suggests the generated tables are currently stale).

## 8. "GENERATED by" header sample (10 files, first alphabetical hits)

`grep -rl "GENERATED by" --include="*.md" --include="*.rs" --include="*.toml" .` (excluding
`target/` and dotdirs), first 10 hits:

```
CONSTITUTION.md
CLAUDE.md
crates/ggen-cli/src/lib.rs
crates/ggen-engine/tests/github_actions_pack_e2e.rs
crates/ggen-engine/src/lib.rs
crates/ggen-engine/src/sync.rs
crates/ggen-engine/src/verbs/law.rs
crates/ggen-engine/src/verbs/graph.rs
crates/ggen-engine/src/verbs/sync.rs
crates/ggen-engine/src/verbs/doctor.rs
```

Note that `crates/ggen-engine/src/*.rs` files carrying a "GENERATED by" marker are
hand-written pipeline source with a generated-*comment* convention (module-doc headers), not
literal ggen-sync output — this survey did not open each file to distinguish
generated-content vs. generated-comment-only in this pass (Unknown; flagged for whoever
verifies the "generated files are never hand-edited" invariant against this sample).

## 9. Release surfaces observed but not deep-audited (Unknown, flagged for follow-up)

`scripts/` contains a substantial homebrew/release surface
(`release-brew.sh`, `update-homebrew-formula.sh`, `update-homebrew-formula-e2e.sh`,
`update-and-validate-homebrew.sh`, `README-homebrew.md`) and a gVisor/OCI container surface
(`build-ggen-oci.sh`, `build-gvisor-runsc.sh`, `run-ggen-gvisor*.sh` x5,
`setup-gvisor*.sh` x4, `install-runsc*.sh` x2, `configure-containerd-gvisor.sh`) — neither
was traced for current CI wiring or dead-code status in this pass. Flagged as a load-path
question for a follow-up agent.

## Evidence commands run (for reproducibility)

```bash
git -C /Users/sac/ggen fetch origin
git -C /Users/sac/ggen worktree add /tmp/wt-selfretro -b retrofit/ggen-self-g1 origin/main
cd /tmp/wt-selfretro
grep -n '"crates/' Cargo.toml
grep -c '^  "crates/' Cargo.toml
ls packs/ | sort; ls packs/ | wc -l
gh pr list --state open --limit 50
gh pr view 354; gh pr view 356
ls packs/ | grep -i mermaid
ls packs/ | grep -iE "as-found|load-path|intervention|temporary-works|reconciliation"
git log --oneline -3 -- packs/github-actions-pack
git log --oneline -3 -- packs/gh-terraform-pack
ls templates/
ls scripts/
grep -rl "GENERATED by" --include="*.md" --include="*.rs" --include="*.toml" .
ls docs/architecture/COMPRESSED_REFERENCE.md
ls docs/crate-audits/
ls crates/ggen-core
ls examples/
ls examples/archive_ggen_core
find crates -maxdepth 1 -iname "stpnt*" -o -maxdepth 1 -iname "genesis-core"
find crates -maxdepth 1 -iname "ggen-yawl*"
grep -rn "^\[\[bin\]\]" crates/*/Cargo.toml Cargo.toml
ls -la | wc -l; ls *.md
```
