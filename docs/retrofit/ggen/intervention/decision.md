# ggen Intervention Decisions

Classifies every real gap found by the as-found and load-path surveys
(`docs/retrofit/ggen/as-found/*.md`, `docs/retrofit/ggen/load-path/*.md`,
`.tcps/retrofit/ggen/load-path.ttl`) into one of five intervention classes, using the
strict rules below. No hypothetical gaps are included — every row cites its source finding.

## Classification rules applied

- **preservation** — already correct; keep as-is, add a regression guard if none exists.
- **repair** — a single authoritative path exists but has a local defect (bug, missing wire,
  stale artifact); fix in place, no new abstraction.
- **strengthening** — the authoritative path is correct but under-guarded (missing gate, missing
  sabotage test, missing enforcement); add a check without changing the path's shape.
- **rehabilitation** — the path is structurally sound but has accumulated enough drift/scope
  creep that it needs a phased rework; REQUIRES a stated continuity plan (what keeps working
  during the rework, how rollback works at each phase).
- **replacement** — the path itself is wrong (duplicate authority, contradicts house pattern,
  or unsalvageable); REQUIRES a stated migration plan (old→new mapping, cutover point, what is
  deleted and when).

Nothing below defaults to rehabilitation without a continuity plan attached, and nothing
defaults to replacement without a migration plan attached, per the task's strict instruction.

## Decisions

### 1. ggen-verify-pack evidence producers not wired into ggen's own sync — **strengthening**
Source: receipt-system.md finding 1 (6 of 8 equivalence classes `Unknown` in the live
`.ggen-v2/receipt.json`); PUBLICATION_JUDGMENT.md condition 15 PARTIAL.
The authoritative path (`ggen sync run` → Stage 2b gates → v2 receipt-epoch builder) is
structurally correct and already proves 2 of 8 classes (`source`, `config`). This is not a
broken path, it is an under-populated one: the pack's 4 evidence producers
(`produce_docs_manifest_sh.tmpl` etc.) exist but this repo's own `ggen.toml` does not invoke
them. Fix: add the producer invocations to ggen's own manifest/CI so `compiled_binary`, `docs`,
`tests`, `gates` stop reading `Unknown`. No new abstraction — reuses the pack as designed.

### 2. `andon` hardcoded `Green` in `sync.rs:1857` — **repair**
Source: architecture-semantics.md finding 1; load-path.ttl (tagged Blocking).
`write_receipt` writes `Andon::Green` unconditionally instead of deriving it from gate/law
outcomes. Single authoritative function, single defect, no continuity/migration plan needed —
change the literal to a computed value from the Stage 2b gate results already in scope.

### 3. Stale pack-count table in `.claude/rules/architecture.md` — **repair**
Source: architecture-semantics.md finding 6 (32 documented vs 33 on disk, `github-actions-pack`
missing) and repository.md (28 documented vs 33 on disk — an earlier, worse instance of the
same defect class already partially fixed once this session).
This is a GENERATED file per CLAUDE.md's own header; the fix is `ggen sync run` to regenerate
it from `.specify/repo-facts.ttl` after the ontology is updated with the missing pack
individuals (`ggen-constitution-pack`, `ggen-release-pack`, `gh-terraform-pack`,
`github-actions-pack`, `level-five-book-pack`). Repair, not rehabilitation: one generation rule,
one stale input.

### 4. Law XII cites a nonexistent test (`reasoner_independence_e2e`) — **repair**
Source: architecture-semantics.md finding 1.
Single ontology individual with a false `provenBy` claim (already self-flagged in the TTL per
the same finding). Fix: either write the named test or correct the citation to
`ccn:mechanized false` with an honest gap statement, matching Laws VII/XI/XIV's pattern.

### 5. Sabotage-fixture coverage gap (12 untested fixtures, 3 packs) — **strengthening**
Source: receipt-system.md finding 6.
Existing packs (`dogfood-lifecycle-pack`, `ma-case-study-pack`, `self-monitoring-pack`) already
have a working Chicago-TDD sabotage-test pattern proven for their *other* fixtures — this is not
a broken path, it is incomplete coverage of an already-correct pattern. Add the missing
`#[test]` fns following the exact pattern of the fixtures that already pass
(`ma_case_hook_actuation.rs`, `self_monitoring_hook_actuation.rs`).

### 6. Constitution ontology not wired into `praxis-core` admission — **rehabilitation**
Source: architecture-semantics.md finding 1; PUBLICATION_JUDGMENT.md condition 12 PARTIAL.
**Continuity plan required and stated:** `DefaultLaw::admit` in `praxis-core/src/law.rs` keeps
functioning unmodified as the runtime admission path throughout the rework. Phase 1: add a
read-only SPARQL bridge that loads `ccn:Law` facts and logs (not enforces) divergence from
`DefaultLaw`'s hardcoded rules. Phase 2 (separate follow-up, out of scope here): once the bridge
has run clean for a defined observation window, flip enforcement for the mechanized=true subset
only (Laws with real `provenBy` evidence), leaving unmechanized Laws VII/XI/XIV as documented
gaps, not silently promoted. Rehabilitation, not repair, because it touches a live admission
path used by every sync — a one-shot patch risks a fail-open regression the load-path survey
already flagged as Blocking.

### 7. No branch protection on `main`, single admin collaborator, no CODEOWNERS — **strengthening**, gated by external authorization
Source: load-path.ttl / all-load-paths.md (Blocking SPOFs, confirmed live via `gh api`).
The path (GitHub PR/merge) is structurally sound; it lacks an enforcement gate. This requires
mutating branch-protection settings and collaborator config on `seanchatmangpt/ggen` — both are
explicitly listed in this task's external-authorization-gated actuation classes. **Not
implemented here.** Named as an authorization packet (see decision-matrix.md row 7) for the
user to approve and execute, most naturally via `gh-terraform-pack`'s already-merged desired
state.

### 8. `just sync` / `just sync-dry` recipes broken (wrong flag names) — **repair**
Source: CLAUDE.md's own documented finding, reconfirmed live by architecture-semantics survey.
`justfile`'s `sync`/`sync-dry` recipes pass `--audit`/`--dry_run true`, neither of which the
live `ggen sync run` verb accepts (only `--dry-run`/`--watch`). Single recipe body, single fix:
correct the flag names to match the real CLI surface.

### 9. Root templates/ directory (90 files) disconnected from pack-scoped convention — **rehabilitation**
Source: repository.md (two disconnected template surfaces; liveness of root `templates/` files
Unknown).
**Continuity plan required and stated:** liveness is unverified — do not delete anything before
Phase 1 (trace every root `templates/*.tmpl` for real `ggen.toml` consumers, distinguishing
live-but-legacy from truly dead) establishes which of the 90 files have zero consumers. Phase 2
(separate follow-up): migrate live-but-legacy templates into their owning pack's
`packs/*/templates/` directory one pack at a time, each migration validated by a real `ggen
sync run` byte-diff before the old file is deleted. Rehabilitation because deleting first and
asking later risks silently breaking a live generation path — exactly the fail-open pattern
this repo's own `coding-agent-mistakes.md` names as Mistake Class 3.

### 10. 34 root-level `.md` files violating the repo's own "No Root Files" rule — **repair**
Source: repository.md.
Not structural — no generation logic depends on file location, only `.claude/rules/_core/
absolute.md` Rule 2. Fix: move each file to `docs/` (or `docs/archive/` per the Markdown
Document Standards rule for superseded docs), no continuity plan needed since these are static
reference docs, not live inputs to any pipeline.

### 11. `scripts/` 150+ one-off `fix_*`/`fix-*` remediation scripts, no archive/current split — **repair**
Source: repository.md.
Same class as #10: organizational drift, not a broken authoritative path. Fix: introduce
`scripts/archive/` (mirroring `examples/`'s existing, if messy, archive convention) and move
scripts with zero recent `git log` activity and zero CI references into it.

### 12. Four differently-named `examples/` archive directories (`_archive`, `archive`,
`archive_2025`, `archive_ggen_core`) — **replacement**
Source: repository.md.
**Migration plan required and stated:** old→new mapping is "all four directories' contents move
under a single canonical `examples/archive/<original-dirname>/` subpath, preserving git history
via `git mv`." Cutover point: a single commit that performs all four moves together (so no
window exists where some archived examples are canonical-path and others aren't). What is
deleted: nothing is deleted, only relocated — `examples/archive_ggen_core/` in particular must
first be confirmed (not yet done — flagged Unknown by the as-found survey) to reference the
deleted `ggen_core` crate before its contents are also marked dead-not-just-archived. This is
replacement (one canonical archive path replaces four) rather than repair because there is no
single existing authoritative location to repair into — all four are equally non-canonical.

### 13. mermaid-pack does not exist despite PR #354 / task-brief premise — **preservation** (of correct state) + repair of the stale claim
Source: architecture-semantics.md finding 4.
There is no live gap here — `packs/` correctly has no mermaid-pack today. The defect is a false
belief in a task brief and possibly in PR #354's own description; the "fix" is documentation
(this decision doc + a note when PR #354 is reviewed/merged), not a code change. Recorded here
so this survey's findings don't get miscited as "we need to build mermaid-pack" downstream.

### 14. `.claude/rules/architecture.md` pack-inventory table generation freshness (recurrence of #3's class) — folded into #3, not a separate row.

## See Also

- `docs/retrofit/ggen/intervention/decision-matrix.md` — full prioritized table
- `docs/retrofit/ggen/intervention/target-state.mmd` — target-state diagram
- `.tcps/retrofit/ggen/intervention.ttl` — RDF facts for each decision
- `docs/retrofit/ggen/as-found/` — evidence base
- `docs/retrofit/ggen/load-path/` — SPOF evidence base
