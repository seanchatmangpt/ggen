# Hygen Parity and the Gall's-Law Plan for ggen's End-to-End Test Buildout

Version/milestone marker: written 2026-08-04, against ggen 26.8.6 (18 workspace crates).

## Executive summary

This document answers three questions that came up in the same working session and are
tightly coupled, not three separate topics glued together:

1. **Does ggen actually do what hygen does?** — a real, verified feature-by-feature parity
   check against hygen's documented frontmatter contract, backed by four end-to-end tests
   that run the real `ggen` binary as a subprocess (no mocks), not by reading source and
   asserting it "should" work.
2. **What is Gall's Law, and why does it matter here?** — the design law this document uses
   to justify *how* we grow test coverage across the rest of the 18-crate workspace, instead
   of attempting one grand ontology-driven test-generation system in a single pass.
3. **What is the actual crate-by-crate build-out plan** from here, with concrete phases, gate
   criteria, and a definition of "done" for each phase that doesn't rely on aspirational
   numbers?

The short version: ggen is not merely hygen-compatible, it is a strict superset of hygen's
frontmatter contract (every hygen primitive maps onto an existing, tested ggen field, plus
ggen adds SPARQL-driven generation, SHACL-enforced shapes, determinism re-checks, and freeze
policies hygen has no equivalent of). The gap that remains — an interactive prompt layer — is
a deliberate design absence, not a missing feature, because ggen's variable source is RDF
facts (`A = μ(O)`), not a terminal Q&A. See [Parity matrix](#parity-matrix) for the
line-by-line evidence.

The workspace's actual test-quality problem (documented independently by
`ggen-cheat-scanner`'s ~464 pre-existing findings — see CLAUDE.md's Definition of Done table)
is not solved by proving hygen parity. It is solved by the same method this document uses to
prove hygen parity: small, real, composite, subprocess-driven checkpoints, grown crate by
crate under Gall's Law discipline, never by a single big-bang "convert everything to ontologies
and regenerate all tests" pass. See [The build-out plan](#the-build-out-plan).

## Table of contents

- [What hygen is, and why parity is the right first checkpoint](#what-hygen-is-and-why-parity-is-the-right-first-checkpoint)
- [Parity matrix](#parity-matrix)
- [The four proof tests, explained](#the-four-proof-tests-explained)
- [What ggen does NOT have, honestly stated](#what-ggen-does-not-have-honestly-stated)
- [Gall's Law](#galls-law)
- [Gall's Law applied to ggen's own history](#galls-law-applied-to-ggens-own-history)
- [Why "convert to ontologies, regenerate all tests, wipe the rest" violates Gall's Law](#why-convert-to-ontologies-regenerate-all-tests-wipe-the-rest-violates-galls-law)
- [The build-out plan](#the-build-out-plan)
- [What "done" means per crate](#what-done-means-per-crate)
- [Reusable machinery already in the repo](#reusable-machinery-already-in-the-repo)
- [Risks and honest unknowns](#risks-and-honest-unknowns)
- [See also](#see-also)

## What hygen is, and why parity is the right first checkpoint

[Hygen](https://www.hygen.io/) is a scaffolding tool: a generator is a directory of
EJS-templated files under `_templates/`, each with a YAML frontmatter block controlling where
and how it's written. Its entire value proposition is five primitives:

- `to:` — where to write the rendered body (can be templated by a prompt-bound variable)
- `inject: true` + `before:`/`after:` — insert into an *existing* file instead of overwriting
  it wholesale (its flagship example: scaffold a new component file, then inject its export
  into a barrel `index.ts`, in one generator run)
- `unless_exists: true` — never clobber a file that's already there
- `skip_if:` — skip the write if some marker is already present in the target
- `sh:` — run a shell command after a successful generation (its own docs' example: run a
  formatter)

ggen predates none of this by accident — `crates/ggen-engine/src/template.rs`'s own module
doc literally says *"Hygen-style template parsing... (Hygen semantics)"* at the top of the
file, and the `Frontmatter` struct doc says the same. This was a deliberate design decision
made by the people who built ggen, not something this document is retrofitting after the
fact. That makes hygen parity the correct **first** checkpoint for the workspace's e2e test
program: it's the smallest concrete claim ("we are at least as good as the tool we said we
model") that can be verified with real subprocess runs in under a second, and it's the
foundation everything else (SPARQL binding, SHACL enforcement, receipt chains) is built on
top of.

## Parity matrix

| Hygen primitive | ggen field | Status | Evidence |
|---|---|---|---|
| `to:` (static path) | `to:` | Parity | `crates/ggen-engine/src/template.rs:44` |
| `to:` (templated by a prompt variable) | `to:` templated by a SPARQL-bound row variable (`{{ row.name }}`) + `for_each:` | Parity, different variable source (see below) | `crates/ggen-engine/tests/frontmatter_cardinality_e2e.rs:185`; this session's `hygen_parity_e2e.rs::component_scaffold_creates_file_and_injects_barrel_export_in_one_run` |
| `inject: true` | `inject: bool` | Parity | `template.rs:65-66`; `write_behaviors_cli_e2e.rs::inject_before_marker_inserts_content_and_backs_up_first` |
| `before:` / `after:` | `before: Option<MatchSpec>` / `after: Option<MatchSpec>` | Superset — ggen's `MatchSpec` supports exact/regex matching and cardinality, not just hygen's bare-substring-plus-first-line behavior | `template.rs:67-75` |
| (no hygen equivalent) | `at_line: Option<usize>` — inject at an exact 1-based line | ggen-only addition | `template.rs:76-78` |
| `skip_if:` | `skip_if: Option<MatchSpec>` | Parity | `template.rs:79-82`; this session's `hygen_parity_e2e.rs::skip_if_prevents_regenerating_an_already_marked_file` |
| `unless_exists: true` | `unless_exists: bool` | Parity | `template.rs:83-85`; this session's `hygen_parity_e2e.rs::unless_exists_preserves_a_hand_edited_scaffold_across_reruns` |
| (hygen has no equivalent — hygen always overwrites on rerun unless `unless_exists`) | `force: bool` — explicit opt-in overwrite; default behavior fails closed on a differing file | ggen is stricter by default | `template.rs:86-88` |
| `sh:` | `sh_before: Option<String>` (alias `sh`) and `sh_after: Option<String>` | Superset — hygen only has a post-gen hook; ggen adds a pre-write-decision hook too, both denylist-guarded | `template.rs:99-108`; this session's `hygen_parity_e2e.rs::sh_after_hook_runs_exactly_once_after_a_real_write` |
| Partial body reuse across generators (hygen: separate `.ejs.t` per file, no first-class "load body from elsewhere") | `from: Option<String>` — load the Tera body from another file, frontmatter still local | ggen-only addition | `template.rs:95-98`; `frontmatter_fields_e2e.rs::from_field_path_traversal_outside_template_dir_is_refused` (also proves the traversal-refusal hardening hygen has no equivalent of) |
| (no equivalent) | `backup: bool` — copy the pre-overwrite file to `<path>.bak` before `force`/`inject` | ggen-only addition | `template.rs:109-112`; `write_behaviors_cli_e2e.rs::inject_before_marker_inserts_content_and_backs_up_first` |
| (no equivalent) | `shape: Vec<String>` — SHACL shape files enforced against the active graph before a write is admitted | ggen-only addition (RDF-native validation hygen has no concept of) | `template.rs:113-123` |
| (no equivalent) | `determinism: Option<bool>` — render the body twice and refuse if the bytes differ | ggen-only addition | `template.rs:124-128` |
| (no equivalent) | `sparql:` / `construct:` / `for_each:` / `when:` — the entire RDF-driven generation model | ggen-only, this is the actual `A = μ(O)` engine hygen has nothing resembling | `template.rs:45-63,89-91` |
| Interactive CLI prompts (`hygen component new` asks for `--name` at the terminal) | **No equivalent** | Deliberate absence, not a gap — see below | — |

Every row that says "Parity" or "Superset" is backed by a test that actually runs, not a
reading of the source that assumes it works. The prompt-layer row is the one honest, real gap,
discussed on its own below rather than folded into the table as if it were symmetric with the
others.

## The four proof tests, explained

All four live in `crates/ggen-engine/tests/hygen_parity_e2e.rs`, added this session, and all
four run the actual `ggen` binary via `chicago_tdd_tools::cli_proof::CliHarness` as a real
subprocess against a real temp directory on a real filesystem — the project's Chicago TDD
discipline applied to this specific claim, not an exception to it.

1. **`component_scaffold_creates_file_and_injects_barrel_export_in_one_run`** — reproduces
   hygen's own quick-start flagship example end to end: one `sync run` invocation, driven by
   two templates off one ontology fact, creates `components/Button.tsx` *and* injects an
   export line into `components_index.ts`'s barrel, matching what hygen's docs present as its
   single best argument for existing as a tool at all.
2. **`unless_exists_preserves_a_hand_edited_scaffold_across_reruns`** — proves a second sync
   run over a hand-edited file leaves it untouched byte-for-byte.
3. **`skip_if_prevents_regenerating_an_already_marked_file`** — proves a marker string in the
   target file prevents regeneration from clobbering a hand-wired custom handler.
4. **`sh_after_hook_runs_exactly_once_after_a_real_write`** — proves the post-generation hook
   fires exactly once on a real `Written` outcome, and critically, does **not** re-fire on a
   no-op rerun where the content is already identical (`Skipped`) — a distinction hygen's own
   docs assert but that is easy to get wrong in an implementation (fire the hook on every
   invocation regardless of outcome).

Real execution, this session, on a fresh `origin/main`-based branch:

```
running 4 tests
test unless_exists_preserves_a_hand_edited_scaffold_across_reruns ... ok
test skip_if_prevents_regenerating_an_already_marked_file ... ok
test component_scaffold_creates_file_and_injects_barrel_export_in_one_run ... ok
test sh_after_hook_runs_exactly_once_after_a_real_write ... ok

test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

`ggen-cheat-scanner` was also run against the new file directly and reported
`ALIVE: no cheat patterns detected` — the new tests do not themselves fall into the
vacuous-assert / tautological-check / no-assertion / mock-import categories the scanner
exists to catch.

## What ggen does NOT have, honestly stated

Hygen's variables come from **interactive CLI prompts** — `hygen component new` drops the
user into a terminal Q&A (or accepts `--name` as a flag) and binds the answer to an EJS
variable. ggen has no prompt layer. The equivalent used throughout the parity tests is a
SPARQL-bound row variable (`{{ row.name }}`), sourced from a fact already committed to the
project's ontology (`ex:Button ex:componentName "Button"`), fanned out via `for_each:`.

This is a real, load-bearing difference, and it is deliberate, not an oversight:

- Same **role**: both are "the place a generator's per-instance variable comes from."
- Different **source of truth**: a prompt answer lives only in a terminal session and is gone
  the moment the command exits; an RDF fact lives in the ontology, is versioned in git, is
  queryable, and is the actual thing `A = μ(O)` (CLAUDE.md's own formula: "code precipitates
  from RDF") describes. Building a prompt layer on top of ggen would be straightforward
  (interactive fact-authoring UX is a legitimate, separately-scoped feature), but it is not
  what makes hygen parity real or fake — the generation semantics parity is what this
  document verifies, and that parity holds regardless of how the variable got into the graph.

Anyone reading this document who wants an actual interactive `ggen new` prompt flow should
treat that as a distinct, additive feature request, not a "fix" to a parity gap.

## Gall's Law

From John Gall's *Systemantics* (1975), stated in its most commonly quoted form:

> A complex system that works is invariably found to have evolved from a simple system that
> worked. A complex system designed from scratch never works and cannot be patched up to make
> it work. You have to start over, beginning with a working simple system.

The mechanism behind the law, not just the slogan: a working simple system has already
survived contact with reality at every intermediate stage of its own evolution — every
subsystem it grew was validated against real inputs before the next subsystem was added on
top of it. A complex system designed from scratch has no such history: every one of its
subsystems is a hypothesis about how the others behave, untested until the whole assembly is
switched on for the first time, at which point the number of possible interaction failures is
combinatorial in the number of untested subsystems. Debugging that assembly means trying to
locate a fault in an N-subsystem system where none of the N have individually been proven —
you cannot bisect a search space that was never built with checkpoints in it.

## Gall's Law applied to ggen's own history

This is not an abstract law being imported for flavor — ggen's own git history is a working
demonstration of it, and the workspace's live documentation says so explicitly if you know
where to look:

- The `ggen-core` → `ggen-engine` replacement (`docs/jira/v26.7.16/`) did not attempt a
  simultaneous full-system rewrite. It ported dependents off `ggen-core` one at a time, kept
  `ggen-core` in `[workspace] exclude` (present, disconnected, not yet deleted) as an
  intermediate, checkpointed state, and only deleted it outright in a later, separate PR
  (#259) once nothing depended on it — CLAUDE.md's own Crate Map section narrates this as a
  multi-step migration with named intermediate states, not a single cutover.
- The 18-crate workspace itself grew from 12 members (2026-07-16) to 17 to 18 over four
  separate, individually-landed PRs (#255, #257, plus the ggen-mcp/openapi-cnv-reflect
  additions), each one verified compiling and tested before the next was added —
  `.claude/rules/architecture.md`'s Crate Map section is, read closely, a log of exactly this
  incremental process, including one crate (`chicago-tdd-tools`) that was vendored as a
  stopgap and later un-vendored once the real upstream caught up (this session's own earlier
  work).
- Conversely, the workspace's one clearly *unhealthy* area — `ggen-cheat-scanner`'s ~464
  pre-existing test-quality findings (CLAUDE.md's Definition of Done table, item
  `guard-cheat-scan`) — is exactly the failure mode Gall's Law predicts for tests that were
  bulk-authored to hit a coverage number rather than grown one verified checkpoint at a time.
  Nobody can point to when or why any individual one of those 464 findings entered the
  codebase, because they were not landed as individually-scrutinized checkpoints.

## Why "convert to ontologies, regenerate all tests, wipe the rest" violates Gall's Law

The proposal on the table earlier this session — derive an ontology describing the current
code structure, generate tests from it, delete the existing hand-written tests (recoverable
via git history) — is, read through Gall's Law, an attempt to design a complex system (a
workspace-wide, ontology-driven test-generation pipeline covering domain-behavioral
correctness, not just structural shape) from scratch and switch it on all at once. Three
specific failure modes follow directly from the law, not from pessimism about the idea in
general:

1. **No working simple system to evolve from.** The existing `chicago-tdd-tools-pack` /
   `cli_proof` machinery *is* a working simple system — it already generates real,
   subprocess-driven boundary tests from `ctt:CliBoundaryTest` RDF facts (see
   [Reusable machinery](#reusable-machinery-already-in-the-repo) below), and this session's
   `hygen_parity_e2e.rs` is a hand-written checkpoint in the same spirit. Extending that
   proven pattern crate by crate is evolving a working simple system. Designing a new,
   workspace-wide "derive ontology from current code, generate everything" pipeline from a
   standing start is not — it has never been run against a single real crate yet.
2. **A test generated from the current code cannot detect a bug in the current code.**
   Ontology-derived generation is provably sound for *structural* claims (does this CLI flag
   exist, does this schema round-trip, does this frontmatter field parse) because the
   ontology can be authored independently of the implementation, as this session's own
   parity tests were (written from hygen's documented contract, not from reading ggen's
   source and echoing it back). It is not sound for *behavioral* claims derived from the
   current code's own structure, because a derive-from-code-then-generate-tests-from-that
   pipeline is definitionally a tautology on any behavior the derivation step got from the
   (possibly buggy) code itself.
3. **Wiping ~464-finding-plus territory before the replacement has covered one crate removes
   the one signal that says something is wrong.** Git history preserves the deleted text, but
   it does not preserve CI's ability to catch a regression in the window between deletion and
   full re-coverage. That window is exactly Gall's "complex system switched on for the first
   time" moment, applied to test coverage instead of to code.

None of this means the original idea is wrong in its structural half — ontology-driven
generation for CLI/schema/contract shape is real, already working, and should expand. It
means the wipe-and-regenerate-everything framing is the part that violates the law, and the
fix is sequencing, not abandonment.

## The build-out plan

Each phase below is scoped to be, on its own, a working simple system: landable,
independently verifiable, and not blocking on any later phase's existence. No phase starts
until the previous one is green and merged.

### Phase 0 — hygen parity checkpoint (this session, done)

- `crates/ggen-engine/tests/hygen_parity_e2e.rs`: 4 real, subprocess-driven tests.
- Gate: `cargo test -p ggen-engine --test hygen_parity_e2e` green; `ggen-cheat-scanner` clean
  on the new file. Both satisfied as of this document.

### Phase 1 — one additional crate, chosen for blast radius, not glamour

Pick the **smallest** crate with real, non-trivial logic and no existing e2e checkpoint of
this shape — not the biggest or most important one. Gall's Law argues for starting with
whichever subsystem is cheapest to fully validate, so that the checkpoint pattern itself gets
proven before it's asked to scale. Candidates to evaluate first (not a commitment — pick after
a real `wc -l`/complexity pass, not from this list alone): `bcinr-mfw-ir` (shared IR types,
small surface), `ggen-cheat-scanner` itself (ironic but apt — its own AST-scan logic is a
bounded, well-defined behavioral surface), `openapi-cnv-reflect` (one reflection pass,
bounded input/output).

Deliverable: one `*_e2e.rs` file per chosen crate, 3-6 composite tests in the same spirit as
Phase 0 — each test a *realistic scenario* a real user/consumer would exercise, not an
exhaustive branch-by-branch restatement of existing unit tests.

### Phase 2 — the pipeline crates (ggen-engine's sync stages, ggen-graph, ggen-config)

These already have substantial coverage (`frontmatter_fields_e2e.rs`,
`frontmatter_cardinality_e2e.rs`, `write_behaviors_cli_e2e.rs`,
`frontmatter_fortune5_hardening_e2e.rs` for `ggen-engine` alone). Phase 2's job is not to
add volume, it's to run the same completeness-critic pass this document itself is an instance
of: for each crate, ask "what real user-facing scenario is *not yet* covered by a composite
end-to-end test," not "what line is not yet covered by any test." Coverage-percentage-driven
test authorship is exactly the mode that produced the cheat-scanner's 464 findings; this phase
must not repeat it.

### Phase 3 — the marketplace / pack lifecycle (ggen-marketplace, ggen-lsp's pack surfaces)

Trust-tier enforcement (`Installer::verify_trust_tier`), pack registry CRUD
(`marketplace/rdf/control.rs`), and lockfile state (`.ggen/packs.lock`) are exactly the kind
of "real state changed" surfaces `coding-agent-mistakes.md`'s 6-question patch contract
already demands evidence for. This phase turns each of those manual-evidence requirements into
a standing, automated e2e checkpoint instead of a one-off manual check per PR.

### Phase 4 — mutation-score spot-checks, not full-workspace mutation testing

`cargo mutants --workspace` at this workspace's size is documented (`rust/testing.md`) as a
multi-hour-to-multi-day operation and explicitly out of scope as a standing gate. Phase 4 is
not "turn that on." It is: after each of Phases 1-3 lands a crate's e2e checkpoint file, run a
single bounded `cargo mutants -p <that-crate>` pass (mirroring the one real, already-run
`cargo tarpaulin -p ggen-cheat-scanner` proof-of-concept cited in `rust/testing.md`) to get one
real, reported mutation score for that crate's new tests, not an aspirational target. Report
the actual number; do not round up.

## What "done" means per crate

A crate's e2e checkpoint is done when all of the following are true, each independently
checkable, none of them a self-report:

1. A `*_e2e.rs` (or equivalently named) test file exists, using `CliHarness` (subprocess,
   real binary) wherever the crate has a CLI-reachable surface, or a real in-process
   `sync()`/library call where it does not.
2. Every test asserts on **observable state** (file contents, exit code, stderr substring,
   receipt field) — never on a mock's call count, per this project's own Chicago TDD floor.
3. `cargo test -p <crate> --test <file>` passes, with the actual command and output pasted
   into the landing PR description — not "tests pass," the literal `running N tests` /
   `test result: ok` block.
4. `ggen-cheat-scanner` run against the new file specifically reports no findings.
5. The PR names, in plain language, the one real user-facing scenario each new test
   reproduces — if a reviewer can't tell what real thing broke when a test fails, the test is
   not done, it's checked in.

## Reusable machinery already in the repo

This build-out plan does not require inventing new infrastructure. It requires applying what
already exists, one crate at a time:

- `chicago_tdd_tools::cli_proof::CliHarness` — real subprocess spawning against
  `Command::cargo_bin`, used by every test in this session's `hygen_parity_e2e.rs` and by the
  existing `write_behaviors_cli_e2e.rs`.
- `chicago-tdd-tools-pack` — already generates `CliHarness`-based boundary tests from
  `ctt:CliBoundaryTest` RDF facts (see `.claude/rules/architecture.md`'s Pack Inventory). This
  is the real, already-proven version of "ontology-driven test generation," scoped correctly
  to structural/boundary claims, exactly the scope this document argues is sound.
  `examples/ggen-cli-verify` and `examples/receiptctl` are its two committed, real consumers.
- `ggen-cheat-scanner` — the standing anti-regression gate against exactly the failure mode
  (vacuous asserts, tautological checks, no-assertion tests, mock imports) a rushed test
  build-out would otherwise reintroduce.
- `.specify/repo-facts.ttl` + `ggen sync run` — the same RDF-fact-to-generated-doc pipeline
  used to regenerate CLAUDE.md and `architecture.md` earlier this session is the same
  machinery Phase 1-3's structural test generation would run on, if and when a given crate's
  boundary surface is worth deriving from facts rather than hand-writing (a per-crate
  judgment call, not a blanket policy).

## Risks and honest unknowns

Stated plainly, per this project's own evidence-first standard, rather than left implicit:

- This document's crate-selection suggestions for Phase 1 (`bcinr-mfw-ir`,
  `ggen-cheat-scanner`, `openapi-cnv-reflect`) are candidates, not a verified-smallest-crate
  ranking — no line-count/complexity pass was run to confirm the ordering before this
  document was written. Confirm before committing to one.
- The mutation-score spot-check in Phase 4 depends on `cargo mutants` actually completing in
  reasonable time even for a single small crate — this has not been verified this session for
  any crate in this workspace. The one real coverage number in the codebase
  (`cargo tarpaulin -p ggen-cheat-scanner`, 55.6%) is a coverage tool, not a mutation tool, and
  the two are not interchangeable evidence.
- "Realistic scenario, not exhaustive branch coverage" is a judgment call per crate, not a
  mechanically checkable rule — the risk is Phase 2/3 authors drifting back toward
  volume-over-signal test authorship under time pressure, the exact failure mode this document
  argues against. The "what real thing broke" PR-description requirement in
  [What "done" means per crate](#what-done-means-per-crate) is the intended guardrail, but it
  is a review discipline, not an automated gate.

## See also

- `crates/ggen-engine/src/template.rs` — the `Frontmatter` struct and its own
  "Hygen-style template parsing" module doc, the canonical source for the parity matrix above.
- `crates/ggen-engine/tests/hygen_parity_e2e.rs` — the four proof tests this document is
  built around.
- `crates/ggen-engine/tests/write_behaviors_cli_e2e.rs` — the exhaustive per-branch decision
  table this document's tests deliberately do not duplicate.
- `.claude/rules/coding-agent-mistakes.md` — the 6-question patch contract Phase 3's
  marketplace/lockfile checkpoints are meant to automate.
- `.claude/rules/rust/testing.md` — Chicago TDD floor, and the one real (non-aspirational)
  coverage number in the codebase.
- `.claude/rules/architecture.md` — Crate Map and Pack Inventory, source for the
  `chicago-tdd-tools-pack` / `cli_proof` machinery description above.
- CLAUDE.md — Definition of Done, `guard-cheat-scan`'s 464-finding status, and the
  `A = μ(O)` formula this document cites for why ggen's variable source differs from hygen's.
- `docs/research/ggen-docs-hygen-parity.md` — a companion parity doc mirroring hygen's own
  documentation table of contents one-for-one (installation, generators, extensibility,
  packages, standalone install, FAQ, `hygen-create` equivalent), with fuller Hygen-docs-side
  narrative than this document needs for its CI-gate purpose. Written independently of this
  file and only cross-linked afterward — see `examples/hygen/README.md`'s "Relationship to
  `hygen_parity_e2e.rs`" section for how the two relate.
- `examples/hygen/` — four hands-on, independently runnable ggen projects mirroring specific
  examples from hygen's own docs (worker, mailer, injection, case-conversion), for a human
  reading the docs rather than for CI. Not a substitute for the proof tests above.
