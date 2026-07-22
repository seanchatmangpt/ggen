# ggen Antipatterns

Every entry in this document is a real defect this project actually shipped, found, and
fixed — not a hypothetical. Each is cited to a real file, PR, or commit where it happened.
This document exists so the same mistake gets recognized faster the next time it appears
in a different pack, not re-discovered from scratch.

Organized by the layer of the pipeline where the defect lives: ontology-authoring,
template-authoring, generation-time (sync/receipt/write), test-authoring, and
governance/campaign-level. Each entry states the pattern, why it's tempting, a real
example, the real fix, and how to catch it going forward.

## How to read this document

Every entry follows the same shape:

- **Pattern** — what the defect looks like in the abstract.
- **Why it's tempting** — the shortcut it's usually a symptom of.
- **Real example** — a specific, cited instance that actually shipped in this repo.
- **The fix** — what was actually done, not a general prescription.
- **Detection** — how to catch this class of defect before it ships again.

---

## Part 1: Ontology/Template Authoring

### 1.1 Hand-transcribed literal masquerading as a derived fact

**Pattern**: A value appears in a template's rendered output that looks like it comes from
the ontology, but is actually hand-typed directly into the template body. It is
indistinguishable from a real projection by reading the generated output alone — the only
way to catch it is to check whether the template's `sparql:` frontmatter actually queries
for that value, or whether it's a dead string sitting in the template text.

**Why it's tempting**: Writing `SELECT ?standing WHERE { ... }` and threading a new
variable through a Tera loop is more work than typing the current-correct value directly.
The literal renders correctly *today*. The bug is invisible until the underlying fact
changes and the template silently keeps printing the stale value.

**Real example**: `packs/crown-conjecture-pack/templates/crown_conjecture_skeleton.lean.tmpl`
(2026-07-22) hand-typed the literal word `"CONJECTURAL"` as prose text in two places, even
though the exact same fact already existed as a real, live ontology property
(`cc:standing`) on the individual the template was rendering. This is the same session that
built the gate described in §5.1 specifically to catch this — the bug was made by the same
author, in a brand-new pack, immediately after building the gate.

Earlier, structurally identical instances: `praxis-core-pack`'s and `star-toml-pack`'s cap05
(Generated verification) closures existed *specifically* because a value was hand-transcribed
instead of SPARQL-derived — see their `currentCeiling` history in
`.specify/pack-l5-promotion.ttl` for the real before/after.

**The fix**: Add the missing variable to the `sparql:` query (`SELECT ... ?standing ...`,
binding it via `BIND(STRAFTER(STR(?standingIri), "...") AS ?standing)` or a direct
property path), replace the literal with `{{ o.standing }}`, resync, and diff the output to
confirm it's byte-identical to the previous (correct-by-coincidence) hardcoded version.

**Detection**: `retrofit/l5-trust-hardening`'s gate 1
(`.specify/gates/l5-cap05-template-derivation-claim.rq`) catches the narrower case (a
`tests/`-targeted proof template lacking `sparql:` frontmatter entirely) for the L5
capability model specifically. The general case — *any* template hardcoding a fact that
duplicates a real ontology property — needs a human or agent to read the template and its
pack's `ontology.ttl` side by side and ask, for every literal-looking string: "is there a
property on some individual that already says this?"

### 1.2 Missing `sparql:` frontmatter on a template that should have one

**Pattern**: A template renders specific, fact-like content (names, counts, paths) with no
`sparql:` frontmatter block at all — meaning *nothing* in it is ontology-derived, it's 100%
hand-authored text dressed up as generated output.

**Why it's tempting**: Bootstrapping a new pack's first template by hand-writing the
"obviously correct" version first, intending to wire real SPARQL derivation later, and
never getting back to it.

**Real example**: this is exactly the failure mode `retrofit/l5-trust-hardening`'s gate 1
was built to catch. Building it against the real corpus surfaced **5 real, previously
undetected cases** in a single pass: `tcps-core-pack`, `wasm4pm-compat-pack`, `mcpp-pack`,
`self-monitoring-pack`, and `ggen-verify-pack` all implicitly claimed cap05 (Generated
verification) closed with zero qualifying `sparql:`-derived proof template.

**The fix**: For each of the 5, `missingCap` was corrected in
`.specify/pack-l5-promotion.ttl` to honestly re-open cap05 rather than leaving a false
closure claim standing — closing the capability for real is separate, later work per pack.

**Detection**: `.specify/gates/l5-cap05-template-derivation-claim.rq`, wired into root
`ggen.toml`'s `[validation].gates` — refuses `ggen sync run` if a pack's `missingCap`
omits `cap05` without a qualifying `l5p:TemplateDerivationRecord`.

### 1.3 Frontmatter-schema collision (writing the wrong ggen.toml dialect's headers)

**Pattern**: `ggen.toml` has two independently-defined, mutually incompatible schemas
(the "declarative-rules" schema and the "frontmatter" schema — see
`.claude/rules/architecture.md`'s "ggen.toml has two schemas" section). A template written
for one schema's frontmatter conventions, used under the other schema, has its frontmatter
block leak *verbatim* into the generated output instead of being stripped and consumed.

**Real example**: the `mfw-pcp-level5-pack` package (verified 2026-07-21) shipped with
every one of its 14 templates carrying an un-stripped `---\nto:...\nsparql:...\n---`
frontmatter header belonging to the *other* schema than the one its own `ggen.toml`
declared. This leaked verbatim into every generated file, breaking the generated
`Cargo.toml` as invalid TOML (`error: key with no value, expected '='` at line 1, column 4)
— invisible until a real `cargo build` was attempted; `ggen sync run` itself succeeded and
reported the files as written.

**The fix**: stripped the dead, redundant header from all 14 templates — a minimal,
semantically-inert fix since the header's `to`/`force`/`sparql` keys duplicated the
consuming `ggen.toml`'s own `[[generation.rules]]` fields.

**Detection**: `ggen sync run`'s own exit code is not sufficient evidence a pack works —
this class of bug is only caught by actually building the generated output
(`cargo build`/`cargo test` in the consumer), never by sync success alone. See §3.4.

---

## Part 2: Generation-Time (sync, receipts, locks)

### 2.1 Stale `ggen.lock` / stale generated-target refusals treated as errors instead of the expected re-lock cycle

**Pattern**: `ggen sync run` refuses with `[FM-PACK-008]` (pack content hash mismatch
against `ggen.lock`) or `[FM-WRITE-005]` (existing generated file differs, refusing silent
clobber) after any real template or ontology edit. This is *correct, intended behavior* —
the pipeline is refusing to silently drift — but it is easy to mistake for a bug and either
work around it improperly or get stuck on it.

**Why it's tempting to mishandle**: the refusal messages are terse enough that a first
encounter can read as "something is broken," prompting either a `force: true` workaround
(masking real drift-detection rather than acknowledging an intentional change) or a
multi-step debugging session for what is actually a one-line fix.

**Real example**: hit twice in one session building `packs/crown-conjecture-pack`
(2026-07-22) — first `[FM-PACK-008]` after editing the pack's ontology, then
`[FM-WRITE-005]` after editing the template and re-running. Both are the *expected*
consequence of legitimately changing pack/template content between syncs.

**The fix** (the correct, minimal response every time): `rm -f ggen.lock` (intentional
re-lock) and/or `rm -f <the stale generated file>` (intentional resync target
replacement), then `ggen sync run` again. Never reach for `force: true` to mask this
unless the write genuinely needs to bypass `freeze_policy` for a real, disclosed reason —
`force: true` and "stale lock/target" are different problems with different correct fixes,
and O-7a (this session's own audit) tracks 140+ pre-existing `force: true` occurrences
specifically because this distinction gets blurred in practice.

**One real, narrower exception worth naming explicitly**: `ver:kd-byte-identity-bootstrap-deadlock`
(`packs/ggen-verify-pack/ontology.ttl`) documents a case where this refusal is *structurally
unresolvable* by the standard re-lock-and-resync fix — `scripts/checks/byte-identity.sh`'s
own check command internally runs a second `ggen sync run`, which is gated by the same
evidence-freshness check whose own evidence is what's stale, a genuine self-referential
deadlock. That case is recorded as a disclosed `ver:KnownDivergence`, not silently patched
around and not left as an unexplained red check.

**Detection**: none needed beyond recognizing the two error codes on sight — `[FM-PACK-008]`
means re-lock, `[FM-WRITE-005]` means the generated target needs to be deleted before the
intentional resync. Neither is evidence of a real product defect by itself.

### 2.2 Receipt-file merge conflicts resolved by hand-splicing instead of regenerating

**Pattern**: `.ggen-v2/receipt.json`/`receipt-log.jsonl` conflict on nearly every merge of
a long-lived branch against an advancing `origin/main`, because both sides ran independent
real syncs that each appended different real receipt entries. The wrong fix is manually
editing the JSON to reconcile both sides' entries by hand.

**Why it's tempting**: a quick glance suggests "just keep both sets of entries" is a safe
merge — but a hand-spliced receipt chain has broken hash linkage (`prev_chain_hash_hex`
must equal the predecessor's real `chain_hash_hex`), which is exactly the tamper pattern
the chain exists to detect. A hand-edited receipt is indistinguishable, to the chain
verifier, from a forged one.

**Real example**: recurred at least four times this session across different branches
(`retrofit/l5-parallel-wave2`'s reconciliation, `retrofit/l5-parallel-wave3`'s rebase onto
a `chore(release)` version bump, others). Every time, resolved identically.

**The fix**: `git checkout --theirs` on both receipt files (take `origin/main`'s real,
currently-valid chain), then run a real `ggen sync run` to produce a *new* receipt entry
that actually describes the post-merge state — never hand-splice, never trust either
parent's stale receipt as correct for a state neither parent ever actually produced.

**Detection**: `ggen receipt verify` after any merge involving these two files — a hand-spliced
chain fails real hash-chain verification immediately.

### 2.3 Committing a platform-specific binary where a real build target belongs

**Pattern**: A compiled, native binary is checked directly into version control in place of
a portable build step, working on the machine that committed it and silently broken
everywhere else.

**Real example**: `scripts/generate_registry_hashes` was a 514-kilobyte **macOS arm64
Mach-O executable**, committed directly, consumed by
`tests/e2e_marketplace.rs::test_hash_generation` via `Command::new("./scripts/generate_registry_hashes")`.
It failed on Linux CI with `Error: Exec format error (os error 8)` (`ENOEXEC`) the moment
the consuming CI job was promoted from advisory to required (2026-07-22, generation-4) —
never caught before because the job's prior advisory status meant this test's result never
blocked anything. The real logic already existed, unused, as
`scripts/generate_registry_hashes.rs`, never wired as a Cargo binary target.

**The fix**: added an explicit `[[bin]]` target (`name = "generate_registry_hashes"`,
`path = "scripts/generate_registry_hashes.rs"`), repointed the test at
`env!("CARGO_BIN_EXE_generate_registry_hashes")`, removed the stale platform-specific
binary, added the source to `[package].include` so `cargo publish` keeps working.

**Detection**: any committed file with a binary magic number (Mach-O `cffaedfe`, ELF
`7f454c46`, PE `4d5a`) in a `scripts/` or similar source-adjacent directory is a red flag on
sight — `file <path>` confirms in one command.

---

## Part 3: Test Authoring

### 3.1 Literal full-string matches against generated/config content instead of membership checks

**Pattern**: A test asserts `content.contains("<exact literal line>")` against a file that
legitimately changes shape over time (a CI workflow's `needs: [...]` array, a config
block's exact key ordering). Any legitimate addition to that line breaks the test, even
though the invariant the test actually cares about (job X is still required) still holds.

**Real example**: `tests/phase2_recipes_test.rs::test_ci_status_requires_phase2` asserted
`ci_workflow.contains("needs: [check, build, test, doctest, phase2, cargo-cicd]")`
verbatim. When generation-4 correctly promoted `integration-tests` to required — adding it
to the real `needs:` list — this test broke, because it was now testing for the *absence*
of the exact dependency that had just been correctly added.

**The fix**: parse out the real `needs:` line and check each required job name's
*membership* individually (`needs_line.contains(required_job)` per name), not the whole
line as one literal. Same invariant, no longer fragile to every future legitimate addition.

**Detection**: any test doing `.contains("<long literal string with several distinct
facts concatenated>")` against a file that isn't itself frozen/versioned content is worth a
second look — ask whether the test cares about the *whole exact string* or a *subset of
real properties* within it.

### 3.2 Test-authoring assumptions stale after a tooling migration

**Pattern**: Tests assert against a tool or file that was the project's convention *at the
time the test was written*, and are never updated when the project migrates to a new
convention — because the old tool/file often still exists on disk (kept for historical
reference), so the test doesn't fail until something else forces it to actually run.

**Real example**: three tests in `tests/infrastructure_validation.rs`
(`test_timeout_check_task_exists`, `test_ci_gate_task_exists`,
`test_pre_commit_hook_task_exists`) shelled out to `cargo make --list-all-steps` and
checked `Makefile.toml` task names — but this repo's own binding rule
(`.claude/rules/_core/absolute.md` rule 4) forbids `cargo make` as an entry point (`just`
is sole sanctioned entry point; `Makefile.toml` is explicitly "historical reference only"
per `CLAUDE.md`), and `cargo-make` is not installed in CI. The shell-out silently produced
empty output and all three assertions failed — invisible until the same advisory-to-required
promotion (§2.3) made this job's result matter for the first time.

**The fix**: repointed at the real, current entry point (`justfile`), checking for each
task's real justfile-era equivalent name (`timeout-check` kept its name; `ci-gate` and
`pre-commit-hook` both map to the real `pre-commit` recipe, this repo's documented current
CI-gate equivalent per `andon/signals.md`'s Definition of Done).

**Detection**: any test invoking a tool named in a project's own "never use this, use X
instead" rule is definitionally testing the wrong thing — grep tests for forbidden-tool
names as a standing practice after any tooling migration.

### 3.3 Tautological sabotage fixtures (the flip test that can't actually fail)

**Pattern**: A "sabotage test" — flip a fact, resync, expect a failure, revert, resync,
confirm clean — is supposed to prove a real dependency exists between the flipped fact and
the observed behavior. If the fixture's *precondition* is itself derived from the same
value being flipped, the test can pass even when the real logic being tested is broken,
because the flip changes two things at once instead of isolating one.

**Real example**: `tcps-std-pack`'s wave-3 cap06 closure attempt (2026-07-22) first
constructed a sabotage fixture by flipping `expectedOutcome` alone — and it still passed
(2/2 tests green) *because the fixture's own template branched the real-world filesystem
precondition on `expectedOutcome` itself*, so flipping the expectation also silently
flipped what the test actually set up on disk, cancelling the intended failure out. Caught
live during the sabotage attempt itself, not asserted in advance.

**The fix**: added a separate `tcps_std:rootScenario` predicate so the real filesystem
precondition no longer derives from the declared expectation — re-ran the flip
(`expectedOutcome` alone, `rootScenario` held fixed) and got a real, non-vacuous failure
(`Os { code: 20, kind: NotADirectory, ... }`) this time.

**Detection**: for any sabotage/drift-sensitivity test, ask explicitly: "does flipping only
the fact under test change exactly one thing, or does it also change the test's own setup
because both derive from the same source?" — the fixture generator and the assertion it
feeds must be independently sourced.

### 3.4 Silent-pass query bugs indistinguishable from a real zero-result answer

**Pattern**: A query executor encounters a construct it doesn't support and returns an
empty result set instead of refusing — which looks identical, from the caller's side, to a
real, correct "no matches" answer.

**Real example**: `praxis-graphlaw`'s custom SPARQL executor silently returned zero rows
for *any* `GRAPH <iri> { ... }` WHERE-clause pattern — `extract_query_plan` had no
`GraphPattern::Graph` arm at all. Found during generation-4's real root-causing of the
`CiIntegrationTestsNeverRun` SPOF (2026-07-22): every test asserting behavior over a named
graph was silently passing against zero real matches, not the behavior it claimed to test.

**The fix**: added a loud, typed refusal (`find_unsupported_construct`), joining the
existing `GraphLawValuesSilentZeroRows` fail-closed net, with a pinned regression test
(`tests/sparql_unsupported_refusal.rs`) — not just a fix, a permanent negative-witness test
so the specific silent-pass shape can't reappear unnoticed.

**Detection**: any query/parser/executor with an unbounded input grammar needs an explicit
"unsupported construct" refusal path, not a default empty-result fallthrough — audit for
match/switch statements over a grammar's node types that have a catch-all arm returning
`Ok(empty)` instead of `Err(unsupported)`.

### 3.5 A closed-form assertion masking silently-accepted invalid input

**Pattern**: An input-validation/rewriting layer accepts a broader class of input than it
should, and nothing downstream ever notices because everything downstream assumes upstream
already validated.

**Real example**: `hooks/parsing.rs::rewrite_hook_alias` silently accepted unrecognized
`hook:` alias predicates, evading all downstream `kh:`-scoped validation entirely — a typo
or unsupported alias would pass through undetected rather than being refused at the point
of first contact.

**The fix**: refused unrecognized predicates at the rewrite step itself (fail at the
boundary, not several layers downstream where the failure would be harder to trace back to
its real cause), with a pinned regression test.

**Detection**: for any rewriting/normalization layer, check explicitly what happens on
input it doesn't recognize — silent pass-through disguised as "permissive" is a bug, not a
feature, unless genuinely and deliberately documented as such.

---

## Part 4: Governance and Campaign-Level

### 4.1 Fabricated documentation claims (a described artifact that doesn't exist)

**Pattern**: Prose documentation describes a test, file, or capability as existing, and it
does not — often because the doc was written aspirationally, or copied from a plan that
was never fully executed, and nobody has since diffed the doc's claims against the real
filesystem.

**Real example**: `ma-case-study-pack`'s prior documentation described a specific test file
as existing; a real file-existence check during wave-3's independent audit (2026-07-22)
found it did not. This is qualitatively different from an *incomplete* capability
(honestly marked open) — this was a claim of *completion* for something that was never
built.

**The fix**: wrote the real file for real — nine of nine tests passing — closing the gap
the documentation had already (falsely) claimed was closed, rather than merely correcting
the prose to say "not yet done."

**Detection**: this is exactly why `retrofit/l5-parallel-wave3`'s audit stage exists as a
structurally separate phase from the cells that do the work — an audit that re-reads
prose cannot distinguish a fabricated claim from a true one; an audit that re-runs the cited
command or checks the cited file's real existence can and did.

### 4.2 Standing/status asserted without paired, checkable evidence

**Pattern**: A maturity/standing field (`l5p:standing`, or any status vocabulary) is set to
a strong value in an ontology or ledger without a structurally-required, paired evidence
citation — relying entirely on the convention that whoever writes the ontology remembers to
have actually checked first.

**Real example**: `.specify/pack-l5-promotion.ttl`'s own header comment already stated the
intended discipline in prose — "do not mark a pack `l5p:Level5` by editing this file...
requires a named, checkable artifact for each capability" — but nothing enforced it. Any
agent (including a past pass in this very session) could have asserted `l5p:standing
l5p:Level5` with zero paired evidence and nothing would have refused it.

**The fix**: `retrofit/l5-trust-hardening`'s gates 2 and 3 — a real SPARQL gate refusing
`l5p:standing l5p:Level5` unless `missingCap` is empty (necessary condition) *and* a paired
`l5p:SabotageEvidence` individual exists (citing a real, already-executed revert-and-reconfirm
cycle). Converts a documented convention into something the write path structurally
refuses to violate.

**Detection**: for any status/maturity vocabulary in this project, ask: "is there a gate
that refuses the strongest value without a paired evidence citation, or does this rely
entirely on the author remembering the rule?" If the latter, it will eventually be
violated — not through malice, through the same kind of oversight this entry documents
happening to this exact ontology.

### 4.3 Doc self-contradiction across sibling artifacts describing the same real-world fact

**Pattern**: Two independently-maintained documents each assert a fact about the same
underlying real system, and drift apart over time as one gets updated and the other
doesn't — with nothing that would ever notice, because no single query spans both.

**Real example**: `ggen-constitution-pack`'s admitted-semantics mirror cited `ccn:LawVII`
with a specific enforcement claim about `sync.rs`'s Andon-level handling — found, during a
reconciliation pass, to be **stale and self-contradicted** by `ggen-release-pack`'s own,
more current condition-12 text describing the same real code path, almost verbatim in the
opposite direction. Caught only by directly reading both and comparing, not by any query.

**The fix**: named the contradiction explicitly in the reconciliation record rather than
silently picking one side — flagged as a real defect for a future pass to correct at the
source, not fixed by fiat during an unrelated reconciliation.

**Detection**: no automated mechanism currently exists for this class of defect in this
repo — it is real, known, and explicitly named as an open gap (see the "what the graph
cannot yet check" analysis that motivated `retrofit/l5-trust-hardening`). A periodic
cross-reference query — any two packs asserting facts about the same real code path,
diffed automatically — is the proposed but not-yet-built fix.

### 4.4 Vacuous instantiation: a structure that type-checks and satisfies every obligation by construction, proving nothing about the real system

**Pattern**: An abstract interface/witness/trait requires several components, and the only
instance ever constructed sets every component to a constant trivial value
(`fun _ => True`, a no-op, a hardcoded pass) — legally satisfying the type system while
carrying zero real connection to the domain it's meant to model. Anything built on top of
this instance is, whether or not its author realizes it, reasoning about the vacuous case,
not the real one.

**Real example**: `mfw-theory/MFW/Concurrency.lean`'s `trivialTemporalIndependenceWitness`
— the *only* `TemporalIndependenceWitness` instance ever constructed anywhere in that
codebase — sets all five real components (`effectsCommute`, `preconditionStable`,
`invariantStable`, `numericFlowCompatible`, `trajectoryPreserved`) to `fun _ _ _ => True`.
The source's own comment states this outright: "carries no proof obligations between its
fields, so any concrete assignment instantiates it. mfact never constructed one." Any proof
of the Crown Conjecture (`TraceSwapPreservesLawful`) attempted against this witness would
type-check while proving nothing about real PDDL 3.1 semantics.

**The fix (in progress, tracked, not yet closed)**: recorded as a first-class
`cc:BlockingFinding` individual in `packs/crown-conjecture-pack/ontology.ttl`, cited by the
generated proof-obligation ledger — real mathematical work (constructing a genuinely
non-trivial witness) is a named prerequisite, not silently assumed solvable.

**Detection**: grep any formal/typed codebase for `fun _ => True` / `fun _ _ => True` /
`trivial` / a no-op used as a *complete instance body* for an interface meant to carry real
domain content — then read the surrounding context to judge whether it's genuinely vacuous
or legitimately trivial-by-design (a real base case is allowed to be trivial; a witness for
a nontrivial domain relation generally should not be).

---

## Part 5: The Meta-Pattern

Reading all four parts together, one shape recurs regardless of layer: **a claim (of
derivation, of correctness, of completion, of standing, of real content) survives exactly
as long as nothing checks it against the reality it claims to describe.** Every entry above
was caught by one of a small number of real mechanisms:

- Running the generated output through its actual consumer (compiler, test suite) instead
  of trusting `ggen sync run`'s exit code alone (§1.3, §2.3).
- An audit stage that re-runs cited commands and re-checks cited files, structurally
  separated from the stage that made the claim (§4.1).
- A gate that refuses a write instead of a convention that asks an author to remember
  (§4.2).
- Grepping for a known-dangerous literal pattern (a forbidden tool name, a vacuous
  constant-value instance, a committed binary) as a standing practice, not a one-time
  sweep (§2.3, §3.2, §4.4).
- Actually flipping the fact a test claims to depend on and confirming the failure is real,
  not assumed (§3.3).

None of these mechanisms are novel individually. What this document argues is narrower:
**the failure mode is the same failure mode every time, at every layer, and the fix is
always some version of "make the claim re-checkable, and actually re-check it"** — not a
smarter author, not more careful prose, a structural check that doesn't depend on anyone
remembering to look.

## See Also

- `.claude/rules/coding-agent-mistakes.md` — the five mistake classes (Decorative
  Completion, Epistemic Bypass, Fail-Open Behavior, Legacy Path Contamination, Contract
  Drift) this document's real examples repeatedly instantiate
- `.claude/rules/andon/signals.md` — the stop-the-line discipline that makes catching these
  in CI actually matter (a signal nobody stops for is not a signal)
- `docs/l5-promotion/L5_PROMOTION_PROGRAM.md` — the generated capability model several of
  Part 1 and Part 4's real examples were found against
- `packs/ggen-verify-pack/ontology.ttl` — the `ver:KnownDivergence` mechanism for disclosed,
  accepted, non-silent exceptions to any of the above
