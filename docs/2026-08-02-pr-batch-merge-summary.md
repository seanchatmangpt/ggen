# PR Batch Merge Summary — 2026-08-02

Eight PRs (#554–#561) merged to `main`, plus two recreated PRs (#562, #563) after a
merge-tool mishap deleted two branches mid-conflict (see "Merge mechanics" below).
This doc summarizes what each PR actually claims to have done, using each PR's own
self-reported standing/claim-ceiling language — not an independent re-verification.

## Merge mechanics note

`gh pr merge --squash --delete-branch` deleted the branches for #558 and #561 before
their conflicting merges completed, which auto-closed those PRs without merging. Both
commits were still reachable by SHA, were re-pushed as new branches, and landed via
new PRs **#562** (superseding #558) and **#563** (superseding #561) after manual
conflict resolution against `main`. All content from the original 8 PRs is on `main`;
#558 and #561 show as closed-not-merged on GitHub but are linked to their replacements.

---

## #554 — `refactor(corpus): remove migrated v26.8.1 enterprise corpus`

Removes the v26.8.1 enterprise corpus (`docs/`, `ontology/`, `planning/`, `tools/`
under `v26.8.1`, a legacy-equivalence pack, three CI workflows) from `ggen` after
transferring it to a separate `seanchatmangpt/ggen-legacy` repo. Relocates two
generic guard scripts to `scripts/ci/`.

**Reported as complete:** destination migration, source-path removal, receipt +
replay (`REPLAY_MATCH`), architecture-foundry runtime, self-host observer, book-gap
audit.

**Reported as incomplete (verbatim from the PR):**
- `general CI / Quality = IN_PROGRESS`
- `generated-law exact-head proof = IN_PROGRESS`
- A pre-existing Self-Host Retrofit inventory rail has "a 4,096-row cap against an
  18,242-file repository" — a known scalability defect, explicitly **not** caused by
  this PR and not used to claim standing.

---

## #555 — `feat(tv): manufacture cyberpunk platform and locally crown NASA Dark Mode`

Builds a "cyberpunk-tv-platform" pack including a WASM control core, browser/WebGL2
rendering, and a Roku (BrightScript) source simulator, verified via a local
(no-CI) run script with receipt replay.

**Reported as complete:** WASM core, browser DOM + WebGL2, Roku source simulation and
packaging, receipt replay, 3 mutation-kill controls.

**Reported as incomplete/exploratory (verbatim):**
- `deckGlRuntime = BLOCKED_DEPENDENCY_TRANSPORT` — the exact pinned deck.gl 9.1.14
  package could not be fetched in this environment; the browser source still contains
  real deck.gl constructors but runs on WebGL2 as a substitute renderer, not deck.gl
  itself.
- `rustImplementation = BLOCKED_TOOLCHAIN_REQUIRED`
- `rokuPhysicalDevice = BLOCKED_DEVICE_REQUIRED` — Roku verification is explicitly
  "source-derived verification, not physical-device execution."
- Aggregate standing self-reported as `PARTIAL_ALIVE`, not `ALIVE`, specifically
  because these edges need capabilities outside the environment.

---

## #556 — `ci: validate exact dteam capability kernel head`

Adds CI to build/test/run a pinned external repo (`seanchatmangpt/dteam` at a fixed
commit) as an independent capability kernel. No generated projection of dteam is
added — validation of the exact pinned source only.

**Reported as complete:** compile, unit tests, e2e demo, evidence upload — as a CI
definition. No exploratory caveats stated in the PR body; this is the smallest/most
mechanical PR in the batch.

---

## #557 — `feat(sbb): add commit-bound capability density`

Adds a "Solution Building Block" (SBB) capability-density system: RDF ontology,
SHACL constraints, SPARQL projection, JSON manifest, and `ggen sbb
schema|inspect|validate|distribution|receipt|replay` commands. Binds each density
unit to one Git commit with an evidence chain (positive witness, negative fixture,
adversarial falsifier, verifier).

**Reported as incomplete (verbatim):** "the implementation reports a maximum claim
ceiling of `PARTIAL_ALIVE`. Exact-head repository CI and an independent verifier are
required before promotion to `ALIVE`." I.e., the author states outright this cannot
self-promote past PARTIAL_ALIVE without external CI + a separate verifying party —
neither of which this PR provides.

---

## #558 → landed as #562 — `feat(sbb): add commit-bound capability density` (Vision 2030 / Maximalism layer)

Note: title collides with #557 in the PR body text quoted by the agent, but this PR's
actual content is the Vision 2030 admission-control and "Combinatorial Maximalism"
layer stacked on #557 — adds `doctor`, `wizard`, `telco`, `maximalism` command
surfaces and a 19-domain / 5-horizon / 9-outcome closure model.

**Reported as complete:** ontology, SHACL, 19-domain coverage, receipt/replay
cryptography validation, cycle refusal.

**Reported as explicitly not-yet-real (verbatim):** "Generated text, source volume,
commit volume, catalog entries, and declared multipliers do not establish standing."
The 49-capability catalog is stated as remaining `DESIGNED` — i.e., **cataloged but
not evidenced** — "no catalog entry counts without exact SBB evidence and independent
acceptance." Aggregate Vision 2030 is explicitly gated as `ALIVE` only if every
capability, all 19 domains, all 5 horizons, all 9 outcomes, and a ≥1000x measured
multiplier all close — none of which is claimed as achieved by this PR itself.

---

## #559 — `docs(v26.8.3): admit ALIVE PRD/ARD authority`

Admits a Product Requirements Document + Architecture Requirements Document bundle as
an "executable authority" (20 requirements, 11 components, traceability, embedded
JSON Schema), independently checked by a second repo (`ggen-legacy`) acting as
verifier.

**Reported as complete:** the PRD/ARD bundle's own internal consistency — 0 findings,
10/10 mutation falsifiers killed, `REPLAY_MATCH`, standing `ALIVE` — but **only**
for the bundle itself.

**Explicitly scoped out (verbatim list) — this is the PR being unusually explicit
about what it is *not*:**
- aggregate ggen runtime or repository standing
- implementation of every target requirement
- Vision 2030 or 1000x capability standing
- release, deployment, security, or certification standing
- ggen-legacy A–K crown
- ecosystem-wide Release/Sunset Admission

This is a requirements/architecture **document**, not a claim that the requirements
are implemented.

---

## #560 — `feat(pcq): manufacture real-time deck.gl marketplace ecosystem`

Adds a reusable ggen pack generating a Next.js 16 + deck.gl real-time marketplace app
settled against a fixed-point ledger ("PCQ", explicitly not a currency/security/FX
claim), with SSE streaming, settlement receipts, and Playwright browser verification.

**Reported as complete:** ontology-driven projection (23/23 template equivalence),
SPARQL refusal gates (0 positive violations against a clean fixture, negative
falsifier confirmed working), ledger balance/overspend-refusal tests, legacy static
verifier.

**Reported as incomplete (verbatim):**
- `exact-head dedicated manufacture/build/SSE/WebGL run: UNKNOWN — push run is not
  exposed by the available PR-only workflow listing`
- "The pull request remains draft and is not merged" (per the PR's own body — this
  refers to its state at authoring time, prior to this session's merge).

---

## #561 → landed as #563 — `feat(tv): 80/20 innovation gaps` (stacked on #555)

Closes a self-identified "product legibility" gap in #555's platform: wires
`orient`, `search`, `explain`, `frontier`, `evidence`, `release`, `demo` into a
generated control plane, adds a Pareto-frontier transport selector and a
byte-grounded evidence index.

**Reported as complete:** 52 innovation assertions / 0 failures, 2 mutation controls
killed, 15 legacy-compatibility assertions, strict wizard input validation.

**Reported as incomplete (verbatim "Remaining 20%" list):**
1. physical Roku execution receipts (still simulation-only, same gap as #555)
2. federated multi-node transport execution
3. a generalized evidence ontology shared across every ggen pack (this PR's evidence
   index is local to this pack only)
4. a native interactive wizard UI — only the deterministic scriptable command surface
   exists

Aggregate repository standing self-reported as `PARTIAL_ALIVE`; "This PR does not
manufacture physical-device or federated evidence."

---

## Cross-PR pattern: what "incomplete" means across this whole batch

Every PR in this batch that touches the Vision 2030 / SBB / Maximalism surface
(#557, #558/#562, #559, #561/#563) uses the same self-imposed vocabulary
(`ALIVE` / `PARTIAL_ALIVE` / `BLOCKED_*` / `UNKNOWN` / `DESIGNED`) and consistently
stops short of claiming `ALIVE` for anything requiring:

1. **External/independent verification** — #557 and #559 both state their own
   internal checks are insufficient; #559's independent check is scoped to the
   PRD/ARD document only, not implementation.
2. **Physical or external-network resources this environment doesn't have** — deck.gl
   package fetch (#555), Roku physical device (#555, #561/#563), federated transport
   (#561/#563).
3. **A declared multiplier or catalog entry being backed by real evidence rather than
   volume** — #558/#562 explicitly disclaims that generated text/commit volume proves
   anything; the 49-capability catalog stays `DESIGNED`, not `ALIVE`.

The two PRs without this pattern are #554 (a mechanical corpus-removal refactor,
which does flag two concrete `IN_PROGRESS` CI items) and #556 (a small, single-purpose
CI validator with no exploratory scope).

## What actually changed on `main` as a result of this session

- 8 feature/doc/refactor PRs merged (2 of them via re-created PRs #562/#563 after a
  branch-deletion incident during merge).
- No code review of file-level diffs was performed by me beyond what was needed to
  resolve merge conflicts (formatting-only diffs in `crates/ggen-cli/src/cmds/sbb/`,
  an additive `telco` module registration, and taking the newer branch's version of
  7 add/add-conflicted `packs/cyberpunk-tv-platform/` files that were confirmed
  byte-superset of `main`'s copy).
- This summary is built from each PR's **self-reported** standing sections, not from
  independently re-running their verification suites — the same practice this repo's
  own `no-overclaiming` rules would flag if presented as verified fact rather than
  reported claim.
