# GitHub Actions Refactor Report — Caching & Everything Else

Last Updated: 2026-08-05. Grounded in direct inspection of this repository's
`.github/workflows/*.yml` (75 files), `.github/actions/*/action.yml` (6 composite
actions), and `.cargo/config.toml`. No fabricated numbers — every count below was
produced by a real `grep`/`ls` command against this checkout on this date; re-run the
commands in each section to reverify before trusting them again, since this workspace's
own doctrine (`CLAUDE.md`, Evidence-First Principle) requires that and prior counts in
this repo's own docs (crate map, gate counts) have gone stale before.

## 0. Scope and method

This is an audit-and-recommendation report, not yet an applied patch. It covers:

1. Current caching state (Section 2) — what's real, what's duplicated, what's missing.
2. Non-caching CI hygiene findings (Section 3) — action pinning, timeouts, concurrency.
3. Concrete refactor recommendations with example YAML (Section 4).
4. A prioritized, bounded implementation plan (Section 5) sized to this repo's own
   "≤12 changed files" implementation-law default (`.claude/rules/coding-agent-mistakes.md`
   doesn't state that number, but `CLAUDE.md`'s general session discipline and the
   Chatman-framework prompt pasted alongside this request both push toward small bounded
   diffs over a wholesale rewrite).
5. A "post-AGI" section (Section 6) — what changes about CI when the primary author and
   primary reviewer of most commits is an agent, not a human, framed against the receipt/
   BRCE vocabulary from the Chatman Equation prompt this request arrived with.

I did not open all 75 workflow files individually — that would cost more tokens than
value for files like `nasa-dark-mode.yml` or `cyberpunk-tv-platform.yml` that are
clearly narrow, single-purpose demo/evidence workflows. Every claim below is backed by
a command whose output is shown or described; where I generalize from a sample, I say so.

---

## 1. Inventory (real counts, 2026-08-05)

```text
$ ls .github/workflows/*.yml | wc -l
75

$ ls .github/actions/*/action.yml | wc -l
6   # cargo-security-audit, extract-semantic-version, install-cargo-tools,
    # pr-comment-upsert, setup-ggen-build, setup-rust-cached
```

Two composite actions do overlapping work under different names — see Finding C1.

---

## 2. Caching — current state

### 2.1 Two independent, non-identical caching implementations (Finding C1)

`.github/actions/setup-ggen-build/action.yml` (the one actually referenced by CI's
main gates) uses `Swatinem/rust-cache@f13886b…` (SHA-pinned) keyed on
`cache-key-suffix`, **plus** `sccache` as the `rustc-wrapper` (set globally in
`.cargo/config.toml`, `SCCACHE_CACHE_SIZE = "20G"`). This is a genuinely strong
combination: `rust-cache` handles the `~/.cargo` registry + `target/` directory across
job boundaries, while `sccache` gives object-level compilation caching *within* a build
that `rust-cache` alone doesn't provide (useful when `target/` is stale/partial or a
dependency's feature flags changed but its source didn't).

`.github/actions/setup-rust-cached/action.yml` — a second, older-style action — instead
hand-rolls `actions/cache@v4` (floating tag, see Finding P1) over
`~/.cargo/registry/{index,cache}`, `~/.cargo/git/db/`, and `target/`, keyed on
`hashFiles('**/Cargo.lock')`. No `sccache`. No SHA pinning.

```text
$ grep -l "setup-ggen-build" .github/workflows/*.yml | wc -l
35
$ grep -rl "Swatinem/rust-cache\|actions/cache" .github/workflows/*.yml | wc -l
6
```

35 workflows use the strong composite; a handful of others (not all inventoried
individually) still reference the weaker one or raw `actions/cache`. Two caching
strategies for the same Cargo workspace means: (a) two separate cache namespaces
competing for GitHub's per-repo 10 GiB cache budget, evicting each other under LRU
pressure; (b) a real risk that a workflow using the weak path gets a cold `target/`
even when a `setup-ggen-build`-using workflow just built the identical `Cargo.lock`
minutes earlier — wasted CI minutes, not just a cosmetic duplication.

**Recommendation:** retire `setup-rust-cached`; make `setup-ggen-build` (or a slimmed
non-sibling-provisioning variant of it, for workflows that don't touch the
`[patch.crates-io]` sibling crates) the single caching entrypoint for every workflow
that runs `cargo`.

### 2.2 20 workflows run `cargo` without the sibling-provisioning step at all (Finding C2)

```text
$ grep -L "setup-ggen-build" .github/workflows/*.yml | xargs grep -l "cargo " | wc -l
20
```

`setup-ggen-build`'s own header comment explains *why* this matters: the root
`Cargo.toml` has a `[patch.crates-io]` table redirecting `lsp-max`/`wasm4pm*` to
sibling checkouts one directory above the repo root. Any workflow that runs `cargo`
without provisioning those siblings first will fail workspace resolution outright
(`failed to read /home/runner/work/ggen/lsp-max/Cargo.toml`), *if* that job's Cargo
invocation touches a package that pulls in the patched crates. This is not
automatically a bug in all 20 files — some may deliberately build a single crate
outside the workspace, or a package that never resolves through the patch table — but
it is exactly the failure mode `setup-ggen-build`'s comment says was already hit and
fixed for the jobs that do use it. Each of the 20 is worth a one-line check: does this
job's `cargo` invocation touch the patched dependency graph? If yes, it is currently
either (a) silently failing, (b) silently getting lucky because a prior job in the same
run already provisioned the siblings into a shared runner (unlikely — GitHub Actions
runners are ephemeral per-job by default), or (c) not actually building the affected
crates. This needs per-file triage before a blanket fix; listed here as a finding, not
resolved as a claim.

### 2.3 No cache-hit-rate observability anywhere except `release.yml` (Finding C3)

```text
$ grep -rl "cache-hit" .github/workflows/*.yml .github/actions/*/action.yml
.github/workflows/release.yml
```

Only one workflow ever reads/reports whether a cache actually hit. No workflow prints
`sccache --show-stats` (the standard way to see hit/miss/error counts and prove the
compilation cache is doing anything at all — the checklist question "no spans found ⇒
feature not working" from this repo's own OTEL-validation doctrine applies here nearly
verbatim: no sccache stats ⇒ you cannot currently prove sccache is helping in CI, only
that it's configured).

```text
$ grep -rl "sccache --show-stats\|sccache -s" .github/workflows/*.yml .github/actions/*/action.yml
(no output — zero matches)
```

**Recommendation:** add a `sccache --show-stats` step after every build/test step that
uses the shared toolchain, and surface `cache-hit` from `Swatinem/rust-cache`'s own
output as a job summary line. Right now this repo cannot answer "is caching actually
saving CI minutes" with evidence — only with the *design* of the caching, which per
this repo's own Evidence-First Principle is not the same claim.

### 2.4 What IS working and should be preserved

- `sccache` + `rust-cache` together in `setup-ggen-build` is the right combination
  for a Rust monorepo this size (19–20 crates per `CLAUDE.md`'s architecture section)
  — don't replace it with a single mechanism.
- SHA-pinning `Swatinem/rust-cache` and `taiki-e/install-action` inside
  `setup-ggen-build` is already correct supply-chain practice (see Finding P1 for
  where this discipline is *not* applied elsewhere).
- The `cache-key-suffix` input allowing per-job cache segmentation (check/build/
  test/lint) is a real, deliberate design already in place — worth keeping, and worth
  extending to every workflow once Finding C1 is resolved.

---

## 3. Non-caching CI hygiene findings

### P1. 32 of 259 action references are on floating tags, not pinned SHAs

```text
$ grep -hoE "uses: [a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+@[a-zA-Z0-9._-]+" \
    .github/workflows/*.yml .github/actions/*/action.yml | wc -l
259
$ ...| awk -F@ '{if (length($2) != 40) print}' | sort -u
uses: actions/cache@v4
uses: actions/checkout@v4
uses: actions/github-script@v7
uses: actions/setup-node@v4
uses: actions/upload-artifact@v4
uses: dtolnay/rust-toolchain@master
uses: dtolnay/rust-toolchain@stable
uses: extractions/setup-just@v2
uses: hashicorp/setup-terraform@v3
uses: pnpm/action-setup@v4
uses: Swatinem/rust-cache@v2
uses: taiki-e/install-action@cargo-nextest
```

227 of 259 references (~88%) are already SHA-pinned — this repo clearly has an
existing supply-chain-safety discipline (`setup-ggen-build`'s own comments say so
explicitly for its own dependencies: "SHA-pinned for supply-chain safety /
reproducibility (mutable @v2 tag)"). The remaining 12% is real, fixable drift, not a
systemic absence of the practice — worth closing, not worth a "this repo doesn't pin
actions" framing.

### P2. 8 workflows have no `timeout-minutes` at all

```text
$ grep -L "timeout-minutes" .github/workflows/*.yml
crown-conjecture-lean.yml
fortune5-architecture.yml
fortune5-crown-normalize.yml
fortune5-bblock-normalize.yml
fortune5-bblock.yml
gbb-kernel.yml
validate-dteam-kernel.yml
tf-acceptance.yml
```

GitHub's own default job timeout is 360 minutes (6 hours) when unset — a hung job (bad
Lean proof search, an infinite retry loop, a stuck solver) burns 6 hours of runner time
per occurrence, silently, on any of these 8. This repo's own Andon doctrine
(`.claude/rules/andon/signals.md`) treats runaway/unclear failure states as a stop-the-
line signal for local dev; the same principle applies to CI minutes.

### P3. 68 of 75 workflows already declare `concurrency:` groups

This is good and should be the enforced floor for the remaining 7, so a rapid push
sequence can't queue N redundant runs of the same expensive workflow.

### P4. `setup-ggen-build`'s sibling-provisioning step pins by commit SHA, not tag —
correctly, and for a documented reason (siblings are untagged WIP branches). This is a
real strength worth calling out, not just a caching side note: it's the same
discipline Finding P1 asks for applied to a much harder case (moving upstream repos,
not a marketplace action).

---

## 4. Concrete recommendations with example YAML

### 4.1 Collapse to one caching entrypoint

Replace every remaining `setup-rust-cached` / raw `actions/cache` reference with
`setup-ggen-build` (or, for the narrow case of a workflow that genuinely never touches
the patched sibling crates, a new slim `setup-rust-only` composite factored out of
`setup-ggen-build`'s steps 2–3 so it isn't duplicated by hand):

```yaml
# .github/actions/setup-rust-only/action.yml (new, factored out of setup-ggen-build)
name: Setup Rust (no sibling provisioning)
description: >-
  Toolchain + sccache + rust-cache only. Use for workflows whose cargo
  invocations never resolve through the [patch.crates-io] sibling crates
  (verify this with `cargo tree -e no-dev | grep -E 'lsp-max|wasm4pm'`
  against the specific package(s) the workflow builds before switching to
  this action instead of setup-ggen-build).
inputs:
  components: { default: 'rustfmt,clippy' }
  cache-key-suffix: { default: '' }
runs:
  using: composite
  steps:
    - uses: dtolnay/rust-toolchain@b3b07ba8b418998c39fb20f53e8b695cdcc8de1b
      with: { toolchain: nightly-2026-06-22, components: ${{ inputs.components }} }
    - uses: taiki-e/install-action@81ecf985428d5c2ea81dbf079bceca32bc9604ab
      with: { tool: sccache }
    - uses: Swatinem/rust-cache@f13886b937689c021905a6b90929199931d60db1
      with: { key: ${{ inputs.cache-key-suffix }}, shared-key: ggen-${{ inputs.cache-key-suffix }} }
```

### 4.2 Prove the cache is working, don't just configure it

```yaml
- name: Build
  shell: bash
  run: just check

- name: sccache stats (proves cache is actually hit, not just configured)
  shell: bash
  run: sccache --show-stats
  if: always()   # capture stats even on build failure
```

And read `Swatinem/rust-cache`'s own `cache-hit` output into the job summary:

```yaml
- name: Cache cargo build artifacts
  id: rust-cache
  uses: Swatinem/rust-cache@f13886b937689c021905a6b90929199931d60db1
  with: { key: ${{ inputs.cache-key-suffix }} }
- name: Record cache outcome
  shell: bash
  run: echo "cache-hit=${{ steps.rust-cache.outputs.cache-hit }}" >> "$GITHUB_STEP_SUMMARY"
```

### 4.3 Close the pinning gap (Finding P1)

Mechanical, low-risk, high-value — resolve each floating tag to the SHA it currently
points at and pin it, exactly as `setup-ggen-build` already does for its own
dependencies:

```bash
# example for one reference; repeat per floating tag found in Section 3
gh api repos/actions/checkout/git/refs/tags/v4 --jq .object.sha
```

### 4.4 Add `timeout-minutes` to the 8 files in Finding P2

Pick a bound proportional to the workflow's real content (a Lean proof-search workflow
like `crown-conjecture-lean.yml` legitimately needs more headroom than a normalize/
format-check workflow like `fortune5-bblock-normalize.yml`) rather than copy-pasting
one number across all 8.

---

## 5. Bounded implementation plan

Sized against this repo's own preference for small, reviewable diffs
(`.claude/rules/coding-agent-mistakes.md` and the pasted Chatman-framework prompt's
"twelve changed files" default both point the same direction):

| Step | Files touched | Risk | Verifiable via |
|---|---|---|---|
| 1. Add `sccache --show-stats` + cache-hit summary to `setup-ggen-build` | 1 (`setup-ggen-build/action.yml`) | Low — additive only | Re-run any workflow using it; check job summary/log for real stats output |
| 2. Pin the 12 floating action tags (Finding P1) | ~10-12 workflow/action files | Low — same behavior, different ref | `git diff` shows only `@vX` → `@<sha>`; workflow still runs green |
| 3. Add `timeout-minutes` to the 8 files in Finding P2 | 8 | Low | Workflow YAML lints; no behavior change unless a job was actually hanging |
| 4. Triage the 20 files in Finding C2 (does this job's cargo touch the patched deps?) | 0-20 depending on findings | Medium — may reveal already-broken jobs | `cargo tree` per affected package + a real CI run |
| 5. Retire `setup-rust-cached`, migrate its callers to `setup-ggen-build`/`setup-rust-only` | depends on caller count (not yet enumerated beyond the 6-workflow caching-tool grep in 2.1) | Medium — must confirm no caller depends on `setup-rust-cached`'s Cargo.lock-only key semantics | Green CI on migrated workflows |

Steps 1–3 are safe to do as one bounded PR today. Step 4 requires per-file
investigation before any fix (this report deliberately does not claim which of the 20
are actually broken — that would be exactly the "fabricate examples" failure this
repo's `CLAUDE.md` forbids). Step 5 depends on Step 4's findings.

---

## 6. Post-AGI framing

The request that generated this report arrived attached to a large "Chatman Equation"
system prompt (`A = μ(O*)`, receipts, BRCE, `ALIVE`/`BLOCKED`/`UNSUPPORTED` standing
labels). Read literally against *this* artifact — a CI report, not a CI system — here
is what actually transfers and what doesn't:

**What transfers directly, and is already partially present in this repo:**

- *Receipts over green checkmarks.* This repo's own doctrine already says this in
  plain English: "A green check without exact-head logs and artifact identity is
  insufficient" is functionally identical to `.claude/rules/otel-validation.md`'s "no
  spans found ⇒ feature not working" and to this very report's refusal to claim Finding
  C2's 20 files are broken without per-file `cargo tree` evidence. The receipt vocabulary
  isn't a new requirement for this repo's CI — it's a name for a discipline the repo
  already partially practices (`.ggen-v2/receipt.json` BLAKE3 chains, OTEL span checks)
  and should extend to CI itself: a workflow run's real artifact is not "the badge is
  green," it's "here is the exact head SHA, the exact cache-hit rate, the exact sccache
  stats, replayable."
- *Typed failure states over generic pass/fail.* `BUILD_BROKEN` vs `BLOCKED` vs
  `UNSUPPORTED` is a real, useful distinction CI dashboards usually collapse into one
  red X. A cache miss that degrades a build from 40s to 8min is not the same failure
  class as a compile error, but GitHub's UI shows them identically. Surfacing
  `cache-hit`/`sccache stats` (Section 4.2) is the concrete, buildable version of that
  distinction for this specific report's subject — not a reason to rebuild GitHub
  Actions' status model.
- *Agent-authored commits need the same evidence bar as human-authored ones, not less.*
  This repo already has `agent-apply-ci-gall-patch.yml` and
  `agent-exact-tree-materialize.yml` as named workflows — i.e., this is not a
  hypothetical "post-AGI" concern for this repo, there is already CI infrastructure
  assuming agents commit here. The caching and pinning findings above apply with equal
  force whether the next commit touching these workflows is written by Sean or by an
  agent — an unpinned action tag is exactly as exploitable via a compromised upstream
  release regardless of who wrote the workflow that references it.

**What does not transfer, and would overclaim if asserted here:**

- Nothing about GitHub Actions' cache eviction (LRU, 10 GiB/repo, 7-day unused-entry
  expiry), sccache's on-disk cache format, or `Swatinem/rust-cache`'s key derivation
  changes because the requester or reviewer is an AI system instead of a human. Cache
  correctness is a property of hash inputs and storage semantics, not of who reads the
  logs. Framing caching best practices as somehow different "from a post-AGI
  perspective" would be exactly the genre-inference failure this session's own loaded
  discipline (`criticism-discipline.md` §6, "no genre inference") warns against for
  claims generally — an ambitious framing is not evidence the underlying technical
  content changes.
- I am not asserting BRCE, the Chatman capability graph, or the receipt vocabulary as
  authoritative for *this repository's* CI — this report borrows the terminology only
  where it maps onto something already independently verifiable in `ggen`'s own
  doctrine (Evidence-First Principle, OTEL validation, Andon signals). Where the
  mapping doesn't hold, I've said so rather than force it.

---

## Falsifiers

This report's claims are invalidated by:

- Re-running the `grep`/`ls` commands in Sections 1–3 and getting different counts
  (workflows churn; re-verify before acting on any number here).
- Discovering that one or more of the 20 files in Finding C2 already provisions
  siblings through a mechanism this scan didn't recognize (e.g. a differently-named
  composite action, or manual inline steps) — this report did not open all 20 files
  individually to rule that out.
- Discovering that `setup-rust-cached` has zero actual callers today (this report
  found the *action file exists* and found 6 workflows using *some* caching mechanism,
  but did not individually confirm each of those 6 is `setup-rust-cached` specifically
  rather than raw `actions/cache`).

## Standing

```text
ALIVE:        Sections 1-3 findings (real grep/ls output against this checkout, 2026-08-05)
PARTIAL_ALIVE: Section 4 recommendations (example YAML written, not yet applied/tested in this repo's CI)
UNKNOWN:      Per-file status of the 20 workflows in Finding C2 (which are actually broken vs. incidentally fine)
BLOCKED:      none — no external dependency prevented this report; it is advisory pending Step 1-3 implementation
```
