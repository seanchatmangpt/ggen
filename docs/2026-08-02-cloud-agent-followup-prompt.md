Read docs/2026-08-02-pr-batch-merge-summary.md on main. It catalogs 8 PRs (#554-#561,
landed as #554-#557, #559, #560, #562, #563) merged into this repo today, each with
its own self-reported standing (ALIVE / PARTIAL_ALIVE / BLOCKED_* / UNKNOWN /
DESIGNED). None of them claim ALIVE at the aggregate level. Your job is to close as
many of the specific, named gaps below as you actually can in this environment, and
open one PR with the result.

Do not re-litigate or re-verify claims already marked ALIVE in that doc — trust them
as given and build on top. Only work the items explicitly marked incomplete:

1. **#555 / #563 (cyberpunk-tv-platform), `deckGlRuntime = BLOCKED_DEPENDENCY_TRANSPORT`**
   The real deck.gl 9.1.14 package could not be fetched in the original environment,
   so the browser renderer fell back to raw WebGL2. Check whether deck.gl 9.1.14 is
   fetchable from this environment (npm registry reachability may differ). If yes,
   wire it in for real and update the receipt/replay evidence to reflect a genuine
   deck.gl run, not a substitute. If still unreachable, say so explicitly and do not
   touch this item further — do not fake the package.

2. **#555 / #563, `rustImplementation = BLOCKED_TOOLCHAIN_REQUIRED`**
   Identify what toolchain was missing (check the pack's local run script under
   `packs/cyberpunk-tv-platform/`) and either install/pin it or document precisely
   why it can't be provided here.

3. **#555 / #563, `rokuPhysicalDevice` / "physical Roku execution receipts"**
   No physical Roku device is available in a cloud agent either — do not attempt to
   fake this. Instead, harden the existing source-derived simulation so the gap
   between simulated and physical execution is narrower and more precisely
   documented (e.g., which BrightScript APIs are simulated vs. stubbed).

4. **#557 (SBB capability density), `PARTIAL_ALIVE` — "exact-head repository CI and
   an independent verifier are required before promotion to ALIVE"**
   Add the missing CI workflow that runs `ggen sbb validate|receipt|replay` against
   the exact merge head, and set up (or point at) an independent verifier the way
   #559's PRD/ARD bundle did with the `ggen-legacy` repo. If a second independent
   repo/verifier isn't available to you, implement the CI half only and state clearly
   that independent-verifier promotion is still open.

5. **#558/#562 (Vision 2030 / Maximalism), 49-capability catalog stuck at `DESIGNED`**
   Pick the highest-leverage subset of those 49 declared-but-unevidenced capabilities
   and actually produce SBB evidence bundles (positive witness, negative fixture,
   adversarial falsifier, verifier) for them, per the schema #557 already defines.
   Do not move a catalog entry to ALIVE without a real evidence bundle backing it —
   that's the exact anti-pattern the PR itself warns against.

6. **#561/#563, "Remaining 20%" items 2-4:**
   - federated multi-node transport execution — implement or scope down to what's
     testable without real multiple nodes (e.g., simulate with local processes and
     say so).
   - a generalized evidence ontology shared across every ggen pack (currently only
     cyberpunk-tv-platform has one) — design and land this as a reusable
     `ggen-evidence` ontology module other packs can adopt, and migrate at least one
     other pack (e.g. `pcq-marketplace-pack` from #560) onto it as proof it
     generalizes.
   - a native interactive wizard UI beyond the scriptable command surface.

7. **#554, explicitly `IN_PROGRESS` items:** `general CI / Quality` and
   `generated-law exact-head proof`. Find out what's actually missing (check
   `.github/workflows/` for what #554 added vs. what's referenced as pending) and
   close them out.

For every item you close, update its status marker in place using the same
vocabulary this repo already uses (ALIVE / PARTIAL_ALIVE / BLOCKED_<reason> /
UNKNOWN) — do not invent new terms. For every item you cannot close, leave it
explicitly marked with the blocking reason rather than silently dropping it.

Constraints:
- Chicago TDD only (no mocks) per this repo's CLAUDE.md.
- `just pre-commit` must pass before you open the PR.
- Do not claim ALIVE for anything that requires evidence you can't actually produce
  in this environment (physical devices, external independent repos you don't have
  access to, etc.) — mark it BLOCKED with the specific missing resource instead.
- Open exactly one PR against `main` with a summary structured like the 8 PRs in
  docs/2026-08-02-pr-batch-merge-summary.md: what changed, exact commit/head
  identities, what's now ALIVE vs. still PARTIAL_ALIVE/BLOCKED, and why.
