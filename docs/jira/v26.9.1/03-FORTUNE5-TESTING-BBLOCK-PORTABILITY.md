# Decouple fortune5-testing-bblock-pack from testing ggen's own repo

## Status update (round 3 — chaos-suite BUILD_BROKEN root cause resolved)

A later round investigated the `chaos` suite specifically (one of the 6 suites
this ticket's own "Remains" section below left `BUILD_BROKEN`). Real branch
`story/GI-CORE-bblock-plan-workspace-path-fix` (repo `~/ggen`, worktree
`~/ggen/.claude/worktrees/bblock-plan-workspace-path-fix`, based on
`story/GI-CORE-bblock-project-wiring`, commit `211d95f14`; not merged/pushed).

**Finding:** the reported defect (a hardcoded `/workspace` path breaking the
chaos suite's write-then-read idempotency check) does **not** exist in
`crates/ggen-cli/src/cmds/bblock.rs` at HEAD — `project_root()` resolves
`std::env::current_dir()` identically for both the plan-write and the
idempotency-check read path. All 3 commits that ever touched the file were
grepped; no literal `/workspace` string was ever present. The real reproduction
traced instead to `/Users/sac/.local/bin/ggen` being a Docker wrapper pinned to
a **stale container image** (`ghcr.io/seanchatmangpt/ggen-ecosystem:v26.8.28`)
built before this crate's current bblock code.

**Before:** the prior scratch-consumer run (`~/gm03-scratch-consumer/VERIFIER_REPORT.json`)
against that stale wrapper showed `standing=BUILD_BROKEN`,
`"cannot read /workspace/.ggen/bblocks/receipts/aws/testing-plan-result.json:
No such file or directory"`.

**After:** built the real native `ggen` binary from this worktree's current
source (`cargo build`, ~4m38s) and re-ran the actual chaos suite against it
from a real non-`/workspace` cwd
(`GGEN_BIN=<built-binary> python3 consumer/testing-bblock/testing_bblock.py run chaos`)
→ `standing=ALIVE`, `build_broken_count=0`, `alive_count=1`
(`~/gm03-scratch-consumer/VERIFIER_REPORT.chaos.after.json`). Added a real
Chicago-style regression test,
`crates/ggen-cli/tests/bblock_plan_workspace_path_test.rs`, driving the
compiled binary twice from a temp dir (write, then idempotency-check read),
asserting no hardcoded `/workspace` in stderr and correct receipt chaining —
`cargo test -p ggen-cli-lib --test bblock_plan_workspace_path_test` →
**"1 passed; 0 failed"**. `grep` for mock usage over the new test file: zero
matches.

**No source-code fix was needed or made** — the defect does not exist at HEAD;
the regression test now guards it. The genuinely remaining gap is
**operational, not code**: `~/.local/bin/ggen`'s pinned Docker image tag is
stale relative to current `main` and should be rebuilt/repinned so consumers
using that wrapper (rather than a natively-built binary) actually see this
already-correct behavior. This worktree/branch has not been merged.

This does not change the ticket's own **PARTIAL** verdict below (5 of the
original 6 `BUILD_BROKEN` suites — `property-fuzz`, `cli-e2e`, `stress`,
`benchmark`, `replay` — are unaffected by this chaos-specific finding and
remain governed by the CLI-argument-drift defect this ticket's own "Remains"
section already discloses, itself since addressed by
`story/GM-03-completion-cli-drift-fix` in `~/ggen-marketplace`; see
`09-SESSION-SUMMARY-ROUND3.md` for the full cross-repo picture).

## Status (updated after real implementation)

**PARTIAL.** Fixed for real in `~/ggen-marketplace`, worktree
`/Users/sac/ggen-marketplace-worktrees/GM-03`, branch
`story/GM-03-fortune5-testing-bblock-portability`, commit `fe8e7ad49` (not
merged/pushed). Files changed: `packs/fortune5-testing-bblock-pack/ontology.ttl`,
`queries/testing-bblock.rq`, `templates/testing_bblock.py.tmpl`.

**Fixed (consumer-admitted facts, verified against a real scratch consumer):**
`discover_repo_root()`/`resolve_ggen()` no longer detect `Cargo.toml`+
`crates/ggen-cli` or default to `target/debug/ggen` (grep confirms those literal
strings now appear only in comments, not detection logic). Five new
consumer-admitted ontology facts on `tb:TestingBBlock`
(`tb:consumerRootMarker`, `tb:consumerBinaryCommand`, `tb:consumerTestCommand`,
`tb:consumerForbiddenTokenFiles`, `tb:consumerForbiddenTokens`), selected via
`queries/testing-bblock.rq`, are baked into the generated script as literal
`CONSUMER_*` constants at `ggen sync run` time. Verified end-to-end at a fresh
scratch consumer, `~/gm03-scratch-consumer` (outside `~/ggen`,
`~/ggen-marketplace`, `~/beam4pm`): `ggen sync run --dry-run`/`ggen sync run`
(real ggen 26.8.28) both succeeded; the generated script's baked constants
(`CONSUMER_ROOT_MARKER="Cargo.toml"`, `CONSUMER_BINARY_COMMAND="ggen"`,
`CONSUMER_TEST_COMMAND="cargo test"`, empty forbidden-token lists) matched the
scratch consumer's own admitted facts, not ggen's. Real execution:
`testing_bblock.py self-test` → `standing=PARTIAL_ALIVE`; `... verify --report`
produced a real BLAKE3-chained 9-receipt report with `protocol-unit=ALIVE` (real
`cargo test` ran), `stdio-http-integration=ALIVE` (real loopback HTTP
byte-identity check), and `security=BLOCKED "no consumer forbidden-token source
files admitted (tb:consumerForbiddenTokenFiles)"` — the intended, correctly-named
skip (2/9 suites ALIVE + 1 correctly BLOCKED-as-skip).

**Remains (out of scope for this ticket, real and cited):** 6 of 9 suites
(`property-fuzz`, `cli-e2e`, `chaos`, `stress`, `benchmark`, `replay`) report
`BUILD_BROKEN` against the scratch consumer. All trace to one real,
pre-existing, **OUT-OF-SCOPE** defect in ggen's own CLI, not in this pack's fix:
the template's suite bodies (unchanged by this diff) invoke
`ggen bblock inspect/plan <group> <provider>` **positionally**, but the real
installed `ggen` 26.8.28 binary's `bblock inspect/plan` now requires
`--group-id`/`--provider` flags —
`error: unexpected argument 'testing' found... Usage: ggen bblock inspect
[OPTIONS] --group-id <GROUP_ID> --provider <PROVIDER>`. This is a real
CLI-argument-syntax drift in `ggen-cli` itself (a defect in ggen, not in this
pack), orthogonal to and outside this ticket's declared scope (root-marker /
binary-resolution / test-command / forbidden-token-file portability only). A
regression check (`ggen sync run --dry-run` inside
`packs/fortune5-testing-bblock-pack` itself, ggen's own repo as one legitimate
consumer with unchanged default ontology facts) still succeeds, generating the
same 13 files with no error.

Chicago testing discipline confirmed:
`grep -rn "unittest.mock|Mock(|MagicMock|patch(|monkeypatch|mockall|jest.mock" packs/fortune5-testing-bblock-pack/`
→ zero matches.

Part of the v26.9.1 Fortune-5-ready bundle work. Context and non-goals for the
whole initiative are established by
[00-FORTUNE5-READY-BUNDLE-OVERVIEW](00-FORTUNE5-READY-BUNDLE-OVERVIEW.md) and the
approved planning session it derives from (real exploration across `~/ggen`,
`~/ggen-marketplace`, `~/ggen_igniter`, `~/beam4pm`, cited verbatim below where this
ticket depends on it). This ticket is the fix for gap 2b of that plan:
`fortune5-testing-bblock-pack` is hardcoded to test ggen's own source tree, not a
consumer project.

## Evidence: what's hardcoded today

File:
`vendor/ggen-marketplace/packs/fortune5-testing-bblock-pack/templates/testing_bblock.py.tmpl`
(601 lines, generates one executable Python script from
`pack.toml` version `26.7.31`, `SUITES` constant lists nine boundary-crossing
suites: `protocol-unit`, `property-fuzz`, `stdio-http-integration`, `cli-e2e`,
`security`, `chaos`, `stress`, `benchmark`, `replay`).

Repo self-detection, not consumer-detection:

- `discover_repo_root()` (line 55) walks `Path.cwd()` and its parents looking for
  `(candidate / "Cargo.toml").is_file() and (candidate / "crates/ggen-cli").is_dir()`
  — this is ggen's own crate layout, not a shape any arbitrary consumer project (a
  BEAM app like beam4pm, or any other language) would have.
- `resolve_ggen(root)` (line 71) defaults to `root / "target/debug/ggen"` when
  `GGEN_BIN` is unset — a Rust `cargo build` debug artifact path, again specific to
  ggen's own build tree.
- `protocol_unit()` (line 108) runs `cargo test -p ggen-cli-lib bblock --lib` —
  invokes ggen's own crate's own unit tests, not anything belonging to the
  consumer.
- `security()` (line 300-301) reads
  `(root / "crates/ggen-cli/src/cmds/bblock.rs").read_text()` directly and greps it
  for forbidden actuation tokens (`reqwest`, `std::net`, `Command::new`,
  `terraform`, `pulumi`, `kubectl`, `aws_sdk`, `azure_`, `google_cloud`) — this
  suite is testing ggen's own compiler source file, not any file the consumer
  owns.
- `cli_e2e()` (line 234) asserts
  `"fortune5-testing-bblock-pack" not in payload.get("packs", [])` and inspects
  `.ggen/packs.lock`/`.ggen/bblocks/...` paths relative to a `tempfile.TemporaryDirectory`
  — this suite is consumer-shaped in isolation (it does operate inside a scratch
  temp dir), but it is only ever invoked with `root`/`ggen` values that
  `discover_repo_root`/`resolve_ggen` resolved to ggen's own tree, so in practice it
  still exercises ggen's own binary against ggen's own checkout.

The pack's `README.md.tmpl`-cited proof of the suite working (per the approved
planning session, not re-derived here) is `chatman-ecosystem/ggen-src`'s e2e test —
itself a checkout of ggen's own source tree, not an independent consumer project.
No prior run of this pack's nine suites against a project that is not ggen itself
has been found in this repo's history.

## Fix

1. **Ontology-admitted consumer facts, not filesystem auto-detection.** Add
   `bpm`-analogous facts to the pack's own `ontology.ttl` (or a new predicate the
   consumer's own ontology instance admits) naming: the consumer's real repo root
   marker file (e.g. `Cargo.toml` for Rust consumers, `mix.exs` for Elixir,
   `package.json` for Node — not hardcoded to Cargo.toml), the consumer's CLI
   binary path or build command, and the consumer's own source-file paths any
   suite needs to inspect (replacing the `bblock.rs`-specific read in `security()`
   with a consumer-admitted list of source paths + forbidden-token list).
2. **Template-time binding, not runtime auto-discovery.** `discover_repo_root()`
   and `resolve_ggen()` currently run *inside the generated script* at test-run
   time; the fix rewrites `testing_bblock.py.tmpl` so the Tera/Jinja template
   variables (bound from the consumer's admitted RDF facts at `ggen sync run`
   time) are baked into the generated script as literal values — the generated
   script for a given consumer stops walking the filesystem guessing at Cargo
   layouts and instead contains that consumer's real root marker, binary path, and
   source paths as generated constants.
3. **`security()` suite generalized.** Replace the literal
   `crates/ggen-cli/src/cmds/bblock.rs` read with a consumer-admitted list of
   "no-direct-actuation" source files and forbidden-token list (empty list is a
   legitimate consumer answer — not every consumer has a bblock-compiler-shaped
   file to check; the suite must not fail closed by inventing a check that doesn't
   apply).
4. **`protocol_unit()` suite generalized.** Replace the literal
   `cargo test -p ggen-cli-lib bblock --lib` invocation with a consumer-admitted
   test command (for beam4pm: `rebar3 eunit` or `mix test`, per
   `~/beam4pm/CLAUDE.md`'s own Build/sync/test section — cite that file's real
   commands, do not invent new ones).

## Acceptance

- `discover_repo_root()`/`resolve_ggen()`-equivalent logic in the regenerated
  template no longer contains the literal strings `crates/ggen-cli` or
  `target/debug/ggen` as auto-detection targets against an arbitrary consumer;
  those become consumer-admitted, template-bound values. (ggen's own repo may
  itself remain a valid *consumer* of the fixed pack — this bullet forbids the
  literal strings as hardcoded detection logic, not as one legitimate consumer's
  admitted fact values.)
- `security()`'s forbidden-token source-file list is consumer-admitted (via
  ontology fact or template variable), not the literal
  `crates/ggen-cli/src/cmds/bblock.rs` path, and the generated suite for a
  consumer with zero admitted source files degrades to a named, explicit skip
  (visible in the `MachineVerifierReport`, not silently passing) rather than
  either crashing or fabricating a pass.
- **Real generation against a fresh scratch consumer project unrelated to ggen's
  own source** — not beam4pm (beam4pm is reserved as the bundle-installer proof
  target per the overview ticket) and not any checkout of ggen itself. A new,
  minimal scratch project (e.g. a fresh `cargo new` or `mix new` in a throwaway
  directory outside `~/ggen`, `~/ggen-marketplace`, and `~/beam4pm`) with the
  fixed pack wired in via `ggen sync run`.
- **Real execution, not just successful generation.** All nine suite entrypoints
  (`protocol-unit`, `property-fuzz`, `stdio-http-integration`, `cli-e2e`,
  `security`, `chaos`, `stress`, `benchmark`, `replay`) must actually run against
  the scratch consumer project and produce real, cited pass/fail/blocked output
  per suite — "generates without error" is explicitly insufficient per this
  ticket's brief; a suite that structurally cannot apply to the scratch consumer
  (e.g. no HTTP surface to test) must report a named `SuiteBlocked` in the
  verifier report, never a silently-omitted or fabricated pass.
- The generated script's own machine-readable output — `SCHEMA =
  "ggen.testing.verifier-report.v1"` (verifier report) and `RECEIPT_SCHEMA =
  "ggen.testing.suite-receipt.v1"` (per-suite BLAKE3-chained receipt), both
  already defined in `testing_bblock.py.tmpl` lines 26-27 — is real output from
  this run, cited by file path and quoted (not paraphrased) status per suite in
  the closing evidence for this ticket. Compilation or generation succeeding
  alone is not acceptance; the `MachineVerifierReport` JSON produced by the real
  run against the real scratch consumer is the crown.
- Suite-by-suite acceptance floor, cited against the real run's report (not
  assumed from the template's design intent):
  - `protocol-unit` — the consumer-admitted test command (never
    `cargo test -p ggen-cli-lib`) actually ran and its real exit code is recorded.
  - `property-fuzz` — the 32 randomized invalid-provider-alias cases (seed
    `0xB10C`, per line 128) ran against the scratch consumer's real `ggen bblock`
    invocation and produced a real pass/fail, not a hardcoded expectation copied
    from ggen's own corpus.
  - `stdio-http-integration` — the loopback HTTP byte-identity check
    (`received != [body]` at line 168) ran for real against the scratch
    consumer's real `ggen bblock providers` output.
  - `cli-e2e` — the black-box artifact check (`infrastructure/testing`,
    `.ggen/packs.lock`, `.ggen/bblocks/groups/testing.json`,
    `.ggen/bblocks/receipts/aws/testing-enable-result.json`) ran inside a real
    temp dir under the scratch consumer, not ggen's own tree.
  - `security` — ran with the consumer-admitted (possibly empty) forbidden-token
    file list, not `bblock.rs`; if the list is empty, the report names this an
    explicit skip with the reason, not a silent pass.
  - `chaos` — the malformed-receipt fault-injection and recovery check (lines
    321-336) ran against the scratch consumer's real `.ggen/bblocks/receipts/`
    tree.
  - `stress`, `benchmark`, `replay` — each ran to completion (or a named
    `SuiteBlocked`/`SuiteFailure`) against the scratch consumer, with real timing
    and exit-code evidence in the receipt chain, not asserted from template
    inspection alone.
- No regression to ggen's own use of this pack as a legitimate consumer: after the
  fix, regenerating `testing_bblock.py.tmpl` against ggen's own repo (with ggen's
  own facts now expressed as admitted ontology values rather than hardcoded
  detection logic) must still produce a script whose nine suites pass against
  ggen's own tree, re-run and cited for real, not assumed to still work because
  the old hardcoded path happened to match.

## Non-goals

- Building a general auto-detection heuristic that guesses a consumer's project
  shape (language, build tool, binary path) without an explicit admitted fact.
  Consumer facts are declared, never inferred.
- Extending this pack to cover consumer languages/build systems beyond what's
  needed to prove portability against one real scratch consumer plus beam4pm's
  actual shape (Elixir/`mix`/`rebar3`) — a general N-language test-command
  registry is out of scope for this ticket.
- Fixing `fortune5-required-capabilities-pack`'s separate hardcoded-path defect
  (tracked in its own ticket in this v26.9.1 set) — this ticket is
  `testing-bblock` only.
- Resurrecting or building the general ggen-core pack-dependency-resolution
  algebra, or the bundle-manifest installer itself — those are separately
  tracked; this ticket's scope ends at making the one pack portable.

## See Also

- [00-FORTUNE5-READY-BUNDLE-OVERVIEW](00-FORTUNE5-READY-BUNDLE-OVERVIEW.md) — the
  index ticket this depends on; states the approved plan, the other two
  portability gaps, and the beam4pm bundle-installer proof this ticket's fix feeds
  into.
- `vendor/ggen-marketplace/packs/fortune5-testing-bblock-pack/templates/testing_bblock.py.tmpl`
  in `~/beam4pm` (submodule-vendored copy of the same file this ticket edits at
  its source in `~/ggen-marketplace`) — the file this ticket rewrites.
- `~/beam4pm/CLAUDE.md` — Build, sync, and test section, source of the real
  `rebar3 eunit`/`mix test` commands cited for the `protocol_unit()` suite fix.
- `~/ggen/docs/jira/v26.8.16/00-OVERVIEW.md` and
  `~/ggen/docs/jira/v26.8.16/01-COMMIT-BOUNDARY.md` — style references this
  ticket's format follows (evidence-cited, numbered acceptance bullets, explicit
  Non-goals, See Also footer).
