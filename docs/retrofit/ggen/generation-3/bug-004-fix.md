# Generation 3: BUG-004 fixed and adversarially proven

## Root cause

`PackAgent::install` (`crates/ggen-marketplace/src/agent/facade.rs`) built its
`InstallByIdInput` with `target_dir: None`. `install_pack_by_id`
(`crates/ggen-marketplace/src/marketplace/install.rs`), when `target_dir` is `None`, falls back
to the **global** `dirs::home_dir().join(".ggen/packs/<pack_id>")` for both the "already
installed" existence check and the actual materialization directory -- completely disconnected
from `self.root` (the project root), where the lockfile already correctly lives
(`lockfile_path()`, two lines above `install` in the same file). Consequence: a caller in an
isolated/CI environment could get a false "already installed" refusal purely from unrelated
global machine state, and a real (non-refused) install would write pack content **outside the
project entirely** while its lockfile entry stayed project-local -- install path and lockfile
root diverged.

## Fix

Two files, minimal diffs:

- `crates/ggen-marketplace/src/agent/facade.rs`: `target_dir: None` -> `target_dir:
  Some(self.root.join(".ggen").join("packs").join(&req.pack_id))`.
- `crates/ggen-cli/src/cmds/agent.rs`: the `install` verb no longer returns the BUG-004 typed
  refusal; it calls `install_impl` (the always-correct implementation that was dead code behind
  the refusal), which lost its `#[allow(dead_code)]`.

## Independent verification (separate agent, fresh run, not the implementer's self-report)

```
cargo test -p ggen-cli-lib --test agent_lifecycle_test
running 4 tests
test agent_install_nonexistent_pack_is_fail_closed ... ok
test agent_install_dry_run_writes_no_durable_state ... ok
test agent_verify_tampered_receipt_is_invalid_via_cli ... ok
test agi_completes_project_lifecycle_through_ggen_agent ... ok
test result: ok. 4 passed; 0 failed
```

```
cargo test -p ggen-marketplace
429 tests passed across 8 test binaries (unit + 6 integration suites + doctests), 0 failed,
1 ignored (unrelated network-example doctest)
```

`cargo build -p ggen-marketplace -p ggen-cli-lib --bin ggen` and
`cargo clippy -p ggen-marketplace -p ggen-cli-lib -- -D clippy::correctness -D
clippy::suspicious -D clippy::perf` both clean.

## Adversarial sabotage proof (executed directly, real binary, real temp dirs, real subprocess
execution -- not a unit-level mock)

**Scenario A** (the actual regression this bug caused, must now succeed): pre-created a decoy
directory at an isolated `$HOME_A/.ggen/packs/decoy-pack` (simulating unrelated global state
from a different project/test run), then ran the real `ggen` binary with `HOME=$HOME_A` against
a fresh, separate project B with no lockfile entry for that pack.

```
$ HOME=$HOME_A GGEN_PACKS_DIR=<registry> ggen agent install decoy-pack --format json
exit 0
{"install_path":".../project_b/.ggen/packs/decoy-pack", ...}
```

Filesystem evidence: content landed under `project_b/.ggen/packs/decoy-pack/` (project-local,
lockfile + receipt + keys all under project B's own `.ggen/`); the global decoy at `$HOME_A`
was completely untouched (still contains only its original unrelated file). The bug -- a false
refusal or a wrongly-global install -- did not reproduce.

**Scenario B** (must still correctly refuse a genuine duplicate): reinstalling the same pack
into the same project without `--force`:

```
$ ggen agent install decoy-pack --format json
exit 1
ERROR: install failed: Installation failed: Pack already installed at
  .../project_b/.ggen/packs/decoy-pack
```

Note the cited path is the **project-local** path, not a global one -- confirming the fix
rescoped the check, it did not disable it. Control: the same command with `--force true`
succeeds (exit 0), confirming the override still works.

Both scenarios passed exactly as required. Temp directories cleaned up after.

## Honest current sync/evidence state

Re-ran the evidence bootstrap for real:

```
build: exit 0
clippy-reference-gate: exit 0   (was 101 in Generation 2's initial state)
test-workspace: exit 101         (still red -- but NOT for BUG-004 anymore, see below)
byte-identity: exit 1            (transitively blocked by test-workspace, correct reflexive behavior)
```

`ggen sync run` still refuses via `[FM-LAW-018]` gate 020 -- correctly, since real evidence is
still red. **This generation does not claim test-workspace reached Green.** A full
`cargo test --workspace` run surfaced two failures unrelated to BUG-004 (all `agent_lifecycle_test.rs`
tests now pass cleanly):

```
test mega_project_all_packs_sync ... FAILED
  second sync must not rewrite anything, but docs/generated/W4PM_COGNITION_BREED_CATALOG.md
  was `written` (crates/ggen-engine/tests/cross_pack_matrix.rs:194)

test ontology_union_and_declaration_order_are_canonical ... FAILED
  assertion `left == right` failed: reversed [packs] declaration order must produce an
  identical receipt payload (crates/ggen-engine/tests/cross_pack_matrix.rs:301)
```

Both are second-sync-idempotency / declaration-order-canonicality violations, in
`wasm4pm-cognition-pack`'s generated breed catalog doc -- a distinct, real defect, unrelated to
`ggen agent install` or anything this generation touched. **Not fixed here** -- named as a new
obligation (see `.tcps/retrofit/ggen/controller/obligations.ttl`) rather than silently absorbed
into BUG-004's scope or left undiscovered. This is exactly the kind of false-standing risk the
controller mandate ranks highest priority; it was only surfaced because this generation ran the
full workspace suite to completion for the first time in a while.

## Standing

BUG-004 itself: **closed**, real fix, independently reproduced, adversarially sabotage-proven
in both directions. `test-workspace`/`byte-identity` evidence classes remain red -- correctly,
honestly -- for a newly-discovered, unrelated defect now tracked separately. No claim of
Managed/L5 standing for ggen's own sync is made.
