# `zcode` POWL/OCEL refusal retip6 — PARTIAL → tip-wins absorb

```text
ggen_tip_before=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
sidebranch=cursor/v2711-zcode-powl-retip6-bc54@f3ddbe5a19af89de45ba634d8a76222f3528d715
absorb_branch=cursor/v2711-vw-zcode-absorb-5954
side_parent=c2cd54f562f1fe675d9ea5fabf989b89a9f8a796
exit=TIMEOUT standing=PARTIAL(bounded_refusal_retip6)
elapsed=>180s sjira=TIMEOUT bound=SKIPPED
refusal_ALIVE≠Foundry_ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-REFUSAL-RETIP6-STANDING` (tip-wins absorb) |
| Sidebranch | `cursor/v2711-zcode-powl-retip6-bc54` @ `f3ddbe5a19af89de45ba634d8a76222f3528d715` |
| Absorb branch | `cursor/v2711-vw-zcode-absorb-5954` |
| Tip before (this absorb) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` (ui_layout exact-retip5 ledger honesty; raced past side parent `c2cd54f56`) |
| Side parent | `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` (Taffy AOT retip7 tip-wins) |
| Agent (side) | `bc-cf179f93-ea03-5661-841d-7d083467b897` |
| UTC (side) | `2026-11-22T17:46:03Z` |
| Prior ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ `cd7c2433b` |
| Reconfirm | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` **PRESENT** @ tip |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests **PRESENT** @ tip |
| Tip-cargo (this retip) | cold `/tmp/ggen-zcode-powl-retip6-court` under host thrash → **TIMEOUT &gt;180s**; sjira still compiling (`ring`/`oxjsonld`); bound **SKIPPED** |
| Status | `/tmp/cursor/zcode-powl-retip6-court.status` → `EXIT_sjira:TIMEOUT EXIT_bound:SKIPPED` |
| Crown absorb | tip-wins via `cursor/v2711-vw-zcode-absorb-5954` (FF only; no force-push) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry `--live` / `--dry-run` | **not claimed** — refusal path only; **refusal ALIVE ≠ Foundry ALIVE** |

## Standing (preserved on absorb)

| Claim | Standing |
|-------|----------|
| Bounded refusal tip-cargo this retip (&lt;3min rule) | **PARTIAL** — budget exceeded; no EXIT 0 this land |
| Court sources PRESENT on tip | **observed** (not promoted to ALIVE without EXIT 0) |
| Foundry / hop-5 `--live` | **not this subject** — refuse Foundry `ALIVE` |
| Prior refusal ALIVE lands | retained as historical; this retip does **not** re-mint ALIVE |

```text
PARTIAL(bounded_refusal_retip6 @ tip c2cd54f562f1fe675d9ea5fabf989b89a9f8a796)
  ⇔  tip-cargo zcode_powl_ocel_court did not EXIT 0 within 3min
  ∧  next verifier named below
  ∧  this honesty sidebranch pushed to origin
```

**Honesty:** this retip is **PARTIAL**. Prior refusal `ALIVE` ≠ this retip `ALIVE`. **refusal ALIVE ≠ Foundry ALIVE** — never invent Foundry crown `ALIVE` from a refuse-path court. This absorb preserves **PARTIAL** and does **not** invent Foundry `ALIVE`.

## Tip-wins absorb (2026-11-22)

| Field | Value |
|-------|-------|
| Tip before | `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` |
| Sidebranch | `f3ddbe5a19af89de45ba634d8a76222f3528d715` (parent `c2cd54f56`) |
| Absorb branch | `cursor/v2711-vw-zcode-absorb-5954` |
| Refusal tip-cargo this retip | **PARTIAL held** — TIMEOUT &gt;3min |
| Prior refusal ALIVE | lineage @ `cd7c2433b` held — **not** re-minted |
| Foundry | **not** promoted (refusal ≠ Foundry) |
| Push policy | FF-only onto `cursor/ecosystem-spec-27-1-1-bc54`; **no** force-push; **no** ManagePullRequest |

## Exact next verifier

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
# Prefer warm refusal target when thrash clears:
CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked \
  --config build.jobs=1 -- --test-threads=1
CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-graph remediation_bound --lib --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 + 4 passed; EXIT 0  → then promote refusal ALIVE on a later retip
```

Foundry `--live` remains out of scope until provider creds; see [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).
