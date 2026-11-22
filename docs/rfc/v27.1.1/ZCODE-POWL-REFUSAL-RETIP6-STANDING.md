# `zcode` POWL/OCEL refusal retip6 — push-first PARTIAL

```text
exit=137 standing=PARTIAL(bounded_refusal_retip6) tip=2685119bd92e607cdb93d3dafdbb80cd9da0447d
elapsed=~8s sjira=KILLED(137) bound=SKIPPED
refusal_ALIVE≠Foundry_ALIVE
side=cursor/v2711-zcode-powl-retip6-bc54
agent=bc-37e9fbe6-cd76-5497-a2ec-0f639d95f4d1
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-zcode-powl-retip6-bc54` |
| Live tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` lineage @ `2685119bd92e607cdb93d3dafdbb80cd9da0447d` (tip-wins ui_layout exact-retip5 PARTIAL) |
| Agent | `bc-37e9fbe6-cd76-5497-a2ec-0f639d95f4d1` |
| UTC | `2026-11-22T17:48:30Z` |
| Prior ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ `cd7c2433b` |
| Reconfirm | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` **PRESENT** @ tip |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests **PRESENT** @ tip |
| Tip-cargo (this retip) | warm `/home/ubuntu/zcode-powl-refusal-target` under host thrash → **EXIT 137** (OOM/killed @ ~8s); bound **SKIPPED** |
| Status | `/tmp/cursor/zcode-powl-retip6-court.status` → `EXIT_sjira:137 EXIT_bound:SKIPPED` |
| Crown absorb | tip-wins onto live crown tip `0c18b3d1a` via `cursor/v2711-vw-zcode-absorb-27b4` (side tip `d1c4740f7`; older `f3ddbe5a1` superseded; no ManagePullRequest; no force-push) |
| Foundry `--live` / `--dry-run` | **not claimed** — refusal path only; **refusal ALIVE ≠ Foundry ALIVE** |

## Standing

| Claim | Standing |
|-------|----------|
| Bounded refusal tip-cargo this retip (&lt;3min rule) | **PARTIAL** — killed under thrash; no EXIT 0 this land |
| Court sources PRESENT on tip | **observed** (not promoted to ALIVE without EXIT 0) |
| Foundry / hop-5 `--live` | **not this subject** — refuse Foundry `ALIVE` |
| Prior refusal ALIVE lands | retained as historical; this retip does **not** re-mint ALIVE |

```text
PARTIAL(bounded_refusal_retip6 @ tip 2685119bd92e607cdb93d3dafdbb80cd9da0447d)
  ⇔  tip-cargo zcode_powl_ocel_court did not EXIT 0 within 3min
  ∧  next verifier named below
  ∧  this honesty sidebranch pushed to origin
```

**Honesty:** this retip is **PARTIAL**. Prior refusal `ALIVE` ≠ this retip `ALIVE`. **refusal ALIVE ≠ Foundry ALIVE** — never invent Foundry crown `ALIVE` from a refuse-path court.

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

Foundry `--live` remains **BLOCKED_HITL** until provider creds; see
[`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10 /
[`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).

## Path honesty cross-pin (2026-11-22)

| Field | Value |
|-------|-------|
| Path pin | [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) |
| Branch | `cursor/v2711-zcode-path-retip-bc54` (`bc-3708a779`) |
| Crown tip | `83c954e5fea4d454f3cc79410a368844a0469fb2` (`e24ce7a73+`) |
| Held | tip-cargo **PARTIAL** (EXIT **137** this retip) vs lineage refusal **ALIVE** @ `cd7c2433b`; hop-5 autonomic remediations slice **PARTIAL**; **refusal ≠ Foundry ALIVE**; `--live` **BLOCKED_HITL** |
| Cargo | **NOT RUN** on path pin (docs-only) |

Exact next (unchanged): tip-cargo § above → **6+4**/EXIT:**0**; founder `--live`:
`cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **0**.
