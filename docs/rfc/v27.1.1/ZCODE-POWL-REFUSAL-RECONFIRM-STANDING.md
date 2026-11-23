# `zcode` POWL/OCEL bounded-refusal reconfirm (photolithography clause 1 / hop 5)

```text
exit=0 standing=ALIVE(bounded_refusal_reconfirm) sjira=6/6 bound=4/4 tip=df9597c1817313f8d5173f425af28aaf485d114c
live=BLOCKED_HITL refusal_ALIVE≠live_ALIVE
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-zcode-refusal-reconfirm-bc54` |
| Ecosystem tip (observed) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `df9597c1817313f8d5173f425af28aaf485d114c` |
| Photolithography | Clause 1 (§13.1) hop 5 — bounded refusal leg only |
| Prior ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ tip `cd7c2433b` (`cursor/v2711-zcode-powl-refusal-alive-bc54`) |
| Court sources | ≡ tip (`cd7c2433b` ⊆ `df9597c18`; no diff on `zcode_powl_ocel_court` / `zcode_powl_ocel` / `remediation_bound`) |
| zcode-cli mount | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` (`chicago_PRESENT`; `foundry_PRESENT`) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court --offline -- --test-threads=1` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib --offline -- --test-threads=1` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-refusal-reconfirm-sjira-cargo.log`, `/tmp/cursor/zcode-powl-refusal-reconfirm-bound.log`, `/tmp/cursor/zcode-powl-refusal-reconfirm-receipt.md` |
| Standing (bounded refusal) | **ALIVE** (reconfirm; in-repo tip-cargo refuse path only) |
| Foundry `--live` | **BLOCKED_HITL** — `~/.zcode/v2/provider_config.json` **ABSENT**; host `ANTHROPIC_API_KEY`/`ZCODE_API_KEY`/`OPENAI_API_KEY` **ABSENT**; prior `Model access is not configured` held; **not** re-probed `--live` this land |
| Clause-1 / hop-5 path crown | **PARTIAL_ALIVE** / **non-ALIVE** |
| Crown push | tip-wins absorb onto `cursor/ecosystem-spec-27-1-1-bc54` via `cursor/v2711-zcode-alive-oor-absorb-2230` (no force) |
| ManagePullRequest | not used |

## Honesty

**refusal ALIVE ≠ `--live` ALIVE.** This reconfirm seals the cheapest in-repo refuse courts only. Sibling Chicago `bun test test/powl-ocel-chicago.test.ts` and `bun scripts/powl-foundry-loop.ts --live` remain out of this land's promotion claim.

Parallel stall ledger `cursor/v2711-zcode-refusal-stall-bc54` (`ZCODE-POWL-REFUSAL-STANDING.md` STALLED) is superseded by this EXIT **0** observation on the assigned reconfirm branch.

## Replay

```bash
SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court --offline -- --test-threads=1
SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-graph remediation_bound --lib --offline -- --test-threads=1
```

Expect: **6 + 4 passed; exit 0**. Exact next for live crown movement: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0** ([`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md)).
