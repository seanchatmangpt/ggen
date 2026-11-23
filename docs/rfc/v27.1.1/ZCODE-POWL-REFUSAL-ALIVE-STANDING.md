# `zcode` POWL/OCEL bounded-refusal tip-cargo ALIVE

```text
exit=0 standing=ALIVE(bounded_refusal) sjira=6/6 bound=4/4 tip=cd7c2433b7e124db6606008a92081d23aa3727ce
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-zcode-powl-refusal-alive-bc54` |
| Ecosystem tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `cd7c2433b7e124db6606008a92081d23aa3727ce` (court sources ≡ prior tip `f1492ed55`) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests (`REFUSED_UNBOUNDED_REMEDIATION` / `RETRY_BUDGET_EXHAUSTED` / write-scope) |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-refusal-sjira-retip.log`, `/tmp/cursor/zcode-powl-refusal-bound-retip.log` |
| Standing (bounded refusal) | **ALIVE** (in-repo tip-cargo refuse path only) |
| Foundry `--dry-run` | **PARTIAL_ALIVE** — mounted zcode `f32df08`; `bun scripts/powl-foundry-loop.ts --dry-run` → **exit 2** / aggregate `PARTIAL_ALIVE` (`/tmp/cursor/zcode-powl-refusal-dryrun.log`) — not dry-run `ALIVE` |
| Foundry `--live` | **BLOCKED_HITL** — `Model access is not configured`; never invent live crown `ALIVE` |
| Ledger | [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) |
| ManagePullRequest | not used |

## Replay

```bash
SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court
SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-graph remediation_bound --lib
```

Expect: **6 + 4 passed; exit 0**. Hop-5 crown / foundry `--live` remain non-ALIVE until provider creds + `--live` → **0**.

## Reconfirm (2026-11-22)

Photolithography clause 1 / hop 5 refuse-path reconfirm on tip `df9597c18` (`cursor/v2711-zcode-refusal-reconfirm-bc54`): sjira **6** + bound **4**; both **exit 0** under exclusive `/home/ubuntu/zcode-powl-refusal-target`. Court sources unchanged since `cd7c2433b`. Standing remains **ALIVE(bounded_refusal)** only — `--live` still **BLOCKED_HITL**. Ledger: [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md). **refusal ALIVE ≠ `--live` ALIVE.** Tip-wins absorb via `cursor/v2711-zcode-alive-oor-absorb-2230`.

## Path honesty cross-pin (2026-11-22)

Lineage **ALIVE** above remains **held** through crown tip `83c954e5f` (`e24ce7a73+`). Tip-cargo honesty on that crown is separately **PARTIAL** (EXIT **137** @ retip6) — do **not** demote this lineage land, and do **not** invent tip EXIT 0 / Foundry `ALIVE` from it. Hop-5 autonomic remediations slice **PARTIAL**; `--live` **BLOCKED_HITL**. Path pin: [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) (`cursor/v2711-zcode-path-retip-bc54` / `bc-3708a779`). Exact next tip-cargo → **6+4**/EXIT:**0**; founder `--live` → `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **0**.
