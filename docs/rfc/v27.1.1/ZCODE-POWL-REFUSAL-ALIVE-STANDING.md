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
