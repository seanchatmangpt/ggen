# zcode_powl_ocel + remediation_bound — retip4 push-first honesty (v27.1.1)

```text
tip=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-zcode-powl-retip4-bc54
courts=zcode_powl_ocel_court + remediation_bound
exit=NOT_RUN
standing=PARTIAL_ALIVE
lineage_alive=cd7c2433b / reconfirm 01eccfffb (6+4 EXIT 0)
live=BLOCKED_HITL
refusal_ALIVE_neq_live_ALIVE=held
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` |
| Court blob | `64c5f1bc` |
| Replay sjira | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/only-fafc690f-tip3-courts cargo test -p ggen-sjira --test zcode_powl_ocel_court` |
| Replay bound | `… cargo test -p ggen-graph remediation_bound --lib` |
| This probe | **EXIT:NOT_RUN** @ 2026-11-22T18:25:59Z — queued; thrash |
| Foundry `--live` | **BLOCKED_HITL** (held; not re-probed) |

**Authority.** No tip EXIT 0 → **PARTIAL_ALIVE**. Lineage refusal ALIVE held. refusal ALIVE ≠ live ALIVE. No Foundry path-crown ALIVE.
