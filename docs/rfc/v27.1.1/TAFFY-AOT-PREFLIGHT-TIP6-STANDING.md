# Taffy AOT preflight — exact-tip6 tip-cargo ALIVE (v27.1.1)

```text
tip_base=6cdb9749204b857b7ddd79aa1c63162c79ff6c80
cargo_tip=15eefb628368afcbb230d082d6ca00c286ca9d42
branch=cursor/v2711-taffy-preflight-tip6-bc54
bc=bc-262603dc-8fb9-50aa-93a9-5a3190c363b9
court=taffy_aot_preflight_court
tests=7
exit=0
standing=ALIVE (exact tip-cargo)
prior_tip6_partial=bc-c92c420b EXIT137 PARTIAL_ALIVE (superseded)
lineage_alive=19c08a3b0 / absorb cde02d1f2
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
ManagePullRequest=not_used
force_push=sidebranch_lease_only
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-TIP6-STANDING` |
| Branch | `cursor/v2711-taffy-preflight-tip6-bc54` |
| Agent | `bc-262603dc-8fb9-50aa-93a9-5a3190c363b9` |
| Tip base (push) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6cdb9749204b857b7ddd79aa1c63162c79ff6c80` |
| Cargo tip (EXIT0) | `15eefb628368afcbb230d082d6ca00c286ca9d42` — sources ≡ tip (court blob `d8923b07e`) |
| Observed at | `2026-11-22T19:11:22Z`→`19:35:55Z` (~24.5 min cold oxrocksdb) |
| Exact command | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-tip6 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` (+ `build.jobs=1` / `--test-threads=1`) |
| Result | **7 passed; EXIT 0** |
| Prior tip6 PARTIAL | `20c0ccbd5` / `bc-c92c420b` EXIT **137** — **superseded** |
| Lineage ALIVE | `19c08a3b0` retained; tip-subject tip6 now **ALIVE** |
| Log / status | `/tmp/cursor/taffy-tip6-court.log` · `/tmp/cursor/taffy-tip6.status` → `TAFFY_EXIT=0` |
| ManagePullRequest | **not used** |
| Foundry ALIVE | **not claimed** |
| Path crown / photolithography | **not** promoted |

**Authority.** Exact tip-cargo EXIT **0** promotes Gate-1 Taffy AOT preflight tip6 to **ALIVE (exact tip-cargo)**. Do **not** invent Foundry zero-HITL or full photolithography path crown `ALIVE`.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `taffy_aot_preflight_court` | `/tmp/gate1-taffy-tip6` | **7 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

---

## Compact return

```text
tip_base=6cdb9749204b857b7ddd79aa1c63162c79ff6c80
cargo_tip=15eefb628
branch=cursor/v2711-taffy-preflight-tip6-bc54
EXIT=0 standing=ALIVE (exact tip-cargo) tests=7
prior_partial=EXIT137 superseded
Foundry_ALIVE=not_claimed ManagePullRequest=not_used
```
