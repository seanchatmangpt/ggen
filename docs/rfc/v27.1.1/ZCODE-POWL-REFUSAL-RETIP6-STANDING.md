# `zcode` POWL/OCEL refusal retip6 — push-first PARTIAL

```text
exit=TIMEOUT standing=PARTIAL(bounded_refusal_retip6) tip=c2cd54f562f1fe675d9ea5fabf989b89a9f8a796
elapsed=>180s sjira=TIMEOUT bound=SKIPPED
refusal_ALIVE≠Foundry_ALIVE
side=cursor/v2711-zcode-powl-retip6-bc54
agent=bc-cf179f93-ea03-5661-841d-7d083467b897
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-zcode-powl-retip6-bc54` |
| Live tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` (≈ `167352d0d+`) |
| Agent | `bc-cf179f93-ea03-5661-841d-7d083467b897` |
| UTC | `2026-11-22T17:46:03Z` |
| Prior ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ `cd7c2433b` |
| Reconfirm | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` **PRESENT** @ tip |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests **PRESENT** @ tip |
| Tip-cargo (this retip) | cold `/tmp/ggen-zcode-powl-retip6-court` under host thrash → **TIMEOUT &gt;180s**; sjira still compiling (`ring`/`oxjsonld`); bound **SKIPPED** |
| Status | `/tmp/cursor/zcode-powl-retip6-court.status` → `EXIT_sjira:TIMEOUT EXIT_bound:SKIPPED` |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry `--live` / `--dry-run` | **not claimed** — refusal path only; **refusal ALIVE ≠ Foundry ALIVE** |

## Standing

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

Foundry `--live` remains out of scope until provider creds; see [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).
