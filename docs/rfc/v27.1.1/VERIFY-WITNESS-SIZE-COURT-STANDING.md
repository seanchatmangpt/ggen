# `verify_witness` size court standing (tip-cargo ALIVE)

```text
exit=0 standing=ALIVE(size_court) passed=8 tip_cargo_subject=b2f6e12d2e9cf74dcb0dcbb2b02d3526c0ae9e7d
absorb_base=f654343155c902a003a852f61df345e986d98b5e
lineage=f5f44571f9a331d46e2895657621b7efd7127f1b
prior_reconfirm=c91b225614a3ca0472303c05bab25601db1d51e1
env_wire=PARTIAL
production_runtime=UNSUPPORTED
honesty=size_court_ALIVE_neq_env_wire_PARTIAL_neq_production_WASM_ALIVE
side=cursor/v2711-vw-env-wire-bc54@b2f6e12d2
absorb=tip-wins code @f65434315 + honesty retip onto cursor/ecosystem-spec-27-1-1-bc54
```

| Field | Value |
|-------|-------|
| Branch (this honesty surface) | tip-wins absorb of `cursor/v2711-vw-env-wire-bc54` @ `b2f6e12d2` |
| Ecosystem tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f65434315` (code tip-wins onto `098f53ea8`; past CASTLE/WebAuthn) |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **8** `#[test]` **PRESENT** (env wire + affidavit ceiling) |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES=256_000`; Gate-1 `262_144`) |
| Placeholder | `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`≪ 256000` / `262144`); SHA-256 `720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034` |
| Tip-cargo (env-wire side) | subject `b2f6e12d2` — warm `/tmp/ggen-verify-witness-env-wire` → **8 passed; EXIT=0** (`/tmp/cursor/vw-env-wire-size-court.log`) |
| Tip-cargo (prior size-only) | subject `f5f44571f` — **6 passed; EXIT=0** (`/tmp/cursor/verify-witness-prod-size-court-side.log`); prior `c91b22561` / cold `cd7c2433b` |
| Standing (size court) | **ALIVE** (size court only; ≠ production WASM `ALIVE`) |
| Env wire (`GGEN_VERIFY_WITNESS_WASM`) | **PARTIAL** — code + Chicago courts; CI env unset → typed `UNSUPPORTED` for production path |
| Production WASM runtime | **UNSUPPORTED** in ggen CI — no CI-mounted real wasm; [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). Affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`) — [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| ManagePullRequest | not used |
| Crown push | tip-wins absorb onto `cursor/ecosystem-spec-27-1-1-bc54` (no force) |

## Replay

```bash
set -o pipefail
cd "$(git rev-parse --show-toplevel)"   # @ origin/cursor/ecosystem-spec-27-1-1-bc54 tip
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-env-wire \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Expect: **8 passed; exit 0** on tip witness sources (env wire **PARTIAL**; production runtime remains **UNSUPPORTED**).

## Exact next verifier for production ALIVE (still open)

Env wire is **PARTIAL**. Affidavit pin size court is **ALIVE** (EXIT **0**; **28673** B @ `563eb322a`) —
[`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md).
Production runtime `ALIVE` still requires a **CI-mounted** real wasm + consume EXIT 0:

```bash
export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
# ggen_graph::witness::verify_production_witness(None) → PartialAlive (CONSTRUCT; never DO)
# CI job that sets the env + runs the court → still required for production ALIVE
```

ggen size-court `ALIVE` + env-wire `PARTIAL` + affidavit pin `ALIVE` ≠ ggen production
runtime `ALIVE`.
