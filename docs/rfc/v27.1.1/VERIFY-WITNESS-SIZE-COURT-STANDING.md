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

---

## Retip2 vs tip `2d7b47b2e+` (2026-11-22) — `cursor/v2711-vw-size-retip2-bc54`

```text
exit_measure=0 standing=ALIVE(size_measure) bytes=28673 tip=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
size_court_tip_cargo=EXIT_137/PARTIAL
env_wire=PARTIAL production_runtime=UNSUPPORTED
honesty=size_measure_ALIVE_neq_tip_cargo_PARTIAL_neq_production_ALIVE_neq_Foundry_ALIVE
side=cursor/v2711-vw-size-retip2-bc54
agent=bc-8fc145c9-27db-529d-b77b-37c0780e106a
prior_pin_note=prior_size_court_land@b2f6e12d2_may_stall_under_tip_thrash; retip2 re-measure on live tip
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-vw-size-retip2-bc54` |
| Live tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` (`2d7b47b2e`; ⊇ OOR retip17 / Truex lineage past `49ebdb27c`) |
| Agent | `bc-8fc145c9-27db-529d-b77b-37c0780e106a` |
| UTC | `2026-11-22T17:50:31Z` |
| Affidavit pin ledger | [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry | **not claimed** (never Foundry `ALIVE` on this subject) |

### Observed (this retip2)

| Probe | Result |
|-------|--------|
| Affidavit gatekeeper `wc -c` | **28673** @ `/tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` (≡ `/tmp/cursor/verify-witness-prod/…`) — **MEASURE_EXIT=0** |
| SHA-256 | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Marketing bound | **&lt;250000** (250KB) — **28673 &lt; 250000** → size measure **ALIVE** |
| Affidavit court bound | ≤ **256000** — held |
| Gate-1 bound | &lt; **262144** — held |
| ggen placeholder @ tip | **35** B (`720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034`) |
| Witness sources vs prior ALIVE land `b2f6e12d2` | **unchanged** (`git diff --stat` empty on `crates/ggen-graph/{src/witness,tests/verify_witness_size_court.rs,fixtures/witness}`) |
| Tip-cargo size court (cold exclusive) | `CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip2-court cargo test -p ggen-graph --test verify_witness_size_court` → **EXIT 137** (OOM-kill under sibling cargo thrash); log `/tmp/cursor/vw-size-retip2-court2.log` |
| Tip-cargo size court (warm reuse) | `CARGO_TARGET_DIR=/tmp/ggen-exact-tip-courts-target` → **EXIT 137** immediate; log `/tmp/cursor/vw-size-retip2-warm-target.log` |
| Prior cold attempt on older tip | **EXIT 137** (`/tmp/cursor/vw-size-retip2-court.log`) |

### Standing (this retip2)

| Claim | Standing |
|-------|----------|
| Production pin bytes &lt;250KB (measure) | **ALIVE** — `wc -c` **28673**; `MEASURE_EXIT=0` |
| Tip-cargo `verify_witness_size_court` this retip | **PARTIAL** — **EXIT 137** (OOM under thrash); court sources **PRESENT** (8 tests); lineage prior tip-cargo **ALIVE** @ `b2f6e12d2` held as historical, not reconfirmed this tip |
| Env wire / production runtime | **PARTIAL** / **UNSUPPORTED** (unchanged) |
| Foundry / hop-5 / crown | **not this subject** — **no** Foundry `ALIVE` |

```text
ALIVE(size_measure @ tip 2d7b47b2ea5eb5108f52fa46682a68a1af7e0097)
  ⇔  wc -c affidavit_gatekeeper_verify_witness.wasm = 28673
  ∧  28673 < 250000
  ∧  MEASURE_EXIT=0
  ∧  this honesty retip cites tip + measure + sidebranch

PARTIAL(size_court_tip_cargo @ tip 2d7b47b2ea5eb5108f52fa46682a68a1af7e0097)
  ⇔  tip-cargo EXIT 137 (OOM) within retip budget
  ∧  exact next = warm size court EXIT 0 (8 passed)
```

**Honesty:** measure `ALIVE` ≠ tip-cargo size-court `ALIVE` this retip ≠ ggen production runtime `ALIVE` ≠ Foundry `ALIVE`.

### Exact next verifier (tip-cargo size court)

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip2-court \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 8 passed; EXIT 0 (when OOM thrash clears)
```

### Replay (measure — held)

```bash
wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 28673 ; MEASURE_EXIT=0 ; 28673 < 250000
```
