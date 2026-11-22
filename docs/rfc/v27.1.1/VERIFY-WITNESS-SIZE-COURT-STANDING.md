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

## Size honesty retip vs tip `6f920670f` (2026-11-22) — `cursor/v2711-vw-size-retip-bc54`

```text
exit_measure=0 standing=ALIVE(size_measure) bytes=28673 tip=6f920670ffaa53ecb7cbd89524fa4375e087f1f7
size_court_tip_cargo=EXIT_137/PARTIAL
env_wire=PARTIAL production_runtime=UNSUPPORTED
honesty=size_measure_ALIVE_neq_tip_cargo_PARTIAL_neq_production_ALIVE_neq_Foundry_ALIVE
side=cursor/v2711-vw-size-retip-bc54
agent=bc-6808d418-7fd9-50a2-879e-0d693c932a29
prior_tip_cargo_ALIVE=b2f6e12d2 (sources ≡ tip; not reconfirmed under thrash)
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-vw-size-retip-bc54` |
| Live tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6f920670ffaa53ecb7cbd89524fa4375e087f1f7` |
| Agent | `bc-6808d418-7fd9-50a2-879e-0d693c932a29` |
| UTC start / end | `2026-11-22T18:01:28Z` → `2026-11-22T18:01:29Z` (court kill); measure held |
| Affidavit pin ledger | [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry | **not claimed** — **never** Foundry `ALIVE` on this subject |

### Observed (this retip)

| Probe | Result |
|-------|--------|
| Affidavit gatekeeper `wc -c` | **28673** @ `/tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` (≡ `/tmp/cursor/verify-witness-prod/…`) — **MEASURE_EXIT=0** |
| SHA-256 | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Marketing / spec bound | **&lt;250KB** — treat as **&lt;256000** (affidavit ceiling) and **&lt;250000** (decimal) — **28673** holds both |
| Affidavit court bound | ≤ **256000** — held |
| Gate-1 bound | &lt; **262144** — held |
| ggen placeholder @ tip | **35** B (`720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034`) |
| Witness sources vs prior tip-cargo ALIVE `b2f6e12d2` | **unchanged** (`git diff --stat` empty on `crates/ggen-graph/{src/witness,tests/verify_witness_size_court.rs,fixtures/witness}`) |
| Tip-cargo size court (warm seed `/tmp/ggen-uilayout-exact-retip-warm` → `/tmp/ggen-vw-size-retip-final`) | **EXIT 137** (OOM-kill at start under sibling cargo thrash); log `/tmp/cursor/vw-size-retip-final.log` |
| Prior cold attempt | **EXIT 101** (oxrocksdb-sys SIGTERM) `/tmp/cursor/vw-size-retip-court.log`; flock waiter **EXIT 137** `/tmp/cursor/vw-size-retip-court2.log` |

### Standing (this retip)

| Claim | Standing |
|-------|----------|
| Production pin bytes &lt;250KB (measure) | **ALIVE** — `wc -c` **28673**; `MEASURE_EXIT=0` |
| Tip-cargo `verify_witness_size_court` this retip | **PARTIAL** — **EXIT 137**; court sources **PRESENT** (8 tests); lineage prior tip-cargo **ALIVE** @ `b2f6e12d2` held as historical, **not** reconfirmed this tip |
| Env wire / production runtime | **PARTIAL** / **UNSUPPORTED** (unchanged) |
| Foundry / hop-5 / crown | **not this subject** — **no** Foundry `ALIVE` |

```text
ALIVE(size_measure @ tip 6f920670ffaa53ecb7cbd89524fa4375e087f1f7)
  ⇔  wc -c affidavit_gatekeeper_verify_witness.wasm = 28673
  ∧  28673 < 250000 ∧ 28673 < 256000 ∧ 28673 < 262144
  ∧  MEASURE_EXIT=0
  ∧  this honesty retip cites tip + measure + sidebranch

PARTIAL(size_court_tip_cargo @ tip 6f920670ffaa53ecb7cbd89524fa4375e087f1f7)
  ⇔  tip-cargo EXIT 137 (OOM) within retip budget
  ∧  exact next = warm size court EXIT 0 (8 passed)
```

**Honesty:** measure `ALIVE` ≠ tip-cargo size-court `ALIVE` this retip ≠ ggen production runtime `ALIVE` ≠ Foundry `ALIVE`.

### Exact next verifier (tip-cargo size court)

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
# Prefer a quiet host (no competing oxrocksdb builds). Warm seed optional.
CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip-final \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 8 passed; EXIT 0 (when OOM thrash clears)
```

### Replay (measure — held)

```bash
wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 28673 ; MEASURE_EXIT=0 ; 28673 < 250000 / 256000 / 262144
sha256sum /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97
```
