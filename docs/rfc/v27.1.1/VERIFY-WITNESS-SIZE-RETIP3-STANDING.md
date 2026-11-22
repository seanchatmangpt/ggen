# `verify_witness` size retip3 — measure vs &lt;250KB (push-first)

```text
exit=measure_0 standing=ALIVE(size_measure) bytes=28673 tip=c2cd54f562f1fe675d9ea5fabf989b89a9f8a796
size_court_tip_cargo=TIMEOUT(170s)/PARTIAL
env_wire=PARTIAL production_runtime=UNSUPPORTED
honesty=size_measure_ALIVE_neq_tip_cargo_PARTIAL_neq_production_ALIVE_neq_Foundry_ALIVE
side=cursor/v2711-vw-size-retip3-bc54
agent=bc-cf179f93-ea03-5661-841d-7d083467b897
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-vw-size-retip3-bc54` |
| Live tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` (≈ `167352d0d+`; tip-wins Taffy retip7 PARTIAL) |
| Agent | `bc-cf179f93-ea03-5661-841d-7d083467b897` |
| UTC | `2026-11-22T17:46:03Z` |
| Prior size court ledger | [`VERIFY-WITNESS-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-SIZE-COURT-STANDING.md) |
| Affidavit pin ledger | [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry | **not claimed** |

## Observed (this retip)

| Probe | Result |
|-------|--------|
| Affidavit gatekeeper wasm `wc -c` | **28673** @ `/tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` |
| SHA-256 | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Marketing bound | **&lt;250000** (250KB) — **28673 &lt; 250000** → measure **ALIVE** |
| Affidavit court bound | ≤ **256000** — held |
| Gate-1 bound | &lt; **262144** — held |
| ggen placeholder | **35** B @ tip (`720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034`) |
| Tip-cargo size court | `cargo test -p ggen-graph --test verify_witness_size_court` → **EXIT 124** / timeout **170s** cold (`/tmp/ggen-vw-size-retip3-court`); warm retry **EXIT 124** / **90s** — thrash; log `/tmp/cursor/vw-size-retip3-court.log` |
| Status | `/tmp/cursor/vw-size-retip3-measure.status` → `AFF_BYTES=28673` |

## Standing

| Claim | Standing |
|-------|----------|
| Production pin bytes &lt;250KB (measure) | **ALIVE** — `wc -c` **28673** |
| Tip-cargo `verify_witness_size_court` this retip | **PARTIAL** — timeout under cargo thrash; court sources **PRESENT** (8 tests) unchanged vs prior ALIVE land |
| Env wire / production runtime | **PARTIAL** / **UNSUPPORTED** (unchanged) |
| Foundry / hop-5 | **not this subject** |

```text
ALIVE(size_measure @ tip c2cd54f562f1fe675d9ea5fabf989b89a9f8a796)
  ⇔  wc -c affidavit_gatekeeper_verify_witness.wasm = 28673
  ∧  28673 < 250000
  ∧  this honesty retip cites tip + measure + sidebranch

PARTIAL(size_court_tip_cargo @ tip c2cd54f562f1fe675d9ea5fabf989b89a9f8a796)
  ⇔  tip-cargo timeout within retip budget
  ∧  next verifier = warm size court EXIT 0
```

**Honesty:** measure `ALIVE` ≠ tip-cargo size-court `ALIVE` this retip ≠ ggen production runtime `ALIVE` ≠ Foundry `ALIVE`.

## Exact next verifier (tip-cargo size court)

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip3-court \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 8 passed; EXIT 0 (when thrash clears)
```

## Replay (measure — held)

```bash
wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 28673
```
