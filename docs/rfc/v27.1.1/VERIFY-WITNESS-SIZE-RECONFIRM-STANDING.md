# `verify_witness` size reconfirm — cheap `wc -c` (&lt;250KB)

```text
exit=measure_0 standing=ALIVE(size_measure) bytes=28673 tip=5203efd759f31ae2a2f5680e9ae33a93a0b1c532
cargo=NOT_RUN (cheap reconfirm — no cargo)
honesty=size_measure_ALIVE_neq_tip_cargo_ALIVE_neq_production_ALIVE_neq_Foundry_ALIVE
side=cursor/v2711-vw-size-reconfirm-bc54
agent=bc-d90bd1f7-6269-5bf7-a89c-70e67614e43d
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-vw-size-reconfirm-bc54` |
| Crown tip (fetch base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5203efd759f31ae2a2f5680e9ae33a93a0b1c532` (live tip; last-known cite `4353560e9…` superseded on fetch) |
| Agent | `bc-d90bd1f7-6269-5bf7-a89c-70e67614e43d` |
| UTC | `2026-11-22T18:29:36Z` |
| Prior measure cite | **28673** ([`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md), [`OPS-SURFACES-ON2-STANDING.md`](./OPS-SURFACES-ON2-STANDING.md)) |
| Affidavit pin ledger | [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| Cargo | **not run** (task: cheap reconfirm only) |
| Foundry | **not claimed** |

## Observed (this reconfirm)

| Probe | Result |
|-------|--------|
| Artifact present | **YES** — `/tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` |
| Mirror present | **YES** — `/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm` |
| `wc -c` (canonical path) | **28673** |
| SHA-256 | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Marketing bound | **&lt;250000** (250KB) — **28673 &lt; 250000** → measure **ALIVE** |
| Prior cite match | **28673** ≡ prior retip3 / ops cite |

## Standing

| Claim | Standing |
|-------|----------|
| Production pin bytes &lt;250KB (measure) | **ALIVE** — file present ∧ `wc -c` **28673** &lt; **250000** |
| Tip-cargo `verify_witness_size_court` | **not this subject** (no cargo) |
| Env wire / production runtime | unchanged — not re-probed |
| Foundry / hop-5 | **not this subject** |

```text
ALIVE(size_measure @ tip 5203efd759f31ae2a2f5680e9ae33a93a0b1c532)
  ⇔  file present: affidavit_gatekeeper_verify_witness.wasm
  ∧  wc -c = 28673
  ∧  28673 < 250000
  ∧  this honesty reconfirm cites tip + exact bytes + sidebranch
```

**Honesty:** measure `ALIVE` ≠ tip-cargo size-court `ALIVE` ≠ ggen production runtime `ALIVE` ≠ Foundry `ALIVE`. Tip-wins absorb of this ledger does **not** invent Foundry `ALIVE`.

## Exact next verifier

Warm tip-cargo size court (when thrash clears; not run this land):

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-vw-size-reconfirm-court \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 8 passed; EXIT 0
```

## Replay (measure — held this land)

```bash
wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 28673
sha256sum /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
# expect: 5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97
```
