# `verify_witness` size court standing (tip-cargo ALIVE)

```text
exit=0 standing=ALIVE(size_court) passed=6 tip_cargo_subject=f5f44571f9a331d46e2895657621b7efd7127f1b
absorb_base=9173c65296cd8db544f6ae3544f3531a4bb9399c
lineage=6a3a623bacd33efdc1ec7efccf6a35141662ca53
prior_reconfirm=c91b225614a3ca0472303c05bab25601db1d51e1
production=UNSUPPORTED
honesty=size_court_ALIVE_neq_production_WASM_ALIVE
side=cursor/v2711-verify-witness-prod-bc54@6c2a9dc4c
absorb=cursor/v2711-verify-witness-prod-absorb-55c2
```

| Field | Value |
|-------|-------|
| Branch (this honesty surface) | `cursor/v2711-verify-witness-prod-absorb-55c2` (side `cursor/v2711-verify-witness-prod-bc54` @ `6c2a9dc4c`) |
| Ecosystem tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `9173c65296cd8db544f6ae3544f3531a4bb9399c` (Smalt GA residual tip-wins on `f5f44571f`; OOR retip11 + CASTLE held) |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **6** `#[test]` **PRESENT** |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`court_verify_witness_size`, budget `262_144`) |
| Placeholder | `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`< 262144` / &lt;250KB); SHA-256 `720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034` |
| Tip-cargo (subject) | subject `f5f44571f` — warm target `/tmp/ggen-verify-witness-tip-ebf4` → **6 passed; EXIT=0** (`/tmp/cursor/verify-witness-prod-size-court-side.log`); witness sources ≡ through absorb base `9173c6529` (Smalt docs-only; **no cargo** this absorb) |
| Tip-cargo (prior reconfirm) | subject `c91b22561` — **6 passed; EXIT=0** (`/tmp/cursor/verify-witness-tip-c91b-reconfirm.log`) |
| Tip-cargo (cold) | subject `cd7c2433b` — **6 passed; EXIT=0** (`/tmp/cursor/verify-witness-tip-cd7c-build.log`) |
| Ancestor cargo | `0478fde59` — **6 passed; exit 0** (retained) |
| Standing (size court) | **ALIVE** (exact tip-cargo on `f5f44571f`; size court only; ≠ production WASM `ALIVE`) |
| Production WASM | **UNSUPPORTED** in ggen — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). Sibling affidavit pin observed **28673** B remotely ≠ ggen production `ALIVE`. |
| ManagePullRequest | not used |
| Crown push | tip-wins absorb onto `cursor/ecosystem-spec-27-1-1-bc54` after Smalt land |

## Replay

```bash
set -o pipefail
cd "$(git rev-parse --show-toplevel)"   # @ origin/cursor/ecosystem-spec-27-1-1-bc54 tip
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-tip-ebf4 \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Expect: **6 passed; exit 0** on tip witness sources (production path remains **UNSUPPORTED**).

## Exact next verifier for production (crown absorb of prod claim)

Owner: **affidavit** (`seanchatmangpt/affidavit`). See
[`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md):

```bash
# mounted affidavit @ 563eb322a (or tip retaining pin)
just gatekeeper-wasm-size-court
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm  # pin 28673
```

ggen crown absorb of **this honesty doc surface** does not require that
verifier; absorb only records size-court `ALIVE` + production `UNSUPPORTED`.
