# verify_witness CI-wasm consume — G2-11 land (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `VERIFY-WITNESS-CI-WASM-CONSUME-STANDING-27.1.1` |
| Branch | `cursor/v2711-vw-ci-consume-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §10; OOR **G2-11** |
| Companions | [`VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md`](./VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md) §3; [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md); [`VERIFY-WITNESS-CI-WASM-RETIP3-STANDING.md`](./VERIFY-WITNESS-CI-WASM-RETIP3-STANDING.md) |
| Crown base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `08941960b16f2445134964c526fb125d49835b29` |
| Pin fixture | `crates/ggen-graph/fixtures/witness/affidavit_gatekeeper_verify_witness.pin.wasm` |
| Pin bytes / SHA | **28673** / `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Affidavit tip | `563eb322a16d805ebc99b2c16f1165462244d7e2` |
| CI workflow | `.github/workflows/verify-witness-ci-wasm.yml` |
| Status | LAND — mount + court + workflow **PRESENT**; production-runtime ledger advance only after CI job EXIT **0** on this branch. API standing remains `PartialAlive` + `ExternalPath` (CONSTRUCT). **Never invent Foundry ALIVE.** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

## 1. What landed

| Claim | Standing | Evidence |
|-------|----------|----------|
| Affidavit pin bytes mounted in-repo (CI fixture) | **PRESENT** | pin wasm + [`AFFIDAVIT_PIN.md`](../../../crates/ggen-graph/fixtures/witness/AFFIDAVIT_PIN.md) |
| Pin identity court (size + SHA-256) | **encoded** | `affidavit_pin_fixture_matches_size_and_sha256` |
| Production consume via explicit path | **encoded** | `affidavit_pin_production_consume_is_partial_alive_external` |
| Production consume via `GGEN_VERIFY_WITNESS_WASM` | **encoded** | `env_ggen_verify_witness_wasm_courts_affidavit_pin_as_partial_external` |
| CI workflow Step A + Step C | **PRESENT** | `verify-witness-ci-wasm.yml` |
| ggen production runtime (ledger G2-11) | **PENDING_CI** until workflow EXIT **0** | then may promote from **UNSUPPORTED** → **ALIVE** (ledger) while API stays `PartialAlive` |
| Foundry / path crown / cos / revops | **unchanged** | Foundry **BLOCKED_HITL**; cos **503**; revops **NXDOMAIN** |

## 2. Exact verifier

```bash
PIN=crates/ggen-graph/fixtures/witness/affidavit_gatekeeper_verify_witness.pin.wasm
test -f "$PIN"
test "$(wc -c < "$PIN" | tr -d ' ')" = "28673"
test "$(sha256sum "$PIN" | awk '{print $1}')" = "5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97"
export GGEN_VERIFY_WITNESS_WASM="$(pwd)/$PIN"
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-verify-witness-ci-wasm \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Expect: **11** tests passed; EXIT **0**.

## 3. Honesty law (held)

`size_court_ALIVE` ≠ `env_wire_PARTIAL` ≠ `affidavit_pin_ALIVE` ≠ `CI_mount_PRESENT` ≠ `production_runtime_ALIVE`.

Placeholder 35 B / unset-env Unsupported / docs alone do **not** close G2-11.
CI consume EXIT **0** on the real pin closes the ledger production-runtime row.

**Refuse:** inventing Foundry `ALIVE`, path-crown `ALIVE`, or Plane-4 DO from this CONSTRUCT land.
