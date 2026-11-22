# `verify_witness` production path (out of ggen)

Companion to `ECOSYSTEM-SPEC-27.1.1` §10 and Gate-1 gap matrix row
`verify_witness WASM < 250KB`.

```text
base=origin/cursor/ecosystem-spec-27-1-1-bc54@a7e190400a354a4f624ab41d59a204f6118d1eed
side=cursor/v2711-vw-env-wire-bc54
side_prod=cursor/v2711-verify-witness-prod-bc54@6c2a9dc4c
side_affidavit=cursor/v2711-vw-affidavit-court-bc54@cc0ea07e4
size_court=ALIVE (prior tip-cargo 6 @ f5f44571f; court extended to 8 for env wire)
env_wire=PARTIAL (code PRESENT + Chicago courts; env unset in CI)
affidavit_size_court=ALIVE (just gatekeeper-wasm-size-court EXIT=0; wc -c 28673 @ 563eb322a; bc-5f5786ff)
production_runtime_ALIVE=UNSUPPORTED (no CI-mounted real wasm)
affidavit_checkout=ABSENT (this worktree); probe mount ALIVE elsewhere
wasm32_target=ABSENT (rustup installed targets: x86_64-unknown-linux-gnu only)
GGEN_VERIFY_WITNESS_WASM=unset (CI default)
honesty=env_wire_PARTIAL_neq_production_ALIVE
```

## Ownership

| Concern | Owner | Standing claim allowed from ggen |
|---------|-------|----------------------------------|
| Size court (`< 262_144` bytes) | **ggen** (`ggen_graph::witness`) | `ALIVE` (size court only) / `PARTIAL_ALIVE` when encoding gate without tip-cargo |
| Env path override (`GGEN_VERIFY_WITNESS_WASM`) | **ggen** (`load_production_verify_witness` / `verify_production_witness`) | `PARTIAL` when env/path courts under `< 256_000`; typed `UNSUPPORTED` when unset |
| Production WASM build / runtime | **affidavit** (`affidavit-gatekeeper-wasm`) | affidavit pin size court **ALIVE**; ggen production runtime remains `UNSUPPORTED` without CI-mounted real wasm + consume EXIT 0 |
| Pack emit of artifact bytes | optional ggen pack | `CONSTRUCT` only |

ggen **does not host** the production `verify_witness` runtime. Plane 4 verify
is CONSTRUCT; DO remains behind BRCE+AS-01 on the owning plane.

**Honesty law:** size-court `ALIVE` ≠ env-wire `PARTIAL` ≠ production WASM `ALIVE`.
Placeholder fixture green never promotes a production-artifact claim.

## Env wire (ggen) — PRESENT / PARTIAL

Loader was already present; this sidebranch tightens production consume with the
affidavit ceiling and Chicago env courts:

| Symbol | Role |
|--------|------|
| `GGEN_VERIFY_WITNESS_WASM` | Absolute path override (or explicit `path` arg) |
| `AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES` | **256_000** — production hand-off refuse at `>=` |
| `VERIFY_WITNESS_SIZE_BUDGET_BYTES` | **262_144** — Gate-1 size court |
| `verify_production_witness(None)` | Load env → refuse over affidavit budget → Gate-1 verify |

Unset env → typed `WitnessRefusal::Unsupported` (production path still
`UNSUPPORTED` in CI). Set env to under-budget wasm → `PartialAlive` +
`ArtifactProvenance::ExternalPath` — **PARTIAL**, not production `ALIVE`.

## In-repo stub (this repository) — measured 2026-11-22

| Path | Role | Bytes |
|------|------|------:|
| `crates/ggen-graph/src/witness/mod.rs` | API: verify + env wire + dual size ceilings | — |
| `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` | Minimal WASM magic stub for the size court | **35** |
| `crates/ggen-graph/tests/verify_witness_size_court.rs` | Chicago size + env-wire court (**8** tests) | — |

Placeholder SHA-256:
`720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034`
(magic `\0asm` + version; **≪** `256_000` / `262_144`).

Replay (size court + env wire):

```bash
set -o pipefail
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-env-wire \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Prior tip-cargo size court (**6** @ `f5f44571f`) remains lineage; this sidebranch
extends to **8** (env wire + affidavit ceiling).

## Out-of-repo production SoT (affidavit) — pin size court ALIVE

Production crate / recipe live in **`seanchatmangpt/affidavit`** (ABSENT this
worktree). Pin `563eb322a16d805ebc99b2c16f1165462244d7e2` size court **ALIVE**
via probe `bc-5f5786ff-6189-5989-82ba-0a4087074e65` — ledger
[`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md).

| Artifact | Role | Bytes | SHA-256 |
|----------|------|------:|---------|
| `affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` | Slim gatekeeper (`verify_witness` export) | **28673** | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| `affidavit-wasm/dist/affidavit_wasm.wasm` | Fat WASI provenance ABI | **718736** | (not gatekeeper — do not cite as &lt;250KB product) |

- Affidavit size court bound: **≤ 256000** (fail-closed in
  `affidavit-gatekeeper-wasm/tests/size_court.rs`).
- ggen production consume: **`< 256_000`** then Gate-1 `< 262_144`. Pin **28673** clears both.
- Inventory: affidavit `docs/GATEKEEPER_VERIFY_WITNESS_WASM_INVENTORY.md`.
- Build: `just gatekeeper-wasm-build` → `./scripts/gatekeeper_wasm_build.sh`.
- Probe replay: `just gatekeeper-wasm-size-court` → EXIT **0**; `wc -c` → **28673**.

Prior remote byte download to
`/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm`
was observation only. The clause-3 probe closed the mounted size court; that
still **does not** promote ggen production standing to `ALIVE`.

## Exact next verifier (production ALIVE — still open)

Env wire is **PARTIAL**. Affidavit pin size court is **ALIVE**. Production
runtime `ALIVE` still requires a **CI-mounted** real wasm + consume EXIT 0:

```bash
# mounted affidavit @ 563eb322a (or later tip retaining pin)
just gatekeeper-wasm-size-court   # expect EXIT=0; wc -c → 28673
export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
# then: ggen_graph::witness::verify_production_witness(None) → PartialAlive
# CI job that sets the env + runs the court → still required for production ALIVE
```

Affidavit pin replay (already EXIT **0** @ `563eb322a`):

```bash
cd "$(git rev-parse --show-toplevel)"   # mounted affidavit repo
just gatekeeper-wasm-size-court
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
sha256sum affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
```

Manual rebuild equivalent (when pin must be regenerated):

```bash
rustup target add wasm32-unknown-unknown
cd affidavit-gatekeeper-wasm
cargo build --locked --lib --target wasm32-unknown-unknown --profile wasm
# optional: wasm-opt -Oz --enable-bulk-memory -o dist/...
```

## Standing discipline

| Claim | Standing |
|-------|----------|
| ggen placeholder size court | **ALIVE** (prior tip-cargo **6** @ `f5f44571f`; sidebranch **8**) |
| ggen `GGEN_VERIFY_WITNESS_WASM` env wire + `<256000` bound | **PARTIAL** (code + Chicago courts on this sidebranch) |
| In-repo production WASM build path | **UNSUPPORTED** (no `verify_witness` workspace member; no wasm32 target) |
| Affidavit production pin size court | **ALIVE** @ `563eb322a` (EXIT **0**; **28673** B) — [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| Production runtime in ggen CI | **UNSUPPORTED** until CI mounts real wasm + consume EXIT 0 |

- Never promote inspection of this markdown file to `ALIVE`.
- Never cite fat `affidavit-wasm` (718736 B) as the &lt;250KB gatekeeper.
- Sidebranch only — tip-wins absorb of honesty must not invent production `ALIVE`.
