# `verify_witness` production path (out of ggen)

Companion to `ECOSYSTEM-SPEC-27.1.1` §10 and Gate-1 gap matrix row
`verify_witness WASM < 250KB`.

```text
base=origin/cursor/ecosystem-spec-27-1-1-bc54@f5f44571f9a331d46e2895657621b7efd7127f1b
lineage_tip=6a3a623bacd33efdc1ec7efccf6a35141662ca53
side=cursor/v2711-verify-witness-prod-bc54
size_court=ALIVE (6 passed; EXIT=0 @ f5f44571f; warm /tmp/ggen-verify-witness-tip-ebf4)
production_in_ggen=UNSUPPORTED
affidavit_checkout=ABSENT
wasm32_target=ABSENT (rustup installed targets: x86_64-unknown-linux-gnu only)
GGEN_VERIFY_WITNESS_WASM=unset
```

## Ownership

| Concern | Owner | Standing claim allowed from ggen |
|---------|-------|----------------------------------|
| Size court (`< 262_144` bytes) | **ggen** (`ggen_graph::witness`) | `ALIVE` (size court only) / `PARTIAL_ALIVE` when encoding gate without tip-cargo |
| Production WASM build / runtime | **affidavit** (`affidavit-gatekeeper-wasm`) | **not** `ALIVE` from ggen alone — remains `UNSUPPORTED` until mounted hand-off courts |
| Pack emit of artifact bytes | optional ggen pack | `CONSTRUCT` only |

ggen **does not host** the production `verify_witness` runtime. Plane 4 verify
is CONSTRUCT; DO remains behind BRCE+AS-01 on the owning plane.

**Honesty law:** size-court `ALIVE` ≠ production WASM `ALIVE`. Placeholder
fixture green never promotes a production-artifact claim.

## In-repo stub (this repository) — measured 2026-11-22

| Path | Role | Bytes |
|------|------|------:|
| `crates/ggen-graph/src/witness/mod.rs` | API: `verify_witness`, `court_verify_witness_size`, typed refusals | — |
| `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` | Minimal WASM magic stub for the size court | **35** |
| `crates/ggen-graph/tests/verify_witness_size_court.rs` | Chicago size court (**6** tests) | — |

Placeholder SHA-256:
`720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034`
(magic `\0asm` + version; **≪** `262_144`).

Replay (size court only):

```bash
set -o pipefail
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-tip-ebf4 \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Reconfirm @ crown `f5f44571f` (lineage `6a3a623ba`): **6 passed; EXIT=0**
(log `/tmp/cursor/verify-witness-prod-size-court-side.log`).

## Out-of-repo production SoT (affidavit) — observed, not absorbed

Production crate / recipe live in **`seanchatmangpt/affidavit`** (ABSENT this
worktree). Observed read-only against tip `563eb322a16d805ebc99b2c16f1165462244d7e2`:

| Artifact | Role | Bytes | SHA-256 |
|----------|------|------:|---------|
| `affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` | Slim gatekeeper (`verify_witness` export) | **28673** | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| `affidavit-wasm/dist/affidavit_wasm.wasm` | Fat WASI provenance ABI | **718736** | (not gatekeeper — do not cite as &lt;250KB product) |

- Affidavit size court bound: **≤ 256000** (fail-closed in
  `affidavit-gatekeeper-wasm/tests/size_court.rs`).
- ggen Gate-1 budget: **&lt; 262144**. Pin **28673** clears both.
- Inventory: affidavit `docs/GATEKEEPER_VERIFY_WITNESS_WASM_INVENTORY.md`.
- Build: `just gatekeeper-wasm-build` → `./scripts/gatekeeper_wasm_build.sh`.

Remote bytes were downloaded to
`/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm`
for size/sha observation only. That probe is **not** a mounted affidavit
checkout, **not** an in-ggen production build path, and **does not** promote
ggen production standing to `ALIVE`.

## Exact next verifier (owner: affidavit → ggen hand-off)

In a **mounted** `seanchatmangpt/affidavit` checkout @ `563eb322a` (or later tip
that retains the pin):

```bash
cd "$(git rev-parse --show-toplevel)"   # affidavit repo
just gatekeeper-wasm-size-court
# expect EXIT=0 and measured bytes ≤ 256000 (pin 28673)
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
sha256sum affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
```

Optional ggen consume (still CONSTRUCT; never DO):

```bash
export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
# then call ggen_graph::witness::verify_production_witness(None)
# OverBudget → WitnessRefusal::OverBudget; missing path → Unsupported
```

Manual rebuild equivalent (when pin must be regenerated):

```bash
rustup target add wasm32-unknown-unknown
cd affidavit-gatekeeper-wasm
cargo build --locked --lib --target wasm32-unknown-unknown --profile wasm
# optional: wasm-opt -Oz --enable-bulk-memory -o dist/...
```

## Standing discipline

| Claim | Standing @ `f5f44571f` / this side branch |
|-------|------------------------------------------|
| ggen placeholder size court | **ALIVE** (tip-cargo **6** @ `f5f44571f`; prior `c91b22561`) |
| In-repo production WASM build path | **UNSUPPORTED** (no `verify_witness` workspace member; no wasm32 target) |
| Affidavit slim pin observed remotely | observation only — **≠** ggen production `ALIVE` |
| Production WASM courted via mounted hand-off | **UNSUPPORTED** until next verifier above exits **0** in mounted affidavit + optional ggen consume |

- Never promote inspection of this markdown file to `ALIVE`.
- Never cite fat `affidavit-wasm` (718736 B) as the &lt;250KB gatekeeper.
- Crown absorb of this honesty surface does not race tip-cargo fences; production
  remains out-of-repo until the exact next verifier closes.
