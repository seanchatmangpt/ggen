# `verify_witness` production path (out of ggen)

Companion to `ECOSYSTEM-SPEC-27.1.1` §10 and Gate-1 gap matrix row
`verify_witness WASM < 250KB`.

```text
base=origin/cursor/ecosystem-spec-27-1-1-bc54@1dbf1cc2e5a779cbaaaae6ad2075809e5c53be79
lineage_tip=6a3a623bacd33efdc1ec7efccf6a35141662ca53
side_prod=cursor/v2711-verify-witness-prod-bc54@6c2a9dc4c
side_affidavit=cursor/v2711-vw-affidavit-court-bc54@cc0ea07e4
size_court=ALIVE (6 passed; EXIT=0 @ f5f44571f; warm /tmp/ggen-verify-witness-tip-ebf4; witness sources ≡ through Smalt absorb 9173c6529)
affidavit_size_court=ALIVE (just gatekeeper-wasm-size-court EXIT=0; wc -c 28673 @ 563eb322a; bc-5f5786ff)
production_in_ggen=UNSUPPORTED
affidavit_checkout=ABSENT (this worktree); probe mount ALIVE elsewhere
wasm32_target=ABSENT (rustup installed targets: x86_64-unknown-linux-gnu only)
GGEN_VERIFY_WITNESS_WASM=unset
honesty=affidavit_ALIVE_neq_ggen_production_ALIVE
```

## Ownership

| Concern | Owner | Standing claim allowed from ggen |
|---------|-------|----------------------------------|
| Size court (`< 262_144` bytes) | **ggen** (`ggen_graph::witness`) | `ALIVE` (size court only) / `PARTIAL_ALIVE` when encoding gate without tip-cargo |
| Production WASM build / runtime | **affidavit** (`affidavit-gatekeeper-wasm`) | affidavit pin size court **ALIVE**; ggen consume remains `UNSUPPORTED` until `GGEN_VERIFY_WITNESS_WASM` wired |
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

Reconfirm @ tip-cargo subject `f5f44571f` (lineage `6a3a623ba`; absorb base
`9173c6529` after Smalt tip-wins — witness sources ≡; **no cargo** this absorb):
**6 passed; EXIT=0** (log `/tmp/cursor/verify-witness-prod-size-court-side.log`).

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
- ggen Gate-1 budget: **&lt; 262144**. Pin **28673** clears both.
- Inventory: affidavit `docs/GATEKEEPER_VERIFY_WITNESS_WASM_INVENTORY.md`.
- Build: `just gatekeeper-wasm-build` → `./scripts/gatekeeper_wasm_build.sh`.
- Probe replay: `just gatekeeper-wasm-size-court` → EXIT **0**; `wc -c` → **28673**.

Prior remote byte download to
`/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm`
was observation only. The clause-3 probe closed the mounted size court; that
still **does not** promote ggen production standing to `ALIVE`.

## Exact next verifier (ggen wire — open)

Affidavit pin size court is **ALIVE**. Remaining hand-off is ggen consume
(still CONSTRUCT; never DO):

```bash
export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
# then call ggen_graph::witness::verify_production_witness(None)
# OverBudget → WitnessRefusal::OverBudget; missing path → Unsupported
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

| Claim | Standing @ absorb base `1dbf1cc2e` (tip-cargo subject `f5f44571f`) |
|-------|------------------------------------------|
| ggen placeholder size court | **ALIVE** (tip-cargo **6** @ `f5f44571f`; prior `c91b22561`; sources ≡ `9173c6529`) |
| In-repo production WASM build path | **UNSUPPORTED** (no `verify_witness` workspace member; no wasm32 target) |
| Affidavit production pin size court | **ALIVE** @ `563eb322a` (EXIT **0**; **28673** B) — [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) |
| ggen `GGEN_VERIFY_WITNESS_WASM` wire / production consume | **UNSUPPORTED** / **PARTIAL** until exact next verifier above courts on tip |

- Never promote inspection of this markdown file to `ALIVE`.
- Never cite fat `affidavit-wasm` (718736 B) as the &lt;250KB gatekeeper.
- Tip-wins absorb of the affidavit size-court ledger (onto tip after Smalt /
  OOR / CASTLE / VW-prod / BRCE-V8) does **not** invent ggen production `ALIVE`;
  ggen wire remains open.
