# Hop-5 mix SUBJECT_MISSING — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP5-MIX-SUBJECT-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop5-mix-subject-bc54` |
| Companion | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5, [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) |
| zcode-cli land | `cursor/v2711-hop5-mix-subject-bc54` @ `fed6f166aac25a0be4b715b6c31b812ea14b5a27` |
| Status | LEDGER (not path-crown `ALIVE`) |

## Diagnosis

Default `UiLayoutCourtAdapter.verifyPostTransition` on ggen tip after #884:

1. `DEFAULT_TAFFY_CMD` → `--bin layout_court` + `fixtures/ui-layout-court/product-subject.json`
2. `resolveTaffySubject` → built `target/debug/layout_court` **PRESENT** → Taffy **PASS**
3. Mode B self-contained spatial court → **PASS**
4. `requireCleanRoom: true` → `mix ggen_igniter.sync --check`
5. `mix` **ABSENT** → historically exit **2** `SUBJECT_MISSING` (conflated Jobs/C4 absence with layout subject absence)

Antifalsifier retained: `requireCleanRoom:false` alone ≠ proof of default-adapter Jobs/C4 `ALIVE`.

## Fix (zcode-cli `resolveCleanRoomSyncCmd`)

After spatial PASS:

| Condition | Result |
|-----------|--------|
| `mix` resolves | run `mix ggen_igniter.sync --check` |
| `mix` ABSENT, `ggen` resolves | fall back to `ggen sync run --dry-run` (`planned_writes:[]`) |
| both ABSENT **and** `layout_court` + `product-subject.json` **PRESENT** | **spatial-only exit 0** + typed Jobs/C4 deferred diagnostic |
| both ABSENT, layout subject **ABSENT** | exit **2** `SUBJECT_MISSING` + exact remediation |

Exact remediation (typed SUBJECT_MISSING when subject truly missing):

1. install Elixir and put `mix` on PATH in an igniter workspace, then `mix ggen_igniter.sync --check`
2. OR set `cleanRoomSyncCmd: ["ggen","sync","run","--dry-run"]` with `ggen` on PATH
3. OR set `requireCleanRoom:false` for spatial-only verify (does **not** claim Jobs/C4 ALIVE)

## Evidence (this land)

| Probe | Exit | Standing |
|-------|------|----------|
| `layout_court` + `fixtures/ui-layout-court/product-subject.json` | **0** | ALIVE (product_subject) |
| default `UiLayoutCourtAdapter.verifyPostTransition(/workspace)` (`mix`/`ggen` ABSENT; layout_court PRESENT) | **0** | ALIVE + Jobs/C4 deferred diagnostic |
| `bun test test/ui-layout-court-chicago.test.ts` (zcode-cli) | **0** | **31 passed** (incl. mix-ABSENT / layout_court PRESENT / ggen fallback / typed remediation) |

## Standing

| Claim | Standing |
|-------|----------|
| Live spatial adapter on ggen tip with layout_court PRESENT | **ALIVE** (exit **0**) |
| Jobs/C4 / igniter `mix` / foundry `--live` | **PARTIAL** / open (`mix` ABSENT) |
| Hop-5 aggregate / path crown | **PARTIAL_ALIVE** — do **not** invent hop-5 crown `ALIVE` from adapter alone |

## Replay

```bash
# ggen tip
test -f crates/ggen-layout-court/src/bin/layout_court.rs
test -f fixtures/ui-layout-court/product-subject.json
cargo run -p ggen-layout-court --bin layout_court -- fixtures/ui-layout-court/product-subject.json
# expect exit 0

# zcode-cli (sibling @ fed6f166)
bun test test/ui-layout-court-chicago.test.ts
# expect 31 passed; exit 0

# live adapter against ggen tip (default requireCleanRoom)
# UiLayoutCourtAdapter.verifyPostTransition(<ggen tip>) → exit 0 when layout_court PRESENT
```
