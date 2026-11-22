# Photolithography path — standing ledger (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-STANDING-27.1.1` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed tip (this ledger) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c5c6e28aadd51d73abf2cca176525078d2ab8856` (matrix pin parent `cbfd94582`; land `479fc83a9`) |
| Status | LEDGER (path standings; **not** a crown `ALIVE` receipt) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Crown rule:** the photolithography path as a whole remains **PARTIAL** / **not**
`ALIVE`. Source presence, workflow definitions, IO-free contracts, and sibling
repo names do not promote the path crown. Hop 4 (igniter) stays **BLOCKED**;
hop 5 (live zcode) stays **UNSUPPORTED**. The in-repo ggen ui-layout fence is
**PRESENT** / **ALIVE-for-presence** only — exclusive tip cargo exit-0 is
**PENDING** (do not invent).

```text
perimeter transducer
  → .ttl / .toml
  → ggen + Taffy AOT
  → ggen ui-layout digest consume (in-repo fence)
  → ggen_igniter clean-room (git diff --exit-code == 0)
  → zcode-cli POWL/OCEL remediations (bounded refusal)
```

---

## Hop table

| # | Hop | Standing | In-repo vs out-of-repo | Exact next verifier | Falsifier |
|---|-----|----------|------------------------|---------------------|-----------|
| 1 | Perimeter transducer (O → O\* admission fence) | **PARTIAL_ALIVE** (prefix fence only) | **In-repo** fence; product perimeter **out-of-repo** (Truex / brand plane) | `cargo test -p ggen-graph --test shui_public_prefix_court` | Court fails; or `shui` dropped from `ADMITTED_TURTLE_PREFIXES`; or private prefix (`truex`, `gall`, …) admits |
| 2 | `.ttl` / `.toml` law + brand rhythm | **PARTIAL_ALIVE** (rhythm + OCEL/POWL vocab); full DTCG inventory **UNSUPPORTED** | **In-repo** fixtures + vocabs; OEM token inventories Gate 2+ / brand plane | `cargo test -p ggen-graph --test brand_toml_rhythm_court` | Fixtures refuse or go missing; rhythm admits touch &lt;44 / gap &lt;8 / off-grid; Gate-2 tokens invented without source |
| 3 | ggen + Taffy AOT pre-flight | **PARTIAL_ALIVE** (ggen six-width seal) | **In-repo** (`ggen-graph::layout::taffy_aot`); consumer adapters **out-of-repo** | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | Six-width seal fails under WCAG 1.4.12 1.5×; overflow refusal omits failing width; silent drop of typed `AotRefusal` |
| 3b | ggen `ui-layout` digest consume (D5 in-repo fence) | **PRESENT** / **ALIVE-for-presence** — `layout::ui_layout` + `tests/ui_layout_court.rs` on tip; exclusive tip cargo exit-0 **PENDING** (land `479fc83a9` recorded exit 0; **do not invent** tip exit 0) | **In-repo** fence only; sibling `ui-layout-court.ts` remains **ABSENT** / **UNSUPPORTED** here | Presence: `test -f crates/ggen-graph/tests/ui_layout_court.rs`. Replay: `cargo test -p ggen-graph --test ui_layout_court` | Court missing from tip; silent admit of missing/stale/mismatched digest; claiming exclusive tip exit 0 without running cargo; conflating with sibling `.ts` |
| 4 | ggen_igniter clean-room | **BLOCKED** / **UNKNOWN**; in-repo contract **PARTIAL_ALIVE** | **Out-of-repo** sibling `seanchatmangpt/ggen_igniter` (not mounted). In-repo: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) + `igniter_cleanroom_court` — **not** live sibling crown | In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court`. Live: in sibling checkout `git diff --exit-code` (exit **0** required). Optional: `just sjira-mutual-admit dry-run` | Sibling dirty tree (exit ≠ 0); claiming `ALIVE` from unmounted checkout; conflating `gall_observe_clean_room` with igniter crown |
| 5 | zcode-cli POWL/OCEL remediations (bounded refusal) | **UNSUPPORTED** (live adapter); in-repo contract **PARTIAL_ALIVE**; live sibling **BLOCKED** | **Out-of-repo** `zcode-cli` Chicago POWL/OCEL + `ui-layout-court.ts`. **In-repo** typed refusal court + bound stub + POWL/OCEL vocab + ggen ui-layout fence — see [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | In-repo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`; `cargo test -p ggen-graph --test ui_layout_court`. Predecessor hop-4: in `ggen_igniter` `git diff --exit-code`. Live: in zcode-cli `bun test test/powl-ocel-chicago.test.ts` then `pnpm test -- ui-layout-court` vs ggen `AotSealReceipt` (fail closed; bounded refusal). GA falsifier **2027-01-01** | Adapter re-implements Taffy law; missing/stale receipt admits; unbounded remediation; claiming zcode `ALIVE` from ggen vocab / unmounted checkout; conflating hop-4 igniter clean-room with hop-5; conflating hop-3b ggen fence with live `.ts` |

**Path standing (aggregate):** **PARTIAL** — hops 1–3 **PARTIAL_ALIVE**, hop 3b
ggen ui-layout **PRESENT** / **ALIVE-for-presence** (tip cargo exit **PENDING**),
hop 4 **BLOCKED**, hop 5 live consumer **UNSUPPORTED** → path crown
**non-ALIVE** / incomplete until hops 4–5 execute on mounted siblings with typed
receipts.

---

## Hop detail (cited paths)

### 1 — Perimeter transducer

Admission edge that turns raw observation into bounded `O*` for manufacture
(`A = μ(O*)`, `AGENTS.md` §3). In this repo the live fence is the public Turtle
prefix / private-namespace gate — not a file named “transducer”.

| Path | Role |
|------|------|
| `crates/ggen-graph/src/vocab/public_prefixes.rs` | `ADMITTED_TURTLE_PREFIXES` / `REFUSED_PRIVATE_PREFIXES` / `is_admitted_turtle_prefix` |
| `crates/ggen-graph/src/vocab/shui.rs` | W3C `shui:` (`http://www.w3.org/ns/shacl-ui/`) |
| `crates/ggen-graph/tests/shui_public_prefix_court.rs` | Chicago court for hop 1 |
| `crates/ggen-graph/src/bin/gall_actuate_code_evaluation.rs` | Private-namespace scan companion |
| `crates/ggen-lsp/assets/policies/no-private-namespace.shacl.ttl` | Author-time SHACL companion |

Truex product admission of `shui:` shapes remains **UNKNOWN** (sibling not
mounted; see `GATE-1-GAP-MATRIX.md`).

### 2 — `.ttl` / `.toml`

| Path | Role |
|------|------|
| `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` | OEM rhythm stub |
| `docs/rfc/v27.1.1/fixtures/cindycat.brand.toml` | OEM rhythm stub |
| `crates/ggen-graph/src/layout/brand.rs` | DTCG 2025.10 rhythm parse/admit + `BrandRefusal` |
| `crates/ggen-graph/tests/brand_toml_rhythm_court.rs` | Chicago court for hop 2 |
| `ontologies/core/ocel2.ttl` | OCEL 2.0 ontology |
| `crates/ggen-graph/src/vocab/ocel.rs` | OCEL vocab constants |
| `crates/ggen-graph/src/vocab/powl.rs` | POWL repair-route vocab |
| `crates/ggen-graph/src/ocel/` | In-repo OCEL surfaces (`pack_events`, lifecycle, …) |

Full color/type/CASTLE inventories remain Gate 2+ — inventing them here is
**REFUSED_UNGROUNDED**.

### 3 — ggen + Taffy AOT

| Path | Role |
|------|------|
| `crates/ggen-graph/src/layout/taffy_aot.rs` | `VIEWPORT_WIDTHS_PX`, `preflight_column_six_widths`, `AotSealReceipt`, `AotRefusal` |
| `crates/ggen-graph/src/layout/mod.rs` | Layout surface module |
| `crates/ggen-graph/tests/taffy_aot_preflight_court.rs` | Chicago court for hop 3 |
| `Cargo.toml` / workspace `taffy` dep | Layout engine pin |

Widths sealed: `320 / 375 / 414 / 768 / 1024 / 1440` under WCAG 1.4.12 **1.5×**.
This hop does **not** prove zcode-cli or igniter consumption.

### 3b — ggen ui-layout digest consume (in-repo D5 fence)

| Path | Role |
|------|------|
| `crates/ggen-graph/src/layout/ui_layout.rs` | Consume `AotSealReceipt` digests; typed `UiLayoutRefusal` / standing |
| `crates/ggen-graph/tests/ui_layout_court.rs` | Chicago court — **PRESENT** / **ALIVE-for-presence** on tip |
| `crates/ggen-graph/src/layout/remediation_bound.rs` | Bound vocabulary stub (retry/write-scope caps) |
| Land commit `479fc83a9` | Recorded court exit **0** (not re-claimed as exclusive tip cargo exit 0) |

Presence on tip `c5c6e28aa` (parent pin `cbfd94582`) is confirmed via
`git cat-file -e …:crates/ggen-graph/tests/ui_layout_court.rs`. This hop is
**not** the sibling zcode-cli `ui-layout-court.ts` adapter.

### 4 — ggen_igniter clean-room

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/ggen_igniter` | Clean-room crown subject — **out-of-repo** |
| `git diff --exit-code` | Exact crown verifier (exit **0** = clean tree) |
| `docs/reference/gate-conventions.md` | ggen vs ggen_igniter gate scoring orientation |
| `docs/jira/v26.11.4/_LANES.md` | Historical IG-1 dirty-tree note (~98 files) — **not** current standing |
| `justfile` (`sjira-mutual-admit`) / `scripts/sjira_mutual_admit.sh` | Hands-free mutual-admit; Elixir leg skips when igniter/mix absent |
| `crates/ggen-graph/src/bin/gall_observe_clean_room.rs` | In-repo GALL W3 clean-room **observe** — distinct subject |
| `scripts/gall/external/24_run_clean_room_rebuild.sh` | In-repo GALL rebuild script — distinct subject |
| `docs/gall/W5_CLEAN_ROOM_REBUILD.md` | GALL W5 doctrine — distinct from igniter crown |

Do **not** promote hop 4 to `ALIVE` from GALL clean-room artifacts or from an
unmounted sibling. In-repo sync idempotence (`scripts/ci/guard-pack-proofs.sh`
`diff -rq` after `ggen sync run`; `.github/workflows/*` `git diff --exit-code`
patterns; `docs/USAGE_GUIDE.md` CI example) proves **ggen** regeneration hygiene,
not ggen_igniter clean-room.

### 5 — zcode-cli POWL/OCEL remediations (bounded refusal)

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/zcode-cli` | Crown subject — **out-of-repo**; typically unmounted → **BLOCKED** |
| `bun test test/powl-ocel-chicago.test.ts` | Exact hop-5 Chicago crown verifier (exit **0** required for live ALIVE) |
| `pnpm test -- ui-layout-court` | Layout consume companion vs ggen `AotSealReceipt` (sibling `.ts`) |
| `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md` | Hop-5 standing ledger + GA ladder through **2027-01-01** |
| `crates/ggen-sjira/src/zcode_powl_ocel.rs` | IO-free typed refusal (`CheckoutUnmounted` / `SubjectMismatch` / court fail) |
| `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` | Chicago in-repo contract court — **PARTIAL_ALIVE** |
| `crates/ggen-graph/src/layout/remediation_bound.rs` | Bound vocabulary stub (retry/write-scope caps) |
| `docs/jira/v26.11.17/code-alignment.md` | Historical `~/zcode-cli/scripts/docs_v261117.py` pointer — not Gate-1 court |
| `crates/ggen-graph/src/vocab/powl.rs` | In-repo POWL shapes for repair routes |
| `crates/ggen-graph/src/vocab/ocel.rs` + `crates/ggen-graph/src/ocel/` | In-repo OCEL shapes / events |
| `crates/ggen-graph/src/layout/taffy_aot.rs` (`AotSealReceipt`, `AotRefusal`) | Receipt the adapter must consume without re-implementing geometry |
| `crates/ggen-graph/src/layout/ui_layout.rs` + `tests/ui_layout_court.rs` | In-repo D5 fence (hop 3b) — not hop-5 live ALIVE |

Bounded refusal means: missing, stale, or mismatched layout digests fail closed
with a typed remediation refusal; POWL/OCEL remediations must not escalate to
ambient DO or silent admit. Unmounted sibling checkout refuses ALIVE promotion
(`REFUSED_ZCODE_CHECKOUT_UNMOUNTED`); wrong subject (including hop-4
`ggen_igniter`) refuses retarget (`REFUSED_ZCODE_SUBJECT_MISMATCH`).

---

## What would promote the path (and what would not)

| Claim | Required evidence | Not sufficient |
|-------|-------------------|----------------|
| Hop 1 `ALIVE` | Product perimeter + ggen fence both execute with typed admit/refuse | Vocab constants alone |
| Hop 3 `ALIVE` as photolithography | ggen seal **and** consumer (zcode/igniter) consumes `AotSealReceipt` | `taffy_aot_preflight_court` green alone |
| Hop 3b ALIVE-for-presence | `ui_layout_court.rs` PRESENT on tip (this ledger) | Inventing exclusive tip cargo exit 0 without running cargo |
| Hop 4 `ALIVE` | Mounted `ggen_igniter` + `git diff --exit-code` exit 0 on that tip | GALL `gall_observe_clean_room`; docs citing the sibling |
| Hop 5 `ALIVE` | zcode-cli court green against a real ggen receipt fixture | In-repo POWL/OCEL modules; ggen hop-3b fence alone |
| **Path `ALIVE`** | Hops 1–5 all `ALIVE` with linked receipts | This ledger file; hop-3b presence alone |

---

## Replay commands (in-repo fence only)

```text
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test ui_layout_court
cargo test -p ggen-sjira --test igniter_cleanroom_court
cargo test -p ggen-sjira --test zcode_powl_ocel_court
cargo test -p ggen-graph remediation_bound --lib
just sjira-mutual-admit dry-run

# presence (no cargo)
test -f crates/ggen-graph/tests/ui_layout_court.rs
test -f crates/ggen-graph/src/layout/ui_layout.rs
```

Sibling verifiers (not executable here without mounts):

```text
# in seanchatmangpt/ggen_igniter (hop 4)
git diff --exit-code

# in seanchatmangpt/zcode-cli (hop 5)
bun test test/powl-ocel-chicago.test.ts
pnpm test -- ui-layout-court
```

---

## Provenance

Materialized as the Gate-1 photolithography standing ledger for
ECOSYSTEM-SPEC-27.1.1. Refresh 2026-11-22 on tip `c5c6e28aa` (ui-layout
PRESENT / ALIVE-for-presence; hop 4 **BLOCKED**; hop 5 **UNSUPPORTED**; path
crown remains **PARTIAL**). No path-crown `ALIVE` asserted. Exclusive tip
`ui_layout_court` cargo exit-0 not invented.
