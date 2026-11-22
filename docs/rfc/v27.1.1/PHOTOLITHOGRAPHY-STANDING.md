# Photolithography path — standing ledger (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-STANDING-27.1.1` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed tip (this ledger) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5f8f0c3a4e8cf4c487d61f6560876b05728eeff0` (hop-4 real manufacture **PARTIAL** exit **2** `REFUSED :no_primitive_space`; hop-5 live after layout-court [#884](https://github.com/seanchatmangpt/ggen/pull/884) / land `d37271961`/`6a89beb38`; `ggen-layout-court` **PRESENT**; ui-layout **ALIVE** evidence `eaf0dad40` exit 0 / 8 passed — ancestor; tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo reconfirm PENDING** quiet cgroup — not tip exit 0) |
| Hop-4 manufacture | 2026-11-22 — `cursor/v2711-hop4-manufacture-bc54` / `/tmp/cursor/igniter-real-cleanroom.md` (`bc-72d61d31`) — BrandTheme.from_brand on tip `fourth-watch.brand.toml` → exit **2** `{:brand_theme_refused, :no_primitive_space}` |
| Hop-5 live | 2026-11-22 — `cursor/v2711-hop5-live-bc54` / `/tmp/cursor/hop5-live-after-884.md` (prior PRESENT pin `cursor/v2711-hop5-present-pin-bc54`; prior cross-mount `/tmp/cursor/hop5-crossmount.md`) |
| Status | LEDGER (path standings; **not** a crown `ALIVE` receipt) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Crown rule:** the photolithography path as a whole remains **PARTIAL** / **not**
`ALIVE`. Source presence, workflow definitions, IO-free contracts, and sibling
repo names do not promote the path crown. Hop 4 (igniter) is **PARTIAL** /
**PARTIAL_ALIVE** — mount **PRESENT** + clean-tree checkpoint **and** a real
manufacture subject executed: tip `fourth-watch.brand.toml` `[rhythm]` →
igniter `BrandTheme.from_brand` → exit **2**
`REFUSED {:brand_theme_refused, :no_primitive_space}` (evidence
`/tmp/cursor/igniter-real-cleanroom.md`). Crown hop-4 **not** `ALIVE`.
**Antifalsifier:** bare `git diff --exit-code` on a post-clone / virgin
working tree ≠ hop-4 `ALIVE`; igniter-fixture-only idempotent emit ≠ tip
manufacture ALIVE. Hop 5 aggregate is **PARTIAL_ALIVE** — in-process zcode bun
courts **ALIVE** (35 passed; exit 0); live
`UiLayoutCourtAdapter.verifyPostTransition` **ALIVE** (exit **0**; tip
`6fdcf4665` / live tip `5f8f0c3a4`; `ggen-layout-court` **PRESENT** via #884;
`layout_court` exit **0**; no new cargo under host load&gt;10 — used prebuilt
`target/debug/layout_court`). `powl-foundry-loop --live` remains **open**
(prior exit **1**; not re-run this pass). Do **not** invent hop-5 / path-crown
`ALIVE` from adapter alone. Do **not** restate package **ABSENT** after land
`d37271961`. The in-repo ggen ui-layout fence remains **ALIVE** on evidence tip
`eaf0dad40` (**8 passed; exit 0**; `bc-c7c9d554`; ancestor of live tip
`5f8f0c3a4`); tip court land `7c0ba2a90`; tip source **11** `#[test]`;
**tip-cargo reconfirm PENDING** (quiet cgroup) — not tip exit 0; ALIVE retained
(not demoted for thrash/OOM). That does **not** promote the path crown.

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
| 3b | ggen `ui-layout` digest consume (D5 in-repo fence) | **ALIVE** — `layout::ui_layout` + `tests/ui_layout_court.rs`; evidence tip `eaf0dad40` **8 passed; exit 0** (`bc-c7c9d554`; ancestor); tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** (quiet cgroup) | **In-repo** fence only; sibling `ui-layout-court.ts` remains **ABSENT** / **UNSUPPORTED** here | Replay: `cargo test -p ggen-graph --test ui_layout_court` | Court missing from tip; silent admit of missing/stale/mismatched digest; conflating with sibling `.ts`; inventing path-crown ALIVE from hop 3b alone; inventing tip exit 0 without quiet cargo |
| 4 | ggen_igniter clean-room / manufacture | **PARTIAL** — mount **PRESENT** (`/home/ubuntu/ggen_igniter` @ `b5a15528…`); clean-tree checkpoint `git diff --exit-code` → **0**; real BrandTheme manufacture on tip brand → exit **2** `REFUSED {:brand_theme_refused, :no_primitive_space}` (tip `[rhythm]` ≠ igniter DTCG `[primitive.space]`). Crown **not** `ALIVE`. `mix` / full sync **BLOCKED** (NIF). In-repo contract **PARTIAL_ALIVE**. | **Out-of-repo** sibling `seanchatmangpt/ggen_igniter`. Evidence: `/tmp/cursor/igniter-real-cleanroom.md`. In-repo: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) + `igniter_cleanroom_court` — **not** manufacture crown | **Next (hop-4 ALIVE):** align tip `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` to DTCG `[primitive.space]` **or** add a ggen rhythm→theme emitter; then re-run BrandTheme emit from tip brand vs igniter fixture → `diff -rq` / `git diff --exit-code` on that subject → exit **0**. Exact driver: `/tmp/cursor/igniter-real-cleanroom.md` §3 path A. **Antifalsifier:** bare post-clone `git diff --exit-code` ≠ `ALIVE`; igniter-fixture-only idempotent emit ≠ tip manufacture. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | Claiming `ALIVE` from bare post-clone diff; claiming ALIVE while tip brand still refuses `:no_primitive_space`; virgin-clone hygiene as manufacture proof; conflating igniter-fixture emit with tip manufacture; conflating `gall_observe_clean_room` with igniter crown |
| 5 | zcode-cli POWL/OCEL remediations (bounded refusal) | **PARTIAL_ALIVE** — in-process bun `ui-layout-court` **ALIVE** (**35 passed; exit 0**); live `UiLayoutCourtAdapter` **ALIVE** (**exit 0**; tip `6fdcf4665`; `ggen-layout-court` **PRESENT** #884; `layout_court` exit **0**; DEFAULT_TAFFY_CMD resolve → built bin, no cargo); foundry `--live` still **open** (prior exit **1**; not re-run under load&gt;10) | **Out-of-repo** `zcode-cli` @ `/agent/repos/zcode-cli` @ `02a9d86e5`. Package `ggen-layout-court` **PRESENT** on tip (`crates/ggen-layout-court` + bin `layout_court`; land `d37271961`). In-repo contract: [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (needs `mix` for igniter sync); `bun test test/powl-ocel-chicago.test.ts` → exit **0**. Replay live adapter: `UiLayoutCourtAdapter.verifyPostTransition(<ggen tip>)` with `target/debug/layout_court` + `fixtures/ui-layout-court/product-subject.json` → exit **0**. Presence: `test -d crates/ggen-layout-court`. Replay in-process: `bun test test/ui-layout-court.test.ts test/ui-layout-court-chicago.test.ts`. GA falsifier **2027-01-01** | Inventing hop-5 / path-crown `ALIVE` from adapter alone while foundry `--live` open; inventing tip-cargo ALIVE under load&gt;10; inventing live `ALIVE` from package **PRESENT** alone; restating package **ABSENT** after land `d37271961`; conflating hop-3b fence with live `layout_court` bin |

**Path standing (aggregate):** **PARTIAL** — hops 1–3 **PARTIAL_ALIVE**, hop 3b
ggen ui-layout **ALIVE** (**8 passed; exit 0** on evidence `eaf0dad40`; tip land
`7c0ba2a90`; tip pin `5f8f0c3a4`), hop 4 **PARTIAL** (mount PRESENT + clean-tree
+ real manufacture exit **2** `REFUSED :no_primitive_space` — **not** `ALIVE`;
antifalsifier: bare post-clone `git diff --exit-code` ≠ `ALIVE`), hop 5
**PARTIAL_ALIVE** (in-process **ALIVE** + live adapter **ALIVE** exit 0; foundry
`--live` still open) → path crown **non-ALIVE** / incomplete until tip brand
aligns to DTCG `[primitive.space]` (or rhythm→theme emitter) and manufacture
compare exits **0**, and foundry `--live` exits 0 on pinned tip. Pass-count
footnotes (tip source, not tip exit 0): hop-1 `shui` **4**; hop-2 brand **11**;
hop-3 taffy **7**; hop-3b ui-layout **11**.


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
| `crates/ggen-graph/tests/ui_layout_court.rs` | Chicago court — **ALIVE** on evidence `eaf0dad40` (**8 passed; exit 0**); tip land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** (quiet cgroup) |
| `crates/ggen-graph/src/layout/remediation_bound.rs` | Bound vocabulary stub (retry/write-scope caps) |
| Land commit `7c0ba2a90` | Tip court land (viewport/expect-free); evidence `eaf0dad40` **8 passed; exit 0** (`bc-c7c9d554`) — quiet reconfirm awaiting |

Court **ALIVE** on evidence tip `eaf0dad40` (`bc-c7c9d554`: `cargo test -p ggen-graph --test ui_layout_court` → **8 passed; exit 0**; ancestor of live tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46`). Tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo reconfirm PENDING** (quiet cgroup) — not tip exit 0. This hop is
**not** the sibling zcode-cli `ui-layout-court.ts` adapter.

### 4 — ggen_igniter clean-room / manufacture

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/ggen_igniter` | Clean-room / manufacture subject — **out-of-repo**; mount **PRESENT** at `/home/ubuntu/ggen_igniter` (HEAD `b5a15528…`) |
| `git diff --exit-code` | Clean-tree **checkpoint only** (exit **0** observed) — **not** manufacture `ALIVE` |
| Tip brand → BrandTheme | **Executed** on tip `fourth-watch.brand.toml` → exit **2** `{:brand_theme_refused, :no_primitive_space}` — schema gap `[rhythm]` vs DTCG `[primitive.space]` |
| Manufacture compare (ggen tip emit vs igniter expected) | **REFUSED** / open — next = align tip brand or add rhythm→theme emitter, then byte-compare → exit **0** |
| `/tmp/cursor/igniter-real-cleanroom.md` | Real clean-room evidence (`bc-72d61d31`) |
| `docs/reference/gate-conventions.md` | ggen vs ggen_igniter gate scoring orientation |
| `docs/jira/v26.11.4/_LANES.md` | Historical IG-1 dirty-tree note (~98 files) — **not** current standing |
| `justfile` (`sjira-mutual-admit`) / `scripts/sjira_mutual_admit.sh` | Hands-free mutual-admit; Elixir leg skips when igniter/mix absent |
| `crates/ggen-graph/src/bin/gall_observe_clean_room.rs` | In-repo GALL W3 clean-room **observe** — distinct subject |
| `scripts/gall/external/24_run_clean_room_rebuild.sh` | In-repo GALL rebuild script — distinct subject |
| `docs/gall/W5_CLEAN_ROOM_REBUILD.md` | GALL W5 doctrine — distinct from igniter crown |

Standing now: **PARTIAL** (mount PRESENT + clean-tree + manufacture subject
executed and **REFUSED** `:no_primitive_space`). Do **not** promote hop 4 to
`ALIVE` from GALL clean-room artifacts, from an unmounted sibling, from bare
post-clone `git diff --exit-code`, or from igniter-fixture-only idempotent emit
while tip brand still refuses. In-repo sync idempotence proves **ggen**
regeneration hygiene, not tip BrandTheme manufacture vs igniter expected.

### 5 — zcode-cli POWL/OCEL remediations (bounded refusal)

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/zcode-cli` | Crown subject — **out-of-repo**; typically unmounted → **BLOCKED** |
| `crates/ggen-layout-court` (bin `layout_court`) | Live consume subject — **PRESENT** on tip @ land `d37271961` (workspace member); live adapter exit **0** (this land) |
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

Standing now: package **PRESENT** + live adapter exit **0**; foundry `--live`
still **open**. Bounded refusal means: missing, stale, or mismatched layout
digests fail closed with a typed remediation refusal; POWL/OCEL remediations
must not escalate to ambient DO or silent admit. Unmounted sibling checkout
refuses ALIVE promotion (`REFUSED_ZCODE_CHECKOUT_UNMOUNTED`); wrong subject
(including hop-4 `ggen_igniter`) refuses retarget
(`REFUSED_ZCODE_SUBJECT_MISMATCH`). Do **not** promote hop 5 / path crown to
`ALIVE` from package presence or adapter exit 0 alone while foundry `--live`
is open. Do **not** restate package **ABSENT** after land `d37271961`.

---

## What would promote the path (and what would not)

| Claim | Required evidence | Not sufficient |
|-------|-------------------|----------------|
| Hop 1 `ALIVE` | Product perimeter + ggen fence both execute with typed admit/refuse | Vocab constants alone |
| Hop 3 `ALIVE` as photolithography | ggen seal **and** consumer (zcode/igniter) consumes `AotSealReceipt` | `taffy_aot_preflight_court` green alone |
| Hop 3b `ALIVE` | `ui_layout_court` **8 passed; exit 0** on evidence `eaf0dad40` (this ledger; ancestor); tip land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup on `5e00619a4` | Presence alone; inventing tip exit 0 without quiet cargo evidence |
| Hop 4 `ALIVE` | Mounted `ggen_igniter` + tip brand admits BrandTheme (`[primitive.space]` or rhythm→theme emitter) + **manufacture compare** (tip emit vs igniter expected) exit 0 | Bare post-clone `git diff --exit-code`; tip brand still `:no_primitive_space`; igniter-fixture-only emit; GALL `gall_observe_clean_room`; clean-tree checkpoint alone |
| Hop 5 `ALIVE` | Live `UiLayoutCourtAdapter` exit 0 **and** `powl-foundry-loop --live` exit 0 against pinned ggen tip with `ggen-layout-court` present | In-process bun alone; hop-3b fence alone; `ggen-layout-court` **PRESENT** alone; adapter exit 0 while foundry `--live` open |
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
test -d crates/ggen-layout-court
```

Sibling verifiers:

```text
# in seanchatmangpt/ggen_igniter (hop 4) — checkpoint only (≠ ALIVE)
cd /home/ubuntu/ggen_igniter && git diff --exit-code

# hop-4 ALIVE next verifier (manufacture REFUSED :no_primitive_space — open):
# A) align tip docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml to DTCG [primitive.space]
#    OR add ggen rhythm→theme emitter
# B) BrandTheme emit tip brand vs igniter test/fixtures/brand_theme/fourth-watch.brand.toml
# C) diff -rq / git diff --exit-code on THAT subject → exit 0
# evidence: /tmp/cursor/igniter-real-cleanroom.md §3
# antifalsifier: bare git diff --exit-code post-clone ≠ ALIVE;
#                igniter-fixture-only idempotent emit ≠ tip manufacture ALIVE

# in seanchatmangpt/zcode-cli (hop 5) — live adapter exit 0; foundry --live still open
bun test test/powl-ocel-chicago.test.ts
pnpm test -- ui-layout-court
# replay live: UiLayoutCourtAdapter.verifyPostTransition(<ggen tip>) → 0
# next live crown: bun scripts/powl-foundry-loop.ts --live → 0
```

---

## Provenance

Materialized as the Gate-1 photolithography standing ledger for
ECOSYSTEM-SPEC-27.1.1. Refresh 2026-11-22 on crown tip `b96100521` (ui-layout
**ALIVE** — **8 passed; exit 0** on evidence `eaf0dad40` via `bc-c7c9d554`; tip
court land `7c0ba2a90`). Tip-pin retip 2026-11-22 on
`cursor/v2711-gate1-retip-bc54` to live tip
`5e00619a422a0d8e451029f0aaf8b7a63e27da46` (ui-layout **ALIVE** retained on
ancestor evidence; tip source **11** `#[test]`; **tip-cargo reconfirm PENDING**
quiet cgroup — not tip exit 0). Hop-4 demotion 2026-11-22 on
`cursor/v2711-photo-hop4-partial-bc54` onto tip `376b5c3092c821eec942a2e16489b6db45c0b726`: hop 4
**PARTIAL_ALIVE** (mount PRESENT + clean-tree only; manufacture compare open —
**not** `ALIVE`; antifalsifier: bare post-clone `git diff --exit-code` ≠
`ALIVE`); hop 5 then **PARTIAL** (in-process ALIVE + live BLOCKED on ABSENT
package). Hop-5 PRESENT pin 2026-11-22 on `cursor/v2711-hop5-present-pin-bc54`
onto tip `750b8ef4a9042d22808a79be3dedff7944d4e321` (land `d37271961`): package
`ggen-layout-court` **PRESENT**; live adapter tip-exit then **PENDING** — not
ABSENT. Hop-5 live 2026-11-22 on `cursor/v2711-hop5-live-bc54` onto tip
`6fdcf4665d52931f7de25909366127bc9128a230`: live adapter exit **0** → hop 5
**PARTIAL_ALIVE** (foundry `--live` still open); path crown remains **PARTIAL**.
Pass-count footnotes (tip source): `shui` **4**, taffy **7**, brand **11**,
ui-layout **11**. No path-crown `ALIVE` asserted.

### Hop-5 cross-mount refresh (`cursor/v2711-hop5-standing-bc54`)

- Prior ABSENT snapshot tip: `376b5c3092c821eec942a2e16489b6db45c0b726` (hop-5 evidence tip `07b689c97` / ancestor `c9e224d33`; prior pin `f6cb18f86`).
- In-process: `bun test test/ui-layout-court*.ts` → **35 passed; exit 0** (**ALIVE** observation).
- Live (stale ABSENT snapshot / pre-#884): `ggen-layout-court` then **ABSENT**; `cargo run -p ggen-layout-court --bin layout_court` → exit **101**; `UiLayoutCourtAdapter.verifyPostTransition` → exit **2** `BLOCKED`/`SUBJECT_MISSING`; `bun scripts/powl-foundry-loop.ts --live` → exit **1**.
- Receipt: `/tmp/cursor/hop5-crossmount.md`. No path-crown `ALIVE`.

### Hop-5 PRESENT pin (`cursor/v2711-hop5-present-pin-bc54`)

- Observed tip: `750b8ef4a9042d22808a79be3dedff7944d4e321` (land `d37271961fa0b3803e683ac13e2a3de088290bbb`).
- Package: `crates/ggen-layout-court` **PRESENT** (workspace member; bin `layout_court`) — presence via `test -d crates/ggen-layout-court` / `git cat-file -e …:crates/ggen-layout-court/Cargo.toml`.
- Live adapter / foundry tip-exit then **PENDING** (do not invent exit 0; do not restate package **ABSENT**).
- Aggregate hop 5 then **PARTIAL**. No path-crown `ALIVE`. Superseded by hop-5 live exit 0 below.

### Hop-5 live after layout-court #884 (`cursor/v2711-hop5-live-bc54`)

- Observed tip: `6fdcf4665d52931f7de25909366127bc9128a230` (`ggen-layout-court` **PRESENT**; land `d37271961` / merge `6a89beb38` / PR #884).
- Host load &gt;10 → **no new cargo**; used prebuilt `target/debug/layout_court`.
- `layout_court` + `fixtures/ui-layout-court/product-subject.json` → exit **0**.
- In-process: `bun test test/ui-layout-court*.ts` → **35 passed; exit 0** (**ALIVE**).
- Live: `UiLayoutCourtAdapter.verifyPostTransition` → exit **0** standing **ALIVE** (explicit bin + DEFAULT_TAFFY_CMD resolve to built bin).
- Foundry `--live` **not** re-run (prior exit 1; `mix` still required) → hop-5 aggregate **PARTIAL_ALIVE**, not hop-5 / path-crown `ALIVE`.
- Receipt: `/tmp/cursor/hop5-live-after-884.md`.

### Hop-4 real manufacture PARTIAL (`cursor/v2711-hop4-manufacture-bc54`)

- Observed tip: `5f8f0c3a4e8cf4c487d61f6560876b05728eeff0`.
- Subject executed: tip `fourth-watch.brand.toml` (`[rhythm]` / `[[rhythm.spacing]]`) → igniter `BrandTheme.from_brand` → exit **2** `{:brand_theme_refused, :no_primitive_space}`.
- Fixture identity: tip sha256 `2c62d52a…` ≠ igniter fixture `d6ece1d8…` (181-line schema gap).
- Igniter-fixture-only idempotent emit: exit **0** (checkpoint only — **not** tip manufacture ALIVE).
- In-repo fences: `taffy_aot_preflight_court` **6 passed**; `brand_toml_rhythm_court` **4 passed** — hops 2–3, not hop-4 crown.
- Full `mix ggen_igniter.sync`: **BLOCKED** (NIF/oxrocksdb toolchain).
- Standing: **PARTIAL** — do **not** promote hop-4 / path crown to `ALIVE`.
- Exact next: align tip brand to DTCG `[primitive.space]` **or** rhythm→theme emitter; then manufacture compare → exit **0** (`/tmp/cursor/igniter-real-cleanroom.md` §3 path A).
- Receipt: `/tmp/cursor/igniter-real-cleanroom.md` (`bc-72d61d31`).

