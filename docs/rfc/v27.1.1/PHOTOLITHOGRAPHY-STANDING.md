# Photolithography path — standing ledger (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-STANDING-27.1.1` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed tip (this ledger) | absorb tip `68d8499b8f029f4debda4cddc805e57687cfa47f` (~`ba9824207+` / `8cee4246f+` / `c11ea7f2a+` / `89ca2da9c+` / `2fd897141+` / `46a026ae9+`; absorb gate1-post @ `7b1120d6e` + #936 @ `0e25df932`; hop-4 mix/NIF residual @ `ae2acc52b` — PATH `mix` **ABSENT** / off-PATH **PRESENT** / NIF **BLOCKED**; hop-4 BrandTheme **ALIVE**; taffy absorb #930 **PRESENT** blob `d8923b07e`; ui_layout tip-cargo **ALIVE** preserved @ `ca52e9c16`/`8cee4246f`; Truex admit-court **ALIVE** @ `truex@68d2d233`; hop-5 **PARTIAL_ALIVE** — adapter exit **0**; foundry `--live` **foundry_exit=1** / **REFUSED** `Model access is not configured`) |
| Hop-4 manufacture | 2026-11-22 — `cursor/v2711-hop4-manufacture-alive-bc54` + reconfirm `cursor/v2711-hop4-primitive-space-bc54` / `/tmp/cursor/igniter-hop4-manufacture-alive.md` (`bc-b528ad50`) — tip `fourth-watch.brand.toml` BrandTheme emit vs igniter fixture → **ALIVE** (`exit=0`, canon `diff -rq=0`, compare-subject `git diff --exit-code=0`); prior PARTIAL exit **2** `:no_primitive_space` closed by dual-schema + emitter ancestry; IGNITER manufacture row corrected REFUSED → **ALIVE**; tip-pin reconfirm absorb #931 `cursor/v2711-photo-hop4-retip-bc54` @ `d191cf792` onto `89ca2da9c+` (dual-schema + emit path present; no cargo) |
| Hop-5 mix SUBJECT | 2026-11-22 — `cursor/v2711-hop5-mix-subject-bc54` / [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) — default adapter exit **0** after Taffy+Mode B PASS (not SUBJECT_MISSING) |
| Hop-5 foundry live2 | 2026-11-22 — `cursor/v2711-hop5-foundry-live2-bc54` / [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md) — Gall `--live` injects `createZcodeAgentExecutor`; **foundry_exit=1** / **REFUSED** (runtime ABSENT at live2); no false `ALIVE` |
| Hop-5 zcode runtime | 2026-11-22 — `cursor/v2711-hop5-zcode-runtime-bc54` / [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md) — Linux tip-matched 3.14.4 `.deb` → `vendor/zcode.cjs` **PRESENT**; darwin `sync:locked`/`sync:local` **UNSUPPORTED**; `--live` **foundry_exit=1** / **REFUSED** (model access ABSENT); no false `ALIVE` |
| Status | LEDGER (path standings; **not** a crown `ALIVE` receipt) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Crown rule:** the photolithography path as a whole remains **PARTIAL** / **not**
`ALIVE`. Source presence, workflow definitions, IO-free contracts, and sibling
repo names do not promote the path crown. Hop 4 BrandTheme manufacture compare
is **ALIVE** (`git diff --exit-code` → **0** on tip dual-schema brand vs igniter
fixture CSS; reconfirm evidence `/tmp/cursor/igniter-hop4-manufacture-alive.md`
on tip `28c3f4d845`; prior `/tmp/cursor/igniter-real-cleanroom.md`). **Antifalsifier:**
bare `git diff --exit-code` on a post-clone / virgin working tree ≠ hop-4
`ALIVE`; `mix ggen_igniter.sync` NIF path remains **BLOCKED** (separate residual).
Hop 5 aggregate is **PARTIAL_ALIVE** —
in-process zcode bun courts **ALIVE** (35 passed; exit 0); `ggen-layout-court`
**PRESENT** (#884 / tip `6fdcf4665`); `layout_court` subject **ALIVE** (exit **0**);
default `UiLayoutCourtAdapter.verifyPostTransition` **ALIVE** (exit **0** after
mix-SUBJECT fix when layout_court PRESENT — Jobs/C4 deferred / ggen dry-run;
see [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md)).
`powl-foundry-loop --live` is **BLOCKED**/**REFUSED** (**foundry_exit=1**; executor wired @ zcode `f32df08`; Linux runtime **PRESENT**; exact failure `Model access is not configured` — [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md)).
`mix ggen_igniter.sync` NIF remains **BLOCKED** (hop-4 residual). Jobs/C4 `mix` remains **ABSENT**.
Do **not** invent hop-5 / path-crown `ALIVE` from adapter alone.
Do **not** restate mix SUBJECT_MISSING after layout_court PRESENT + spatial PASS.
Do **not** claim `mix sync` ALIVE while NIF remains **BLOCKED**. The in-repo ggen
ui-layout fence remains **ALIVE** on evidence tip `eaf0dad40` (**8 passed;
exit 0**; `bc-c7c9d554`; ancestor of live tip `6fdcf4665`); tip court land
`7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo ALIVE** (**11 passed; exit 0** exclusive
`CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` under `/tmp/ggen-uilayout-tip-alive-protected` @ `ca52e9c16`; sources ≡ tip `120598fb3`).
That does **not** promote the path crown.

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
| 1 | Perimeter transducer (O → O\* admission fence) | **PARTIAL_ALIVE** (ggen prefix fence) + Truex product admit-court **ALIVE** @ `truex@68d2d233` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED**; vitest **6/6** exit 0) | **In-repo** fence; product perimeter **out-of-repo** (Truex **ALIVE** / brand plane) | Fence: `cargo test -p ggen-graph --test shui_public_prefix_court`. Truex replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233` | Court fails; or `shui` dropped from `ADMITTED_TURTLE_PREFIXES`; or private prefix (`truex`, `gall`, …) admits; or Truex admit-court regresses off merged tip |
| 2 | `.ttl` / `.toml` law + brand rhythm | **PARTIAL_ALIVE** (rhythm + OCEL/POWL vocab + dual-schema `[primitive.space]` on Fourth Watch **and** CindyCat); full DTCG color/type/CASTLE **UNSUPPORTED** | **In-repo** fixtures + vocabs; OEM token inventories Gate 2+ / brand plane | `cargo test -p ggen-graph --test brand_toml_rhythm_court` (incl. `oem_fixtures_carry_dtcg_primitive_space_from_rhythm`) | Fixtures refuse or go missing; rhythm admits touch &lt;44 / gap &lt;8 / off-grid; Gate-2 tokens invented without source; dropping `[primitive.space]` from either OEM |
| 3 | ggen + Taffy AOT pre-flight | **PARTIAL_ALIVE** — tip subject **PRESENT** (`git ls-tree` blob `d8923b07e` on `89ca2da9c+`; absorb #930); tip-cargo **PENDING**/SKIPPED thrash; consumer adapters **out-of-repo** | **In-repo** (`ggen-graph::layout::taffy_aot`); consumer adapters **out-of-repo** | Quiet exclusive: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test taffy_aot_preflight_court --target-dir /tmp/ggen-taffy-preflight-target` | Six-width seal fails under WCAG 1.4.12 1.5×; overflow refusal omits failing width; silent drop of typed `AotRefusal`; inventing tip-cargo `ALIVE` without quiet exit 0 |
| 3b | ggen `ui-layout` digest consume (D5 in-repo fence) | **ALIVE** (tip-cargo) — exclusive quiet cargo **11 passed; exit 0** @ `ca52e9c16` (sources ≡ tip `120598fb3`); prior evidence `eaf0dad40` **8 passed; exit 0** (`bc-c7c9d554`); tip court land `7c0ba2a90`; tip source **11** `#[test]` | **In-repo** fence only; sibling `ui-layout-court.ts` remains **ABSENT** / **UNSUPPORTED** here | Replay: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` | Court missing from tip; silent admit of missing/stale/mismatched digest; conflating with sibling `.ts`; inventing path-crown ALIVE from hop 3b alone |
| 4 | ggen_igniter clean-room / manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` (`[rhythm]` + DTCG `[primitive.space]`; emitter `99255d53c` on tip) → igniter `BrandTheme` emit vs igniter fixture CSS → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-hop4-manufacture-alive.md`; tip `28c3f4d845`; prior `igniter-real-cleanroom.md` / `88e84d545`; reconfirm `cursor/v2711-hop4-primitive-space-bc54`; IGNITER manufacture row **ALIVE**). Mount **PRESENT**; clean-tree checkpoint also **0**. `mix ggen_igniter.sync` NIF path still **BLOCKED** (residual; default PATH `mix` **ABSENT**; off-PATH elixir-install **PRESENT** without HITL — [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)). In-repo contract **PARTIAL_ALIVE**. | **Out-of-repo** sibling `seanchatmangpt/ggen_igniter` @ `/home/ubuntu/ggen_igniter` / `/agent/repos/ggen_igniter`. Fixtures: `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml`. In-repo: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) + `igniter_cleanroom_court`; residual ledger [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md) | **Replay:** BrandTheme emit tip brand + igniter `test/fixtures/brand_theme/fourth-watch.brand.toml` (canon path for Source header) → `diff -rq` **0**; `git diff --exit-code` on compare subject → **0**. **Next residual:** exclusive quiet `CXX=g++-13` NIF → `mix ggen_igniter.sync --check` → **0**. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | Claiming hop-4 `ALIVE` from bare post-clone `git diff --exit-code` alone; claiming `mix sync` ALIVE while NIF **BLOCKED**; conflating off-PATH `mix` PRESENT with Jobs/C4 ALIVE; conflating igniter-fixture-only idempotent emit with tip manufacture; re-opening `:no_primitive_space` by dropping DTCG surface from tip fixture |
| 5 | zcode-cli POWL/OCEL remediations (bounded refusal) | **PARTIAL_ALIVE** — in-process bun **ALIVE**; live `UiLayoutCourtAdapter` **ALIVE** (**exit 0** after mix-SUBJECT fix); `layout_court` exit **0**; foundry `--live` **foundry_exit=1** / **REFUSED** (`Model access is not configured`; runtime PRESENT) | **Out-of-repo** `zcode-cli` @ `f32df08`. Package `ggen-layout-court` **PRESENT**. Contract: [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md); mix: [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md); live2: [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md); runtime: [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md) | Exact next: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0**; Jobs/C4 residual: `mix ggen_igniter.sync --check` → **0**. Replay adapter → **0**. GA falsifier **2027-01-01** | Inventing hop-5 crown `ALIVE` while foundry_exit≠0; inventing ALIVE from executor injection alone; restating `live executor not configured` after zcode `f32df08` |

**Path standing (aggregate):** **PARTIAL** — hops 1–3 **PARTIAL_ALIVE** (as prior), hop 3b
ggen ui-layout **ALIVE** (**8 passed; exit 0** on evidence `eaf0dad40`; tip land
`7c0ba2a90`), hop 4 BrandTheme manufacture compare **ALIVE** (`diff_exit=0` on tip
dual-schema vs igniter fixture; antifalsifier: bare post-clone ≠ `ALIVE`; `mix
sync` NIF still **BLOCKED**), hop 5 **PARTIAL_ALIVE** (in-process **ALIVE** + live
adapter **ALIVE** exit 0 after mix-SUBJECT fix; foundry `--live` **foundry_exit=1** /
**REFUSED** (`Model access is not configured`; runtime **PRESENT**)) → path crown **non-ALIVE** until
hop-5 foundry `--live` exit 0 (and optional `mix sync --check`) on pinned tip. Pass-count footnotes (tip source, not tip exit
0): hop-1 `shui` **4**; hop-2 brand **11**; hop-3 taffy **7**; hop-3b ui-layout
**11**.


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

Truex product admission of `shui:` shapes is **ALIVE** — sibling Chicago
admit-court **6/6 vitest exit 0** reconfirmed @
`truex@68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6)
**MERGED** → `main` merge `b4672131`; mount `/tmp/truex`; receipt
`/tmp/cursor/truex-merged-admit-alive.md`). ggen prefix fence remains a
separate hop-1 leg (tip-cargo PENDING). See `GATE-1-GAP-MATRIX.md` /
`OUT-OF-REPO-NEXT-VERIFIERS.md` G2-7.

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
| `crates/ggen-graph/tests/ui_layout_court.rs` | Chicago court — **ALIVE** tip-cargo **11 passed; exit 0** @ `ca52e9c16` (≡ tip `120598fb3`); prior evidence `eaf0dad40` (**8 passed; exit 0**); tip land `7c0ba2a90`; tip source **11** `#[test]` |
| `crates/ggen-graph/src/layout/remediation_bound.rs` | Bound vocabulary stub (retry/write-scope caps) |
| Land commit `7c0ba2a90` | Tip court land (viewport/expect-free); evidence `eaf0dad40` **8 passed; exit 0** (`bc-c7c9d554`) — quiet reconfirm awaiting |

Court **ALIVE** on evidence tip `eaf0dad40` (`bc-c7c9d554`: `cargo test -p ggen-graph --test ui_layout_court` → **8 passed; exit 0**; ancestor of live tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46`). Tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo ALIVE** (**11 passed; exit 0** @ `ca52e9c16` ≡ tip `120598fb3`). This hop is
**not** the sibling zcode-cli `ui-layout-court.ts` adapter.

### 4 — ggen_igniter clean-room / manufacture

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/ggen_igniter` | Clean-room / manufacture subject — **out-of-repo**; mount **PRESENT** at `/home/ubuntu/ggen_igniter` (HEAD `b5a15528…`) |
| `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` | Dual-schema tip brand (`[rhythm]` + DTCG `[primitive.space]`) — BrandTheme manufacture input |
| BrandTheme manufacture compare | **ALIVE** — tip emit vs igniter fixture CSS; compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-hop4-manufacture-alive.md`; prior `igniter-real-cleanroom.md`) |
| `git diff --exit-code` (bare mount) | Clean-tree **checkpoint** (exit **0**) — antifalsifier: alone ≠ hop-4 `ALIVE` |
| `mix ggen_igniter.sync --check` | **BLOCKED** residual (NIF/oxrocksdb; default PATH `mix` **ABSENT**; see [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)) |
| `docs/reference/gate-conventions.md` | ggen vs ggen_igniter gate scoring orientation |
| `crates/ggen-graph/src/bin/gall_observe_clean_room.rs` | In-repo GALL W3 clean-room **observe** — distinct subject |

Standing now: BrandTheme manufacture compare **ALIVE** (`diff_exit=0`). Do
**not** re-promote from bare post-clone `git diff --exit-code` alone
(antifalsifier). Do **not** claim `mix sync` ALIVE while NIF remains **BLOCKED**.

### 5 — zcode-cli POWL/OCEL remediations (bounded refusal)

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/zcode-cli` | Crown subject — **out-of-repo**; typically unmounted → **BLOCKED** |
| `crates/ggen-layout-court` (bin `layout_court`) | Live consume subject — **PRESENT** on tip @ land `d37271961`; `layout_court` exit **0**; default adapter exit **0** (spatial ALIVE; Jobs/C4 deferred when mix/ggen ABSENT) — do **not** invent hop-5 crown `ALIVE` |
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

Standing now: package **PRESENT** + `layout_court` exit **0**; default live
adapter exit **0** (spatial **ALIVE**; Jobs/C4 deferred when `mix`/`ggen`
ABSENT — not SUBJECT_MISSING); foundry `--live` still **open**/`BLOCKED`
(prior exit **1** `live executor not configured`). Hop-5 aggregate
**PARTIAL_ALIVE**. **Antifalsifier:** `requireCleanRoom: false` exit 0 ≠
Jobs/C4 `ALIVE`; adapter exit 0 while foundry `--live` open ≠ hop-5 crown
`ALIVE`; fold tip `3d9da101` false crown claim demoted. Bounded refusal
means: missing, stale, or mismatched layout digests fail closed with a typed
remediation refusal; POWL/OCEL remediations must not escalate to ambient DO
or silent admit. Unmounted sibling checkout refuses ALIVE promotion
(`REFUSED_ZCODE_CHECKOUT_UNMOUNTED`); wrong subject (including hop-4
`ggen_igniter`) refuses retarget (`REFUSED_ZCODE_SUBJECT_MISMATCH`). Do
**not** promote hop 5 / path crown to `ALIVE` from package presence,
`layout_court` exit 0, or spatial-only adapter alone. Do **not** restate
mix SUBJECT_MISSING after layout_court PRESENT + spatial PASS.

---

## What would promote the path (and what would not)

| Claim | Required evidence | Not sufficient |
|-------|-------------------|----------------|
| Hop 1 `ALIVE` | Product perimeter + ggen fence both execute with typed admit/refuse | Vocab constants alone |
| Hop 3 `ALIVE` as photolithography | ggen seal **and** consumer (zcode/igniter) consumes `AotSealReceipt` | `taffy_aot_preflight_court` green alone |
| Hop 3b `ALIVE` | tip-cargo **11 passed; exit 0** @ `ca52e9c16` (≡ tip `120598fb3`); prior evidence `eaf0dad40` **8 passed; exit 0**; tip land `7c0ba2a90`; tip source **11** `#[test]` | Presence alone; inventing tip exit 0 without quiet cargo evidence |
| Hop 4 `ALIVE` | Mounted `ggen_igniter` + **manufacture compare** (tip dual-schema BrandTheme emit vs igniter fixture) exit 0, then `git diff --exit-code` on that comparison subject — **met** 2026-11-22 (`diff_exit=0`; reconfirm `/tmp/cursor/igniter-hop4-manufacture-alive.md` @ `28c3f4d845`; prior `igniter-real-cleanroom.md`) | Bare post-clone `git diff --exit-code`; GALL `gall_observe_clean_room`; docs citing the sibling; clean-tree checkpoint alone; igniter-fixture-only idempotent emit |
| Hop 5 `ALIVE` | Default live `UiLayoutCourtAdapter` exit 0 **and** `powl-foundry-loop --live` exit 0 against pinned ggen tip with `ggen-layout-court` present | In-process bun alone; hop-3b fence alone; `ggen-layout-court` **PRESENT** alone; `requireCleanRoom: false` exit 0; adapter exit 0 while foundry `--live` open |
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
# in seanchatmangpt/ggen_igniter (hop 4) — checkpoint only (≠ manufacture ALIVE)
cd /home/ubuntu/ggen_igniter && git diff --exit-code

# hop-4 BrandTheme manufacture compare — ALIVE (2026-11-22; diff_exit=0):
# tip dual-schema docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml
#   → BrandTheme emit vs igniter test/fixtures/brand_theme/fourth-watch.brand.toml
#   → git diff --exit-code on compare subject → 0
# evidence: /tmp/cursor/igniter-hop4-manufacture-alive.md (reconfirm @ 28c3f4d845)
# prior: /tmp/cursor/igniter-real-cleanroom.md
# antifalsifier: bare git diff --exit-code post-clone ≠ ALIVE
# residual: mix ggen_igniter.sync --check (NIF) still BLOCKED

# in seanchatmangpt/zcode-cli (hop 5) — default adapter exit 0 (Jobs/C4 deferred); foundry --live open
bun test test/ui-layout-court-chicago.test.ts
# expect 31 passed; exit 0 (incl. mix ABSENT + layout_court PRESENT → exit 0)
# live: UiLayoutCourtAdapter.verifyPostTransition(<ggen tip>) → 0
# next live crown: bun scripts/powl-foundry-loop.ts --live → 0 (needs live executor / mix)
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
`6fdcf4665d52931f7de25909366127bc9128a230`: `layout_exit=0`; default
`adapter_exit=2` (`mix` SUBJECT_MISSING) → hop 5 **PARTIAL** (demoted fold tip
`3d9da101` false `PARTIAL_ALIVE`/adapter 0; foundry `--live` still open); path
crown remains **PARTIAL**.
Hop-4 manufacture ALIVE 2026-11-22 on `cursor/v2711-hop4-manufacture-bc54`
onto tip base `3ec8afd8538d1dd6b6d39b8efc41e3a7448166c7`: dual-schema tip
`fourth-watch.brand.toml`; BrandTheme compare `git diff --exit-code` → **0**
(`/tmp/cursor/igniter-real-cleanroom.md`; `bc-4861c787`); prior PARTIAL exit
**2** `:no_primitive_space` closed; antifalsifier retained for bare post-clone;
`mix sync` NIF residual **BLOCKED**. Hop-4 manufacture **ALIVE** reconfirm
2026-11-22 on `cursor/v2711-hop4-manufacture-alive-bc54` onto tip
`28c3f4d84596fc1a0735d268f51bc2c74db526b5` (emitter `99255d53c` + dual-schema
`88e84d545` ancestors): tip BrandTheme `exit=0`, canon `diff -rq=0`,
compare-subject `git diff --exit-code=0` (`/tmp/cursor/igniter-hop4-manufacture-alive.md`;
`bc-b528ad50`). Hop-4 primitive-space reconfirm 2026-11-22 on
`cursor/v2711-hop4-primitive-space-bc54` onto tip
`68b2f33c7e88d31075277f41ee46f01eed415782`: dual-schema retained; BrandTheme
compare `diff -rq` **0** + `git diff --exit-code` → **0**; rhythm-only still
`REFUSED :no_primitive_space`; `IGNITER-CLEANROOM-STANDING` manufacture row
corrected from stale REFUSED → **ALIVE**. Path crown remains **PARTIAL**.
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
- Aggregate hop 5 then **PARTIAL**. No path-crown `ALIVE`. Superseded by hop-5 live demotion below (default adapter exit 2).

### Hop-5 live after layout-court #884 (`cursor/v2711-hop5-live-bc54`)

- Observed tip: `704025461a343ba95f4d3c4561825350bf6c9163` (`ggen-layout-court` **PRESENT**; land `d37271961` / merge `6a89beb38` / PR #884). Fold tip `3d9da101` claimed `PARTIAL_ALIVE` (adapter 0) — **demoted**.
- Host load &gt;10 / thrash → **no new cargo**; used prebuilt `target/debug/layout_court` (`layout_exit=0`).
- Default `UiLayoutCourtAdapter.verifyPostTransition(/agent/repos/ggen)` → exit **2** (`adapter_exit=2`) — Taffy+Mode B PASS; clean-room `mix` **SUBJECT_MISSING**.
- **Antifalsifier:** prior false `PARTIAL_ALIVE` / adapter exit **0** demoted — `requireCleanRoom: false` exit 0 ≠ default-adapter `ALIVE`.
- In-process: `bun test test/ui-layout-court*.ts` → **35 passed; exit 0** (**ALIVE**).
- Aggregate hop-5 standing: **PARTIAL**. Receipt: `/tmp/cursor/hop5-live-after-884.md`. No path-crown `ALIVE`.

### Hop-5 mix SUBJECT_MISSING fix (`cursor/v2711-hop5-mix-subject-bc54`)

- Observed tip (retip): `28c3f4d84596fc1a0735d268f51bc2c74db526b5` (`origin/cursor/ecosystem-spec-27-1-1-bc54`; hop-5 absorbed on tip; ancestor `5f28d0f90`).
- Diagnosis: default adapter exit **2** after Taffy+Mode B PASS was `mix` SUBJECT_MISSING, not missing `layout_court`.
- Fix (zcode-cli @ `fed6f166`): `resolveCleanRoomSyncCmd` — ggen dry-run fallback; spatial-only exit **0** when layout_court PRESENT + mix/ggen ABSENT; typed remediation when subject truly ABSENT.
- Evidence (reconfirm): `layout_exit=0`; default `adapter_exit=0` (Jobs/C4 deferred diagnostic); Chicago **31 passed; exit 0**; see [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md).
- Hop-5 aggregate remains **PARTIAL_ALIVE** (foundry `--live` **foundry_exit=1** / **REFUSED** `Model access is not configured`; runtime PRESENT; Jobs/C4 open — next: provider creds then `bun scripts/powl-foundry-loop.ts --live` → 0).
- Do **not** invent hop-5 / path-crown `ALIVE`. **Tip-cargo `ui_layout` ALIVE** (**11 passed; exit 0** @ `ca52e9c16` ≡ tip `120598fb3`); path crown still **PARTIAL**.

### Gate1 photo retip2 (`cursor/v2711-gate1-photo-retip2-bc54`)

- Observed tip: `d05a131c7521d5a6325ade2e8295fee650659d5a` (past ~`28c3f4d845`).
- Honesty pin: Truex **ALIVE** @ `68d2d233`; hop-4 manufacture **ALIVE** (`diff=0`); hop-5 **PARTIAL_ALIVE** (adapter **0**; `--live` open / model access **REFUSED**); brand.toml hot-swap **PRESENT** / production **UNSUPPORTED**; ui_layout tip-cargo **ALIVE**; FM-PACK **PRESENT**/**PARTIAL_ALIVE** (inventory); cos **BLOCKED** (Gate-0 / Foundry); Gate1 overall **PARTIAL**.
- Path crown remains **PARTIAL**. No cargo. No new `ALIVE` invented.
### Foundry standing retip (`cursor/v2711-foundry-standing-retip-bc54`)

- Observed tip: `d05a131c7521d5a6325ade2e8295fee650659d5a` (`d05a131c7`).
- Honest path: hops 1–3 **PARTIAL_ALIVE** (unchanged); hop-4 **ALIVE**; hop-5 **PARTIAL_ALIVE**; `mix sync` NIF **BLOCKED**.
- Exact next BLOCKED/PENDING: NIF → exclusive `CXX=g++-13` then `mix ggen_igniter.sync --check` → **0**; ui_layout tip-cargo **ALIVE** (**11 passed; exit 0** @ `ca52e9c16`); foundry `--live` → provider creds then `bun scripts/powl-foundry-loop.ts --live` → **0**.
- No cargo on this retip. No path-crown / zero-HITL `ALIVE`.

### Photo hop-4 retip absorb #931 (`cursor/v2711-photo-hop4-retip-bc54`)

- Observed tip: `89ca2da9c37d7ba61abd665fe5c8de176201ae33` (~`2fd897141+` / `46a026ae9+`; prior pin `120598fb3`; ui_layout tip-cargo **ALIVE** preserved).
- Hop-4 BrandTheme manufacture **ALIVE** reconfirmed **without cargo**: tip `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` still dual-schema (`[rhythm]` + DTCG `[primitive.space]`); `crates/ggen-graph/src/layout/brand.rs` still exports `emit_brand_theme_dtcg` / `map_rhythm_brand_for_igniter`; emitter `99255d53c` + dual-schema `88e84d545` + manufacture tip `28c3f4d845` ancestors of live tip.
- Prior manufacture evidence cited (not re-run Mix): `/tmp/cursor/igniter-hop4-manufacture-alive.md` (`bc-b528ad50`) — tip BrandTheme exit **0**, canon `diff -rq` **0**, compare-subject `git diff --exit-code` → **0**; `/tmp/cursor/igniter-real-cleanroom.md` (`cursor/v2711-hop4-primitive-space-bc54`) — same `diff_exit=0`.
- Hop-5 remains **PARTIAL_ALIVE** (adapter **0**; foundry `--live` **foundry_exit=1** / **REFUSED** `Model access is not configured`). Path crown **PARTIAL**. No new `ALIVE` invented. No cargo.
- Exact next residual: exclusive `CXX=g++-13` NIF → `mix ggen_igniter.sync --check` → **0**; hop-5 crown: provider creds → `bun scripts/powl-foundry-loop.ts --live` → **0**.

### Taffy preflight absorb #930 (`cursor/v2711-taffy-preflight-retip-bc54`)

- Observed tip: `89ca2da9c37d7ba61abd665fe5c8de176201ae33`.
- `git ls-tree` confirms `crates/ggen-graph/tests/taffy_aot_preflight_court.rs` blob `d8923b07e` (**7** `#[test]`) on tip — standing **PRESENT**.
- Tip-cargo **SKIPPED** (tip-exclusive ui_layout thrash). Historical evidence exit **0** @ `0478fde59` retained. No tip-cargo `ALIVE`. ui_layout tip-cargo **ALIVE** preserved.
- Exact next: quiet exclusive `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test taffy_aot_preflight_court --target-dir /tmp/ggen-taffy-preflight-target` → **0**.

### Hop-4 mix/NIF residual honesty (`cursor/v2711-hop4-mix-nif-bc54`)

- Base tip: absorb onto `c11ea7f2a` (~`89ca2da9c+` / `2fd897141+` / `46a026ae9+`). Ledger: [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md). ui_layout tip-cargo **ALIVE** preserved.
- Default PATH `mix`/`elixir` **ABSENT**; off-PATH elixir-install `mix` **PRESENT** without founder HITL; apt 1.14 **UNSUPPORTED**; pin 1.20.4-otp-29 **ABSENT** (installable).
- NIF docs **PRESENT**; exclusive `CXX=g++-13` → `mix ggen_igniter.sync --check` still **BLOCKED** (prior `oxrocksdb-sys` SIGTERM). No cargo thrash.
- Exact next: quiet exclusive pod → `CXX=g++-13` NIF → `mix ggen_igniter.sync --check` → **0**.
