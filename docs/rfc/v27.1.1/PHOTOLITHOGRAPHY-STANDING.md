# Photolithography path — standing ledger (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-STANDING-27.1.1` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed tip (this ledger) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `4b22529df2939412c41107fe5e9eea66ba6a4dad` (`ggen-layout-court` **PRESENT** @ `d37271961`; ui-layout **ALIVE** evidence `eaf0dad40` exit 0 / 8 passed — ancestor; tip court land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING**; live hop-5 tip-exit **PENDING**; hop-4 demotion antifalsifier retained) |
| Hop-5 cross-mount | 2026-11-22 — `cursor/v2711-hop5-standing-bc54` / `/tmp/cursor/hop5-crossmount.md` |
| Status | LEDGER (path standings; **not** a crown `ALIVE` receipt) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Crown rule:** the photolithography path as a whole remains **PARTIAL** / **not**
`ALIVE`. Source presence, workflow definitions, IO-free contracts, and sibling
repo names do not promote the path crown. Hop 4 (igniter) is **PARTIAL_ALIVE**
(mount **PRESENT** + clean-tree checkpoint only — manufacture compare still
open; **not** `ALIVE`; `mix` **ABSENT**). **Antifalsifier:** bare
`git diff --exit-code` on a post-clone / virgin working tree ≠ hop-4 `ALIVE`
(independent verify demoted claim-agent `ALIVE` → `PARTIAL_ALIVE`; VM agent
bare-diff `ALIVE` must **not** re-promote). Hop 5 aggregate is **PARTIAL** —
in-process zcode bun courts **ALIVE** (exit 0); live `UiLayoutCourtAdapter` /
`powl-foundry-loop --live` tip-exit **PENDING** (`ggen-layout-court` **PRESENT** @ `d37271961` — do not invent live `ALIVE`).
The in-repo ggen ui-layout fence remains **ALIVE** on evidence tip `eaf0dad40`
(**8 passed; exit 0**; `bc-c7c9d554`; ancestor of live tip `376b5c309`); tip
court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo reconfirm
PENDING** (quiet cgroup) — not tip exit 0; ALIVE retained (not demoted for
thrash/OOM). That does **not** promote the path crown.

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
| 4 | ggen_igniter clean-room | **PARTIAL_ALIVE** — mount **PRESENT** (`/home/ubuntu/ggen_igniter`, HEAD `b5a15528…`; also `/agent/repos/ggen_igniter`); clean-tree checkpoint `git diff --exit-code` → **0** only. Manufacture crown **open**; `mix` **ABSENT** → live `mix ggen_igniter.sync --check` **BLOCKED**. In-repo contract **PARTIAL_ALIVE**. Do **not** promote to `ALIVE`. | **Out-of-repo** sibling `seanchatmangpt/ggen_igniter` (mounted via public `gh` clone; env.repos still ggen-only). In-repo: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) + `igniter_cleanroom_court` — **not** manufacture crown | **Next (manufacture compare — still open):** on tip `376b5c309` (or pinned tip) run admitted ggen+Taffy AOT manufacture targeting igniter-expected artifacts; byte-compare manufacture output vs igniter expected; then `git diff --exit-code` (or documented byte-identity verifier) on **that** comparison subject → exit **0**. **Antifalsifier:** bare `git diff --exit-code` post-clone ≠ `ALIVE`. Checkpoint only: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. Live next (after mix): `mix ggen_igniter.sync --check` (exit **0**/`4`). In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | Claiming `ALIVE` from bare post-clone `git diff --exit-code` (FALSE ALIVE / demoted); virgin-clone hygiene as manufacture proof; conflating clean-tree checkpoint with ggen tip AOT vs igniter expected; conflating `gall_observe_clean_room` with igniter crown; treating `git diff --exit-code` as Jobs/C4 refuse gate |
| 5 | zcode-cli POWL/OCEL remediations (bounded refusal) | **PARTIAL** — in-process bun `ui-layout-court` **ALIVE** (**35 passed; exit 0**); `ggen-layout-court` **PRESENT** @ `d37271961`; live adapter / foundry `--live` tip-exit **PENDING** (historical ABSENT: adapter exit **2**; foundry live exit **1** — do **not** invent live `ALIVE`) | **Out-of-repo** `zcode-cli` @ `/agent/repos/zcode-cli` (mounted). Package **PRESENT** on tip `4b22529df`. In-repo: [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | Exact next: `cargo run -p ggen-layout-court --bin layout_court -- fixtures/ui-layout-court/product-subject.json` → 0; then `UiLayoutCourtAdapter.verifyPostTransition(/agent/repos/ggen)` → 0; `bun scripts/powl-foundry-loop.ts --live` → 0 | Inventing live `ALIVE` from package presence or in-process bun alone |

**Path standing (aggregate):** **PARTIAL** — hops 1–3 **PARTIAL_ALIVE**, hop 3b
ggen ui-layout **ALIVE** (**8 passed; exit 0** on evidence `eaf0dad40`; tip land
`7c0ba2a90` on tip `07b689c97` / ancestor `c9e224d33`),
hop 4 **PARTIAL_ALIVE** (mount PRESENT + clean-tree only; manufacture compare
open — **not** `ALIVE`; antifalsifier: bare post-clone `git diff --exit-code` ≠
`ALIVE`), hop 5 **PARTIAL** (in-process **ALIVE**; live **BLOCKED** on ABSENT
`ggen-layout-court`) → path crown **non-ALIVE** / incomplete until hop-4
manufacture compare and live hop-5 exit 0 on pinned tip. Pass-count footnotes
(tip source, not tip exit 0): hop-1 `shui` **4**; hop-2 brand **11**; hop-3
taffy **7**; hop-3b ui-layout **11**.


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

### 4 — ggen_igniter clean-room

| Path | Role |
|------|------|
| *(sibling)* `seanchatmangpt/ggen_igniter` | Clean-room / manufacture subject — **out-of-repo**; mount **PRESENT** at `/home/ubuntu/ggen_igniter` (HEAD `b5a15528…`) |
| `git diff --exit-code` | Clean-tree **checkpoint only** (exit **0** observed) — **not** manufacture `ALIVE` |
| Manufacture compare (ggen tip AOT vs igniter expected) | **Still open** — exact next verifier for hop-4 `ALIVE` |
| `docs/reference/gate-conventions.md` | ggen vs ggen_igniter gate scoring orientation |
| `docs/jira/v26.11.4/_LANES.md` | Historical IG-1 dirty-tree note (~98 files) — **not** current standing |
| `justfile` (`sjira-mutual-admit`) / `scripts/sjira_mutual_admit.sh` | Hands-free mutual-admit; Elixir leg skips when igniter/mix absent |
| `crates/ggen-graph/src/bin/gall_observe_clean_room.rs` | In-repo GALL W3 clean-room **observe** — distinct subject |
| `scripts/gall/external/24_run_clean_room_rebuild.sh` | In-repo GALL rebuild script — distinct subject |
| `docs/gall/W5_CLEAN_ROOM_REBUILD.md` | GALL W5 doctrine — distinct from igniter crown |

Standing now: **PARTIAL_ALIVE** (mount PRESENT + clean-tree checkpoint). Do
**not** promote hop 4 to `ALIVE` from GALL clean-room artifacts, from an
unmounted sibling, or from bare post-clone `git diff --exit-code` (antifalsifier;
independent verify demoted that FALSE `ALIVE`). In-repo sync idempotence
(`scripts/ci/guard-pack-proofs.sh` `diff -rq` after `ggen sync run`;
`.github/workflows/*` `git diff --exit-code` patterns; `docs/USAGE_GUIDE.md` CI
example) proves **ggen** regeneration hygiene, not ggen tip AOT vs igniter
expected manufacture.

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
| Hop 3b `ALIVE` | `ui_layout_court` **8 passed; exit 0** on evidence `eaf0dad40` (this ledger; ancestor); tip land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup on `5e00619a4` | Presence alone; inventing tip exit 0 without quiet cargo evidence |
| Hop 4 `ALIVE` | Mounted `ggen_igniter` + **manufacture compare** (ggen tip AOT output vs igniter expected) exit 0, then `git diff --exit-code` on that comparison subject | Bare post-clone `git diff --exit-code`; GALL `gall_observe_clean_room`; docs citing the sibling; clean-tree checkpoint alone |
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

Sibling verifiers:

```text
# in seanchatmangpt/ggen_igniter (hop 4) — checkpoint only (≠ ALIVE)
cd /home/ubuntu/ggen_igniter && git diff --exit-code

# hop-4 ALIVE next verifier (manufacture compare — still open):
# 1) on tip 376b5c309: admitted ggen+Taffy AOT manufacture → igniter-expected artifacts
# 2) byte-compare manufacture output vs igniter expected
# 3) git diff --exit-code (or documented byte-identity) on THAT subject → exit 0
# antifalsifier: bare git diff --exit-code post-clone ≠ ALIVE

# in seanchatmangpt/zcode-cli (hop 5)
bun test test/powl-ocel-chicago.test.ts
pnpm test -- ui-layout-court
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
`ALIVE`); hop 5 **PARTIAL** (in-process ALIVE + live BLOCKED); path crown
remains **PARTIAL**. Pass-count footnotes (tip source): `shui` **4**, taffy
**7**, brand **11**, ui-layout **11**. No path-crown `ALIVE` asserted.

### Hop-5 cross-mount refresh (`cursor/v2711-hop5-standing-bc54`)

- Observed tip: `376b5c3092c821eec942a2e16489b6db45c0b726` (hop-5 evidence tip `07b689c97` / ancestor `c9e224d33`; prior pin `f6cb18f86`).
- In-process: `bun test test/ui-layout-court*.ts` → **35 passed; exit 0** (**ALIVE** observation).
- Live: `ggen-layout-court` **ABSENT**; `cargo run -p ggen-layout-court --bin layout_court` → exit **101**; `UiLayoutCourtAdapter.verifyPostTransition` → exit **2** `BLOCKED`/`SUBJECT_MISSING`; `bun scripts/powl-foundry-loop.ts --live` → exit **1**.
- Receipt: `/tmp/cursor/hop5-crossmount.md`. No path-crown `ALIVE`.

### Honesty retip (`cursor/v2711-foundry-honesty-bc54`)

- Observed tip: `4b22529df2939412c41107fe5e9eea66ba6a4dad`.
- `ggen-layout-court` **PRESENT** @ `d37271961`.
- Live hop-5 tip-exit **PENDING** (no cargo this audit) — package land ≠ live `ALIVE`.
- Path crown remains **PARTIAL**.
