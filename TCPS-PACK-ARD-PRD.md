# TCPS Pack — Architecture & Product Requirements (Level 5)

**Document Type**: ARD (Architecture Requirements Document) + PRD (Product Requirements Document)
**Status**: DRAFT / READY FOR IMPLEMENTATION
**Date**: 2026-07-19
**Author**: Claude Code (Planning Phase)
**Target Audience**: Implementation agents

---

## Executive Summary

`docs/packs/PACK_MATURITY_MODEL.md`'s own calibration rule states plainly: *"nothing in
`packs/` today exceeds L2 on any dimension. No pack may be scored above L2 without a named
artifact proving it."* L5 means *"pack + ggen alone build, verify, and evolve that entire
part of the system. A consumer with zero knowledge of the target's implementation gets a
finished, tested, receipted subsystem."* Every pack in this repo has fallen short of that
bar for the same structural reason: there was never a real, independent, already-finished
reference implementation to check generated output against — every fidelity claim reduced
to "verified by hand, once."

That reason does not apply here. Sean Chatman supplied two real, finished, self-authored
zips this session:

- **`豊田コード生産方式_v26.7.19.zip`** ("structure definition version") — a self-contained,
  `no_std`, zero-dependency, `#![forbid(unsafe_code)]` Rust crate translating the *original*
  Toyota Production System vocabulary directly into Japanese-named Rust types and typestate
  state machines, by explicit design choice (its own `README.md`: *"原初の豊田生産方式を英語の
  一般的なソフトウェア語彙へ翻訳せず"* — "not translating the original Toyota Production System
  into generic English software vocabulary"). 24 modules under `src/`, one real `試験.rs`
  test module.
- **`..._製品版_v26.7.19_Production_Source.zip`** ("product version") — the same core
  (`tcps-core`) plus `tcps-std`/`tcps-ffi`/`tcps-wasm`/`tcps-cli`, real CI (7 GitHub Actions
  workflows + GitLab CI + Azure Pipelines), a cross-target support matrix, packaging
  manifests for 6 ecosystems, and SBOM/in-toto/SPDX evidence.

This document specifies **two ggen packs** (`tcps-core-pack`, `tcps-release-pack`) that
*generate* a TCPS-shaped project from RDF, using the two zips as **checked-in, read-only
conformance fixtures** — never a vendored dependency, never hand-edited. The headline
mechanism that makes L5 achievable here, not aspirational: **the reference's own `試験.rs`
test file, copied in byte-identical, passes unmodified against the generated code.** A
verbatim third-party test file passing against ontology-generated output is a stronger
fidelity proof than any hand-verification this repo has produced before.

**Success Criteria**: both packs sync cleanly into a new example project; the reference's
own real tests pass against generated code with zero modification; regeneration is
idempotent; pack-shapes gates refuse malformed ontology on both packs; no `ggen-engine`
changes are required.

---

## 1. Architecture & Pack Design

### 1.1 Directory Structure

```
packs/tcps-core-pack/
├── pack.toml
├── ontology.ttl                  # tcps: vocabulary + 24 Module individuals
├── shapes.ttl                    # SHACL: no dangling tcps:dependsOnModule
├── templates/
│   ├── lib.rs.tmpl                # #![no_std] header + #[path]/pub use list
│   ├── 語彙.rs.tmpl                # one .rs.tmpl per reference module (24 total —
│   ├── 自働化.rs.tmpl              #  pattern described once in §2, not enumerated
│   └── ...                        #  file-by-file here)
└── reference/                    # verbatim copy of 豊田コード生産方式_v26.7.19/ —
    └── 豊田コード生産方式_v26.7.19/   #  conformance oracle, NEVER pack output, NEVER hand-edited

packs/tcps-release-pack/
├── pack.toml
├── ontology.ttl                  # tcps: Platform/Tier/PackagingTarget/CiWorkflow individuals
├── shapes.ttl                    # SHACL: every PackagingTarget has a manifestPath, etc.
├── templates/
│   ├── ci_workflow.yml.tmpl
│   └── packaging_manifest.tmpl
└── reference/                    # curated subset of the product zip (§1.3)

examples/tcps-generated/
├── ggen.toml                     # wires both packs as path deps, aggregate_modules=true
├── schema/domain.ttl             # empty/minimal — see §7 "zero consumer ontology" claim
├── Cargo.toml
├── src/lib.rs                    # single include!("ggen_pack_mods.rs") line
└── tests/tcps_conformance_e2e.rs # new, Chicago TDD, §4
```

This mirrors `examples/receiptctl`'s established convention exactly: packs wired as path
deps in `[packs]`, `[templates] aggregate_modules = true` for the single-`include!`
consumer surface (`examples/receiptctl/src/lib.rs`: `include!("ggen_pack_mods.rs");`),
project-specific facts (here: none) supplied via the consumer's own `schema/domain.ttl`.

### 1.2 Reference Fixtures Are Data, Not Dependencies

The two zips are extracted into `reference/` subdirectories as plain files — never added
to any `Cargo.toml`, never a workspace member, never `path = "..."` in any manifest. They
are read by:
1. **Pack authors** (this implementation), to transcribe real types/functions into
   `ontology.ttl`.
2. **The e2e test** (`tcps_conformance_e2e.rs`), to diff generated output's structure
   against them and to copy `試験.rs` in byte-identical as the generated proof suite.

This is the one point most likely to be misread by an implementation agent: **do not**
add `tcps-core` as a crate dependency anywhere in the ggen workspace. See §7.

### 1.3 `tcps-release-pack` Reference Scope

Full 1:1 transcription of the product zip's ~120 files is out of scope this round (named
in §9, not silently dropped). The curated subset covers one representative artifact per
category, proving the pattern:

| Category | Reference file(s) |
|---|---|
| CI workflow | `.github/workflows/{tier1,verify,security}.yml` |
| Supply-chain policy | `deny.toml` |
| Packaging manifest | `packaging/{deb/control.in,npm/package.json.in,nuget/tcps.nuspec.in}` |
| Target tiers | `targets/tier1.txt`, `targets/tier2.txt` |

---

## 2. Ontology → Generated-Module Matrix (`tcps-core-pack`)

Every reference module becomes one `tcps:Module` individual. Columns: reference source
(fully read this session, byte counts confirmed), the ontology individual, the template,
and the specific real behavior that must round-trip byte-for-byte through `試験.rs`.

| Reference module | Real content (read in full) | `tcps:Module` individual | Must survive round-trip via `試験.rs` |
|---|---|---|---|
| `語彙.rs` (the vocabulary base every other module depends on) | `有無<値>`/`成否<成功,失敗>`/`真偽` enums, 15 numeric type aliases (`数量=u32`, `時刻=u64`, ...), `小さい方`/`大きい方` const fns | `tcps:VocabModule` | (no direct test; every other module depends on it) |
| `原点.rs` (729 B) | `原点` enum (4 variants: 誰かのために/人を楽にする/人を中心に置く/良い品を早く安く届ける), `生産目的` struct + `新規()` ctor | `tcps:OriginModule` | `原初形は二本柱を閉じる` (indirectly, via `豊田生産方式::原初形()`) |
| `系譜.rs` (4045 B) | `人物`/`原問題`/`発明`/`不変条件` enums, `歴史記録` struct, fixed-capacity `系譜台帳<const 上限>` with `空()`/`追加する()`/`件数()`/`取得()`, `原初系譜()` seeding 4 real historical records (豊田佐吉→人力織機, ..., 大野耐一→後工程引取り) | `tcps:LineageModule` | `原初形は二本柱を閉じる`: `方式.系譜.件数() >= 4` |
| `品質.rs` (884 B) | `異常` enum (10 variants), `品質判定` enum, `異常票`/`良品`/`不良品` structs | `tcps:QualityModule` | `異常があれば生産線は停止する` |
| `標準作業.rs` (2612 B) | `作業種別` (7 variants), `作業手順`, fixed-capacity `標準作業<const 上限>` with `新規`/`手順を追加する`/`廃止する`/`現行か` | `tcps:StandardWorkModule` | (exercised via 改善's `標準を更新して再開する`) |
| `自働化.rs` (2979 B) | Typestate `生産線<状態>` with zero-sized state markers `稼働中`/`停止中`/`対策中`/`再開可能`; `作業する(self, 品質判定, 時刻) -> 作業結果` matching `良品完成`/`異常停止` | `tcps:JidokaModule` | `異常があれば生産線は停止する` (headline jidoka test) |
| `かんばん.rs` (1072 B) | `かんばん種別`, `かんばん` struct, `引取り()`/`生産指示へ変える()` | `tcps:KanbanModule` | `引取量と補充量は等しい` |
| `必要時生産.rs` (2179 B) | `後工程要求`/`手持ち`/`補充指示`/`不足`/`作り過ぎ`, `かんばんを発行する`, `引き取る` (quantity-conservation), `作り過ぎを判定する` | `tcps:JitModule` | `引取量と補充量は等しい` |
| `平準化.rs` (1455 B) | `生産枠`, fixed-capacity `平準化箱<const 上限>` with `追加する`/`件数`/`取得` | `tcps:HeijunkaModule` | `平準化箱は固定上限を越えない` |
| `アンドン.rs` (1206 B) | `灯色` (緑/黄/赤), `呼出し`, fixed-capacity `アンドン盤<const 上限>` with `点灯する`/`件数` | `tcps:AndonModule` | (exercised via a fixed-capacity-overflow case, same shape as 平準化's) |
| `改善.rs` (2979 B) | `真因`(8)/`対策`(8) enums, `改善票`, `標準不一致`, typestate transitions `生産線<停止中>::真因対策を登録する` → `対策工程::対策を完了する` → `再開工程::標準を更新して再開する` (validates 旧標準/新標準/親標準 linkage) | `tcps:KaizenModule` | (implicit in the full jidoka→kaizen→resume chain; a 6th e2e assertion may exercise this explicitly) |
| `受領証.rs` (2168 B) | `受領種別` (10 variants: 生産/停止/改善/標準更新/引取り/補充/選択/許可/実行/拒否), `要約値([u8;32])`, `受領証` struct (chain-shaped: `前要約`/`後要約`), `簡易要約()` (a real FNV-1a-derived non-cryptographic hash — its own `資料/立脚点.md` states plainly it is not a cryptographic digest), fixed-capacity `受領台帳<const 上限>` | `tcps:ReceiptModule` | (no direct `試験.rs` case for the hash itself; structural round-trip only) |
| `自動選択.rs` (8357 B, only in the product zip's richer form — the structure-definition zip's version is simpler) | `認知品種`, `測度` (7 weighted fields + `最小値`/`乗法質量`), `候補`, `方策<const 上限>`, `選択要求`, `拒否理由` (7 variants + `番号()`), `選択提案`/`選択結果`, `選択する()` — real eligibility/readiness/quality-scoring selection logic | `tcps:AutoSelectModule` | (own module has no `試験.rs` case in the structure-definition zip; scope this round to the simpler variant, named in §9) |
| `青い川のダム.rs` (2693 B) | `能力札`, `許可済み選択`, `許可拒否` (4 variants), `仲介者::許可する()` (tool-match / expiry / permission-bitmask / policy-digest checks — the select≠authorize≠execute separation named in the READMEs), `実行器` trait | `tcps:AuthorizationDamModule` | (no direct `試験.rs` case; structural + trait-bound round-trip) |
| `全体.rs` (1972 B) | `自働化柱`/`必要時生産柱` structs (the two-pillars record), `豊田生産方式` aggregate, `原初形()`, `二本柱が閉じている()` | `tcps:WholeModule` | `原初形は二本柱を閉じる` |
| `現地現物.rs` (728 B) | `観察種別` (5 variants), `現地観察`, `観察立脚点` enum + `現地確認済みか()` | `tcps:GenchiGenbutsuModule` | (no direct case; structural) |
| `ポカヨケ.rs` (1265 B) | `防止条件<入力>` trait, `範囲防止`(range check)/`一致防止`(bitmask check) impls | `tcps:PokaYokeModule` | (no direct case; structural + trait-bound round-trip) |
| `五回なぜ.rs` (1155 B) | Fixed-size-5 `五回なぜ` with `追加する`/`件数`/`最後` | `tcps:FiveWhysModule` | (no direct case; structural) |
| `人間中心.rs` (641 B) | `人の役割` (5 variants), `人間中心判定` + `成立する()` (4-flag AND) | `tcps:HumanCenteredModule` | (no direct case; structural) |
| `ムリムラムダ.rs` (1073 B) | `生産損失`(3)/`ムダ種類`(7), `損失記録`, `作り過ぎ記録()` | `tcps:MuriMuraMudaModule` | (no direct case; structural) |
| `安全.rs` (534 B) | `安全状態`, `安全停止`, `安全を確認する()` | `tcps:SafetyModule` | (no direct case; structural) |
| `タクト.rs` (801 B) | `稼働可能時間`/`顧客必要数`/`必要数なし`/`タクト時間`, `タクトを計算する()` = `稼働可能時間.時間 / 顧客必要数.数量` with a zero-guard | `tcps:TaktModule` | `顧客必要数からタクトを計算する` (headline arithmetic test) |
| `価値流.rs` (1556 B) | `工程関係`, fixed-capacity `価値流<const 上限>` with `接続する`/`次工程`(graph traversal) | `tcps:ValueStreamModule` | (no direct case; structural) |
| `工程能力.rs` (987 B) | `工程能力`/`能力要求`, `能力超過`(3 variants), `能力を判定する()` (3-way threshold check) | `tcps:ProcessCapabilityModule` | (no direct case; structural) |

**Pattern applied to all 24 rows** (described once, not repeated per-file in templates):
each `tcps:Module` individual carries `tcps:hasType` (→ struct/enum individuals with
`tcps:field`/`tcps:variant` children matching the real declarations above verbatim),
`tcps:hasFunction` (→ name/params/a small real-expression vocabulary sufficient for these
modules' actually-simple bodies — division, bitmask AND, match-arms, saturating
arithmetic, fixed-array bounds checks), and `tcps:dependsOnModule` edges reproducing every
real `use crate::X::Y` line (e.g. `自働化` → `品質`, `語彙`; `改善` → `品質`, `標準作業`,
`自働化`, `語彙`). `templates/lib.rs.tmpl` reproduces `src/lib.rs`'s real
`#![no_std]`/`#![forbid(unsafe_code)]`/`#![deny(missing_debug_implementations)]` header and
the exact `#[path]`/`pub use` list, in the same order.

---

## 3. L5 Dimension → Acceptance Criteria Matrix

Per `docs/packs/PACK_MATURITY_MODEL.md` Matrix 1. Every dimension below names one concrete
artifact — no dimension is scored from prose alone.

| Dimension | L5 definition (verbatim from the maturity model) | This pack pair's artifact |
|---|---|---|
| **Generation depth** | "The entire crate surface — types, logic, tests, docs — precipitates from RDF." | All 24 reference modules transcribed to `ontology.ttl`/templates, not a subset (§2's full table is the artifact). |
| **Handler-gap size** | "Zero handler gap: behavior itself is specified in the ontology." | Every function body in §2 is captured in the ontology's expression vocabulary, not left as a hand-written stub the consumer must fill in. |
| **Ontology expressiveness** | "A different template set could regenerate an equivalent system." | `tcps:hasFunction`'s expression vocabulary is structural (match arms, arithmetic, bounds checks), not string-templated Rust source — a different renderer (e.g. a future C backend) could consume the same triples. |
| **Consumer effort** | "Consumer wires `ggen.toml`. Done." | `examples/tcps-generated`'s `schema/domain.ttl` supplies zero project-specific facts — see §1, §7. |
| **Test generation** | "Generated proof suite is sufficient evidence on its own — passing it certifies the subsystem." | **The headline artifact**: the reference author's own `試験.rs`, copied byte-identical (verified via checksum, not eyeballed), passes with 0 failures against generated code (§4 case 2). |
| **Regeneration lifecycle** | "Regen is the only maintenance verb; drift is impossible by construction." | Sync-twice-diff idempotency (§4 case 4); the fixture itself is the drift detector — any future edit to `ontology.ttl` that stops matching the checked-in reference is caught by case 1's structural diff, not by memory. |
| **Target-API fidelity** | "Pack tracks the target's ontology, not its API — fidelity is definitional, not checked." | Reworded honestly for what's actually proven this round: fidelity against the pinned v26.7.19 reference is checked, not definitional-by-construction — a real, strong, but explicitly bounded claim (see §9 for what "definitional" would still require: tracking the reference past v26.7.19). |

---

## 4. Test Strategy (Chicago TDD)

`examples/tcps-generated/tests/tcps_conformance_e2e.rs`, reusing `CliHarness`/`TempDir`
conventions from `reflexive_law_e2e.rs`/`framework_packs_e2e.rs`. No mocks; every
assertion is against a real synced project on disk.

```rust
#[test]
fn generated_module_list_matches_reference_structurally() {
    // sync the example, then:
    let generated: Vec<String> = list_rs_files(&project.join("src"));
    let reference: Vec<String> = list_rs_files(
        &pack_dir.join("reference/豊田コード生産方式_v26.7.19/src"));
    assert_eq!(generated, reference, "module set must match the real reference 1:1");
}

#[test]
fn reference_test_suite_passes_unmodified_against_generated_code() {
    // Copy the reference's own 試験.rs into the synced project's tests/ verbatim
    // (byte-for-byte — assert a checksum match against the reference file before
    // copying, so a future accidental edit to the fixture is caught here first).
    // Then: cargo test inside the synced project.
    let output = CliHarness::cargo_bin("cargo")
        .args(["test"])
        .current_dir(&project)
        .run()
        .expect("cargo test on synced project");
    output.assert_success();
    // This is the headline L5 fidelity proof — see §3.
}

#[test]
fn no_std_and_safety_attributes_survive_regeneration() {
    let lib_rs = std::fs::read_to_string(project.join("src/lib.rs")).unwrap();
    assert!(lib_rs.contains("#![no_std]"));
    assert!(lib_rs.contains("#![forbid(unsafe_code)]"));
    assert!(lib_rs.contains("#![deny(missing_debug_implementations)]"));
}

#[test]
fn sync_is_idempotent() {
    // sync twice, diff -rq src/ — byte-identical, same convention as guard-pack-proofs.
}

#[test]
fn dangling_dependency_edge_refuses_sync() {
    // an ontology with tcps:dependsOnModule pointing at an undeclared module
    // → sync refuses with FM-PACK-013, nothing written.
}

#[test]
fn release_pack_packaging_shape_refuses_missing_manifest_path() {
    // tcps-release-pack's own negative case — independent pack, independent gate.
}
```

---

## 5. Implementation Roadmap

- [ ] **Phase 1** — copy `豊田コード生産方式_v26.7.19/` verbatim into
      `packs/tcps-core-pack/reference/`; author `ontology.ttl`/`shapes.ttl` for all 24
      modules per §2; author the 24 `.rs.tmpl` templates + `lib.rs.tmpl`.
- [ ] **Phase 2** — copy the curated product-zip subset (§1.3) into
      `packs/tcps-release-pack/reference/`; author its `ontology.ttl`/`shapes.ttl`/templates.
- [ ] **Phase 3** — scaffold `examples/tcps-generated/` (wiring both packs, modeled on
      `receiptctl`); write `tcps_conformance_e2e.rs` per §4.
- [ ] **Phase 4** — independently re-run every command in §8 (not just trust a workflow
      agent's self-report); update `docs/packs/PACK_MATURITY_MODEL.md`'s scoring table only
      after the artifacts above exist and pass.

---

## 6. Error Handling

No new error codes. Both packs' negative cases (`dangling_dependency_edge_refuses_sync`,
`release_pack_packaging_shape_refuses_missing_manifest_path`) are ordinary pack-shapes
violations, caught by the existing `FM-PACK-012`/`FM-PACK-013` mechanism (pack `shapes.ttl`
evaluated against the union graph, proven in this session's prior rounds). No
`crates/ggen-engine` changes are required anywhere in this document.

---

## 7. Dependency Graph — the one thing not to get wrong

```
examples/tcps-generated
├── packs/tcps-core-pack       (path dep, ggen.toml [packs])
├── packs/tcps-release-pack    (path dep, ggen.toml [packs])
└── ggen-engine's existing sync pipeline (unmodified)

packs/tcps-core-pack/reference/豊田コード生産方式_v26.7.19/   ← DATA, not a Cargo dependency
packs/tcps-release-pack/reference/...                        ← DATA, not a Cargo dependency
```

Both new packs are ordinary path-dep codegen packs. **Zero new crate dependencies are
added to the ggen workspace's `Cargo.toml`.** The reference zips' contents live under
`reference/` as plain files read by pack authors and by the e2e test — they are never
`path = "..."`'d into any `[dependencies]` table, never a workspace member. An
implementation agent that adds `tcps-core = { path = "..." }` anywhere has misread this
document.

---

## 8. Success Criteria (Definition of Done)

### 8.1 Code Quality
- [ ] `just check` — workspace build clean
- [ ] `cargo fmt --check` on any touched crate (expected: none outside pack/example content)
- [ ] No `crates/ggen-engine` diff

### 8.2 Functional Requirements
- [ ] Both packs sync cleanly into `examples/tcps-generated`
- [ ] All 24 modules generated, matching the reference's file list (§4 case 1)
- [ ] **The reference's own `試験.rs` passes unmodified against generated code** (§4 case 2
      — non-negotiable, the document's central claim)
- [ ] `no_std`/`forbid(unsafe_code)`/`deny(missing_debug_implementations)` present in
      generated `lib.rs` (§4 case 3)
- [ ] Idempotent regeneration (§4 case 4)
- [ ] Both packs' shapes gates refuse their respective negative cases (§4 cases 5–6)

### 8.3 Verification Commands
```bash
cargo test --manifest-path examples/tcps-generated/Cargo.toml --test tcps_conformance_e2e
just check
cargo fmt --check -p <any touched crate>
```

### 8.4 Documentation
- [ ] `docs/packs/PACK_MATURITY_MODEL.md` updated with `tcps-core-pack`/`tcps-release-pack`
      rows, citing this document's artifacts — only after they exist and pass, never before

---

## 9. What Is Named as Out of Scope, Not Smuggled

- `tcps-ffi`/`tcps-wasm`/`tcps-cli`/`tcps-std` (the product zip's other four crates) — real
  follow-on scope.
- Full 1:1 coverage of every CI/packaging file in the product zip (§1.3 covers one
  representative per category this round).
- `自動選択.rs`'s richer product-zip form (measure-weighted candidate selection) — this
  round targets the structure-definition zip's simpler shape only.
- Reasoner-independence (generating + validating under both `praxis-graphlaw` and
  Oxigraph) — not attempted; both packs are ordinary codegen with no law/hook surface, so
  this is a straightforward, separate follow-on.
- Tracking the reference crate past v26.7.19 — inherently longitudinal, same honesty
  convention `PRECISION_LEDGER.md` already uses for other packs' longitudinal dimensions.
- Publishing anything, running any real CI provider, cryptographic (vs. FNV-1a-derived)
  hashing in `簡易要約` — `資料/立脚点.md` (the reference's own standpoint doc) already states
  plainly that production use requires swapping in an audited digest (e.g. BLAKE3); this
  pack pair reproduces the reference's own `簡易要約` faithfully, including that limitation,
  rather than silently "fixing" it into something the reference doesn't actually do.

---

## 10. Worked Examples

**Example A — a function body captured in the ontology (`タクトを計算する`)**

Reference (`src/タクト.rs`):
```rust
pub const fn タクトを計算する(
    稼働可能時間: 稼働可能時間,
    顧客必要数: 顧客必要数,
) -> 成否<タクト時間, 必要数なし> {
    if 顧客必要数.数量 == 0 {
        return 成否::失敗(必要数なし);
    }
    成否::成功(タクト時間 { 周期: 稼働可能時間.時間 / 顧客必要数.数量 })
}
```

Ontology individual (illustrative Turtle, `tcps:hasFunction`):
```turtle
tcps:タクトを計算する
    a tcps:Function ;
    tcps:module tcps:TaktModule ;
    tcps:param [ tcps:name "稼働可能時間" ; tcps:type tcps:稼働可能時間 ] ;
    tcps:param [ tcps:name "顧客必要数" ; tcps:type tcps:顧客必要数 ] ;
    tcps:returns tcps:成否_タクト時間_必要数なし ;
    tcps:guard [ tcps:condition "顧客必要数.数量 == 0" ; tcps:onTrue tcps:失敗_必要数なし ] ;
    tcps:expr [ tcps:op "divide" ; tcps:lhs "稼働可能時間.時間" ; tcps:rhs "顧客必要数.数量" ;
                tcps:bindsTo "周期" ] .
```

Template fragment (`templates/タクト.rs.tmpl`, illustrative):
```tera
pub const fn {{ fn.name }}(
{% for p in fn.params %}    {{ p.name }}: {{ p.type }},
{% endfor %}) -> {{ fn.returns }} {
    if {{ fn.guard.condition }} {
        return {{ fn.guard.onTrue }};
    }
    成否::成功({{ fn.returns_ctor }} { 周期: {{ fn.expr.lhs }} / {{ fn.expr.rhs }} })
}
```

This is the exact function `試験.rs`'s `顧客必要数からタクトを計算する` test exercises —
regenerated output must produce an identical AST shape for that test to pass unmodified.

**Example B — a typestate transition (`自働化`'s jidoka halt)**

Reference: `生産線<稼働中>::作業する(self, 品質判定, 時刻) -> 作業結果`, matching
`品質判定::異常(異常)` to produce `作業結果::異常停止 { 生産線: 生産線<停止中>, 異常票 }`.
Ontology captures the state-marker types (`稼働中`/`停止中` as zero-sized `tcps:hasType`
individuals) and the match arms as `tcps:hasFunction`'s `tcps:matchArm` children — the
same real information the reference's `異常があれば生産線は停止する` test checks
(`結果` must be `作業結果::異常停止`, never `良品完成`, when given `異常(品質不良)`).

**Example C — fixed-capacity generic collection (`平準化箱<const 上限>`)**

Reference: `追加する` returns `成否::失敗(平準化箱満杯)` once `件数 >= 上限`. Ontology
captures `上限` as a `tcps:constGeneric` on the `tcps:StandardWorkModule`... actually
`tcps:HeijunkaModule` type individual, and `追加する`'s bounds-check guard the same way
Example A's `タクトを計算する` guard is captured — this is the same "guard + expr" vocabulary
reused, not a new mechanism per module (per §2's "pattern applied to all 24 rows").

---

## 11. Next Steps for Implementation Agent

1. Clone this document to working memory.
2. Start with **Phase 1** (§5): copy the reference verbatim, author the ontology for all
   24 modules — do not skip modules to save time; a partial module set contradicts §3's
   "Generation depth" artifact claim.
3. Chicago TDD throughout: write `tcps_conformance_e2e.rs`'s cases as you go, not only at
   the end.
4. Run `just check` frequently.
5. Report progress per phase.
6. **Flag blockers immediately** if a reference function's body doesn't fit the ontology's
   guard/expr vocabulary cleanly (e.g. `自動選択.rs`'s `選択する()` has real loop-with-early-
   exit logic that may need a richer expression shape than Example A's) — surface this as a
   named scope question, don't quietly simplify the ontology and let the fidelity claim
   drift from what §3 actually promises.

---

**Document Version**: 1.0
**Status**: Ready for Implementation
