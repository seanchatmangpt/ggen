# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

## Gate 0 — out of scope for ggen

| Requirement | Current evidence | Next exact verifier | Owner |
|-------------|------------------|---------------------|-------|
| GCP billing | none in ggen | external billing console / infra receipt | external (infra) |
| servengine M4 | none in ggen | servengine M4 court in that repo | external (servengine) |
| cos ledger fixes | none in ggen | cos ledger replay/verify | external (cos) |
| year-end SOW | none in ggen | commercial SOW register | external (ops) |

Do not promote any Gate 0 row to `ALIVE` from this repository.

---

## Gate 1 — in-repo / sibling (Dec 6–18)

| Requirement | Current evidence | Next exact verifier / command | Owner |
|-------------|------------------|-------------------------------|-------|
| Admit W3C `shui:` as public vocab (ggen fence) | `vocab::shui` + `is_admitted_turtle_prefix("shui")` court (this PR); Truex product admission still external | `cargo test -p ggen-graph --test shui_public_prefix_court` | **ggen** (fence) + **truex** (product admit) |
| Truex runtime admits `shui:` shapes | sibling `seanchatmangpt/truex` not mounted — **UNKNOWN** | in truex: admit-court that loads a `shui:`-prefixed shape graph and exits 0; refuse still holds for private `truex:` | **truex** |
| Taffy AOT pre-flight (widths 320/375/393/600/840/1280 + WCAG 1.4.12 1.5×) | **PARTIAL_ALIVE** — `layout::taffy_aot` six-width seal (`preflight_column_six_widths`) admits valid trees at all six widths under 1.5×; refuses width-local overflow naming the failing width (e.g. 320); touch/gap/overlap typed refusals remain. Photolithography still open until zcode-cli / igniter consume the receipt | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | **ggen** |
| zcode-cli `ui-layout-court.ts` | **UNSUPPORTED** in this repo | in zcode-cli: `pnpm test -- ui-layout-court` (or repo’s exact court runner) against a ggen layout receipt fixture | **zcode-cli** |
| `curve25519-dalek` v4 / cgap-aoq docs | lock has 4.1.3 + 5.0.0; no Gate-1 pin doc | document admitted path; court: `rg -n 'curve25519-dalek' Cargo.lock` + dedicated docs path under `docs/rfc/v27.1.1/` or cgap-aoq tree; refuse undocumented major | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | Affidavit pack **PARTIAL**; size court **UNKNOWN** | build witness wasm then `wc -c verify_witness.wasm` / size court asserting `&lt; 262144` | external (witness/affidavit) with optional ggen pack emit |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **PARTIAL_ALIVE** — `layout::brand` parse/admit + fixtures `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml`; court refuses touch &lt;44, gap &lt;8, off-grid spacing; admits both OEM stubs. Full DTCG color/type/CASTLE inventories remain Gate 2+ | `cargo test -p ggen-graph --test brand_toml_rhythm_court` | **ggen** (rhythm schema) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | Gate 2–3 — not a Gate-1 falsifier | external |

---

## Recommended next single falsifier (Gate 1)

Brand rhythm schema court is green (**PARTIAL_ALIVE**). Replay:

```text
cargo test -p ggen-graph --test brand_toml_rhythm_court
```

Taffy AOT six-width seal court is green (**PARTIAL_ALIVE** — ggen fence only; zcode/igniter still open):

```text
cargo test -p ggen-graph --test taffy_aot_preflight_court
```

Next **new** Gate-1 falsifier to author:

1. Wire zcode-cli `ui-layout-court.ts` (or igniter) to consume `AotSealReceipt` without re-implementing geometry law — photolithography remains non-ALIVE until that boundary executes.
2. Or advance sibling rows still open: Truex `shui:` product admit; `curve25519-dalek` Gate-1 pin doc; `verify_witness` WASM size court.
