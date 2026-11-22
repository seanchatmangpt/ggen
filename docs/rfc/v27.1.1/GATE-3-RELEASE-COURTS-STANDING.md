# Gate 3 — Seven release courts standing (GA 2027-01-01)

| Field | Value |
|-------|-------|
| Document ID | `GATE-3-RELEASE-COURTS-STANDING` |
| Binding | `ECOSYSTEM-SPEC-27.1.1` §2 (D1–D7) + §3 Gate 2–3 (“7 release courts”) |
| Target GA | **2027-01-01** |
| Observed base | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Companion tip (Gate-1 land) | `cursor/ecosystem-spec-27-1-1-bc54` @ `22d108f54fe78c9d66bb4ba1c3b24597a8e05e15` (PR #849) |
| Materialized | 2026-11-22 |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` |
`BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `OPEN / UNKNOWN`.

**Law:** no row is `ALIVE` without a named verifier that exited 0 on an exact
subject. Source presence, workflow YAML, and inventory prose alone never promote
to `ALIVE`. F = 0.

Identity of the seven courts: ECOSYSTEM-SPEC-27.1.1 enumerates exactly **seven**
core release deltas (D1–D7) and schedules “7 release courts” under Gate 2–3
(owner: release governance). This ledger binds one release court per delta.
Risk IDs R-01…R-07 are the **subjects** of court RC-D7, not seven additional
courts (see sibling R-01…R-07 resolution work).

---

## Standing table (seven courts)

| Court | Subject (ECOSYSTEM-SPEC delta) | Standing | Evidence (observed) | Next exact verifier |
|-------|--------------------------------|----------|---------------------|---------------------|
| **RC-D1** | Zero-DOM WebGPU canvas Mode A + FFB Mode B (D1) | **UNSUPPORTED** | No live Zero-DOM / WebGPU / FFB crate surface under workspace members (`REPO-INVENTORY-27.1.1.md`) | Sibling UI-plane court that paints from an admitted Taffy AOT receipt and exits 0; refuse ambient DOM/DO without BRCE+AS-01 |
| **RC-D2** | Taffy AOT pre-flight — widths `320/375/414/768/1024/1440` + WCAG 1.4.12 @ **1.5×** (D2) | **PARTIAL_ALIVE** | Gate-1 land on #849: `ggen-graph::layout::taffy_aot` six-width seal + `tests/taffy_aot_preflight_court.rs` present; photolithography / consumer boundary still open | `cargo test -p ggen-graph --test taffy_aot_preflight_court` (replay); then zcode-cli/igniter consume of `AotSealReceipt` for GA `ALIVE` |
| **RC-D3** | Truex admits W3C SHACL 1.2 `shui:` (`http://www.w3.org/ns/shacl-ui/`) (D3) | **PARTIAL_ALIVE** (ggen fence) / **PARTIAL_ALIVE** (Truex product admit-court; not `main`) | ggen: `vocab::shui` + `shui_public_prefix_court` (evidence tip `0478fde59`); Truex sibling Chicago admit-court **6/6 vitest exit 0** @ `truex@e28e68300` (`cursor/v2711-truex-admit-court-bc54` / truex#6) — not full product `ALIVE` until merge to truex `main` + tip-sync ggen fence | ggen: `cargo test -p ggen-graph --test shui_public_prefix_court`; Truex: merge [#6](https://github.com/seanchatmangpt/truex/pull/6) → `main`; replay `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 |
| **RC-D4** | DTCG 2025.10 brand manifests — `fourth-watch.brand.toml`, `cindycat.brand.toml` (D4) | **PARTIAL_ALIVE** (rhythm) / **UNSUPPORTED** (full token+CASTLE inventory) | `layout::brand` + fixtures under `docs/rfc/v27.1.1/fixtures/` + `brand_toml_rhythm_court` on #849; color/type/CASTLE inventories not admitted | `cargo test -p ggen-graph --test brand_toml_rhythm_court`; Gate 2+ CASTLE/token inventory court once sources are admitted |
| **RC-D5** | zcode-cli `ui-layout-court.ts` adapter (D5) | **PARTIAL** (in-process sibling **ALIVE**) / live **BLOCKED**; in-repo ggen fence **ALIVE** (evidence only) | In-repo: `ggen_graph::layout::ui_layout` + `ui_layout_court` (**8 passed; exit 0** evidence `eaf0dad40`); fixture stub `docs/rfc/v27.1.1/fixtures/ui-layout-court.ts`; `ggen-layout-court` package **PRESENT** on tip. Live `UiLayoutCourtAdapter` / foundry `--live` still open (see [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5) | Live: `UiLayoutCourtAdapter.verifyPostTransition(/agent/repos/ggen)` → exit 0; in-process: `bun test test/ui-layout-court.test.ts` → exit 0; ggen fence replay: `cargo test -p ggen-graph --test ui_layout_court` |
| **RC-D6** | Plane 5 front doors — `revops.fnox.io`, cos/FinFab, Fourth Watch + CindyCat (D6) | **UNKNOWN** / **OUT OF SCOPE** (ggen DO) | Fleet census names only; Plane 5 DO is BRCE+WebAuthn AS-01 only — not claimable from ggen evidence | External Plane-5 / hosting court with AS-01-gated DO receipt; do not promote from this repo |
| **RC-D7** | Unresolved risks **R-01 … R-07** hard GA gate (D7) | **OPEN / UNKNOWN** (all seven) | Spec §11: risk bodies not mounted; “closed” without typed evidence is `REFUSED_UNGROUNDED` | Per-risk typed receipt **or** typed refusal before GA; materialize bodies from admitted risk register first (sibling R-01…R-07 resolution courts) |

---

## Promotion rules (through GA)

1. **RC-D2 / RC-D3 / RC-D4** may advance from `PARTIAL_ALIVE` → `ALIVE` only when
   the named verifier exits 0 **and** the GA consumer boundary named in the
   “Next exact verifier” cell also exits 0 (zcode/Truex/CASTLE as applicable).
2. **RC-D1** stays `UNSUPPORTED` until the UI-plane sibling mounts a court.
   **RC-D5** may advance from `PARTIAL` → `ALIVE` only when live
   `UiLayoutCourtAdapter` (or admitted foundry `--live`) exits 0 on the pinned
   tip; in-repo ggen fence / in-process bun alone never promote RC-D5 crown.
3. **RC-D6** remains non-`ALIVE` from ggen forever unless a BRCE+AS-01 DO receipt
   is supplied by the Plane-5 owner and cited here with command + exit.
4. **RC-D7** refuses silent closure: inventing R-0N prose is `REFUSED_UNGROUNDED`.
5. Any claim of crown Gate-3 `ALIVE` while any row above is not `ALIVE` (or an
   admitted typed refusal for that row) is **REFUSED**.

---

## Replay (this ledger)

```text
# Confirm binding + seven-row table exist
test -f docs/rfc/v27.1.1/GATE-3-RELEASE-COURTS-STANDING.md
rg -n '^\| \*\*RC-D[1-7]\*\*' docs/rfc/v27.1.1/GATE-3-RELEASE-COURTS-STANDING.md

# Optional: replay Gate-1 PARTIAL_ALIVE courts when #849 (or merge) is checked out
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
```

---

## Provenance

Materialized as the Gate-3 release-governance standing slice for
ECOSYSTEM-SPEC-27.1.1. Standings observed against `origin/main` and the Gate-1
companion tip cited above. Audit retip on `cursor/v2711-ecospec-audit-bc54`
(against tip `d11758cf4`): RC-D3 Truex product → **PARTIAL_ALIVE** (sibling
admit-court); RC-D5 → **PARTIAL** / live **BLOCKED** (aligned with photo hop 5 /
GATE-1). No Plane 5 / cos / GCP / Zero-DOM / Truex-`main` / R-0N / RC-D5-live
row is asserted `ALIVE`.
