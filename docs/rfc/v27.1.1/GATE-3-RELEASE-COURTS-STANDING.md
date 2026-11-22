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
| **RC-D2** | Taffy AOT pre-flight — widths `320/375/393/600/840/1280` + WCAG 1.4.12 @ **1.5×** (D2) | **PARTIAL_ALIVE** | Gate-1 land on #849: `ggen-graph::layout::taffy_aot` six-width seal + `tests/taffy_aot_preflight_court.rs` present; photolithography / consumer boundary still open | `cargo test -p ggen-graph --test taffy_aot_preflight_court` (replay); then zcode-cli/igniter consume of `AotSealReceipt` for GA `ALIVE` |
| **RC-D3** | Truex admits W3C SHACL 1.2 `shui:` (`http://www.w3.org/ns/shacl-ui/`) (D3) | **PARTIAL_ALIVE** (ggen fence) / **UNKNOWN** (Truex product) | ggen: `vocab::shui` + `shui_public_prefix_court` on #849; Truex sibling not mounted | ggen: `cargo test -p ggen-graph --test shui_public_prefix_court`; Truex: admit-court loading a `shui:`-prefixed shape graph exit 0 (private `truex:` still refused) |
| **RC-D4** | DTCG 2025.10 brand manifests — `fourth-watch.brand.toml`, `cindycat.brand.toml` (D4) | **PARTIAL_ALIVE** (rhythm) / **UNSUPPORTED** (full token+CASTLE inventory) | `layout::brand` + fixtures under `docs/rfc/v27.1.1/fixtures/` + `brand_toml_rhythm_court` on #849; color/type/CASTLE inventories not admitted | `cargo test -p ggen-graph --test brand_toml_rhythm_court`; Gate 2+ CASTLE/token inventory court once sources are admitted |
| **RC-D5** | zcode-cli `ui-layout-court.ts` adapter (D5) | **UNSUPPORTED** | `zcode-cli` not a workspace member; no `ui-layout-court.ts` in-tree | In zcode-cli: exact court runner (e.g. `pnpm test -- ui-layout-court`) against a ggen layout receipt fixture; fail closed on missing/stale digest |
| **RC-D6** | Plane 5 front doors — `revops.fnox.io`, cos/FinFab, Fourth Watch + CindyCat (D6) | **UNKNOWN** / **OUT OF SCOPE** (ggen DO) | Fleet census names only; Plane 5 DO is BRCE+WebAuthn AS-01 only — not claimable from ggen evidence | External Plane-5 / hosting court with AS-01-gated DO receipt; do not promote from this repo |
| **RC-D7** | Unresolved risks **R-01 … R-07** hard GA gate (D7) | **OPEN / UNKNOWN** (all seven) | Spec §11: risk bodies not mounted; “closed” without typed evidence is `REFUSED_UNGROUNDED` | Per-risk typed receipt **or** typed refusal before GA; materialize bodies from admitted risk register first (sibling R-01…R-07 resolution courts) |

---

## Promotion rules (through GA)

1. **RC-D2 / RC-D3 / RC-D4** may advance from `PARTIAL_ALIVE` → `ALIVE` only when
   the named verifier exits 0 **and** the GA consumer boundary named in the
   “Next exact verifier” cell also exits 0 (zcode/Truex/CASTLE as applicable).
2. **RC-D1 / RC-D5** stay `UNSUPPORTED` until the owning sibling repo mounts a
   court; ggen may emit receipts/tokens but cannot self-certify those planes.
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
companion tip cited above. No Plane 5 / cos / GCP / Zero-DOM / zcode-cli /
Truex-product / R-0N row is asserted `ALIVE`.
