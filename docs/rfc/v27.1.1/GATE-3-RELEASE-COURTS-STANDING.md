# Gate 3 — Seven release courts standing (GA 2027-01-01)

| Field | Value |
|-------|-------|
| Document ID | `GATE-3-RELEASE-COURTS-STANDING` |
| Binding | `ECOSYSTEM-SPEC-27.1.1` §2 (D1–D7) + §3 Gate 2–3 (“7 release courts”) |
| Target GA | **2027-01-01** |
| Observed base | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Companion tip (Gate-1 land) | `cursor/ecosystem-spec-27-1-1-bc54` @ `7bb443e99` (PR #849; tip7 live-tip ALIVE + Construct lock sync; Construct full job **PENDING_CI**) |
| Materialized | 2026-11-22 |
| Honesty retip | 2026-11-22 3:13 PM PST — RC-D3 Truex `#6` **MERGED**; Gate1 tip7 fences **ALIVE**; Foundry / Plane-5 / RC-D1 / RC-D7 **held** non-ALIVE |

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
| **RC-D2** | Taffy AOT pre-flight — widths `320/375/414/768/1024/1440` + WCAG 1.4.12 @ **1.5×** (D2) | **ALIVE** (ggen tip7 fence) / GA consumer **PARTIAL** | tip7 live-tip **7**/EXIT:**0** @ `b07cda77c+` (`/tmp/gate1-tip7-taffy.log`); sealed tip-cargo @ `2685119bd`/`63da993cc`. Photolith / zcode-igniter `AotSealReceipt` consume still open for GA crown | Replay: `cargo test -p ggen-graph --test taffy_aot_preflight_court` → **7**/EXIT:**0**; then zcode/igniter consume for GA `ALIVE` |
| **RC-D3** | Truex admits W3C SHACL 1.2 `shui:` (`http://www.w3.org/ns/shacl-ui/`) (D3) | **ALIVE** (Truex admit-court ≡ G2-7) / ggen fence tip7 **ALIVE** | Truex [#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** `b4672131` @ `truex@68d2d233`; Chicago admit-court **6**/EXIT:**0**. ggen `shui_public_prefix_court` tip7 **4**/EXIT:**0**. ≠ Foundry / path-crown `ALIVE` | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0**; `cargo test -p ggen-graph --test shui_public_prefix_court` → **4**/EXIT:**0** |
| **RC-D4** | DTCG 2025.10 brand manifests — `fourth-watch.brand.toml`, `cindycat.brand.toml` (D4) | **ALIVE** (completeness+rhythm+hotswap tip7) / **UNSUPPORTED** (full token+CASTLE inventory) | tip7 brand **5+6+12**/EXIT:**0** (`/tmp/gate1-tip7-brand.log`). Production theme **UNSUPPORTED**. Full DTCG/CASTLE inventory **UNSUPPORTED** | Replay brand courts → EXIT:**0**; Gate 2+ CASTLE/token inventory once sources admitted |
| **RC-D5** | zcode-cli `ui-layout-court.ts` adapter (D5) | **PARTIAL** (ggen tip7 fence **ALIVE**; in-process sibling **ALIVE**) / live **BLOCKED_HITL** | ggen `ui_layout_court` tip7 **11**/EXIT:**0**. zcode POWL/OCEL Chicago replay **11**/EXIT:**0** @ 2026-11-22 3:13 PM PST (`/tmp/zcode-powl-ocel-replay.log`) ≠ foundry `--live`. Live `--live` **foundry_exit=1** | Live: `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** (Founder HITL); non-substitute: `bun test test/powl-ocel-chicago.test.ts` → **0** |
| **RC-D6** | Plane 5 front doors — `revops.fnox.io`, cos/FinFab, Fourth Watch + CindyCat (D6) | **UNKNOWN** (revops NXDOMAIN) / cos **BLOCKED** (503) / **OUT OF SCOPE** (ggen DO) | Probe held: cos `/readyz` **503** `DB_UNAVAILABLE`; revops **NXDOMAIN**. Plane 5 DO is BRCE+WebAuthn AS-01 only | External Plane-5 / hosting court with AS-01-gated DO receipt; do not promote from this repo |
| **RC-D7** | Unresolved risks **R-01 … R-07** hard GA gate (D7) | **BLOCKED** (all seven bodies); contract fence **ALIVE** **7** ≠ bodies | Spec §11: risk bodies not mounted; contract-only antifalsifier ≠ body close | Per-risk typed receipt **or** typed refusal before GA; Founder HITL admit register |

---

## Promotion rules (through GA)

1. **RC-D2 / RC-D3 / RC-D4** fence rows may cite tip7 / Truex-merge `ALIVE` for
   the named ggen/Truex verifier. GA consumer boundary (zcode/igniter consume,
   full DTCG/CASTLE inventory) stays **PARTIAL** / **UNSUPPORTED** until that
   boundary exits 0 — fence `ALIVE` ≠ Gate-3 crown `ALIVE`.
2. **RC-D1** stays `UNSUPPORTED` until the UI-plane sibling mounts a court.
   **RC-D5** may advance from `PARTIAL` → live `ALIVE` only when foundry
   `--live` exits 0 on the pinned tip; in-repo ggen fence / POWL-OCEL bun alone
   never promote RC-D5 live crown. **≠ Foundry ALIVE invented.**
3. **RC-D6** remains non-`ALIVE` from ggen forever unless a BRCE+AS-01 DO receipt
   is supplied by the Plane-5 owner and cited here with command + exit.
4. **RC-D7** refuses silent closure: inventing R-0N prose is `REFUSED_UNGROUNDED`.
5. Any claim of crown Gate-3 `ALIVE` while any row above is not `ALIVE` (or an
   admitted typed refusal for that row) is **REFUSED**. Zero-HITL / Foundry /
   path crowns stay **PARTIAL** while Foundry `--live` is **BLOCKED_HITL**.

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
