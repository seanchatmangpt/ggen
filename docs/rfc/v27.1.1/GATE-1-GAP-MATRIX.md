# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

**Evidence refresh:** 2026-11-22 court exit reconfirm on tip
`889075fe32e0ee18f4d9dc14859faf8353ade659`
(`origin/cursor/ecosystem-spec-27-1-1-bc54`, ≥ `a54b9803`). Standings below are
from recorded command/binary exits on that tip — not memory. Do not promote any
row to `ALIVE` without a recorded exit **0** for that exact subject tip.
Horizontal spine HITL ledger:
[`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md).

**Gate 2 commercial priority (not a Gate-1 falsifier):** CindyCat patronage **FIRST** —
LinkedIn direct outreach before content calendar; Fourth Watch as natural follow-on,
never cold cyber open. See [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md),
[`CINDYCAT-REVERSE-GLOO.md`](./CINDYCAT-REVERSE-GLOO.md),
[`LINKEDIN-DEAL-PLAYBOOK.md`](./LINKEDIN-DEAL-PLAYBOOK.md). Do not promote those docs
to deal `ALIVE`.

## Gate 0 — out of scope for ggen

Full standing matrix + stub citations: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| Requirement | Current evidence | Next exact verifier | Owner |
|-------------|------------------|---------------------|-------|
| GCP billing | none in ggen | external billing console / infra receipt | external (infra) |
| servengine M4 | none in ggen | servengine M4 court in that repo | external (servengine) |
| cos ledger fixes | none in ggen | cos ledger replay/verify | external (cos) |
| year-end SOW | none in ggen | commercial SOW register | external (ops) |

Do not promote any Gate 0 row to `ALIVE` from this repository.

---

## Gate 1 — in-repo / sibling (Dec 6–18)

| Requirement | Standing + live evidence | Tip SHA (subject) | Next exact verifier / command | Owner |
|-------------|--------------------------|-------------------|-------------------------------|-------|
| Admit W3C `shui:` as public vocab (ggen fence) | **ALIVE** — `shui_public_prefix_court` **3 passed; exit 0** on tip `889075fe3`. Truex product admit is a separate row. | tip `889075fe3` | `cargo test -p ggen-graph --test shui_public_prefix_court` | **ggen** (fence) |
| Truex runtime admits `shui:` shapes | **UNKNOWN** — sibling `seanchatmangpt/truex` not mounted in this worktree | n/a (sibling) | in truex: admit-court that loads a `shui:`-prefixed shape graph and exits 0; refuse still holds for private `truex:` | **truex** |
| Taffy AOT pre-flight (widths 320/375/414/768/1024/1440 + WCAG 1.4.12 1.5×) | **ALIVE** (six-width seal court) — `taffy_aot_preflight_court` **6 passed; exit 0** on tip `889075fe3`. Photolithography / zcode-cli consume still open as a separate hop → not claimed here. Hop: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md). | tip `889075fe3` | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | **ggen** |
| ggen `ui-layout-court` (AotSealReceipt digest consume) | **ALIVE** — `layout::ui_layout` + `tests/ui_layout_court.rs`; six-width @ **320/375/414/768/1024/1440** + WCAG 1.4.12 **1.5×**; fail-closed on missing/stale/mismatched digests. Sibling `ui-layout-court.ts` remains out-of-repo / **UNSUPPORTED**. (Owned by sibling agent — not re-implemented here.) | tip land `7c0ba2a90` | `cargo test -p ggen-graph --test ui_layout_court` | **ggen** (in-repo fence); **zcode-cli** (TS adapter, separate) |
| zcode POWL/OCEL bounded remediation (in-repo stub) | **PARTIAL_ALIVE** — standing + `remediation_bound` + `ggen-sjira` IO-free court on tip (integrate / [#862](https://github.com/seanchatmangpt/ggen/pull/862)). Live sibling Chicago court **UNKNOWN** / **UNSUPPORTED** (unmounted). Ledger: [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | tip `889075fe3` | In-repo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`. Live: `bun test test/powl-ocel-chicago.test.ts` in zcode-cli | **ggen** (contract); **zcode-cli** (execution) |
| `curve25519-dalek` v4 / cgap-aoq docs | **PARTIAL_ALIVE** (lock + pin doc) — `Cargo.lock` resolves **4.1.3** and **5.0.0**; policy recorded in [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md). Policy court that refuses undocumented major still absent → not crown `ALIVE`. | tip `889075fe3` (lock observation) | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` (expect 4.1.3 + 5.0.0); next ALIVE: Gate-1 policy court per pin standing §5 | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | **ALIVE** (offline size court) — `verify_witness_size_court` **6 passed; exit 0** on tip `889075fe3` (placeholder `&lt; 262144`); production runtime out-of-repo ([`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)) remains external / typed `UNSUPPORTED` — not production-artifact crown | tip `889075fe3` | `cargo test -p ggen-graph --test verify_witness_size_court` | **ggen** (size court) + external (witness/affidavit production build) |
| R-01…R-07 resolution contract | **ALIVE** (contract court) — `r01_r07_resolution_court` **7 passed; exit 0** on tip `889075fe3`; risk *bodies* remain **BLOCKED** — [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | tip `889075fe3` | `cargo test -p ggen-graph --test r01_r07_resolution_court` | **ggen** (contract); risk-register owner (bodies) |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **ALIVE** (rhythm court) — `brand_toml_rhythm_court` **4 passed; exit 0** on tip `889075fe3`. Full DTCG color/type/CASTLE inventories remain Gate 2+ → not claimed here. **Commercial** priority is CindyCat-first ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)), not Fourth Watch cold cyber. | tip `889075fe3` | `cargo test -p ggen-graph --test brand_toml_rhythm_court` | **ggen** (rhythm schema) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | n/a | Gate 2–3 — not a Gate-1 falsifier; commercial open is CindyCat patronage before Fourth Watch cold cyber ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | external |
| BRCE + WebAuthn AS-01 (`:prime256v1`) ungated-DO fence | **ALIVE** (seat fence court) — `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` **6 passed; exit 0** on tip `889075fe3`; crypto verify **UNSUPPORTED**; live BRCE **BLOCKED**; standing [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | tip `889075fe3` | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | **ggen** (fence); BRCE/Plane-5 own verify+actuate |
| ggen_igniter clean-room | **BLOCKED** (sibling unmounted) — in-repo contract **PARTIAL_ALIVE** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | tip `889075fe3` | Sibling: `git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | **ggen_igniter** (crown); **ggen** (contract) |

### Command evidence log (this refresh)

Tip subject: `889075fe32e0ee18f4d9dc14859faf8353ade659` (≥ `a54b9803`).
`CARGO_BUILD_JOBS=1`. Full `cargo test -p ggen-graph --test …` rebuilds were repeatedly
SIGKILL/SIGTERM under shared-target OOM contention; exits below are from the named
court harness on that tip (warm court binary, tip-synced court crate, or exact
`cargo test -p ggen-abb-sbb`).

| Subject | Command / binary | Result |
|---------|------------------|--------|
| `shui:` fence | `shui_public_prefix_court` warm court binary | `3 passed; 0 failed` → exit **0** |
| `taffy_aot` | `taffy_aot_preflight_court` warm court binary | `6 passed; 0 failed` → exit **0** |
| `brand.toml` | `brand_toml_rhythm_court` warm court binary | `4 passed; 0 failed` → exit **0** |
| `verify_witness` size court | tip-synced `verify_witness_size_court` harness | `6 passed; 0 failed` → exit **0** |
| R-01…R-07 contract | tip-synced `r01_r07_resolution_court` harness | `7 passed; 0 failed` → exit **0** |
| BRCE AS-01 seat | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | `6 passed; 0 failed` → exit **0** |
| zcode POWL stub | `zcode_powl_ocel_court` | not in this reconfirm set; live CLI **UNSUPPORTED** |
| `ui-layout-court` | sibling-owned; not re-run here | **ALIVE** on land `7c0ba2a90` (prior); do not regress |
| `ui-layout-court.ts` (sibling) | `find . -name 'ui-layout-court.ts'` | empty in ggen → sibling adapter **UNSUPPORTED** here |
| `verify_witness.wasm` (production) | `find . \( -name 'verify_witness*' -o -name '*witness*.wasm' \)` | placeholder only → production **UNSUPPORTED** |
| `curve25519-dalek` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` + pin standing | versions **4.1.3** + **5.0.0** present; pin doc → **PARTIAL_ALIVE**; policy court still open |
| ggen_igniter checkout | local mount probe | **BLOCKED** (unmounted) |

Replay via Cargo (preferred CI falsifier; same courts):

```text
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test ui_layout_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
cargo test -p ggen-graph --test brand_fluid_clamp_court
cargo test -p ggen-graph --test verify_witness_size_court
cargo test -p ggen-graph --test r01_r07_resolution_court
cargo test -p ggen-abb-sbb --test brce_as01_seat_court
cargo test -p ggen-sjira --test zcode_powl_ocel_court
```

---

## Recommended next single falsifier (Gate 1)

ggen fence courts **exit 0** on tip `889075fe3`:

- `shui:` public-prefix fence → **ALIVE**
- brand rhythm → **ALIVE** (DTCG inventory still Gate 2+)
- Taffy AOT six-width seal → **ALIVE** (photolithography consume still open)
- `verify_witness` size court → **ALIVE** (production WASM still out-of-repo)
- BRCE AS-01 seat → **ALIVE** (crypto / live BRCE still open)
- R-01…R-07 contract → **ALIVE** (bodies **BLOCKED**)
- zcode POWL in-repo stub → **PARTIAL_ALIVE** (live CLI open; not in this exit set)

ggen in-repo `ui-layout-court` digest consume → **ALIVE** on land `7c0ba2a90` (sibling-owned; not re-implemented here).

Next **new** Gate-1 falsifier to author (pick one):

1. Mount sibling zcode-cli `ui-layout-court.ts` against a pinned ggen `AotSealReceipt` fixture (out-of-repo hop; in-repo fence already ALIVE). Hop ledger: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md).
2. Or advance sibling / docs rows still open: Truex `shui:` product admit; `curve25519-dalek` Gate-1 **policy court** (pin doc now PRESENT — [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md)); production `verify_witness.wasm` build on the affidavit plane; igniter clean-room probe.
