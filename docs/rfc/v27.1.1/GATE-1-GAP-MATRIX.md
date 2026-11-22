# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

**Evidence refresh:** 2026-11-22 (courts @ earlier tip) / consistency refresh onto
ecosystem tip `57626beaa2aa3dc89fb2f7b58b33dffdb11830d0`
(`origin/cursor/ecosystem-spec-27-1-1-bc54`). Post-`566ee1b56` lands on tip:
BRCE AS-01 [#861], verify_witness / shui Truex fence / R-01…R-07 courts, zcode
POWL/OCEL stub — treat newly merged courts as **PARTIAL** until exit reconfirm
on this tip. Standings below are from live tree inspection + prior court exits —
not memory. Do not promote any row to `ALIVE` without a recorded command exit 0
for that exact subject tip. Horizontal spine HITL ledger:
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
| Admit W3C `shui:` as public vocab (ggen fence) | **PARTIAL_ALIVE** (merged on tip; prior tip recorded `shui_public_prefix_court` **3 passed; exit 0** — reconfirm on `57626beaa`). Truex product admit is a separate row. | tip `57626beaa` (subject earlier `16239f823`) | `cargo test -p ggen-graph --test shui_public_prefix_court` | **ggen** (fence) |
| Truex runtime admits `shui:` shapes | **UNKNOWN** — sibling `seanchatmangpt/truex` not mounted in this worktree | n/a (sibling) | in truex: admit-court that loads a `shui:`-prefixed shape graph and exits 0; refuse still holds for private `truex:` | **truex** |
| Taffy AOT pre-flight (widths 320/375/393/600/840/1280 + WCAG 1.4.12 1.5×) | **PARTIAL_ALIVE** — `layout::taffy_aot` six-width seal + `LayoutRefusal` parity present; prior tip court **6 passed; exit 0** (reconfirm on tip). Photolithography / zcode-cli consume still open → not crown `ALIVE`. Hop: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md). | tip `57626beaa` | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | **ggen** |
| ggen `ui-layout-court` (AotSealReceipt digest consume) | **ALIVE** — `layout::ui_layout` + `tests/ui_layout_court.rs` on tip `eaf0dad40`; consume six-width @ WCAG 1.4.12 1.5× digests; fail-closed on missing/stale/mismatched; **8 passed; exit 0** (Cloud VM4 `bc-88761271` transcript after FF onto tip). Court land `479fc83a9` introduced the fence (ancestry). Sibling `ui-layout-court.ts` remains out-of-repo / not claimed here. | tip `eaf0dad40` (land `479fc83a9`) | `cargo test -p ggen-graph --test ui_layout_court` | **ggen** (in-repo fence); **zcode-cli** (TS adapter, separate) |
| zcode POWL/OCEL bounded remediation (in-repo stub) | **PARTIAL_ALIVE** — standing + `remediation_bound` + `ggen-sjira` IO-free court on tip (integrate / [#862](https://github.com/seanchatmangpt/ggen/pull/862)). Live sibling Chicago court **UNKNOWN** / **UNSUPPORTED** (unmounted). Ledger: [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | tip `57626beaa` | In-repo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`. Live: `bun test test/powl-ocel-chicago.test.ts` in zcode-cli | **ggen** (contract); **zcode-cli** (execution) |
| `curve25519-dalek` v4 / cgap-aoq docs | **PARTIAL_ALIVE** (lock + pin doc) — `Cargo.lock` resolves **4.1.3** and **5.0.0**; policy recorded in [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md). Policy court that refuses undocumented major still absent → not crown `ALIVE`. | tip `9f6fecf83` (lock observation) | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` (expect 4.1.3 + 5.0.0); next ALIVE: Gate-1 policy court per pin standing §5 | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | **ALIVE** (offline size court) — `ggen_graph::witness` API + court `verify_witness_size_court` **6 passed; exit 0** (2026-11-22) on tip `45974aaf6` (placeholder `&lt; 262144`); production runtime out-of-repo ([`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)) remains external / typed `UNSUPPORTED` — not production-artifact crown | tip `45974aaf6` (subject `180c9e4a8`) | `cargo test -p ggen-graph --test verify_witness_size_court` | **ggen** (size court) + external (witness/affidavit production build) |
| R-01…R-07 resolution contract | **PARTIAL_ALIVE** (merged contract court; risk bodies **BLOCKED**) — [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | tip `57626beaa` (`acc05a4f0`) | `cargo test -p ggen-graph --test r01_r07_resolution_court` | **ggen** (contract); risk-register owner (bodies) |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **PARTIAL_ALIVE** — `layout::brand` + fluid `slope_cqi` + fixtures; prior tip court **4 passed; exit 0** (reconfirm on tip). Full DTCG color/type/CASTLE inventories remain Gate 2+ → not crown `ALIVE`. **Commercial** priority is CindyCat-first ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)), not Fourth Watch cold cyber. | tip `57626beaa` | `cargo test -p ggen-graph --test brand_toml_rhythm_court` | **ggen** (rhythm schema) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | n/a | Gate 2–3 — not a Gate-1 falsifier; commercial open is CindyCat patronage before Fourth Watch cold cyber ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | external |
| BRCE + WebAuthn AS-01 (`:prime256v1`) ungated-DO fence | **PARTIAL_ALIVE** — [#861](https://github.com/seanchatmangpt/ggen/pull/861) on tip; doctrine + `ggen-abb-sbb::as01_seat` presence/shape refusal (`REFUSED_NO_AS01_SEAT`); crypto verify **UNSUPPORTED**; live BRCE **BLOCKED**; standing [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | tip `57626beaa` (`822b1fe13`) | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | **ggen** (fence); BRCE/Plane-5 own verify+actuate |
| ggen_igniter clean-room | **BLOCKED** (sibling unmounted) — in-repo contract **PARTIAL_ALIVE** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | tip `57626beaa` | Sibling: `git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | **ggen_igniter** (crown); **ggen** (contract) |

### Command evidence log (this refresh)

| Subject | Command / binary | Result |
|---------|------------------|--------|
| `shui:` fence | `shui_public_prefix_court` (prior tip binary) | prior: `3 passed; 0 failed` → exit **0** — **reconfirm on `57626beaa`** |
| `taffy_aot` | `taffy_aot_preflight_court` (six-width seal binary) | prior: `6 passed; 0 failed` → exit **0** — reconfirm |
| `brand.toml` | `brand_toml_rhythm_court` | prior: `4 passed; 0 failed` → exit **0** — reconfirm |
| `verify_witness` size court | `verify_witness_size_court` | `6 passed; 0 failed` → exit **0** on tip `45974aaf6` |
| R-01…R-07 contract | `r01_r07_resolution_court` | merged on tip — replay exit reconfirm |
| BRCE AS-01 seat | `brce_as01_seat_court` | merged via [#861](https://github.com/seanchatmangpt/ggen/pull/861) — replay |
| zcode POWL stub | `zcode_powl_ocel_court` | merged on tip — replay; live CLI **UNSUPPORTED** |
| `ui-layout-court` | `cargo test -p ggen-graph --test ui_layout_court` | **ALIVE** — **8 passed; exit 0** on tip `eaf0dad40` (land `479fc83a9`; Cloud VM4 `bc-88761271`) |
| `ui-layout-court.ts` (sibling) | `find . -name 'ui-layout-court.ts'` | empty in ggen → sibling adapter **UNSUPPORTED** here |
| `verify_witness.wasm` (production) | `find . \( -name 'verify_witness*' -o -name '*witness*.wasm' \)` | placeholder only → production **UNSUPPORTED** |
| `curve25519-dalek` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` + pin standing | versions **4.1.3** + **5.0.0** present; pin doc [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) → **PARTIAL_ALIVE**; policy court still open |
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

ggen fences **merged** on tip (PARTIAL until tip exit reconfirm):

- `shui:` public-prefix fence → **PARTIAL_ALIVE** (prior ALIVE exit on earlier tip; reconfirm)
- brand rhythm → **PARTIAL_ALIVE**
- Taffy AOT six-width seal → **PARTIAL_ALIVE**
- `verify_witness` size court → **ALIVE** (offline size court; exit 0 on tip `45974aaf6`; production WASM still out-of-repo)
- BRCE AS-01 seat → **PARTIAL_ALIVE** (crypto / live BRCE still open)
- zcode POWL in-repo stub → **PARTIAL_ALIVE** (live CLI open)
- R-01…R-07 contract → **PARTIAL_ALIVE** (bodies **BLOCKED**)

ggen in-repo `ui-layout-court` digest consume → **ALIVE** on tip `eaf0dad40` (**8 passed; exit 0**; land `479fc83a9`; Cloud VM4 `bc-88761271`; `cargo test -p ggen-graph --test ui_layout_court`).

Next **new** Gate-1 falsifier to author (pick one):

1. Mount sibling zcode-cli `ui-layout-court.ts` against a pinned ggen `AotSealReceipt` fixture (out-of-repo hop; in-repo fence already ALIVE). Hop ledger: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md).
2. Or advance sibling / docs rows still open: Truex `shui:` product admit; `curve25519-dalek` Gate-1 **policy court** (pin doc now PRESENT — [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md)); production `verify_witness.wasm` build on the affidavit plane; igniter clean-room probe.
