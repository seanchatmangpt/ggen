# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

**Evidence refresh:** 2026-11-22T07:18:09Z (courts) / refreshed onto ecosystem tip
`566ee1b56322da6e8a6099ccc4d992cb6fec9454`
(`origin/cursor/ecosystem-spec-27-1-1-bc54`). Standings below are from live tree
inspection + court exits — not memory. Subject tip SHAs are `git log -1` over the
owning paths after fast-forward. Do not promote any row to `ALIVE` without a
recorded command exit 0 for that exact subject.

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
| Admit W3C `shui:` as public vocab (ggen fence) | **ALIVE** (ggen fence only) — `crates/ggen-graph/src/vocab/shui.rs` + `public_prefixes.rs`; court `shui_public_prefix_court` **3 passed; exit 0** (2026-11-22). Truex product admit is a separate row. | `16239f8232aa84055c2db0414810e637ff35d2dc` | `cargo test -p ggen-graph --test shui_public_prefix_court` | **ggen** (fence) |
| Truex runtime admits `shui:` shapes | **UNKNOWN** — sibling `seanchatmangpt/truex` not mounted in this worktree | n/a (sibling) | in truex: admit-court that loads a `shui:`-prefixed shape graph and exits 0; refuse still holds for private `truex:` | **truex** |
| Taffy AOT pre-flight (widths 320/375/393/600/840/1280 + WCAG 1.4.12 1.5×) | **PARTIAL_ALIVE** — `layout::taffy_aot` six-width seal present; court `taffy_aot_preflight_court` **6 passed; exit 0** (includes `six_width_seal_admits_valid_layout_under_1_5x` + width-local overflow at 320). Photolithography / zcode-cli consume still open → not crown `ALIVE`. Hop: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md). | `16239f8232aa84055c2db0414810e637ff35d2dc` | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | **ggen** |
| zcode-cli `ui-layout-court.ts` | **UNSUPPORTED** — `find` / ripgrep: **zero** `ui-layout-court.ts` (or non-doc hits) in this repo tree | n/a (sibling) | in zcode-cli: `pnpm test -- ui-layout-court` (or that repo’s exact court runner) against a ggen `AotSealReceipt` fixture | **zcode-cli** |
| `curve25519-dalek` v4 / cgap-aoq docs | **PARTIAL_ALIVE** (lock) / **UNKNOWN** (Gate-1 pin doc) — `Cargo.lock` resolves **4.1.3** and **5.0.0**; no dedicated pin/policy doc under `docs/rfc/v27.1.1/` | lock tip = branch `566ee1b56322da6e8a6099ccc4d992cb6fec9454` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` then author `docs/rfc/v27.1.1/` (or cgap-aoq) pin doc; refuse undocumented major | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | Affidavit pack **PARTIAL_ALIVE** (`packs/affidavit-pack/**` present); size court / artifact **UNKNOWN** — no `verify_witness.wasm`, no `&lt;262144` court in tree (`find` empty) | affidavit pack last touch `2364663b8960e8e0d837edc9e611b2083a749561` | build witness wasm then `wc -c verify_witness.wasm` / size court asserting `&lt; 262144` | external (witness/affidavit) with optional ggen pack emit |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **PARTIAL_ALIVE** — `layout::brand` + fixtures `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml`; court `brand_toml_rhythm_court` **4 passed; exit 0** (admits both OEM stubs; refuses touch &lt;44, gap &lt;8, off-grid). Full DTCG color/type/CASTLE inventories remain Gate 2+ → not crown `ALIVE`. **Commercial** priority is CindyCat-first ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)), not Fourth Watch cold cyber. | `16239f8232aa84055c2db0414810e637ff35d2dc` | `cargo test -p ggen-graph --test brand_toml_rhythm_court` | **ggen** (rhythm schema) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | n/a | Gate 2–3 — not a Gate-1 falsifier; commercial open is CindyCat patronage before Fourth Watch cold cyber ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | external |

### Command evidence log (this refresh)

| Subject | Command / binary | Result |
|---------|------------------|--------|
| `shui:` fence | `shui_public_prefix_court` (built test binary) | `3 passed; 0 failed` → exit **0** |
| `taffy_aot` | `taffy_aot_preflight_court` (six-width seal binary) | `6 passed; 0 failed` → exit **0** |
| `brand.toml` | `brand_toml_rhythm_court` | `4 passed; 0 failed` → exit **0** |
| `ui-layout-court.ts` | `find . -name 'ui-layout-court.ts'` | empty → **UNSUPPORTED** |
| `verify_witness.wasm` | `find . \( -name 'verify_witness*' -o -name '*witness*.wasm' \)` | empty → size court **UNKNOWN** |
| `curve25519-dalek` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` | versions **4.1.3** + **5.0.0** present; pin doc absent |

Replay via Cargo (preferred CI falsifier; same courts):

```text
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
```

---

## Recommended next single falsifier (Gate 1)

ggen fences already green under recorded exits:

- `shui:` public-prefix fence → **ALIVE** (fence scope)
- brand rhythm → **PARTIAL_ALIVE**
- Taffy AOT six-width seal → **PARTIAL_ALIVE**

Next **new** Gate-1 falsifier to author (pick one):

1. Wire zcode-cli `ui-layout-court.ts` (or igniter) to consume `AotSealReceipt` without re-implementing geometry law — photolithography remains non-`ALIVE` until that boundary executes. Hop ledger: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md).
2. Or advance sibling / docs rows still open: Truex `shui:` product admit; `curve25519-dalek` Gate-1 pin doc; `verify_witness` WASM size court (`wc -c` / `&lt;262144`).
