# CROWN-PROGRESS-RECEIPT — ECOSYSTEM-SPEC-27.1.1

| Field | Value |
|-------|-------|
| Document ID | `CROWN-PROGRESS-RECEIPT-27.1.1` |
| Status | **PROGRESS RECEIPT** (honest; **not** goal-complete; **not** crown `ALIVE`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1f10949a78523348951f55583d94bdac7bf63c66` (tip-wins absorb zcode POWL refusal reconfirm **ALIVE** `cursor/v2711-zcode-refusal-reconfirm-bc54` @ `8bd3fe407` / `bc-934bba30` + OOR retip14 `cursor/v2711-oor-retip14-bc54` @ `94e5318c6` onto tip `1f10949a7` which already carries stall pin `43c941f88`; ⊇ HITL `--live` / CASTLE / VW / OOR retip13 / BRCE-V8 / Smalt) |
| Tip subject | tip-wins absorb zcode refusal reconfirm **ALIVE** (sjira **6**/bound **4** EXIT **0** @ tip `df9597c18` / zcode-cli `f32df08`) superseding interim stall **STALLED** ledger; **refusal ALIVE ≠ `--live` ALIVE** — foundry `--live` remains **BLOCKED_HITL** / **foundry_exit=1**; OOR retip14 cos **503** / healthz **200** / revops **NXDOMAIN** held; path crown **PARTIAL**; CASTLE / Smalt / affidavit / BRCE **V8** / seat+R01 / ui_layout honesty held |
| Ledger pins cited | Gate-1 pins; Truex **ALIVE** @ `68d2d233`; `shui:` **ALIVE (lineage tip-cargo)** @ `008a81d21`; CCS #3 **ALIVE** @ `861a6e73a`; CCS #2 `write_apply` **ALIVE** **7+1+1** @ `cde02d1f2`; `verify_witness` **ALIVE** @ `c91b22561`; `lifecycle.rs` **ALIVE**; `ui_layout` **ALIVE (exact tip-cargo)** @ `a508e6bae` (lineage `59c090984`); brand rhythm/completeness **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged); **FM-PACK-001** **ALIVE (lineage tip-cargo)** absorbed; `r01_r07` contract **ALIVE** (exact tip-cargo **7** @ `e7569893a`); BRCE AS-01 seat fence **ALIVE** (exact tip-cargo **6** @ `e7569893a`); **OOR:** cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL** / BRCE **V8** **BLOCKED** / R-01…R-07 bodies **BLOCKED** |
| Cargo this receipt | **no** this retip (docs-only residual honesty). Prior exact-tip `ui_layout_court` EXIT:0 @ `a508e6bae` retained; BRCE+R01 @ `e7569893a` retained. Production theme **UNSUPPORTED**; **V8**/bodies blockers held (no false `ALIVE`) |
| Goal standing | **INCOMPLETE** — do **not** claim ECOSYSTEM-SPEC-27.1.1 crown complete |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`. Honesty retip also uses
`COMPILING` for tip-present courts without recorded tip EXIT **0**, and `STALLED`
for scheduled tip-cargo reconfirms whose agents have not pushed evidence.

**Authority:** §13 crown bindings in
[`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md); detail from
[`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md),
[`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md),
[`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md),
[`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md),
[`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md),
[`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md),
[`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md),
[`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md).
Dedicated ledgers win over stale §13 prose where they disagree (notably Smalt
FinOps: FOUNDRY/OOR **ALIVE** @ `91997da76f0f` vs older §13.3 FinOps
**UNSUPPORTED** wording).

Compact return line:

```text
tip=1f10949a78523348951f55583d94bdac7bf63c66
subject=1f10949a7+zcode-refusal-reconfirm-ALIVE+OOR-retip14+stall-superseded+HITL-LIVE+CASTLE/VW
side=cursor/v2711-zcode-refusal-reconfirm-bc54@8bd3fe407+cursor/v2711-oor-retip14-bc54@94e5318c6
cargo_tip_ui_layout=a508e6bae9e61d7894ad98df8c507fdb1fb28eb2
oor_retip=retip14@94e5318c6 (cos 503 / healthz 200 / revops NXDOMAIN held)
clause1=PARTIAL (igniter clean-room ALIVE reconfirm EXIT:0 @bc-fd60df35/d0fec2f2f; prior f1492ed55/bc-53b0e099; hop1 Truex ALIVE @68d2d233 + shui lineage ALIVE @008a81d21; path crown still non-ALIVE; foundry --live BLOCKED_HITL foundry_exit=1 exact next foundry_exit=0; zcode refuse-path reconfirm ALIVE sjira6/bound4 EXIT0 @bc-934bba30/8bd3fe407 tip df9597c18 — refusal ALIVE≠--live ALIVE; stall 43c941f88 superseded)
clause2=PARTIAL_ALIVE (CCS#3 construct ALIVE @861a6e73a; CCS#2 write_apply tip-cargo ALIVE 7+1+1 @cde02d1f2; lifecycle ALIVE @513704d04/acd9705c1; BRCE seat fence ALIVE @e7569893a; r01_contract ALIVE @e7569893a; V8 broker BLOCKED owner=BRCE HITL; R01-R07 bodies BLOCKED owner=risk-register HITL G3-5; V9 BLOCKED)
clause3=PARTIAL+BLOCKED (cos/readyz 503 DB_UNAVAILABLE; healthz 200 PARTIAL; revops NXDOMAIN UNKNOWN — retip14 held; Smalt residual mix ALIVE @91997da76f0f)
clause4=PARTIAL
gate1=PARTIAL (Truex ALIVE @68d2d233; shui ALIVE lineage @008a81d21; ui_layout ALIVE exact tip-cargo @a508e6bae/bc-70188ac3; FM-PACK-001 ALIVE lineage absorbed; taffy/fluid/hotswap ALIVE @cde02d1f2/19c08a3b0; construct+lifecycle ALIVE; write_apply ALIVE @cde02d1f2; verify_witness size ALIVE 8 @b2f6e12d2; env_wire PARTIAL; affidavit pin ALIVE @563eb322a; brand_toml exact tip-cargo ALIVE @77eb46fd0 unchanged; r01_r07 ALIVE exact tip @e7569893a; BRCE seat ALIVE exact tip @e7569893a; brand production UNSUPPORTED; verify_witness production runtime UNSUPPORTED; BRCE V8 BLOCKED; R01-R07 bodies BLOCKED; zcode refusal reconfirm ALIVE)
blockers=foundry--live; cos/readyz-503; WebAuthn/BRCE-V8/V9; revops-NXDOMAIN; R01-R07-bodies; verify_witness-prod-runtime; production-theme
```

---

## Clause 1 — Photolithography path (§13.1)

| Claim | Standing |
|-------|----------|
| **Path crown** | **PARTIAL** / **non-ALIVE** |
| Hop 1 perimeter (`shui:` fence + Truex admit) | Fence **ALIVE (lineage tip-cargo)** — **4 passed; exit 0** @ `008a81d21` (sources ≡ tip `c5101d597`; **no tip EXIT:0**); Truex admit-court **ALIVE** @ `truex@68d2d233` (merged `b4672131`; not re-run) |
| Hop 2 `.ttl`/`.toml` rhythm + dual-schema | brand rhythm/completeness **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged); full DTCG/CASTLE inventory **UNSUPPORTED** |
| Hop 3 Taffy AOT six-width seal | In-repo fence tip-cargo **ALIVE** (**7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2`); consumer bind open → path **PARTIAL_ALIVE** |
| Hop 3b ggen `ui_layout` digest consume | **ALIVE (exact tip-cargo)** **11 passed; exit 0** @ cargo tip `a508e6bae` (`bc-70188ac3`; `/tmp/ggen-uilayout-exact-tip`; sources ≡ live tip); lineage `59c090984` / `ca52e9c16` / `eaf0dad40`; tip source **11** |
| Hop 4 ggen_igniter BrandTheme manufacture | **ALIVE** (`diff_exit=0` dual-schema tip brand vs igniter fixture); clean-room crown **ALIVE** reconfirm EXIT:**0** @ tip `d0fec2f2f` (`bc-fd60df35` / `/tmp/cursor/igniter-cleanroom-reconfirm.md`; prior `f1492ed55` / `bc-53b0e099`); NIF **ALIVE**; Jobs/C4 admitted `--check` **ALIVE** (exit **0**; bare `--check` **REFUSED_INVOCATION** antifalsifier); path crown **PARTIAL** (unchanged) |
| Hop 5 zcode POWL/OCEL / foundry | **PARTIAL_ALIVE** — in-repo bounded-refusal tip-cargo **ALIVE** (sjira **6**/bound **4** @ `cd7c2433b`; reconfirm **exit 0** @ tip `df9597c18` / `cursor/v2711-zcode-refusal-reconfirm-bc54` @ `8bd3fe407` / `bc-934bba30`; stall pin `43c941f88` **superseded**); adapter exit **0**; Linux tip-matched runtime **PRESENT**; `--dry-run` exit **2** / **PARTIAL**; `--live` **BLOCKED_HITL** / **foundry_exit=1** / **REFUSED** (`Model access is not configured` @ zcode `f32df08`) — **refusal ALIVE ≠ `--live` ALIVE** |

**Exact next verifiers**

1. provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0** ([`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); keys [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md)).
2. `ui_layout` exact tip-cargo **ALIVE** @ `a508e6bae` (`bc-70188ac3`) — sealed; brand_toml exact tip-cargo already **ALIVE** @ `77eb46fd0`; BRCE seat + R-01 already **ALIVE** @ `e7569893a`.
3. Jobs/C4 residual **closed** — replay admitted `--pack-dir sample-pack … --check` → **0** ([`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)).

Antifalsifier: adapter exit 0 ≠ path crown `ALIVE`; runtime materialization alone ≠ hop-5 `ALIVE`; bare post-clone `git diff` ≠ hop-4 `ALIVE`; NIF `.so` / bare exit-2 ≠ Jobs/C4 `ALIVE`.

---

## Clause 2 — CCS / BRCE + WebAuthn AS-01 (§13.2)

Detail binder: [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) (tip-wins absorb `cursor/v2711-brce-v8-bodies-bc54` @ `e54d58bac` onto crown `fe72be0fb` ⊇ VW/Smalt/OOR/CASTLE; write_apply @ `cde02d1f2`; seat+R01 @ `e7569893a`; prior absorb `cbeb11d44` / evidence `861a6e73a`).

| Claim | Standing |
|-------|----------|
| **CCS / BRCE slice crown** | **PARTIAL_ALIVE** |
| CCS #1 AS-01 CONSTRUCT→DO | **PARTIAL_ALIVE** — V1 seat exact tip-cargo **ALIVE** (**6** @ `e7569893a`) + V7 ES256 fixture land (**4** @ `fb6e7fc6e`); live WebAuthn **UNSUPPORTED**; broker **V8** **BLOCKED**; Plane-5 DO **BLOCKED** |
| CCS #2 MCP `ggen_write_apply` + CP17 | **ALIVE** — tip-cargo **7+1+1** exit **0** on tip `cde02d1f2` (evidence `42c5b9ba4`; [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md)) |
| CCS #3 `construct_hash_gate` | **ALIVE** tip-cargo — **4 passed; exit 0** @ `861a6e73a` (`cursor/v2711-ccs-brce-tipcargo-bc54` / absorb `cbeb11d44`) |
| `lifecycle.rs` (A2A CP17 path) | **ALIVE** — **PRESENT** on tip (`513704d04`); tip-cargo **6 passed; exit 0** @ `acd9705c1` ([`LIFECYCLE-TIPWINS-STANDING.md`](./LIFECYCLE-TIPWINS-STANDING.md)); construct **ALIVE** retained; BRCE **V8** **BLOCKED** |
| CCS #5 SovereignLease Ed25519 | Fixture **ALIVE**; **UNSUPPORTED** as AS-01/`prime256v1` |
| CCS #8 Plane-5 front doors | **BLOCKED** / **UNKNOWN** (revops DNS; cos `/readyz` **503**) |
| R-01…R-07 contract / bodies | Contract **ALIVE** (**7** @ `e7569893a`); bodies **BLOCKED** (OOR **G3-5**) |

**Exact next verifiers**

1. **V8** broker (**BLOCKED**; owner **BRCE**; Founder HITL mount + live WebAuthn): in mounted BRCE broker checkout — intent without AS-01 → `REFUSED_*`; with verified seat → execution receipt + replay digest → exit **0**. In-repo `brce_as01_seat_court` / V7 do **not** close. Then **V9** / OOR **G2-5**. **No** V8 `ALIVE` this retip.
2. **R-01…R-07 bodies** (**BLOCKED**; owner risk-register; Founder HITL; OOR **G3-5**): admit sovereign risk-register bodies (no invention) → per-ID typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract fence `r01_r07_resolution_court` does **not** close bodies.
3. Clause-2 tip-cargo **done** retained: CCS #3 construct_hash @ `861a6e73a`; seat **6** + R01 contract **7** @ `e7569893a`; CCS #2 write_apply **7+1+1** @ `cde02d1f2`; lifecycle **6/6** @ `acd9705c1` (binder [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md)).

Fence / fixture / contract exit 0 ≠ Plane-5 / CONSTRUCT→DO / body-closure `ALIVE`. Seat + R01-*contract* **ALIVE** ≠ BRCE **V8** `ALIVE`.

---

## Clause 3 — Ops surfaces (Gate 0 + Smalt + Plane 5 / OOR) (§13.3)

| Surface | Standing | Exact next verifier |
|---------|----------|---------------------|
| Gate 0 GCP billing / cos ledger / servengine M4 / SOW | **BLOCKED** / **UNKNOWN** (OUT OF SCOPE for ggen `ALIVE`) | OOR **G0-1…G0-4**; cos: `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** |
| Smalt 4-cloud + FinOps (spine) | **ALIVE** (sibling) — doctor/aws/gcp/azure/twilio **0,0,0,0,0** @ `875efbb87`; FinOps **ALIVE** @ `91997da76f0f` (**19 passed**) — ≠ Foundry / zero-HITL crown | Replay FinOps + §4.1 4-cloud; residual GA economic face / b4b (SMALT crown still treats full RAID residual as open → keep crown **PARTIAL** for non-spine) |
| `revops.fnox.io` | **UNKNOWN** / **BLOCKED** (DNS fail / NXDOMAIN) | `getent hosts revops.fnox.io`; `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** |
| cos `/readyz` | **BLOCKED** (**503** `DB_UNAVAILABLE`) | same curl → **200** |
| OOR aggregate | Tracks Gate 0/2/3; Zero-HITL still **PARTIAL** | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) BLOCKED/PENDING table |

**Clause-3 aggregate:** **PARTIAL** (Smalt spine green) **+ BLOCKED** (Gate 0 / revops / cos / Plane-5 DO). Not ops crown `ALIVE`.

---

## Clause 4 — Foundry finish / zero-HITL spine (§13.4)

| Claim | Standing |
|-------|----------|
| **Foundry / zero-HITL crown** | **PARTIAL** — incomplete; clause-4 GA falsifiers would still fire today |
| ggen + Taffy | tip-cargo **ALIVE** (`taffy_aot` **7** @ `19c08a3b0`; `ggen-layout-court` **PRESENT**) |
| zcode-cli | **PARTIAL** (adapter **0**; foundry `--live` **BLOCKED**/**REFUSED**) |
| CASTLE | Sibling courts **ALIVE** @ `2774039ea` — ≠ Foundry crown |
| Smalt | Spine **ALIVE** @ `91997da76f0f` / `875efbb87` — ≠ Foundry crown |
| revops / cos | **BLOCKED** |

Founder HITL still required for: **cos DB**, **WebAuthn live (V8/V9)**, **remaining fence tip-cargo**, **foundry `--live`**.

**Exact next verifiers (Foundry §1a)**

1. `bun scripts/powl-foundry-loop.ts --live` → **0** (provider `/login` or `~/.zcode/v2/provider_config.json`; ui_layout already **ALIVE**).
2. `ui_layout` exact tip-cargo **ALIVE** @ `a508e6bae` (`bc-70188ac3`) — sealed ([`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)); brand_toml exact + BRCE seat + R-01 already **ALIVE**.
3. `curl … https://cos.fnox.io/readyz` → **200**.
4. BRCE **V8** then **V9**.

Do **not** invent zero daily founder HITL `ALIVE` from Gate-1 fence green, CASTLE sibling courts, or Smalt column exits alone.

---

## Gate 1 — in-repo / sibling falsifiers ([`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md))

| Requirement | Standing | Exact next verifier |
|-------------|----------|---------------------|
| `shui:` public-prefix fence | **ALIVE (lineage tip-cargo)** (**4 passed; exit 0** @ crown `008a81d21`; sources ≡ tip `c5101d597`; **no tip EXIT:0**) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked` |
| Truex admits `shui:` | **ALIVE** @ `68d2d233` (merged `b4672131`; not re-run) | `pnpm exec vitest run test/admit/admit-court.test.ts` → **0** |
| Taffy AOT pre-flight | **ALIVE** tip-cargo (**7 passed; exit 0** @ crown `19c08a3b0`) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` |
| ggen `ui_layout_court` | **ALIVE (exact tip-cargo)** (**11 passed; exit 0** @ `a508e6bae`; `bc-70188ac3`; sources ≡ live tip) | Replay: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip --config build.jobs=1 -- --test-threads=1` |
| FM-PACK-001 `packs/gh-terraform-pack` | **ALIVE** (lineage tip-cargo absorbed @ crown `e73bf686c`; `composed_packs_e2e` **2 passed; exit 0** @ evidence `19c08a3b0` / court tip `a508e6bae`; sources ≡ absorb tip) | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-engine --test composed_packs_e2e --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 -- --nocapture` → **0**; ledger [`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md) |
| zcode POWL in-repo stub | **ALIVE** (bounded refusal tip-cargo **6+4**; exit 0 @ `cd7c2433b`; reconfirm **exit 0** @ tip `df9597c18` / `8bd3fe407` / `bc-934bba30`; stall `43c941f88` superseded) | Replay: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`; `cargo test -p ggen-graph remediation_bound --lib`; live next: G2-9 foundry `--live` (**BLOCKED_HITL**); ledger [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) |
| `curve25519-dalek` pin | **PARTIAL_ALIVE** (lock+doc; policy court open) | pin standing §5 policy court |
| `verify_witness` size | **ALIVE** (tip-wins **8** @ `b2f6e12d2` → crown `f65434315`; prior **6** @ `f5f44571f`); env wire **PARTIAL** (`AFFIDAVIT_…_BUDGET_BYTES=256_000`); affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`); production runtime **UNSUPPORTED** | size court; affidavit: [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md); prod: [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| R-01…R-07 contract | **ALIVE** (exact tip-cargo **7** @ `e7569893a`); bodies **BLOCKED** | Replay: `cargo test -p ggen-graph --test r01_r07_resolution_court` → EXIT **0** |
| Brand rhythm / hot-swap | fluid+hot-swap tip-cargo **ALIVE** (**6+6** @ `cde02d1f2`/`19c08a3b0`); rhythm+completeness **ALIVE (exact tip-cargo)** (**5+12** @ `77eb46fd0`, unchanged); production **UNSUPPORTED** (`REFUSED_PRODUCTION`) | Replay brand exact tip; [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) / [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) |
| BRCE AS-01 seat fence | **ALIVE** (exact tip-cargo **6** @ `e7569893a`); **V8** **BLOCKED** | Replay: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`; next **V8** |
| ggen_igniter manufacture | **ALIVE** (BrandTheme compare); clean-room crown **ALIVE** reconfirm (`bc-fd60df35` @ `d0fec2f2f`; prior `bc-53b0e099`); NIF **ALIVE**; Jobs/C4 admitted `--check` **ALIVE**; path **PARTIAL** | clean-room replay `git diff --exit-code` → **0**; manufacture replay → **0**; Jobs/C4 replay pack/template `--check` → **0** |
| Gate-0 cos | **BLOCKED** (external) | cos `/readyz` → **200** |

**Gate-1 aggregate:** **PARTIAL** — `ui_layout` **ALIVE (exact tip-cargo)** @ `a508e6bae` (`bc-70188ac3`); **FM-PACK-001** **ALIVE (lineage tip-cargo)** absorbed @ `e73bf686c`; Taffy + brand fluid/hotswap tip-cargo **ALIVE** @ `cde02d1f2`/`19c08a3b0`; `write_apply` **ALIVE** @ `cde02d1f2`; construct+lifecycle **ALIVE**; brand rhythm/completeness **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged); `r01_r07` contract **ALIVE** (exact tip-cargo **7** @ `e7569893a`); BRCE AS-01 seat fence **ALIVE** (exact tip-cargo **6** @ `e7569893a`); production theme **UNSUPPORTED**; BRCE **V8** **BLOCKED**. Zero-HITL fab / production WASM / production brand theme / live BRCE **not** `ALIVE`.

Recommended next Gate-1 falsifier: hop-5 foundry `--live` → **0** (provider keys); `ui_layout` exact tip-cargo sealed.

---

## Blockers (active)

| # | Blocker | Standing | Exact next |
|---|---------|----------|------------|
| 1 | foundry `--live` / ZCode runtime | **BLOCKED_HITL**/**REFUSED** | Founder HITL → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** ([`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md)) — **not** `ALIVE` until exit **0** |
| 2 | remaining Gate-1 fence tip-cargo (`ui_layout`) | **ALIVE (exact tip-cargo)** @ `a508e6bae`/`bc-70188ac3` (brand_toml exact @ `77eb46fd0`; BRCE seat + R01 exact @ `e7569893a`; taffy+fluid+hotswap+shui **ALIVE**) | sealed — [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md) |
| 3 | cos `/readyz` DB | **BLOCKED** (**503**) | curl → HTTP **200** |
| 4 | Live WebAuthn / BRCE broker / Plane-5 DO | **UNSUPPORTED** / **BLOCKED** | **V8** (owner **BRCE**; Founder HITL mount+WebAuthn): mounted broker court without AS-01 → `REFUSED_*`; with seat → receipt+replay → **0**; then **V9** / OOR **G2-3…G2-5** ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5) |
| 5 | revops.fnox.io DNS | **UNKNOWN**/**BLOCKED** (**NXDOMAIN**) | `getent hosts` ∧ `/readyz` **200** |
| 6 | `mix ggen_igniter.sync` NIF / Jobs/C4 | **ALIVE** (admitted `--check` exit **0**; bare `--check` **REFUSED_INVOCATION** antifalsifier — [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)) | Replay pack/template `--check` → **0** (residual closed) |
| 7 | R-01…R-07 risk bodies | **BLOCKED** | Founder HITL admit sovereign risk-register → per-ID typed receipt or `REFUSED_*` (OOR **G3-5**; [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2); contract fence ≠ closure |
| 8 | production `verify_witness.wasm` runtime | Affidavit pin **ALIVE**; env wire **PARTIAL**; production runtime **UNSUPPORTED** (no CI mount) | CI-mounted wasm + `GGEN_VERIFY_WITNESS_WASM` + consume EXIT **0**; [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md); [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| 9 | production brand theme / CASTLE inventory | **UNSUPPORTED** | [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) / OOR G2-12/G2-13 |

---

## Explicit non-claims

- ECOSYSTEM-SPEC-27.1.1 goal is **not** complete.
- Photolithography path crown is **not** `ALIVE`.
- CCS/BRCE Plane-5 DO is **not** `ALIVE`.
- Ops / Gate-0 / revops/cos are **not** `ALIVE` from ggen.
- Foundry zero-HITL is **not** `ALIVE`.
- `ui_layout` **ALIVE (exact tip-cargo)** @ `a508e6bae` (`bc-70188ac3`); **FM-PACK-001** **ALIVE (lineage tip-cargo)** absorbed @ `e73bf686c`; taffy/fluid/hotswap tip-cargo **ALIVE** @ `cde02d1f2`/`19c08a3b0`; `write_apply` **ALIVE** **7+1+1** @ `cde02d1f2`; construct+lifecycle **ALIVE**. Brand rhythm/completeness **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged); `r01_r07` contract **ALIVE** exact tip @ `e7569893a`. Production theme **UNSUPPORTED**. BRCE **V8** **BLOCKED**. OOR: cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL**.
- Presence of this file is documentation standing only — never crown `ALIVE`.

---

## Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — branch `cursor/v2711-crown-progress-bc54` against tip `ca52e9c16` |
| Absorb retip | 2026-11-22 — `cursor/v2711-gate1-uilayout-alive-bc54` absorbs `8090022f4` onto tip `120598fb3` |
| Taffy preflight retip | 2026-11-22 — `cursor/v2711-taffy-preflight-retip-bc54`: `git ls-tree` **PRESENT** blob `d8923b07e` on tip `120598fb3`/`a29e1cd92`/`8090022f4`/`27436608c`; tip-cargo SKIPPED (thrash); standing **PRESENT** / tip-cargo **PENDING** — no tip-cargo `ALIVE` |
| Taffy+brand tip-cargo absorb | 2026-11-22 — `cursor/v2711-taffy-brand-absorb-a77d` tip-wins onto crown `19c08a3b0`: taffy **7** + fluid **6** + hotswap **6** EXIT:0 (warm `/tmp/gate1-taffy`); production **UNSUPPORTED**; supersedes stale CROWN/PHOTO PENDING; prior quiet `7a3f2ab51` @ `bc97ceac9` not ancestor; no ManagePullRequest |
| Absorb tip-cargo CCS | 2026-11-22 — `cursor/v2711-ccs-brce-tipcargo-bc54` @ `cbeb11d44` (evidence `861a6e73a`: construct_hash **4** + es256 **4** + seat **6** exit **0**) docs-only onto crown `c329b7ee5+`; tip-wins clause-2 ALIVE; V8 **BLOCKED**; production/hop-5 crown pins preserved; no cargo; no ManagePullRequest |
| Absorb igniter clean-room | 2026-11-22 — tip-wins cherry-pick `cursor/v2711-igniter-cleanroom-alive-bc54` @ `49e986ee7` onto crown `42717a7a0c8e2a67cdbeebdab259480eaf90138c`; evidence tip `f1492ed55` / `bc-53b0e099` mount `git diff --exit-code` → **0** → clause-1 clean-room **ALIVE**; Jobs/C4 tip-wins **ALIVE** retained (no mix/`write_apply` thrash); OOR **BLOCKED** unchanged; docs-only; no cargo; no ManagePullRequest |
| Absorb igniter clean-room reconfirm | 2026-11-22 — tip-wins absorb `cursor/v2711-igniter-reconfirm-bc54` @ `ec079cc16` (`bc-fd60df35`) onto crown `d0fec2f2f` (⊇ HITL `--live` BLOCKED_HITL + Smalt residual mix + OOR retip12 + affidavit `7ddfd8b6e` + BRCE-V8/VW/Smalt): clean-room **ALIVE** EXIT:**0** (`/tmp/cursor/igniter-cleanroom-reconfirm.md`); photolithography path crown remains **PARTIAL** (not promoted); HITL/Smalt/affidavit/OOR honesty retained; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| CCS residual honesty | 2026-11-22 — `cursor/v2711-ccs-residual-honesty-bc54` on tip `008a81d21`: CCS #3 **ALIVE** retained; CCS #2 tip-cargo later **ALIVE** @ `cde02d1f2`; `lifecycle.rs` **ALIVE** (tip `513704d04`; tip-cargo 6/6 @ `acd9705c1`); **V8**/**V9** **BLOCKED** unchanged; docs-only; no cargo; no ManagePullRequest |
| Gate-1 honesty retip | 2026-11-22 — `cursor/v2711-gate1-honesty-retip-5536` tip-wins onto `fdcad534d`: pin `write_apply` **ALIVE** **7+1+1** @ `cde02d1f2`; taffy/fluid/hotswap **ALIVE** @ `cde02d1f2`; demote brand rhythm/completeness → **COMPILING**/**PARTIAL_ALIVE**; `r01_r07` → **PARTIAL**/**COMPILING**; construct+lifecycle **ALIVE**; BRCE **V8** **BLOCKED**; OOR cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL**; docs-only; no cargo; no ManagePullRequest |
| r01 + brand lineage tip-cargo absorb | 2026-11-22 — `cursor/v2711-r01-lineage-alive-2a69`: r01 **7** EXIT:0 @ `cd7c2433b` → **ALIVE (lineage tip-cargo)**; brand **5+12** @ `c91b22561` reaffirmed (honesty COMPILING demotion corrected to lineage ALIVE); bodies **BLOCKED**; exact-tip reconfirm SCHEDULED; dual thrash PID 1065550 killed; no ManagePullRequest |
| Truex+shui honesty retip | 2026-11-22 — `cursor/v2711-truex-shui-honesty-9045` tip-wins onto `c5101d597` (retains r01 lineage ALIVE @ `cd7c2433b`): Truex admit-court **ALIVE** @ `68d2d233`/`b4672131`; ggen `shui:` fence → **ALIVE (lineage tip-cargo)** @ `008a81d21` (sources ≡ tip; **no tip EXIT:0**); Hop 1 tip-cargo PENDING cleared; docs-only; no cargo; no ManagePullRequest |
| Gate-1 honesty retip (FM-PACK absorb tip) | 2026-11-22 — `cursor/v2711-gate1-honesty-retip-dd93` tip-wins onto `e73bf686c`: pin tip to FM-PACK-001 ALIVE absorb after OOR retip9 `fbfaa3ad9`; brand_toml exact tip-cargo **ALIVE** unchanged @ `77eb46fd0`; `ui_layout` lineage **ALIVE** (exact-tip then in-flight @ `a508e6bae`/`bc-70188ac3`); OOR cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL** / BRCE **V8** **BLOCKED** / R-01…R-07 bodies **BLOCKED**; docs-only; no cargo; no ManagePullRequest |
| ui_layout exact tip-cargo ALIVE | 2026-11-22 — `cursor/uilayout-exact-tip-alive-9f6c` (`bc-70188ac3`) tip-wins: exclusive `/tmp/ggen-uilayout-exact-tip` → **11 passed; EXIT:0** @ cargo tip `a508e6bae` (sources ≡ live tip); promote lineage → **ALIVE (exact tip-cargo)**; no ManagePullRequest |
| Affidavit gatekeeper size-court tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-vw-affidavit-court-bc54` @ `cc0ea07e4` (probe `bc-5f5786ff`) onto crown `1dbf1cc2e` (⊇ BRCE-V8 `1dbf1cc2e` + VW-prod `fe72be0fb` + Smalt `9173c6529` + OOR retip11 + CASTLE): affidavit pin size court **ALIVE** (EXIT **0**; `wc -c` **28673** @ `563eb322a`); ggen `GGEN_VERIFY_WITNESS_WASM` wire **UNSUPPORTED**/PARTIAL; **no** invented ggen production `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| verify_witness env-wire tip-wins absorb | 2026-11-22 — tip-wins code `f65434315` (rebase `b2f6e12d2` / `cursor/v2711-vw-env-wire-bc54` onto `098f53ea8`) + honesty retip: `AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES=256_000` in `verify_production_witness`; Chicago **8/8** EXIT **0**; env wire **PARTIAL**; size court **ALIVE**; production runtime **UNSUPPORTED** (no CI-mounted real wasm); **no** invented production `ALIVE`; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Founder HITL `--live` next-verifier tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-hitl-live-next-bc54` @ `723c3a14f` (`bc-ea2b3d10`) onto crown `d0fec2f2f` via `cursor/v2711-hitl-live-absorb-8df9` (⊇ Smalt residual mix `d0fec2f2f` + OOR retip12 `79b64496b` + affidavit `7ddfd8b6e` + BRCE-V8 `1dbf1cc2e` + VW/CASTLE): `--live` stays **BLOCKED_HITL** / **foundry_exit=1**; exact next `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**; **no** invented `--live` `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Founder HITL `--live` re-probe tip-wins absorb | 2026-11-22 — §6 body tip-wins via `7ea96a70d` (CI blank-EOF + HITL retip of `0364129c5` / `bc-cdfdae43`); `cursor/v2711-hitl-live-retip-absorb-d91a` finishes absorb honesty + CROWN pin onto `7ea96a70d` (⊇ CASTLE digest-bind / VW CI-wasm / OOR retip13 / CI blank-EOF): exact §3 re-run **foundry_exit=1** / **REFUSED** (`Model access is not configured`); standing remains **BLOCKED_HITL**; exact next `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**; **no** invented `--live` `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| zcode POWL refusal stall tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-zcode-refusal-stall-bc54` @ `43c941f88` (`bc-2e1bd867`) onto crown `df9597c18` via `cursor/v2711-zcode-refusal-stall-absorb-6523` (OOR retip14 **ABSENT** → zcode-only): lineage tip-cargo **ALIVE** (`c91b22561` ⊆ crown; courts @ `cd7c2433b`); exact-tip reconfirm **STALLED** (reconfirm remote **ABSENT** at that fetch); **no** invented ALIVE from stalled agents; HITL `--live` **BLOCKED_HITL** held; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) — **superseded** by reconfirm ALIVE absorb below |
| zcode POWL refusal reconfirm ALIVE + OOR retip14 tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-zcode-refusal-reconfirm-bc54` @ `8bd3fe407` (`bc-934bba30`: sjira **6**/bound **4** EXIT **0** @ tip `df9597c18` / zcode-cli `f32df08`) + `cursor/v2711-oor-retip14-bc54` @ `94e5318c6` onto crown tip `1f10949a7` (⊇ stall pin `43c941f88`) via `cursor/v2711-zcode-alive-oor-absorb-2230`: refusal **ALIVE**; `--live` **BLOCKED_HITL**; path crown **PARTIAL**; OOR cos **503** / healthz **200** / revops **NXDOMAIN** held; stall **STALLED** superseded; **refusal ALIVE ≠ `--live` ALIVE**; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Founder HITL `--live` re-probe retip2 (sidebranch) | 2026-11-22 — `cursor/v2711-hitl-retip2-bc54` (`bc-43dfbc3c`) from crown tip `01eccfffb` (⊇ refusal ALIVE + OOR retip14): exact `bun scripts/powl-foundry-loop.ts --live` @ zcode `f32df08` → **foundry_exit=1** / **aggregateStanding=REFUSED** / diagnostic `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.`; registry **ABSENT**; provider env **ABSENT**; vendor **PRESENT**; standing held **BLOCKED_HITL** — **not** `ALIVE`; exact next still `foundry_exit=0`; docs-only; no cargo; no ManagePullRequest; **sidebranch push only** (no crown push) |
| Clause-2 BRCE V8 / R01 bodies tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-brce-v8-bodies-bc54` @ `e54d58bac` onto crown `fe72be0fb` (⊇ VW absorb `fe72be0fb` + Smalt GA residual `9173c6529` + OOR retip11 `f5f44571f` + CASTLE `b01224bfa`): seat fence + R01 *contract* exact tip-cargo **ALIVE** retained @ `e7569893a`; BRCE **V8** broker **BLOCKED** (owner **BRCE**; Founder HITL; OOR **G2-3**); R-01…R-07 *bodies* **BLOCKED** (owner risk-register; Founder HITL; OOR **G3-5**); **no** invented V8/bodies `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Sources | GATE-1-GAP-MATRIX, PHOTOLITHOGRAPHY, FOUNDRY-FINISH, OUT-OF-REPO-NEXT-VERIFIERS, SMALT-RAID, CCS-AUTHORITY, BRCE-WEBAUTHN, ECOSYSTEM-SPEC §13 |
| Method | Absorb tip-cargo ALIVE standing; no new cargo; no ManagePullRequest |
