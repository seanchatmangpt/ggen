# OPS-SURFACES-ON2-STANDING — O(N)→O(N²) ops honesty retip (v27.1.1)

```text
tip_base=ab0a523620f19fc2d697b6ccc782e1a881baf433
branch=cursor/v2711-ops-brand-alive-bc54
agent=bc-fc25b5e7-b186-5e3d-972e-bb46f6c07df7
utc=2026-11-22T18:57:15Z
cargo=none (docs-only retip; brand tip-cargo already sealed)
ManagePullRequest=not_used
Foundry_ALIVE=not_claimed
cos_ALIVE=not_claimed
revops_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `OPS-SURFACES-ON2-STANDING` |
| Branch | `cursor/v2711-ops-brand-alive-bc54` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `ab0a523620f19fc2d697b6ccc782e1a881baf433` |
| Prior pin | `e6d00965a0b0b96df9e284d857de1371c89a3dd2` (`cursor/v2711-ops-surfaces-retip-bc54`) — superseded by this brand-ALIVE retip |
| Scope | Docs-only push-first honesty for O(N)→O(N²) ops surfaces — **no heavy cargo** |
| Companions | [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md), [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md), [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md), [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md), [`VERIFY-WITNESS-SIZE-RECONFIRM-STANDING.md`](./VERIFY-WITNESS-SIZE-RECONFIRM-STANDING.md), [`OOR-COS-REVOPS-PROBE-STANDING.md`](./OOR-COS-REVOPS-PROBE-STANDING.md), [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md), [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md), [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md), [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This ledger re-pins O(N)→O(N²) ops honesty onto live tip `ab0a52362` after brand tip-cargo sealed **ALIVE** (hot-swap @ `c4e757a14` / land `3ae705d1c+`; completeness+rhythm @ `2685119bd` / land `bf3165322`). **brand tip-cargo `ALIVE` ≠ Foundry `ALIVE` ≠ production hot-swap `ALIVE`.** Do not invent tip-subject production `ALIVE`, Plane-5 product `ALIVE`, cos/revops `ALIVE`, or Foundry / zero-HITL crown `ALIVE`. Light probes only (HTTP/DNS/`wc -c`/smalt HEAD).

---

## 1. Standing table (O(N)→O(N²) ops surfaces)

| Surface | Standing | Evidence tip/SHA | Size / EXIT if any | Exact next verifier |
|---------|----------|------------------|--------------------|---------------------|
| **brand.toml tip-cargo** (hot-swap + completeness + rhythm) | **ALIVE (exact tip-cargo)** | tip `ab0a52362` ⊇ hot-swap EXIT:**0**/**6** @ cargo tip `c4e757a14` (land `3ae705d1c+`; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md)) ∧ completeness **5** + rhythm **12** EXIT:**0** @ cargo tip `2685119bd` (land `bf3165322`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)) — sealed at those cargo tips; **≠** live-tip reconfirm | hot-swap **6**/EXIT:**0**; completeness **5** + rhythm **12**/EXIT:**0** | sealed — replay only if sources drift: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0**; `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |
| **brand.toml production theme / production hot-swap DO** | **UNSUPPORTED** (`REFUSED_PRODUCTION`) | tip `ab0a52362` ⊇ [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md); `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` — **tip-cargo ALIVE ≠ production ALIVE** | tip-cargo this land: **none**; production refuse held | `cargo test -p ggen-graph --lib layout::brand::production_theme_tests` → expect `REFUSED_PRODUCTION` / **UNSUPPORTED** held |
| **Smalt 4-Cloud RAID** / GA economic face | **ALIVE (lineage)** | ggen tip `ab0a52362` ⊇ absorb `2c144954c` / base `571571720`; smalt pin `91997da76f0f` (parent `875efbb87`); warm `/tmp/smalt-probe` HEAD match | FinOps EXIT **0** / **19** + treasury **8** + tax/erp/rates **0,0,0** retained @ `91997da76f0f`; 4-cloud **0,0,0,0,0** @ `875efbb87` — **not re-run** | `cd /path/to/smalt # tip 91997da76f0f356dc2be914bdb0a5b8227518719` → `mix test test/fin_ops_calculator_test.exs` → exit **0** / **19 passed** ([`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4.2) |
| **`verify_witness` size** (&lt;250KB) | **ALIVE (size_measure)** | tip lineage ⊇ retip3 / reconfirm measure; this land `wc -c` reconfirm @ tip `ab0a52362` | **`wc -c` = 28673** &lt; 250000 → measure **ALIVE**; tip-cargo size court **not this land** | Replay measure: `wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` → **28673**. Tip-cargo (optional): `CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip3-court cargo test -p ggen-graph --test verify_witness_size_court --locked --config build.jobs=1 -- --test-threads=1` → **8**/EXIT:**0** |
| **cos.fnox.io `/readyz`** | **BLOCKED** (HTTP **503** `DB_UNAVAILABLE`) | tip `ab0a52362`; probe `2026-11-22T18:57:15Z` | `readyz` **503**; `/healthz` **200** → liveness **PARTIAL** ≠ readiness | `curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → expect HTTP **200** + admitted ready body (**not** `DB_UNAVAILABLE`) |
| **revops.fnox.io** front door | **UNKNOWN** (`NXDOMAIN`) | tip `ab0a52362`; probe `2026-11-22T18:57:15Z` | `getent` exit **2** (empty); `curl` exit **6** / `http=000` | `getent hosts revops.fnox.io` → non-empty A/AAAA; then `curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → HTTP **200** |

### 1a. Adjacent spine honesty (held; not promoted)

| Surface | Standing | Evidence tip/SHA | Exact next verifier |
|---------|----------|------------------|---------------------|
| **CASTLE** digest-bind wire | **ALIVE**/`WIRED` (sibling); spine crown **PARTIAL_ALIVE** | castle `e0d99c6`; sibling courts @ `2774039ea` | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13.2 / §14.2 |
| **Foundry `--live`** (hop-5) | **BLOCKED_HITL** / **REFUSED** | zcode `f32df08+`; **foundry_exit=1** / `Model access is not configured` | Founder `/login`\|`/setup` → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** ([`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md)) |
| **Foundry / zero-HITL crown** | **PARTIAL** — **not** `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Close cos **503** + revops DNS + `--live` + BRCE V8 before any crown promote |

---

## 2. Light probes this land (2026-11-22T18:57:15Z)

| Probe | Result | Standing implication |
|-------|--------|----------------------|
| `git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54` | `ab0a523620f19fc2d697b6ccc782e1a881baf433` | tip base |
| `git merge-base --is-ancestor 2685119bd HEAD` | exit **0** | brand completeness/rhythm cargo tip **held** |
| `git merge-base --is-ancestor 3ae705d1c HEAD` | exit **0** | brand hot-swap ALIVE land **held** |
| `git merge-base --is-ancestor bf3165322 HEAD` | exit **0** | brand tip-cargo completeness/rhythm land **held** |
| `git merge-base --is-ancestor 2c144954c HEAD` | exit **0** | Smalt RAID absorb lineage **held** |
| `git -C /tmp/smalt-probe rev-parse HEAD` | `91997da76f0f356dc2be914bdb0a5b8227518719` | smalt pin **held** |
| `wc -c …/affidavit_gatekeeper_verify_witness.wasm` | **28673** | VW size measure **ALIVE** held |
| `curl https://cos.fnox.io/healthz` | HTTP **200** `{"status":"ok"}` | liveness **PARTIAL** |
| `curl https://cos.fnox.io/readyz` | HTTP **503** `{"status":"DB_UNAVAILABLE"}` | readiness **BLOCKED** |
| `getent hosts revops.fnox.io` | exit **2** (empty) | revops DNS **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | revops **UNKNOWN** |
| Heavy cargo / mix | **not run** | brand tip-cargo already sealed; no invent |

---

## 3. Honesty fences (do not invent)

1. **brand tip-cargo `ALIVE` ≠ Foundry `ALIVE` ≠ production hot-swap `ALIVE`.** Production theme / production DO stays **UNSUPPORTED** (`REFUSED_PRODUCTION`).
2. Hot-swap fence API ceiling remains `HOT_SWAP_STANDING_CEILING` = **PARTIAL_ALIVE** on receipts — court tip-cargo EXIT **0** seals Gate-1 fence cargo only.
3. Smalt RAID **ALIVE (lineage)** ≠ Foundry / zero-HITL crown `ALIVE`.
4. VW size measure **ALIVE** (`28673`) ≠ tip-cargo size-court `ALIVE` this land ≠ production runtime `ALIVE`.
5. cos `/healthz` **200** ≠ cos product / ledger `ALIVE`; `/readyz` **503** stays **BLOCKED** — **never invent cos `ALIVE`**.
6. revops **NXDOMAIN** stays **UNKNOWN** — **never invent revops `ALIVE`**; never rewrite DNS failure as success.
7. Foundry `--live` stays **BLOCKED_HITL** until `foundry_exit=0`; Foundry crown stays **PARTIAL** — **never invent Foundry `ALIVE`**.

---

## 4. Exact next verifiers — cos / revops (highest-leverage OOR)

Plane-5 readiness is the highest-leverage residual that unblocks CASTLE LinkedIn AND-gate + Foundry OOR adjacency without competing with Gate1 tip7 cargo:

```bash
# cos readiness (pass ⇒ readiness may advance from BLOCKED; still ≠ product ALIVE alone)
TZ=America/Los_Angeles date '+%Y-%m-%d %H:%M:%S %Z'
date -u '+%Y-%m-%dT%H:%M:%SZ'
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
# pass criterion: HTTP 200 + admitted ready body (not DB_UNAVAILABLE)
# until then: cos readiness BLOCKED; do not invent cos ALIVE
```

```bash
# revops front door (pass ⇒ DNS present ∧ readiness 200; still ≠ product ALIVE alone)
getent hosts revops.fnox.io
# pass criterion: non-empty A/AAAA (exit 0)
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz
# pass criterion: HTTP 200 + admitted ready body
# until then: revops UNKNOWN (NXDOMAIN / resolve failure); do not invent revops ALIVE
```

Plane-5 product `ALIVE` requires **both** cos `/readyz` **200** ∧ revops `/readyz` **200**. Neither subject is `ALIVE` from this ledger.

---

## 5. Row deltas (vs prior pin `e6d00965a` / `cursor/v2711-ops-surfaces-retip-bc54`)

| Surface | Prior standing | This retip | Delta |
|---------|----------------|------------|-------|
| brand.toml tip-cargo (hot-swap + completeness/rhythm) | tip-cargo this land **none**; production-focused row **UNSUPPORTED** | **ALIVE (exact tip-cargo)** @ `2685119bd` (+ hot-swap @ `c4e757a14`) | **promoted** — tip now ⊇ sealed brand tip-cargo ALIVE |
| brand.toml production theme / production hot-swap | **UNSUPPORTED** | **UNSUPPORTED** | **held** (explicit separate row) |
| Smalt 4-Cloud RAID | **ALIVE (lineage)** | **ALIVE (lineage)** | **held** |
| `verify_witness` size | **ALIVE (28673)** | **ALIVE (28673)** | **held** (reconfirm) |
| cos `/readyz` | **BLOCKED** (503) | **BLOCKED** (503) | **held** (reconfirm `2026-11-22T18:57:15Z`) |
| revops front door | **UNKNOWN** (NXDOMAIN) | **UNKNOWN** (NXDOMAIN) | **held** (reconfirm) |
| Foundry / zero-HITL | **PARTIAL** / **BLOCKED_HITL** | **PARTIAL** / **BLOCKED_HITL** | **held** — not invented |

---

## 6. Compact return

```text
tip_base=ab0a52362
branch=cursor/v2711-ops-brand-alive-bc54
brand_tip_cargo=ALIVE(hotswap@c4e757a14+completeness/rhythm@2685119bd)
brand_production=UNSUPPORTED
smalt=ALIVE(lineage)@91997da76f0f
vw_size=ALIVE(28673)
cos_readyz=BLOCKED(503)
revops=UNKNOWN(NXDOMAIN)
foundry_live=BLOCKED_HITL
Foundry_ALIVE=not_claimed
cargo=none ManagePullRequest=not_used
```
