# OPS-SURFACES-ON2-STANDING — O(N)→O(N²) ops honesty retip (v27.1.1)

```text
tip_base=e6d00965a0b0b96df9e284d857de1371c89a3dd2
branch=cursor/v2711-ops-surfaces-retip-bc54
agent=bc-af14f771-10b9-5200-8884-79f46c0a0c12
utc=2026-11-22T18:24:30Z
cargo=none (Gate1 tip7 serial owns EXIT0 cargos)
ManagePullRequest=not_used
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `OPS-SURFACES-ON2-STANDING` |
| Branch | `cursor/v2711-ops-surfaces-retip-bc54` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `e6d00965a0b0b96df9e284d857de1371c89a3dd2` |
| Scope | Docs-first push-first honesty for O(N)→O(N²) ops surfaces — **no heavy cargo** |
| Companions | [`BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md`](./BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md), [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md), [`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md), [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md), [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md), [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md), [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This ledger re-pins known honesty onto live tip `e6d00965a` without inventing tip-subject tip-cargo `ALIVE`, production `ALIVE`, Plane-5 product `ALIVE`, or Foundry / zero-HITL crown `ALIVE`. Light probes only (HTTP/DNS/`wc -c`/smalt HEAD); Gate1 tip7 owns EXIT0 cargos.

---

## 1. Standing table (O(N)→O(N²) ops surfaces)

| Surface | Standing | Evidence tip/SHA | Size / EXIT if any | Exact next verifier |
|---------|----------|------------------|--------------------|---------------------|
| **brand.toml hot-swap** (production theme / production DO) | **UNSUPPORTED** (`REFUSED_PRODUCTION`) | tip `e6d00965a` ⊇ prod-retip honesty; court blob lineage ≡ `22e417ab3` / ALIVE absorb `19c08a3b0`/`cde02d1f2` — **lineage ≠ production** | tip-cargo this land: **none**; production refuse held | `cargo test -p ggen-graph --lib layout::brand::production_theme_tests` → expect `REFUSED_PRODUCTION` / **UNSUPPORTED** held. Court tip-subject (orthogonal, thrash-owned): `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| **Smalt 4-Cloud RAID** / GA economic face | **ALIVE (lineage)** | ggen tip `e6d00965a` ⊇ absorb `2c144954c` / base `571571720`; smalt pin `91997da76f0f` (parent `875efbb87`); warm `/tmp/smalt-probe` HEAD match | FinOps EXIT **0** / **19** + treasury **8** + tax/erp/rates **0,0,0** retained @ `91997da76f0f`; 4-cloud **0,0,0,0,0** @ `875efbb87` — **not re-run** (`mix` spare) | `cd /path/to/smalt # tip 91997da76f0f356dc2be914bdb0a5b8227518719` → `mix test test/fin_ops_calculator_test.exs` → exit **0** / **19 passed** ([`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4.2) |
| **`verify_witness` size** (&lt;250KB) | **ALIVE (size_measure)** | tip lineage ⊇ retip3 `2685119bd` / absorb onto `0c18b3d1a+`; measure reconfirm this land | **`wc -c` = 28673** &lt; 250000 → measure **ALIVE**; tip-cargo size court last **EXIT 137** thrash → **PARTIAL** (not this land) | Replay measure: `wc -c /tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` → **28673**. Tip-cargo (Gate1 thrash clear): `CARGO_TARGET_DIR=/tmp/ggen-vw-size-retip3-court cargo test -p ggen-graph --test verify_witness_size_court --locked --config build.jobs=1 -- --test-threads=1` → **8**/EXIT:**0** |
| **cos.fnox.io `/readyz`** | **BLOCKED** (HTTP **503** `DB_UNAVAILABLE`) | tip `e6d00965a`; probe `2026-11-22T18:24:30Z` | `readyz` **503**; `/healthz` **200** → liveness **PARTIAL** ≠ readiness | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → expect HTTP **200** (not `DB_UNAVAILABLE`) |
| **revops.fnox.io** front door | **UNKNOWN** (`NXDOMAIN`) | tip `e6d00965a`; probe `2026-11-22T18:24:30Z` | `host` **NXDOMAIN**; `curl` exit **6** / `http=000` | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200** |

### 1a. Adjacent spine honesty (held; not promoted)

| Surface | Standing | Evidence tip/SHA | Exact next verifier |
|---------|----------|------------------|---------------------|
| **CASTLE** digest-bind wire | **ALIVE**/`WIRED` (sibling); spine crown **PARTIAL_ALIVE** | castle `e0d99c6`; sibling courts @ `2774039ea` | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13.2 / §14.2 |
| **Foundry `--live`** (hop-5) | **BLOCKED_HITL** / **REFUSED** | zcode `f32df08+`; **foundry_exit=1** / `Model access is not configured` | Founder `/login`\|`/setup` → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** ([`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md)) |
| **Foundry / zero-HITL crown** | **PARTIAL** — **not** `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Close cos **503** + revops DNS + `--live` + BRCE V8 before any crown promote |

---

## 2. Light probes this land (2026-11-22T18:24:30Z)

| Probe | Result | Standing implication |
|-------|--------|----------------------|
| `git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54` | `e6d00965a0b0b96df9e284d857de1371c89a3dd2` | tip base |
| `git merge-base --is-ancestor 2c144954c HEAD` | exit **0** | Smalt RAID absorb lineage **held** |
| `git -C /tmp/smalt-probe rev-parse HEAD` | `91997da76f0f356dc2be914bdb0a5b8227518719` | smalt pin **held** |
| `wc -c …/affidavit_gatekeeper_verify_witness.wasm` | **28673** | VW size measure **ALIVE** held |
| `curl https://cos.fnox.io/healthz` | HTTP **200** `{"status":"ok"}` | liveness **PARTIAL** |
| `curl https://cos.fnox.io/readyz` | HTTP **503** `{"status":"DB_UNAVAILABLE"}` | readiness **BLOCKED** |
| `host revops.fnox.io` | **NXDOMAIN** | revops **UNKNOWN** |
| Heavy cargo / mix | **not run** | Gate1 tip7 owns EXIT0 cargos |

---

## 3. Honesty fences (do not invent)

1. Production brand hot-swap stays **UNSUPPORTED** — lineage hot-swap court `ALIVE` @ `19c08a3b0` ≠ production theme `ALIVE`.
2. Smalt RAID **ALIVE (lineage)** ≠ Foundry / zero-HITL crown `ALIVE`.
3. VW size measure **ALIVE** (`28673`) ≠ tip-cargo size-court `ALIVE` this land ≠ production runtime `ALIVE`.
4. cos `/healthz` **200** ≠ cos product / ledger `ALIVE`; `/readyz` **503** stays **BLOCKED**.
5. revops **NXDOMAIN** stays **UNKNOWN** — never rewrite DNS failure as success.
6. Foundry `--live` stays **BLOCKED_HITL** until `foundry_exit=0`.

---

## 4. Highest-value next verifier (single command)

Plane-5 readiness is the highest-leverage residual that unblocks CASTLE LinkedIn AND-gate + Foundry OOR adjacency without competing with Gate1 tip7 cargo:

```bash
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
# pass criterion: HTTP 200 with admitted ready body (not DB_UNAVAILABLE)
# until then: cos readiness BLOCKED; Plane-5 product not ALIVE
```

---

## 5. Compact return

```text
tip_base=e6d00965a
branch=cursor/v2711-ops-surfaces-retip-bc54
brand_hotswap=UNSUPPORTED
smalt=ALIVE(lineage)@91997da76f0f
vw_size=ALIVE(28673)
cos_readyz=BLOCKED(503)
revops=UNKNOWN(NXDOMAIN)
foundry_live=BLOCKED_HITL
cargo=none ManagePullRequest=not_used
```
