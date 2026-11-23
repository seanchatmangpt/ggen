# Gate 0 / 2 / 3 — OOR standing honesty matrix (through GA 2027-01-01)

| Field | Value |
|-------|-------|
| Document ID | `GATE023-OOR-STANDING-MATRIX-27.1.1` |
| Binding | `ECOSYSTEM-SPEC-27.1.1` Gate 0 + Gate 2–3 out-of-repo legs; GA **2027-01-01** |
| Companion index | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md) |
| Sidebranch | `cursor/v2711-construct-alive-retip-bc54` (matrix origin `cursor/v2711-gate023-oor-matrix-bc54`) |
| Observed ggen tip (write base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`59c399935`** (⊇ cos DB pin + Construct **ALIVE** + Gate1 tip7 + G2-11); Foundry BLOCKED_HITL / cos BLOCKED / **revops host UNSUPPORTED (MISSING)** |
| Probe time | 2026-11-22 4:35:00 PM PST / `2026-11-23T00:35Z` (retip30) |
| Cargo / tip-cargo | Gate1 tip7 **ALIVE**; Construct **ALIVE** @ `45086131007`; Foundry `--live` **foundry_exit=1**; fnox GTM: `revops.fnox.io` **MISSING** |
| Crown absorb | tip-wins onto `origin/cursor/ecosystem-spec-27-1-1-bc54` (no force; parent owns ManagePullRequest) |
| Companion retip | [`CONSTRUCT-RECEIPT-EXCLUDE-STANDING.md`](./CONSTRUCT-RECEIPT-EXCLUDE-STANDING.md); [`VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md`](./VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md) |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** No row is promoted to `ALIVE` from this matrix. Connector inventory,
sibling tip SHAs, fixture/fence tip-cargo, and docs presence are observation only
until the named next verifier exits **0** (or a typed `REFUSED_*` is filed with
owner). Dual facets stay explicit (fixture **PARTIAL** ≠ live **UNSUPPORTED**).
Primary standing is the GA-facing subject of the next verifier. **Founder next**
for hop-5 / Foundry live remains `bun bin/zcode.js login` + `bun scripts/powl-foundry-loop.ts --live` (**not** bare `zcode /login`)
where applicable — **do not invent** `--live` / Foundry / Plane-5 `ALIVE`.

Owner column: **Founder HITL** = human must provision / actuate; **Agent** =
automatable on an admitted mount once preconditions hold (still not ambient DO).

---

## Tip probe reconfirm (2026-11-22 11:02:01 PST) — retip19

Held vs OOR retip18 / Gate023 matrix @ `4060f9d6b` — standings **held**; no false `ALIVE`.
Task probe hosts `*.ggen.dev` are **NXDOMAIN** aliases (not the tracked Plane-5 subjects).

| Probe | Result | Standing |
|-------|--------|----------|
| `curl … https://cos.fnox.io/readyz` | **503** `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl … https://cos.fnox.io/healthz` | **200** `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `getent hosts revops.fnox.io` / `dig +short` | empty / **NXDOMAIN** | **UNSUPPORTED** (host MISSING — fnox GTM; not a DNS-restore subject) |
| `curl … https://revops.fnox.io/` (and `/readyz`) | exit **6** / `http=000` | **UNSUPPORTED** (do not invent host) |
| `curl … https://cos.ggen.dev/readyz` | exit **6** / `http=000` (**NXDOMAIN**) | alias **UNKNOWN** (≠ readiness claim) |
| `host revops.ggen.dev` / `curl … https://revops.ggen.dev/` | **NXDOMAIN** / exit **6** | alias **UNKNOWN** |

Exact next verifiers (unchanged):

```text
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
# pass: HTTP 200 (not DB_UNAVAILABLE)
getent hosts revops.fnox.io
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz
# pass: DNS non-empty ∧ HTTP 200
```

---

## Standing count @ tip `dd61fefb2` (25 tracked OOR rows + named crowns; counts held)

| Standing (primary) | Count | IDs / subjects |
|--------------------|------:|----------------|
| **ALIVE** | **6** | G2-6, G2-7, G2-10, G2-11, G2-12, G3-2 |
| **PARTIAL** / **PARTIAL_ALIVE** | **3** | G2-8, G2-9, G3-3 |
| **BLOCKED** | **8** | G0-1, G0-2, G0-3, G2-2, G2-3, G2-5, G2-16, G3-5 |
| **UNSUPPORTED** | **4** | G2-1, G2-4, G2-13, G3-1 |
| **UNKNOWN** | **4** | G0-4, G2-14, G2-15, G3-4 |
| **Total tracked** | **25** | G0-1…4 + G2-1…16 + G3-1…5 |

Named companion crowns (not extra G-rows): Foundry zero-HITL **PARTIAL**; foundry `--live` **BLOCKED_HITL**; BRCE **V8** **BLOCKED**; R-01…R-07 bodies **BLOCKED**; live WebAuthn **UNSUPPORTED**; CASTLE wire **ALIVE**/`WIRED` (≡ G2-12 facet); Smalt RAID **ALIVE** (≡ G2-6).

---

## Named OOR crown subjects (push-first honesty)

| Subject | Standing | Evidence tip | Exact next verifier | Owner |
|---------|----------|--------------|---------------------|-------|
| **Foundry `--live`** / hop-5 (G2-9) | **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`; `Model access is not configured` @ zcode `f32df08`) — Foundry crown **PARTIAL** ≠ `--live` `ALIVE` | tip `4060f9d6b`; pins [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10; [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a/§9; [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** | **Founder HITL** (+ zcode-cli surface) |
| **Zero-HITL Foundry crown** | **PARTIAL** — **not** Foundry `ALIVE` | tip `4060f9d6b`; [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Close HITL residuals: cos DB + WebAuthn live + `--live` → **0** before any crown promote | **Founder HITL** (residuals) / **Agent** (replay fences only) |
| **cos** readiness + ledger (G0-2 / G2-2) | **BLOCKED** (`/readyz` **503** false-`DB_UNAVAILABLE`; L10: SELECT 1 OK + CREATE under `fnox_cos_app`; billing hold `018FD7…`); `/healthz` **PARTIAL** | tip `65fb49ec9`; [`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md); fnox `OPERATOR-AND-GATE-readyz.md` | Founder: `billing → JWKS → migrate → redeploy → readyz 200` | **Founder HITL** (billing+GCP) / **Agent** (probe once ready) |
| **revops.fnox.io** front door (G2-1 / D6) | **UNSUPPORTED** (host **MISSING**; NXDOMAIN intentional) | tip `59c399935`; fnox `cos/docs/gtm/JOBS-LOCK-NOV21-RUNBOOK.md`; [`REVOPS-HOST-MISSING-STANDING.md`](./REVOPS-HOST-MISSING-STANDING.md) | Do **not** invent DNS. RevOps *function* → cos `/readyz` **200** (G2-2) | **Refuse invent** / **Founder** (cos DB only) |
| **BRCE V8** broker (G2-3) | **BLOCKED** (broker unmounted; V1/V7 fences ≠ V8) | tip `4060f9d6b`; [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3; [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 | After live WebAuthn §3 exit **0**: in mounted BRCE broker — intent **without** AS-01 → `REFUSED_*`; **with** verified seat → execution receipt + replay digest → exit **0** | **Founder HITL** (mount + seat) / **Agent** (court once mounted) |
| **R-01…R-07 bodies** (G3-5 / RC-D7) | **BLOCKED** (all seven; contract fence **ALIVE** **7** @ `e7569893a` ≠ bodies) | tip `4060f9d6b`; [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Admit sovereign risk-register → per-ID typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract-only antifalsifier: `cargo test -p ggen-graph --test r01_r07_resolution_court` | **Founder HITL** (register admit) |
| **WebAuthn AS-01** live/production (G2-4) | **UNSUPPORTED** (live); V7 fixture **PARTIAL_ALIVE** / land **ALIVE** @ `fb6e7fc6e` ≠ live | tip `4060f9d6b`; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered/missing RP-ID/alg≠ES256 → `REFUSED_*`. Then **V8** → **V9**. Non-substitute: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` | **Founder HITL** |
| **zcode-cli** `/login` + POWL foundry live | **BLOCKED_HITL** (same falsifier as Foundry `--live`); adapter/path **PARTIAL**; runtime **PRESENT** ≠ live | tip `4060f9d6b`; zcode `f32df08`; [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §8/§10 | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**. Replay (non-substitute): `bun test test/powl-ocel-chicago.test.ts` → **0** | **Founder HITL** |
| **Gate 0 billing** (G0-1 GCP) | **BLOCKED** (OUT OF SCOPE for ggen DO; contract prose ≠ reconciliation) | tip `4060f9d6b`; [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) §3.1 | Owner artifact: `docs/marketplace/billing-contract.md` DoD + Weekly Reconciliation. Console: `https://console.cloud.google.com/billing` → admitted account Gate 0→GA. Weekly: reconciliation → exit **0** / `✓ RECONCILED` + GCS audit export. ggen must not claim `ALIVE` | **Founder HITL** (infra/finance) |
| **Gate 3 release courts** (RC-D1…D7 aggregate) | Mixed — see Gate 3 table; crown **not** `ALIVE` while any court open | tip `4060f9d6b`; [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | Per-court next verifiers in Gate 3 section below; GA **2027-01-01** refuses silent close | **Founder HITL** (RC-D1/D6/D7) / **Agent** (RC-D2/D3/D4/D5 replay) |

---

## Gate 0 — Nov 21 – Dec 5 (OUT of ggen DO)

Detail: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G0-1** | GCP billing | **BLOCKED** | tip `4060f9d6b`; billing-contract prose **PRESENT** ≠ live | Weekly reconciliation in `docs/marketplace/billing-contract.md` + GCP Billing console receipt → exit **0** / `✓ RECONCILED` | **Founder HITL** |
| **G0-2** | cos ledger fixes | **BLOCKED** | tip `65fb49ec9`; false-`DB_UNAVAILABLE` + billing hold; [`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md) | Founder `billing → JWKS → migrate → redeploy → readyz 200`; then ledger → **0** | **Founder HITL** / **Agent** (probe) |
| **G0-3** | servengine M4 | **BLOCKED** | tip `4060f9d6b`; sibling ABSENT | In mounted servengine: CI/milestone job labeled **M4** → exit **0** + typed receipt | **Founder HITL** / **Agent** (once mounted) |
| **G0-4** | year-end SOW | **UNKNOWN** | tip `4060f9d6b`; no register in ggen | Ops register: signed instrument parties+scope+period through **2027-01-01** | **Founder HITL** |

---

## Gate 2 — out-of-repo subjects

### Plane 5 / BRCE / WebAuthn / Smalt

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G2-1** | revops.fnox.io | **UNSUPPORTED** (MISSING) | tip `59c399935`; fnox GTM forbids host; [`REVOPS-HOST-MISSING-STANDING.md`](./REVOPS-HOST-MISSING-STANDING.md) | Function path = cos G2-2 → `/readyz` **200**; refuse inventing `revops.fnox.io` | **Refuse invent** |
| **G2-2** | cos.fnox.io readiness + ledger | **BLOCKED** (false-`DB_UNAVAILABLE` + billing hold); healthz **PARTIAL** | tip `65fb49ec9`; L10 SELECT 1 OK; need live image `skip_table_creation` + operator AND-gate; [`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md) | Founder `billing → JWKS → migrate → redeploy → readyz 200` | **Founder HITL** / **Agent** (probe) |
| **G2-3** | BRCE human seat broker (**V8**) | **BLOCKED** | tip `4060f9d6b`; [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) | Live WebAuthn first → mounted broker refuse/admit court → exit **0** | **Founder HITL** |
| **G2-4** | WebAuthn AS-01 production | **UNSUPPORTED** (live); V7 **PARTIAL** ≠ live | tip `4060f9d6b`; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) | Founder HITL RP + hardware ceremony → exit **0**; then **V8** | **Founder HITL** |
| **G2-5** | Plane 5 DO path (aggregate) | **BLOCKED** | tip `59c399935`; G2-1 host **UNSUPPORTED** | Ladder: G2-2 cos `/readyz` **200** → AS-01 host court on **cos** (BRCE **V9**) — **not** revops DNS | **Founder HITL** |
| **G2-6** | Smalt 4-cloud RAID / FinOps | **ALIVE** — ≠ Foundry / zero-HITL | tip-wins lineage; smalt `875efbb87` + `91997da76f0f`; [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | Replay FinOps + 4-cloud + residual §4.3 → exits **0** | **Agent** (replay) |

### Manufacture / CASTLE / commercial

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G2-7** | Truex admits `shui:` | **ALIVE** (admit-court); fence exact-tip **PARTIAL** | `truex@68d2d233` merged; tip lineage through `1935ed718` | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → **0** | **Agent** (replay) |
| **G2-8** | zcode `ui-layout-court` / adapter | **PARTIAL** | tip `4060f9d6b`; adapter exit **0**; [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → **0** (G2-9) | **Founder HITL** (live) / **Agent** (adapter replay) |
| **G2-9** | zcode foundry `--live` / HITL | **PARTIAL** (path); live **BLOCKED_HITL** | tip `4060f9d6b`; **foundry_exit=1** | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** | **Founder HITL** |
| **G2-10** | ggen_igniter clean-room / manufacture | **ALIVE** (clean-room + BrandTheme); path crown **PARTIAL** | tip lineage; [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd …/ggen_igniter && git diff --exit-code` → **0**; ≠ `--live` Foundry | **Agent** (replay) |
| **G2-11** | verify_witness production WASM | **ALIVE** (CI Mount pin+consume EXIT **0** @ run `45073805083` / tip `6a6957113`); API `PartialAlive` held | tip `6a6957113`+; [`VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md`](./VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md); local **11**/EXIT:**0** @ `2f826872b` | Replay: workflow `verify-witness-ci-wasm.yml` EXIT **0** | **Agent** (replay) — **≠ Foundry ALIVE** |
| **G2-12** | CASTLE publish / digest-bind | **ALIVE** (sibling courts); spine wire **PARTIAL** (`WIRED`); Foundry **PARTIAL** | castle `e0d99c6`; [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §14 | Replay digest-bind / redaction courts → **0**; ≠ LinkedIn send; ≠ zero-HITL | **Agent** (replay) |
| **G2-13** | Full DTCG brand inventory | **UNSUPPORTED** | tip `4060f9d6b`; rhythm stubs only | Admit OEM token sources; castle/brand inventory court → exit **0** | **Founder HITL** / brand plane |
| **G2-14** | CASTLE CindyCat public mint | **UNKNOWN** | tip `4060f9d6b`; mint receipt absent | `castle publish post --subject cindycat-mint-probe …` + redaction court; depends G2-2 | **Founder HITL** / CASTLE |
| **G2-15** | Event #1 patronage | **UNKNOWN** | tip `4060f9d6b`; [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) | Ops register: 0% custody + named payer + amount **>$250** | **Founder HITL** |
| **G2-16** | LinkedIn/Jobs automated send | **BLOCKED** | tip `4060f9d6b`; cos still **503** | Clear G2-2 → CASTLE AND-gate (cos ready ∧ 2× durable LOW) | **Founder HITL** / CASTLE |

---

## Gate 3 — seven release courts (out-of-repo legs)

Detail: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md). GA horizon: **2027-01-01**.

| ID | Court | Standing | Evidence tip | Exact next verifier | Owner |
|----|-------|----------|--------------|---------------------|-------|
| **G3-1** | **RC-D1** Zero-DOM WebGPU + FFB | **UNSUPPORTED** | tip `4060f9d6b`; UI-plane unmounted | Mounted UI-plane court: admitted `AotSealReceipt` → Mode A/B paint → exit **0**; ambient DOM DO without BRCE+AS-01 → `REFUSED_*` | **Founder HITL** / UI plane |
| **G3-2** | **RC-D3** Truex `shui:` product | **ALIVE** (admit-court; ≡ G2-7); fence **PARTIAL** | `truex@68d2d233` | Same as G2-7 replay | **Agent** (replay) |
| **G3-3** | **RC-D5** zcode ui-layout live | **PARTIAL** (≡ G2-8; foundry_exit=1) | tip `4060f9d6b` | `powl-foundry-loop --live` → **0** (G2-9 / HITL) | **Founder HITL** |
| **G3-4** | **RC-D6** Plane 5 front doors | **PARTIAL** / OUT OF SCOPE from ggen | tip `59c399935`; revops host **UNSUPPORTED**; cos **503** | Ladder: G2-2 cos **200** → G2-5 AS-01 on cos (G2-1 not a DNS restore) | **Founder HITL** |
| **G3-5** | **RC-D7** R-01…R-07 bodies | **BLOCKED** (all seven) | tip `4060f9d6b`; [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Admit sovereign risk-register → per-ID receipt/`REFUSED_*` before **2027-01-01** | **Founder HITL** |

**RC-D2 / RC-D4** in-repo legs (Taffy seal, brand rhythm): tracked in Gate-1 matrices — not duplicated as OOR DO; consumer / inventory legs remain G2-8 / G2-13 for GA `ALIVE`.

---

## Top 3 next verifiers (founder-facing)

| Rank | Subject | Standing | Exact command |
|------|---------|----------|---------------|
| **1** | Foundry `--live` / `bun bin/zcode.js login` | **BLOCKED_HITL** | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| **2** | cos `/readyz` (G0-2 / G2-2) | **BLOCKED** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`) |
| **3** | WebAuthn AS-01 live → BRCE **V8** | **UNSUPPORTED** → **BLOCKED** | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 ceremony → exit **0**; then [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3 broker court → exit **0** |

Honorable mention: G2-1 `revops.fnox.io` is **UNSUPPORTED**/MISSING (fnox GTM) — do **not** chase DNS ALIVE; close cos G2-2 instead.

---

## Explicit non-claims

- This matrix does **not** promote Gate 0 billing / cos / servengine / SOW to `ALIVE`.
- Live WebAuthn stays **UNSUPPORTED**; BRCE **V8**/**V9** and R-01…R-07 bodies stay **BLOCKED**.
- cos `/readyz` **503** and revops **NXDOMAIN** are reconfirmed (retip19 @ 11:02:01 PST) — Plane-5 front doors are **not** `ALIVE`. `*.ggen.dev` aliases are also **NXDOMAIN** (not a readiness promote).
- foundry `--live` stays **BLOCKED_HITL** (`foundry_exit=1`) — refusal tip-cargo / runtime PRESENT / adapter exit 0 ≠ live `ALIVE`.
- Smalt / Truex / CASTLE sibling / igniter clean-room `ALIVE` rows do **not** promote Foundry / zero-HITL / path crown / LinkedIn send.
- No ManagePullRequest on #849. No force-push. No cargo.

---

## Aggregate replay (this ledger)

```text
test -f docs/rfc/v27.1.1/GATE023-OOR-STANDING-MATRIX.md
rg -n '^\| \*\*G[023]-' docs/rfc/v27.1.1/GATE023-OOR-STANDING-MATRIX.md | wc -l
# Expect 25 clause rows (G0×4 + G2×16 + G3×5)
rg -n 'zcode /login|powl-foundry-loop.ts --live|BLOCKED_HITL|2027-01-01' docs/rfc/v27.1.1/GATE023-OOR-STANDING-MATRIX.md
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
getent hosts revops.fnox.io; curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz
```

---

## Cross-links

| Doc | Role |
|-----|------|
| [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | Full Gate 0/2/3 provenance + GA notes |
| [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md) | Compact next-verifier matrix |
| [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) | Gate 0 billing / cos / servengine / SOW |
| [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) | Commercial priority |
| [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | D1–D7 release courts |
| [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Zero-HITL / horizontal spine |
| [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | cos / revops probe ledger |
| [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) | Founder `--live` action card |
| [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) | V8 broker pin |
| [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) | AS-01 live ceremony pin |
| [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | RC-D7 bodies pin |
| [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §17–§18 | Compact surface→standing retips |

---

## Provenance

Materialized 2026-11-22 on `cursor/v2711-gate023-oor-matrix-bc54` (`bc-0754e974`)
from `origin/cursor/ecosystem-spec-27-1-1-bc54` @
`4060f9d6b59d4989c5534408bf51845877d765e8`. Synthesized from Gate0/2/3 companions,
OOR ledger, Foundry / HITL / BRCE V8 / WebAuthn live / R01-R07 bodies / Smalt /
CASTLE pins, plus HTTP+DNS probe reconfirm at 10:29:22 PST. Standings **held**;
**25** tracked rows; **no** invented `ALIVE`; docs-only; no cargo; no
ManagePullRequest; tip-wins absorb onto crown (no force).

Tip-wins absorb side `de9682e3ff1b017b666801c1826bb3177f360f60` onto crown parent `4060f9d6b59d4989c5534408bf51845877d765e8` (no force; no ManagePullRequest).

Retip19 (`cursor/v2711-oor-retip19-bc54` / `bc-f83f4cbf`) @ tip
`dd61fefb29fa7ea6e1cf789f002ccd9879393988` — probe 2026-11-22 11:02:01 PST /
`2026-11-22T19:02:01Z`: cos `/readyz` **503** `DB_UNAVAILABLE` (**BLOCKED**);
revops **NXDOMAIN** (**UNKNOWN**); `*.ggen.dev` aliases **NXDOMAIN**. Standings
**held**; no invented `ALIVE`; docs-only; no cargo; no ManagePullRequest.



## Retip20 (2026-11-22 11:19:24 PST)

cos **BLOCKED** (HTTP 503) · revops **UNKNOWN** · next: readyz 200.

## Retip21 (2026-11-22 12:36:00 PST) — Construct preempt + OOR reconfirm

Tip **`6ae116c31`**. Probe held: cos `/readyz` **503** `DB_UNAVAILABLE` (**BLOCKED**); `/healthz` **200** (**PARTIAL**); revops **NXDOMAIN** (**UNKNOWN**). In-repo Construct FM-WRITE-005 / FM-PACK-008/013 fixed @ `e568bef1d` (local validate-examples **55/55**); wasm-opt `--enable-bulk-memory` @ `6ae116c31` (local wasm-pack ALIVE; CI Manufacture **UNKNOWN** settling). Gate1 tip-cargo **CLOSED/ALIVE** held. Foundry `--live` **BLOCKED_HITL**. **No invented ALIVE.** Detail: [`OOR-RETIP21-CONSTRUCT-PREEMPT-STANDING.md`](./OOR-RETIP21-CONSTRUCT-PREEMPT-STANDING.md).

## Retip22 (2026-11-22 1:25:00 PST) — G2-11 pin consume + Construct settle

Tip **`08941960b`**. G2-11 prod runtime **PENDING_CI** (pin fixture + workflow on PR #950; local court **11**/EXIT:**0** @ `2f826872b`) — not production `ALIVE` until CI EXIT **0**. Construct/polyglot re-settling after TCPS refresh + cyberpunk paren. Foundry `--live` re-probe **foundry_exit=1** → **BLOCKED_HITL**. cos **503** / revops **NXDOMAIN** held. **No invented Foundry ALIVE.** Detail: [`VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md`](./VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md).

## Retip23 (2026-11-22 12:35:00 PST) — Construct receipt-exclude after FM-PACK-013 unmask

Tip **`6a6957113`** + receipt-exclude land. Emit-before-sync closed FM-PACK-013; Construct check-mode then false-positived on tracked `**/.ggen-v2/**` receipt/lease churn (~92 paths) — same class as publish-candidate `DRIFT_PATHSPEC`. `scripts/validate-examples.sh` now excludes that churn; source projections still fail closed. G2-11 Mount pin Step C still **PENDING_CI**. Foundry **BLOCKED_HITL** / cos **503** / revops **NXDOMAIN** held. **No invented ALIVE.** Detail: [`CONSTRUCT-RECEIPT-EXCLUDE-STANDING.md`](./CONSTRUCT-RECEIPT-EXCLUDE-STANDING.md).

## Retip24 (2026-11-22 2:10:00 PM PST) — Foundry/cos/revops reconfirm + Construct local pathspec

Tip **`c0be492e1`**. Foundry `--live` **foundry_exit=1** / `REFUSED` / provider **ABSENT** → **BLOCKED_HITL**. cos `/readyz` **503** `DB_UNAVAILABLE` (**BLOCKED**); `/healthz` **200** (**PARTIAL**); revops **NXDOMAIN** (**UNKNOWN**). Construct local double-sync + DRIFT_PATHSPEC EXIT **0** (**ALIVE** local ≠ CI). G2-11 Mount Step C still **in_progress** (**PENDING_CI**). Path crown **PARTIAL**. **No invented Foundry / zero-HITL / path ALIVE.** Detail: [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §7.

## Retip25 (2026-11-22 2:15:00 PM PST) — G2-11 Mount pin ALIVE

Tip **`1aa20cdce`**. G2-11 production-runtime ledger **ALIVE** — CI run [`45073805083`](https://github.com/seanchatmangpt/ggen/actions/runs/45073805083) EXIT **0** on `6a6957113` (Step A + Step C). API `PartialAlive` held. **≠ Foundry / path / zero-HITL `ALIVE`.** Foundry **BLOCKED_HITL** / cos **503** / revops **NXDOMAIN** / Construct CI **PENDING_CI** held. Detail: [`VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md`](./VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md).

## Retip26 (2026-11-22 2:39:00 PM PST) — Gate1 tip7 live-tip ALIVE + Construct local 55/55

Tip **`b07cda77c`**. Gate1 tip7 live-tip reconfirm **ALIVE**: shui **4**/EXIT:**0** + uilayout **11**/EXIT:**0** ([`GATE1-TIP7-LIVE-RECONFIRM-STANDING.md`](./GATE1-TIP7-LIVE-RECONFIRM-STANDING.md)). Local Construct **55/55 EXIT 0**. G2-11 Mount **ALIVE** held. CI Construct still **PENDING_CI** (Actions queue). Foundry **BLOCKED_HITL** / cos **503** / revops **NXDOMAIN** / path crown **PARTIAL** held. **No invented Foundry / zero-HITL / path ALIVE.**

## Retip27 (2026-11-22 3:11:00 PM PST) — interview-assist lock sync + Foundry re-probe held

Tip **`7bb443e99`**. CI Construct validate-examples **55/55 EXIT 0** on `b07cda77c` (run [`45078869386`](https://github.com/seanchatmangpt/ggen/actions/runs/45078869386)), then **failed** at Interview Assist `npm ci` (lock vs `package.json` next/vitest drift). Lock regenerated (`npm install --package-lock-only`); tip-wins `7bb443e99`. Full Construct job **PENDING_CI** (run [`45081862735`](https://github.com/seanchatmangpt/ggen/actions/runs/45081862735)). Foundry `--live` re-probe **foundry_exit=1** / `REFUSED` / provider **ABSENT** → **BLOCKED_HITL** (`/tmp/foundry-live-retip27.log`). cos `/readyz` **503** `DB_UNAVAILABLE` (**BLOCKED**); `/healthz` **200** (**PARTIAL**); revops **NXDOMAIN** (**UNKNOWN**). G2-11 Mount **ALIVE** / Gate1 tip7 **ALIVE** held. Path crown **PARTIAL**. **No invented Foundry / zero-HITL / path ALIVE.**

## Retip28 (2026-11-22 4:25:00 PM PST) — Construct full ALIVE + OOR held

Tip **`c19ad401d`** (subject Construct tip `43d6301bb` + tip-wins #956). Construct full job **ALIVE** — run [`45086131007`](https://github.com/seanchatmangpt/ggen/actions/runs/45086131007) EXIT **0** (byte-identical replay, lock refuse, IA ladder, workstation-path refuse). pytest tip-wins `43d6301bb`; IA portable `fileURLToPath` tip-wins. Foundry `--live` re-probe **foundry_exit=1** / provider **ABSENT** / no model API keys → **BLOCKED_HITL**. cos `/readyz` **503** `DB_UNAVAILABLE` (**BLOCKED**); `/healthz` **200** (**PARTIAL**); revops **NXDOMAIN** (**UNKNOWN**). Gate1 tip7 **ALIVE** / G2-11 Mount **ALIVE** held. Path crown **PARTIAL**. Zero-HITL fab **NOT COMPLETE**. **Construct ALIVE ≠ Foundry ALIVE.** Detail: [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md).

## Retip29 (2026-11-22 4:30:00 PM PST) — cos DB_UNAVAILABLE root-cause pin from fnox

Tip absorb of this land. Cloned `seanchatmangpt/fnox`; pinned cause of live `/readyz` **503** `DB_UNAVAILABLE` to `FnoxCos.Repo` `SELECT 1` failure (`cos/lib/fnox_cos_web/controllers/health_controller.ex`) — distinct from `MIGRATIONS_PENDING` / `DB_PRIVILEGE`. Agent **UNSUPPORTED** for GCP restore (no `gcloud` auth). Exact Founder next: restore Cloud SQL + `DATABASE_URL` → `/readyz` **200** `ready`. Foundry **BLOCKED_HITL** / Construct **ALIVE** held. Detail: [`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md).

## Retip30 (2026-11-22 4:35:00 PM PST) — revops.fnox.io host MISSING (fnox GTM)

Cloned `seanchatmangpt/revops` + re-read fnox GTM (`JOBS-LOCK-NOV21-RUNBOOK.md`): `revops.fnox.io` is **MISSING** (`infra/gcp/revops` absent); do **not** publish/CTA there; RevOps *function* host of record is **`cos.fnox.io` only**. Prior G2-1 **UNKNOWN**/“restore DNS” framing **superseded** → **UNSUPPORTED**. DNS empty reconfirmed (`dig +short` empty; cos/arcus/smalt resolve). Plane-5 ladder corrected: G2-2 cos readiness → AS-01 on cos (not invent revops DNS). Foundry **BLOCKED_HITL** held. Detail: [`REVOPS-HOST-MISSING-STANDING.md`](./REVOPS-HOST-MISSING-STANDING.md).

## Retip33 (2026-11-22 4:50:00 PM PST) — R-01…R-07 bodies admitted; R-06 CLOSED

Mounted admitted risk-register fixture from `zcode-cli` ecospec §4 @ `f32df08`. Bodies **ADMITTED** (not invented). **R-06 CLOSED** on commits `0cff19a`/`7b2c181` + chicago **11**/EXIT:**0**. R-01…R-05+R-07 remain **ADMITTED_OPEN**. Contract fence + ungrounded refuse held. Foundry / cos / WebAuthn **not** promoted. Detail: [`R01-R07-STANDING.md`](./R01-R07-STANDING.md); [`fixtures/r01-r07-sovereign-risk-register.md`](./fixtures/r01-r07-sovereign-risk-register.md).

## Retip34 (2026-11-22 5:15:00 PM PST) — Construct reconfirm ALIVE + R-02 PARTIAL + tip-wins #957

Construct full job **ALIVE** reconfirm — run [`45091416189`](https://github.com/seanchatmangpt/ggen/actions/runs/45091416189) EXIT **0** on subject `65c6a8034` (≠ Foundry). R-02 fixed-memory court **PARTIAL** @ repop `825751aa8` / [repop#1](https://github.com/seanchatmangpt/repop/pull/1) (**10+28**/EXIT:**0**; soak UNVERIFIED ≠ CLOSED). R-05 `#154` tip courts **16+6**/EXIT:**0** off-main (≠ main CLOSED). Tip-wins #957 honesty/R-body absorb onto crown after this pin. Foundry **BLOCKED_HITL** / cos **BLOCKED** / revops **UNSUPPORTED** / WebAuthn **UNSUPPORTED** / zero-HITL **NOT COMPLETE** held. Detail: [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md); [`R01-R07-STANDING.md`](./R01-R07-STANDING.md).

## Retip35 (2026-11-22 6:09:00 PM PST) — Construct tip ALIVE @ d6c04b390 + tip-wins #957

Construct tip-subject **ALIVE** — run [`45093580438`](https://github.com/seanchatmangpt/ggen/actions/runs/45093580438) EXIT **0** on tip `d6c04b390` (full ladder incl. Manufacture + IA + workstation refuse). Tip-wins #957: **R-02 CLOSED** (soak), R-03/R-04 OPEN honesty, Gate1 live-tip **45**/EXIT:**0**, VW **11**/EXIT:**0**, Foundry `--live` **foundry_exit=1** held. **≠ Foundry / zero-HITL / path crown ALIVE.** Detail: [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md).

## Retip32 (2026-11-22 4:45:00 PM PST) — OOR ledger honesty sync

Synced [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) to tip evidence: G2-1 **UNSUPPORTED** (MISSING); G2-2/G0-2 Founder AND-gate; G2-5 ladder drops invent-revops-DNS; G2-11 **ALIVE** (CI consume `45073805083`); G3-4 **BLOCKED**. Foundry **BLOCKED_HITL** / WebAuthn **UNSUPPORTED** / V8 **BLOCKED** held. Live re-probe: cos healthz **200** / readyz **503**; foundry_exit=1 model access.

## Retip31 (2026-11-22 4:40:00 PM PST) — cos operator AND-gate (false-DB_UNAVAILABLE)

Refined cos pin from fnox L10 + `OPERATOR-AND-GATE-readyz.md`: live `/readyz` **503** can be **false-`DB_UNAVAILABLE`** after **SELECT 1 OK** when CREATE `schema_migrations` runs under `fnox_cos_app`. On-disk fix `skip_table_creation: true` must be on live revision. Operator sequence **billing hold `018FD7…` → JWKS → cos-migrate → redeploy → readyz 200**. Agents must not clear billing. Foundry **BLOCKED_HITL** / revops host **UNSUPPORTED** held. Detail: [`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md).
