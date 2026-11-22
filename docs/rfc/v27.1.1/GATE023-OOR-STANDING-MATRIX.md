# Gate 0 / 2 / 3 — OOR standing honesty matrix (through GA 2027-01-01)

| Field | Value |
|-------|-------|
| Document ID | `GATE023-OOR-STANDING-MATRIX-27.1.1` |
| Binding | `ECOSYSTEM-SPEC-27.1.1` Gate 0 + Gate 2–3 out-of-repo legs; GA **2027-01-01** |
| Companion index | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md) |
| Sidebranch | `cursor/v2711-oor-retip19-bc54` (`bc-f83f4cbf`; matrix origin `cursor/v2711-gate023-oor-matrix-bc54` / `bc-0754e974`) |
| Observed ggen tip (write base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`dd61fefb29fa7ea6e1cf789f002ccd9879393988`** (`dd61fefb2`; tip-wins absorb parent; Foundry `--live` BLOCKED_HITL / cos BLOCKED / revops UNKNOWN **held**) |
| Probe time | 2026-11-22 11:02:01 PST / `2026-11-22T19:02:01Z` (retip19) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty matrix) |
| Crown absorb | tip-wins onto `origin/cursor/ecosystem-spec-27-1-1-bc54` (no force; no ManagePullRequest) |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** No row is promoted to `ALIVE` from this matrix. Connector inventory,
sibling tip SHAs, fixture/fence tip-cargo, and docs presence are observation only
until the named next verifier exits **0** (or a typed `REFUSED_*` is filed with
owner). Dual facets stay explicit (fixture **PARTIAL** ≠ live **UNSUPPORTED**).
Primary standing is the GA-facing subject of the next verifier. **Founder next**
for hop-5 / Foundry live remains `zcode /login` + `bun scripts/powl-foundry-loop.ts --live`
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
| `getent hosts revops.fnox.io` / `host` | exit **2** / **NXDOMAIN** | **UNKNOWN** |
| `curl … https://revops.fnox.io/` (and `/readyz`) | exit **6** / `http=000` | **UNKNOWN** |
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
| **ALIVE** | **5** | G2-6, G2-7, G2-10, G2-12, G3-2 |
| **PARTIAL** / **PARTIAL_ALIVE** | **4** | G2-8, G2-9, G2-11, G3-3 |
| **BLOCKED** | **8** | G0-1, G0-2, G0-3, G2-2, G2-3, G2-5, G2-16, G3-5 |
| **UNSUPPORTED** | **3** | G2-4, G2-13, G3-1 |
| **UNKNOWN** | **5** | G0-4, G2-1, G2-14, G2-15, G3-4 |
| **Total tracked** | **25** | G0-1…4 + G2-1…16 + G3-1…5 |

Named companion crowns (not extra G-rows): Foundry zero-HITL **PARTIAL**; foundry `--live` **BLOCKED_HITL**; BRCE **V8** **BLOCKED**; R-01…R-07 bodies **BLOCKED**; live WebAuthn **UNSUPPORTED**; CASTLE wire **ALIVE**/`WIRED` (≡ G2-12 facet); Smalt RAID **ALIVE** (≡ G2-6).

---

## Named OOR crown subjects (push-first honesty)

| Subject | Standing | Evidence tip | Exact next verifier | Owner |
|---------|----------|--------------|---------------------|-------|
| **Foundry `--live`** / hop-5 (G2-9) | **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`; `Model access is not configured` @ zcode `f32df08`) — Foundry crown **PARTIAL** ≠ `--live` `ALIVE` | tip `4060f9d6b`; pins [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10; [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a/§9; [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) | `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** | **Founder HITL** (+ zcode-cli surface) |
| **Zero-HITL Foundry crown** | **PARTIAL** — **not** Foundry `ALIVE` | tip `4060f9d6b`; [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Close HITL residuals: cos DB + WebAuthn live + `--live` → **0** before any crown promote | **Founder HITL** (residuals) / **Agent** (replay fences only) |
| **cos** readiness + ledger (G0-2 / G2-2) | **BLOCKED** (`/readyz` **503** `DB_UNAVAILABLE`); `/healthz` **PARTIAL** ≠ ALIVE | tip `dd61fefb2`; probe 11:02 PST retip19; [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §22 held | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** (not `DB_UNAVAILABLE`); then mounted fnox `just cos-ledger-verify` (or named CI ledger replay) → exit **0** | **Founder HITL** (DB) / **Agent** (probe + ledger replay once mounted) |
| **revops.fnox.io** front door (G2-1 / D6) | **UNKNOWN** (**NXDOMAIN**; curl exit **6**) | tip `dd61fefb2`; probe 11:02 PST retip19; [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §22 | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200** | **Founder HITL** (DNS/hosting) / **Agent** (probe) |
| **BRCE V8** broker (G2-3) | **BLOCKED** (broker unmounted; V1/V7 fences ≠ V8) | tip `4060f9d6b`; [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3; [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 | After live WebAuthn §3 exit **0**: in mounted BRCE broker — intent **without** AS-01 → `REFUSED_*`; **with** verified seat → execution receipt + replay digest → exit **0** | **Founder HITL** (mount + seat) / **Agent** (court once mounted) |
| **R-01…R-07 bodies** (G3-5 / RC-D7) | **BLOCKED** (all seven; contract fence **ALIVE** **7** @ `e7569893a` ≠ bodies) | tip `4060f9d6b`; [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Admit sovereign risk-register → per-ID typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract-only antifalsifier: `cargo test -p ggen-graph --test r01_r07_resolution_court` | **Founder HITL** (register admit) |
| **WebAuthn AS-01** live/production (G2-4) | **UNSUPPORTED** (live); V7 fixture **PARTIAL_ALIVE** / land **ALIVE** @ `fb6e7fc6e` ≠ live | tip `4060f9d6b`; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered/missing RP-ID/alg≠ES256 → `REFUSED_*`. Then **V8** → **V9**. Non-substitute: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` | **Founder HITL** |
| **zcode-cli** `/login` + POWL foundry live | **BLOCKED_HITL** (same falsifier as Foundry `--live`); adapter/path **PARTIAL**; runtime **PRESENT** ≠ live | tip `4060f9d6b`; zcode `f32df08`; [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §8/§10 | `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**. Replay (non-substitute): `bun test test/powl-ocel-chicago.test.ts` → **0** | **Founder HITL** |
| **Gate 0 billing** (G0-1 GCP) | **BLOCKED** (OUT OF SCOPE for ggen DO; contract prose ≠ reconciliation) | tip `4060f9d6b`; [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) §3.1 | Owner artifact: `docs/marketplace/billing-contract.md` DoD + Weekly Reconciliation. Console: `https://console.cloud.google.com/billing` → admitted account Gate 0→GA. Weekly: reconciliation → exit **0** / `✓ RECONCILED` + GCS audit export. ggen must not claim `ALIVE` | **Founder HITL** (infra/finance) |
| **Gate 3 release courts** (RC-D1…D7 aggregate) | Mixed — see Gate 3 table; crown **not** `ALIVE` while any court open | tip `4060f9d6b`; [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | Per-court next verifiers in Gate 3 section below; GA **2027-01-01** refuses silent close | **Founder HITL** (RC-D1/D6/D7) / **Agent** (RC-D2/D3/D4/D5 replay) |

---

## Gate 0 — Nov 21 – Dec 5 (OUT of ggen DO)

Detail: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G0-1** | GCP billing | **BLOCKED** | tip `4060f9d6b`; billing-contract prose **PRESENT** ≠ live | Weekly reconciliation in `docs/marketplace/billing-contract.md` + GCP Billing console receipt → exit **0** / `✓ RECONCILED` | **Founder HITL** |
| **G0-2** | cos ledger fixes | **BLOCKED** | tip `dd61fefb2`; `/readyz` **503** (retip19) | `curl … https://cos.fnox.io/readyz` → **200**; then fnox ledger court → exit **0** | **Founder HITL** / **Agent** (probe) |
| **G0-3** | servengine M4 | **BLOCKED** | tip `4060f9d6b`; sibling ABSENT | In mounted servengine: CI/milestone job labeled **M4** → exit **0** + typed receipt | **Founder HITL** / **Agent** (once mounted) |
| **G0-4** | year-end SOW | **UNKNOWN** | tip `4060f9d6b`; no register in ggen | Ops register: signed instrument parties+scope+period through **2027-01-01** | **Founder HITL** |

---

## Gate 2 — out-of-repo subjects

### Plane 5 / BRCE / WebAuthn / Smalt

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G2-1** | revops.fnox.io | **UNKNOWN** | tip `dd61fefb2`; NXDOMAIN / curl exit **6** (retip19) | `getent hosts revops.fnox.io`; `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** | **Founder HITL** / **Agent** (probe) |
| **G2-2** | cos.fnox.io readiness + ledger | **BLOCKED** (readiness); healthz **PARTIAL** | tip `dd61fefb2`; `/readyz` **503** (retip19) | `curl … https://cos.fnox.io/readyz` → **200**; then ledger court | **Founder HITL** / **Agent** (probe) |
| **G2-3** | BRCE human seat broker (**V8**) | **BLOCKED** | tip `4060f9d6b`; [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) | Live WebAuthn first → mounted broker refuse/admit court → exit **0** | **Founder HITL** |
| **G2-4** | WebAuthn AS-01 production | **UNSUPPORTED** (live); V7 **PARTIAL** ≠ live | tip `4060f9d6b`; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) | Founder HITL RP + hardware ceremony → exit **0**; then **V8** | **Founder HITL** |
| **G2-5** | Plane 5 DO path (aggregate) | **BLOCKED** | tip `4060f9d6b` | Ladder: G2-1 DNS∧**200** → G2-2 **200** → AS-01 host court (BRCE **V9**) | **Founder HITL** |
| **G2-6** | Smalt 4-cloud RAID / FinOps | **ALIVE** — ≠ Foundry / zero-HITL | tip-wins lineage; smalt `875efbb87` + `91997da76f0f`; [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | Replay FinOps + 4-cloud + residual §4.3 → exits **0** | **Agent** (replay) |

### Manufacture / CASTLE / commercial

| ID | Subject | Standing | Evidence tip | Exact next verifier | Owner |
|----|---------|----------|--------------|---------------------|-------|
| **G2-7** | Truex admits `shui:` | **ALIVE** (admit-court); fence exact-tip **PARTIAL** | `truex@68d2d233` merged; tip lineage through `1935ed718` | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → **0** | **Agent** (replay) |
| **G2-8** | zcode `ui-layout-court` / adapter | **PARTIAL** | tip `4060f9d6b`; adapter exit **0**; [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → **0** (G2-9) | **Founder HITL** (live) / **Agent** (adapter replay) |
| **G2-9** | zcode foundry `--live` / HITL | **PARTIAL** (path); live **BLOCKED_HITL** | tip `4060f9d6b`; **foundry_exit=1** | `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** | **Founder HITL** |
| **G2-10** | ggen_igniter clean-room / manufacture | **ALIVE** (clean-room + BrandTheme); path crown **PARTIAL** | tip lineage; [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd …/ggen_igniter && git diff --exit-code` → **0**; ≠ `--live` Foundry | **Agent** (replay) |
| **G2-11** | verify_witness production WASM | **PARTIAL** (env + affidavit **ALIVE**); prod runtime **UNSUPPORTED** | tip `4060f9d6b`; [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) | CI-mount real wasm + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0** | **Founder HITL** (CI mount) / **Agent** (court) |
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
| **G3-4** | **RC-D6** Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE from ggen | tip `dd61fefb2`; revops NXDOMAIN; cos **503** (retip19) | Same ladder as G2-1 + G2-2 + G2-5 | **Founder HITL** |
| **G3-5** | **RC-D7** R-01…R-07 bodies | **BLOCKED** (all seven) | tip `4060f9d6b`; [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Admit sovereign risk-register → per-ID receipt/`REFUSED_*` before **2027-01-01** | **Founder HITL** |

**RC-D2 / RC-D4** in-repo legs (Taffy seal, brand rhythm): tracked in Gate-1 matrices — not duplicated as OOR DO; consumer / inventory legs remain G2-8 / G2-13 for GA `ALIVE`.

---

## Top 3 next verifiers (founder-facing)

| Rank | Subject | Standing | Exact command |
|------|---------|----------|---------------|
| **1** | Foundry `--live` / zcode `/login` | **BLOCKED_HITL** | `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| **2** | cos `/readyz` (G0-2 / G2-2) | **BLOCKED** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`) |
| **3** | WebAuthn AS-01 live → BRCE **V8** | **UNSUPPORTED** → **BLOCKED** | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 ceremony → exit **0**; then [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3 broker court → exit **0** |

Honorable mention (DNS): `getent hosts revops.fnox.io` ∧ `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** (G2-1 **UNKNOWN**).

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
