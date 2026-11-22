# Out-of-repo Gate 0 / 2 / 3 — next verifiers through GA 2027-01-01

| Field | Value |
|-------|-------|
| Document ID | `OUT-OF-REPO-NEXT-VERIFIERS` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `GATE-0-STANDING`, `GATE-3-RELEASE-COURTS-STANDING`, `FOUNDRY-FINISH-STANDING` |
| Target GA | **2027-01-01** |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a54b9803a113f111e5f622ef701e6e2b6b40bce2` |
| Materialized | 2026-11-22 |
| Repo scope | `seanchatmangpt/ggen` — **tracks** sibling standing; does not execute sibling DO |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** No row below is promoted to `ALIVE` from this repository. Connector
inventory, fleet census, README prose, and sibling tip SHAs are observation only
until the named verifier exits **0** (or a typed `REFUSED_*` is recorded with
owner). In-repo ggen fences are cited only where they bound an out-of-repo hop.

---

## Gate 0 — Nov 21 – Dec 5 (all OUT of ggen DO)

Detail: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G0-1 | **GCP billing** | **BLOCKED** | ggen DO out of scope; no live Marketplace Usage API / invoice pipeline in this worktree | External (infra/finance): GCP Billing / Marketplace console receipt for admitted billing account covering Gate 0 window → GA; weekly reconciliation exit 0 to customer GCS audit path per `docs/marketplace/billing-contract.md` | Unreconciled billing through **2027-01-01** falsifies Gate 0 close |
| G0-2 | **cos ledger fixes** | **BLOCKED** | Sibling `seanchatmangpt/fnox` / cos not mounted; no ledger replay from ggen | In **cos** / **fnox** checkout: ledger replay/verify court for admitted Gate 0 fix set → exit 0 + typed receipt or `REFUSED_*` (e.g. `just cos-ledger-verify` or repo-documented mix target) | Gate 0 cos row cannot become `ALIVE` from ggen; cos `/readyz` still **503** `DB_UNAVAILABLE` per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| G0-3 | **servengine M4** | **BLOCKED** | Sibling servengine not mounted; archive stubs only (`servengine-frontend` foreign path) | In **servengine** repo: M4 milestone court / CI gate exit 0 (exact runner per that repo) | M4 unproven at GA → hard Gate 0 risk alongside R-01…R-07 |
| G0-4 | **year-end SOW** | **UNKNOWN** | No commercial SOW register in ggen | Ops/commercial: signed year-end SOW artifact (parties, scope, period through **2027-01-01**) in admitted register; verify presence — do not author SOW body here (`REFUSED_UNGROUNDED`) | Missing SOW at GA remains **UNKNOWN** / carry-forward with owner |

---

## Gate 2 — post Gate 1 (out-of-repo subjects)

ECOSYSTEM-SPEC §3 Gate 2–3: revops plane, full brand manifests, CASTLE publish, release courts.
Photolithography / spine hops that require mounted siblings are included when execution is **out-of-repo**.

### 2.1 Plane 5 / economic ops / trust

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-1 | **revops.fnox.io** front door (D6) | **UNKNOWN** | DNS resolution failure observed 2026-11-22 (`curl` exit 6) | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200** on admitted ready path | If **2027-01-01** still no resolvable front door, D6 / spine falsifies |
| G2-2 | **cos.fnox.io** readiness + ledger | **BLOCKED** / **PARTIAL** | `/readyz` → **503** `DB_UNAVAILABLE`; `/ledger` HTTP **404**; sibling unmounted | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` (pass: **200** not `DB_UNAVAILABLE`); then fnox/cos ledger court in mounted checkout (see [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4) | CASTLE LinkedIn/Jobs AND-gate needs cos `/readyz` **200** ∧ 2× durable LOW — stays **BLOCKED** until G2-2 passes |
| G2-3 | **Live BRCE broker** (CONSTRUCT→DO) | **BLOCKED** | No broker runtime mounted; doctrine + intent manufacture only in ggen | Broker integration court: intent without verified AS-01 seat → refuse; with seat → execution receipt + replay (owner **BRCE** / trust plane) — see [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) V8 | AS-01 fence in ggen **PARTIAL** ≠ broker **ALIVE** at GA |
| G2-4 | **WebAuthn AS-01** (`:prime256v1` / ES256) verify | **UNSUPPORTED** | No `p256` / WebAuthn RP in ggen workspace members | Dedicated court: known-good assertion fixture → admit; tampered / wrong alg → `REFUSED_*` ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) V7) | Plane-5 DO without V7+V8 is **REFUSED** at GA |
| G2-5 | **Plane 5 DO path** (aggregate) | **BLOCKED** | ggen constitutional OUT OF SCOPE; no AS-01 runtime receipt | External host court: request without AS-01 → refuse; with AS-01 → BRCE receipt ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) V9; [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) CCS #8) | Crown Foundry-finish falsifies if founder still actuates Plane-5 without BRCE+AS-01 |
| G2-6 | **Smalt 4-cloud RAID** / economic ops | **UNKNOWN** + **BLOCKED** | `seanchatmangpt/smalt` not mounted; zero in-repo RAID surface | In mounted **smalt**: `just doctor`; `mix test test/aws`; `mix test test/gcp`; `mix test test/fin_ops_calculator_test.exs`; Azure/Twilio columns per parity receipts ([`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4) | GA fails if any column lacks HEAD-pinned green receipt in smalt |

### 2.2 Manufacture consumers (photolithography / UI / witness)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-7 | **Truex** admits W3C `shui:` shapes (D3 product) | **UNKNOWN** | Sibling `seanchatmangpt/truex` unmounted; ggen prefix fence **PARTIAL** only | In **truex**: admit-court loading a `shui:`-prefixed shape graph → exit 0; private `truex:` still refused | RC-D3 crown needs Truex **and** ggen fence green |
| G2-8 | **zcode-cli** `ui-layout-court.ts` (D5) | **UNSUPPORTED** | Not in ggen tree; adapter lives in sibling only | In **zcode-cli**: `pnpm test -- ui-layout-court` (or exact court runner) against ggen `AotSealReceipt` fixture — fail closed on missing/stale digest ([`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5) | Taffy **PARTIAL** without consume → photolithography non-ALIVE at GA |
| G2-9 | **zcode-cli** Chicago POWL/OCEL bounded remediation | **UNKNOWN** | Sibling not checked out here; connector inventory only **PARTIAL** | In mounted **zcode-cli**: `bun test test/powl-ocel-chicago.test.ts` ([`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) §5) | Unbounded remediation at GA → **REFUSED** per R-06 / ECOSYSTEM |
| G2-10 | **ggen_igniter** clean-room crown | **BLOCKED** | Checkout unmounted in cloud workspace | In **ggen_igniter**: `git diff --exit-code` → exit **0** ([`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) §4) | Dirty igniter at GA blocks photolithography hop 4 |
| G2-11 | **verify_witness** production WASM &lt;250KB | **UNSUPPORTED** | Production build on affidavit/witness plane; ggen has placeholder size court only | `rustup target add wasm32-unknown-unknown`; build release wasm32 artifact; `wc -c verify_witness.wasm` &lt; **262144**; optional hand-off via `GGEN_VERIFY_WITNESS_WASM` ([`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)) | Placeholder court green ≠ production **ALIVE** at GA |

### 2.3 CASTLE / brand / commercial (Gate 2–3)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-12 | **CASTLE publish** / RedactionCourt (spine) | **UNKNOWN** | `seanchatmangpt/castle` unmounted; no in-ggen publish surface | In **castle**: `cargo test --test publish_redaction_court`; `cargo test --test cli_publish_brand`; `castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §3.1) | README mint claims ≠ spine **ALIVE** without ggen digest bind |
| G2-13 | **Full DTCG brand inventory** (Fourth Watch / CindyCat) | **UNSUPPORTED** | Rhythm stubs in ggen only; color/type/CASTLE tokens Gate 2+ | Gate 2+ token inventory court from **admitted brand sources** (not invented in ggen); after admission: extend beyond `cargo test -p ggen-graph --test brand_toml_rhythm_court` | Inventing tokens in ggen → **REFUSED** (ECOSYSTEM non-goal) |
| G2-14 | **CASTLE CindyCat public mint** (commercial) | **UNKNOWN** | No mint receipt | `castle publish` with `--brand` CindyCat + RedactionCourt per [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md); depends G2-2 AND-gate for outbound send | Commercial mint **UNKNOWN** until castle + cos ready |
| G2-15 | **Event #1** patronage (CindyCat-first) | **UNKNOWN** | No external acknowledgment in ggen | Written acknowledgment: 0% custody, named third-party payer, amount &gt;$250 — ops/commercial register ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | Deal crown stays **UNKNOWN** at GA without external receipt |
| G2-16 | **LinkedIn/Jobs automated send** | **BLOCKED** | stop-worked: cos `/readyz` + 2× durable LOW not observed | Clear G2-2; then CASTLE plane AND-gate court per castle publish README | Send without gate → GA defect |

---

## Gate 3 — seven release courts (out-of-repo legs only)

Full table: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md).
Rows below are the **sibling / Plane-5** legs; in-repo PARTIAL courts (D2/D3 fence/D4 rhythm) are tracked in Gate-1 matrix, not duplicated here.

| ID | Court | Delta | Standing (out-of-repo leg) | Blocker | Next exact verifier | GA note |
|----|-------|-------|----------------------------|---------|---------------------|---------|
| G3-1 | **RC-D1** | Zero-DOM WebGPU + FFB (D1) | **UNSUPPORTED** | No UI-plane repo mounted in ggen | Sibling UI-plane court that paints from admitted Taffy AOT receipt → exit 0; refuse ambient DOM DO without BRCE+AS-01 | D1 required for GA per ECOSYSTEM §2 |
| G3-2 | **RC-D3** | Truex `shui:` product | **UNKNOWN** | Same as G2-7 | Same as G2-7 | RC-D3 **ALIVE** needs Truex court + ggen fence |
| G3-3 | **RC-D5** | zcode `ui-layout-court.ts` | **UNSUPPORTED** | Same as G2-8 | Same as G2-8 | RC-D5 ties to photolithography crown |
| G3-4 | **RC-D6** | Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE | Same as G2-1, G2-2, G2-5 | External Plane-5 / hosting court with AS-01-gated DO receipt; [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) probes | Never promote from ggen evidence alone |
| G3-5 | **RC-D7** | Risks **R-01 … R-07** (D7) | **BLOCKED** (all seven bodies) | Sovereign risk-register bodies not mounted | Per risk: typed receipt **or** typed `REFUSED_*` after register admits bodies — do not invent prose ([`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2) | Hard GA gate; silent closure → **REFUSED_UNGROUNDED** |

**RC-D2 / RC-D4 in-repo legs** (Taffy seal, brand rhythm): reconfirm on tip via Gate-1 commands; **consumer / inventory** legs remain G2-8, G2-13 above for GA **ALIVE**.

---

## Aggregate replay (this ledger — ggen only)

```text
test -f docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md
rg -n '^\| G[023]-' docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md | wc -l
# Expect ≥ 20 tracked out-of-repo rows; zero rows marked ALIVE without sibling exit 0 cited elsewhere
```

Sibling verifiers in §§Gate 0–3 must run in owning checkouts; paste exit codes into
the companion standing doc for that subject before refreshing standings here.

---

## Cross-links

| Doc | Role |
|-----|------|
| [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Horizontal spine HITL summary |
| [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) | Gate 0 stub citations |
| [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) | Commercial priority |
| [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | D1–D7 release courts |
| [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) | In-repo Gate-1 verifiers (not rewritten here) |

---

## Provenance

Materialized on branch `cursor/v2711-gate023-next-verifiers-bc54` from ecosystem
tip `a54b9803`. Standings synthesized from existing v27.1.1 standing ledgers only.
No sibling court was executed in this workspace pass. No out-of-repo row is asserted
`ALIVE`.