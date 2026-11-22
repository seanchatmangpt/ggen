# Out-of-repo Gate 0 / 2 / 3 — next verifiers through GA 2027-01-01

| Field | Value |
|-------|-------|
| Document ID | `OUT-OF-REPO-NEXT-VERIFIERS` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `GATE-0-STANDING`, `GATE-3-RELEASE-COURTS-STANDING`, `FOUNDRY-FINISH-STANDING` |
| Target GA | **2027-01-01** |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `38b27737e37e85d7deef3df86737487c1fcca62b` |
| Materialized | 2026-11-22 |
| Hardened | 2026-11-22 — `cursor/v2711-oor-ledger-harden-bc54` |
| Ops refresh | 2026-11-22 — `cursor/v2711-ops-standing-refresh-bc54` (tip pin + probe reconfirm; standings held) |
| Tip pin | 2026-11-22 — `cursor/oor-tip-pin-f857` after dalek pin tip `38b27737e` (standings held; no capability promotion) |
| Repo scope | `seanchatmangpt/ggen` — **tracks** sibling standing; does not execute sibling DO |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** No row below is promoted to `ALIVE` from this repository. Connector
inventory, fleet census, README prose, and sibling tip SHAs are observation only
until the named verifier exits **0** (or a typed `REFUSED_*` is recorded with
owner). In-repo ggen fences are cited only where they bound an out-of-repo hop.
Every row MUST carry standing **and** an exact next verifier (command, URL, or
named owner artifact) — not vague “run the court” prose.

### Named crown cross-links (through GA **2027-01-01**)

| Named tip claim | Ledger IDs | Owner standing doc |
|-----------------|------------|--------------------|
| **Gate 0 billing** | G0-1 (+ G0-2…G0-4 Gate 0 set) | [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) |
| **Gate 2 BRCE human seat / WebAuthn production** | G2-3, G2-4, G2-5 | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 V7–V9 |
| **Gate 3 release courts** | G3-1…G3-5 | [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) |
| **GA date 2027-01-01** | all `GA note` cells + Foundry falsifiers | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |

---

## Gate 0 — Nov 21 – Dec 5 (all OUT of ggen DO)

Detail: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G0-1 | **GCP billing** (Gate 0 billing) | **BLOCKED** | ggen DO out of scope; no live Marketplace Usage API / invoice pipeline in this worktree | Owner artifact: `docs/marketplace/billing-contract.md` (DoD + Weekly Reconciliation). Console: `https://console.cloud.google.com/billing` → admitted account covering Gate 0 window→GA. Weekly: run the reconciliation block in that contract (`gcloud firestore documents list` + Marketplace usage count → exit 0 / `✓ RECONCILED`) and export receipt to the customer GCS audit path named there. ggen must not claim this row `ALIVE`. | Unreconciled billing through **2027-01-01** falsifies Gate 0 close |
| G0-2 | **cos ledger fixes** | **BLOCKED** | Sibling `seanchatmangpt/fnox` / cos not mounted; no ledger replay from ggen | (1) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`). (2) In mounted `seanchatmangpt/fnox`: owner artifact [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 2 — run that repo’s documented ledger replay/verify target (`just cos-ledger-verify` when present in fnox Justfile; else the CLAUDE.md/CI job named for cos ledger replay) → exit 0 + typed receipt or `REFUSED_*`. | Gate 0 cos row cannot become `ALIVE` from ggen; cos `/readyz` still **503** `DB_UNAVAILABLE` per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| G0-3 | **servengine M4** | **BLOCKED** | Sibling servengine not mounted; archive stubs only (`servengine-frontend` foreign path) | Owner artifact: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) §3.3. In mounted **servengine** checkout: run the CI/milestone job the repo labels **M4** (`.github/workflows/*` or Justfile/`mix`/`cargo` target whose name/description binds M4) → exit 0 + typed receipt. **Refuse** `examples/archive/factory-paas/verify_mermaid.mjs` and `/Users/sac/srve/servengine-frontend` as verifiers. | M4 unproven at GA → hard Gate 0 risk alongside R-01…R-07 |
| G0-4 | **year-end SOW** | **UNKNOWN** | No commercial SOW register in ggen | Owner artifact: ops/commercial year-end SOW register (external custody). Verify: signed instrument present with parties + scope + period through **2027-01-01** (`test -f <admitted-register-path>` on the ops host, or register UI export digest filed outside ggen). Do not author SOW body here (`REFUSED_UNGROUNDED`). | Missing SOW at GA remains **UNKNOWN** / carry-forward with owner |

---

## Gate 2 — post Gate 1 (out-of-repo subjects)

ECOSYSTEM-SPEC §3 Gate 2–3: revops plane, full brand manifests, CASTLE publish, release courts.
Photolithography / spine hops that require mounted siblings are included when execution is **out-of-repo**.

### 2.1 Plane 5 / economic ops / trust (incl. BRCE human seat / WebAuthn production)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-1 | **revops.fnox.io** front door (D6) | **UNKNOWN** | DNS resolution failure observed 2026-11-22 (`curl` exit 6) | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200** on admitted ready path | If **2027-01-01** still no resolvable front door, D6 / spine falsifies |
| G2-2 | **cos.fnox.io** readiness + ledger | **BLOCKED** / **PARTIAL** | `/readyz` → **503** `DB_UNAVAILABLE`; `/ledger` HTTP **404**; sibling unmounted | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` (pass: **200** not `DB_UNAVAILABLE`); then fnox/cos ledger court in mounted checkout ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4) | CASTLE LinkedIn/Jobs AND-gate needs cos `/readyz` **200** ∧ 2× durable LOW — stays **BLOCKED** until G2-2 passes |
| G2-3 | **BRCE human seat broker** (live CONSTRUCT→DO) | **BLOCKED** | No broker runtime mounted; doctrine + intent manufacture only in ggen | Owner artifact: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**. In mounted BRCE broker checkout: integration court — intent **without** verified AS-01 human seat → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit 0. (In-repo fence only: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` — does **not** promote this row.) | AS-01 fence in ggen **PARTIAL** ≠ broker **ALIVE** at GA |
| G2-4 | **WebAuthn AS-01 production** (human seat `:prime256v1` / ES256) | **UNSUPPORTED** | No `p256` / WebAuthn RP in ggen workspace members | Owner artifact: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V7**. In BRCE/trust-plane (or future ggen adapter) checkout: dedicated court — (1) known-good ES256/`prime256v1` assertion fixture → admit; (2) tampered assertion → `REFUSED_*`; (3) alg ≠ ES256 → refuse → exit 0. Companion fence: `rg -n 'p256\|webauthn\|web-authn\|passkeys' Cargo.toml crates/*/Cargo.toml Cargo.lock` → empty or documented admit (V5). | Plane-5 DO without V7+V8 is **REFUSED** at GA |
| G2-5 | **Plane 5 DO path** (aggregate; AS-01-gated) | **BLOCKED** | ggen constitutional OUT OF SCOPE; no AS-01 runtime receipt | Owner artifacts: BRCE **V9** + CCS #8 ([`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md)). External host court: request **without** AS-01 → refuse; **with** AS-01 → BRCE receipt. Probe ladder: G2-1 + G2-2 curls green, then AS-01 session + BRCE intent for the named front-door change ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6). | Crown Foundry-finish falsifies if founder still actuates Plane-5 without BRCE+AS-01 |
| G2-6 | **Smalt 4-cloud RAID** / economic ops | **UNKNOWN** + **BLOCKED** | `seanchatmangpt/smalt` not mounted; zero in-repo RAID surface | In mounted **smalt**: `just doctor`; `mix test test/aws`; `mix test test/gcp`; `mix test test/fin_ops_calculator_test.exs`; Azure/Twilio columns per parity receipts ([`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4) | GA fails if any column lacks HEAD-pinned green receipt in smalt |

### 2.2 Manufacture consumers (photolithography / UI / witness)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-7 | **Truex** admits W3C `shui:` shapes (D3 product) | **UNKNOWN** | Sibling `seanchatmangpt/truex` unmounted; ggen prefix fence **PARTIAL** only | In **truex**: admit-court loading a `shui:`-prefixed shape graph → exit 0; private `truex:` still refused. ggen non-substitute: `cargo test -p ggen-graph --test shui_public_prefix_court` | RC-D3 crown needs Truex **and** ggen fence green |
| G2-8 | **zcode-cli** `ui-layout-court.ts` (D5) | **UNSUPPORTED** | Not in ggen tree; adapter lives in sibling only | In **zcode-cli**: `pnpm test -- ui-layout-court` (or exact court runner) against ggen `AotSealReceipt` fixture — fail closed on missing/stale digest ([`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5) | Taffy **PARTIAL** without consume → photolithography non-ALIVE at GA |
| G2-9 | **zcode-cli** Chicago POWL/OCEL bounded remediation | **UNKNOWN** | Sibling not checked out here; connector inventory only **PARTIAL** | In mounted **zcode-cli**: `bun test test/powl-ocel-chicago.test.ts` ([`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) §5) | Unbounded remediation at GA → **REFUSED** per R-06 / ECOSYSTEM |
| G2-10 | **ggen_igniter** clean-room crown | **BLOCKED** | Checkout unmounted in cloud workspace | In **ggen_igniter**: `git diff --exit-code` → exit **0** ([`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) §4) | Dirty igniter at GA blocks photolithography hop 4 |
| G2-11 | **verify_witness** production WASM &lt;250KB | **UNSUPPORTED** | Production build on affidavit/witness plane; ggen has placeholder size court only | Owner artifact: [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). In affidavit/witness checkout: `rustup target add wasm32-unknown-unknown`; `cargo build --release --target wasm32-unknown-unknown -p verify_witness`; `wc -c verify_witness.wasm` → value **&lt; 262144**; optional hand-off `GGEN_VERIFY_WITNESS_WASM=/abs/path/to/verify_witness.wasm` | Placeholder court green ≠ production **ALIVE** at GA |

### 2.3 CASTLE / brand / commercial (Gate 2–3)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-12 | **CASTLE publish** / RedactionCourt (spine) | **UNKNOWN** | `seanchatmangpt/castle` unmounted; no in-ggen publish surface | In **castle**: `cargo test --test publish_redaction_court`; `cargo test --test cli_publish_brand`; `castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §3.1) | README mint claims ≠ spine **ALIVE** without ggen digest bind |
| G2-13 | **Full DTCG brand inventory** (Fourth Watch / CindyCat) | **UNSUPPORTED** | Rhythm stubs in ggen only; color/type/CASTLE tokens Gate 2+ | Owner: brand plane + CASTLE. Admit OEM token sources (do not invent in ggen); then in **castle**/brand checkout: inventory court that loads admitted `brands/fourth-watch.brand.toml` + `brands/cindycat.brand.toml` full DTCG color/type/CASTLE sets → exit 0. ggen non-substitute: `cargo test -p ggen-graph --test brand_toml_rhythm_court` ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | Inventing tokens in ggen → **REFUSED** (ECOSYSTEM non-goal) |
| G2-14 | **CASTLE CindyCat public mint** (commercial) | **UNKNOWN** | No mint receipt | In **castle**: `castle publish post --subject cindycat-mint-probe --public-anonymize --brand brands/cindycat.brand.toml` + `cargo test --test publish_redaction_court` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md)); depends G2-2 AND-gate for outbound send | Commercial mint **UNKNOWN** until castle + cos ready |
| G2-15 | **Event #1** patronage (CindyCat-first) | **UNKNOWN** | No external acknowledgment in ggen | Owner artifact: ops/commercial register entry — written acknowledgment with **0% custody**, named third-party payer, amount **&gt;$250** ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) §3). Verify presence on ops host; do not fabricate receipt text here. | Deal crown stays **UNKNOWN** at GA without external receipt |
| G2-16 | **LinkedIn/Jobs automated send** | **BLOCKED** | stop-worked: cos `/readyz` + 2× durable LOW not observed | Clear G2-2 (`curl …/readyz` → 200); then on CASTLE plane: AND-gate court requiring cos readiness **and** 2× durable LOW receipts on disk before Jobs/LinkedIn send ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §4 item 3; castle publish README) | Send without gate → GA defect |

---

## Gate 3 — seven release courts (out-of-repo legs only)

Full table: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md).
Rows below are the **sibling / Plane-5** legs; in-repo PARTIAL courts (D2/D3 fence/D4 rhythm) are tracked in Gate-1 matrix, not duplicated here.
GA horizon for all courts: **2027-01-01**.

| ID | Court | Delta | Standing (out-of-repo leg) | Blocker | Next exact verifier | GA note |
|----|-------|-------|----------------------------|---------|---------------------|---------|
| G3-1 | **RC-D1** | Zero-DOM WebGPU + FFB (D1) | **UNSUPPORTED** | No UI-plane repo mounted in ggen | Owner artifacts: [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §5 Mode A/B; [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) RC-D1. In mounted UI-plane (Zero-DOM WebGPU / FFB) checkout: court that loads admitted ggen `AotSealReceipt` → paints Mode A canvas (or Mode B FFB) → exit 0; ambient DOM DO without BRCE+AS-01 → `REFUSED_*` | D1 required for GA per ECOSYSTEM §2 |
| G3-2 | **RC-D3** | Truex `shui:` product | **UNKNOWN** | Same as G2-7 | Same as G2-7 (`shui:` admit-court in truex + ggen `shui_public_prefix_court`) | RC-D3 **ALIVE** needs Truex court + ggen fence |
| G3-3 | **RC-D5** | zcode `ui-layout-court.ts` | **UNSUPPORTED** | Same as G2-8 | Same as G2-8 (`pnpm test -- ui-layout-court` in zcode-cli) | RC-D5 ties to photolithography crown |
| G3-4 | **RC-D6** | Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE | Same as G2-1, G2-2, G2-5 | Same probe ladder as G2-1 + G2-2 + G2-5 (DNS/`readyz` + AS-01-gated BRCE receipt); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §3–§4 | Never promote from ggen evidence alone |
| G3-5 | **RC-D7** | Risks **R-01 … R-07** (D7) | **BLOCKED** (all seven bodies) | Sovereign risk-register bodies not mounted | Owner artifact: [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2 crown matrix. Per ID R-01…R-07: admit body from sovereign risk register (no invention) → typed receipt **or** typed `REFUSED_*` before **2027-01-01**. In-repo contract fence only: `cargo test -p ggen-graph --test r01_r07_resolution_court` (does not close any body). | Hard GA gate; silent closure → **REFUSED_UNGROUNDED** |

**RC-D2 / RC-D4 in-repo legs** (Taffy seal, brand rhythm): reconfirm on tip via Gate-1 commands; **consumer / inventory** legs remain G2-8, G2-13 above for GA **ALIVE**.

---

## Aggregate replay (this ledger — ggen only)

```text
test -f docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md
rg -n '^\| G[023]-' docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md | wc -l
# Expect ≥ 25 tracked out-of-repo rows; zero rows marked ALIVE without sibling exit 0 cited elsewhere
rg -n 'Gate 0 billing|BRCE human seat|WebAuthn AS-01 production|2027-01-01' docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md
# Foundry-finish pointer must remain live:
rg -n 'OUT-OF-REPO-NEXT-VERIFIERS' docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md
```

Sibling verifiers in §§Gate 0–3 must run in owning checkouts; paste exit codes into
the companion standing doc for that subject before refreshing standings here.

---

## Cross-links

| Doc | Role |
|-----|------|
| [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Horizontal spine HITL summary; points here for out-of-repo next verifiers |
| [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) | Gate 0 billing / cos / servengine / SOW stub citations |
| [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | Gate 2 BRCE human seat + WebAuthn AS-01 production (V7–V9) |
| [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) | Commercial priority |
| [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | D1–D7 release courts through GA **2027-01-01** |
| [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) | In-repo Gate-1 verifiers (not rewritten here) |
| [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | RC-D7 body matrix (G3-5) |

---

## Provenance

Materialized on branch `cursor/v2711-gate023-next-verifiers-bc54` from ecosystem
tip `a54b9803`. Hardened on `cursor/v2711-oor-ledger-harden-bc54` against tip
`14a3183fd` — filled exact next-verifier command/URL/owner-artifact cells; named
Gate 0 billing, Gate 2 BRCE human seat / WebAuthn production, Gate 3 release
courts, and GA **2027-01-01**. Ops tip pin refreshed on
`cursor/v2711-ops-standing-refresh-bc54` against tip `7bf148d82` (HTTP + mount
reconfirm; standings held). Tip pin retipped on `cursor/oor-tip-pin-f857` to
crown tip `38b27737e` (post–curve25519-dalek Gate-1 pin standing; standings
held — no capability row promoted). Standings synthesized from existing v27.1.1
standing ledgers only. No sibling court was executed in this workspace pass. No
out-of-repo row is asserted `ALIVE`.
