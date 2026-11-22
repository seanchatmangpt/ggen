# Out-of-repo Gate 0 / 2 / 3 — next verifiers through GA 2027-01-01

| Field | Value |
|-------|-------|
| Document ID | `OUT-OF-REPO-NEXT-VERIFIERS` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `GATE-0-STANDING`, `GATE-3-RELEASE-COURTS-STANDING`, `FOUNDRY-FINISH-STANDING` |
| Target GA | **2027-01-01** |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5345d131e628e9f9a765f7f0be633ad2be6268da` |
| Materialized | 2026-11-22 |
| Hardened | 2026-11-22 — `cursor/v2711-oor-ledger-harden-bc54` |
| Ops refresh | 2026-11-22 — `cursor/v2711-ops-standing-refresh-bc54` (tip pin + probe reconfirm; standings held) |
| Tip pin | 2026-11-22 — `cursor/v2711-hop5-mix-subject-bc54` — hop-5 mix SUBJECT_MISSING fix (`resolveCleanRoomSyncCmd`) → default adapter exit **0** / Jobs/C4 deferred; hop-4 BrandTheme manufacture **ALIVE** (`diff_exit=0` dual-schema); Truex admit-court **ALIVE** @ `truex@68d2d233` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED**); Smalt FinOps **ALIVE** + G2-12 **ALIVE** retained; Foundry crown stays **PARTIAL_ALIVE**; hop-5 aggregate **PARTIAL_ALIVE** (foundry `--live` open); full RAID/`ALIVE` **not** invented |
| Hop-4 evidence | `/tmp/cursor/igniter-real-cleanroom.md` (`bc-4861c787`) — tip dual-schema BrandTheme manufacture compare `diff_exit=0` (**ALIVE**) |
| Hop-5 evidence | [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) (`adapter_exit=0` / `layout_exit=0`; Chicago **31/0**; foundry_exit=1 open; prior demote `/tmp/cursor/hop5-live-after-884.md`) |
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
| G0-1 | **GCP billing** (Gate 0 billing) | **BLOCKED** | ggen DO out of scope; no live Marketplace Usage API / invoice pipeline in this worktree; `docs/marketplace/billing-contract.md` **PRESENT** (artifact ≠ reconciliation) | Owner artifact: `docs/marketplace/billing-contract.md` (DoD + Weekly Reconciliation). Console: `https://console.cloud.google.com/billing` → admitted account covering Gate 0 window→GA. Weekly: run the reconciliation block in that contract (`gcloud firestore documents list` + Marketplace usage count → exit 0 / `✓ RECONCILED`) and export receipt to the customer GCS audit path named there. ggen must not claim this row `ALIVE`. | Unreconciled billing through **2027-01-01** falsifies Gate 0 close |
| G0-2 | **cos ledger fixes** | **BLOCKED** | Sibling `seanchatmangpt/fnox` / cos not mounted (ABSENT retip5); no ledger replay from ggen; `/readyz` reconfirm **503** `DB_UNAVAILABLE` | (1) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`). (2) In mounted `seanchatmangpt/fnox`: owner artifact [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 2 — run that repo’s documented ledger replay/verify target (`just cos-ledger-verify` when present in fnox Justfile; else the CLAUDE.md/CI job named for cos ledger replay) → exit 0 + typed receipt or `REFUSED_*`. | Gate 0 cos row cannot become `ALIVE` from ggen; cos `/readyz` still **503** `DB_UNAVAILABLE` per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| G0-3 | **servengine M4** | **BLOCKED** | Sibling servengine not mounted (ABSENT retip5 at `/agent/repos` + `/home/ubuntu`); archive stubs only (`servengine-frontend` foreign path) | Owner artifact: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) §3.3. In mounted **servengine** checkout: run the CI/milestone job the repo labels **M4** (`.github/workflows/*` or Justfile/`mix`/`cargo` target whose name/description binds M4) → exit 0 + typed receipt. **Refuse** `examples/archive/factory-paas/verify_mermaid.mjs` and `/Users/sac/srve/servengine-frontend` as verifiers. | M4 unproven at GA → hard Gate 0 risk alongside R-01…R-07 |
| G0-4 | **year-end SOW** | **UNKNOWN** | No commercial SOW register in ggen | Owner artifact: ops/commercial year-end SOW register (external custody). Verify: signed instrument present with parties + scope + period through **2027-01-01** (`test -f <admitted-register-path>` on the ops host, or register UI export digest filed outside ggen). Do not author SOW body here (`REFUSED_UNGROUNDED`). | Missing SOW at GA remains **UNKNOWN** / carry-forward with owner |

---

## Gate 2 — post Gate 1 (out-of-repo subjects)

ECOSYSTEM-SPEC §3 Gate 2–3: revops plane, full brand manifests, CASTLE publish, release courts.
Photolithography / spine hops that require mounted siblings are included when execution is **out-of-repo**.

### 2.1 Plane 5 / economic ops / trust (incl. BRCE human seat / WebAuthn production)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-1 | **revops.fnox.io** front door (D6) | **UNKNOWN** | DNS still fails on retip5: `getent` exit **2**; `host` **NXDOMAIN**; `curl` exit **6** (`http=000`) | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200** on admitted ready path | If **2027-01-01** still no resolvable front door, D6 / spine falsifies |
| G2-2 | **cos.fnox.io** readiness + ledger | **BLOCKED** / **PARTIAL** | retip5 reconfirm: `/readyz` → **503** `{"status":"DB_UNAVAILABLE"}`; `/ledger` HTTP **404**; fnox sibling **ABSENT** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` (pass: **200** not `DB_UNAVAILABLE`); then fnox/cos ledger court in mounted checkout ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4) | CASTLE LinkedIn/Jobs AND-gate needs cos `/readyz` **200** ∧ 2× durable LOW — stays **BLOCKED** until G2-2 passes |
| G2-3 | **BRCE human seat broker** (live CONSTRUCT→DO) | **BLOCKED** | No broker runtime mounted; doctrine + intent manufacture only in ggen | Owner artifact: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**. In mounted BRCE broker checkout: integration court — intent **without** verified AS-01 human seat → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit 0. (In-repo fence only: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` — does **not** promote this row.) | AS-01 fence in ggen **PARTIAL** ≠ broker **ALIVE** at GA |
| G2-4 | **WebAuthn AS-01 production** (human seat `:prime256v1` / ES256) | **PARTIAL_ALIVE** (in-repo V7 fixture crypto-shape) + production ceremony **UNSUPPORTED** | Full WebAuthn RP / hardware ceremony still absent; in-repo adapter court landed | Owner artifact: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V7**. In-repo replay: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` → exit 0 (known-good admit; tampered `REFUSED_*`; alg≠ES256 refuse). Production next (OOR): RP/hardware ceremony court in BRCE/trust-plane. Companion V5: `p256` admitted only in `ggen-abb-sbb` for V7 — `rg` must stay free of silent `webauthn`/`passkeys` RP deps. | Fixture PARTIAL ≠ Plane-5 DO; need V8+V9 before GA DO |
| G2-5 | **Plane 5 DO path** (aggregate; AS-01-gated) | **BLOCKED** | ggen constitutional OUT OF SCOPE; no AS-01 runtime receipt; crypto still **UNSUPPORTED** (G2-4) | Owner artifacts: BRCE **V9** + CCS #8 ([`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md)). Exact next probe ladder: (1) G2-1 `getent hosts revops.fnox.io` ∧ `curl -sS -w '
| G2-6 | **Smalt 4-cloud RAID** / economic ops | **PARTIAL_ALIVE** | Sibling tip `smalt@91997da76f0f` (parent `875efbb87` — tip includes both): doctor/aws/gcp/azure/twilio exits **0,0,0,0,0** @ `875efbb87` (`bc-ad9ebffa`); FinOps **ALIVE** @ `91997da76f0f` — `mix test test/fin_ops_calculator_test.exs` → exit **0**, **19 passed**. Not full RAID/`ALIVE` (remaining GA economic face / b4b open). ggen has zero in-repo RAID surface | Replay FinOps: `mix test test/fin_ops_calculator_test.exs` → exit **0** @ `91997da76f0f`. Replay 4-cloud: §4.1 commands at `875efbb87` (⊆ tip). Next: remaining GA economic face / b4b courts → exit **0**; cite HEAD + exit in [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | GA fails if any column or FinOps court lacks HEAD-pinned green receipt in smalt |

### 2.2 Manufacture consumers (photolithography / UI / witness)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-7 | **Truex** admits W3C `shui:` shapes (D3 product) | **ALIVE** (admit-court) | Sibling Chicago admit-court **6/6 vitest exit 0** reconfirmed @ `truex@68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main` merge `b4672131`; mount `/tmp/truex`; receipt `/tmp/cursor/truex-merged-admit-alive.md`): `shui:` **ADMITTED**; private `truex:` **REFUSED_PRIVATE**; unknown **REFUSED_UNKNOWN**. ggen prefix fence **ALIVE** on evidence ancestor `0478fde59` (tip source **4**; tip-cargo PENDING) — fence ≠ Truex product crown | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233`. Companion fence (non-substitute): `cargo test -p ggen-graph --test shui_public_prefix_court` | Admit-court **ALIVE** on merged tip; tip-sync ggen fence still valuable for RC-D3 aggregate |
| G2-8 | **zcode-cli** `ui-layout-court.ts` / `UiLayoutCourtAdapter` (D5) | **PARTIAL_ALIVE** — in-process **ALIVE** + live adapter **ALIVE** (exit **0**; mix-SUBJECT fix) | Sibling mounted; `ggen-layout-court` **PRESENT**; `layout_court` exit **0**; default `verifyPostTransition` exit **0** when layout_court PRESENT (mix ABSENT → Jobs/C4 deferred, not SUBJECT_MISSING). Foundry `--live` still open → not hop-5 crown ALIVE. Ledger: [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (G2-9). Replay: adapter verify → **0**; `bun test test/ui-layout-court-chicago.test.ts` → **0** | Claiming hop-5 crown ALIVE from adapter alone while foundry `--live` open; restating mix SUBJECT_MISSING after layout_court PRESENT + spatial PASS |
| G2-9 | **zcode-cli** Chicago POWL/OCEL + foundry loop | **PARTIAL** / live **BLOCKED**/**REFUSED** | Mounted; Gall `--live` @ zcode `f32df08` injects `createZcodeAgentExecutor`; **foundry_exit=1** / **REFUSED** (`ZCode runtime is missing`; darwin lock **UNSUPPORTED** on Linux); G2-8 adapter exit **0**; `mix` **ABSENT**. Ledger: [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md) | Exact next: `bun run sync:local` (or supported-platform `sync:locked`) + provider creds → `bun scripts/powl-foundry-loop.ts --live` → exit **0**; Jobs/C4: `mix ggen_igniter.sync --check` → **0**; chicago: `bun test test/powl-ocel-chicago.test.ts` → **0** | Unbounded remediation at GA → **REFUSED** per R-06 / ECOSYSTEM; inventing ALIVE from executor wire alone |
| G2-10 | **ggen_igniter** clean-room / manufacture | **ALIVE** — tip BrandTheme manufacture compare `git diff --exit-code` → **0** (dual-schema tip brand; `/tmp/cursor/igniter-real-cleanroom.md`) | Prior `:no_primitive_space` closed; bare post-clone `git diff --exit-code` ≠ `ALIVE` (antifalsifier); `mix`/NIF still **BLOCKED** | **Replay:** BrandTheme emit tip `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` + igniter fixture → `diff -rq` / `git diff --exit-code` → **0**. **Next residual:** NIF + `mix ggen_igniter.sync --check` → 0. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. Contract: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | Claiming hop-4 `ALIVE` from bare post-clone diff; inventing `mix sync` ALIVE while NIF BLOCKED |
| G2-11 | **verify_witness** production WASM &lt;250KB | **UNSUPPORTED** | Production build on affidavit/witness plane; ggen has placeholder size court only | Owner artifact: [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). In affidavit/witness checkout: `rustup target add wasm32-unknown-unknown`; `cargo build --release --target wasm32-unknown-unknown -p verify_witness`; `wc -c verify_witness.wasm` → value **&lt; 262144**; optional hand-off `GGEN_VERIFY_WITNESS_WASM=/abs/path/to/verify_witness.wasm` | Placeholder court green ≠ production **ALIVE** at GA |

### 2.3 CASTLE / brand / commercial (Gate 2–3)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-12 | **CASTLE publish** / RedactionCourt (spine) | **ALIVE** (sibling courts @ `2774039ea`) | Spine crown still lacks ggen layout/brand digest bind; LinkedIn send still **BLOCKED** on G2-2; in-ggen publish **UNSUPPORTED** | Replay in **castle** @ `2774039ea` (`bc-1bffb072`): `cargo test --test publish_redaction_court` → **13/13** exit **0**; `cargo test --test cli_publish_brand` → **6/6** exit **0**; `./target/debug/castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → exit **0** / `redaction_court=ALIVE`. Next crown: bind ggen digest into mint ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §3) | Sibling court **ALIVE** ≠ Foundry/zero-HITL **ALIVE**; ≠ LinkedIn send |
| G2-13 | **Full DTCG brand inventory** (Fourth Watch / CindyCat) | **UNSUPPORTED** | Rhythm stubs in ggen only; color/type/CASTLE tokens Gate 2+ | Owner: brand plane + CASTLE. Admit OEM token sources (do not invent in ggen); then in **castle**/brand checkout: inventory court that loads admitted `brands/fourth-watch.brand.toml` + `brands/cindycat.brand.toml` full DTCG color/type/CASTLE sets → exit 0. ggen non-substitute: `cargo test -p ggen-graph --test brand_toml_rhythm_court` ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | Inventing tokens in ggen → **REFUSED** (ECOSYSTEM non-goal) |
| G2-14 | **CASTLE CindyCat public mint** (commercial) | **UNKNOWN** | No mint receipt; castle sibling **ABSENT** this worktree (retip5) | In **castle**: `castle publish post --subject cindycat-mint-probe --public-anonymize --brand brands/cindycat.brand.toml` + `cargo test --test publish_redaction_court` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md)); depends G2-2 AND-gate for outbound send | Commercial mint **UNKNOWN** until castle + cos ready |
| G2-15 | **Event #1** patronage (CindyCat-first) | **UNKNOWN** | No external acknowledgment in ggen | Owner artifact: ops/commercial register entry — written acknowledgment with **0% custody**, named third-party payer, amount **&gt;$250** ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) §3). Verify presence on ops host; do not fabricate receipt text here. | Deal crown stays **UNKNOWN** at GA without external receipt |
| G2-16 | **LinkedIn/Jobs automated send** | **BLOCKED** | stop-worked: cos `/readyz` retip5 still **503** `DB_UNAVAILABLE` + 2× durable LOW not observed; castle **ABSENT** | Clear G2-2 (`curl …/readyz` → 200); then on CASTLE plane: AND-gate court requiring cos readiness **and** 2× durable LOW receipts on disk before Jobs/LinkedIn send ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §4 item 3; castle publish README) | Send without gate → GA defect |

---

## Gate 3 — seven release courts (out-of-repo legs only)

Full table: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md).
Rows below are the **sibling / Plane-5** legs; in-repo PARTIAL courts (D2/D3 fence/D4 rhythm) are tracked in Gate-1 matrix, not duplicated here.
GA horizon for all courts: **2027-01-01**.

| ID | Court | Delta | Standing (out-of-repo leg) | Blocker | Next exact verifier | GA note |
|----|-------|-------|----------------------------|---------|---------------------|---------|
| G3-1 | **RC-D1** | Zero-DOM WebGPU + FFB (D1) | **UNSUPPORTED** | No UI-plane repo mounted in ggen | Owner artifacts: [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §5 Mode A/B; [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) RC-D1. In mounted UI-plane (Zero-DOM WebGPU / FFB) checkout: court that loads admitted ggen `AotSealReceipt` → paints Mode A canvas (or Mode B FFB) → exit 0; ambient DOM DO without BRCE+AS-01 → `REFUSED_*` | D1 required for GA per ECOSYSTEM §2 |
| G3-2 | **RC-D3** | Truex `shui:` product | **ALIVE** (admit-court; same as G2-7) | Same as G2-7 (`truex@68d2d233` **MERGED**; vitest **6/6** exit 0) | Same as G2-7 replay + companion ggen `shui_public_prefix_court` (tip-cargo PENDING) | Admit-court **ALIVE** on merged tip; fence tip-sync still open |
| G3-3 | **RC-D5** | zcode `ui-layout-court.ts` / live adapter | **PARTIAL_ALIVE** (default adapter exit **0**; foundry_exit=1) | Same as G2-8 (`ggen-layout-court` PRESENT; adapter exit **0** / Jobs/C4 deferred; `resolveCleanRoomSyncCmd` path) | Exact next: `powl-foundry-loop --live` → 0 (G2-9) | RC-D5 ties to photolithography crown |
| G3-4 | **RC-D6** | Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE | Same as G2-1, G2-2, G2-5 (retip5: revops NXDOMAIN; cos **503**) | Same probe ladder as G2-1 + G2-2 + G2-5 (DNS/`readyz` + AS-01-gated BRCE receipt); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §3–§4 | Never promote from ggen evidence alone |
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
held — no capability row promoted). Tip pin retipped again on
`cursor/v2711-oor-retip-bc54` to live crown tip `6b66659c4` (post–Gate-1
reconfirm merge `43e8541c8` + `ggen-sjira` subject-mismatch court fix;
standings held — no capability row promoted; no false `ALIVE`). Tip pin
refreshed once more on the same branch to live crown tip `0ae8d8108` (post–
ui-layout ALIVE pin `eaf0dad40` exit 0 absorb commit; standings held — no
capability row promoted; no false `ALIVE`). Tip pin retipped on
`cursor/v2711-oor-retip3-bc54` to live crown tip `61311cb53` (post–CCS+BRCE
standing retip `61311cb53` / fmt gate `f1639510e`; standings held — no
capability row promoted; no false `ALIVE`). Tip pin retipped on
`cursor/v2711-gate1-retip-bc54` to live crown tip
`5e00619a422a0d8e451029f0aaf8b7a63e27da46` (post–ui-layout rustfmt tip;
standings held — no capability row promoted; no false `ALIVE`; tip-cargo
reconfirm PENDING quiet cgroup). Tip pin retipped on
`cursor/v2711-oor-retip2-bc54` to live crown tip `5e00619a4` (post–
ui-layout JSON fixture harness `6ab75e6a3` + rustfmt tip fmt gate
`5e00619a4`; standings held — no capability row promoted; no false
`ALIVE`). Tip pin retipped on `cursor/v2711-oor-retip3-bc54` to live
crown tip `4e6ca0e2e` (post–gate1-retip `c4dd9729c` + ui-layout
expect-free/ALIVE/tidy `30b30d70c`/`07b689c97`/`4e6ca0e2e`; full
standing retip content + newest Observed tip SHA; standings held — no
capability row promoted; no false `ALIVE`). Tip pin retipped on `cursor/v2711-hop5-standing-bc54` onto live crown tip `f6cb18f86` (brand hot-swap `f6cb18f86` + castle `32f2e316f` + CCS/revops/oor ancestors); hop-5 **PARTIAL** evidence retained (in-process ALIVE + live BLOCKED; evidence tip `07b689c97`); standings held — no capability row promoted; no false `ALIVE`). Tip pin retipped on `cursor/v2711-photo-hop4-partial-bc54` onto tip `376b5c309` (hop-4 demotion to **PARTIAL_ALIVE**; antifalsifier: bare post-clone `git diff --exit-code` ≠ `ALIVE`; hop-5 PARTIAL retained). Tip pin retipped on
`cursor/v2711-truex-admit-standing-bc54` onto crown tip `58a93a94f`
(Truex product admit G2-7 / Gate-1 row **PARTIAL_ALIVE** — sibling
admit-court **6/6 vitest exit 0** @ `truex@e28e68300` on
`cursor/v2711-truex-admit-court-bc54` / truex#6; `shui:` ADMITTED /
`truex:` REFUSED_PRIVATE / unknown REFUSED_UNKNOWN; next = merge truex
PR + tip-sync ggen fence parity; **not** full `ALIVE` until truex
`main`; hop-4 **PARTIAL_ALIVE** retained; no false product `ALIVE`).
Tip pin retipped on `cursor/v2711-castle-alive-bc54` onto tip `d0e3ba473`
— G2-12 CASTLE sibling spine courts **ALIVE** @ castle `2774039ea`
(`bc-1bffb072`: `publish_redaction_court` 13/13, `cli_publish_brand` 6/6,
spine-probe `redaction_court=ALIVE`, exits 0,0,0); Foundry/zero-HITL crown
**not** promoted; spine crown wire remains **UNKNOWN**. Tip pin retipped on
`cursor/v2711-oor-retip4-bc54` onto live crown tip `5480d7ada` (post–CASTLE
ALIVE land `5480d7ada`; prior Observed pin `d0e3ba473`; standings held —
G2-12 **ALIVE** retained; Foundry stays **PARTIAL_ALIVE**; no false
promotion). Tip pin retipped on `cursor/v2711-smalt-partial-bc54` onto tip
`8a68da481` — G2-6 Smalt RAID columns **ALIVE** + fin_ops **PARTIAL_ALIVE**
@ `smalt@875efbb87` (`bc-ad9ebffa`: doctor/aws/gcp/azure/twilio exits
0,0,0,0,0; retry `bc-1e876087`: burn-path FinOps exit 0; canonical
`test/fin_ops_calculator_test.exs` **ABSENT** — restore still next);
Foundry crown stays **PARTIAL_ALIVE**; full RAID/`ALIVE` **not** invented
while FinOps is non-canonical.
Tip pin retipped on `cursor/v2711-oor-retip5-bc54` onto live crown tip
`4b22529df` — non-cargo HTTP/mount/fence reconfirm; G2-8/G3-3 advanced
from `ggen-layout-court` **ABSENT** → source **PRESENT** (live still
**BLOCKED** pending built `layout_court`; no cargo thrash); in-process
bun **35/35** exit **0**; G2-1/G2-2/G0-2/G3-4 standings **held**
(NXDOMAIN / **503**); G2-6 crown **PARTIAL_ALIVE** + G2-12 **ALIVE**
retained; Foundry stays **PARTIAL_ALIVE**; no false full RAID/`ALIVE`.
Tip pin + exact next-verifier refresh on `cursor/v2711-ccs-brce-next-bc54`
onto live crown tip `750b8ef4a9042d22808a79be3dedff7944d4e321` — G2-4 WebAuthn crypto stays **UNSUPPORTED**
(`as01_assertion` **ABSENT**; named `brce_as01_es256_assertion_court` + V5 `rg`);
G2-5 Plane-5 DO stays **BLOCKED** (G2-1/G2-2 curl ladder + AS-01 host court);
retip5 probe standings tip-wins; Smalt G2-6 tip-wins (columns **ALIVE** +
fin_ops **PARTIAL_ALIVE**); no false `ALIVE`; cargo not run. Tip pin retipped on `cursor/v2711-smalt-finops-alive-bc54` onto tip
`81774629e` — G2-6 FinOps **ALIVE** @ `smalt@91997da76f0f` (parent
`875efbb87` — tip includes both; `mix test test/fin_ops_calculator_test.exs`
exit **0**, **19 passed**); 4-cloud retained @ `875efbb87`; CCS/BRCE
G2-4/G2-5 exact next verifiers retained; retip5 probe tip-wins; Foundry crown stays
**PARTIAL_ALIVE**; full RAID/`ALIVE` **not** invented. Tip pin retipped on
`cursor/v2711-hop5-live-bc54` onto tip `6fdcf4665` — G2-8/G3-3 **PARTIAL_ALIVE**
(live adapter exit **0**; package **PRESENT** #884; foundry `--live` open);
present-pin ABSENT→PRESENT prose folded; Smalt FinOps **ALIVE** + G2-12 **ALIVE**
+ CCS/BRCE exact next verifiers tip-wins retained; Foundry stays
**PARTIAL_ALIVE**. Tip pin retipped on `cursor/v2711-hop4-manufacture-bc54`
onto tip `3a69becce` — G2-10 hop-4 BrandTheme manufacture **ALIVE** `diff_exit=0` (dual-schema; prior PARTIAL exit **2**
`REFUSED {:brand_theme_refused, :no_primitive_space}` closed); evidence
`/tmp/cursor/igniter-real-cleanroom.md` (`bc-4861c787`); residual = NIF/`mix sync --check`;
antifalsifier: bare post-clone ≠ hop-4 `ALIVE`; no false path-crown `ALIVE`.
Tip pin retipped on `cursor/v2711-truex-merged-standing-bc54` onto crown tip
`99255d53c` — G2-7 / G3-2 Truex admit-court **ALIVE** @
`truex@68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6)
**MERGED** → `main` merge `b4672131`; vitest **6/6** exit **0** reconfirmed on
mount `/tmp/truex`; receipt `/tmp/cursor/truex-merged-admit-alive.md`); hop-4 BrandTheme manufacture
**ALIVE** + hop-5 honesty (default adapter exit **2**) + G2-12 **ALIVE** +
Smalt FinOps **ALIVE** retained; Foundry stays **PARTIAL_ALIVE**; no false
path-crown / RAID `ALIVE`. Tip pin retipped on
`cursor/v2711-hop5-mix-subject-bc54` onto crown tip `5345d131e` — G2-8 / G3-3
**PARTIAL_ALIVE** after mix-SUBJECT / `resolveCleanRoomSyncCmd` fix (default
adapter exit **0**; Jobs/C4 deferred; foundry `--live` open); Truex G2-7/G3-2
**ALIVE** tip-wins; hop-4 BrandTheme manufacture **ALIVE** (`diff_exit=0`) +
Smalt FinOps **ALIVE** + G2-12 **ALIVE** retained; Foundry stays **PARTIAL_ALIVE**;
no false hop-5 / path-crown `ALIVE`.
Standings synthesized from existing v27.1.1 standing ledgers only plus this
worktree’s probes. Tracked rows remain **25** (`G0-1…4`, `G2-1…16`, `G3-1…5`).
