# Out-of-repo Gate 0 / 2 / 3 — next verifiers through GA 2027-01-01

| Field | Value |
|-------|-------|
| Document ID | `OUT-OF-REPO-NEXT-VERIFIERS` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `GATE-0-STANDING`, `GATE-3-RELEASE-COURTS-STANDING`, `FOUNDRY-FINISH-STANDING` |
| Target GA | **2027-01-01** |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `47aebeb7588da7c04b2de3233986c8ea2dac4252` (sidebranch `cursor/v2711-oor-retip18-bc54`; prior tip-wins CASTLE/VW/OOR retip17 lineage through `8c8b1e6e5`) — cos/revops standings **held** (BLOCKED/PARTIAL/UNKNOWN); env wire **PARTIAL**; production WASM runtime **UNSUPPORTED**; CASTLE digest-bind wire **ALIVE**/`WIRED` @ castle `e0d99c6`; Foundry zero-HITL crown **PARTIAL**; live WebAuthn **UNSUPPORTED**; **V8** **BLOCKED** |
| Materialized | 2026-11-22 |
| Hardened | 2026-11-22 — `cursor/v2711-oor-ledger-harden-bc54` |
| Ops refresh | 2026-11-22 — `cursor/v2711-ops-standing-refresh-bc54` (tip pin + probe reconfirm; standings held) |
| Tip pin | 2026-11-22 09:56:16 PST — `cursor/v2711-oor-retip18-bc54` onto tip `47aebeb75` (`47aebeb7588da7c04b2de3233986c8ea2dac4252`; prior retip17/`571571720` absorb `8c8b1e6e5`): probe unchanged vs retip17 — cos `https://cos.fnox.io/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA` empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL** ≠ ALIVE; revops **UNKNOWN**). No cargo. No ManagePullRequest. No false `ALIVE`. |
| Hop-4 evidence | `/tmp/cursor/igniter-real-cleanroom.md` (`bc-4861c787`) — tip dual-schema BrandTheme manufacture compare `diff_exit=0` (**ALIVE**); clean-room crown reconfirm `/tmp/cursor/igniter-cleanroom-reconfirm.md` (`bc-fd60df35`) mount EXIT **0** (**ALIVE**; prior `/tmp/cursor/igniter-cleanroom-alive-tip.md` / `bc-53b0e099`); path crown **PARTIAL** |
| Hop-5 evidence | [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) (`adapter_exit=0` / `layout_exit=0`; Chicago **31/0`); runtime **PRESENT** ([`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md)); foundry `--live` **foundry_exit=1** / **REFUSED** `Model access is not configured` @ zcode `f32df08` |
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
| G0-2 | **cos ledger fixes** | **BLOCKED** | Sibling `seanchatmangpt/fnox` / cos not mounted (ABSENT); no ledger replay from ggen; retip13 `/readyz` reconfirm **503** `DB_UNAVAILABLE` | (1) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`). (2) In mounted `seanchatmangpt/fnox`: owner artifact [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 2 — run that repo’s documented ledger replay/verify target (`just cos-ledger-verify` when present in fnox Justfile; else the CLAUDE.md/CI job named for cos ledger replay) → exit 0 + typed receipt or `REFUSED_*`. | Gate 0 cos row cannot become `ALIVE` from ggen; cos `/readyz` still **503** `DB_UNAVAILABLE` per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §16 |
| G0-3 | **servengine M4** | **BLOCKED** | Sibling servengine not mounted (ABSENT retip5 at `/agent/repos` + `/home/ubuntu`); archive stubs only (`servengine-frontend` foreign path) | Owner artifact: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) §3.3. In mounted **servengine** checkout: run the CI/milestone job the repo labels **M4** (`.github/workflows/*` or Justfile/`mix`/`cargo` target whose name/description binds M4) → exit 0 + typed receipt. **Refuse** `examples/archive/factory-paas/verify_mermaid.mjs` and `/Users/sac/srve/servengine-frontend` as verifiers. | M4 unproven at GA → hard Gate 0 risk alongside R-01…R-07 |
| G0-4 | **year-end SOW** | **UNKNOWN** | No commercial SOW register in ggen | Owner artifact: ops/commercial year-end SOW register (external custody). Verify: signed instrument present with parties + scope + period through **2027-01-01** (`test -f <admitted-register-path>` on the ops host, or register UI export digest filed outside ggen). Do not author SOW body here (`REFUSED_UNGROUNDED`). | Missing SOW at GA remains **UNKNOWN** / carry-forward with owner |

---

## Gate 2 — post Gate 1 (out-of-repo subjects)

ECOSYSTEM-SPEC §3 Gate 2–3: revops plane, full brand manifests, CASTLE publish, release courts.
Photolithography / spine hops that require mounted siblings are included when execution is **out-of-repo**.

### 2.1 Plane 5 / economic ops / trust (incl. BRCE human seat / WebAuthn production)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-1 | **revops.fnox.io** front door (D6) | **UNKNOWN** | DNS still fails on retip13: `getent` exit **2**; `dig` A/AAAA empty; `host` **NXDOMAIN**; `curl` exit **6** (`http=000`) | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200** on admitted ready path | If **2027-01-01** still no resolvable front door, D6 / spine falsifies |
| G2-2 | **cos.fnox.io** readiness + ledger | **BLOCKED** / **PARTIAL** | retip13 reconfirm: `/readyz` → **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` → **200** `{"status":"ok"}` (liveness **PARTIAL** ≠ readiness); fnox sibling **ABSENT** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` (pass: **200** not `DB_UNAVAILABLE`); then fnox/cos ledger court in mounted checkout ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4) | CASTLE LinkedIn/Jobs AND-gate needs cos `/readyz` **200** ∧ 2× durable LOW — stays **BLOCKED** until G2-2 passes |
| G2-3 | **BRCE human seat broker** (live CONSTRUCT→DO) | **BLOCKED** | No broker runtime mounted; doctrine + intent manufacture only in ggen; V7 fixture ≠ broker; **V8** still open | Owner artifact: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**. In mounted BRCE broker checkout: integration court — intent **without** verified AS-01 human seat → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit 0. (In-repo fence only: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` / V7 fixture — does **not** promote this row.) | Fixture/fence **PARTIAL** ≠ broker **ALIVE** at GA |
| G2-4 | **WebAuthn AS-01 production** (human seat `:prime256v1` / ES256) | **UNSUPPORTED** (live/production WebAuthn ceremony) + in-repo V7 fixture crypto-shape **PARTIAL_ALIVE** (land **ALIVE** @ `fb6e7fc6e`; ≠ production) | Full WebAuthn RP / hardware ceremony still absent; in-repo adapter court **PRESENT** on tip lineage through `ca52e9c16` | Owner artifact: [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 (Founder HITL; owner **BRCE**/trust-plane). Exact next: admit RP-ID → enroll hardware → live assertion court (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered / missing RP-ID / alg≠ES256 → `REFUSED_*`. In-repo replay only (non-substitute): `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` → exit 0. Companion V5: `p256` in `ggen-abb-sbb` only — `rg` free of silent `webauthn`/`passkeys` RP deps. Then **V8** (**BLOCKED**) ahead of **V9**. | Fixture PARTIAL ≠ live crypto / Plane-5 DO; need ceremony→V8→V9 before GA DO; **not** `ALIVE` |
| G2-5 | **Plane 5 DO path** (aggregate; AS-01-gated) | **BLOCKED** | ggen constitutional OUT OF SCOPE; no AS-01 runtime receipt; live WebAuthn ceremony still **UNSUPPORTED** (G2-4 production); V7 fixture does not close this row | Owner artifacts: BRCE **V9** + CCS #8 ([`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md)). Exact next probe ladder: (1) G2-1 `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ **200**; (2) G2-2 `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** not `DB_UNAVAILABLE`; (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6). | Never promote from ggen fixture alone; Foundry falsifies if founder still actuates Plane-5 without BRCE+AS-01 |
| G2-6 | **Smalt 4-cloud RAID** / economic ops | **ALIVE** — 4-cloud exits **0,0,0,0,0** @ `smalt@875efbb87` + FinOps **ALIVE** @ `91997da76f0f` (EXIT **0** / **19** reconfirm tip-wins onto `571571720`) + residual b4b treasury/tax/erp/rates exits **0,0,0,0** (8/8/8/7 passed) @ `91997da76f0f` (`bc-c700dafb`). ggen has zero in-repo RAID surface. ≠ Foundry / zero-HITL crown | Replay residual: [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4.3/§6b/§6c. Replay FinOps + §4.1 4-cloud. | GA fails if any column, FinOps, or residual court lacks HEAD-pinned green receipt in smalt |

### 2.2 Manufacture consumers (photolithography / UI / witness)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-7 | **Truex** admits W3C `shui:` shapes (D3 product) | **ALIVE** (admit-court); fence exact-tip5 **PARTIAL_ALIVE** | Sibling Chicago admit-court **6/6 vitest exit 0** **held** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main` merge `b4672131`; mount `/tmp/truex`; exact-tip4 tip-wins absorb onto crown `49ebdb27c` via `cursor/v2711-truex-exact-retip4-absorb-5ce6` / side `eeabe900e`; receipt `/tmp/cursor/truex-merged-admit-alive.md` + `/tmp/cursor/truex-admit-exact-retip4.status`): `shui:` **ADMITTED**; private `truex:` **REFUSED_PRIVATE**; unknown **REFUSED_UNKNOWN**. ggen prefix fence **ALIVE** (lineage) — tip-cargo **4 passed; exit 0** @ `26256ca9d` (sources ≡ crown `678a6c106`); exact-tip5 honesty tip-wins absorb onto `167352d0d` / side `d96b2e867` via `cursor/v2711-shui-exact-tip5-absorb-0777`; EXIT:0 reconfirm tip-wins onto `2d7b47b2e` / side `7f8e0765b` via `cursor/v2711-shui-exact-tip5-alive-absorb-3acf` ⇒ fence tip EXIT:**137** **PARTIAL_ALIVE** **held** ([`SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md) / [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md)) — fence ≠ Truex product crown. ≠ Foundry `ALIVE` | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233`. Companion fence (non-substitute): `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` | Admit-court **ALIVE** **held** (also reconfirm `cursor/v2711-truex-exact-retip-bc54` EXIT **0** @ tip `a3e5d3109` — [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md)); ggen fence lineage **ALIVE** @ `008a81d21`/`26256ca9d`; exact-tip tip-cargo **PARTIAL_ALIVE** **held** (EXIT **137**) |
| G2-8 | **zcode-cli** `ui-layout-court.ts` / `UiLayoutCourtAdapter` (D5) | **PARTIAL_ALIVE** — in-process **ALIVE** + live adapter **ALIVE** (exit **0**; mix-SUBJECT fix) | Sibling mounted; `ggen-layout-court` **PRESENT**; `layout_court` exit **0**; default `verifyPostTransition` exit **0** when layout_court PRESENT (mix ABSENT → Jobs/C4 deferred, not SUBJECT_MISSING). Foundry `--live` still open → not hop-5 crown ALIVE. Ledger: [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (G2-9). Replay: adapter verify → **0**; `bun test test/ui-layout-court-chicago.test.ts` → **0** | Claiming hop-5 crown ALIVE from adapter alone while foundry `--live` open; restating mix SUBJECT_MISSING after layout_court PRESENT + spatial PASS |
| G2-9 | **zcode-cli** Chicago POWL/OCEL + foundry loop | **PARTIAL** / live **BLOCKED**/**REFUSED** | Mounted; Gall `--live` @ zcode `f32df08` injects `createZcodeAgentExecutor`; Linux tip-matched runtime **PRESENT** (`vendor/zcode.cjs` via 3.14.4 linux `.deb` — darwin `sync:locked` / `sync:local` remain **UNSUPPORTED**); **foundry_exit=1** / **REFUSED** (`Model access is not configured`); provider registry **ABSENT**; host provider env names **ABSENT**; no admitted `--live` fixture bypass (**REFUSED**). G2-8 adapter exit **0**; `mix` **ABSENT**. Ledgers: [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md), [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md), [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) (**BLOCKED_HITL**; ≠ `--live` `ALIVE`) | Exact next: HITL provision (`/login` Coding Plan API key on Linux, or `~/.zcode/v2/provider_config.json` with non-empty `config.providerConfigRules.providerRules[].config.access.apiKey` + `defaultModelSelection`, or preflight env `ANTHROPIC_API_KEY`/`ZCODE_API_KEY`/…) → `bun scripts/powl-foundry-loop.ts --live` → exit **0**; Jobs/C4 residual **closed** (replay admitted pack `--check` → **0**); chicago: `bun test test/powl-ocel-chicago.test.ts` → **0** | Unbounded remediation at GA → **REFUSED** per R-06 / ECOSYSTEM; inventing ALIVE from runtime materialization alone; restating runtime missing after vendor PRESENT; inventing ALIVE from dry-run / empty registry |
| G2-10 | **ggen_igniter** clean-room / manufacture | **ALIVE** — (a) clean-room crown reconfirm tip `d0fec2f2f` / `bc-fd60df35` (`/tmp/cursor/igniter-cleanroom-reconfirm.md`) mount `git diff --exit-code` → **0** (prior `f1492ed55` / `bc-53b0e099`); (b) tip BrandTheme manufacture compare `git diff --exit-code` → **0** (dual-schema tip brand; `/tmp/cursor/igniter-real-cleanroom.md`); Jobs/C4 residual **ALIVE** tip-wins (admitted `--pack-dir sample-pack … --check` exit **0**); path crown **PARTIAL** | Prior `:no_primitive_space` closed; bare post-clone `git diff --exit-code` ≠ manufacture `ALIVE` (antifalsifier); clean-room EXIT:0 ≠ path crown `ALIVE`; NIF **ALIVE**; bare `mix … --check` **REFUSED_INVOCATION** antifalsifier | **Replay:** BrandTheme emit tip `docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml` + igniter fixture → `diff -rq` / `git diff --exit-code` → **0**. **Jobs/C4 replay:** admitted `--pack-dir test/fixtures/sample-pack … --check` → 0. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. Contract: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) / [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md) | Claiming hop-4 / path crown `ALIVE` from bare post-clone diff; inventing Jobs/C4 `ALIVE` from NIF `.so` or exit-2 `REFUSED:INVOCATION` |
| G2-11 | **verify_witness** production WASM &lt;250KB | Affidavit pin / measure **ALIVE** (`wc -c` **28673** @ `563eb322a`; tip-wins retip3 side `4396ab1b7` → crown via `cursor/v2711-vw-zcode-absorb-27b4`); ggen env wire **PARTIAL** (tip-wins `b2f6e12d2` / `<256000` gate; Chicago **8/8**); tip-cargo size court this retip3 **PARTIAL** (EXIT:**137**); production runtime **UNSUPPORTED**; placeholder size court lineage **ALIVE** ≠ prod | Affidavit checkout **ABSENT** this worktree; no CI-mounted real wasm; wasm32 **ABSENT**; `GGEN_VERIFY_WITNESS_WASM` **unset** in CI. Pin court closed by probe `bc-5f5786ff`: `just gatekeeper-wasm-size-court` EXIT **0**; `wc -c` **28673** (SHA `5faae464…`) — ledger [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md) / [`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md) | Owner: [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). Exact next for production `ALIVE`: CI-mounted wasm + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0**. Measure replay (held): `wc -c` → **28673**. Tip-cargo next: `/tmp/ggen-vw-size-retip3-court` → **8**/EXIT:**0** | Affidavit pin / measure **ALIVE** ≠ tip-cargo size court **PARTIAL** ≠ env-wire **PARTIAL** ≠ ggen production runtime **ALIVE**; do not cite fat `affidavit-wasm` (718736 B) as gatekeeper; ≠ Foundry `ALIVE` |

### 2.3 CASTLE / brand / commercial (Gate 2–3)

| ID | Subject | Standing | Blocker | Next exact verifier | GA note |
|----|---------|----------|---------|---------------------|---------|
| G2-12 | **CASTLE publish** / RedactionCourt (spine) | **ALIVE** (sibling courts); digest-bind production wire **ALIVE**/`WIRED` @ castle `e0d99c6` ⇒ spine wire **PARTIAL_ALIVE**; Foundry zero-HITL crown remains **PARTIAL** | LinkedIn send still **BLOCKED** on G2-2; in-ggen publish **UNSUPPORTED**; Foundry/HITL/OOR/WebAuthn not promoted | Replay: castle `cursor/digest-bind-admit-ca8a` @ `e0d99c6` — `cargo test --lib publish::digest_bind` → **4/4** exit **0**; `cargo test --test publish_digest_bind_court` → **6/6** EXIT **0** (retip3 reconfirm tip-wins onto `82348aeb4`); `publish_redaction_court` **13/13**; `cli_publish_brand` **6/6**; spine-probe + `--ggen-layout-digest` → exit **0** / `redaction_court=ALIVE` / `digest_bind.crown_wire=WIRED` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §14; castle `docs/sjira/v26.11.22/DIGEST-BIND-ADMIT.md`) | Wire **ALIVE**/`WIRED` ≠ Foundry/zero-HITL **ALIVE**; ≠ LinkedIn send; no ManagePullRequest on #849 |
| G2-13 | **Full DTCG brand inventory** (Fourth Watch / CindyCat) | **UNSUPPORTED** | Rhythm stubs in ggen only; color/type/CASTLE tokens Gate 2+ | Owner: brand plane + CASTLE. Admit OEM token sources (do not invent in ggen); then in **castle**/brand checkout: inventory court that loads admitted `brands/fourth-watch.brand.toml` + `brands/cindycat.brand.toml` full DTCG color/type/CASTLE sets → exit 0. ggen non-substitute: `cargo test -p ggen-graph --test brand_toml_rhythm_court` ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | Inventing tokens in ggen → **REFUSED** (ECOSYSTEM non-goal) |
| G2-14 | **CASTLE CindyCat public mint** (commercial) | **UNKNOWN** | No mint receipt; castle sibling **ABSENT** this worktree (retip5) | In **castle**: `castle publish post --subject cindycat-mint-probe --public-anonymize --brand brands/cindycat.brand.toml` + `cargo test --test publish_redaction_court` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md)); depends G2-2 AND-gate for outbound send | Commercial mint **UNKNOWN** until castle + cos ready |
| G2-15 | **Event #1** patronage (CindyCat-first) | **UNKNOWN** | No external acknowledgment in ggen | Owner artifact: ops/commercial register entry — written acknowledgment with **0% custody**, named third-party payer, amount **&gt;$250** ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) §3). Verify presence on ops host; do not fabricate receipt text here. | Deal crown stays **UNKNOWN** at GA without external receipt |
| G2-16 | **LinkedIn/Jobs automated send** | **BLOCKED** | stop-worked: cos `/readyz` retip13 still **503** `DB_UNAVAILABLE` + 2× durable LOW not observed; castle **ABSENT** | Clear G2-2 (`curl …/readyz` → 200); then on CASTLE plane: AND-gate court requiring cos readiness **and** 2× durable LOW receipts on disk before Jobs/LinkedIn send ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §4 item 3; castle publish README) | Send without gate → GA defect |

---

## Gate 3 — seven release courts (out-of-repo legs only)

Full table: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md).
Rows below are the **sibling / Plane-5** legs; in-repo PARTIAL courts (D2/D3 fence/D4 rhythm) are tracked in Gate-1 matrix, not duplicated here.
GA horizon for all courts: **2027-01-01**.

| ID | Court | Delta | Standing (out-of-repo leg) | Blocker | Next exact verifier | GA note |
|----|-------|-------|----------------------------|---------|---------------------|---------|
| G3-1 | **RC-D1** | Zero-DOM WebGPU + FFB (D1) | **UNSUPPORTED** | No UI-plane repo mounted in ggen | Owner artifacts: [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §5 Mode A/B; [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) RC-D1. In mounted UI-plane (Zero-DOM WebGPU / FFB) checkout: court that loads admitted ggen `AotSealReceipt` → paints Mode A canvas (or Mode B FFB) → exit 0; ambient DOM DO without BRCE+AS-01 → `REFUSED_*` | D1 required for GA per ECOSYSTEM §2 |
| G3-2 | **RC-D3** | Truex `shui:` product | **ALIVE** (admit-court; same as G2-7); fence exact-tip5 **PARTIAL_ALIVE** | Same as G2-7 (`truex@68d2d233` **MERGED**; vitest **6/6** exit 0; exact-tip4 tip-wins onto `49ebdb27c`; fence exact-tip5 tip-wins onto `167352d0d`; EXIT:0 reconfirm tip-wins onto `2d7b47b2e` EXIT:**137** PARTIAL held) | Same as G2-7 replay + companion ggen fence next (`/tmp/ggen-shui-exact-tip5`; lineage @ `26256ca9d` non-substitute) | Admit-court **ALIVE** **held**; ggen fence lineage **ALIVE**; exact-tip5 **PARTIAL_ALIVE** **held** |
| G3-3 | **RC-D5** | zcode `ui-layout-court.ts` / live adapter | **PARTIAL_ALIVE** (default adapter exit **0**; foundry_exit=1) | Same as G2-8 (`ggen-layout-court` PRESENT; adapter exit **0** / Jobs/C4 deferred; `resolveCleanRoomSyncCmd` path) | Exact next: `powl-foundry-loop --live` → 0 (G2-9) | RC-D5 ties to photolithography crown |
| G3-4 | **RC-D6** | Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE | Same as G2-1, G2-2, G2-5 (retip13: revops NXDOMAIN; cos **503**) | Same probe ladder as G2-1 + G2-2 + G2-5 (DNS/`readyz` + AS-01-gated BRCE receipt); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §3–§4 / §16 | Never promote from ggen evidence alone |
| G3-5 | **RC-D7** | Risks **R-01 … R-07** (D7) | **BLOCKED** (all seven bodies) | Sovereign risk-register bodies not mounted | Owner artifact: [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2 crown matrix. Per ID R-01…R-07: admit body from sovereign risk register (no invention) → typed receipt **or** typed `REFUSED_*` before **2027-01-01**. In-repo contract fence only: `cargo test -p ggen-graph --test r01_r07_resolution_court` (does not close any body). | Hard GA gate; silent closure → **REFUSED_UNGROUNDED** |

**RC-D2 / RC-D4 in-repo legs** (Taffy seal, brand rhythm/hot-swap): lineage tip-cargo **ALIVE** @ `26256ca9d` on crown `678a6c106` per [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); production theme **UNSUPPORTED** (`REFUSED_PRODUCTION`); **consumer / inventory** legs remain G2-8, G2-13 above for GA **ALIVE**.

---

## BLOCKED / PENDING — exact next verifiers (retip18 reconfirm @ tip `47aebeb75`; probe held from retip17/`571571720` + retip16/`acd3b8c89` + retip15/`01eccfffb` + retip14/`df9597c18` + retip13/`f65434315` + `098f53ea8` + retip12/`7ddfd8b6e` + retip11/`6a3a623ba` + retip10/`a40338aea` + retip9/`a508e6bae` + retip8/`5e131a75e` + honesty-retip8/`97c613452` + retip7 `a3b1c9410`)

| Subject | Standing | Exact next verifier |
|---------|----------|---------------------|
| Zero-HITL Foundry crown | **PARTIAL** | See [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a / §6 — founder HITL still needed for cos DB, WebAuthn live/production (**UNSUPPORTED**), foundry `--live` (model access); ui_layout exact tip-cargo **ALIVE** @ `376d0d283` (prior `a508e6bae`) |
| tip-cargo (`ui_layout`) | **ALIVE (exact tip-cargo)** @ `376d0d283` | Exact-retip `cursor/v2711-uilayout-exact-retip-bc54`: `/tmp/ggen-uilayout-exact-retip-hl` → **11 passed; exit 0** @ cargo tip `376d0d283` (sources ≡ live tip; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)). Prior exact `a508e6bae` (`bc-70188ac3`) + exact-retip5 EXIT:**124** **PARTIAL** superseded by this EXIT:**0**. Lineage prior: **11** @ `59c090984` / `ca52e9c16`. No crown absorb; no ManagePullRequest |
| foundry `--live` / model access (G2-9) | **REFUSED** / **BLOCKED_HITL** | Runtime **PRESENT**; **foundry_exit=1** / `Model access is not configured`; registry/env **ABSENT**; Founder HITL pin [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); keys [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** — hop-5 **REFUSED** until then; **do not invent** `--live` / Foundry crown `ALIVE` |
| `mix ggen_igniter.sync` NIF / Jobs/C4 (G2-10 residual) | **ALIVE** — admitted `--pack-dir sample-pack … --check` exit **0**; NIF **ALIVE**; bare `--check` **REFUSED_INVOCATION** antifalsifier (default PATH `mix` **ABSENT**; off-PATH **PRESENT** — [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)) | Replay admitted pack/template `--check` → **0** |
| cos DB / `/readyz` (G0-2 / G2-2) | **BLOCKED** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (retip18 reconfirm **503** `DB_UNAVAILABLE`; held from retip17) |
| WebAuthn live/production (G2-4) + BRCE Plane-5 (G2-3 / G2-5) | **UNSUPPORTED** (WebAuthn production) / **BLOCKED** (broker + Plane-5) | Exact next WebAuthn: [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 (Founder HITL RP/hardware ceremony). Then BRCE **V8** broker then **V9** ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5) |
| revops.fnox.io (G2-1) | **UNKNOWN** | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200** (retip18: NXDOMAIN / curl exit **6**; held from retip17) |

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
| [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md) | Push-first OOR honesty matrix (standing + evidence tip + next verifier + owner) |
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
onto live crown tip `750b8ef4a9042d22808a79be3dedff7944d4e321` — G2-4 WebAuthn crypto then **UNSUPPORTED**
(`as01_assertion` then **ABSENT**; named `brce_as01_es256_assertion_court` + V5 `rg`);
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
no false hop-5 / path-crown `ALIVE`. Tip pin retipped on `cursor/v2711-ccs-webauthn-retip-bc54` onto tip `d05a131c` — G2-4 V7 fixture crypto-shape **PARTIAL_ALIVE** (`as01_assertion` **PRESENT**; land `fb6e7fc6e` **4 passed; exit 0**) while live/production WebAuthn ceremony stays **UNSUPPORTED**; G2-3 broker **BLOCKED** (**V8**); G2-5 Plane-5 DO **BLOCKED** (**V9** + G2-1/G2-2 curl ladder; truncated G2-5 verifier cell repaired); CCS `construct_hash_gate` **PRESENT**/**PARTIAL** (land `3ec8afd85`); hop-5/Truex/Smalt/CASTLE tip-wins retained; no false live-crypto / Plane-5 `ALIVE`; cargo not run.
Standings synthesized from existing v27.1.1 standing ledgers only plus this
worktree’s probes. Tracked rows remain **25** (`G0-1…4`, `G2-1…16`, `G3-1…5`).
Tip pin retipped on `cursor/v2711-companion-honesty-bc54` onto crown
`678a6c106` (post–Gate-1 honesty `b565b8e83`): ECOSYSTEM §7–8 + companions
align shui/taffy/hot-swap lineage **ALIVE** @ `26256ca9d`; brand production
**UNSUPPORTED**; hop-5 foundry `--live` **REFUSED**; Foundry crown **not**
`ALIVE`; Observed ggen tip pin corrected from stale `8cee4246f`; no cargo;
no ManagePullRequest.
Tip pin retipped on `cursor/v2711-foundry-standing-retip-bc54` onto crown tip `d05a131c7` — FOUNDRY/PHOTO/OOR honesty:
ggen+Taffy **PRESENT**; zcode adapter **PARTIAL**; CASTLE **ALIVE** (sibling); Smalt **ALIVE**;
revops/cos **BLOCKED**; Zero-HITL **PARTIAL**; hop-4 **ALIVE**; hop-5 **PARTIAL_ALIVE**;
mix sync NIF **BLOCKED**; G2-5 truncated verifier repaired; no cargo; no false zero-HITL `ALIVE`. Tip pin retipped on `cursor/v2711-oor-retip6-bc54` onto live crown tip `ca52e9c16` (`ca52e9c1679d66d38d6cf7e196195ee758bee597`) — non-cargo honesty reconfirm: cos **BLOCKED** (`/readyz` **503**); WebAuthn live/production **UNSUPPORTED**; ZCode runtime ABSENT → Hop5 `--live` **BLOCKED**/**REFUSED**; tip-cargo **PENDING**/in-flight; production WASM/brand rows **UNSUPPORTED**; tracked rows remain **25**; no cargo; no ManagePullRequest; no false zero-HITL/`ALIVE`. Absorb hop-5 foundry-live2 `01f85f32d` (#918) + hop-5-zcode-runtime `4ea058dac` (#917) onto tip past `fc8d54ac8`/`a3b1c9410` — Linux runtime **PRESENT**; `--live` **BLOCKED** on model access; tip-cargo **PENDING** spared; no cargo; no ManagePullRequest.
Tip pin retipped on `cursor/v2711-oor-retip7-cos-bc54` onto tip `a3b1c9410`
(`a3b1c94103984e2d362c068deb61cd700615e158`) — Gate0/ops probe-only: cos
`/readyz` **503** `DB_UNAVAILABLE` → **BLOCKED**; `/healthz` **200** →
**PARTIAL**; revops **NXDOMAIN**/`getent` exit **2**/`curl` exit **6** →
**UNKNOWN**; tip-cargo locks untouched / **PENDING**; no cargo; no
ManagePullRequest; no false Plane-5/`ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip8-bc54` onto tip `5e131a75e` at 2026-11-22 06:09:47 PST — probe unchanged: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200**; `dig` A/AAAA empty; revops **NXDOMAIN**/curl exit **6** → standings held; no cargo; no ManagePullRequest; no false `ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip9-0e29` onto tip `a508e6bae` (`a508e6bae9e61d7894ad98df8c507fdb1fb28eb2`) at 2026-11-22 07:12:40 PST — probe unchanged vs retip8: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip10-ecdf` onto tip `a40338aea` (`a40338aea16bccd72cbc8baffe26b004155b34d4`) at 2026-11-22 07:37:36 PST — probe unchanged vs retip9: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip11-fec0` onto tip `6a3a623ba` (`6a3a623bacd33efdc1ec7efccf6a35141662ca53`) at 2026-11-22 07:42:56 PST — post–ui_layout exact tip-cargo absorb; probe unchanged vs retip10: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip12-5958` onto tip `7ddfd8b6e` (`7ddfd8b6efd10ed21cd514f7bb17a5a63056a79c`) at 2026-11-22 07:58:41 PST — tip-wins after affidavit absorb race (probe base `1dbf1cc2e`); probe unchanged vs retip11: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`.
Tip pin retipped on `cursor/v2711-oor-retip13-bc54` onto tip `f65434315` (`f654343155c902a003a852f61df345e986d98b5e`; parent `098f53ea8`) at 2026-11-22 08:13:20 PST — tip-wins vw-env-wire absorb; probe unchanged vs retip12: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §16. Tip-wins absorb onto crown `cc2b344bc` (⊇ BRCE V8).
Tip pin retipped on `cursor/v2711-oor-retip14-bc54` onto tip `df9597c18` (`df9597c1817313f8d5173f425af28aaf485d114c`; parent `7ea96a70d`) at 2026-11-22 08:29:02 PST — tip-wins HITL `--live` re-probe absorb; probe unchanged vs retip13: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §17. Tip-wins absorb onto crown via `cursor/v2711-zcode-alive-oor-absorb-2230`.
Tip pin retipped on `cursor/v2711-oor-retip15-bc54` onto tip `01eccfffb` (`01eccfffb56c1c59be67a220eadb5bd98783c806`; parent `1f10949a7`) at 2026-11-22 08:40:12 PST — tip-wins zcode refusal ALIVE + OOR retip14 absorb; probe unchanged vs retip14: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §18. Tip-wins absorb onto crown (content @ `ee2e8f63c` ⊇ `7bcf6cff2`; absorb honesty via `cursor/v2711-oor15-curve-absorb-ded8`).
Tip pin retipped on `cursor/v2711-oor-retip16-bc54` onto tip `acd3b8c89` (`acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3`; parent `b49ca04ea`) at 2026-11-22 08:59:55 PST — probe unchanged vs retip15: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §19.
Tip pin retipped on `cursor/v2711-oor-retip17-bc54` onto tip `571571720` (`571571720620cd7462b9a740e3bf0b0fdeebc17a`; prior retip16 pin `acd3b8c89` / `423e7a831`) at 2026-11-22 09:20:38 PST — probe unchanged vs retip16: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §20. Tip-wins absorb onto crown tip `8c8b1e6e5` (⊇ Foundry PARTIAL honesty + Smalt RAID absorb `2c144954c`; sidebranch `e5dadcc03` / honesty `4cdbabff7` / side tip `b9ddaa833`).
Tip pin retipped on `cursor/v2711-oor-retip18-bc54` onto tip `47aebeb75` (`47aebeb7588da7c04b2de3233986c8ea2dac4252`; prior retip17 pin `571571720` / absorb `8c8b1e6e5`) at 2026-11-22 09:56:16 PST — probe unchanged vs retip17: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`; `dig` A/AAAA empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000` (revops readyz **and** healthz) → standings held (cos readiness **BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**); no cargo; no ManagePullRequest; no false `ALIVE`. See §21. Sidebranch only — leave tip-wins absorb for absorb agent.

---

## 16. Retip13 honesty probe (2026-11-22 08:13:20 PST)

Branch `cursor/v2711-oor-retip13-bc54` @ `e2ca6a69a` from probe tip
`f654343155c902a003a852f61df345e986d98b5e` (parent `098f53ea8589c7ab05195fb6563c374bbc7765a4`);
tip-wins absorb onto crown `cc2b344bc` (⊇ BRCE V8 / `8d761d8a1`).
**No cargo.** No ManagePullRequest on #849.
Standings **held**; no false `ALIVE`. First probe pass 08:11:26 PST on
`098f53ea8`; tip-wins reconfirm 08:13:20 PST on `f65434315` — identical codes.

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -o /tmp/cos-readyz13.json -w '%{http_code}' https://cos.fnox.io/readyz` | **503** | `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl -sS -o /tmp/cos-healthz13.json -w '%{http_code}' https://cos.fnox.io/healthz` | **200** | `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl -sS -o /tmp/revops-readyz13.json -w '%{http_code}' https://revops.fnox.io/readyz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §16. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).

---

## 17. Retip14 honesty probe (2026-11-22 08:29:02 PST)

Branch `cursor/v2711-oor-retip14-bc54` @ `72662eeff` from probe tip
`df9597c1817313f8d5173f425af28aaf485d114c` (parent
`7ea96a70df61ad782078775e20ff692de50ec067`; prior retip13 pin `f65434315` /
`e2ca6a69a`). **No cargo.** No ManagePullRequest on #849. Tip-wins absorb onto crown via
`cursor/v2711-zcode-alive-oor-absorb-2230`. Standings **held**; no false `ALIVE`.

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` | **503** | `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/healthz` | **200** | `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §17. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).

---

## 18. Retip15 honesty probe (2026-11-22 08:40:12 PST)

Branch `cursor/v2711-oor-retip15-bc54` @ `5c969ac05` (side tip `a6f90ea9b`)
from probe tip `01eccfffb56c1c59be67a220eadb5bd98783c806` (parent
`1f10949a78523348951f55583d94bdac7bf63c66`; prior retip14 pin `df9597c18` /
`72662eeff`). **No cargo.** No ManagePullRequest on #849. Tip-wins absorb onto
crown tip `bebea5dd0` via `cursor/v2711-oor15-curve-absorb-ded8`. Standings
**held**; no false `ALIVE`. ALIVE gate unmet (requires cos `/readyz` **200** ∧
revops `/readyz` **200**).

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` | **503** | `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/healthz` | **200** | `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz` | `getent` exit **2**; curl exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §18. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).

---

## 19. Retip16 honesty probe (2026-11-22 08:59:55 PST)

Branch `cursor/v2711-oor-retip16-bc54` @ `0032fc0b8` (side tip `423e7a831`) from probe tip
`acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3` (parent
`b49ca04eaea9fd54301570491c9749a15ebc08ee`; prior retip15 pin `01eccfffb` /
`a6f90ea9b`). **No cargo.** No ManagePullRequest on #849. Sidebranch only —
crown **not** absorbed. Standings **held**; no false `ALIVE`. ALIVE gate unmet
(requires cos `/readyz` **200** ∧ revops `/readyz` **200**). Earlier same-host
probe 08:58:06 PST identical codes.

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/readyz` | **503** / exit **0** | `{"status":"DB_UNAVAILABLE"}` (`time_total≈0.29s`; `remote_ip=34.8.148.142`) | **BLOCKED** |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/healthz` | **200** / exit **0** | `{"status":"ok"}` (`time_total≈0.29s`; `remote_ip=34.8.148.142`) | **PARTIAL** (liveness ≠ readiness) |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://revops.fnox.io/readyz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §19. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).

## 20. Retip17 honesty probe (2026-11-22 09:20:38 PST)

Branch `cursor/v2711-oor-retip17-bc54` @ `4cdbabff7` (side tip `b9ddaa833`;
sidebranch tip `e5dadcc03`) from probe tip
`571571720620cd7462b9a740e3bf0b0fdeebc17a` (prior retip16 pin `acd3b8c89` /
`423e7a831`). **No cargo.** No ManagePullRequest on #849. Tip-wins absorb onto
crown tip `8c8b1e6e5` (⊇ Foundry PARTIAL honesty retip + Smalt RAID absorb
`2c144954c`). Standings **held**; no false `ALIVE`. ALIVE gate unmet (requires
cos `/readyz` **200** ∧ revops `/readyz` **200**). Probe unchanged vs §19. No
Foundry `ALIVE`.

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/readyz` | **503** / exit **0** | `{"status":"DB_UNAVAILABLE"}` (`time_total≈0.21s`; `remote_ip=34.8.148.142`) | **BLOCKED** |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/healthz` | **200** / exit **0** | `{"status":"ok"}` (`time_total≈0.13s`; `remote_ip=34.8.148.142`) | **PARTIAL** (liveness ≠ readiness) |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://revops.fnox.io/readyz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §20. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).

---

## 21. Retip18 honesty probe (2026-11-22 09:56:16 PST)

Branch `cursor/v2711-oor-retip18-bc54` from probe tip
`47aebeb7588da7c04b2de3233986c8ea2dac4252` (live crown
`origin/cursor/ecosystem-spec-27-1-1-bc54`; prior retip17 pin `571571720` /
absorb `8c8b1e6e5` / honesty `4cdbabff7`). **No cargo.** No ManagePullRequest
on #849. Sidebranch only — crown **not** absorbed (prefer leave for absorb
agent). Standings **held**; no false `ALIVE`. ALIVE gate unmet (requires cos
`/readyz` **200** ∧ revops `/readyz` **200**). Probe unchanged vs §20 /
retip17 (cos ready z **503** `DB_UNAVAILABLE` **BLOCKED**; healthz **200**
**PARTIAL**; revops **NXDOMAIN** **UNKNOWN**). No Foundry `ALIVE`.

Exact probes (America/Los_Angeles):

| Probe | HTTP / exit | Body / note | Standing |
|-------|-------------|-------------|----------|
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/readyz` | **503** / exit **0** | `{"status":"DB_UNAVAILABLE"}` (`time_total≈0.42s`; `remote_ip=34.8.148.142`) | **BLOCKED** |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://cos.fnox.io/healthz` | **200** / exit **0** | `{"status":"ok"}` (`time_total≈0.13s`; `remote_ip=34.8.148.142`) | **PARTIAL** (liveness ≠ readiness) |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://revops.fnox.io/readyz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |
| `curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\n' https://revops.fnox.io/healthz` | exit **6** / `http=000` | `Could not resolve host: revops.fnox.io` (**NXDOMAIN**) | **UNKNOWN** |

DNS corroboration: `dig +short revops.fnox.io A`/`AAAA` empty; `host` exit **1**
**NXDOMAIN**; `getent` exit **2**; `dig +short cos.fnox.io A` → `34.8.148.142`
(edge **PARTIAL** only).

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Companion detail:
[`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §21. Next verifiers remain
the BLOCKED/PENDING table above (cos `/readyz` → **200**; revops DNS ∧ `/readyz`
**200**).
