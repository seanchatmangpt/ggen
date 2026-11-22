# Gate 0 / 2 / 3 — tip-honest next-verifier matrix (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `GATE-0-2-3-NEXT-VERIFIERS-27.1.1` |
| Binding | `ECOSYSTEM-SPEC-27.1.1` Gate 0 + Gate 2–3 out-of-repo legs |
| Companion index | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) (full provenance + GA notes) |
| Target GA | **2027-01-01** |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3`** (`acd3b8c89`) |
| Sidebranch | `cursor/v2711-gate023-next-retip-bc54` |
| Probe time | 2026-11-22 09:00:45 PST / `2026-11-22T17:00:45Z` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty retip) |
| Crown absorb | **HELD** tip-wins onto `origin/cursor/ecosystem-spec-27-1-1-bc54` @ pre-absorb `497db5eae` via `cursor/v2711-gate023-next-absorb-8489` (side `04e90176e`); no ManagePullRequest |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** No row is promoted to `ALIVE` from this retip. Connector inventory,
sibling tip SHAs, fixture/fence tip-cargo, and docs presence are observation only
until the named next verifier exits **0** (or a typed `REFUSED_*` is filed with
owner). Dual facets (e.g. fixture **PARTIAL** ≠ live **UNSUPPORTED**) stay
explicit — primary standing is the GA-facing subject of the next verifier.

---

## Tip probe reconfirm (2026-11-22 09:00:45 PST)

Held vs OOR retip15 / crown tip `acd3b8c89` — standings **held**; no false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz` | **503** `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/healthz` | **200** `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `getent hosts revops.fnox.io` | exit **2** | **UNKNOWN** |
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** | **UNKNOWN** |
| `curl … https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge **PARTIAL** only |

---

## Matrix summary (25 tracked rows @ tip `acd3b8c89`)

| Standing | Count | IDs |
|----------|------:|-----|
| **ALIVE** | **5** | G2-6, G2-7, G2-10, G2-12, G3-2 |
| **PARTIAL** | **4** | G2-8, G2-9, G2-11, G3-3 |
| **BLOCKED** | **8** | G0-1, G0-2, G0-3, G2-2, G2-3, G2-5, G2-16, G3-5 |
| **UNSUPPORTED** | **3** | G2-4, G2-13, G3-1 |
| **UNKNOWN** | **5** | G0-4, G2-1, G2-14, G2-15, G3-4 |
| **Total** | **25** | G0-1…4 + G2-1…16 + G3-1…5 |

Named companion pins (not extra matrix rows): BRCE **V8** **BLOCKED**; live WebAuthn **UNSUPPORTED**; HITL `--live` **BLOCKED_HITL**; R-01…R-07 bodies **BLOCKED**; Smalt RAID **ALIVE** (≡ G2-6); CASTLE sibling courts **ALIVE** / wire **PARTIAL** (≡ G2-12); VW production runtime **UNSUPPORTED** (G2-11 composite **PARTIAL**).

---

## Gate 0 — Nov 21 – Dec 5 (OUT of ggen DO)

Detail: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| Clause | Standing | Tip SHA cited | Exact next verifier |
|--------|----------|---------------|---------------------|
| **G0-1** GCP billing | **BLOCKED** | tip `acd3b8c89` (doctrine held; no live Marketplace Usage API / invoice in this worktree) | Owner artifact: `docs/marketplace/billing-contract.md` (DoD + Weekly Reconciliation). Console: `https://console.cloud.google.com/billing` → admitted account covering Gate 0→GA. Weekly: reconciliation block in that contract (`gcloud firestore documents list` + Marketplace usage count → exit 0 / `✓ RECONCILED`) + GCS audit export. ggen must not claim `ALIVE`. |
| **G0-2** cos ledger fixes | **BLOCKED** | tip `acd3b8c89`; probe **503** `DB_UNAVAILABLE` | (1) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`). (2) In mounted `seanchatmangpt/fnox`: [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 — `just cos-ledger-verify` (or named CI ledger replay) → exit 0 + typed receipt/`REFUSED_*`. |
| **G0-3** servengine M4 | **BLOCKED** | tip `acd3b8c89` (sibling ABSENT) | In mounted **servengine**: CI/milestone job labeled **M4** → exit 0 + typed receipt. Refuse `examples/archive/factory-paas/verify_mermaid.mjs` / foreign laptop paths. |
| **G0-4** year-end SOW | **UNKNOWN** | tip `acd3b8c89` (no commercial register in ggen) | Ops/commercial year-end SOW register (external): signed instrument with parties + scope + period through **2027-01-01**. Do not author SOW body here (`REFUSED_UNGROUNDED`). |

---

## Gate 2 — out-of-repo subjects

### Plane 5 / BRCE / WebAuthn / Smalt

| Clause | Standing | Tip SHA cited | Exact next verifier |
|--------|----------|---------------|---------------------|
| **G2-1** revops.fnox.io front door | **UNKNOWN** | tip `acd3b8c89`; probe NXDOMAIN / curl exit **6** | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS **and** HTTP **200**. |
| **G2-2** cos.fnox.io readiness + ledger | **BLOCKED** (readiness); healthz **PARTIAL** ≠ ALIVE | tip `acd3b8c89`; `/readyz` **503**; `/healthz` **200** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** not `DB_UNAVAILABLE`; then fnox/cos ledger court ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4). |
| **G2-3** BRCE human seat broker (**V8**) | **BLOCKED** | tip `acd3b8c89`; pin [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) | Founder HITL: complete live WebAuthn first ([`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3). In mounted BRCE broker: intent **without** AS-01 → `REFUSED_*`; **with** verified seat → execution receipt + replay digest → exit **0**. In-repo V1/V7 fences ≠ V8 `ALIVE`. |
| **G2-4** WebAuthn AS-01 production | **UNSUPPORTED** (live); V7 fixture **PARTIAL** ≠ live | tip `acd3b8c89`; pin [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) | Founder HITL: admit RP-ID → enroll hardware → live assertion (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered/missing RP-ID/alg≠ES256 → `REFUSED_*`. Non-substitute: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court`. Then **V8** → **V9**. |
| **G2-5** Plane 5 DO path (aggregate) | **BLOCKED** | tip `acd3b8c89` | Ladder: (1) G2-1 DNS ∧ `/readyz` **200**; (2) G2-2 `/readyz` **200**; (3) host court — without AS-01 → refuse; with AS-01 → BRCE receipt ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6; BRCE **V9**). |
| **G2-6** Smalt 4-cloud RAID / FinOps | **ALIVE** | tip-wins retip absorb onto `571571720` / side `99bd2938a`; smalt `875efbb87` + FinOps/`91997da76f0f` ([`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §6c) | Replay residual §4.3/§6b + FinOps + §4.1 4-cloud (exits **0,0,0,0,0** + residual **0,0,0,0**; FinOps EXIT **0** / **19** reconfirm). ≠ Foundry / zero-HITL crown. |

### Manufacture consumers / CASTLE / commercial

| Clause | Standing | Tip SHA cited | Exact next verifier |
|--------|----------|---------------|---------------------|
| **G2-7** Truex admits `shui:` | **ALIVE** (admit-court); fence exact-tip5 **PARTIAL_ALIVE** | tip-wins exact-tip4 onto `49ebdb27c` / side `eeabe900e`; fence tip-wins exact-tip5 onto `167352d0d` / side `d96b2e867`; `truex@68d2d233` (merged) | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233`. Fence (non-substitute): `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` ([`SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md)). ≠ Foundry. |
| **G2-8** zcode `ui-layout-court` / adapter | **PARTIAL** | tip `acd3b8c89`; adapter exit **0** ([`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md)) | Exact next: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (G2-9). Replay: adapter verify → **0**; `bun test test/ui-layout-court-chicago.test.ts` → **0**. |
| **G2-9** zcode foundry `--live` / HITL | **PARTIAL** (path); live **BLOCKED_HITL** / **REFUSED** | tip `acd3b8c89`; pin [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); zcode `f32df08`; **foundry_exit=1** | Founder HITL provision model access → `cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**. Do **not** invent `--live` `ALIVE` from runtime PRESENT / adapter exit 0 / refusal tip-cargo. |
| **G2-10** ggen_igniter clean-room / manufacture | **ALIVE** (clean-room + BrandTheme); path crown **PARTIAL**; Foundry `--live` **BLOCKED_HITL** (≠ clean-room) | tip-wins absorb onto pre-absorb `94ea8cf25` / side `5c6eae752`; evidence `/tmp/cursor/igniter-cleanroom-retip.md` (`bc-17cf3ea2`); prior `d0fec2f2f` / `bc-fd60df35` | Replay: `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0**; BrandTheme emit + compare → **0**; Jobs/C4 admitted `--pack-dir sample-pack … --check` → **0**. Bare post-clone diff ≠ path crown `ALIVE`; clean-room EXIT:0 ≠ `--live` Foundry `ALIVE`. |
| **G2-11** verify_witness production WASM | **PARTIAL** (env wire + affidavit **ALIVE**); production runtime **UNSUPPORTED** | tip `acd3b8c89`; pin [`VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md`](./VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md); affidavit `563eb322a` | Exact next for production: CI-mount real wasm + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0**. Affidavit replay: `just gatekeeper-wasm-size-court` → **0**; `wc -c` → **28673**. Do **not** invent production `ALIVE`. |
| **G2-12** CASTLE publish / digest-bind | **ALIVE** (sibling courts); spine wire **PARTIAL** (`WIRED`); Foundry crown **PARTIAL** | tip-wins retip3 absorb onto `82348aeb4` / side `946e77f15`; castle `e0d99c6` / `2774039ea` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §14) | Replay: `cargo test --lib publish::digest_bind` → **4/4**; `cargo test --test publish_digest_bind_court` → **6/6** EXIT **0** (retip3 reconfirm); `publish_redaction_court` **13/13**; `cli_publish_brand` **6/6**; spine-probe + `--ggen-layout-digest` → `digest_bind.crown_wire=WIRED`. ≠ LinkedIn send; ≠ zero-HITL / Foundry `ALIVE`. |
| **G2-13** Full DTCG brand inventory | **UNSUPPORTED** | tip `acd3b8c89`; rhythm stubs only ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | Admit OEM token sources; in castle/brand checkout: inventory court for full DTCG color/type/CASTLE sets → exit 0. Non-substitute: `cargo test -p ggen-graph --test brand_toml_rhythm_court`. |
| **G2-14** CASTLE CindyCat public mint | **UNKNOWN** | tip `acd3b8c89` (mint receipt absent) | In **castle**: `castle publish post --subject cindycat-mint-probe --public-anonymize --brand brands/cindycat.brand.toml` + `cargo test --test publish_redaction_court`; depends G2-2 AND-gate for outbound. |
| **G2-15** Event #1 patronage | **UNKNOWN** | tip `acd3b8c89` | Ops register: written acknowledgment with **0% custody**, named third-party payer, amount **>$250**. Do not fabricate receipt text. |
| **G2-16** LinkedIn/Jobs automated send | **BLOCKED** | tip `acd3b8c89`; cos `/readyz` still **503** | Clear G2-2 (`curl …/readyz` → **200**); then CASTLE AND-gate court requiring cos readiness ∧ 2× durable LOW before send. |

---

## Gate 3 — seven release courts (out-of-repo legs)

Detail: [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md).

| Clause | Standing | Tip SHA cited | Exact next verifier |
|--------|----------|---------------|---------------------|
| **G3-1** RC-D1 Zero-DOM WebGPU + FFB | **UNSUPPORTED** | tip `acd3b8c89` (UI-plane unmounted) | In mounted UI-plane: court loads admitted `AotSealReceipt` → Mode A/B paint → exit 0; ambient DOM DO without BRCE+AS-01 → `REFUSED_*`. |
| **G3-2** RC-D3 Truex `shui:` product | **ALIVE** (admit-court; ≡ G2-7); fence exact-tip5 **PARTIAL_ALIVE** | tip-wins exact-tip4 onto `49ebdb27c`; fence tip-wins exact-tip5 onto `167352d0d`; `truex@68d2d233` | Same as G2-7 replay + companion fence (non-substitute). |
| **G3-3** RC-D5 zcode ui-layout live | **PARTIAL** (≡ G2-8; foundry_exit=1) | tip `acd3b8c89` | Exact next: `powl-foundry-loop --live` → 0 (G2-9 / HITL pin). |
| **G3-4** RC-D6 Plane 5 front doors | **UNKNOWN** / OUT OF SCOPE from ggen | tip `acd3b8c89`; revops NXDOMAIN; cos **503** | Same ladder as G2-1 + G2-2 + G2-5. |
| **G3-5** RC-D7 R-01…R-07 bodies | **BLOCKED** (all seven bodies) | tip `acd3b8c89`; pin [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md); contract tip-cargo **ALIVE** @ `e7569893a` ≠ bodies | Founder HITL: admit sovereign risk-register → per-ID typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract only: `cargo test -p ggen-graph --test r01_r07_resolution_court` (does not close bodies). |

---

## Named companion pins (tip `acd3b8c89`)

| Companion | Standing | Exact next verifier |
|-----------|----------|---------------------|
| BRCE **V8** broker | **BLOCKED** | [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3 — mounted broker refuse/admit court → exit **0** |
| Live WebAuthn / FIDO2 | **UNSUPPORTED** | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 — Founder HITL RP + hardware ceremony → exit **0** |
| HITL foundry `--live` | **BLOCKED_HITL** | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 — provision keys → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| R-01…R-07 bodies | **BLOCKED** | [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) §4 — admit register → per-ID receipt/`REFUSED_*` |
| Smalt GA RAID | **ALIVE** | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4 — replay 4-cloud + FinOps + residual |
| CASTLE spine | **ALIVE** courts / wire **PARTIAL** | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) — digest-bind + redaction courts |
| VW production CI-wasm | **UNSUPPORTED** (prod); env **PARTIAL**; affidavit **ALIVE** | [`VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md`](./VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md) §3 |

---

## Explicit non-claims

- This retip does **not** promote Gate 0 billing/cos/servengine/SOW to `ALIVE`.
- Live WebAuthn stays **UNSUPPORTED**; BRCE **V8**/**V9** stay **BLOCKED**; R-01…R-07 bodies stay **BLOCKED**.
- cos `/readyz` **503** and revops **NXDOMAIN** are reconfirmed — Plane-5 front doors are **not** `ALIVE`.
- foundry `--live` stays **BLOCKED_HITL** (`foundry_exit=1`) — refusal tip-cargo ≠ live `ALIVE`.
- Smalt / Truex / CASTLE sibling / igniter clean-room `ALIVE` rows do **not** promote Foundry / zero-HITL / path crown / LinkedIn send.
- verify_witness affidavit pin `ALIVE` ≠ ggen production runtime `ALIVE`.
- No crown absorb. No ManagePullRequest on #849. No force-push. No cargo.

---

## Aggregate replay (this ledger)

```text
test -f docs/rfc/v27.1.1/GATE-0-2-3-NEXT-VERIFIERS.md
rg -n '^\| \*\*G[023]-' docs/rfc/v27.1.1/GATE-0-2-3-NEXT-VERIFIERS.md | wc -l
# Expect 25 clause rows (G0×4 + G2×16 + G3×5)
rg -n 'acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3' docs/rfc/v27.1.1/GATE-0-2-3-NEXT-VERIFIERS.md
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
getent hosts revops.fnox.io; curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz
```

---

## Cross-links

| Doc | Role |
|-----|------|
| [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | Full Gate 0/2/3 provenance + GA notes |
| [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) | Gate 0 stub citations |
| [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md) | Commercial priority |
| [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | D1–D7 release courts |
| [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Zero-HITL / horizontal spine |
| [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md) | Crown progress (not goal-complete) |

---

## Provenance

Materialized 2026-11-22 on `cursor/v2711-gate023-next-retip-bc54` from
`origin/cursor/ecosystem-spec-27-1-1-bc54` @
`acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3` (tip-wins absorb bodies content tip +
webauthn **UNSUPPORTED** held). Synthesized from Gate0/2/3 companions, OOR
ledger, BRCE V8 / WebAuthn live / HITL live / R01-R07 bodies / Smalt / CASTLE /
VW pins. HTTP+DNS probe reconfirm at 09:00:45 PST — standings held; **25**
tracked rows; **no** false `ALIVE`; docs-only; no cargo; no crown absorb; no
ManagePullRequest.
