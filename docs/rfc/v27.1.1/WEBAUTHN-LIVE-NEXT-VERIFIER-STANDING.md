# Live WebAuthn / FIDO2 (CCS clause 2) — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | tip-wins absorb of `cursor/v2711-webauthn-retip2-bc54` @ `7ac7fee56` via `cursor/v2711-bodies-tip-absorb-1d17` (prior tip-wins absorb `cursor/v2711-webauthn-next-bc54` @ `15854bbe2` → crown `33a5420ad` / `a7e190400` retained in lineage) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 Clause 2 — CCS / BRCE + WebAuthn AS-01 |
| Companions | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 V7–V9; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md); [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) CCS #1; [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-4** (→ **G2-3**/**G2-5**); [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) |
| Observed ggen tip (retip2 base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `bebea5dd0d4c9af533457e04e0ed068ae8844f69` (`bebea5dd0`; ⊇ HITL retip2 BLOCKED_HITL absorb + zcode refusal ALIVE + OOR retip14) |
| Prior tip observation | Sidebranch probe tip `d0fec2f2f`; tip-wins absorb `33a5420ad` on `a7e190400`; retip2 re-pins honesty on `bebea5dd0+` — live WebAuthn remains **UNSUPPORTED** |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty pin / retip2) |
| Status | LEDGER — live/production WebAuthn ceremony **UNSUPPORTED**; BRCE **V8** **BLOCKED** ahead of **V9**; **not** CONSTRUCT→DO / Plane-5 / live-crypto `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for live
WebAuthn / FIDO2 production (CCS clause 2 / OOR **G2-4**) so a later crown absorb
can cite one closed ceremony + falsifier set. It does **not** rewrite Gate-1 / OOR
tip-pin headers. It does **not** promote V7 fixture exit 0, V1 seat fence tip-cargo,
or CCS #2/#3 manufacture gates to live-crypto / broker / Plane-5 `ALIVE`.

---

## 1. Standing pin (honesty)

| Claim | Standing | Evidence (held; not re-run this land) |
|-------|----------|---------------------------------------|
| V7 ES256 / `:prime256v1` fixture court | Fixture land **ALIVE** @ `fb6e7fc6e` (**4 passed; exit 0**); path **PARTIAL_ALIVE** (crypto shape) | `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` — **≠** live ceremony |
| V1 ungated-DO seat fence | Exact tip-cargo **ALIVE** (**6** @ `e7569893a`) | `… --test brce_as01_seat_court` — fence ≠ broker |
| CCS #2 `write_apply` / CCS #3 `construct_hash_gate` | tip-cargo **ALIVE** (manufacture) | ≠ Plane-5 DO / AS-01 live |
| `webauthn` / `web-authn` / `passkeys` RP deps in ggen | **ABSENT** (`rg` free; `p256` only in `ggen-abb-sbb` for V7) | V5 companion |
| Full clientDataJSON / authenticatorData / RP-ID bind | **ABSENT** in ggen | V7 verifies challenge+SEC1+ES256 only |
| Hardware authenticator enrollment / attestation | **ABSENT** | out of ggen |
| **Live WebAuthn / FIDO2 RP + hardware ceremony** | **UNSUPPORTED** | this pin — §3 |
| Live BRCE broker (admit→actuate→receipt) | **BLOCKED** (**V8**) | after §3 ceremony; owner **BRCE** |
| Plane-5 front-door DO | **BLOCKED** (**V9** / OOR **G2-5**) | after **V8** |
| Clause-2 crown / CCS #1 AS-01 slice | **PARTIAL_ALIVE** | fences **ALIVE**; live **UNSUPPORTED** / broker **BLOCKED** |

---

## 2. Owner / HITL

| Role | Owner |
|------|-------|
| Exact next live WebAuthn converge | **Founder HITL** — admit RP + run hardware FIDO2 ceremony (agent cannot invent authenticator / RP secrets) |
| Execution surface | **BRCE** / trust-plane (mounted BRCE broker checkout or documented RP host) — **out of ggen** |
| In-repo fence / fixture (non-substitute) | **ggen** — V1 seat + V7 ES256 courts only |
| OOR row | **G2-4** ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)); unlocks **G2-3** (**V8**) then **G2-5** (**V9**) |
| Slice ledgers | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2 |

**Sequencing law (held):** live WebAuthn ceremony **UNSUPPORTED** → (§3 Founder HITL) → **V8** broker **BLOCKED** → **V9** Plane-5. Do **not** skip to V8/V9 `ALIVE` without §3 exit **0**.

---

## 3. Exact next verifier (absorb text)

**Falsifier for live WebAuthn / FIDO2 production (CCS clause 2 / G2-4):** the
ceremony court below must exit **0** with typed admit + refuse legs. Until then
standing stays **UNSUPPORTED** — **not** `ALIVE`.

```bash
# Founder HITL — live WebAuthn/FIDO2 AS-01 production ceremony
# Owner: BRCE / trust-plane (out of ggen). Agent must not invent RP or authenticator material.
#
# Preconditions (admit — do not invent in ggen):
#   1) Mount BRCE broker / trust-plane checkout that owns the WebAuthn RP (or documented RP host).
#   2) Admit RP-ID + origin for AS-01 human seat under ES256 / :prime256v1 (COSE alg -7).
#   3) Enroll hardware authenticator (platform or roaming) with attestation bound to that RP-ID.
#   4) Issue a fresh challenge; collect live assertion:
#        clientDataJSON + authenticatorData + signature (ECDSA P-256 / ES256).
#
# Exact falsifier court (in mounted BRCE/trust-plane — NOT ggen V7 fixture):
#   A) Live assertion with verified AS-01 seat (RP-ID + challenge + ES256) → admit
#      + typed execution-bound seat receipt → exit 0
#   B) Tampered signature / wrong challenge → REFUSED_AS01_SIGNATURE_INVALID (or peer typed refuse)
#   C) Missing/wrong RP-ID bind OR incomplete clientDataJSON/authenticatorData → REFUSED_*
#   D) alg ≠ ES256 OR curve ≠ prime256v1 → REFUSED_AS01_ALG (or peer)
#
# Pass: A–D all typed; process exit 0 on the known-good live path.
# Until then: UNSUPPORTED — live/production WebAuthn ceremony absent.
#
# Antifalsifiers (do NOT promote from these):
#   - cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court   # V7 fixture ≠ live ALIVE
#   - cargo test -p ggen-abb-sbb --test brce_as01_seat_court               # V1 fence ≠ live ALIVE
#   - SovereignLease Ed25519 / EdDSA                                      # ≠ AS-01 prime256v1
#   - CCS #2 write_apply / CCS #3 construct_hash_gate tip-cargo           # manufacture ≠ live crypto
#   - Claiming V8 broker ALIVE before this ceremony                       # REFUSED_UNGROUNDED
#
# After §3 exit 0 only — next ladder (still BLOCKED until those courts pass):
#   V8 (G2-3): mounted BRCE broker — intent without AS-01 → REFUSED_*;
#              with verified seat → execution receipt + replay digest → exit 0
#   V9 (G2-5): revops/cos readyz ladder + host court with AS-01 → BRCE receipt
```

Owner artifact for V8/V9 wording: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5.
Plane-5 probe detail: [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-5**.

---

## 4. Explicit non-claims

- This side branch does **not** push `cursor/ecosystem-spec-27-1-1-bc54`.
- This land does **not** claim live WebAuthn, BRCE **V8**, Plane-5 **V9**, Foundry finish, or zero-HITL `ALIVE`.
- V7 fixture / V1 seat fence tip-cargo do **not** close **G2-4** production.
- Do not absorb this ledger as crown `ALIVE` — absorb only after the §3 ceremony court exits **0**, or retain **UNSUPPORTED** with this pin.
- **V8** remains **BLOCKED** ahead of **V9** even after docs land; ceremony pin ≠ broker pin.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — side `cursor/v2711-webauthn-next-bc54` on observed tip `d0fec2f2f` (⊇ `00b53cec9`); docs-only; live WebAuthn **UNSUPPORTED** held; **V8** **BLOCKED** ahead of **V9**; no cargo; no ManagePullRequest; no crown push |
| Tip-wins absorb | 2026-11-22 — `33a5420ad` onto crown `a7e190400`; standing **UNSUPPORTED** retained |
| Prior evidence chain | BRCE seat fence tip-cargo **ALIVE** @ `e7569893a` → V7 ES256 fixture land @ `fb6e7fc6e` → CCS-BRCE residual honesty → OOR G2-4 production still **UNSUPPORTED** |
| Absorb guidance | Tip-wins later onto crown when safe; cite §3 verbatim; standing **UNSUPPORTED** until ceremony exit **0** — do **not** invent live-crypto / V8 / Plane-5 `ALIVE` |
| Re-pin retip2 | 2026-11-22 — `cursor/v2711-webauthn-retip2-bc54` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `bebea5dd0` (⊇ HITL retip2 BLOCKED_HITL); `bc-83ab488f`; docs-only honesty re-pin; live WebAuthn **UNSUPPORTED** held; **V8** **BLOCKED**; exact next §3 Founder HITL ceremony; **no** invented live-crypto / V8 / Plane-5 `ALIVE`; sidebranch push only; no ManagePullRequest; **no** crown push |
| Tip-wins absorb (retip2) | 2026-11-22 — `cursor/v2711-bodies-tip-absorb-1d17` absorbs `7ac7fee56` onto crown tip `59861d48a` (⊇ bodies-next BLOCKED + OOR retip15 + curve + zcode refusal); standing remains **UNSUPPORTED**; **V8** **BLOCKED**; **no** invented live-crypto / V8 / Plane-5 `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Honesty retip (V8 companion) | 2026-11-22 — `cursor/v2711-brce-v8-retip-bc54` @ `7a7581393` from crown tip `893070836` (`bc-4de06fe4`); live WebAuthn remains **UNSUPPORTED**; **V8** **BLOCKED**; bodies **BLOCKED**; contract **ALIVE** retained; cite §3 verbatim; **no** false live-crypto / V8 `ALIVE`; sidebranch only at pin time |
| Tip-wins absorb (V8 retip companion) | 2026-11-22 — `cursor/v2711-brce-v8-retip-absorb-08f2` tip-wins absorb of `7a7581393` onto crown tip `2bee92cd6` (⊇ HITL action-card + perimeter path); standing remains **UNSUPPORTED**; **V8** **BLOCKED**; bodies **BLOCKED**; contract **ALIVE** retained; **no** invented live-crypto / V8 `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |

---

## 6. Re-pin retip2 (2026-11-22) — `cursor/v2711-webauthn-retip2-bc54`

| Field | Value |
|-------|-------|
| Base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `bebea5dd0d4c9af533457e04e0ed068ae8844f69` (`bebea5dd0+`; tip-wins HITL retip2 BLOCKED_HITL + zcode refusal ALIVE) |
| Branch | `cursor/v2711-webauthn-retip2-bc54` |
| Agent | `bc-83ab488f` (`https://cursor.com/agents/bc-83ab488f-d17b-5914-a8e4-03dbd7d630b8`) |
| UTC | `2026-11-22T16:47:01Z` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty re-pin) |

### Observed (this retip2)

| Probe | Result |
|-------|--------|
| `webauthn` / `web-authn` / `passkeys` RP deps (`rg` Cargo.toml / crates/*/Cargo.toml / Cargo.lock) | **ABSENT** (`rg` exit **1**) |
| `p256` in `crates/ggen-abb-sbb` (V7 only) | **PRESENT** |
| `as01_assertion.rs` / `brce_as01_es256_assertion_court` | **PRESENT** (V7 fixture path; ≠ live) |
| Hardware authenticator / live RP ceremony | **ABSENT** (out of ggen; Founder HITL) |
| Live BRCE broker mount | **ABSENT** → **V8** **BLOCKED** |

### Standing (this retip2)

| Claim | Standing |
|-------|----------|
| Live WebAuthn / FIDO2 RP + hardware ceremony (G2-4) | **UNSUPPORTED** — **not** promoted; do **not** invent `ALIVE` |
| BRCE **V8** broker (G2-3) | **BLOCKED** (broker unmounted; after §3) |
| Plane-5 **V9** (G2-5) | **BLOCKED** (after **V8**) |
| Exact next (unchanged) | Founder HITL §3 ceremony court → exit **0** (admit RP-ID → enroll hardware → live assertion A–D) |

**Exact next verifier one-liner (Founder HITL):** admit RP-ID + enroll hardware → live assertion court (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered / missing RP-ID / alg≠ES256 → `REFUSED_*` (full falsifier: §3).

**Antifalsifiers held:** V7 fixture / V1 seat fence / SovereignLease Ed25519 / CCS #2/#3 tip-cargo ≠ live WebAuthn `ALIVE`. Tip-wins absorb onto crown via `cursor/v2711-bodies-tip-absorb-1d17` — standing **not** promoted (**UNSUPPORTED**; **V8** **BLOCKED**).

---

## 7. Honesty retip (2026-11-22) — companion of `cursor/v2711-brce-v8-retip-bc54`

| Field | Value |
|-------|-------|
| Base (retip) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `893070836ef83ab711e657cfacc98bb4dc7ea369` (`893070836`) |
| Sidebranch | `cursor/v2711-brce-v8-retip-bc54` @ `7a7581393` |
| Absorb branch | `cursor/v2711-brce-v8-retip-absorb-08f2` onto crown tip `2bee92cd6` (`bc-172dc079`) |
| Agent (retip) | `bc-4de06fe4` |
| UTC | `2026-11-22T17:09:58Z` (retip); absorb `2026-11-22T17:15:00Z` |

### Standing (this retip / tip-wins absorb) — no false `ALIVE`

| Claim | Standing |
|-------|----------|
| Live WebAuthn / FIDO2 RP + hardware ceremony (G2-4) | **UNSUPPORTED** |
| BRCE **V8** broker (G2-3) | **BLOCKED** until broker admit |
| R-01…R-07 *bodies* | **BLOCKED** |
| R-01…R-07 *contract* | tip-cargo **ALIVE** retained (**7** @ `e7569893a`) |

**Exact next (unchanged §3):** Founder HITL admit RP-ID → enroll hardware → live assertion court (`clientDataJSON` + `authenticatorData` + ES256/`prime256v1`) — known-good → exit **0**; tampered / missing RP-ID / alg≠ES256 → `REFUSED_*`. Then **V8** broker court ([`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3 / §6). Tip-wins absorb via `cursor/v2711-brce-v8-retip-absorb-08f2` — standing **not** promoted.
