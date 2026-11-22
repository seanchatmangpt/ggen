# BRCE V8 broker — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `BRCE-V8-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | tip-wins absorb `cursor/v2711-brce-v8-retip-bc54` @ `7a7581393` onto crown tip `2bee92cd6` via `cursor/v2711-brce-v8-retip-absorb-08f2` (⊇ HITL action-card `8629691f8` + perimeter path `2bee92cd6`; prior tip-wins absorb `d90d0189f` / `8d761d8a1`) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 Clause 2 — BRCE broker admits intent only with verified AS-01 seat |
| Companions | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3; [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-3** |
| Observed ggen tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2bee92cd6827e77e4d8465dbeb834d43484eb675` (`2bee92cd6`; ⊇ HITL Founder action card §8 + perimeter→ttl/toml path honesty) |
| Prior tip observation | Sidebranch honesty retip on `893070836` @ `7a7581393` (`bc-4de06fe4`); tip-wins absorb retains **BLOCKED** / **UNSUPPORTED** honesty — **no** V8 / body / live-crypto `ALIVE` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty retip absorb) |
| Status | LEDGER — **V8** broker **BLOCKED** until broker admit; R-01…R-07 bodies **BLOCKED**; live WebAuthn **UNSUPPORTED**; R-01…R-07 *contract* tip-cargo **ALIVE** retained (**7** @ `e7569893a`); **no** V8 / body / live-crypto `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for BRCE
**V8** (OOR **G2-3**) so crown can cite one closed broker court. Tip-wins absorb
retains **BLOCKED** with this pin — it does **not** promote V1 seat fence tip-cargo,
V7 fixture exit 0, CCS #2/#3 manufacture gates, or R-01…R-07 *contract* tip-cargo to
broker / Plane-5 / body `ALIVE`.

---

## 1. Standing pin (honesty)

| Claim | Standing | Exact reason (held evidence; not invented) |
|-------|----------|--------------------------------------------|
| **V8** BRCE broker (admit→actuate→receipt) | **BLOCKED** | Broker unmounted — doctrine + intent manufacture only in ggen; no broker runtime mounted ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 V8; OOR **G2-3**: “No broker runtime mounted”) |
| **R-01…R-07 bodies** | **BLOCKED** ahead of V8 | Bodies unmounted — risk-register prose absent; contract fence ≠ body closure ([`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; OOR **G3-5** / V10). Residual body work remains open ahead of / parallel to V8 unstick — do **not** invent body `ALIVE`/`CLOSED` |
| **Live WebAuthn / FIDO2 RP + hardware** | **UNSUPPORTED** | No `webauthn` / `web-authn` / `passkeys` RP deps; V7 is fixture ECDSA only ([`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §1; OOR **G2-4**) |
| V1 ungated-DO seat fence | tip-cargo **ALIVE** (**6** @ `e7569893a`) | Fence ≠ broker — does **not** close V8 |
| V7 ES256 / `:prime256v1` fixture court | Fixture land **ALIVE** @ `fb6e7fc6e` | Fixture ≠ live ceremony / V8 |
| R-01…R-07 *contract* | tip-cargo **ALIVE** (**7** @ `e7569893a`) | Contract ≠ body closure |
| Plane-5 front-door DO (**V9** / G2-5) | **BLOCKED** | After **V8** only |
| Clause-2 crown / CCS #1 AS-01 slice | **PARTIAL_ALIVE** | Fences **ALIVE**; live **UNSUPPORTED** / broker **BLOCKED** / bodies **BLOCKED** |

---

## 2. Owner / HITL

| Role | Owner |
|------|-------|
| Exact next V8 converge | **Founder HITL** — mount BRCE broker runtime + complete live WebAuthn AS-01 ceremony first |
| Execution surface | **BRCE** (mounted BRCE broker checkout) — **out of ggen** |
| In-repo fence / fixture (non-substitute) | **ggen** — V1 seat + V7 ES256 courts only |
| R-01…R-07 body residual | Risk-register owner (multi-repo); **Founder HITL** admits register path ([`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2) |
| OOR row | **G2-3** ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)); after live WebAuthn **G2-4** / [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 |
| Slice ledgers | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2 |

**Sequencing law (held):** live WebAuthn ceremony **UNSUPPORTED** → ([`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 Founder HITL) → **V8** broker **BLOCKED** (this pin §3) → **V9** Plane-5. Do **not** claim V8 `ALIVE` without §3 exit **0**. R-01…R-07 bodies stay **BLOCKED** ahead of V8 until admitted register bodies close (OOR **G3-5**).

---

## 3. Exact next verifier (unstick V8)

**Falsifier for BRCE V8 / G2-3:** the broker court below must exit **0** with typed
refuse + admit legs on a **mounted** BRCE broker checkout. Until then standing
stays **BLOCKED** — **not** `ALIVE`.

```bash
# Founder HITL — BRCE V8 broker court (OOR G2-3)
# Owner: BRCE (out of ggen). Agent must not invent broker mount or AS-01 seat material.
#
# Preconditions (admit — do not invent in ggen):
#   1) Complete live WebAuthn / FIDO2 AS-01 ceremony first
#      (WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md §3 → exit 0).
#      Until then live WebAuthn remains UNSUPPORTED — claiming V8 ALIVE is REFUSED_UNGROUNDED.
#   2) Mount BRCE broker / trust-plane checkout that owns admit→actuate→receipt.
#
# Exact falsifier court (in mounted BRCE broker checkout — NOT ggen V1/V7 fences):
#   A) Intent WITHOUT verified AS-01 human seat → REFUSED_*
#   B) Intent WITH verified AS-01 seat → execution receipt + replay digest → exit 0
#
# Pass: A–B typed; process exit 0 on the known-good verified-seat path.
# Until then: BLOCKED — broker unmounted (doctrine + intent manufacture only in ggen).
#
# Antifalsifiers (do NOT promote from these):
#   - cargo test -p ggen-abb-sbb --test brce_as01_seat_court               # V1 fence ≠ V8 ALIVE
#   - cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court   # V7 fixture ≠ V8 ALIVE
#   - cargo test -p ggen-graph --test r01_r07_resolution_court            # contract ≠ body / V8
#   - CCS #2 write_apply / CCS #3 construct_hash_gate tip-cargo           # manufacture ≠ broker
#   - Claiming V8 ALIVE before live WebAuthn ceremony                     # REFUSED_UNGROUNDED
#
# After §3 exit 0 only — next ladder (still BLOCKED until those courts pass):
#   V9 (G2-5): revops/cos readyz ladder + host court with AS-01 → BRCE receipt
#   R-01…R-07 bodies (G3-5 / V10): Founder HITL admit risk-register → per-ID
#     typed receipt or REFUSED_* (bodies remain BLOCKED ahead of V8 until then)
```

Owner artifact wording: [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**.
Live WebAuthn precondition: [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3.
Bodies residual: [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2.

---

## 4. Explicit non-claims

- Tip-wins absorb onto crown retains **BLOCKED** / **UNSUPPORTED** honesty — it does **not** invent broker / body / live-crypto `ALIVE`.
- This land does **not** claim BRCE **V8** `ALIVE`, live WebAuthn `ALIVE`, Plane-5 **V9** `ALIVE`, or R-01…R-07 body `ALIVE`/`CLOSED`.
- V1 seat / V7 fixture / R01-*contract* tip-cargo do **not** close **G2-3**.
- Retain **BLOCKED** with this pin until the §3 broker court exits **0** — docs absorb ≠ V8 `ALIVE`.
- R-01…R-07 bodies remain **BLOCKED** ahead of V8; contract fence alone is **REFUSED_UNGROUNDED** as body closure.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Side-branch pin | 2026-11-22 — `cursor/v2711-brce-v8-next-bc54` @ `d90d0189f` on observed tip `f65434315` (⊇ `098f53ea8`); docs-only; **V8** **BLOCKED** (broker unmounted); R-01…R-07 bodies **BLOCKED** ahead of V8; live WebAuthn **UNSUPPORTED**; no cargo; no ManagePullRequest; no crown push (`bc-493b3e0e`) |
| Tip-wins absorb | 2026-11-22 — re-apply side pin onto live crown `8d761d8a1` (⊇ `f65434315` / vw-env-wire honesty retip); standing remains **V8** **BLOCKED**, R-01…R-07 bodies **BLOCKED**, live WebAuthn **UNSUPPORTED**; cite §3 verbatim; **no** invented V8 / body / live-crypto `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Prior evidence chain | BRCE seat fence tip-cargo **ALIVE** @ `e7569893a` → V7 ES256 fixture land @ `fb6e7fc6e` → CCS-BRCE / BRCE-WEBAUTHN residual honesty (V8 **BLOCKED**) → WebAuthn live next-verifier (**UNSUPPORTED**) → OOR **G2-3** broker still open |
| Honesty retip | 2026-11-22 — `cursor/v2711-brce-v8-retip-bc54` @ `7a7581393` from crown tip `893070836` (`bc-4de06fe4`); docs-only; **V8** remains **BLOCKED** until broker admit; bodies **BLOCKED**; live WebAuthn **UNSUPPORTED**; R-01…R-07 *contract* **ALIVE** retained (**7** @ `e7569893a`); cite §3 + §6 verbatim; **no** false `ALIVE`; no cargo; no ManagePullRequest; sidebranch only at pin time |
| Tip-wins absorb (retip) | 2026-11-22 — `cursor/v2711-brce-v8-retip-absorb-08f2` tip-wins absorb of `7a7581393` onto crown tip `2bee92cd6` (⊇ HITL action-card `8629691f8` + perimeter path); standing remains **V8** **BLOCKED**, bodies **BLOCKED**, live WebAuthn **UNSUPPORTED**; contract **ALIVE** retained; cite §3 + §6 verbatim; **no** invented V8 / body / live-crypto `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |

---

## 6. Honesty retip (2026-11-22) — `cursor/v2711-brce-v8-retip-bc54`

| Field | Value |
|-------|-------|
| Base (retip) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `893070836ef83ab711e657cfacc98bb4dc7ea369` (`893070836`) |
| Sidebranch | `cursor/v2711-brce-v8-retip-bc54` @ `7a7581393` |
| Absorb branch | `cursor/v2711-brce-v8-retip-absorb-08f2` onto crown tip `2bee92cd6` (`bc-172dc079`) |
| Agent (retip) | `bc-4de06fe4` (`https://cursor.com/agents/bc-4de06fe4-dbbe-5cad-a7a8-ec7ac6693888`) |
| UTC | `2026-11-22T17:09:58Z` (retip); absorb `2026-11-22T17:15:00Z` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty retip absorb) |

### Observed (this retip)

| Probe | Result |
|-------|--------|
| Live BRCE broker mount (admit→actuate→receipt) in ggen | **ABSENT** → **V8** **BLOCKED** (doctrine + intent manufacture + V1/V7 fences only) |
| `webauthn` / `web-authn` / `passkeys` RP deps (`rg` Cargo.toml / crates/*/Cargo.toml / Cargo.lock) | **ABSENT** (`rg` exit **1**) → live WebAuthn **UNSUPPORTED** |
| `p256` in `crates/ggen-abb-sbb` (V7 fixture only) | **PRESENT** |
| `brce_as01_seat_court` / `brce_as01_es256_assertion_court` | **PRESENT** (V1/V7 fences; ≠ broker / live ceremony) |
| `r01_r07_resolution_court` | **PRESENT** — *contract* tip-cargo **ALIVE** retained (**7** @ `e7569893a`); ≠ body closure |
| Sovereign risk-register R-01…R-07 bodies | **ABSENT** / unmounted → bodies **BLOCKED** |

### Standing (this retip) — no false `ALIVE`

| Claim | Standing |
|-------|----------|
| **V8** BRCE broker (G2-3) | **BLOCKED** until Founder HITL mounts broker + completes live WebAuthn AS-01 admit (§3 court exit **0**) |
| R-01…R-07 *bodies* (G3-5 / V10) | **BLOCKED** (sovereign risk-register unmounted) |
| Live WebAuthn / FIDO2 RP + hardware (G2-4) | **UNSUPPORTED** |
| R-01…R-07 *contract* fence | tip-cargo **ALIVE** retained (**7** @ `e7569893a`) — contract ≠ body / V8 |
| V1 seat fence / V7 ES256 fixture | tip-cargo / land **ALIVE** retained — fence ≠ broker `ALIVE` |
| Plane-5 **V9** (G2-5) | **BLOCKED** (after **V8**) |

### Exact next verifier commands (held; unchanged)

**V8 / G2-3 (broker admit — until then BLOCKED):**

```bash
# Founder HITL — BRCE V8 broker court (OOR G2-3)
# Preconditions: live WebAuthn §3 exit 0 first; mount BRCE broker checkout.
# A) Intent WITHOUT verified AS-01 → REFUSED_*
# B) Intent WITH verified AS-01 → execution receipt + replay digest → exit 0
# Full falsifier: §3 above. Antifalsifiers: V1/V7/R01-contract/CCS tip-cargo ≠ V8 ALIVE.
```

**Bodies / G3-5 (until then BLOCKED):**

```bash
# Founder HITL — admit sovereign risk-register → per-ID typed receipt or REFUSED_*
# In-repo contract replay only (non-substitute):
CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -- --test-threads=1
```

**Live WebAuthn / G2-4 (until then UNSUPPORTED):**

```bash
# Founder HITL — WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md §3 ceremony court → exit 0
# Antifalsifier: cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court  # ≠ live ALIVE
```

**Non-claims:** this retip / tip-wins absorb does **not** invent V8 / body / live-crypto / Plane-5 `ALIVE`. Tip-wins absorb onto crown via `cursor/v2711-brce-v8-retip-absorb-08f2` — standing **not** promoted (**V8** **BLOCKED**; bodies **BLOCKED**; WebAuthn **UNSUPPORTED**; contract **ALIVE** retained). No ManagePullRequest; no force-push.
