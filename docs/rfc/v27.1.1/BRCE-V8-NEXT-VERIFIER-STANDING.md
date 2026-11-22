# BRCE V8 broker — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `BRCE-V8-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | `cursor/v2711-brce-v8-next-bc54` (honesty sidebranch; **not** crown) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 Clause 2 — BRCE broker admits intent only with verified AS-01 seat |
| Companions | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V8**; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2; [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3; [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-3** |
| Observed ggen tip (side base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f654343155c902a003a852f61df345e986d98b5e` (`f65434315`; ⊇ `098f53ea8`) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty pin) |
| Status | LEDGER — **V8** broker **BLOCKED** (broker unmounted); R-01…R-07 bodies **BLOCKED** ahead of V8; live WebAuthn **UNSUPPORTED**; **no** V8 / body / live-crypto `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for BRCE
**V8** (OOR **G2-3**) so a later crown absorb can cite one closed broker court.
It does **not** push crown. It does **not** promote V1 seat fence tip-cargo, V7
fixture exit 0, CCS #2/#3 manufacture gates, or R-01…R-07 *contract* tip-cargo to
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

- This side branch does **not** push `cursor/ecosystem-spec-27-1-1-bc54`.
- This land does **not** claim BRCE **V8** `ALIVE`, live WebAuthn `ALIVE`, Plane-5 **V9** `ALIVE`, or R-01…R-07 body `ALIVE`/`CLOSED`.
- V1 seat / V7 fixture / R01-*contract* tip-cargo do **not** close **G2-3**.
- Do not absorb this ledger as crown `ALIVE` — absorb only after the §3 broker court exits **0**, or retain **BLOCKED** with this pin.
- R-01…R-07 bodies remain **BLOCKED** ahead of V8; contract fence alone is **REFUSED_UNGROUNDED** as body closure.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — side `cursor/v2711-brce-v8-next-bc54` on observed tip `f65434315` (⊇ `098f53ea8`); docs-only; **V8** **BLOCKED** (broker unmounted); R-01…R-07 bodies **BLOCKED** ahead of V8; live WebAuthn **UNSUPPORTED**; no cargo; no ManagePullRequest; no crown push |
| Prior evidence chain | BRCE seat fence tip-cargo **ALIVE** @ `e7569893a` → V7 ES256 fixture land @ `fb6e7fc6e` → CCS-BRCE / BRCE-WEBAUTHN residual honesty (V8 **BLOCKED**) → WebAuthn live next-verifier (**UNSUPPORTED**) → OOR **G2-3** broker still open |
| Absorb guidance | Tip-wins later onto crown when safe; cite §3 verbatim; standing **BLOCKED** until broker court exit **0** — do **not** invent V8 / body / live-crypto `ALIVE` |
