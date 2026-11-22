# Live WebAuthn / FIDO2 (CCS clause 2) — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | `cursor/v2711-webauthn-next-bc54` (sidebranch only — **no** crown push) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 Clause 2 — CCS / BRCE + WebAuthn AS-01 |
| Companions | [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 V7–V9; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md); [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) CCS #1; [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-4** (→ **G2-3**/**G2-5**) |
| Observed ggen tip (read-only) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d0fec2f2f2a539852e3977216b8057e1d5b2bb8a` (`d0fec2f2f`; ⊇ igniter clean-room reconfirm + HITL `--live` pin + Smalt residual `00b53cec9`) |
| Prior tip observation | First read-only fetch this land: `00b53cec9134e97cc2967bfd61af9a156e7defa7` — crown advanced before sidebranch land; pin retargeted to live tip **without** pushing crown |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty pin) |
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
| Prior evidence chain | BRCE seat fence tip-cargo **ALIVE** @ `e7569893a` → V7 ES256 fixture land @ `fb6e7fc6e` → CCS-BRCE residual honesty → OOR G2-4 production still **UNSUPPORTED** |
| Absorb guidance | Tip-wins later onto crown when safe; cite §3 verbatim; standing **UNSUPPORTED** until ceremony exit **0** — do **not** invent live-crypto / V8 / Plane-5 `ALIVE` |
