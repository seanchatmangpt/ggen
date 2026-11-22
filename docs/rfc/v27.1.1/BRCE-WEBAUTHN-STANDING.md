# BRCE + WebAuthn Seat AS-01 (`:prime256v1`) — Standing Record

| Field | Value |
|-------|-------|
| Document ID | `BRCE-WEBAUTHN-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §1.1 / Plane 5 |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f5f44571f9a331d46e2895657621b7efd7127f1b` (OOR retip11 on `6a3a623ba`; seat exact tip-cargo evidence `e7569893a`) |
| Tip pin | 2026-11-22 — residual honesty `cursor/v2711-brce-v8-bodies-bc54` on crown `f5f44571f`: V1 seat fence tip-cargo **ALIVE** retained (**6 passed; exit 0** @ `e7569893a`; [`BRCE-AS01-SEAT-TIP-CARGO-STANDING.md`](./BRCE-AS01-SEAT-TIP-CARGO-STANDING.md)); V7 fixture land retained @ `fb6e7fc6e`. Live WebAuthn crypto **UNSUPPORTED**; broker **V8** **BLOCKED** (exact next §5; owner **BRCE**; Founder HITL); Plane-5 **V9** **BLOCKED**. **No** invented V8 `ALIVE`. |
| Live filename | `BRCE-WEBAUTHN-STANDING.md` (AS-01 / V1–V11); Clause-2 crown binder [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) |
| Target GA | **2027-01-01** |
| Slice standing | **PARTIAL_ALIVE** (doctrine + V1 presence fence + **V7 fixture crypto-shape**); live BRCE broker / live WebAuthn ceremony / Plane-5 DO still **BLOCKED** / **UNSUPPORTED** — never claim Plane-5 `ALIVE` from this land |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This record is inspection + typed fence evidence. It does **not** claim a live
WebAuthn ceremony, BRCE broker runtime, or Plane-5 DO path as `ALIVE`.

---

## 0. Honesty matrix (residual honesty @ `f5f44571f` / `6a3a623ba+`)

| Bucket | Subjects | Standing | Exact next verifier |
|--------|----------|----------|---------------------|
| **ALIVE (fixture / tip-cargo courts)** | ABB/SBB `unauthorized_do_request_is_refused`; SovereignLease Ed25519 admit; V7 ES256/`prime256v1` fixture court land **4 passed; exit 0** @ `fb6e7fc6e`; V1 seat fence exact tip-cargo **6 passed; exit 0** @ `e7569893a` | Fixture / kernel / seat fence only | Replay: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court`; `… --test brce_as01_seat_court`; `… --test falsifiers unauthorized_do_request_is_refused`; `cargo test -p ggen-sjira --test lease_epoch_test` |
| **PARTIAL_ALIVE** | Whole AS-01 CONSTRUCT→DO slice; doctrine / CI broker fences; V7 crypto-shape path (fixture ≠ hardware) | Path ceiling | Seat fence **ALIVE** ≠ broker **ALIVE**; do not promote to live |
| **BLOCKED** | Live BRCE broker (admit→actuate→receipt) — **V8** | Broker unmounted | **V8** — In mounted BRCE broker checkout: intent without AS-01 → `REFUSED_*`; with verified seat → execution receipt + replay digest → exit **0** → then **V9**/G2-5. Owner: **BRCE**. **HITL:** Founder mounts broker + live WebAuthn before V8 `ALIVE`. In-repo seat/V7 fences do **not** close. OOR **G2-3** |
| **UNSUPPORTED** | Live WebAuthn / FIDO2 RP + hardware ceremony; full clientDataJSON / authenticatorData / RP-ID bind; Plane-5 DO from ggen alone | No RP deps; V7 is fixture ECDSA only | Production RP/hardware court (OOR G2-4 beyond fixture) + **V9** / OOR **G2-5** Plane-5 probe ladder |

---

## 1. Mandate (from ECOSYSTEM-SPEC-27.1.1)

```text
SELECT ≠ CONSTRUCT ≠ DO
```

Sole lawful **CONSTRUCT → DO** gate: **BRCE + WebAuthn AS-01**.
Raw input, model output, generated code, proof text, hooks, and semantic
derivations have **no ambient execution authority**.

AS-01 seat cryptography named by the mandate: **ECDSA P-256 / `:prime256v1`**
(COSE/WebAuthn `ES256`). Distinct from in-repo Ed25519/`EdDSA` lease signing.

---

## 2. In-repo inventory (exists)

| Subject | Paths / evidence | Standing | Notes |
|---------|------------------|----------|-------|
| BRCE-only DO doctrine | `AGENTS.md` §3; `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md`; TAI / Fortune5 / GBB standards | **PARTIAL_ALIVE** | Search + CI grep/jq fences; not a broker runtime |
| `REQUIRED_BROKER = "BRCE"` | `crates/ggen-architecture/src/lib.rs`; `canonical.rs`; `self_play.rs`; certification standards | **PARTIAL_ALIVE** | Nested workspace; manufactures `ArchitectureIntent` addressed to BRCE; **does not actuate** |
| BRCE intent CI fences | `.github/workflows/{gbb-kernel,gbb-certification,fortune5-architecture,tai-enterprise-rebuild,ontostar-mustar-powlv2-agents,non-llm-safe-ea-self-play}.yml` | **PARTIAL_ALIVE** | Assert `requiredBroker`/`REQUIRED_BROKER`/`directActuation == false` |
| Pack / sync BRCE ceiling | `crates/ggen-engine/src/pack.rs`, `pack_scope.rs`; `docs/pack_capabilities_guide.md` | **PARTIAL_ALIVE** | Caps never grant DO / bypass BRCE |
| ABB/SBB refuses DO authority | `crates/ggen-abb-sbb` `Refusal::AuthorityExceeded` / `unauthorized_do_request_is_refused` | **ALIVE** (CONSTRUCT ceiling fixture) | Authority clamp ≠ AS-01 seat |
| SovereignLease (Ed25519 / `EdDSA`) | `crates/ggen-sjira/src/lease.rs` + `execute` lease gate; CLI `sjira` | **ALIVE** (lease admit fixture) | Multi-party Ed25519; **not** WebAuthn AS-01 / `prime256v1` |
| Ed25519 sync receipts | `crates/ggen-engine` pack install signing; `ggen-mcp` receipt verify when sig present | **PARTIAL_ALIVE** | Receipt integrity; not seat DO gate |
| `ed25519-dalek` 3.x / `curve25519-dalek` 4.x+5.x | workspace `Cargo.toml` / `Cargo.lock`; [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | **PARTIAL_ALIVE** | Pin doc PRESENT; Gate-1 policy court still open |
| WebAuthn class in ontologies | `marketplace/packages/identity-access-management/ontology/identity_access_management.ttl`; autofde registry | **UNSUPPORTED** (runtime) | Catalogue / registry labels only |
| Ungated-DO-without-seat typed refusal (this slice) | `crates/ggen-abb-sbb/src/as01_seat.rs` + `tests/brce_as01_seat_court.rs` | **ALIVE** (exact tip-cargo fence **6** @ `e7569893a`) / fixture land @ `0478fde59` | Presence/shape fence tip-cargo **ALIVE**. Fence ≠ broker **V8** / Plane-5 `ALIVE`. |
| AS-01 ES256/`prime256v1` assertion fixture court (V7) | `crates/ggen-abb-sbb/src/as01_assertion.rs` + `tests/brce_as01_es256_assertion_court.rs`; admitted `p256` dep in `ggen-abb-sbb` | **PARTIAL_ALIVE** (crypto-shape path) / fixture court land **ALIVE** @ `fb6e7fc6e` (**4 passed; exit 0**) | Known-good fixture admits; tampered → `REFUSED_AS01_SIGNATURE_INVALID`; alg≠ES256 → `REFUSED_AS01_ALG`. **Not** hardware ceremony / RP / broker / Plane-5 `ALIVE`. Live crypto remains **UNSUPPORTED**. |

---

## 3. Out-of-repo / absent (BLOCKED or UNSUPPORTED)

| Subject | Standing | Why |
|---------|----------|-----|
| WebAuthn / FIDO2 ceremony (navigator credentials, authenticator, RP) | **UNSUPPORTED** | No `webauthn` / `web-authn-rs` / `passkeys` RP; V7 is fixture ECDSA only |
| Full WebAuthn assertion (clientDataJSON / authenticatorData / RP ID bind) | **UNSUPPORTED** | V7 adapter verifies challenge+SEC1+ES256 only — not FIDO2 ceremony |
| Live BRCE broker (admit intent → actuate → execution receipt) | **BLOCKED** / **UNKNOWN** | Doctrine + intent manufacture only; broker realization not mounted in this checkout — see §5 **V8** |
| Plane 5 front doors (revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat) DO | **BLOCKED** / **UNSUPPORTED** in ggen | ECOSYSTEM-SPEC: out of ggen DO; exact next §5 **V9** + OOR **G2-5** — must not claim `ALIVE` from this repo |
| Equating SovereignLease Ed25519 with AS-01 P-256 | **REFUSED_UNGROUNDED** | Different algorithm, purpose, and authority surface |
| Equating V7 fixture exit 0 with live WebAuthn crypto | **REFUSED_UNGROUNDED** | Fixture ECDSA ≠ RP/hardware ceremony |

---

## 4. What this slice seals (and what it does not)

**Sealed (PARTIAL_ALIVE path / ALIVE fixture courts):**

- Standing census of BRCE / AS-01 / lease / ed25519 vs WebAuthn/`prime256v1`.
- A Chicago court that refuses **ungated DO** when no AS-01 seat claim is
  presented, and refuses wrong broker / wrong declared alg-curve shape.
- Declared seat constants: broker `BRCE`, alg `ES256`, curve `prime256v1`.
- V7 fixture court: ECDSA P-256 (`p256`) verifies known-good ES256 assertion;
  refuses tampered signatures and non-ES256 alg (**crypto shape** only).

**Explicitly not sealed:**

- Full WebAuthn / FIDO2 ceremony (clientDataJSON, authenticatorData, RP ID).
- Hardware authenticator enrollment, attestation, or RP ID binding.
- BRCE broker actuation or Plane-5 execution receipts.
- Promotion of SovereignLease Ed25519 to AS-01 standing.
- Promotion of V7 fixture exit 0 to Plane-5 / CONSTRUCT→DO / live-crypto `ALIVE`.

---

## 5. Exact next verifiers through 2027-01-01 GA

Replay commands are the falsifiers. Exit 0 is required before promoting the
named claim.

### Now → Gate 1 close (≤ 2026-12-18)

| # | Claim to advance | Exact verifier | Owner |
|---|------------------|----------------|-------|
| V1 | Ungated DO without seat remains refused (presence fence) | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` (equiv. `--lib as01_seat`) | **ggen** |
| V2 | Architecture intents still address BRCE only | `cargo test --manifest-path crates/ggen-architecture/Cargo.toml autonomics_emit_brce_intents_without_actuation` | **ggen** |
| V3 | ABB/SBB still refuses DO authority | `cargo test -p ggen-abb-sbb --test falsifiers unauthorized_do_request_is_refused` | **ggen** |
| V4 | SovereignLease remains Ed25519/`EdDSA` (non-AS-01) | `cargo test -p ggen-sjira --test lease_epoch_test` | **ggen** |
| V5 | No silent `p256`/WebAuthn dep without docs | `rg -n 'p256|webauthn|web-authn|passkeys' Cargo.toml crates/*/Cargo.toml Cargo.lock` → empty **or** documented admit (this slice: `p256` in `crates/ggen-abb-sbb/Cargo.toml` for V7 only; no `webauthn`/`passkeys` RP) | **ggen** |
| V6 | Gate-1 `curve25519-dalek` pin/docs court | pin doc [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) PRESENT; next: policy court refusing undocumented major (see pin standing §5 / `GATE-1-GAP-MATRIX.md`) | **ggen** (+ cgap-aoq) |
| V7 | WebAuthn AS-01 assertion verifies under `ES256`/`prime256v1` | `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` — known-good → admit; tampered → `REFUSED_AS01_SIGNATURE_INVALID`; alg≠ES256 → `REFUSED_AS01_ALG` | **ggen** (in-repo adapter; crypto-shape **PARTIAL**; fixture land exit 0 — **not** hardware/Plane-5 ALIVE; live ceremony still **UNSUPPORTED**) |

### Gate 2–3 (post Gate 1 → GA 2027-01-01)

| # | Claim to advance | Standing now | Exact next verifier | Owner |
|---|------------------|--------------|---------------------|-------|
| V8 | BRCE broker admits intent only with verified AS-01 seat | **BLOCKED** (broker unmounted) | In mounted BRCE broker checkout: intent **without** verified AS-01 seat → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit **0**. In-repo fence `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` / V7 fixture court do **not** close this row. **HITL:** Founder mounts broker runtime + live WebAuthn AS-01 ceremony before claiming V8 `ALIVE`. | **BRCE** (out of ggen unless adapter mounted); Founder HITL |
| V9 | Plane 5 front-door DO path | **BLOCKED** | Probe ladder (all required): (a) OOR **G2-1** `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200**; (b) OOR **G2-2** `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); (c) external host court: request **without** AS-01 → refuse; **with** AS-01 → BRCE receipt for the named front-door change ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6; OOR **G2-5**). Never promote from ggen fence alone. **HITL:** after **V8**. | Plane 5 / revops / cos; Founder HITL |
| V10 | R-01…R-07 closed | **BLOCKED** (bodies) | Admit sovereign risk-register bodies (no invention) → per-ID typed receipt **or** typed `REFUSED_*` before GA (ECOSYSTEM-SPEC §11; OOR **G3-5**; [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2). Contract fence `r01_r07_resolution_court` does **not** close. **HITL:** Founder admits register. | multi-repo risk-register owner; Founder HITL |
| V11 | GA crown | open | aggregate standing: V1–V10 green; no ungated DO; F=0 | release governance |

### Promotion law

| From | To | Requires |
|------|-----|----------|
| PARTIAL_ALIVE (this doc + V1) | PARTIAL_ALIVE (crypto shape) | V7 fixture court without claiming hardware ALIVE — **landed** (`fb6e7fc6e`; ancestor of tip `d05a131c`) |
| PARTIAL_ALIVE (crypto shape) | ALIVE (seat verify on tip) | V7 + tip-cargo replay exit 0 (still open this retip — cargo not run) |
| ALIVE (seat) | ALIVE (CONSTRUCT→DO) | V8 broker receipt on exact tip |
| any | Plane 5 ALIVE | V9 — **never** from ggen-only evidence |
| any | Live WebAuthn ceremony ALIVE | RP/hardware court — V7 fixture alone is **REFUSED_UNGROUNDED** |

---

## 6. Companion artifacts

| Path | Role |
|------|------|
| `docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md` | mandate / planes / GA |
| `docs/rfc/v27.1.1/REPO-INVENTORY-27.1.1.md` | census |
| `docs/rfc/v27.1.1/GATE-1-GAP-MATRIX.md` | Gate-1 verifiers |
| `crates/ggen-abb-sbb/src/as01_seat.rs` | presence-only seat gate |
| `crates/ggen-abb-sbb/tests/brce_as01_seat_court.rs` | Chicago falsifier (V1) |
| `crates/ggen-abb-sbb/src/as01_assertion.rs` | V7 ES256/`prime256v1` assertion verify (crypto shape) |
| `crates/ggen-abb-sbb/tests/brce_as01_es256_assertion_court.rs` | Chicago falsifier (V7) |
| [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | G2-3/G2-4/G2-5 live broker / WebAuthn / Plane-5 DO |
| [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) | ~8 CCS ledger (CCS-1 / CCS-3 / CCS-8) |
| [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) | Clause-2 crown honesty binder (tip pin + next V8→V9/G2-5) |

---

## 7. Slice receipt (this land / retip)

| Claim | Standing |
|-------|----------|
| Doctrine BRCE-only DO in-repo | **PARTIAL_ALIVE** |
| SovereignLease Ed25519 | **ALIVE** (lease fixture) / **UNSUPPORTED** as AS-01 |
| ABB/SBB DO-authority refuse | **ALIVE** (CONSTRUCT ceiling fixture) |
| WebAuthn AS-01 `:prime256v1` fixture verify (V7) | Fixture court land **ALIVE** @ `fb6e7fc6e` (**4 passed; exit 0**); path **PARTIAL_ALIVE** (crypto shape) — not hardware / RP ceremony |
| Live WebAuthn / FIDO2 crypto | **UNSUPPORTED** |
| Live BRCE broker / Plane 5 DO | **BLOCKED** — exact next §5 **V8**/**V9** + OOR **G2-3**/**G2-5** (Founder HITL; **no** V8 `ALIVE`) |
| Ungated DO without seat typed refusal | V1 seat fence exact tip-cargo **ALIVE** (**6** @ `e7569893a`); path still ≠ broker |
| R-01…R-07 bodies (V10) | **BLOCKED** — OOR **G3-5**; contract fence ≠ body closure |
| Whole AS-01 CONSTRUCT→DO path | **PARTIAL_ALIVE** → next falsifiers **V8–V11** + OOR **G2-3/G2-5** (G2-4 production still **UNSUPPORTED** beyond fixture) through GA |
| Observed tip | residual honesty `cursor/v2711-brce-v8-bodies-bc54` on crown `f5f44571f` (`6a3a623ba+`; docs-only; no cargo) |
