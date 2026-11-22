# BRCE + WebAuthn Seat AS-01 (`:prime256v1`) — Standing Record

| Field | Value |
|-------|-------|
| Document ID | `BRCE-WEBAUTHN-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §1.1 / Plane 5 |
| Observed base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1b793c12d` (2026-11-22) |
| Target GA | **2027-01-01** |
| Slice standing | **PARTIAL_ALIVE** (doctrine + presence-only ungated-DO refusal); WebAuthn/`prime256v1` verify **UNSUPPORTED** in-repo |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This record is inspection + typed fence evidence. It does **not** claim a live
WebAuthn ceremony, BRCE broker runtime, or Plane-5 DO path as `ALIVE`.

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
| BRCE-only DO doctrine | `AGENTS.md` §3; `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md`; TAI / Fortune5 / GBB standards | **PARTIAL_ALIVE** | Text + CI grep/jq fences; not a broker runtime |
| `REQUIRED_BROKER = "BRCE"` | `crates/ggen-architecture/src/lib.rs`; `canonical.rs`; `self_play.rs`; certification standards | **PARTIAL_ALIVE** | Nested workspace; manufactures `ArchitectureIntent` addressed to BRCE; **does not actuate** |
| BRCE intent CI fences | `.github/workflows/{gbb-kernel,gbb-certification,fortune5-architecture,tai-enterprise-rebuild,ontostar-mustar-powlv2-agents,non-llm-safe-ea-self-play}.yml` | **PARTIAL_ALIVE** | Assert `requiredBroker`/`REQUIRED_BROKER`/`directActuation == false` |
| Pack / sync BRCE ceiling | `crates/ggen-engine/src/pack.rs`, `pack_scope.rs`; `docs/pack_capabilities_guide.md` | **PARTIAL_ALIVE** | Caps never grant DO / bypass BRCE |
| ABB/SBB refuses DO authority | `crates/ggen-abb-sbb` `Refusal::AuthorityExceeded` / `unauthorized_do_request_is_refused` | **ALIVE** (CONSTRUCT ceiling) | Authority clamp ≠ AS-01 seat |
| SovereignLease (Ed25519 / `EdDSA`) | `crates/ggen-sjira/src/lease.rs` + `execute` lease gate; CLI `sjira` | **ALIVE** (lease admit) | Multi-party Ed25519; **not** WebAuthn AS-01 / `prime256v1` |
| Ed25519 sync receipts | `crates/ggen-engine` pack install signing; `ggen-mcp` receipt verify when sig present | **PARTIAL_ALIVE** | Receipt integrity; not seat DO gate |
| `ed25519-dalek` 3.x / `curve25519-dalek` 4.x+5.x | workspace `Cargo.toml` / `Cargo.lock` | **PARTIAL_ALIVE** | Gate-1 pin/docs court still open per inventory |
| WebAuthn class in ontologies | `marketplace/packages/identity-access-management/ontology/identity_access_management.ttl`; autofde registry | **UNSUPPORTED** (runtime) | Catalogue / registry labels only |
| Ungated-DO-without-seat typed refusal (this slice) | `crates/ggen-abb-sbb/src/as01_seat.rs` + `tests/brce_as01_seat_court.rs` | **PARTIAL_ALIVE** | Presence/shape fence only — **no crypto verify** |

---

## 3. Out-of-repo / absent (BLOCKED or UNSUPPORTED)

| Subject | Standing | Why |
|---------|----------|-----|
| WebAuthn / FIDO2 ceremony (navigator credentials, authenticator, RP) | **UNSUPPORTED** | No `webauthn` / `web-authn-rs` / `passkeys` crate; no RP server in workspace members |
| AS-01 seat verify over `:prime256v1` / ES256 | **UNSUPPORTED** | No `p256` dependency; no COSE/WebAuthn assertion verifier under live crates |
| Live BRCE broker (admit intent → actuate → execution receipt) | **BLOCKED** / **UNKNOWN** | Doctrine + intent manufacture only; broker realization not mounted in this checkout |
| Plane 5 front doors (revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat) DO | **UNSUPPORTED** in ggen | ECOSYSTEM-SPEC: out of ggen DO; must not claim `ALIVE` from this repo |
| Equating SovereignLease Ed25519 with AS-01 P-256 | **REFUSED_UNGROUNDED** | Different algorithm, purpose, and authority surface |

---

## 4. What this slice seals (and what it does not)

**Sealed (PARTIAL_ALIVE):**

- Standing census of BRCE / AS-01 / lease / ed25519 vs WebAuthn/`prime256v1`.
- A Chicago court that refuses **ungated DO** when no AS-01 seat claim is
  presented, and refuses wrong broker / wrong declared alg-curve shape.
- Declared seat constants: broker `BRCE`, alg `ES256`, curve `prime256v1`.

**Explicitly not sealed:**

- Cryptographic verification of WebAuthn assertions or P-256 signatures.
- Hardware authenticator enrollment, attestation, or RP ID binding.
- BRCE broker actuation or Plane-5 execution receipts.
- Promotion of SovereignLease Ed25519 to AS-01 standing.

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
| V5 | No silent `p256`/WebAuthn dep without docs | `rg -n 'p256|webauthn|web-authn|passkeys' Cargo.toml crates/*/Cargo.toml Cargo.lock` → empty or documented admit | **ggen** |
| V6 | Gate-1 `curve25519-dalek` pin/docs court | document admitted path under `docs/rfc/v27.1.1/` + lock grep court (see `GATE-1-GAP-MATRIX.md`) | **ggen** (+ cgap-aoq) |

### Gate 2–3 (post Gate 1 → GA 2027-01-01)

| # | Claim to advance | Exact verifier (when surface exists) | Owner |
|---|------------------|--------------------------------------|-------|
| V7 | WebAuthn AS-01 assertion verifies under `ES256`/`prime256v1` | dedicated court: known-good assertion fixture → admit; tampered → `REFUSED_*`; alg≠ES256 → refuse | BRCE / trust plane (may land adapter in ggen later) |
| V8 | BRCE broker admits intent only with verified AS-01 seat | broker integration: intent without seat → refuse; with verified seat → execution receipt with replay | **BRCE** (out of ggen unless adapter mounted) |
| V9 | Plane 5 front-door DO path | external host court: request without AS-01 → refuse; with AS-01 → BRCE receipt | Plane 5 / revops / cos |
| V10 | R-01…R-07 closed | typed receipt or typed refusal per risk register (ECOSYSTEM-SPEC §11) | multi-repo |
| V11 | GA crown | aggregate standing: V1–V10 green; no ungated DO; F=0 | release governance |

### Promotion law

| From | To | Requires |
|------|-----|----------|
| PARTIAL_ALIVE (this doc + V1) | PARTIAL_ALIVE (crypto shape) | V7 fixture court without claiming hardware ALIVE |
| PARTIAL_ALIVE | ALIVE (seat verify) | V7 + replay on CI tip |
| ALIVE (seat) | ALIVE (CONSTRUCT→DO) | V8 broker receipt on exact tip |
| any | Plane 5 ALIVE | V9 — **never** from ggen-only evidence |

---

## 6. Companion artifacts

| Path | Role |
|------|------|
| `docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md` | mandate / planes / GA |
| `docs/rfc/v27.1.1/REPO-INVENTORY-27.1.1.md` | census |
| `docs/rfc/v27.1.1/GATE-1-GAP-MATRIX.md` | Gate-1 verifiers |
| `crates/ggen-abb-sbb/src/as01_seat.rs` | presence-only seat gate |
| `crates/ggen-abb-sbb/tests/brce_as01_seat_court.rs` | Chicago falsifier (V1) |

---

## 7. Slice receipt (this land)

| Claim | Standing |
|-------|----------|
| Doctrine BRCE-only DO in-repo | **PARTIAL_ALIVE** |
| SovereignLease Ed25519 | **ALIVE** (lease) / **UNSUPPORTED** as AS-01 |
| WebAuthn AS-01 `:prime256v1` verify | **UNSUPPORTED** |
| Live BRCE broker / Plane 5 DO | **BLOCKED** / **UNSUPPORTED** (out of ggen) |
| Ungated DO without seat typed refusal | **PARTIAL_ALIVE** (V1 court) |
| Whole AS-01 CONSTRUCT→DO path | **PARTIAL_ALIVE** → next falsifiers V7–V11 through GA |
