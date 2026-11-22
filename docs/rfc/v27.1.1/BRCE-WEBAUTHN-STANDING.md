# BRCE + WebAuthn Seat AS-01 (`:prime256v1`) — Standing Record

| Field | Value |
|-------|-------|
| Document ID | `BRCE-WEBAUTHN-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §1.1 / Plane 5 |
| Observed base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `750b8ef4a9042d22808a79be3dedff7944d4e321` (2026-11-22; prior pins `5e00619a4` / `f1639510e`) |
| Tip pin | 2026-11-22 — `cursor/v2711-ccs-brce-next-bc54` (exact next verifiers for V7 crypto **UNSUPPORTED** + Plane-5 DO **BLOCKED**; V7 court / `construct_hash_gate` still **ABSENT** on tip; cargo not run) |
| Live filename | `BRCE-WEBAUTHN-STANDING.md` (not `BRCE-AS01-STANDING.md`) |
| Target GA | **2027-01-01** |
| Slice standing | **PARTIAL_ALIVE** (doctrine + presence-only ungated-DO refusal; V1 court historical evidence **6 passed; exit 0** on `0478fde59` — fence only; tip-cargo pending on `750b8ef4a`); WebAuthn/`prime256v1` crypto verify **UNSUPPORTED** (`as01_assertion.rs` **ABSENT**); live BRCE / Plane-5 DO **BLOCKED** — no false `ALIVE` |

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
| BRCE-only DO doctrine | `AGENTS.md` §3; `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md`; TAI / Fortune5 / GBB standards | **PARTIAL_ALIVE** | Search + CI grep/jq fences; not a broker runtime |
| `REQUIRED_BROKER = "BRCE"` | `crates/ggen-architecture/src/lib.rs`; `canonical.rs`; `self_play.rs`; certification standards | **PARTIAL_ALIVE** | Nested workspace; manufactures `ArchitectureIntent` addressed to BRCE; **does not actuate** |
| BRCE intent CI fences | `.github/workflows/{gbb-kernel,gbb-certification,fortune5-architecture,tai-enterprise-rebuild,ontostar-mustar-powlv2-agents,non-llm-safe-ea-self-play}.yml` | **PARTIAL_ALIVE** | Assert `requiredBroker`/`REQUIRED_BROKER`/`directActuation == false` |
| Pack / sync BRCE ceiling | `crates/ggen-engine/src/pack.rs`, `pack_scope.rs`; `docs/pack_capabilities_guide.md` | **PARTIAL_ALIVE** | Caps never grant DO / bypass BRCE |
| ABB/SBB refuses DO authority | `crates/ggen-abb-sbb` `Refusal::AuthorityExceeded` / `unauthorized_do_request_is_refused` | **ALIVE** (CONSTRUCT ceiling) | Authority clamp ≠ AS-01 seat |
| SovereignLease (Ed25519 / `EdDSA`) | `crates/ggen-sjira/src/lease.rs` + `execute` lease gate; CLI `sjira` | **ALIVE** (lease admit) | Multi-party Ed25519; **not** WebAuthn AS-01 / `prime256v1` |
| Ed25519 sync receipts | `crates/ggen-engine` pack install signing; `ggen-mcp` receipt verify when sig present | **PARTIAL_ALIVE** | Receipt integrity; not seat DO gate |
| `ed25519-dalek` 3.x / `curve25519-dalek` 4.x+5.x | workspace `Cargo.toml` / `Cargo.lock`; [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | **PARTIAL_ALIVE** | Pin doc PRESENT; Gate-1 policy court still open |
| WebAuthn class in ontologies | `marketplace/packages/identity-access-management/ontology/identity_access_management.ttl`; autofde registry | **UNSUPPORTED** (runtime) | Catalogue / registry labels only |
| Ungated-DO-without-seat typed refusal (this slice) | `crates/ggen-abb-sbb/src/as01_seat.rs` + `tests/brce_as01_seat_court.rs` | **PARTIAL_ALIVE** | Presence/shape fence only — **no crypto verify**. Historical Gate-1 evidence: **6 passed; exit 0** on `0478fde59` ([`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md)); tip-cargo pending on `750b8ef4a`. Fence exit 0 on ancestor ≠ tip-subject path `ALIVE`. |
| AS-01 ES256/`prime256v1` assertion court (V7) | `crates/ggen-abb-sbb/src/as01_assertion.rs` + `tests/brce_as01_es256_assertion_court.rs` | **UNSUPPORTED** (**ABSENT** on tip `750b8ef4a`) | Exact next: land those paths, then run §5 **V7**. Do **not** cite WIP/`p256` drafts as tip evidence. |

---

## 3. Out-of-repo / absent (BLOCKED or UNSUPPORTED)

| Subject | Standing | Why |
|---------|----------|-----|
| WebAuthn / FIDO2 ceremony (navigator credentials, authenticator, RP) | **UNSUPPORTED** | No `webauthn` / `web-authn-rs` / `passkeys` crate; no RP server in workspace members |
| AS-01 seat verify over `:prime256v1` / ES256 | **UNSUPPORTED** | No `p256` dep on tip; `as01_assertion.rs` / `brce_as01_es256_assertion_court.rs` **ABSENT** — see §5 **V7** |
| Live BRCE broker (admit intent → actuate → execution receipt) | **BLOCKED** / **UNKNOWN** | Doctrine + intent manufacture only; broker realization not mounted in this checkout — see §5 **V8** |
| Plane 5 front doors (revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat) DO | **BLOCKED** / **UNSUPPORTED** in ggen | ECOSYSTEM-SPEC: out of ggen DO; exact next §5 **V9** + OOR **G2-5** — must not claim `ALIVE` from this repo |
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
- Promotion of an absent V7 court (or WIP draft) to crypto-shape `PARTIAL_ALIVE`.

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
| V6 | Gate-1 `curve25519-dalek` pin/docs court | pin doc [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) PRESENT; next: policy court refusing undocumented major (see pin standing §5 / `GATE-1-GAP-MATRIX.md`) | **ggen** (+ cgap-aoq) |

### Gate 2–3 (post Gate 1 → GA 2027-01-01)

| # | Claim to advance | Standing now | Exact next verifier | Owner |
|---|------------------|--------------|---------------------|-------|
| V7 | WebAuthn AS-01 assertion verifies under `ES256`/`prime256v1` | **UNSUPPORTED** (`as01_assertion.rs` **ABSENT** on tip `750b8ef4a`) | (1) Presence: `test -f crates/ggen-abb-sbb/src/as01_assertion.rs && test -f crates/ggen-abb-sbb/tests/brce_as01_es256_assertion_court.rs` → both must exist. (2) Court: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` → exit **0** with cases: known-good ES256/`prime256v1` fixture → admit; tampered sig → `REFUSED_AS01_SIGNATURE_INVALID` (or typed `REFUSED_*`); alg≠ES256 → `REFUSED_AS01_ALG`. (3) Companion V5: `rg -n 'p256\|webauthn\|web-authn\|passkeys' Cargo.toml crates/*/Cargo.toml Cargo.lock` → empty **or** documented admit only. Fixture crypto-shape exit 0 ≠ hardware / RP / Plane-5 `ALIVE`. | **ggen** (adapter) or BRCE/trust plane |
| V8 | BRCE broker admits intent only with verified AS-01 seat | **BLOCKED** (broker unmounted) | In mounted BRCE broker checkout: intent **without** verified AS-01 seat → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit **0**. In-repo fence `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` does **not** close this row. | **BRCE** (out of ggen unless adapter mounted) |
| V9 | Plane 5 front-door DO path | **BLOCKED** | Probe ladder (all required): (a) OOR **G2-1** `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200**; (b) OOR **G2-2** `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); (c) external host court: request **without** AS-01 → refuse; **with** AS-01 → BRCE receipt for the named front-door change ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6; OOR **G2-5**). Never promote from ggen fence alone. | Plane 5 / revops / cos |
| V10 | R-01…R-07 closed | **BLOCKED** (bodies) | typed receipt or typed refusal per risk register (ECOSYSTEM-SPEC §11) | multi-repo |
| V11 | GA crown | open | aggregate standing: V1–V10 green; no ungated DO; F=0 | release governance |

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
| `crates/ggen-abb-sbb/src/as01_assertion.rs` | **ABSENT** on tip — named V7 crypto-shape target |
| `crates/ggen-abb-sbb/tests/brce_as01_es256_assertion_court.rs` | **ABSENT** on tip — named V7 Chicago falsifier target |
| [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | G2-3/G2-4/G2-5 live broker / WebAuthn / Plane-5 DO |
| [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) | ~8 CCS ledger (CCS-1 / CCS-8) |

---

## 7. Slice receipt (this land / retip)

| Claim | Standing |
|-------|----------|
| Doctrine BRCE-only DO in-repo | **PARTIAL_ALIVE** |
| SovereignLease Ed25519 | **ALIVE** (lease) / **UNSUPPORTED** as AS-01 |
| WebAuthn AS-01 `:prime256v1` verify | **UNSUPPORTED** (V7 court **ABSENT** on tip — exact next §5 **V7**) |
| Live BRCE broker / Plane 5 DO | **BLOCKED** / **UNSUPPORTED** (out of ggen) — exact next §5 **V8**/**V9** + OOR **G2-3**/**G2-5** |
| Ungated DO without seat typed refusal | **PARTIAL_ALIVE** (V1 court; historical evidence exit 0 on `0478fde59`; tip-cargo pending on `750b8ef4a`) |
| Whole AS-01 CONSTRUCT→DO path | **PARTIAL_ALIVE** → next falsifiers **V7–V11** (crypto still **UNSUPPORTED**; live DO **BLOCKED**) + OOR **G2-3/G2-4/G2-5** through GA |
| Observed tip | `750b8ef4a9042d22808a79be3dedff7944d4e321` (`cursor/v2711-ccs-brce-next-bc54`; prior honesty pin `5e00619a4`) |
