# CCS-BRCE-STANDING — Clause 2 honesty (ECOSYSTEM-SPEC-27.1.1 §13.2)

| Field | Value |
|-------|-------|
| Document ID | `CCS-BRCE-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 (Clause 2 — CCS / BRCE + WebAuthn AS-01) |
| Companions | [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) (~8 CCS ledger); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) (AS-01 / V1–V11); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) (G2-3…G2-5) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c832cabdcd3378ef528a4ee2ee387d5731d376a8` (absorb; evidence lineage through `a3b1c9410`) |
| Tip pin | 2026-11-22 — absorb `cursor/v2711-ccs-brce-retip-ecd6` @ `16d3c2f08` (#921) onto tip `c832cabdc` (docs-only honesty; **no cargo**; tip-cargo locks **spared**) |
| Repo | `seanchatmangpt/ggen` |
| Clause-2 standing | **PARTIAL_ALIVE** |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Fixture / presence / land exit 0 ≠ Plane-5 / CONSTRUCT→DO `ALIVE`.
Tip-cargo was **not** run this retip (locks spared). Source presence without tip
exit 0 remains **PARTIAL_ALIVE**.

Compact return line:

```text
tip=c832cabdcd3378ef528a4ee2ee387d5731d376a8
standing=PARTIAL_ALIVE
next=V8-broker→V9/G2-5; tip-cargo construct_hash_gate (spared)
```

---

## 0. Tip confirmation (inspection only — no cargo)

| Subject | Evidence on tip `c832cabdc` (ancestor `a3b1c9410`) | Standing |
|---------|-----------------------------|----------|
| **CCS #3 `construct_hash_gate`** | File **PRESENT**: `crates/ggen-lsp/src/a2a_mcp/construct_hash_gate.rs`. Land `3ec8afd85` (PR **#901** MERGED 2026-11-22 — `feat(ccs): construct_hash_gate CP17 fail-closed fence`) is ancestor of tip. `lifecycle.rs` still **ABSENT**. | **PARTIAL_ALIVE** (PRESENT; tip-cargo pending / **spared**) |
| **V7 ES256 / `:prime256v1` fixture court** | **PRESENT**: `crates/ggen-abb-sbb/src/as01_assertion.rs` + `tests/brce_as01_es256_assertion_court.rs`; `p256` admitted in `ggen-abb-sbb` only. Land `fb6e7fc6e` ancestor; historical fixture court **4 passed; exit 0**. | Fixture court land **ALIVE** @ `fb6e7fc6e`; path **PARTIAL_ALIVE** (crypto shape) |
| **Live WebAuthn / FIDO2 RP + hardware** | `rg` free of `webauthn` / `web-authn` / `passkeys` RP deps in workspace `Cargo.toml` / `crates/*/Cargo.toml`. V7 is fixture ECDSA only. | **UNSUPPORTED** |
| **Live BRCE broker (admit→actuate→receipt)** | Doctrine + intent manufacture + seat/assertion fences only; broker runtime unmounted. | **BLOCKED** → exact next **V8** |
| **Plane-5 DO** (revops / cos / CindyCat front doors) | Constitutional OUT OF SCOPE for ggen DO; V7/V1 fences do not close. | **BLOCKED** → exact next **V9** / OOR **G2-5** |

---

## 1. Honesty matrix (Clause 2 crown)

| Bucket | Subjects | Standing |
|--------|----------|----------|
| **ALIVE (fixture courts only)** | V7 ES256 assertion court land @ `fb6e7fc6e`; V1 ungated-DO seat fence land @ `0478fde59`; ABB/SBB `unauthorized_do_request_is_refused`; SovereignLease Ed25519 admit | Fixture / kernel — **never** Plane-5 DO |
| **PARTIAL_ALIVE** | Clause-2 crown; CCS #1 AS-01 slice; CCS #2 CP17 write_apply courts **PRESENT**; CCS #3 `construct_hash_gate` **PRESENT** (PR #901); V7 crypto-shape path | Presence / land without tip-cargo this retip |
| **BLOCKED** | Live BRCE broker; Plane-5 front-door DO | **V8** then **V9**/G2-5 |
| **UNSUPPORTED** | Live WebAuthn / FIDO2 ceremony; production RP+hardware; SovereignLease as AS-01/`prime256v1` | Beyond V7 fixture |

---

## 2. Exact next verifiers

Replay commands are the falsifiers. Exit **0** required before promoting the
named claim. **This retip does not run cargo** (tip-cargo locks spared).

| # | Claim | Standing now | Exact next verifier |
|---|-------|--------------|---------------------|
| tip-cargo `construct_hash_gate` | CCS #3 path toward tip `ALIVE` | **PARTIAL_ALIVE** (PRESENT) | When tip-cargo is free: `cargo test -p ggen-lsp --lib construct_hash_gate` → exit **0**. Optional companion: `cargo test -p ggen-lsp --features a2a --lib` covering `corroborate_hashes`. Do **not** claim CCS #3 path `ALIVE` from presence alone. |
| tip-cargo V7 / V1 replay | Fixture courts still green on tip | Land cited; tip replay **PENDING**/spared | When free: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court`; `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` → exit **0** |
| **V8** | BRCE broker admits intent only with verified AS-01 seat | **BLOCKED** (broker unmounted) | In mounted BRCE broker checkout: intent **without** verified AS-01 → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit **0**. In-repo V1/V7 fences do **not** close this row. Owner: **BRCE**. See [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; OOR **G2-3**. |
| **V9** / OOR **G2-5** | Plane-5 front-door DO path | **BLOCKED** | Probe ladder (all required): (1) `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200**; (2) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6). Never promote from ggen fixture alone. |

### Promotion law (Clause 2)

| From | To | Requires |
|------|-----|----------|
| PRESENT `construct_hash_gate` | CCS #3 tip `ALIVE` | tip-cargo `--lib construct_hash_gate` exit **0** |
| V7 fixture land | Live WebAuthn ceremony `ALIVE` | RP/hardware court — fixture alone is **REFUSED_UNGROUNDED** |
| PARTIAL (crypto shape) | CONSTRUCT→DO `ALIVE` | **V8** broker receipt on exact tip |
| any | Plane-5 DO `ALIVE` | **V9** / G2-5 — **never** from ggen-only evidence |

---

## 3. What this retip seals (and does not)

**Sealed:** Clause-2 honesty pin at tip `a3b1c9410` — construct_hash_gate
**PRESENT** (PR #901); V7 fixture court standing held; live WebAuthn
**UNSUPPORTED**; BRCE broker + Plane-5 DO **BLOCKED** with exact next **V8 →
V9/G2-5**; tip-cargo named but **spared**.

**Not sealed:** tip-cargo exit 0; live broker; live WebAuthn; Plane-5 DO;
`lifecycle.rs`; zero-HITL / Foundry crown; any promotion of fixture exit 0 to
CONSTRUCT→DO `ALIVE`.

---

## 4. Slice receipt

| Claim | Standing |
|-------|----------|
| Clause-2 CCS/BRCE crown | **PARTIAL_ALIVE** |
| `construct_hash_gate` (PR #901 / `3ec8afd85`) | **PRESENT** / **PARTIAL_ALIVE** (tip-cargo spared) |
| V7 ES256 fixture court | Land **ALIVE** @ `fb6e7fc6e`; path **PARTIAL_ALIVE** |
| Live WebAuthn crypto | **UNSUPPORTED** |
| Live BRCE broker | **BLOCKED** → **V8** |
| Plane-5 DO | **BLOCKED** → **V9** / **G2-5** |
| Cargo this retip | **none** (locks spared) |
| Observed tip | `a3b1c94103984e2d362c068deb61cd700615e158` |
