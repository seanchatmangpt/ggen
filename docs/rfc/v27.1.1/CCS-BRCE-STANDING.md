# CCS-BRCE-STANDING — Clause 2 honesty (ECOSYSTEM-SPEC-27.1.1 §13.2)

| Field | Value |
|-------|-------|
| Document ID | `CCS-BRCE-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 (Clause 2 — CCS / BRCE + WebAuthn AS-01) |
| Companions | [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) (~8 CCS ledger); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) (AS-01 / V1–V11); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) (G2-3…G2-5) |
| Observed tip | absorb tip `ABSORB_TIP_SHA` (~`c11ea7f2a+` / `2fd897141+`; #932 construct_hash absorb) |
| Tip pin | 2026-11-22 — absorb #932 `cursor/v2711-construct-hash-retip-bc54` @ `27f586390` onto tip `c11ea7f2a+`; docs-only honesty; tip-cargo locks **spared** (no cargo this absorb) |
| Repo | `seanchatmangpt/ggen` |
| Clause-2 standing | **PARTIAL_ALIVE** |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Fixture / presence / land exit 0 ≠ Plane-5 / CONSTRUCT→DO `ALIVE`.
Tip-cargo was **not** run this retip (load high → docs-only **PRESENT**). Source
presence without tip exit 0 remains **PARTIAL_ALIVE**.

Compact return line:

```text
exit=spared
standing=PARTIAL_ALIVE
tip=ABSORB_TIP_SHA
next=CARGO_BUILD_JOBS=1 --target-dir /tmp/ggen-construct-hash-target cargo test -p ggen-lsp --lib construct_hash_gate; then as01 V7/V1; then V8→V9/G2-5
```

---

## 0. Tip confirmation (inspection only — no cargo)

| Subject | Evidence on tip `ABSORB_TIP_SHA` (~`c11ea7f2a+` / `2fd897141+`) | Standing |
|---------|-----------------------------|----------|
| **CCS #3 `construct_hash_gate`** | File **PRESENT**: `crates/ggen-lsp/src/a2a_mcp/construct_hash_gate.rs` (tip source **4** `#[test]`). Land `3ec8afd85` (PR **#901** MERGED 2026-11-22 — `feat(ccs): construct_hash_gate CP17 fail-closed fence`) is ancestor of tip. `lifecycle.rs` still **ABSENT**. | **PRESENT** / **PARTIAL_ALIVE** (tip-cargo spared — load high) |
| **V7 ES256 / `:prime256v1` fixture court** | **PRESENT**: `crates/ggen-abb-sbb/src/as01_assertion.rs` + `tests/brce_as01_es256_assertion_court.rs` (tip source **4** `#[test]`); `p256` admitted in `ggen-abb-sbb` only. Land `fb6e7fc6e` ancestor; historical fixture court **4 passed; exit 0**. | Fixture court land **ALIVE** @ `fb6e7fc6e`; path **PARTIAL_ALIVE** (crypto shape); tip-cargo spared |
| **V1 ungated-DO seat fence** | **PRESENT**: `crates/ggen-abb-sbb/tests/brce_as01_seat_court.rs` (tip source **6** `#[test]`); historical **6 passed; exit 0** @ `0478fde59`. | Fence path **PARTIAL_ALIVE**; tip-cargo spared |
| **Live WebAuthn / FIDO2 RP + hardware** | `rg` free of `webauthn` / `web-authn` / `passkeys` RP deps in workspace `Cargo.toml` / `crates/*/Cargo.toml`. V7 is fixture ECDSA only. | **UNSUPPORTED** |
| **Live BRCE broker (admit→actuate→receipt)** | Doctrine + intent manufacture + seat/assertion fences only; broker runtime unmounted. | **BLOCKED** → exact next **V8** |
| **Plane-5 DO** (revops / cos / CindyCat front doors) | Constitutional OUT OF SCOPE for ggen DO; V7/V1 fences do not close. | **BLOCKED** → exact next **V9** / OOR **G2-5** |

---

## 1. Honesty matrix (Clause 2 crown)

| Bucket | Subjects | Standing |
|--------|----------|----------|
| **ALIVE (fixture courts only)** | V7 ES256 assertion court land @ `fb6e7fc6e`; V1 ungated-DO seat fence land @ `0478fde59`; ABB/SBB `unauthorized_do_request_is_refused`; SovereignLease Ed25519 admit | Fixture / kernel — **never** Plane-5 DO |
| **PARTIAL_ALIVE** | Clause-2 crown; CCS #1 AS-01 slice; CCS #2 CP17 write_apply courts **PRESENT**; CCS #3 `construct_hash_gate` **PRESENT** (PR #901; tip **4** tests); V7 crypto-shape path; V1 seat fence tip **6** tests | Presence / land without tip-cargo this retip (load high → docs-only) |
| **BLOCKED** | Live BRCE broker; Plane-5 front-door DO | **V8** then **V9**/G2-5 |
| **UNSUPPORTED** | Live WebAuthn / FIDO2 ceremony; production RP+hardware; SovereignLease as AS-01/`prime256v1` | Beyond V7 fixture |

---

## 2. Exact next verifiers

Replay commands are the falsifiers. Exit **0** required before promoting the
named claim. **This retip does not run cargo** (load high; tip-cargo locks spared).

| # | Claim | Standing now | Exact next verifier |
|---|-------|--------------|---------------------|
| tip-cargo `construct_hash_gate` | CCS #3 path toward tip `ALIVE` | **PRESENT** / **PARTIAL_ALIVE** | When load quiet: `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --lib construct_hash_gate --target-dir /tmp/ggen-construct-hash-target` → exit **0**. Optional companion: `cargo test -p ggen-lsp --features a2a --lib` covering `corroborate_hashes`. Do **not** claim CCS #3 path `ALIVE` from presence alone. |
| tip-cargo V7 / V1 replay | Fixture courts still green on tip | Land cited; tip replay **PENDING**/spared | When free: `CARGO_BUILD_JOBS=1 cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court --target-dir /tmp/ggen-construct-hash-target`; `… --test brce_as01_seat_court` → exit **0** |
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

**Sealed:** Clause-2 honesty pin at tip `2fd897141` (~`27436608c+`) —
`construct_hash_gate` **PRESENT** (PR #901; tip **4** `#[test]`); as01 V7 (**4**)
+ V1 seat (**6**) sources **PRESENT**; live WebAuthn **UNSUPPORTED**; BRCE broker
+ Plane-5 DO **BLOCKED** with exact next **V8 → V9/G2-5**; tip-cargo named but
**spared** (load high → docs-only).

**Not sealed:** tip-cargo exit 0; live broker; live WebAuthn; Plane-5 DO;
`lifecycle.rs`; zero-HITL / Foundry crown; any promotion of fixture exit 0 to
CONSTRUCT→DO `ALIVE`.

---

## 4. Slice receipt

| Claim | Standing |
|-------|----------|
| Clause-2 CCS/BRCE crown | **PARTIAL_ALIVE** |
| `construct_hash_gate` (PR #901 / `3ec8afd85`) | **PRESENT** / **PARTIAL_ALIVE** (tip-cargo spared; load high) |
| V7 ES256 fixture court | Land **ALIVE** @ `fb6e7fc6e`; path **PARTIAL_ALIVE**; tip source **4** |
| V1 AS-01 seat fence | Land **ALIVE** @ `0478fde59`; path **PARTIAL_ALIVE**; tip source **6** |
| Live WebAuthn crypto | **UNSUPPORTED** |
| Live BRCE broker | **BLOCKED** → **V8** |
| Plane-5 DO | **BLOCKED** → **V9** / **G2-5** |
| Cargo this retip | **none** (load high → docs-only PRESENT) |
| Observed tip | `2fd8971414519f5a1c51f6b04c1f7a85831def52` |
