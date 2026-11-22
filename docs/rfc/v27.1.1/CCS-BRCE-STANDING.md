# CCS-BRCE-STANDING — Clause 2 honesty (ECOSYSTEM-SPEC-27.1.1 §13.2)

| Field | Value |
|-------|-------|
| Document ID | `CCS-BRCE-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 (Clause 2 — CCS / BRCE + WebAuthn AS-01) |
| Companions | [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) (~8 CCS ledger); [`CCS-CONSTRUCT-ALIVE-STANDING.md`](./CCS-CONSTRUCT-ALIVE-STANDING.md) (CCS #3 tip-cargo **ALIVE**); [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md) (CCS #2 tip-cargo **ALIVE**); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) (AS-01 / V1–V11); [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) (bodies **BLOCKED**); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) (G2-3…G2-5 / G3-5) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f5f44571f9a331d46e2895657621b7efd7127f1b` (OOR retip11 on `6a3a623ba`; seat+R01 contract exact tip-cargo **ALIVE** @ `e7569893a`; **V8** broker + R-01…R-07 bodies still **BLOCKED**) |
| Tip pin | 2026-11-22 — residual honesty `cursor/v2711-brce-v8-bodies-bc54` on crown `f5f44571f` (`6a3a623ba+`): pin **V8** broker **BLOCKED** + R-01…R-07 bodies **BLOCKED** with exact next verifiers/owners/HITL; retain CCS #2/#3 + seat fence **ALIVE**; **no** invented V8/`bodies` `ALIVE` |
| Repo | `seanchatmangpt/ggen` |
| Clause-2 standing | **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; live broker **BLOCKED** — **not** V8 `ALIVE`; risk bodies **BLOCKED**) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Fixture / presence / land exit 0 ≠ Plane-5 / CONSTRUCT→DO `ALIVE`.
Tip-cargo **ran** this retip for CCS #3 + AS-01 fixture replay courts only.
**V8** broker and Plane-5 DO remain **BLOCKED** — do **not** claim V8 `ALIVE`.

Compact return line:

```text
write_apply=0 (7+1+1 passed)
construct_hash=0 (4 passed)
es256=0 (4 passed)
seat=0 (6 passed @ e7569893a)
r01_contract=0 (7 passed @ e7569893a)
packages=ggen-mcp,ggen-lsp+a2a,ggen-abb-sbb,ggen-graph
evidence=42c5b9ba4
tip=f5f44571f (6a3a623ba+)
standing=PARTIAL_ALIVE
next=V8 broker BLOCKED (owner BRCE; HITL mount+WebAuthn) → V9/G2-5; R01-R07 bodies BLOCKED (owner risk-register; OOR G3-5)
```

---

## 0. Tip confirmation (tip-cargo @ `861a6e73a`)

| Subject | Evidence on tip `861a6e73a10864eecacae83de88099a83f176a64` | Standing |
|---------|-----------------------------|----------|
| **CCS #3 `construct_hash_gate`** | `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --features a2a --lib a2a_mcp::construct_hash_gate::tests --target-dir /tmp/ggen-ccs-construct-alive-601f-target` → **4 passed; exit 0** (see [`CCS-CONSTRUCT-ALIVE-STANDING.md`](./CCS-CONSTRUCT-ALIVE-STANDING.md)). Land `3ec8afd85` (PR **#901**). | **ALIVE** (tip-cargo) |
| **V7 ES256 / `:prime256v1` fixture court** | `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` (same target-dir) → **4 passed; exit 0**. Land `fb6e7fc6e` ancestor. | Fixture court land **ALIVE** @ `fb6e7fc6e`; path **ALIVE** (tip-cargo replay); live ceremony **UNSUPPORTED** |
| **V1 ungated-DO seat fence** | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` (same target-dir) → **6 passed; exit 0**. Land @ `0478fde59`. | Fence path **ALIVE** (tip-cargo replay) |
| **Live WebAuthn / FIDO2 RP + hardware** | `rg` free of `webauthn` / `web-authn` / `passkeys` RP deps in workspace `Cargo.toml` / `crates/*/Cargo.toml`. V7 is fixture ECDSA only. | **UNSUPPORTED** |
| **Live BRCE broker (admit→actuate→receipt)** | Doctrine + intent manufacture + seat/assertion fences only; broker runtime unmounted. | **BLOCKED** → exact next **V8** |
| **Plane-5 DO** (revops / cos / CindyCat front doors) | Constitutional OUT OF SCOPE for ggen DO; V7/V1 fences do not close. | **BLOCKED** → exact next **V9** / OOR **G2-5** |

---

## 1. Honesty matrix (Clause 2 crown)

| Bucket | Subjects | Standing |
|--------|----------|----------|
| **ALIVE (tip-cargo / fixture courts)** | CCS #2 `write_apply` **7+1+1 passed; exit 0** @ `42c5b9ba4`; CCS #3 `construct_hash_gate` **4 passed; exit 0**; V7 ES256 assertion court tip replay **4 passed; exit 0**; V1 seat fence exact tip-cargo **6 passed; exit 0** @ `e7569893a`; R-01…R-07 *contract* exact tip-cargo **7 passed; exit 0** @ `e7569893a` | Fixture / kernel / CCS #2+#3 / contract fence — **never** Plane-5 DO / body closure |
| **PARTIAL_ALIVE** | Clause-2 crown; CCS #1 AS-01 slice | Crown ≠ CONSTRUCT→DO **ALIVE** until **V8**/**V9** |
| **BLOCKED** | Live BRCE broker (**V8**); Plane-5 front-door DO (**V9**/G2-5); R-01…R-07 *bodies* (OOR **G3-5**) | Exact next §2 + [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2 |
| **UNSUPPORTED** | Live WebAuthn / FIDO2 ceremony; production RP+hardware; SovereignLease as AS-01/`prime256v1` | Beyond V7 fixture |

---

## 2. Exact next verifiers

Replay commands are the falsifiers. Exit **0** required before promoting the
named claim. Tip-cargo **ran** 2026-11-22 on `861a6e73a` (this branch).

| # | Claim | Standing now | Exact next verifier | Owner / HITL |
|---|-------|--------------|---------------------|--------------|
| tip-cargo `construct_hash_gate` | CCS #3 CP17 fence on tip | **ALIVE** @ `861a6e73a` — **4 passed; exit 0** | Construct **ALIVE** retained; next live ladder **V8**/**V9** (**BLOCKED**) | **ggen** (replay) |
| **CCS #2 `write_apply` tip-cargo** | CP17 MCP write gate on tip `cde02d1f2` | **ALIVE** @ `42c5b9ba4` — **7+1+1 passed; exit 0** (see [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md)) | Replay §2.1 commands; **V8**/**V9** remain **BLOCKED** | **ggen** (replay) |
| **`lifecycle.rs`** | `crates/ggen-lsp/src/a2a_mcp/lifecycle.rs` | **ALIVE** — **PRESENT** + tip-cargo **6/6** @ `513704d04`/`acd9705c1` | Replay: `cargo test -p ggen-lsp --features a2a --test a2a_lifecycle_hands_free_test`; BRCE **V8** still **BLOCKED** | **ggen** (replay) |
| tip-cargo V7 / V1 seat | Fixture + seat fence green | V7 land **ALIVE** @ `fb6e7fc6e`; V1 seat exact tip-cargo **ALIVE** **6** @ `e7569893a` | Fence replay: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` / `… brce_as01_es256_assertion_court`. Live WebAuthn ceremony — **UNSUPPORTED** | **ggen** (fence); Founder HITL for live ceremony |
| R-01…R-07 *contract* | `r01_r07_resolution_court` | **ALIVE** exact tip-cargo **7** @ `e7569893a` | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court --locked -- --test-threads=1`. Contract ≠ body closure | **ggen** (contract) |
| **V8** | BRCE broker admits intent only with verified AS-01 seat | **BLOCKED** (broker unmounted) | In mounted BRCE broker checkout: intent **without** verified AS-01 → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit **0**. In-repo V1/V7 fences do **not** close this row. See [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; OOR **G2-3**. | **BRCE** (out of ggen). **HITL:** Founder mounts broker + live WebAuthn AS-01 before claiming V8 `ALIVE` |
| **V9** / OOR **G2-5** | Plane-5 front-door DO path | **BLOCKED** | Probe ladder (all required): (1) `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200**; (2) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6). Never promote from ggen fixture alone. | Plane 5 / revops / cos. **HITL:** Founder after **V8** |
| **R-01…R-07 bodies** | Risk-register prose + typed closure | **BLOCKED** (bodies unmounted) | Admit sovereign risk-register bodies (do **not** invent) → per-ID typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract fence alone does **not** close. OOR **G3-5** / [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2 | Risk-register owner (multi-repo). **HITL:** Founder admits register path |

### 2.1 CCS #2 `ggen_write_apply` — tip-cargo **ALIVE**

Standing: **ALIVE** on tip `cde02d1f2` (evidence land `42c5b9ba4`; target `/tmp/ggen-write-apply-target`). Full receipt: [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md).

| # | Court | Result |
|---|-------|--------|
| 1 | Confirm + CP17 behavioral | `write_apply_confirm_test` → **7 passed; exit 0** |
| 2 | Fabricated-hash bypass | `write_apply_refuses_a_fabricated_graph_hash_bypass` → **1 passed; exit 0** |
| 3 | Legitimate CP17 path | `write_apply_accepts_a_real_dry_run_graph_hash` → **1 passed; exit 0** |

**V8** broker and **V9** / OOR **G2-5** remain **BLOCKED** — unchanged; CCS #2 tip-cargo **ALIVE** does **not** close them.
R-01…R-07 *bodies* remain **BLOCKED** — contract tip-cargo **ALIVE** @ `e7569893a` does **not** close them.

### Promotion law (Clause 2)

| From | To | Requires |
|------|-----|----------|
| PRESENT `construct_hash_gate` | CCS #3 tip `ALIVE` | tip-cargo `--lib construct_hash_gate` exit **0** |
| V7 fixture land | Live WebAuthn ceremony `ALIVE` | RP/hardware court — fixture alone is **REFUSED_UNGROUNDED** |
| PARTIAL (crypto shape) | CONSTRUCT→DO `ALIVE` | **V8** broker receipt on exact tip |
| any | Plane-5 DO `ALIVE` | **V9** / G2-5 — **never** from ggen-only evidence |

---

## 3. What this retip seals (and does not)

**Sealed:** Clause-2 tip-cargo fences at `861a6e73a` — `construct_hash_gate` **4 passed; exit 0**;
as01 V7 **4** + V1 **6** tip replay **exit 0**; live WebAuthn **UNSUPPORTED**; BRCE broker
**BLOCKED** (**V8** not run — **no V8 `ALIVE`**); Plane-5 DO **BLOCKED** (**V9**/G2-5).

**Not sealed:** live broker (**V8** **BLOCKED**); live WebAuthn ceremony; Plane-5 DO (**V9**/G2-5 **BLOCKED**); R-01…R-07 bodies (**BLOCKED**); zero-HITL / Foundry crown; CONSTRUCT→DO `ALIVE`. Construct + lifecycle + seat + R01-*contract* tip-cargo **ALIVE** ≠ **V8** `ALIVE` ≠ body closure.

**Also sealed prior:** CCS #2 `write_apply` tip-cargo **ALIVE** @ `42c5b9ba4` (§2.1); seat fence + R01 contract exact tip-cargo **ALIVE** @ `e7569893a`.

---

## 4. Slice receipt

| Claim | Standing |
|-------|----------|
| Clause-2 CCS/BRCE crown | **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; broker/Plane-5/bodies **BLOCKED**) |
| `construct_hash_gate` (PR #901 / `3ec8afd85`) | **ALIVE** tip-cargo **4 passed; exit 0** @ `861a6e73a` |
| V7 ES256 fixture court | Land **ALIVE** @ `fb6e7fc6e`; tip replay **ALIVE** **4 passed; exit 0** |
| V1 AS-01 seat fence | Exact tip-cargo **ALIVE** **6** @ `e7569893a` (land `0478fde59`) |
| R-01…R-07 *contract* | Exact tip-cargo **ALIVE** **7** @ `e7569893a` |
| R-01…R-07 *bodies* | **BLOCKED** → OOR **G3-5** / risk-register HITL (**not** claimed `ALIVE`) |
| Live WebAuthn crypto | **UNSUPPORTED** |
| Live BRCE broker | **BLOCKED** → **V8** (**not** claimed `ALIVE`) |
| Plane-5 DO | **BLOCKED** → **V9** / **G2-5** |
| Cargo (this residual honesty) | **no** — docs-only; no invented tip-cargo |
| Evidence tip (prior) | `861a6e73a` / `e7569893a` / `42c5b9ba4` retained |
| Observed tip (crown) | `f5f44571f9a331d46e2895657621b7efd7127f1b` (`6a3a623ba` + OOR retip11) |
| Side branch | `cursor/v2711-brce-v8-bodies-bc54` |
| CCS #2 `write_apply` tip-cargo | **ALIVE** @ `42c5b9ba4` — **7+1+1 passed; exit 0** (§2.1) |
| `lifecycle.rs` on tip | **ALIVE** — **PRESENT**/tip-cargo **6/6** @ `513704d04`/`acd9705c1` |
