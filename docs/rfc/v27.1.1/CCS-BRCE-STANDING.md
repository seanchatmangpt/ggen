# CCS-BRCE-STANDING — Clause 2 honesty (ECOSYSTEM-SPEC-27.1.1 §13.2)

| Field | Value |
|-------|-------|
| Document ID | `CCS-BRCE-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 (Clause 2 — CCS / BRCE + WebAuthn AS-01) |
| Companions | [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) (~8 CCS ledger); [`CCS-CONSTRUCT-ALIVE-STANDING.md`](./CCS-CONSTRUCT-ALIVE-STANDING.md) (CCS #3 tip-cargo **ALIVE**); [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md) (CCS #2 tip-cargo **ALIVE**); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) (AS-01 / V1–V11); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) (G2-3…G2-5) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c91b225614a3ca0472303c05bab25601db1d51e1` (CCS #2 write_apply tip-cargo **ALIVE** land `b5cb632c6`; V8 broker still **BLOCKED**) |
| Tip pin | 2026-11-22 — CCS #2 `write_apply` tip-cargo **ALIVE** on tip `c91b22561` (evidence `b5cb632c6`; 7+1+1 passed exit **0**); CCS #3 construct retained **ALIVE**; **V8**/**V9** OOR **BLOCKED** unchanged |
| Repo | `seanchatmangpt/ggen` |
| Clause-2 standing | **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; live broker **BLOCKED** — **not** V8 `ALIVE`) |

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
seat=0 (6 passed)
packages=ggen-mcp,ggen-lsp+a2a,ggen-abb-sbb
evidence=b5cb632c6
tip=c91b22561
standing=PARTIAL_ALIVE
next=lifecycle package courts | V8/V9 OOR BLOCKED
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
| **ALIVE (tip-cargo / fixture courts)** | CCS #2 `write_apply` **7+1+1 passed; exit 0** @ `b5cb632c6`; CCS #3 `construct_hash_gate` **4 passed; exit 0**; V7 ES256 assertion court tip replay **4 passed; exit 0**; V1 seat fence tip replay **6 passed; exit 0**; historical lands @ `fb6e7fc6e` / `0478fde59` | Fixture / kernel / CCS #2+#3 path — **never** Plane-5 DO |
| **PARTIAL_ALIVE** | Clause-2 crown; CCS #1 AS-01 slice; `lifecycle.rs` **PRESENT** (tip `513704d04`; tip-cargo ALIVE 6/6 @ `acd9705c1`) — land `cursor/mcp-a2a-hands-free-lifecycle-bc54` | Crown ≠ CONSTRUCT→DO **ALIVE** until **V8**/**V9** |
| **BLOCKED** | Live BRCE broker; Plane-5 front-door DO | **V8** then **V9**/G2-5 |
| **UNSUPPORTED** | Live WebAuthn / FIDO2 ceremony; production RP+hardware; SovereignLease as AS-01/`prime256v1` | Beyond V7 fixture |

---

## 2. Exact next verifiers

Replay commands are the falsifiers. Exit **0** required before promoting the
named claim. Tip-cargo **ran** 2026-11-22 on `861a6e73a` (this branch).

| # | Claim | Standing now | Exact next verifier |
|---|-------|--------------|---------------------|
| tip-cargo `construct_hash_gate` | CCS #3 CP17 fence on tip | **ALIVE** @ `861a6e73a` — **4 passed; exit 0** | Residual: land `lifecycle.rs` from `cursor/mcp-a2a-hands-free-lifecycle-bc54` @ `05c6032f0` → package courts exit **0**; full A2A CP17 path still **PARTIAL** until lifecycle lands |
| **CCS #2 `write_apply` tip-cargo** | CP17 MCP write gate on tip `c91b22561` | **ALIVE** @ `b5cb632c6` — **7+1+1 passed; exit 0** (see [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md)) | Replay §2.1 commands; **V8**/**V9** remain **BLOCKED** |
| **`lifecycle.rs`** | `crates/ggen-lsp/src/a2a_mcp/lifecycle.rs` | **ABSENT** on tip `008a81d21` (`git ls-tree` — no blob) | Land path: merge/cherry from `cursor/mcp-a2a-hands-free-lifecycle-bc54` (`lifecycle.rs` @ `05c6032f0`); then A2A package courts exit **0** |
| tip-cargo V7 / V1 replay | Fixture courts green on tip | **ALIVE** @ `861a6e73a` — V7 **4** / V1 **6** passed; exit **0** | Live WebAuthn ceremony — **UNSUPPORTED** (not closed by fixture replay) |
| **V8** | BRCE broker admits intent only with verified AS-01 seat | **BLOCKED** (broker unmounted) | In mounted BRCE broker checkout: intent **without** verified AS-01 → `REFUSED_*`; intent **with** verified seat → execution receipt + replay digest → exit **0**. In-repo V1/V7 fences do **not** close this row. Owner: **BRCE**. See [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; OOR **G2-3**. |
| **V9** / OOR **G2-5** | Plane-5 front-door DO path | **BLOCKED** | Probe ladder (all required): (1) `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200**; (2) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §4 item 6). Never promote from ggen fixture alone. |

### 2.1 CCS #2 `ggen_write_apply` — tip-cargo **ALIVE**

Standing: **ALIVE** on tip `c91b22561` (evidence land `b5cb632c6`; target `/tmp/ggen-write-apply-target`). Full receipt: [`CCS-WRITE-APPLY-ALIVE-STANDING.md`](./CCS-WRITE-APPLY-ALIVE-STANDING.md).

| # | Court | Result |
|---|-------|--------|
| 1 | Confirm + CP17 behavioral | `write_apply_confirm_test` → **7 passed; exit 0** |
| 2 | Fabricated-hash bypass | `write_apply_refuses_a_fabricated_graph_hash_bypass` → **1 passed; exit 0** |
| 3 | Legitimate CP17 path | `write_apply_accepts_a_real_dry_run_graph_hash` → **1 passed; exit 0** |

**V8** broker and **V9** / OOR **G2-5** remain **BLOCKED** — unchanged; CCS #2 tip-cargo **ALIVE** does **not** close them.

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

**Not sealed:** live broker (**V8** **BLOCKED**); live WebAuthn ceremony; Plane-5 DO (**V9**/G2-5 **BLOCKED**); full A2A lifecycle package courts; zero-HITL / Foundry crown; CONSTRUCT→DO `ALIVE`.

**Also sealed this retip:** CCS #2 `write_apply` tip-cargo **ALIVE** @ `b5cb632c6` (§2.1).

---

## 4. Slice receipt

| Claim | Standing |
|-------|----------|
| Clause-2 CCS/BRCE crown | **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; broker/Plane-5 **BLOCKED**) |
| `construct_hash_gate` (PR #901 / `3ec8afd85`) | **ALIVE** tip-cargo **4 passed; exit 0** @ `861a6e73a` |
| V7 ES256 fixture court | Land **ALIVE** @ `fb6e7fc6e`; tip replay **ALIVE** **4 passed; exit 0** |
| V1 AS-01 seat fence | Land **ALIVE** @ `0478fde59`; tip replay **ALIVE** **6 passed; exit 0** |
| Live WebAuthn crypto | **UNSUPPORTED** |
| Live BRCE broker | **BLOCKED** → **V8** (**not** claimed `ALIVE`) |
| Plane-5 DO | **BLOCKED** → **V9** / **G2-5** |
| Cargo (evidence branch) | **yes** @ `861a6e73a` — construct_hash + as01 V7/V1 courts; loadavg_1m≈7.7 at start |
| Cargo (crown absorb) | **no** — docs-only cherry-pick onto crown `c329b7ee5+` |
| Evidence tip | `861a6e73a10864eecacae83de88099a83f176a64` (`cursor/v2711-ccs-brce-tipcargo-bc54`) |
| Observed tip (crown) | `008a81d21f14d467be9da7951b39c8d655acb5f0` |
| CCS #2 `write_apply` tip-cargo | **ALIVE** @ `b5cb632c6` — **7+1+1 passed; exit 0** (§2.1) |
| `lifecycle.rs` on tip | **ABSENT** — land `cursor/mcp-a2a-hands-free-lifecycle-bc54` @ `05c6032f0` |
