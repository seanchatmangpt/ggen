# CCS-AUTHORITY-STANDING — Crystallized Control Surfaces (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CCS-AUTHORITY-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (planes / Gate schedule); `AGENTS.md` §3 Manufacture and authority |
| Status | MATERIALIZED standing ledger (not an execution receipt) |
| Materialized | 2026-11-22 |
| Observed tip | tip lineage @ `8743b874a8e6898cd795912a819a6b7335b461f8` (~`dd2822464+` / `2fd897141+`; construct_hash_gate land `3ec8afd85` / PR **#901** + V7 ES256 land `fb6e7fc6e` are ancestors; prior pins `c832cabdc` / `a3b1c9410` / `d05a131c` also ancestors) |
| Base (main) | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Tip pin | 2026-11-22 — absorb #932 onto tip `dd2822464+` (CCS #3 `construct_hash_gate` **PRESENT** PR #901 tip **4** tests; as01 V7 **4** / V1 seat **6** **PRESENT**; V7 crypto-shape **PARTIAL**; live WebAuthn **UNSUPPORTED**; broker / Plane-5 DO **BLOCKED**; docs-only; cargo **not** run; tip-cargo locks **spared**) |
| Live filename | `CCS-AUTHORITY-STANDING.md` (~8 CCS ledger); Clause-2 crown binder [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) |
| Repo | `seanchatmangpt/ggen` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence is never promoted to path `ALIVE` without tip-cargo exit 0. Fixture-court `ALIVE` ≠ Plane-5 / CONSTRUCT→DO `ALIVE`.

---

## 0. Honesty matrix (this retip @ `75c9c07cb` ~`dd2822464+`)

| Bucket | Subjects | Standing |
|--------|----------|----------|
| **ALIVE (fixture courts)** | ABB/SBB `unauthorized_do_request_is_refused`; SovereignLease Ed25519 admit; V7 ES256/`prime256v1` fixture court land **4 passed; exit 0** @ `fb6e7fc6e`; V1 ungated-DO seat fence historical **6 passed; exit 0** @ `0478fde59` | Fixture / kernel only — **never** Plane-5 DO |
| **PARTIAL_ALIVE** | CCS #1–#7 path slices; MCP CP17 `write_apply` courts **PRESENT** (tip-cargo pending/spared); CCS #3 `construct_hash_gate.rs` **PRESENT** (PR **#901** / `3ec8afd85`) + 4 in-lib refuse courts (tip-cargo spared); V7 crypto-shape path (fixture ≠ live ceremony) | Presence / land evidence without tip-cargo this retip |
| **BLOCKED** / **UNSUPPORTED** | Live WebAuthn / FIDO2 RP+hardware (**UNSUPPORTED**); live BRCE broker (**BLOCKED** → **V8**); Plane-5 front-door DO (**BLOCKED** → **V9** / OOR **G2-5**); `lifecycle.rs` still **ABSENT** | Exact next: [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2; [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 |

---

## 1. Constitutional law

```text
SELECT ≠ CONSTRUCT ≠ DO
```

- **SELECT** chooses admitted observation (`O*`).
- **CONSTRUCT** manufactures reversible artifacts, plans, intents, graphs, and receipts.
- **DO** actuates machine state through an authorized boundary only.

**Humans act only at Crystallized Control Surfaces (CCS).**
Raw model output, generated code, proof text, hooks, tickets, agents, and semantic
derivations have **no ambient DO authority** (`AGENTS.md` §3; ECOSYSTEM-SPEC-27.1.1 §1.1).

Sole lawful **CONSTRUCT → DO** seal where external actuation is admitted:

```text
BRCE + WebAuthn AS-01  (:prime256v1 / NIST P-256)
```

Local manufacture may mutate a project tree only through a CCS that itself fails
closed without human (or lease-bound) corroboration. That local write is still
**not** Plane-5 DO and does not substitute for BRCE+AS-01.

---

## 2. What a CCS is

A Crystallized Control Surface is a **named, fail-closed gate** where:

1. plane role is explicit (SELECT / CONSTRUCT / DO);
2. a seal mechanism refuses unauthenticated or uncorroborated actuation;
3. standing is claimable only via a named next verifier;
4. model/agent output may propose CONSTRUCT candidates but cannot self-grant DO.

TAI 555→1 collapses ambient HITL sprawl onto these few edges — not onto chat,
not onto PR comments, and not onto generated artifacts.

---

## 3. The ~8 Crystallized Control Surfaces

| # | Name | Plane | Seal mechanism | Standing | Next verifier |
|---|------|-------|----------------|----------|---------------|
| 1 | **BRCE + WebAuthn AS-01** | Plane 5 — Front doors / constitutional DO | **BRCE + WebAuthn AS-01 `:prime256v1`** — exclusive CONSTRUCT→DO broker | **PARTIAL_ALIVE** — doctrine + V1 seat fence + **V7 ES256/`prime256v1` fixture crypto-shape** (**PRESENT**; land **4 passed; exit 0** @ `fb6e7fc6e`). Live WebAuthn RP/hardware **UNSUPPORTED**; live BRCE broker **BLOCKED**; Plane-5 DO **BLOCKED**. Slice is **not** CONSTRUCT→DO / Plane-5 `ALIVE`. | Replay fixtures: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` (V7); `… --test brce_as01_seat_court` (V1). Next live: **V8** (broker) → **V9** / OOR **G2-5**. Never promote path `ALIVE` from fixture alone. Clause-2 binder: [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md). |
| 2 | **MCP `ggen_write_apply`** | Plane 1 — Manufacture (local write) | `confirm: true` **and** Gall **CP17** `expected_graph_hash` (independent dry-run corroboration). Not BRCE; cannot grant Plane-5 DO. | **PARTIAL_ALIVE** — CP17 courts **PRESENT** on tip `a3b1c9410`; tip cargo **not** re-run / locks spared (inspection ≠ `ALIVE`; see §4.1) | `cargo test -p ggen-mcp --test write_apply_confirm_test`; `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_refuses_a_fabricated_graph_hash_bypass` |
| 3 | **A2A construct / lifecycle apply** | Plane 1 — Manufacture (A2A/MCP lifecycle) | `confirm: true` + `construct_hash_gate` / `corroborate_expected_graph_hash` (CP17 parity with write_apply); lifecycle apply still open | **PARTIAL_ALIVE** — `construct_hash_gate.rs` **PRESENT** on tip `75c9c07cb` (PR **#901** / land `3ec8afd85`; 4 in-lib refuse courts **PRESENT**; tip-cargo spared — load high). `lifecycle.rs` still **ABSENT**. | Tip replay when free: `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --lib construct_hash_gate --target-dir /tmp/ggen-construct-hash-target` (or `--features a2a` covering `corroborate_hashes`). Next residual: land/merge `lifecycle.rs` + package courts exit 0. Do not claim A2A CP17 path `ALIVE` from presence alone. |
| 4 | **Sync receipt verify** | Plane 2 — Trust | BLAKE3 receipt chain over `{graph_hash, outputs}`; `ggen receipt verify` (zero args → `.ggen-v2/receipt.json`) | **PARTIAL_ALIVE** — verb + handler live; proves consequence identity, not DO grant | `ggen receipt verify` after a real sync; `cargo test -p ggen-engine --test receipt_chain_e2e` (or package receipt courts) |
| 5 | **SovereignLease** | Plane 2 — Trust / authority | ed25519-dalek multi-sig lease admission (`ggen-sjira`); IO-free kernel. Distinct from WebAuthn P-256 AS-01 — does not open Plane-5 DO. | **ALIVE** (lease admit fixture); **UNSUPPORTED** as AS-01 / `:prime256v1` | `cargo test -p ggen-sjira --test lease_epoch_test --test execute_lease_test` |
| 6 | **ABB/SBB Stage 2c admission** | Plane 1 — Manufacture admission | `admit()` / `plan()` / depgraph (cycle + unbound-port refusal); Stage 2c gate into ggen-engine. Emits admitted structure only — no DO. | **PARTIAL_ALIVE** (path); DO-authority refuse fixture **ALIVE** | `cargo test -p ggen-abb-sbb --test falsifiers` (incl. `unauthorized_do_request_is_refused`) |
| 7 | **CLI dry-run → sync (operator)** | Plane 1 — Manufacture (human CLI) | Human-operated `ggen sync run --dry-run` then apply; receipt verify closes the loop. Model text cannot press this edge. | **PARTIAL_ALIVE** — CLI verbs live; operator discipline is the seal until AS-01 wraps release DO | `ggen sync run --dry-run` then real sync; `ggen receipt verify` |
| 8 | **Plane 5 front doors** | Plane 5 — revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat | **BRCE + WebAuthn AS-01 `:prime256v1` only** | **UNKNOWN** / **OUT OF SCOPE** / live DO **BLOCKED** for ggen claims (revops DNS **UNKNOWN**; cos `/readyz` **BLOCKED** per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md)); live WebAuthn ceremony **UNSUPPORTED** (V7 fixture **PARTIAL** ≠ production) | Exact Plane-5 DO next (OOR **G2-5** / BRCE **V9**): (1) `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ **200**; (2) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** not `DB_UNAVAILABLE`; (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt. Never promote fleet-census naming to `ALIVE` from this repo |

### Non-CCS (explicit)

| Subject | Why not a CCS |
|---------|----------------|
| Raw LLM / agent chat | No seal; CONSTRUCT candidate only |
| Generated code / packs / templates | Projection; no ambient DO (`AGENTS.md`) |
| Ontology catalogue `iam:WebAuthn` class | Description, not runtime AS-01 |
| `confirm: true` alone | Insufficient — CP17 requires hash corroboration |
| CI green label | Metadata; not a DO grant |

---

## 4. CP17 hash-gate courts (in-repo)

Gall checkpoint **CP17**: `confirm: true` is never sufficient. Apply/construct must
independently recompute the current graph hash via dry-run and refuse fabricated
or stale `expected_graph_hash` without touching the filesystem.

### 4.1 On observed tip — cite as PARTIAL_ALIVE (tip-cargo pending/spared)

Courts are **PRESENT** on tip `a3b1c9410`. Honesty law: source presence without a
recorded tip-cargo exit **0** on this retip is **PARTIAL_ALIVE**, not path `ALIVE`.
Tip-cargo locks were **spared** this retip (no cargo).

| Court | Path | Claim | Command |
|-------|------|-------|---------|
| Confirm + CP17 behavioral court | `crates/ggen-mcp/tests/write_apply_confirm_test.rs` | **PARTIAL_ALIVE** — confirm-first / empty-hash / real dry-run hash courts **PRESENT**; tip cargo **not** re-run | `cargo test -p ggen-mcp --test write_apply_confirm_test` |
| Fabricated-hash bypass court | `crates/ggen-mcp/tests/pipeline_tools_test.rs` (`write_apply_refuses_a_fabricated_graph_hash_bypass`) | **PARTIAL_ALIVE** — fabricated hash → `Unsupported`, filesystem untouched (presence only until tip exit 0) | `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_refuses_a_fabricated_graph_hash_bypass` |
| Legitimate CP17 path | same file (`write_apply_accepts_a_real_dry_run_graph_hash`) | **PARTIAL_ALIVE** — real `ggen_sync_dry_run` hash accepted (presence only until tip exit 0) | `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_accepts_a_real_dry_run_graph_hash` |
| Implementation | `crates/ggen-mcp/src/tools/write_apply.rs` | CP17 preflight dry-run before write | (covered by courts above) |
| **`construct_hash_gate` (CCS #3)** | `crates/ggen-lsp/src/a2a_mcp/construct_hash_gate.rs` | **PARTIAL_ALIVE** — **PRESENT** on tip `75c9c07cb` (PR **#901** / land `3ec8afd85`); pure `corroborate_hashes` + 4 in-lib refuse courts **PRESENT**; wired into `ggen.construct` / MCP construct; tip cargo **spared** (load high) | `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --lib construct_hash_gate --target-dir /tmp/ggen-construct-hash-target` |

### 4.2 Still ABSENT on tip — cite as PARTIAL_ALIVE / ABSENT

| Court / surface | Branch evidence | Standing |
|-----------------|-----------------|----------|
| `corroborate_graph_hash` / `lifecycle.rs` | `cursor/mcp-a2a-hands-free-lifecycle-bc54` → `crates/ggen-lsp/src/a2a_mcp/lifecycle.rs` | **PARTIAL_ALIVE** (**ABSENT** on observed tip `a3b1c9410`) |

Do not claim full A2A lifecycle CP17 `ALIVE` until `lifecycle.rs` lands and package courts exit 0 on the admitted tip. `construct_hash_gate` presence alone is **PARTIAL**.

---

## 5. Plane map (authority)

| Plane | Role | DO authority |
|-------|------|--------------|
| 0 — Observation | Select admitted `O*` | SELECT only |
| 1 — Manufacture (ggen) | packs, graphs, sync, MCP/A2A write gates | CONSTRUCT; local write only via CCS 2/3/7 |
| 2 — Trust | receipts, leases, Truex/Affidavit | CONSTRUCT; DO only via BRCE |
| 3 — Layout / UI courts | Taffy AOT + zcode courts | CONSTRUCT; runtime DO blocked without AS-01 |
| 4 — Witness WASM | `verify_witness` | verify / CONSTRUCT |
| 5 — Front doors | revops / cos / Fourth Watch / CindyCat | **DO via BRCE+WebAuthn AS-01 `:prime256v1` only** |

---

## 6. Search corroboration (this land)

Observed in-repo (live crates + docs; ontology_catalogue noise excluded where noted):

| Term | Finding |
|------|---------|
| BRCE | Exclusive DO doctrine (`AGENTS.md`, architecture TAI/CI-CD ARDs); ungated-DO seat fence **PARTIAL_ALIVE** on tip; live broker **BLOCKED** |
| WebAuthn | Ontology class + **V7 fixture** `as01_assertion` **PRESENT** (`p256` admitted in `ggen-abb-sbb` only); live RP/hardware ceremony **UNSUPPORTED** (`rg` free of `webauthn`/`passkeys` RP deps) |
| `:prime256v1` | AS-01 WebAuthn COSE/ES256 curve binding; V7 fixture court **PARTIAL_ALIVE** (crypto shape); live ceremony still **UNSUPPORTED** — next broker **V8** / Plane-5 **V9** |
| CCS | This document crystallizes the standing set; prior prose used “control surface” loosely |
| SELECT≠CONSTRUCT≠DO | `AGENTS.md`; ECOSYSTEM-SPEC-27.1.1 §1.1 (companion PR); CROWN/GALL docs |
| `construct_hash_gate.rs` | **PRESENT** on tip `75c9c07cb` (§4.1; PR **#901**); tip-cargo spared (load high) |
| `lifecycle.rs` | Still **ABSENT** on tip `75c9c07cb` (§4.2) |
| confirm gates | `ggen_write_apply` + CP17 courts **PARTIAL_ALIVE** on observed tip (§4.1; tip cargo pending) |
| brand.toml hot-swap | **PARTIAL_ALIVE** on tip (`brand_toml_hotswap_court`); production theme **UNSUPPORTED** / `REFUSED_PRODUCTION` ([`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md); Gate-1 / ECOSYSTEM §8) — not Plane-5 DO |
| zero-HITL fab crown | **not** `ALIVE` — Foundry / ECOSYSTEM D1 remain incomplete until out-of-repo hops close |
| Plane-5 DO | **BLOCKED** — exact next CCS #8 / BRCE **V9** / OOR **G2-5** probe ladder |

---

## 7. Falsifiers

1. Any path that mutates production / Plane-5 state without BRCE+AS-01 → **REFUSED**.
2. Any agent/tool that treats model text as DO → **REFUSED_NO_AMBIENT_AUTHORITY**.
3. `confirm: true` without matching current graph hash → **REFUSED** (CP17).
4. Claiming CCS-1 or CCS-8 path `ALIVE` from this repository alone → **REFUSED_UNGROUNDED**.
5. Claiming CCS-3 path `ALIVE` from `construct_hash_gate` presence without tip-cargo exit 0 → **REFUSED_UNGROUNDED**.

---

## 8. Provenance

Materialized for GOAL SLICE: SELECT≠CONSTRUCT≠DO + ~8 CCS standing.
Honesty retip 2026-11-22 on `cursor/v2711-ccs-ops-honesty-bc54` against crown
`5e00619a4` (CP17 demoted **ALIVE → PARTIAL_ALIVE**). Next-verifier retip
2026-11-22 on `cursor/v2711-ccs-brce-next-bc54` against crown
`750b8ef4a` (V7 then still **ABSENT**). CCS3 land `3ec8afd85` (PR **#901**) +
V7 land `fb6e7fc6e` absorbed onto tip lineage. Prior webauthn retip pinned
`d05a131c`. **This retip** `cursor/v2711-ccs-brce-retip-ecd6` against tip
`2fd8971414519f5a1c51f6b04c1f7a85831def52` (~`27436608c+`): `construct_hash_gate.rs` **PRESENT** (docs-only; tip-cargo spared)
(**PARTIAL_ALIVE**; tip-cargo **spared**); V7 fixture crypto-shape
**PARTIAL_ALIVE** / fixture court land exit 0 cited; live WebAuthn ceremony
**UNSUPPORTED**; live BRCE broker **BLOCKED** (**V8**); Plane-5 DO **BLOCKED**
(**V9**/G2-5). `lifecycle.rs` remains **ABSENT**. Clause-2 binder:
[`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md). Cargo **not** run. No Plane-5 /
cos / GCP / live-WebAuthn / production WASM / zero-HITL crown claim is asserted
as `ALIVE`.
