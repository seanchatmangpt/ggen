# CCS-AUTHORITY-STANDING — Crystallized Control Surfaces (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CCS-AUTHORITY-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (planes / Gate schedule); `AGENTS.md` §3 Manufacture and authority |
| Status | MATERIALIZED standing ledger (not an execution receipt) |
| Materialized | 2026-11-22 |
| Observed tip | PR [#849](https://github.com/seanchatmangpt/ggen/pull/849) / `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `4b22529df2939412c41107fe5e9eea66ba6a4dad` (foundry honesty retip; prior `5e00619a4`) |
| Base (main) | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Tip pin | 2026-11-22 — `cursor/v2711-foundry-honesty-bc54` after crown tip `4b22529df` (CCS standings held PARTIAL; Plane-5 DO still not ALIVE; cargo not run) |
| Live filename | `CCS-AUTHORITY-STANDING.md` (not `CCS-BRCE-STANDING.md`) |
| Repo | `seanchatmangpt/ggen` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence and open PR branches are never promoted to `ALIVE` without an exact verifier exit.

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
| 1 | **BRCE + WebAuthn AS-01** | Plane 5 — Front doors / constitutional DO | **BRCE + WebAuthn AS-01 `:prime256v1`** — exclusive CONSTRUCT→DO broker | **PARTIAL_ALIVE** — doctrine + in-repo ungated-DO seat fence on tip (`as01_seat` / `brce_as01_seat_court`; Gate-1 evidence **6 passed; exit 0** on `0478fde59`). WebAuthn/`prime256v1` crypto verify **UNSUPPORTED** (`as01_assertion.rs` **ABSENT** on `750b8ef4a`); live BRCE broker **BLOCKED**. Slice is **not** CONSTRUCT→DO `ALIVE`. | Fence replay (no promote): `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`. Crypto next (**UNSUPPORTED**→crypto-shape): BRCE §5 **V7** — `test -f crates/ggen-abb-sbb/src/as01_assertion.rs && test -f crates/ggen-abb-sbb/tests/brce_as01_es256_assertion_court.rs` then `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` (known-good admit; tampered → `REFUSED_*`; alg≠ES256 refuse). Live next: **V8** (broker+seat) → **V9** / OOR **G2-5** (Plane-5 DO **BLOCKED**). Do not claim path `ALIVE` from fence alone. |
| 2 | **MCP `ggen_write_apply`** | Plane 1 — Manufacture (local write) | `confirm: true` **and** Gall **CP17** `expected_graph_hash` (independent dry-run corroboration). Not BRCE; cannot grant Plane-5 DO. | **PARTIAL_ALIVE** — CP17 courts **PRESENT** on tip `750b8ef4a`; cargo **not** re-run on this tip (inspection ≠ `ALIVE`; see §4.1) | `cargo test -p ggen-mcp --test write_apply_confirm_test`; `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_refuses_a_fabricated_graph_hash_bypass` |
| 3 | **A2A construct / lifecycle apply** | Plane 1 — Manufacture (A2A/MCP lifecycle) | `confirm: true` + `corroborate_graph_hash` / `construct_hash_gate` (CP17 parity with write_apply) | **PARTIAL_ALIVE** — landed on feature branches (`cursor/mcp-a2a-hands-free-lifecycle-bc54`, `cursor/a2a-construct-hash-gate-bc54`); **`construct_hash_gate.rs` ABSENT** and **`lifecycle.rs` ABSENT** on observed tip `750b8ef4a` | Merge onto crown + `cargo test -p ggen-lsp --features a2a` (or branch court covering `corroborate_graph_hash` / `construct_hash_gate`); until merge, standing stays PARTIAL |
| 4 | **Sync receipt verify** | Plane 2 — Trust | BLAKE3 receipt chain over `{graph_hash, outputs}`; `ggen receipt verify` (zero args → `.ggen-v2/receipt.json`) | **PARTIAL_ALIVE** — verb + handler live; proves consequence identity, not DO grant | `ggen receipt verify` after a real sync; `cargo test -p ggen-engine --test receipt_chain_e2e` (or package receipt courts) |
| 5 | **SovereignLease** | Plane 2 — Trust / authority | ed25519-dalek multi-sig lease admission (`ggen-sjira`); IO-free kernel. Distinct from WebAuthn P-256 AS-01 — does not open Plane-5 DO. | **PARTIAL_ALIVE** (lease admit path); **UNSUPPORTED** as AS-01 / `:prime256v1` | `cargo test -p ggen-sjira --test lease_epoch_test --test execute_lease_test` |
| 6 | **ABB/SBB Stage 2c admission** | Plane 1 — Manufacture admission | `admit()` / `plan()` / depgraph (cycle + unbound-port refusal); Stage 2c gate into ggen-engine. Emits admitted structure only — no DO. | **PARTIAL_ALIVE** | `cargo test -p ggen-abb-sbb --test falsifiers` |
| 7 | **CLI dry-run → sync (operator)** | Plane 1 — Manufacture (human CLI) | Human-operated `ggen sync run --dry-run` then apply; receipt verify closes the loop. Model text cannot press this edge. | **PARTIAL_ALIVE** — CLI verbs live; operator discipline is the seal until AS-01 wraps release DO | `ggen sync run --dry-run` then real sync; `ggen receipt verify` |
| 8 | **Plane 5 front doors** | Plane 5 — revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat | **BRCE + WebAuthn AS-01 `:prime256v1` only** | **UNKNOWN** / **OUT OF SCOPE** / live DO **BLOCKED** for ggen claims (revops DNS **UNKNOWN**; cos `/readyz` **BLOCKED** per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md)); AS-01 crypto **UNSUPPORTED** | Exact Plane-5 DO next (OOR **G2-5** / BRCE **V9**): (1) `getent hosts revops.fnox.io` ∧ `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ **200**; (2) `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → **200** not `DB_UNAVAILABLE`; (3) host court — request without AS-01 → refuse; with AS-01 → BRCE receipt. Crypto prerequisite: BRCE **V7** (still **UNSUPPORTED** on tip). Never promote fleet-census naming to `ALIVE` from this repo |

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

### 4.1 On observed tip — cite as PARTIAL_ALIVE (tip-cargo pending)

Courts are **PRESENT** on tip `750b8ef4a`. Honesty law: source presence without a
recorded exit **0** on the admitted tip is **PARTIAL_ALIVE**, not `ALIVE`.

| Court | Path | Claim | Command |
|-------|------|-------|---------|
| Confirm + CP17 behavioral court | `crates/ggen-mcp/tests/write_apply_confirm_test.rs` | **PARTIAL_ALIVE** — confirm-first / empty-hash / real dry-run hash courts **PRESENT** on tip `750b8ef4a`; tip cargo **not** re-run | `cargo test -p ggen-mcp --test write_apply_confirm_test` |
| Fabricated-hash bypass court | `crates/ggen-mcp/tests/pipeline_tools_test.rs` (`write_apply_refuses_a_fabricated_graph_hash_bypass`) | **PARTIAL_ALIVE** — fabricated hash → `Unsupported`, filesystem untouched (presence only until tip exit 0) | `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_refuses_a_fabricated_graph_hash_bypass` |
| Legitimate CP17 path | same file (`write_apply_accepts_a_real_dry_run_graph_hash`) | **PARTIAL_ALIVE** — real `ggen_sync_dry_run` hash accepted (presence only until tip exit 0) | `cargo test -p ggen-mcp --test pipeline_tools_test -- write_apply_accepts_a_real_dry_run_graph_hash` |
| Implementation | `crates/ggen-mcp/src/tools/write_apply.rs` | CP17 preflight dry-run before write | (covered by courts above) |

### 4.2 Feature-branch only — cite as PARTIAL_ALIVE / ABSENT on tip

| Court / surface | Branch evidence | Standing |
|-----------------|-----------------|----------|
| `corroborate_graph_hash` / `lifecycle.rs` | `cursor/mcp-a2a-hands-free-lifecycle-bc54` → `crates/ggen-lsp/src/a2a_mcp/lifecycle.rs` | **PARTIAL_ALIVE** (**ABSENT** on observed tip `750b8ef4a`) |
| `construct_hash_gate` / `corroborate_expected_graph_hash` | `cursor/a2a-construct-hash-gate-bc54` → `crates/ggen-lsp/src/a2a_mcp/construct_hash_gate.rs` | **PARTIAL_ALIVE** (**ABSENT** on observed tip `750b8ef4a`) |

Do not claim A2A CP17 `ALIVE` until those gates merge and their package courts exit 0 on the admitted tip.

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
| WebAuthn | `iam:WebAuthn` ontology class only; crypto verify **UNSUPPORTED** (`as01_assertion` **ABSENT**; `rg` empty for `p256`/`webauthn`/`passkeys` in workspace Cargo manifests + lock on tip `750b8ef4a`) |
| `:prime256v1` | Named as AS-01 WebAuthn COSE/ES256 curve binding; no in-ggen WebAuthn verifier on tip — exact next BRCE **V7** |
| CCS | This document crystallizes the standing set; prior prose used “control surface” loosely |
| SELECT≠CONSTRUCT≠DO | `AGENTS.md`; ECOSYSTEM-SPEC-27.1.1 §1.1 (companion PR); CROWN/GALL docs |
| `corroborate_graph_hash` / `lifecycle.rs` / `construct_hash_gate.rs` | Feature branches only (§4.2); **ABSENT** on tip `750b8ef4a` |
| confirm gates | `ggen_write_apply` + CP17 courts **PARTIAL_ALIVE** on observed tip (§4.1; tip cargo pending) |
| brand.toml hot-swap | OEM hot-swap admit fence landed on tip lineage (`f6cb18f86`); crown still **PARTIAL_ALIVE** / not Plane-5 DO — see Gate-1 / ECOSYSTEM §8 |
| zero-HITL fab crown | **not** `ALIVE` — Foundry / ECOSYSTEM D1 remain incomplete until out-of-repo hops close |
| Plane-5 DO | **BLOCKED** — exact next CCS #8 / BRCE **V9** / OOR **G2-5** probe ladder |

---

## 7. Falsifiers

1. Any path that mutates production / Plane-5 state without BRCE+AS-01 → **REFUSED**.
2. Any agent/tool that treats model text as DO → **REFUSED_NO_AMBIENT_AUTHORITY**.
3. `confirm: true` without matching current graph hash → **REFUSED** (CP17).
4. Claiming CCS-1 or CCS-8 `ALIVE` from this repository alone → **REFUSED_UNGROUNDED**.

---

## 8. Provenance

Materialized for GOAL SLICE: SELECT≠CONSTRUCT≠DO + ~8 CCS standing.
Honesty retip 2026-11-22 on `cursor/v2711-ccs-ops-honesty-bc54` against crown
`5e00619a4` (CP17 demoted **ALIVE → PARTIAL_ALIVE**). Next-verifier retip
2026-11-22 on `cursor/v2711-ccs-brce-next-bc54` against crown
`origin/cursor/ecosystem-spec-27-1-1-bc54` @ `750b8ef4a9042d22808a79be3dedff7944d4e321`:
exact **V7** crypto (**UNSUPPORTED**, court **ABSENT**) + Plane-5 DO (**BLOCKED**,
**V9**/G2-5 probe ladder) next verifiers filled; A2A `construct_hash_gate.rs` /
`lifecycle.rs` remain **ABSENT**. Cargo **not** re-run. No Plane-5 / cos / GCP /
WebAuthn-crypto / production WASM / zero-HITL crown claim is asserted as `ALIVE`.
