# R-01 … R-07 resolution standing — ECOSYSTEM-SPEC-27.1.1 §11

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-STANDING-27.1.1` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1.md` §11 (D7 hard GA gate) |
| Observed base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `fe72be0fb` (⊇ VW+Smalt + OOR retip11; contract evidence tip `e7569893a`) |
| Tip pin | 2026-11-22 — tip-wins absorb `cursor/v2711-brce-v8-bodies-bc54` @ `e54d58bac`: contract tip-cargo **ALIVE** retained; all seven *bodies* **BLOCKED** with owner/HITL next verifiers; **no** invented body `ALIVE`/`CLOSED` |
| Contract tip-cargo | **ALIVE** (exact tip-cargo) — `r01_r07_resolution_court` **7 passed; exit 0** @ `e7569893a` (`/tmp/ggen-gate1-brce-r01-target`); lineage `cd7c2433b` retained |
| Exact-tip reconfirm | **DONE** — EXIT **0** @ `e7569893a` ([`R01-R07-TIP-CARGO-STANDING.md`](./R01-R07-TIP-CARGO-STANDING.md)) |
| Bodies standing | **BLOCKED** (all seven) — OOR **G3-5** / RC-D7 |
| Target GA | **2027-01-01** |
| Repo scope | `seanchatmangpt/ggen` — risk *bodies* are multi-repo; resolution *contract* is in-repo |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Do **not** invent risk bodies. Source presence of this standing file is not
closure. “Closed” without an admitted risk-register body is `REFUSED_UNGROUNDED`.

**Exact next verifier (all seven bodies — `NEXT_VERIFIER_RISK_REGISTER`):**
Admit risk-register bodies for R-01…R-07 from the sovereign risk register
(do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01.

---

## 1. Resolution contract (in-repo — falsifiable)

| Subject | Evidence | Standing | Next exact verifier |
|---------|----------|----------|---------------------|
| §11 enumeration R-01…R-07 | `ECOSYSTEM-SPEC-27.1.1.md` §11 table | **PARTIAL_ALIVE** (spec text) | Keep IDs stable; refuse invented R-08+ via court |
| Closure rule: typed receipt **or** typed refusal before GA | Spec §11 “Closure rule” column | **PARTIAL_ALIVE** (doctrine) | Per-ID receipt/refusal after register admits body |
| Ungrounded “closed” → `REFUSED_UNGROUNDED` | `ggen_graph::ecosystem_risk::resolve_closure` + Chicago court | **ALIVE** (exact tip-cargo **7** @ `e7569893a`) | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court --locked -- --test-threads=1` |
| Risk-register prose for each R-* | **Not mounted** in this checkout | **UNSUPPORTED** (bodies) / crown rows **BLOCKED** | §2 — Founder HITL admits sovereign risk register; then per-ID typed receipt or `REFUSED_*` |

The executable court seals the **contract fence**, not per-risk closure.
Contract tip-cargo **ALIVE** ≠ any R-* body `ALIVE`/`CLOSED`.

```text
cargo test -p ggen-graph --test r01_r07_resolution_court
```

---

## 2. Crown matrix (R-01 … R-07 bodies — out-of-repo / BLOCKED)

Exact risk prose was **not** mounted as a sibling artifact (ECOSYSTEM-SPEC §11).
Each ID therefore stays **BLOCKED** for body closure until the admitted register
lands. Inventing bodies here would be `REFUSED_UNGROUNDED`.

| ID | Body standing | In-repo falsifiable? | Next exact verifier (through GA 2027-01-01) | Owner / HITL |
|----|---------------|----------------------|-----------------------------------------------|--------------|
| R-01 | **BLOCKED** | No (body unmounted) | (1) Founder HITL admits sovereign risk-register path (do **not** invent prose). (2) Materialize R-01 body under that admitted path. (3) File typed receipt **or** typed `REFUSED_*` before **2027-01-01**. Contract replay alone does **not** close: `cargo test -p ggen-graph --test r01_r07_resolution_court`. OOR **G3-5**. | Risk-register owner (multi-repo); **Founder HITL** |
| R-02 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-02** (admit → typed receipt/`REFUSED_*`; no invention). | Risk-register owner; **Founder HITL** |
| R-03 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-03**. | Risk-register owner; **Founder HITL** |
| R-04 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-04**. | Risk-register owner; **Founder HITL** |
| R-05 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-05**. | Risk-register owner; **Founder HITL** |
| R-06 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-06**. | Risk-register owner; **Founder HITL** |
| R-07 | **BLOCKED** | No (body unmounted) | Same ladder as R-01 for ID **R-07**. | Risk-register owner; **Founder HITL** |

Do **not** promote any crown row to `ALIVE` from this repository while bodies are absent.
Silent “closed” without admitted body → `REFUSED_UNGROUNDED`.

---

## 3. What this slice seals (and what it does not)

**Sealed (contract fence ALIVE lineage tip-cargo; bodies BLOCKED):**

- Standing census that R-01…R-07 remain open GA gates (bodies).
- Typed stub `ecosystem_risk` that refuses ungrounded closed claims.
- Chicago court binding stub ↔ ECOSYSTEM-SPEC §11 ↔ this standing file — tip-cargo **7 passed; exit 0** @ `e7569893a` (lineage `cd7c2433b`). Exact-tip reconfirm **DONE**. Bodies still **BLOCKED**.

**Explicitly not sealed:**

- Risk-body text for any R-*.
- Per-risk typed receipt or typed refusal that would close a GA gate.
- Multi-repo closure evidence (D7).

---

## 4. GA ladder

| Checkpoint | Required standing move |
|------------|------------------------|
| Register admit | Risk bodies materialize under an admitted path; `risk_body_admitted=true` becomes lawful |
| Per-ID close | Each R-* gains typed receipt **or** typed `REFUSED_*` — never silent-close |
| GA 2027-01-01 | Any R-* still without typed closure remains a hard GA blocker (D7) |

---

## 5. Explicit non-claims

- No risk-register file was invented or transcribed from memory.
- No R-* is claimed `ALIVE` or `CLOSED`.
- Gate-1 layout / `shui:` / brand courts are **not** remapped onto R-01…R-07 here.

---

## 6. Replay

```text
test -f docs/rfc/v27.1.1/R01-R07-STANDING.md
rg -n 'BLOCKED|REFUSED_UNGROUNDED|R-0[1-7]' docs/rfc/v27.1.1/R01-R07-STANDING.md
cargo test -p ggen-graph --test r01_r07_resolution_court
```

---

## 7. Provenance

Materialized as the R-01…R-07 resolution companion to ECOSYSTEM-SPEC-27.1.1 §11.
In-repo falsifier covers the resolution contract only; crown body rows stay
`BLOCKED` with the shared risk-register next verifier until admitted prose lands.

---

## 8. Tip-cargo receipt (2026-11-22)

| Field | Value |
|-------|-------|
| Subject SHA | `cd7c2433b` (ancestor of live crown) |
| Target | `/tmp/ggen-r01-r07-tip` |
| Result | **7 passed; 0 failed; EXIT:0** |
| Standing | **ALIVE (lineage tip-cargo)** |
| Bodies | still **BLOCKED** (do not invent closure) |
| Kill log | dual thrash PID 1065550 SIGTERM — `/cursor/stores/self/r01-dual-thrash-kill-20261122.md` |
| Receipt | `/tmp/cursor/r01-r07-tip-cargo-result.md` |

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-r01-r07-tip \
  cargo test -p ggen-graph --test r01_r07_resolution_court --locked --config build.jobs=1 -- --test-threads=1
```
