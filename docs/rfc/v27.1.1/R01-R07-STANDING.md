# R-01 … R-07 resolution standing — ECOSYSTEM-SPEC-27.1.1 §11

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-STANDING-27.1.1` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1.md` §11 (D7 hard GA gate) |
| Observed base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `22d108f54` (2026-11-22) |
| Target GA | **2027-01-01** |
| Repo scope | `seanchatmangpt/ggen` — risk *bodies* are multi-repo; resolution *contract* is in-repo |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Do **not** invent risk bodies. Source presence of this standing file is not
closure. “Closed” without an admitted risk-register body is `REFUSED_UNGROUNDED`.

---

## 1. Resolution contract (in-repo — falsifiable)

| Subject | Evidence | Standing | Next exact verifier |
|---------|----------|----------|---------------------|
| §11 enumeration R-01…R-07 | `ECOSYSTEM-SPEC-27.1.1.md` §11 table | **PARTIAL_ALIVE** (spec text) | Keep IDs stable; refuse invented R-08+ via court |
| Closure rule: typed receipt **or** typed refusal before GA | Spec §11 “Closure rule” column | **PARTIAL_ALIVE** (doctrine) | Per-ID receipt/refusal after register admits body |
| Ungrounded “closed” → `REFUSED_UNGROUNDED` | `ggen_graph::ecosystem_risk::resolve_closure` + Chicago court | **PARTIAL_ALIVE** | `cargo test -p ggen-graph --test r01_r07_resolution_court` |
| Risk-register prose for each R-* | **Not mounted** in this checkout | **UNSUPPORTED** (bodies) | Admit sovereign risk register; materialize bodies without invention |

The executable court seals the **contract fence**, not per-risk closure.

```text
cargo test -p ggen-graph --test r01_r07_resolution_court
```

---

## 2. Crown matrix (R-01 … R-07 bodies — out-of-repo / BLOCKED)

Exact risk prose was **not** mounted as a sibling artifact (ECOSYSTEM-SPEC §11).
Each ID therefore stays **BLOCKED** for body closure until the admitted register
lands. Inventing bodies here would be `REFUSED_UNGROUNDED`.

| ID | Body standing | In-repo falsifiable? | Next exact verifier (through GA 2027-01-01) |
|----|---------------|----------------------|-----------------------------------------------|
| R-01 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-02 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-03 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-04 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-05 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-06 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |
| R-07 | **BLOCKED** | No (body unmounted) | Admit risk-register bodies for R-01…R-07 from the sovereign risk register (do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01 |

Do **not** promote any crown row to `ALIVE` from this repository while bodies are absent.

---

## 3. What this slice seals (and what it does not)

**Sealed (PARTIAL_ALIVE):**

- Standing census that R-01…R-07 remain open GA gates.
- Typed stub `ecosystem_risk` that refuses ungrounded closed claims.
- Chicago court binding stub ↔ ECOSYSTEM-SPEC §11 ↔ this standing file.

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
