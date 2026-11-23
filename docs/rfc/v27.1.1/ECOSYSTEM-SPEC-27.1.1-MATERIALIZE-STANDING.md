# ECOSYSTEM-SPEC-27.1.1 — materialize verify standing (docs-only)

```text
crown_tip=f0a0e07bc860b2d170d34ef7b51c41e99341403b
branch=cursor/v2711-ecospec-materialize-verify-bc54
utc=2026-11-22T18:35Z
cargo=none (tip7 owns EXIT0)
ManagePullRequest=not_used
invented_ALIVE=none
```

| Field | Value |
|-------|-------|
| Document ID | `ECOSYSTEM-SPEC-27.1.1-MATERIALIZE-STANDING` |
| Spec path | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`f0a0e07bc860b2d170d34ef7b51c41e99341403b`** (`f0a0e07bc`; Gate023 header pin @ `4060f9d6b`) |
| Sidebranch | `cursor/v2711-ecospec-materialize-verify-bc54` |
| Cargo / tip-cargo | **NOT RUN** (docs-only; tip7 owns Gate1 EXIT0) |
| Spec status | **MATERIALIZED** (requirements binder present; **not** an execution receipt; **not** crown `ALIVE`) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** This ledger verifies that `ECOSYSTEM-SPEC-27.1.1` is on tip and that
named crown companions exist with honest standings. Presence of the binder or
this receipt does **not** promote photo / CCS·BRCE / ops / Foundry / Gate1 crowns
to `ALIVE`. Do not invent tip-subject tip-cargo `ALIVE` from tip6 PARTIAL /
EXIT **137** thrash.

---

## 1. Materialization check

| Check | Result |
|-------|--------|
| Spec file exists | **YES** — `docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md` |
| Header Status | **MATERIALIZED** (2026-11-22) |
| Crown objective body | **YES** — §13 photo / CCS·BRCE / ops / foundry |
| Retip honesty matrices | **YES** — §17 @ `9c56420dd`; §18 @ `c4e757a14` (tip citations **stale** vs live `f0a0e07bc`; standings **held** from companion ledgers) |

---

## 2. Required coverage → ledger → standing @ tip `f0a0e07bc`

| Required topic | Spec bind | Source-of-truth ledger | Standing (held; not re-run) |
|----------------|-----------|------------------------|-----------------------------|
| Photolithography path | §13.1 | [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | Path crown **PARTIAL** / **non-ALIVE**; tip6 Gate1 courts **PARTIAL_ALIVE** EXIT **137**; hop-4 igniter **ALIVE (lineage)**; hop-5 `--live` **BLOCKED_HITL** |
| CCS / BRCE / WebAuthn | §13.2 | [`CCS-BRCE-WEBAUTHN-STANDING.md`](./CCS-BRCE-WEBAUTHN-STANDING.md); [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | Clause-2 **PARTIAL_ALIVE**; **V8** **BLOCKED**; bodies **BLOCKED**; live WebAuthn **UNSUPPORTED**; fences ≠ Plane-5 DO `ALIVE` |
| O(N)→O(N²) ops surfaces | §13.3 (+ binder index gap closed this land) | [`OPS-SURFACES-ON2-STANDING.md`](./OPS-SURFACES-ON2-STANDING.md) | brand prod **UNSUPPORTED**; Smalt RAID **ALIVE (lineage)** ≠ Foundry; VW measure **ALIVE** / court **PARTIAL**; cos `/readyz` **BLOCKED** (**503**); revops **UNKNOWN** (**NXDOMAIN**) |
| Foundry finish spine | §13.4 | [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | Foundry finish **PARTIAL**; `--live` **BLOCKED_HITL**; **never** Foundry `ALIVE` |
| Gate1 falsifiers | §6–§10 / §17–§18; [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) | tip6 exact-tip ledgers + [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md) | Exact-tip Gate1 courts **PARTIAL** / **PARTIAL_ALIVE** (EXIT **137** thrash); lineage tip-cargo **ALIVE** held where named — **do not upgrade** |
| Gate0/2/3 OOR next verifiers | §3 Gate 0/2–3; §18 companions | [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | Matrix tip pin `4060f9d6b` ⊆ `f0a0e07bc`; cos **BLOCKED**; revops **UNKNOWN**; Foundry `--live` **BLOCKED_HITL**; **V8**/bodies **BLOCKED**; WebAuthn live **UNSUPPORTED** — **no** invented OOR `ALIVE` |

---

## 3. Binder gaps closed this land

| Gap | Pre-verify | Fix |
|-----|------------|-----|
| `OPS-SURFACES-ON2-STANDING.md` not indexed in §13.3 / §15 | Ledger **PRESENT** on tip (`e6d00965a`⊆); binder silent | Cite in §13.3 ops table + §15 companion artifacts |
| `GATE023-OOR-STANDING-MATRIX.md` only in §18 companion line | Matrix **PRESENT** (`f0a0e07bc`); missing from §15 index | Add §15 companion row |
| Live tip honesty pin vs §17/§18 matrix SHAs | Matrices cite `9c56420dd` / `c4e757a14` | This receipt pins **`f0a0e07bc`**; does **not** rewrite §17/§18 tables (standings held) |

No standing promotions. No cargo.

---

## 4. Tip6 PARTIAL — Gate1 remaining falsifier list (one line)

```text
Gate1 remaining (exact-tip PARTIAL/EXIT137; lineage ALIVE held — tip7 owns EXIT0): shui_public_prefix; taffy_aot_preflight; brand_toml completeness+rhythm; brand_toml hotswap; ui_layout digest; verify_witness_size tip-cargo; zcode_powl_ocel+remediation_bound — plus UNSUPPORTED: VW production runtime; brand production theme; zcode-cli ui-layout-court.ts
```

---

## 5. Compact return

```text
spec_path=docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md
materialized=YES
gaps=OPS-ON2+GATE023 index (closed); tip-matrix SHA lag held
side=cursor/v2711-ecospec-materialize-verify-bc54
crown_tip=f0a0e07bc
cargo=NOT_RUN
standing=MATERIALIZED+PARTIAL (no crown ALIVE)
```
