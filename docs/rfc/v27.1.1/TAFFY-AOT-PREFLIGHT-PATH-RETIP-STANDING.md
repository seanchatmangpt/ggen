# Taffy AOT preflight — path honesty retip (v27.1.1)

```text
tip_base=a3e5d310940c995be428a7760bfa8d73d9dc4456
tip_note=⊇ e24ce7a732c18b13c279c7c1ec1cf42558ebc212+
branch=cursor/v2711-taffy-path-retip-bc54
bc=bc-0064f363-9ad8-5b37-8034-2da0b63190af
court=taffy_aot_preflight_court
package=ggen-graph
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
impl_blob=275ea25dd5a0082eb16b23c56ac494dfa3018165
tests=7
cargo=SKIPPED (docs-only; Gate1 tip6 owns thrash)
standing=PARTIAL / PARTIAL_ALIVE (path slice ggen+Taffy AOT; exact tip-cargo PENDING)
lineage_alive=19c08a3b0 / absorb cde02d1f2 (7 passed; exit 0)
path_crown=PARTIAL (not full photolithography path ALIVE)
foundry=PARTIAL (no Foundry ALIVE)
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-STANDING` |
| Side branch | `cursor/v2711-taffy-path-retip-bc54` |
| Tip base (crown) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a3e5d310940c995be428a7760bfa8d73d9dc4456` (⊇ `e24ce7a73`+) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 D2 / photolithography **hop 3** (`ggen + Taffy AOT`) |
| Companions | [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md); [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |
| Prior sidebranches | `cursor/v2711-taffy-preflight-retip7-bc54` @ `1bc005fe3` (**PRESENT**; tip-wins absorb `c2cd54f56`); `…-retip` @ `ef6fb84ed` (**PRESENT**); `…-retip3`/`…-retip4`/`…-retip6` **remote ABSENT** |
| Crown absorb | **ABSORBED** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` @ pre-absorb `a3e5d3109` (side `af9db256d`; FF) |
| ManagePullRequest | **not used** |
| Cargo | **NOT RUN** this land — Gate1 tip6 owns thrash; exact tip-cargo remains next verifier |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `PARTIAL` | `PRESENT` | `PENDING`.

**Authority.** This document is the push-first **path-slice** honesty ledger for
photolithography hop 3 (`ggen + Taffy AOT pre-flight`) on tip `a3e5d3109`
(⊇ `e24ce7a73`+). It does **not** invent exact-tip tip-cargo `ALIVE` on this tip.
Lineage tip-cargo **ALIVE** @ `19c08a3b0` / absorb `cde02d1f2` is **held**, not
re-proven here. It does **not** promote the full photolithography path crown or
Foundry / zero-HITL to `ALIVE`.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠
photolithography **path slice** `PARTIAL_ALIVE` ≠ photolithography **path crown**
`ALIVE` ≠ Foundry `ALIVE`.

---

## 1. Standing pin (this path retip @ `a3e5d3109`)

| Claim | Standing | Evidence (this land) |
|-------|----------|----------------------|
| Court source on tip | **PRESENT** | `git ls-tree a3e5d3109 crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → blob `d8923b07eb4325ace90087a604b9daec0a83d29e` (unchanged vs retip7) |
| Impl source on tip | **PRESENT** | `git ls-tree a3e5d3109 crates/ggen-graph/src/layout/taffy_aot.rs` → blob `275ea25dd5a0082eb16b23c56ac494dfa3018165` |
| Tip `#[test]` count | **7** | `rg -c '#\[test\]' crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → **7** |
| Exact tip-cargo on `a3e5d3109` | **PENDING** / **PARTIAL** | Cargo **SKIPPED** — docs-only land; Gate1 tip6 owns thrash (`n≈8` concurrent `cargo test`; loadavg_1m≈5.3). Budget rule held: no tip-cargo under thrash. |
| Lineage tip-cargo (held, not re-run) | **ALIVE** | **7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2` (`cursor/v2711-taffy-brand-absorb-a77d`; `/tmp/gate1-taffy`) — retained; **not** re-executed this land |
| Photolithography **hop-3 path slice** (`ggen+Taffy AOT`) | **PARTIAL_ALIVE** | In-repo fence lineage tip-cargo **ALIVE**; exact tip-cargo **PENDING**; consumer adapters **out-of-repo** still open — see [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 3 |
| Photolithography **path crown** | **PARTIAL** / **non-ALIVE** | Hop-3 slice **PARTIAL_ALIVE** ≠ full path (`perimeter→ttl/toml→ggen+Taffy→igniter→zcode`) crown `ALIVE` |
| Foundry / zero-HITL crown | **PARTIAL** (unchanged) | **No** Foundry `ALIVE` invented this land |

```text
tip_base=a3e5d310940c995be428a7760bfa8d73d9dc4456
tip_ancestor_pin=e24ce7a732c18b13c279c7c1ec1cf42558ebc212
present=yes
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
cargo=SKIPPED
thrash_n≈8
loadavg_1m≈5.3
lineage_alive_held=19c08a3b0/cde02d1f2
exact_tip_reconfirm=PENDING
standing=PARTIAL / PARTIAL_ALIVE
path_slice=ggen+Taffy AOT (hop 3)
path_crown=PARTIAL (not ALIVE)
foundry=PARTIAL (no Foundry ALIVE)
next=§3 exact verifier → EXIT 0
ManagePullRequest=not_used
crown_absorb=ABSORBED @ a3e5d3109←af9db256d
```

---

## 2. Prior retip7 (held; tip_base was `167352d0d`)

Retip7 push-first honesty (`cursor/v2711-taffy-preflight-retip7-bc54` @
`1bc005fe3`; tip-wins absorb `c2cd54f56`) pinned the same court blob
`d8923b07e` / 7 tests on tip `167352d0d`, cargo SKIPPED under thrash →
**PARTIAL**, lineage **ALIVE** held. That land refused crown absorb at first;
absorb `c2cd54f56` later landed the ledger on crown. This path retip **re-pins**
standing to live tip `a3e5d3109` (⊇ `e24ce7a73`+) without inventing tip-cargo
`ALIVE`.

| Sidebranch | `git ls-remote origin` |
|------------|------------------------|
| `cursor/v2711-taffy-preflight-retip-bc54` | **PRESENT** (`ef6fb84ed…`) |
| `cursor/v2711-taffy-preflight-retip7-bc54` | **PRESENT** (`1bc005fe3…`) |
| `…-retip3` / `…-retip4` / `…-retip6` | **ABSENT** |

**Path-retip law:** push-first docs-only. Gate1 tip6 owns tip-cargo thrash.
Exact tip-cargo remains the **next** verifier (§3), not a gate for remote
presence of this honesty ledger.

---

## 3. Exact next verifier (tip-cargo reconfirm → EXIT 0)

**Falsifier for exact tip-cargo `ALIVE` on live crown:** the command below must
exit **0** with **7 passed** against tip `a3e5d3109` (or a later tip whose
`taffy_aot` + court blobs remain `275ea25dd` / `d8923b07e`). Until then exact tip
reconfirm stays **PENDING** / aggregate fence standing for this retip stays
**PARTIAL** / path slice **PARTIAL_ALIVE** — lineage **ALIVE** held, not invented
as tip-subject EXIT:0.

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ a3e5d3109 (or successor with same court blob)
git ls-tree HEAD crates/ggen-graph/tests/taffy_aot_preflight_court.rs
# expect blob d8923b07eb4325ace90087a604b9daec0a83d29e

df -h /tmp
# Prefer quiet tip (thrash_n→0) + exclusive jobs=1 — Gate1 tip6 owns thrash
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-taffy \
  cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 7 passed; EXIT:0
```

Success promotes this ledger's exact tip reconfirm → **ALIVE (exact tip-cargo)**.
It still does **not** promote photolithography path crown or Foundry zero-HITL
to `ALIVE`. Path slice remains capped at hop-3 fence until consumer bind + path
crown verifiers close.

---

## 4. Compact return line

```text
tip_base=a3e5d310940c995be428a7760bfa8d73d9dc4456
tip_ancestor_pin=e24ce7a73+
branch=cursor/v2711-taffy-path-retip-bc54
bc=bc-0064f363
EXIT=skipped (PARTIAL; docs-only; tip6 thrash)
standing=PARTIAL / PARTIAL_ALIVE (PRESENT + lineage ALIVE held; exact tip PENDING; hop-3 path slice)
path_crown=PARTIAL (not full path ALIVE)
foundry=PARTIAL (no Foundry ALIVE)
next=CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked → EXIT 0
ManagePullRequest=not_used
```
