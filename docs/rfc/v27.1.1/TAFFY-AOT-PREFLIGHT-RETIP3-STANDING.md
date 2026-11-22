# taffy_aot_preflight_court — retip3 honesty (v27.1.1)

```text
ggen_tip_base=e6d00965a0b0b96df9e284d857de1371c89a3dd2
cargo_tip_obs=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch_create_base=497db5eae5a86f68c823192504c4051ac5ae9f79
branch=cursor/v2711-taffy-preflight-retip3-bc54
bc=bc-dd12edfa-bea5-594c-8e87-b8fb6b7fa1c9
court=taffy_aot_preflight_court
package=ggen-graph
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
impl_blob=275ea25dd5a0082eb16b23c56ac494dfa3018165
tests=7
tip_cargo_exit=137
prior_exit=143
standing=PARTIAL_ALIVE (PRESENT + lineage ALIVE held; exact tip EXIT:0 not obtained)
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-RETIP3-STANDING` |
| Branch | `cursor/v2711-taffy-preflight-retip3-bc54` |
| Branch-create tip | `497db5eae5a86f68c823192504c4051ac5ae9f79` (initial `origin/cursor/ecosystem-spec-27-1-1-bc54` fetch) |
| Tip verified (docs land) | `e6d00965a0b0b96df9e284d857de1371c89a3dd2` (⊇ `c4e757a14`; court/impl blobs ≡ cargo obs) |
| Cargo tip observation | `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4` (sources ≡ through docs tip) |
| Observed at | `2026-11-22T18:22:02Z` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 D2 / photolithography hop 3 |
| Court blob on tip | `d8923b07eb4325ace90087a604b9daec0a83d29e` (**7** `#[test]`; ≡ lineage ALIVE) |
| Impl blob on tip | `275ea25dd5a0082eb16b23c56ac494dfa3018165` |
| Tip-cargo command | see §2 |
| Tip-cargo EXIT | **137** (SIGKILL under tip-exclusive guardian thrash while compiling `oxrocksdb-sys`) |
| Prior EXIT this land | **143** (SIGTERM mid-compile @ `/tmp/gate1-taffy` before guardian) |
| Lineage tip-cargo ALIVE (held) | **7 passed; EXIT 0** @ crown `19c08a3b0` absorb `cde02d1f2` — **not** re-proven as exact tip EXIT:0 |
| Photolithography path | remains **PARTIAL_ALIVE** (consumer bind open) — fence ≠ path crown |
| Crown absorb | **not performed** (OOR absorb may be running; leave crown alone) |
| ManagePullRequest | **not used** (#849 untouched) |

**Honesty law:** `PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠
photolithography path crown `ALIVE`. This land does **not** invent tip-cargo `ALIVE`
on `e6d00965a` / `cc2a4ea66` / `497db5eae`.

---

## 1. Standing pin

| Claim | Standing | Evidence |
|-------|----------|----------|
| Court source on tip | **PRESENT** | `git ls-tree e6d00965a crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → blob `d8923b07e…` |
| Impl source on tip | **PRESENT** | `git ls-tree …/taffy_aot.rs` → blob `275ea25dd…` |
| Tip `#[test]` count | **7** | `rg -c '#\[test\]' …/taffy_aot_preflight_court.rs` |
| Exact tip-cargo on live tip | **PARTIAL** / **not ALIVE** | Real verifier ran @ `cc2a4ea66` (blobs ≡ tip); **EXIT:137** (and earlier **EXIT:143**) — no `test result: ok` |
| Lineage tip-cargo | **ALIVE** (held) | **7** @ `19c08a3b0` / `cde02d1f2` |
| Aggregate fence standing | **PARTIAL_ALIVE** | PRESENT + lineage ALIVE + exact tip EXIT ≠ 0 |

```text
tip_base=e6d00965a0b0b96df9e284d857de1371c89a3dd2
cargo_tip_obs=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch_create_base=497db5eae5a86f68c823192504c4051ac5ae9f79
present=yes
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
EXIT=137
prior_EXIT=143
lineage_alive_held=19c08a3b0/cde02d1f2
exact_tip_reconfirm=NOT_ALIVE
standing=PARTIAL_ALIVE
next=§2 exact verifier → EXIT 0
ManagePullRequest=not_used
crown_absorb=none
```

---

## 2. Exact verifier (ran; EXIT 137)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# docs tip: e6d00965a… (branch created @ 497db5eae…; cargo obs @ cc2a4ea66; blobs ≡)
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --target-dir /tmp/gate1-exact-tip3/bc-fafc-exclusive-taffy-retip3 \
  --config build.jobs=1 -- --test-threads=1
# observed: EXIT:137 (SIGKILL; oxrocksdb mid-compile; tip-exclusive guardian thrash)
# expect for ALIVE: 7 passed; EXIT 0
```

Logs: `/tmp/cursor/taffy-preflight-retip3g.log` · status:
`/tmp/cursor/taffy-preflight-retip3g.status` · receipt:
`/tmp/cursor/taffy-preflight-retip3-receipt.md`.

Earlier attempt (pre-guardian): `CARGO_TARGET_DIR=/tmp/gate1-taffy` → **EXIT:143**.

---

## 3. Compact return

```text
base_tip=e6d00965a0b0b96df9e284d857de1371c89a3dd2
cargo_tip_obs=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch_create=497db5eae5a86f68c823192504c4051ac5ae9f79
branch=cursor/v2711-taffy-preflight-retip3-bc54
EXIT=137
standing=PARTIAL_ALIVE
lineage_alive=19c08a3b0/cde02d1f2
crown_absorb=none ManagePullRequest=not_used
```
