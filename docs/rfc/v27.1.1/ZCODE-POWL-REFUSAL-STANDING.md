# ZCODE / POWL bounded-refusal standing (photolithography clause 1 / hop 5)

```text
exit=0 standing=ALIVE(bounded_refusal_reconfirm)
sjira=6/6 bound=4/4 tip=01eccfffb56c1c59be67a220eadb5bd98783c806
agent=bc-d4e6c905 live=BLOCKED_HITL refusal_ALIVE≠live_ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-REFUSAL-STANDING-27.1.1` |
| Sidebranch | `cursor/v2711-zcode-refusal-reconfirm-bc54` |
| Agent | `bc-d4e6c905-2fa8-5da6-b274-104f7fe4fac3` |
| Ecosystem tip (observed) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `01eccfffb56c1c59be67a220eadb5bd98783c806` (⊇ prior `~f65434315`) |
| Photolithography | Clause 1 (§13.1) hop 5 — **bounded refusal** leg only |
| Prior ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ evidence tip `cd7c2433b` (`cursor/v2711-zcode-powl-refusal-alive-bc54` / `c91b22561` ⊆ tip) |
| Sibling reconfirm (already tip-wins) | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) @ `8bd3fe407` / `bc-934bba30` on tip `df9597c18` — EXIT **0**; absorbed onto crown |
| Court sources | unchanged vs `cd7c2433b` (`zcode_powl_ocel_court` / `zcode_powl_ocel` / `remediation_bound` — empty `git diff --stat`) |
| zcode-cli mount | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/zcode-refusal-reconfirm-target cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked --offline -- --test-threads=1` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib --locked --offline -- --test-threads=1` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-refusal-reconfirm-sjira-tip.log`, `/tmp/cursor/zcode-powl-refusal-reconfirm-bound-tip.log` |
| Observed at | `2026-11-22T16:37:57Z` |
| Standing (bounded refusal) | **ALIVE** (exact-tip reconfirm; in-repo refuse path only) |
| Prior stall ledger claim | **STALLED** / **SCHEDULED** — **cleared** by this EXIT **0** push |
| Foundry `--live` | **BLOCKED_HITL** — not re-probed; prior `Model access is not configured` held ([`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md)) |
| Clause-1 / hop-5 path crown | **PARTIAL_ALIVE** / **non-ALIVE** |
| Crown push | tip-wins absorb onto `cursor/ecosystem-spec-27-1-1-bc54` via `cursor/v2711-hitl-refusal-absorb-0f6f` (no force) |
| ManagePullRequest | **not used** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `SCHEDULED` | `STALLED`.

**Authority.** Exact tip-cargo replay of the documented refuse-path verifiers on tip `01eccfffb` produced EXIT **0**. Therefore bounded-refusal reconfirm standing is **ALIVE**. This does **not** mint hop-5 / foundry `--live` / photolithography path-crown `ALIVE`.

---

## 1. Standing matrix

| Claim | Standing | Evidence |
|-------|----------|----------|
| In-repo hop-5 contract court (`zcode_powl_ocel_court`) | **ALIVE** | **6 passed; exit 0** on tip `01eccfffb` |
| In-repo remediation bound vocab (`remediation_bound`) | **ALIVE** | **4 passed; exit 0** on tip `01eccfffb` |
| Bounded-refusal reconfirm (this land) | **ALIVE** | Both courts EXIT **0**; tip-wins absorb onto crown |
| Lineage tip-cargo @ `cd7c2433b` | **ALIVE** | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md); sources ≡ tip |
| Sibling reconfirm @ `df9597c18` | **ALIVE** | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) |
| Exact-tip reconfirm previously **STALLED** | **cleared** | Was RUNNING/`branchName=null`/remote ABSENT; now EXIT **0** + this push |
| Foundry `--live` / hop-5 crown | **BLOCKED_HITL** / **PARTIAL** | Unchanged; **refusal ALIVE ≠ `--live` ALIVE** |
| Photolithography path crown (hops 1–5) | **PARTIAL** / **non-ALIVE** | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) |

---

## 2. Documented verifier (narrowest refuse path)

From [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) §4.1 / [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# tip observed: 01eccfffb56c1c59be67a220eadb5bd98783c806

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/zcode-refusal-reconfirm-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked --offline -- --test-threads=1
# result: 6 passed; 0 failed; exit 0

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/zcode-refusal-reconfirm-target \
  cargo test -p ggen-graph remediation_bound --lib --locked --offline -- --test-threads=1
# result: 4 passed; 0 failed; exit 0
```

Diagnostic (sjira):

```text
running 6 tests
test clean_probe_exit_zero_admits_alive_receipt_shape ... ok
test court_nonzero_exit_is_refused ... ok
test igniter_subject_with_git_diff_exit_zero_still_mismatches_zcode ... ok
test unmounted_checkout_refuses_alive_promotion ... ok
test verifier_pins_chicago_court_and_predecessor_igniter ... ok
test wrong_subject_is_refused_not_silently_retargeted ... ok
test result: ok. 6 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Diagnostic (bound):

```text
running 4 tests
test layout::remediation_bound::tests::default_layout_budget_matches_zcode_ui_layout_court ... ok
test layout::remediation_bound::tests::exhausted_signature_budget_refuses_not_alive ... ok
test layout::remediation_bound::tests::write_scope_conflict_code_is_stable ... ok
test layout::remediation_bound::tests::zero_budget_is_explicit_unbounded_refusal ... ok
test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 40 filtered out
```

---

## 3. Exact next verifier (live / path crown — not this land)

```bash
# Founder HITL — provider creds required
# /login Coding Plan API key on Linux, or ~/.zcode/v2/provider_config.json
cd /agent/repos/zcode-cli
bun scripts/powl-foundry-loop.ts --live
# expect: foundry_exit=0 for hop-5 live ALIVE
```

Until then: hop-5 aggregate remains **PARTIAL_ALIVE**; photolithography path crown remains **non-ALIVE**.

---

## 4. Antifalsifiers

- Agent status **RUNNING** alone ≠ tip-cargo `ALIVE` (prior stall observation held until EXIT **0** + push)
- Lineage ALIVE @ `cd7c2433b` ≠ automatic exact-tip ALIVE without replay (this land supplies the replay)
- Bounded-refusal tip-cargo **ALIVE** ≠ foundry `--live` / hop-5 crown / path-crown `ALIVE`
- Sibling Chicago `bun test test/powl-ocel-chicago.test.ts` not claimed ALIVE from these in-repo courts
