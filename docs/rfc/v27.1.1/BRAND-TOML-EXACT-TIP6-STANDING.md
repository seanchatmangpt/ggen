# brand.toml exact-tip6 — PARTIAL_ALIVE push-first honesty (v27.1.1)

```text
tip_base=552c85f291bae312677e4448e5afa87400b86632
court_tip=83c954e5fea4d454f3cc79410a368844a0469fb2
cited_tip_at_kickoff=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch=cursor/v2711-brand-exact-tip6-bc54
bc=bc-c92c420b-176d-5056-89e8-f4fa88893816
court=brand_toml_completeness_court + brand_toml_rhythm_court
budget_s=1500
exit=137
standing=PARTIAL_ALIVE
lineage_alive_tip=77eb46fd05d5bc847050a8f0a9147e1133e52c83
completeness_lineage=ALIVE (5)
rhythm_lineage=ALIVE (12)
exact_tip_reconfirm=PARTIAL (EXIT:137)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-brand-exact-tip6-bc54` |
| Agent | `bc-c92c420b-176d-5056-89e8-f4fa88893816` |
| Tip base (push) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `552c85f291bae312677e4448e5afa87400b86632` (live FF beyond kickoff cite `cc2a4ea66…` and court tip `83c954e5f…`) |
| Court tip (probe) | `83c954e5fea4d454f3cc79410a368844a0469fb2` — brand sources ≡ tip at push (completeness blob `a07477c6…`; rhythm blob `4843ed9f…`) |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-brand-exact-tip6` (isolated; seeded from `/tmp/ggen-brand-exact-tip` warm 4.1G) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:137** (SIGKILL) @ `2026-11-22T18:16:50Z` — cargo killed before any compile output; log empty (0 bytes). Quiet window never reached (6×15s polls; cargo≈8 / cc1plus≈3–4) |
| Thrash | concurrent `/tmp/gate1-exact-tip3/bc-fafc-*` courts; loadavg≈5.4–6.2 |
| Log / status | `/tmp/cursor/brand-exact-tip6.log` · `/tmp/cursor/brand-exact-tip6.status` → `BRAND_EXIT=137` |
| Lineage tip-cargo ALIVE | **held** — completeness **5** + rhythm **12**; EXIT 0 @ `77eb46fd0` (ancestor of tip) — **not** reconfirmed this tip6 |
| Prior retip5 | `cursor/v2711-brand-exact-retip5-bc54` — PARTIAL under &lt;3min budget; see [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) |
| Production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) — unchanged |
| Crown absorb | **none** (EXIT≠0; push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip6 push-first honesty: exclusive tip-cargo did **not** obtain EXIT 0 under pod thrash → standing **PARTIAL_ALIVE**. Lineage tip-cargo **ALIVE** @ `77eb46fd0` remains lineage evidence only. Do **not** invent tip-subject `ALIVE`, production theme standing, tip-wins crown absorb, or Foundry `ALIVE` from source presence / lineage alone.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries completeness (5) + rhythm (12) courts | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **137** (SIGKILL before first cargo log line; thrash n≈8) |
| Lineage tip-cargo ALIVE @ `77eb46fd0` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 552c85f29 (⊇ 83c954e5f ⊇ 77eb46fd0)
df -h /tmp
# Prefer quiet tip (thrash_n→0); exclusive jobs=1
CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked \
  --target-dir /tmp/ggen-brand-exact-tip6 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: completeness 5 + rhythm 12; EXIT 0 → tip-subject ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `77eb46fd0` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=552c85f291bae312677e4448e5afa87400b86632
court_tip=83c954e5fea4d454f3cc79410a368844a0469fb2
branch=cursor/v2711-brand-exact-tip6-bc54
bc=bc-c92c420b
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=SIGKILL_instant_under_thrash_n≈8
next=CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1
lineage_alive=77eb46fd05d5bc847050a8f0a9147e1133e52c83
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
