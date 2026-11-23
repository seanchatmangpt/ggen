# Truex admit-court — exact-tip retip3 → tip-wins exact-tip4 absorb (v27.1.1)

```text
ggen_tip_before=49ebdb27c35ca8c666796482510de481fde7fcd9
sidebranch=cursor/v2711-truex-exact-retip3-bc54@eeabe900efb3e7a952514234aad29ab2f8dc08db
absorb_branch=cursor/v2711-truex-exact-retip4-absorb-5ce6
truex_sha=68d2d2331696f18949f4877b994320018b6365e5
tests=6/6
exit=0
standing=ALIVE (sibling admit-court; tip-wins absorb; not Foundry)
```

| Field | Value |
|-------|-------|
| Document ID | `TRUEX-ADMIT-EXACT-RETIP3-STANDING` (exact-tip4 tip-wins absorb) |
| Sidebranch | `cursor/v2711-truex-exact-retip3-bc54` @ `eeabe900efb3e7a952514234aad29ab2f8dc08db` (probe base was `8c8b1e6e5`) |
| Absorb branch | `cursor/v2711-truex-exact-retip4-absorb-5ce6` |
| Agent (retip3) | `bc-cdf903e5-cfdb-588e-863c-d95980ed6a2f` |
| Observed ggen tip (before absorb) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `49ebdb27c35ca8c666796482510de481fde7fcd9` (OOR retip17 tip-wins; ⊇ Foundry PARTIAL `8c8b1e6e5`) |
| Truex subject | `68d2d2331696f18949f4877b994320018b6365e5` (PR tip; [truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED**) |
| Mount | `/tmp/truex` (at subject SHA during court) |
| Command | `pnpm exec vitest run test/admit/admit-court.test.ts` |
| Result | **Test Files 1 passed; Tests 6 passed; EXIT 0** |
| Log (retip3) | `/tmp/gate1-exact-tip3/truex-admit-retip3.log` |
| Log (exact-tip4 reconfirm) | `/tmp/gate1-exact-tip4/truex-admit-retip4.log` |
| Status file | `/tmp/cursor/truex-admit-exact-retip4.status` → `EXIT:0` |
| Observed at (exact-tip4) | `2026-11-22T17:31:05Z` |
| Prior retip3 | sidebranch @ `eeabe900e` on tip then `8c8b1e6e5` — reapplied onto live tip `49ebdb27c` (OOR17 raced ahead) |
| Prior retip2 | `cursor/v2711-truex-exact-retip2-bc54` @ `798b29dc8` (tip then `2c144954c`) — superseded |
| Companion fence | ggen `shui_public_prefix_court` — **non-substitute**; separate sidebranch |
| Foundry / path-crown | **not claimed** — admit-court `ALIVE` ≠ Foundry `ALIVE` |
| Crown absorb | tip-wins via `cursor/v2711-truex-exact-retip4-absorb-5ce6` onto `49ebdb27c` (FF only; no force-push) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Sibling Truex Chicago admit-court EXIT **0** (6/6) at pinned SHA `68d2d233`, reconfirmed on live ggen tip `49ebdb27c` (exact-tip4). This does **not** mint Foundry `ALIVE`, photolithography path-crown `ALIVE`, or ggen fence tip-cargo `ALIVE`. Do not invent production / Foundry standing from this land.

---

## Replay

```bash
git -C /tmp/truex rev-parse HEAD
# expect: 68d2d2331696f18949f4877b994320018b6365e5
cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts
# expect: 6 passed; EXIT 0
```

---

## Tip-wins absorb (exact-tip4 — 2026-11-22T17:31:05Z)

| Field | Value |
|-------|-------|
| Tip before | `49ebdb27c35ca8c666796482510de481fde7fcd9` |
| Sidebranch content | `eeabe900efb3e7a952514234aad29ab2f8dc08db` (parent `8c8b1e6e5`; diverged from OOR17) |
| Reapply | cherry-pick onto live tip → exact-tip4 absorb branch |
| Admit-court reconfirm | EXIT **0** / **6 passed** @ `truex@68d2d233` — **ALIVE held** |
| Foundry | **not** promoted (stays PARTIAL / `--live` BLOCKED_HITL lineage) |
| Push policy | FF-only onto `cursor/ecosystem-spec-27-1-1-bc54`; **no** force-push; **no** ManagePullRequest |

---

## Compact return

```text
tip_before=49ebdb27c
sidebranch=cursor/v2711-truex-exact-retip3-bc54@eeabe900e
absorb=cursor/v2711-truex-exact-retip4-absorb-5ce6
truex=68d2d233 EXIT=0 tests=6/6 standing=ALIVE(admit-court) held
Foundry=NOT_CLAIMED ManagePullRequest=not_used force_push=none
```
