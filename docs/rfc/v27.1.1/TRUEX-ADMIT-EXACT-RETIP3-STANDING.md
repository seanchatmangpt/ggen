# Truex admit-court — exact-tip retip3 standing (v27.1.1)

```text
ggen_tip=8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26
truex_sha=68d2d2331696f18949f4877b994320018b6365e5
tests=6/6
exit=0
standing=ALIVE (sibling admit-court; not Foundry; not crown absorb)
```

| Field | Value |
|-------|-------|
| Document ID | `TRUEX-ADMIT-EXACT-RETIP3-STANDING` |
| Sidebranch | `cursor/v2711-truex-exact-retip3-bc54` |
| Agent | `bc-cdf903e5-cfdb-588e-863c-d95980ed6a2f` |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26` (Foundry PARTIAL honesty retip tip) |
| Truex subject | `68d2d2331696f18949f4877b994320018b6365e5` (PR tip; [truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED**) |
| Mount | `/tmp/truex` (at subject SHA during court) |
| Command | `pnpm exec vitest run test/admit/admit-court.test.ts` |
| Result | **Test Files 1 passed; Tests 6 passed; EXIT 0** |
| Log | `/tmp/gate1-exact-tip3/truex-admit-retip3.log` |
| Status file | `/tmp/cursor/truex-admit-exact-retip3.status` → `EXIT:0` |
| Observed at | `2026-11-22T17:28:24Z` |
| Prior retip2 | `cursor/v2711-truex-exact-retip2-bc54` @ `798b29dc8` (tip then `2c144954c`) — superseded as tip pin by this retip3 |
| Companion fence | ggen `shui_public_prefix_court` — **non-substitute**; separate sidebranch |
| Foundry / path-crown | **not claimed** |
| Crown absorb | **none** — sidebranch only |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Sibling Truex Chicago admit-court EXIT **0** (6/6) at pinned SHA `68d2d233`, reconfirmed on ggen tip `8c8b1e6e5`. This does **not** mint Foundry `ALIVE`, photolithography path-crown `ALIVE`, or ggen fence tip-cargo `ALIVE`. Do not invent production / Foundry standing from this land.

---

## Replay

```bash
git -C /tmp/truex rev-parse HEAD
# expect: 68d2d2331696f18949f4877b994320018b6365e5
cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts
# expect: 6 passed; EXIT 0
```

---

## Compact return

```text
branch=cursor/v2711-truex-exact-retip3-bc54
ggen_tip=8c8b1e6e5
truex=68d2d233 EXIT=0 tests=6/6 standing=ALIVE(admit-court)
Foundry=NOT_CLAIMED crown_absorb=REFUSED ManagePullRequest=not_used
```
