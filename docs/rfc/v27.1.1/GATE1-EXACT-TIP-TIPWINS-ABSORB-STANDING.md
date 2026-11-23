# Gate1 exact-tip tip-wins absorb — honesty ledger (v27.1.1)

```text
tip_before_known=881f2ec168651c8634af41fee8a39106f8ead2b6
tip_before_attempt=cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3
tip_before_absorb=31b495a6ae2a6989f2021f088410fea07f78cd8f
branch=cursor/ecosystem-spec-27-1-1-bc54
method=cherry-pick tip-wins (--theirs on standing md modify/delete)
ManagePullRequest=not_used
force_push=none
Foundry=BLOCKED_HITL (held; not invented ALIVE)
```

| # | Side | Tip SHA | Standing | Notes |
|---|------|---------|----------|-------|
| 1 | `cursor/v2711-shui-exact-tip3-bc54` | `123d47fe85fa255a80eb6005e5a7c112c3ee8d36` | **ALIVE** exact tip-cargo (4/4 EXIT 0) | Court blob `3cda065f`; tip subject `3afe4545e` |
| 2 | `cursor/v2711-brand-exact-retip3-bc54` | `c532a3ca4a9748f577c085902cc4551638bc6b70` | **ALIVE** exact tip-cargo (5+12 EXIT 0) | Production theme **UNSUPPORTED** |
| 3 | `cursor/v2711-taffy-preflight-retip5-bc54` | `2dd03acae3fc278319b5446345f7fe0a819721ca` | **ALIVE** exact tip-cargo (7 EXIT 0) | Path-crown not invented |
| 4 | `cursor/v2711-uilayout-exact-retip3-bc54` | `4f5b54fb1c88a69bf897e3fcdf2edbf51c68a9e8` | **ALIVE** exact tip-cargo (11 EXIT 0) | Sibling TS **UNSUPPORTED** |
| 5 | `cursor/v2711-truex-exact-retip2-bc54` | `0d3e5d232df80457e9cbdb0d2cd2b8e1d6984476` | **ALIVE** sibling admit-court (6/6 EXIT 0) | Not ggen fence tip-cargo ALIVE |
| 6 | `cursor/v2711-zcode-powl-retip4-bc54` | `124d1941bbefa59ec4229ce1b95cf4fe9622d294` | **ALIVE** exact tip-cargo bounded refusal (6+4 EXIT 0) | Foundry `--live` **BLOCKED_HITL** held |
| 7 | `cursor/v2711-vw-ci-wasm-retip3-bc54` | `4e7d0ad47783a50afaec4d37c923d59f4fe59223` | size **ALIVE** (8 EXIT 0); prod **UNSUPPORTED** | size≠env≠affidavit≠production |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Docs-only tip-wins absorb of Gate1 exact-tip EXIT0 sides onto crown.
Do **not** invent Foundry `--live` `ALIVE`. Do **not** collapse honesty classes.

## Honesty (held)

1. **tip-cargo ALIVE ≠ live-tip reconfirm.** Each side's EXIT 0 is exact tip-cargo
   at the side's observed tip subject (or sibling admit-court for Truex). Absorbing
   the standing onto crown does **not** reconfirm the court at the post-absorb crown
   tip. Live-tip reconfirm remains a separate verifier.
2. **tip7 EXIT137 PARTIAL ≠ tip3 ALIVE tip-cargo.**
   [`SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md)
   stays **PARTIAL_ALIVE** (EXIT 137). Tip3 ALIVE tip-cargo
   ([`SHUI-PUBLIC-PREFIX-EXACT-TIP3-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP3-STANDING.md))
   does **not** rewrite tip7 PARTIAL. Known crown tip `881f2ec16` carried tip7
   PARTIAL honesty; tip3 ALIVE is additive, not a substitute.
3. **Foundry stays BLOCKED_HITL.** Side 6 zcode refusal ALIVE ≠ Foundry `--live`
   ALIVE. No Foundry crown ALIVE invented from this absorb.

## Absorbed standing files

- `SHUI-PUBLIC-PREFIX-EXACT-TIP3-STANDING.md`
- `BRAND-TOML-EXACT-RETIP3-STANDING.md`
- `TAFFY-AOT-PREFLIGHT-RETIP5-STANDING.md`
- `UILAYOUT-EXACT-RETIP3-STANDING.md`
- `TRUEX-ADMIT-EXACT-RETIP2-STANDING.md`
- `ZCODE-POWL-RETIP4-STANDING.md`
- `VERIFY-WITNESS-CI-WASM-RETIP3-STANDING.md`

## Replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git log --oneline cfd93c1cb..origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip-wins cherry-picks for the seven sides + this honesty ledger
```
