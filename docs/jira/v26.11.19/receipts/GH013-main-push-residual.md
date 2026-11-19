# GH013 / pre-push — ggen `origin/main` residual

Standing: BLOCKED

## Identity

| Field | Value |
|---|---|
| Date | 2026-11-19 |
| Subject tip | `d2d4988e6fb9575831a21decbea500ef58d28d19` |
| Lawful land | `origin/preserve/v26.11.16-main` (= tip) |
| Dogfood floor | `fd4f99bbd4971fb1ca5a0f3a3a2382aaba32261e` (ancestor of tip) |
| `origin/main` | `b21cbfa5a9312901be2915afef23fb880a308f69` (preserve ahead **135**) |

Dogfood law: preserve path is lawful. Do **not** fake `origin/main`.
No rebase / force-push / `--no-verify`.

## Exact block (two layers)

### A — GitHub ruleset (true GH013)

Repo ruleset `default` (id `14609371`), enforcement **active**, target
`~DEFAULT_BRANCH` (`main`):

- `pull_request` required (`required_approving_review_count: 0`)
- `non_fast_forward`
- `deletion`

Classic branch-protection API returns 404; **rulesets** enforce PR-only
direct pushes. Prior witness (session land):

```
PUSH_MAIN=rejected GH013 Changes must be made through a pull request
```

Even a locally green Full Tier push cannot update `refs/heads/main` without
a pull request. Opening/merging that PR is operator-owned (135 commits;
CI must pass; no bypass actors on the ruleset).

### B — Local Full Tier pre-push (`.git/hooks/pre-push`)

Hook runs **only** when `remote_ref == refs/heads/main`. Gates on a
`git archive` scratch under `~/.cache/ggen-pre-push/<sha12>-src`:

| # | Gate | Recipe |
|---|---|---|
| 1/4 | Check | `just check` → `timeout 300s cargo check --workspace` |
| 2/4 | Lint | `just lint` |
| 3/4 | Format | `just fmt-check` |
| 4/4 | Unit tests | `just test-lib` |

Witnessed refusal log:
`/Users/sac/srve/docs/jira/v26.11.19/receipts/L14-ggen-push-main.log`
(subject ref `31ba588e1424`, ancestor of tip):

```
Pre-Push Validation (Full Tier, ref 31ba588e1424)
  [1/4] Check... FAIL     # timeout 300s cargo check (cold scratch)
  [2/4] Lint... PASS
  [3/4] Format... PASS
  [4/4] Unit tests... FAIL
    error: could not find Cargo.toml in …/31ba588e1424-src
BLOCKED: pushed ref 31ba588e1424 failed gates. Push refused.
error: failed to push some refs to 'https://github.com/seanchatmangpt/ggen.git'
```

Host pressure at residual write: Data volume **~99%** full (~10 Gi free);
`~/.cache/ggen-pre-push` had grown to **~47 Gi** across stale SHA caches.
Stale caches (non-tip) purged this session so a future Full Tier attempt is
not doomed by ENOSPC — **does not** clear layer A (PR ruleset).

Full Tier was **not** re-run against tip this session: cold `cargo check` +
`test-lib` in a fresh scratch is unsafe at ~10 Gi free and is unnecessary
once layer A alone blocks `main`.

## Preserve / binary (closed)

| Check | Result |
|---|---|
| `origin/preserve/v26.11.16-main` | `d2d4988e6fb9575831a21decbea500ef58d28d19` |
| `fd4f99bbd` ancestor of preserve | yes |
| `git push origin HEAD:preserve/v26.11.16-main` | EXIT=0 (up-to-date) |
| `~/.local/bin/ggen` | `26.11.5`; sha256 matches `target/debug/ggen` built from tip |

## Unblock (operator)

1. Free disk to a durable margin **before** Full Tier (or rely on CI for the PR).
2. Open PR `preserve/v26.11.16-main` → `main` (ruleset path); wait required checks.
3. Merge without force-push / without ruleset bypass.
4. Retarget local `main` tracking after merge; keep preserve as dogfood land until then.

## Standing

**BLOCKED** — `origin/main` remains unupdated. Product twin + dogfood floor live
on preserve ≥ `fd4f99bbd` (tip `d2d4988e`). Wave COMPLETE (srve/fnox) is
unaffected under dogfood def.
