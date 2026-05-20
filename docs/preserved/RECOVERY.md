# Preserved Artifacts — Recovery Guide

This directory holds artifacts preserved before being removed from the live
working tree of ggen. Per repository preservation policy, no ggen source code
is lost even if we decide not to use it.

## Files

### `craftplan-local-d2e4c18.bundle`

A git bundle containing the local-only commit `d2e4c18 "Update craftplan"`
from the `vendors/craftplan` submodule. This commit existed only in the local
submodule checkout (not pushed to `https://github.com/puemos/craftplan.git`)
and was bundled before submodule removal for v26.5.19.

**Contents (commit `d2e4c1839934f8f650fb5e00291c2a4bb9d25ea2`, author Sean
Chatman, 2026-02-08):** Docker setup, integration test infrastructure, agent
behaviour module + four agents (catalog, inventory, orders, production),
web-layer agent controllers and socket handlers, deploy/dev/health/db-init
scripts, ~6000 lines of Elixir. Parent commit `11b3d0e` ("refactor(web):
replace direct Ash calls with domain code interfaces") is the merge-base with
upstream and is present in upstream's history (tag `v0.3.1`).

**Recover into a fresh craftplan clone:**

```bash
git clone https://github.com/puemos/craftplan.git /tmp/craftplan
cd /tmp/craftplan
git fetch /path/to/ggen/docs/preserved/craftplan-local-d2e4c18.bundle
git cherry-pick d2e4c18
```

## Submodule pins (removed in v26.5.19)

Five vendored git submodules were removed. All upstream provenance verified
prior to removal:

| Path | Pinned SHA | Upstream | Status |
|---|---|---|---|
| `external/unrdf` | `1b104cc98584e7a05b177bd109034affb478476f` | https://github.com/seanchatmangpt/unrdf.git | Reachable from `origin/chore/ggen-parent-snapshot-2026-03-31` |
| `vendors/a2a-rs` | `d509b2edd4cb8a5d37b0c4e2c0336dc82e9655b1` | https://github.com/EmilLindfors/a2a-rs.git | Reachable from `origin/master` |
| `vendors/craftplan` | `d2e4c1839934f8f650fb5e00291c2a4bb9d25ea2` | https://github.com/puemos/craftplan.git | **Local-only** — preserved as `craftplan-local-d2e4c18.bundle` |
| `vendors/cre` | `a5c8a9da6420f896ae311a763dada2695d388842` | https://github.com/seanchatmangpt/cre.git | Reachable from `origin/master` |
| `vendors/gen_pnet` | `f7f9a26d1f4f3e5aebcbc08a2e9729c3f1d7eeb2` | https://github.com/joergen7/gen_pnet.git | Reachable from `origin/master` |

To re-pin a submodule after recovery:

```bash
git submodule add <upstream-url> <path>
git -C <path> checkout <pinned-sha>
git add <path>
```

## Archive tags

Branches and worktrees removed for v26.5.19 are archived as annotated tags
under `archive/*`. List with:

```bash
git tag --list 'archive/*'
```

Resurrect a removed branch:

```bash
git checkout -b <branch-name> refs/tags/archive/<branch-name>-<YYYYMMDD>
```

The pre-removal submodule pin manifest is preserved in the annotation body
of `archive/submodules-pre-removal-<YYYYMMDD>`:

```bash
git show archive/submodules-pre-removal-<YYYYMMDD>
```
