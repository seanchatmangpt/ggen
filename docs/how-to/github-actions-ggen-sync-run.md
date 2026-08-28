# Run `ggen sync run` through GitHub Actions

This repository exposes `.github/workflows/ggen-sync-run.yml` as the GitHub-native execution rail for deterministic GGen manufacture.

The rail deliberately separates construction from publication:

```text
caller repository @ exact SHA
        +
GGen release asset @ expected SHA-256
        +
ggen.toml [packs] entries pinned to exact ggen-marketplace commit SHAs
        |
        v
reusable workflow (contents: read)
        |
        v
ggen sync run
        |
        +--> generated worktree
        +--> ggen.lock / intrinsic GGen receipt
        +--> tracked.patch + untracked-output.tar.gz
        +--> GitHub execution receipt artifact
```

The reusable workflow never commits or pushes. A separate authorized publication job may consume the replay artifact after verification. This keeps `CONSTRUCT` (`ggen sync run`) separate from repository `DO` authority.

## Consumer manifest

A consumer declares existing marketplace packs directly through GGen's native Git pack reference. The `version` must be a full 40-hex commit SHA and the `subdir` must resolve under `packs/`.

```toml
[project]
name = "consumer"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[packs]
castle = { git = "https://github.com/seanchatmangpt/ggen-marketplace.git", version = "4c4232515b43d40cef8288c43eacfab2c31ab485", subdir = "packs/castle-pack" }
```

The workflow refuses path packs, mutable branch/tag pack pins, alternate Git hosts, and Git repositories other than `seanchatmangpt/ggen-marketplace`.

`ggen sync run` remains the only GGen manufacturing command. GGen itself clones the exact pack commit, validates `pack.toml`/ontology/templates, computes the pack content hash, and records lock state.

## Caller workflow

Pin the reusable workflow by a full GGen repository commit SHA. Do not call it through `main` or a mutable tag.

```yaml
name: Manufacture

on:
  workflow_dispatch:

permissions:
  contents: read

jobs:
  sync:
    permissions:
      contents: read
    uses: seanchatmangpt/ggen/.github/workflows/ggen-sync-run.yml@<FULL_GGEN_COMMIT_SHA>
    with:
      ggen_release: v26.8.27
      ggen_asset_sha256: ab442ced90a9836fd4eb07a5d61eb58293843cd515d864699fc0d0453444a035
      working_directory: .
      dry_run: false
```

The SHA-256 above is the GitHub Release asset digest for `ggen-x86_64-unknown-linux-gnu.tar.gz` in release `v26.8.27`. When selecting another release, pin the exact release tag and its exact Linux x86_64 asset digest together.

## Admission policy

Before execution, the workflow fails closed unless all of these are true:

- `working_directory` stays inside the caller checkout and contains `ggen.toml`.
- the GGen release asset SHA-256 is explicitly supplied and matches the downloaded bytes;
- at least one `[packs]` entry exists;
- every pack uses `https://github.com/seanchatmangpt/ggen-marketplace.git`;
- every pack `version` is an exact 40-hex Git commit SHA;
- every pack `subdir` is a relative `packs/<name>` path.

The checkout uses `persist-credentials: false`, and the reusable workflow has only `contents: read`. Pack resolution therefore does not inherit repository write authority.

## Evidence artifact

Every run uploads `ggen-sync-<run-id>-<attempt>` even when `ggen sync run` fails. The artifact contains, as available:

- `github-sync-receipt.json` — caller SHA, workflow ref, GGen identities, admitted pack pins, exit code, and changed standing;
- `packs.json` — normalized marketplace pack identities;
- `stdout.log` / `stderr.log` / `sync-exit-code.txt`;
- `ggen.lock` and `.ggen/receipts/latest.json` when emitted by GGen;
- `tracked.patch` and `diff-stat.txt`;
- `untracked-output.tar.gz`, excluding the `.ggen-v2` Git-pack transport cache;
- `git-status.txt`.

The workflow exposes `changed`, `sync_exit_code`, `ggen_binary_sha256`, and `receipt_sha256` as reusable-workflow outputs.

## Executable contract

`.github/workflows/ggen-sync-run-selftest.yml` calls the reusable workflow against `tests/fixtures/github-sync-run/`. That fixture uses the existing `github-actions-pack` from `ggen-marketplace` at an exact commit SHA and supplies RDF workflow facts as the consumer input.

The contract is intentionally end-to-end: GitHub checks out the caller, downloads and hash-verifies the released GGen binary, admits the pinned marketplace reference, and then executes the real `ggen sync run` boundary. A static YAML parse is not accepted as proof of this path.
