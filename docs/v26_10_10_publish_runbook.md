# v26.10.10 Publish Runbook

All six crates at 26.10.10. Verified dep graph (path+version edges require ancestors on registry first):

- `ggen-abb-sbb`: no internal deps (leaf)
- `ggen-config`: no internal deps (leaf)
- `ggen-marketplace` → `ggen-config ^26.10.8`
- `ggen-engine` → `ggen-abb-sbb`, `ggen-marketplace`, `ggen-config ^26.10.10`
- `ggen-graph`: `ggen-engine` is dev-dep only (stripped on package) — no ordering constraint
- root `ggen` → `ggen-config`, `ggen-marketplace` (workspace deps), bin-local
- `graphlaw` workspace req `26.10.10` (caret) resolves to registry `graphlaw 26.10.12` + `graphlaw-eyeron 0.7.8` (verified via index.crates.io) — nothing to publish there.

## Preconditions
1. Clean tree on main; all lanes landed and integrated.
2. Gates green: `just check`, `just test`, `just lint`, `just pre-commit` — record counts.
3. crates.io auth: token present in `~/.cargo/credentials.toml` (no builtin `cargo whoami`; first publish is the auth check — do the leaf dry-run first).

## HALT 1 — before first publish
Confirm gates + auth above; get operator go-ahead.

## Ordered publish (leaf-first)
Run from repo root, one at a time; after each, verify + sleep for registry lag:

```sh
cargo publish -p <crate> --dry-run   # before the real one
cargo publish -p <crate>
sleep 30
curl -sf https://crates.io/api/v1/crates/<crate>/<version> | jq -e .version
```

Order: `ggen-abb-sbb` → `ggen-config` (these two independent) → `ggen-marketplace` → `ggen-engine` → `ggen-graph` → `ggen` (root, last).

On failure: do not retry blind; read the error, fix forward, re-run the dry-run.

## Post-publish
1. `git tag -a v26.10.10` with message from `docs/v26_10_10_release_notes.md`; verify `git tag -v`/`git show v26.10.10`.
2. **HALT 2 — before `git push origin v26.10.10`.** Tag push is irreversible-promotion surface.
3. Ambient promotion: `just promote` (builds release `ggen`, installs to `~/.local/bin`, echoes `--version` — capture output).
4. File receipt: publish exit codes, per-crate registry verification output, tag SHA, promote `--version`, gate counts → `docs/release/`.
