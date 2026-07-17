#!/usr/bin/env bash
# scripts/ci/guard-publish-target.sh
#
# Enforces the two invariants FR-007 / User Story 2
# (specs/014-ggen-core-replacement/spec.md) actually require:
#   1. No workspace member other than the root package is ever named "ggen".
#   2. The vendored replacement-engine crates (which originated under a
#      colliding name -- see docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md)
#      can never be published.
#
# Scope note: this does NOT require `publish = false` on every workspace
# crate. Crates pre-existing before this migration (ggen-core, ggen-cli-lib,
# ggen-graph, ggen-config, ggen-marketplace, ggen-lsp, genesis-types-v2,
# genesis-core-v2, cpmp) already ship without a `publish = false` guard;
# closing that pre-existing gap is a separate, broader policy decision this
# ticket does not authorize.
set -euo pipefail
ALLOWED_PACKAGE="ggen"
VENDORED_ENGINE_MANIFESTS=(
  "crates/ggen-engine/Cargo.toml"
  "crates/praxis-core/Cargo.toml"
  "crates/praxis-graphlaw/Cargo.toml"
)

cargo publish --dry-run --package "$ALLOWED_PACKAGE" || {
  echo "FAIL: dry-run publish of '$ALLOWED_PACKAGE' itself failed." >&2
  exit 1
}

guard_status=0

# 1. Name-collision check, workspace-wide -- via `cargo metadata`'s own
#    [package].name resolution, not a naive first-match `name = "..."` grep
#    (which also matches [[bin]]/[[test]] target names -- e.g. ggen-cli's own
#    `[[bin]] name = "ggen"` at line 2, distinct from its real package name
#    `ggen-cli-lib` at line 29).
root_manifest="$(pwd)/Cargo.toml"
while IFS=$'\t' read -r name manifest_path; do
  if [ "$manifest_path" = "$root_manifest" ]; then
    continue
  fi
  if [ "$name" = "$ALLOWED_PACKAGE" ]; then
    echo "ERROR: $manifest_path declares name = \"$ALLOWED_PACKAGE\" -- collides with the root package." >&2
    guard_status=1
  fi
done < <(cargo metadata --no-deps --format-version=1 2>/dev/null | jq -r '.packages[] | [.name, .manifest_path] | @tsv')

# 2. publish=false check, scoped to the vendored replacement-engine crates.
for manifest in "${VENDORED_ENGINE_MANIFESTS[@]}"; do
  if ! grep -q '^publish *= *false' "$manifest"; then
    echo "ERROR: $manifest has no 'publish = false' -- required for vendored replacement-engine crates." >&2
    guard_status=1
  fi
done

# 3. Bin-target collision check, workspace-wide -- via `cargo metadata`'s own
#    [[bin]] target enumeration (`.packages[].targets[]`, filtered to
#    `kind == "bin"` and `name == "ggen"`), not a `grep '\[\[bin\]\]'` that would
#    also miss `autobins`-derived targets or double-count `[[test]]`/`[[bench]]`
#    entries that merely happen to be named "ggen". Regression guard for the
#    duplicate-bin issue fixed in commit 4f655b164e962519004692e941ecd78a7ce9c876's
#    predecessor 3862fe0008d1ce2bebcaef05594983ce9f476f8a ("fix(publish-safety):
#    remove root's duplicate ggen bin, mark ggen-cli-lib unpublishable"): before
#    that fix, BOTH the root `ggen` package (`[[bin]] name = "ggen"`, since
#    removed via `autobins = false`) and `ggen-cli-lib` declared a bin named
#    "ggen", racing for `target/debug/ggen`. Real per-package target enumeration
#    (not a trivial `count == 1` check) means this would have reported both
#    offending packages had it run against that pre-fix state.
#
#    The canonical owner today is `ggen-cli-lib` (crates/ggen-cli/Cargo.toml) --
#    confirmed via `cargo metadata` (2026-07-16) and via this justfile's own
#    `GGEN := "cargo run --bin ggen --"` variable, which resolves unambiguously
#    only because exactly one package owns that bin name. NOTE: this differs from
#    root owning the bin -- root is now `autobins = false` (pure library crate;
#    see 3862fe0008d1ce2bebcaef05594983ce9f476f8a's own Cargo.toml comment).
CANONICAL_GGEN_BIN_PACKAGE="ggen-cli-lib"
bin_hits="$(cargo metadata --no-deps --format-version=1 2>/dev/null \
  | jq -r '.packages[] | .name as $pkg | .manifest_path as $mp
           | .targets[] | select(any(.kind[]; . == "bin") and .name == "ggen")
           | [$pkg, $mp] | @tsv')"
bin_hit_count="$(printf '%s\n' "$bin_hits" | grep -c . || true)"
if [ "$bin_hit_count" -ne 1 ]; then
  echo "ERROR: expected exactly one workspace target with kind=[\"bin\"] name=\"ggen\", found $bin_hit_count:" >&2
  while IFS=$'\t' read -r pkg mp; do
    [ -n "$pkg" ] && echo "  - package \"$pkg\" ($mp)" >&2
  done <<< "$bin_hits"
  guard_status=1
else
  bin_owner="$(printf '%s\n' "$bin_hits" | cut -f1)"
  if [ "$bin_owner" != "$CANONICAL_GGEN_BIN_PACKAGE" ]; then
    echo "ERROR: the sole bin=\"ggen\" target belongs to \"$bin_owner\", expected \"$CANONICAL_GGEN_BIN_PACKAGE\"." >&2
    guard_status=1
  fi
fi

exit $guard_status
