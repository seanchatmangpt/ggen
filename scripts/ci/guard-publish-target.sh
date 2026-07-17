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

exit $guard_status
