#!/usr/bin/env bash
# guard-version-consistency — fail if the workspace version has drifted between its three
# declared sources of truth:
#   .specify/repo-facts.ttl  rf:workspaceVersion "X.Y.Z" ;
#   Cargo.toml               [workspace.package] version = "X.Y.Z"
#   ggen.toml                [project] version = "X.Y.Z"
#
# Part of packs/ggen-release-pack's publish-candidate workflow: this must pass before a
# v$VERSION tag is created from Cargo.toml's version, otherwise the tag can name a version
# that .specify/repo-facts.ttl or ggen.toml disagree with.
set -euo pipefail

REPO_FACTS=".specify/repo-facts.ttl"
CARGO_TOML="Cargo.toml"
GGEN_TOML="ggen.toml"

for f in "$REPO_FACTS" "$CARGO_TOML" "$GGEN_TOML"; do
  if [ ! -f "$f" ]; then
    echo "BUILD_BROKEN: $f not found"
    exit 1
  fi
done

repo_facts_version="$(grep -m1 'rf:workspaceVersion' "$REPO_FACTS" | sed -E 's/.*rf:workspaceVersion *"([^"]+)".*/\1/')"
cargo_version="$(awk '/^\[workspace.package\]/{f=1} f && /^version/{print; exit}' "$CARGO_TOML" | sed -E 's/version *= *"([^"]+)"/\1/')"
ggen_toml_version="$(awk '/^\[project\]/{f=1} f && /^version/{print; exit}' "$GGEN_TOML" | sed -E 's/version *= *"([^"]+)"/\1/')"

echo "repo-facts.ttl rf:workspaceVersion = ${repo_facts_version:-<missing>}"
echo "Cargo.toml [workspace.package] version = ${cargo_version:-<missing>}"
echo "ggen.toml [project] version = ${ggen_toml_version:-<missing>}"

if [ -z "$repo_facts_version" ] || [ -z "$cargo_version" ] || [ -z "$ggen_toml_version" ]; then
  echo "BUILD_BROKEN: one or more version fields could not be parsed"
  exit 1
fi

if [ "$repo_facts_version" != "$cargo_version" ] || [ "$cargo_version" != "$ggen_toml_version" ]; then
  echo "BUILD_BROKEN: version drift detected — repo-facts=${repo_facts_version} cargo=${cargo_version} ggen.toml=${ggen_toml_version}"
  exit 1
fi

echo "ALIVE: version consistent across repo-facts.ttl, Cargo.toml, ggen.toml (${cargo_version})"
