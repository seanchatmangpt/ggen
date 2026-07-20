#!/usr/bin/env bash
set -euo pipefail

subject="322. Laboratory Two: Add a Real Consumption Path"
before="$(mktemp -d)"
after="$(mktemp -d)"

ggen sync run
cp -R src "$before/"
cargo test --all-targets
ggen sync run
cp -R src "$after/"

diff -ru "$before/src" "$after/src"
printf 'verified: %s\n' "$subject"
