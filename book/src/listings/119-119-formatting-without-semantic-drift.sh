#!/usr/bin/env bash
set -euo pipefail

subject="119. Formatting without Semantic Drift"
before="$(mktemp -d)"
after="$(mktemp -d)"

ggen sync run
cp -R src "$before/"
cargo test --all-targets
ggen sync run
cp -R src "$after/"

diff -ru "$before/src" "$after/src"
printf 'verified: %s\n' "$subject"
