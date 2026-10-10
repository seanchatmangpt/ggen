#!/usr/bin/env sh
set -eu
cargo fmt --all -- --check
cargo test --all-targets
cargo clippy --all-targets -- -D warnings
