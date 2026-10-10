#!/usr/bin/env sh
set -eu
cargo install cargo-audit --locked
cargo install cargo-deny --locked
cargo install cargo-llvm-cov --locked
