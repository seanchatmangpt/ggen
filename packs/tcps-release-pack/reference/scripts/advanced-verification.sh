#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
rustup toolchain install nightly --profile minimal --component miri --component rust-src
cargo +nightly miri test -p tcps-core
if cargo llvm-cov --version >/dev/null 2>&1; then
  cargo llvm-cov --workspace --all-features --lcov --output-path dist/lcov.info
else
  echo "cargo-llvm-covがないため被覆率工程を省略しました" >&2
fi
if cargo audit --version >/dev/null 2>&1; then
  cargo audit
else
  echo "cargo-auditがないため脆弱性台帳工程を省略しました" >&2
fi
if cargo deny --version >/dev/null 2>&1; then
  cargo deny check
else
  echo "cargo-denyがないため許可・供給元工程を省略しました" >&2
fi
