#!/usr/bin/env sh
set -eu
TOOLCHAIN=${TCPS_RUST_TOOLCHAIN:-1.97.1}
if ! command -v rustup >/dev/null 2>&1; then
  echo "rustupがありません。https://rustup.rs の組織承認済み導入手順を実行してください。" >&2
  exit 2
fi
rustup toolchain install "$TOOLCHAIN" --profile minimal --component rustfmt --component clippy --component rust-src
rustup override set "$TOOLCHAIN"
if [ "${TCPS_INSTALL_NIGHTLY:-0}" = "1" ]; then
  rustup toolchain install nightly --profile minimal --component rust-src
fi
python3 "$(dirname "$0")/../tools/lifecycle.py" doctor
