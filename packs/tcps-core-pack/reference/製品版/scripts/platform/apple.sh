#!/usr/bin/env sh
set -eu
[ "$(uname -s)" = "Darwin" ] || { echo "Apple SDK構築はmacOS上で実行してください" >&2; exit 2; }
xcode-select -p >/dev/null
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in   aarch64-apple-darwin x86_64-apple-darwin   aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios   aarch64-apple-tvos aarch64-apple-tvos-sim   aarch64-apple-watchos aarch64-apple-watchos-sim   aarch64-apple-visionos aarch64-apple-visionos-sim
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product ffi || true
done
