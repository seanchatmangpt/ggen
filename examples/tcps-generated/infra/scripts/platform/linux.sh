#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in   x86_64-unknown-linux-gnu i686-unknown-linux-gnu aarch64-unknown-linux-gnu   x86_64-unknown-linux-musl aarch64-unknown-linux-musl   armv7-unknown-linux-gnueabihf riscv64gc-unknown-linux-gnu   powerpc64le-unknown-linux-gnu s390x-unknown-linux-gnu
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
done
