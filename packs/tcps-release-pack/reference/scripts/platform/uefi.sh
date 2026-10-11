#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in x86_64-unknown-uefi i686-unknown-uefi aarch64-unknown-uefi
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
done
