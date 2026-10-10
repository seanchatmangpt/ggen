#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in wasm32-unknown-unknown wasm32-wasip1 wasm32-wasip2 wasm32v1-none
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
  case "$TARGET" in
    wasm32v1-none) ;;
    *) "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product wasm || true ;;
  esac
done
