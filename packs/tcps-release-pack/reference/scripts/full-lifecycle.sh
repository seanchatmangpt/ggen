#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
PY=${PYTHON:-python3}
"$PY" "$ROOT/tools/lifecycle.py" doctor
"$PY" "$ROOT/tools/lifecycle.py" verify
"$PY" "$ROOT/tools/lifecycle.py" matrix
HOST=$(rustc -vV | sed -n 's/^host: //p')
"$PY" "$ROOT/tools/lifecycle.py" build --target "$HOST" --product core
"$PY" "$ROOT/tools/lifecycle.py" build --target "$HOST" --product std
"$PY" "$ROOT/tools/lifecycle.py" build --target "$HOST" --product ffi
"$PY" "$ROOT/tools/lifecycle.py" build --target "$HOST" --product cli
"$ROOT/scripts/smoke/cli.sh"
"$ROOT/scripts/smoke/c-abi.sh"
if [ "${TCPS_WASM_SMOKE:-0}" = "1" ]; then "$ROOT/scripts/smoke/wasm-node.sh"; fi
"$PY" "$ROOT/tools/lifecycle.py" package --target "$HOST"
"$PY" "$ROOT/tools/lifecycle.py" sbom
"$PY" "$ROOT/tools/lifecycle.py" provenance
"$PY" "$ROOT/tools/lifecycle.py" checksums
"$PY" "$ROOT/tools/lifecycle.py" verify-release
