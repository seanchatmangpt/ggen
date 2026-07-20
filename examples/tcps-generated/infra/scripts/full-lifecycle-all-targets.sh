#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
PY=${PYTHON:-python3}
LIFE="$ROOT/tools/lifecycle.py"

"$PY" "$LIFE" doctor
"$PY" "$LIFE" verify
"$PY" "$LIFE" matrix --output targets/target-matrix-active.json

# The universal no_std core is the only product meaningful on every built-in target.
"$PY" "$LIFE" build-all --tiers 1,2 --product core
"$PY" "$LIFE" build-all --tiers 3 --product core

HOST=$(rustc -vV | sed -n 's/^host: //p')
"$PY" "$LIFE" test --target "$HOST"
"$PY" "$LIFE" benchmark --iterations "${TCPS_BENCH_ITERATIONS:-1000000}"
for PRODUCT in std ffi cli; do
  "$PY" "$LIFE" build --target "$HOST" --product "$PRODUCT"
done
"$ROOT/scripts/smoke/cli.sh"
"$ROOT/scripts/smoke/c-abi.sh"
if [ "${TCPS_WASM_SMOKE:-0}" = "1" ]; then "$ROOT/scripts/smoke/wasm-node.sh"; fi
"$PY" "$LIFE" package --target "$HOST"

"$PY" "$LIFE" sbom
"$PY" "$LIFE" provenance
"$PY" "$LIFE" checksums
"$PY" "$LIFE" verify-release
