#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"
command -v node >/dev/null 2>&1 || { echo "nodeがありません" >&2; exit 2; }
python3 tools/lifecycle.py build --target wasm32-unknown-unknown --product wasm
node tests/wasm/smoke.mjs target/wasm32-unknown-unknown/release/tcps_wasm.wasm
