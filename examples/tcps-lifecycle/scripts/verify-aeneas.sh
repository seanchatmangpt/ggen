#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
ROOT="$(pwd)"
LLBC_DIR="$ROOT/.aeneas"
MODEL_DIR="$ROOT/TCPSLifecycle/AeneasModel"
RECEIPT="$ROOT/receipts/aeneas.json"
mkdir -p "$LLBC_DIR" "$MODEL_DIR" "$ROOT/receipts"

: "${CHARON_BIN:=charon}"
: "${AENEAS_BIN:=aeneas}"
command -v "$CHARON_BIN" >/dev/null
command -v "$AENEAS_BIN" >/dev/null

(
  cd aeneas-kernel
  "$CHARON_BIN" cargo --preset=aeneas --dest-file="$LLBC_DIR/tcps-aeneas-kernel.llbc"
)

find "$MODEL_DIR" -maxdepth 1 -type f -name '*.lean' -delete
(
  cd "$MODEL_DIR"
  "$AENEAS_BIN" -backend lean -split-files "$LLBC_DIR/tcps-aeneas-kernel.llbc"
)

test -n "$(find "$MODEL_DIR" -maxdepth 1 -type f -name '*.lean' -print -quit)"
lake build

python3 - "$LLBC_DIR/tcps-aeneas-kernel.llbc" "$MODEL_DIR" "$RECEIPT" <<'PY'
import hashlib, json, pathlib, sys
llbc = pathlib.Path(sys.argv[1])
model_dir = pathlib.Path(sys.argv[2])
receipt = pathlib.Path(sys.argv[3])
files = sorted(model_dir.glob('*.lean'))
h = hashlib.sha256()
h.update(llbc.read_bytes())
for path in files:
    h.update(path.name.encode())
    h.update(path.read_bytes())
receipt.write_text(json.dumps({
    'schema': 'tcps-aeneas-refinement/v1',
    'status': 'extracted-and-kernel-checked',
    'llbc': str(llbc),
    'lean_models': [str(path) for path in files],
    'digest': h.hexdigest(),
    'semantic_target': 'TCPSLifecycle.Refinement.rustStep_sound',
    'remaining_obligation': 'bind Aeneas-generated step symbol to rustStep_sound without admitted axioms'
}, indent=2) + '\n')
PY
