#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
ROOT="$(pwd)"
LLBC_DIR="$ROOT/.aeneas"
MODEL_DIR="$ROOT/TCPSLifecycle/AeneasModel"
BINDING="$ROOT/TCPSLifecycle/AeneasBinding.lean"
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

status="extracted-unbound"
exit_code=2
if [[ -f "$BINDING" ]]; then
  if grep -Eq 'sorry|admit|axiom' "$BINDING"; then
    status="binding-refused-for-admission-token"
    exit_code=3
  elif grep -q 'AeneasModel' "$BINDING" \
    && grep -q 'rustStep_sound' "$BINDING" \
    && grep -q 'rustClassifyPhase' "$BINDING"; then
    status="refinement-bound-and-kernel-checked"
    exit_code=0
  else
    status="binding-refused-for-missing-symbol-link"
    exit_code=4
  fi
fi

python3 - "$LLBC_DIR/tcps-aeneas-kernel.llbc" "$MODEL_DIR" "$BINDING" "$RECEIPT" "$status" <<'PY'
import hashlib, json, pathlib, sys
llbc = pathlib.Path(sys.argv[1])
model_dir = pathlib.Path(sys.argv[2])
binding = pathlib.Path(sys.argv[3])
receipt = pathlib.Path(sys.argv[4])
status = sys.argv[5]
files = sorted(model_dir.glob('*.lean'))
h = hashlib.sha256()
h.update(llbc.read_bytes())
for path in files:
    h.update(path.name.encode())
    h.update(path.read_bytes())
if binding.exists():
    h.update(binding.name.encode())
    h.update(binding.read_bytes())
receipt.write_text(json.dumps({
    'schema': 'tcps-aeneas-refinement/v2',
    'status': status,
    'llbc': str(llbc),
    'lean_models': [str(path) for path in files],
    'binding': str(binding) if binding.exists() else None,
    'digest': h.hexdigest(),
    'semantic_targets': [
        'TCPSLifecycle.Refinement.rustStep_sound',
        'TCPSLifecycle.Refinement.rustClassifyPhase'
    ],
    'standing': 'pass' if status == 'refinement-bound-and-kernel-checked' else 'refused'
}, indent=2) + '\n')
PY

exit "$exit_code"
