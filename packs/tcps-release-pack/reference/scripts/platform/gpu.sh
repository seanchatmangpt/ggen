#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in nvptx64-nvidia-cuda amdgcn-amd-amdhsa bpfel-unknown-none bpfeb-unknown-none
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
done
