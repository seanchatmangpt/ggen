#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in   thumbv6m-none-eabi thumbv7m-none-eabi thumbv7em-none-eabi thumbv7em-none-eabihf   thumbv8m.base-none-eabi thumbv8m.main-none-eabi thumbv8m.main-none-eabihf   riscv32imac-unknown-none-elf riscv64gc-unknown-none-elf   aarch64-unknown-none x86_64-unknown-none
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
done
