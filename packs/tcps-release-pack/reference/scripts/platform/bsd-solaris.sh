#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PY=${PYTHON:-python3}
for TARGET in x86_64-unknown-freebsd x86_64-unknown-netbsd x86_64-unknown-illumos x86_64-pc-solaris sparcv9-sun-solaris
 do
  "$PY" "$ROOT/tools/lifecycle.py" build --target "$TARGET" --product core || true
done
