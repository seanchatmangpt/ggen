#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"
mkdir -p .lifecycle
HOST=$(rustc -vV | sed -n 's/^host: //p')
cargo build -p tcps-ffi --release --target "$HOST"
OUT="target/$HOST/release"
CC=${CC:-cc}
case "$(uname -s)" in
  Darwin)
    "$CC" tests/c/abi_smoke.c -Iinclude -L"$OUT" -ltcps_ffi -Wl,-rpath,"$OUT" -o .lifecycle/c-abi-smoke
    ;;
  *)
    "$CC" tests/c/abi_smoke.c -Iinclude -L"$OUT" -ltcps_ffi -Wl,-rpath,"$OUT" -o .lifecycle/c-abi-smoke
    ;;
esac
.lifecycle/c-abi-smoke
