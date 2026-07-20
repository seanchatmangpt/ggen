#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
HOST=$(rustc -vV | sed -n 's/^host: //p')
rm -rf .lifecycle/repro-a .lifecycle/repro-b
CARGO_TARGET_DIR=.lifecycle/repro-a cargo build -p tcps-core --release --target "$HOST"
CARGO_TARGET_DIR=.lifecycle/repro-b cargo build -p tcps-core --release --target "$HOST"
A=$(find .lifecycle/repro-a -name 'libtcps_core*.rlib' -type f | head -1)
B=$(find .lifecycle/repro-b -name 'libtcps_core*.rlib' -type f | head -1)
[ -n "$A" ] && [ -n "$B" ] || { echo "rlibが見つかりません" >&2; exit 2; }
cmp "$A" "$B" || { echo "再現構築不一致" >&2; exit 1; }
echo "再現構築一致"
