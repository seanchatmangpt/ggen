#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
python3 tools/lifecycle.py verify
MODE=--dry-run
[ "${TCPS_PUBLISH:-0}" = "1" ] && MODE=
for PACKAGE in tcps-core tcps-std tcps-ffi tcps-wasm tcps-cli; do
  echo "公開検査: $PACKAGE"
  # shellcheck disable=SC2086
  cargo publish -p "$PACKAGE" $MODE
  if [ "${TCPS_PUBLISH:-0}" = "1" ]; then
    echo "crates.io索引反映待機: $PACKAGE"
    sleep "${TCPS_PUBLISH_WAIT_SECONDS:-30}"
  fi
done
