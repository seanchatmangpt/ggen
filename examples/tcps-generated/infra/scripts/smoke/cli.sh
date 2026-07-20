#!/usr/bin/env sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"
OUTPUT=$(cargo run -p tcps-cli --bin tcps -- 検査)
printf '%s\n' "$OUTPUT"
printf '%s' "$OUTPUT" | grep '成立:' >/dev/null
