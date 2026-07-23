#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS product-evidence fabric v5'
printf '%s\n' '43 contracts; 36 exact; 5 declared derivatives; 2 independent; 129 manifest entries'

cargo test --test tcps_chicago_tdd_1000x -- --nocapture
