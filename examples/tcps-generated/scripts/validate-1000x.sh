#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS product-evidence fabric'
printf '%s\n' '43 contracts; 37 exact; 4 declared derivatives; 2 independent; 129 manifest entries'

cargo test --test tcps_chicago_tdd_1000x -- --nocapture
