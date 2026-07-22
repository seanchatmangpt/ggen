#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS Auto Select 8^4 behavioral fabric'
printf '%s\n' '8 authority × 8 readiness × 8 time × 8 mode = 4,096 real judgments'
printf '%s\n' 'expected: 1,670 selected; 1,264 no-eligible; 1,162 no-ready'

cargo test \
  --test tcps_chicago_tdd_1000x \
  --test tcps_chicago_tdd_8pow4 \
  -- --nocapture
