#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS Chicago TDD 8^4 combinatorial innovation fabric'
printf '%s\n' '8 × 8 × 8 × 8 = 4,096 exhaustive scenario cells'
printf '%s\n' 'coverage ladder: 8 / 64 / 512 / 4,096'

cargo test \
  --test tcps_chicago_tdd_1000x \
  --test tcps_chicago_tdd_8pow4 \
  -- --nocapture
