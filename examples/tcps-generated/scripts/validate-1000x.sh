#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS Chicago TDD 1000x validation fabric'
printf '%s\n' '43 capabilities × 4 surfaces × 7 laws = 1,204 obligations'

cargo test --test tcps_chicago_tdd_1000x -- --nocapture
