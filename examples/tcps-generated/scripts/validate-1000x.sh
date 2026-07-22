#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

printf '%s\n' 'TCPS product-evidence fabric'
printf '%s\n' '43 capability contracts; 41 manifest-bound; 129 manifest entries'

cargo test --test tcps_chicago_tdd_1000x -- --nocapture
