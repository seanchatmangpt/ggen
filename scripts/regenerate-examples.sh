#!/usr/bin/env bash
# Regenerate every live ggen example using the same discovery/admission contract
# enforced by scripts/validate-examples.sh.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec "$ROOT_DIR/scripts/validate-examples.sh" --write "$@"
