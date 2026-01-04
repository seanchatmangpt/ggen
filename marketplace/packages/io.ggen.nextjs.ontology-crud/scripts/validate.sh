#!/usr/bin/env bash
set -euo pipefail

# Fast 80/20 validation for the Next.js ontology CRUD app:
# - install deps if missing
# - lint
# - production build (includes type check)

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

export NEXT_TELEMETRY_DISABLED=1
export CI=1

if [ ! -d node_modules ]; then
  if [ -f package-lock.json ]; then
    npm ci --prefer-offline --no-progress
  else
    npm install --prefer-offline --no-progress
    npm pkg set devDependencies.@typescript-eslint/eslint-plugin="^7.0.0" devDependencies.@typescript-eslint/parser="^7.0.0" >/dev/null 2>&1 || true
    npm install --prefer-offline --no-progress
  fi
fi

npm run lint
npm run build






