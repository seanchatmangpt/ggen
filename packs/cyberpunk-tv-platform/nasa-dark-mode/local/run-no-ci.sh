#!/usr/bin/env bash
set -euo pipefail
PACK="$(cd "$(dirname "$0")/.." && pwd)"
EVIDENCE="$PACK/.ggen/evidence/no-ci"
mkdir -p "$EVIDENCE"
cd "$PACK"

node capsule.mjs
python3 local/package_roku.py
first_roku_sha="$(sha256sum generated/.ggen/evidence/nasa-dark-mode-roku.zip | awk '{print $1}')"
python3 local/package_roku.py
second_roku_sha="$(sha256sum generated/.ggen/evidence/nasa-dark-mode-roku.zip | awk '{print $1}')"
test "$first_roku_sha" = "$second_roku_sha"
(
  cd generated
  node verify/verify.mjs --verify-package .ggen/evidence/nasa-dark-mode-roku.zip
)

clang --target=wasm32 -O2 -nostdlib -Wl,--no-entry -Wl,--export=policy_version -Wl,--export=apply_remote \
  -o "$EVIDENCE/nasa-core.wasm" local/nasa_core.c
sed 's/mode = (mode + 1) & 3;/mode = mode;/' local/nasa_core.c > "$EVIDENCE/nasa-core-mutant.c"
clang --target=wasm32 -O2 -nostdlib -Wl,--no-entry -Wl,--export=policy_version -Wl,--export=apply_remote \
  -o "$EVIDENCE/nasa-core-mutant.wasm" "$EVIDENCE/nasa-core-mutant.c"
node local/verify_wasm.mjs "$EVIDENCE/nasa-core.wasm" "$EVIDENCE/nasa-core-mutant.wasm"
python3 local/verify_roku.py
xvfb-run -a -s '-screen 0 1920x1080x24' python3 local/verify_browser.py

set +e
(
  cd generated/browser
  timeout 20s npm install --offline --ignore-scripts --no-audit --no-fund
) >"$EVIDENCE/npm-offline.log" 2>&1
offline_status=$?
(
  cd generated/browser
  timeout 20s npm view deck.gl@9.1.14 version --fetch-retries=0 --fetch-timeout=3000
) >"$EVIDENCE/npm-registry.log" 2>&1
registry_status=$?
set -e

rust_status=0
if command -v rustc >/dev/null 2>&1 && command -v cargo >/dev/null 2>&1; then
  rust_standing=ALIVE
else
  rust_standing=BLOCKED_TOOLCHAIN_REQUIRED
  rust_status=127
fi
node - "$EVIDENCE" "$offline_status" "$registry_status" "$rust_status" <<'NODE'
const fs = require('fs');
const path = require('path');
const [dir, offline, registry, rust] = process.argv.slice(2);
const report = {
  schema: 'ggen.nasa-dark-mode.dependency-probes.v1',
  deckGl: {
    standing: Number(offline) === 0 || Number(registry) === 0 ? 'ALIVE' : 'BLOCKED_DEPENDENCY_TRANSPORT',
    pinnedVersion: '9.1.14',
    offlineInstallExit: Number(offline),
    registryProbeExit: Number(registry),
    offlineLog: fs.readFileSync(path.join(dir, 'npm-offline.log'), 'utf8').slice(-4000),
    registryLog: fs.readFileSync(path.join(dir, 'npm-registry.log'), 'utf8').slice(-4000)
  },
  rust: {
    standing: Number(rust) === 0 ? 'ALIVE' : 'BLOCKED_TOOLCHAIN_REQUIRED',
    probeExit: Number(rust)
  },
  physicalRoku: { standing: 'BLOCKED_DEVICE_REQUIRED' }
};
fs.writeFileSync(path.join(dir, 'dependency-probes.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify(report));
NODE
node local/aggregate.mjs
