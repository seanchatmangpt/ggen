#!/bin/bash
export CARGO_REGISTRY_TOKEN="${CARGO_REGISTRY_TOKEN}"
CRATES=(
  "crates/ggen-utils"
  "crates/ggen-receipt"
  "crates/pictl-types"
  "crates/pictl-algos"
  "crates/a2a-generated"
  "crates/ggen-transport"
  "crates/ggen-canonical"
  "crates/ggen-semantic-bit"
  "crates/ggen-ontology-core"
  "crates/ggen-codegen"
  "crates/ggen-domain"
  "crates/ggen-marketplace"
  "crates/ggen-core"
  "crates/ggen-config"
  "crates/ggen-cli-validation"
  "crates/ggen-config-clap"
  "crates/ggen-a2a-registry"
  "crates/ggen-prompt-mfg"
  "crates/ggen-test-audit"
  "crates/ggen-test-opt"
  "crates/ggen-e2e"
  "crates/ggen-a2a"
  "crates/ggen-node"
  "crates/ggen-cli"
)

CHANGED=1
while [ $CHANGED -eq 1 ]; do
  CHANGED=0
  for crate in "${CRATES[@]}"; do
    if [ ! -d "$crate" ]; then continue; fi
    cd "$crate"
    echo "Attempting to publish $crate..."
    OUT=$(cargo publish --allow-dirty 2>&1)
    RES=$?
    if [ $RES -eq 0 ]; then
      echo "Success: $crate"
      CHANGED=1
      sleep 3
    elif echo "$OUT" | grep -q "is already uploaded"; then
      echo "Already uploaded: $crate"
    elif echo "$OUT" | grep -q "has no dependencies"; then
      echo "Failed: $crate - $OUT"
    else
      echo "Failed: $crate"
    fi
    cd - > /dev/null
  done
done
echo "Attempting to publish root ggen..."
cargo publish --allow-dirty