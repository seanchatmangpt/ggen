#!/usr/bin/env bash
# eden-manufacture-demo.sh — F1: manufacture the custom AuthorityDelta Rust type
# from the admitted Eden authority graph (A = μ(O*)) via the manifest path.
#
# This is the "μ" half of Phase 3: where Phase 3 delivered O* (the admitted
# eden-authority.ttl graph), this projects the byte-class AuthorityDelta Rust
# TYPE out of it. The field set + Rust types are DERIVED from the ontology's
# eden:fieldName / eden:rustType annotations — not hard-coded in the template.
#
# Usage:  bash scripts/eden-manufacture-demo.sh
# Requires:  a built ggen binary (cargo build -p ggen-cli-lib --bin ggen).

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
GGEN="${GGEN:-$REPO_ROOT/target/debug/ggen}"
MANIFEST="$REPO_ROOT/.specify/specs/200-eden-authority/ggen.toml"
GEN="$REPO_ROOT/.specify/specs/200-eden-authority/generated/authority_delta.rs"

if [[ ! -x "$GGEN" ]]; then
  echo "REFUSED: ggen binary not found at $GGEN (run: cargo build -p ggen-cli-lib --bin ggen)"
  exit 64
fi

echo "== μ: ggen sync --manifest (graph → custom Rust type) =="
"$GGEN" sync --manifest "$MANIFEST" | grep -E '"status"|files_synced|inference_rules_executed|generation_rules_executed' || true

echo "== A: generated AuthorityDelta =="
cat "$GEN"
echo

# The type must be DERIVED from the ontology's eden:rustType annotations.
grep -q "pub struct AuthorityDelta" "$GEN"
grep -q "pub class_byte: u8," "$GEN"
grep -q "pub tick: u64," "$GEN"
grep -q "pub object_id: String," "$GEN"

# Determinism: a second run is byte-identical.
cp "$GEN" "$GEN.prev"
"$GGEN" sync --manifest "$MANIFEST" >/dev/null
if diff "$GEN.prev" "$GEN" >/dev/null; then echo "DETERMINISTIC: yes"; else echo "DETERMINISTIC: no"; rm -f "$GEN.prev"; exit 1; fi
rm -f "$GEN.prev"

echo "MANUFACTURE OK: AuthorityDelta projected from eden-authority.ttl (class_byte: u8, tick: u64)."
