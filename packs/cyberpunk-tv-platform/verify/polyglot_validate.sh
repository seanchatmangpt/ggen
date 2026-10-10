#!/usr/bin/env bash
set -euo pipefail
SOURCE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GENERATED="${1:-$SOURCE/generated}"

cd "$SOURCE"
python3 verify/polyglot_validate.py

test -s ontology/platform.ttl
test -s ontology/platform-shapes.ttl
test -s ontology/vision2030.ttl
test -s queries/extract-vision2030.rq
test -s rules/escrow.n3
test -s rules/settlement.dl
grep -q 'ORDER BY' queries/extract-vision2030.rq
! grep -qi 'SELECT[[:space:]]*\*' queries/extract-vision2030.rq
! grep -Eqi '\b(exec|spawn|system|socket)\b' rules/escrow.n3
! grep -Eqi '\b(exec|spawn|system|socket)\b' rules/settlement.dl

cd "$GENERATED"
node ../verify/chicago-tdd.mjs
mkdir -p .ggen/evidence
cp "$SOURCE/.ggen/evidence/polyglot-python.json" .ggen/evidence/polyglot-python.json
cat > .ggen/evidence/polyglot-shell.json <<'JSON'
{
  "schema": "ggen.cyberpunk-tv.polyglot-shell.v1",
  "language": "bash",
  "executed": ["python", "javascript", "sparql", "n3", "datalog", "rdf", "shacl", "rust", "wasm"],
  "standing": "PARTIAL_ALIVE"
}
JSON
printf '%s\n' '{"standing":"PARTIAL_ALIVE","polyglot":"EXECUTED"}'
