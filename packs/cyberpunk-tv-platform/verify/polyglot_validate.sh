#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
mkdir -p .ggen/evidence

python3 verify/polyglot_validate.py
node verify/chicago-tdd.mjs

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

cat > .ggen/evidence/polyglot-shell.json <<'JSON'
{
  "schema": "ggen.cyberpunk-tv.polyglot-shell.v1",
  "language": "bash",
  "executed": ["python", "javascript", "sparql", "n3", "datalog", "rdf", "shacl", "rust", "wasm"],
  "standing": "PARTIAL_ALIVE"
}
JSON
printf '%s\n' '{"standing":"PARTIAL_ALIVE","polyglot":"EXECUTED"}'
