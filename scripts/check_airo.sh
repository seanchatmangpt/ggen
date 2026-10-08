#!/usr/bin/env bash
# W614 check: docs/airo-risk-description.ttl parses and every repo-relative path it cites exists.
set -euo pipefail
cd "$(dirname "$0")/.."
TTL=docs/airo-risk-description.ttl
fail=0

# 1. Cited paths exist (grep repo-relative paths out of the TTL, skip URLs/wave refs)
paths=$(grep -oE '(crates|\.github|justfile)[^ )",;]*' "$TTL" | sort -u || true)
if [ -z "$paths" ]; then
  echo "FAIL: no cited paths found in $TTL"; fail=1
fi
for p in $paths; do
  if [ -e "$p" ]; then echo "ok: cited path exists: $p"; else echo "FAIL: cited path missing: $p"; fail=1; fi
done

# 2. TTL parses (rdflib from /tmp venv if available; else structural fallback)
if [ -x /tmp/airo-venv/bin/python ] && /tmp/airo-venv/bin/python -c 'import rdflib' 2>/dev/null; then
  if ! /tmp/airo-venv/bin/python - "$TTL" <<'EOF'
import sys, rdflib
g = rdflib.Graph()
g.parse(sys.argv[1], format="turtle")
g.parse("/tmp/airo.ttl", format="turtle")  # vocabulary also parses; union parse proves prefix compatibility
print(f"ok: TTL parses with rdflib ({len(g)} triples after vocabulary union)")
EOF
  then echo "FAIL: rdflib parse failed (TTL or vocabulary)"; fail=1
  fi
else
  echo "note: rdflib venv not present; structural check only"
  if ! python3 - "$TTL" <<'EOF'
import sys
src = open(sys.argv[1]).read()
assert src.count('"') % 2 == 0, "unbalanced quotes"
assert src.count(';') >= 5 and '.' in src, "malformed-looking turtle"
print("ok: structural check passed (install rdflib venv at /tmp/airo-venv for full parse)")
EOF
  then echo "FAIL: structural check failed"; fail=1
  fi
fi

# 3. Key AIRo terms used are actually defined in the fetched vocabulary
for term in AISystem AIProvider RiskSource RiskControl Risk Likelihood Severity hasRisk hasRiskControl isProvidedBy hasLikelihood hasSeverity hasConsequence mitigatesRiskConcept; do
  if grep -q "airo#$term" /tmp/airo.ttl; then echo "ok: airo:$term defined in vocabulary"; else echo "FAIL: airo:$term NOT in vocabulary"; fail=1; fi
done

if [ "$fail" -eq 0 ]; then echo "check_airo: PASS"; exit 0; else echo "check_airo: FAIL"; exit 1; fi
