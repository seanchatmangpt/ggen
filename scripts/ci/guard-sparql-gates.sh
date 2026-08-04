#!/usr/bin/env bash
# guard-sparql-gates — makes .specify/gates/*.rq an ENFORCED invariant, not
# just a docs/gates/*.md claim about one.
#
# Root cause this guard closes (retrofit:GeneratedTableDriftManifested,
# mirrored from scripts/ci/guard-pack-count.sh's own header): six of these
# gate files were committed in 6b512449f declaring no `PREFIX` line at all
# despite using prefixed names (cmx:/cli:), so every one of them was
# syntactically invalid SPARQL -- confirmed via the real parser
# (`ggen_graph::check_sparql_syntax`, the same oxigraph `SparqlEvaluator`
# ggen-lsp's sparql_analyzer already uses): "error at 13:13: expected
# OPTIONAL". Nothing executed any of them; docs/gates/*.md nonetheless
# published them as enforced invariants. This guard is the actual fix: it
# does not merely re-declare the gates correct (that would drift again the
# next time someone edits the ontology's `cmx:askBody` literals or the
# `proof-gate.rq.tera` template that renders them) -- it re-parses and
# re-executes every gate file for real, on every run, the same discipline
# every other guard-*.sh in this pre-commit chain already applies to its own
# fact class.
#
# What this guard actually checks, and what it deliberately does NOT:
#   1. SYNTAX -- every *.rq file directly under $GATES_DIR is parsed with the
#      real SPARQL parser. A parse failure is always BUILD_BROKEN.
#   2. SEMANTICS -- six gates (every-action-has-binding,
#      every-binding-has-output-pattern, every-binding-has-template,
#      every-command-has-handler, every-generator-has-action,
#      no-orphan-actions) are additionally EXECUTED against a Store loaded
#      from $ONTOLOGY_FILES (this repo's own cmx:/cli: ontology data, the
#      same files ggen.toml's [ontology].imports already lists). ASK ->
#      false, or any SELECT/CONSTRUCT violation row, is BUILD_BROKEN.
#   3. The remaining four gates (cross-pack-contamination.rq and the three
#      l5-*.rq gates) get syntax-check only from this guard: their real data
#      is a full pack-composed graph assembled by ggen-engine's [law]
#      pipeline at `ggen sync run` time, which this repo's own ggen.toml
#      does not currently wire up at all (no [law] section) -- confirmed by
#      grep. Their logic already has real Chicago-TDD execution proof
#      against that pipeline in
#      crates/ggen-engine/tests/pack_e2e.rs::cross_pack_conflicting_rdf_type_aborts_sync_citing_contamination_gate
#      and are exercised with a COPY of the real gate file, not a
#      reimplementation. Reconstructing full pack composition inside this
#      standalone guard would risk a second, divergent graph-assembly
#      implementation for a check that already has one -- not attempted
#      here. See crates/ggen-graph/src/bin/sparql_gate_check.rs's own module
#      doc for the same disclosure in the code that enforces it.
#
# Exit non-zero with a BUILD_BROKEN: line and the runner's own per-gate
# PASS/FAIL/SYNTAX_OK output on any parse failure or invariant violation;
# print an ALIVE: line otherwise.
set -euo pipefail

GATES_DIR="${SPARQL_GATES_DIR:-.specify/gates}"
# Space-separated list of Turtle files providing the cmx:/cli: data the six
# semantically-checked gates above query against -- the same files
# ggen.toml's [ontology].imports lists for this same vocabulary.
: "${SPARQL_GATES_ONTOLOGY_FILES:=.specify/combinatorial.ttl .specify/combinatorial-instances.ttl .specify/cli-commands.ttl}"
read -r -a ONTOLOGY_FILES <<< "$SPARQL_GATES_ONTOLOGY_FILES"

if [ ! -d "$GATES_DIR" ]; then
  echo "BUILD_BROKEN: $GATES_DIR directory not found"
  exit 1
fi

for f in "${ONTOLOGY_FILES[@]}"; do
  if [ ! -f "$f" ]; then
    echo "BUILD_BROKEN: ontology file $f not found (SPARQL_GATES_ONTOLOGY_FILES)"
    exit 1
  fi
done

output="$(cargo run --quiet -p ggen-graph --bin sparql_gate_check -- "$GATES_DIR" "${ONTOLOGY_FILES[@]}" 2>&1)" && status=0 || status=$?

echo "$output"

if [ "$status" -ne 0 ]; then
  echo "BUILD_BROKEN: one or more .specify/gates/*.rq files failed to parse or execute -- see FAIL lines above"
  exit 1
fi

echo "ALIVE: all *.rq files under $GATES_DIR parse; the six gates with known ontology data (every-action-has-binding, every-binding-has-output-pattern, every-binding-has-template, every-command-has-handler, every-generator-has-action, no-orphan-actions) evaluate their real invariant against ${ONTOLOGY_FILES[*]}"
