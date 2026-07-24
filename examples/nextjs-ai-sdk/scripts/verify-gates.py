from pathlib import Path
import json
from rdflib import Graph, Namespace, RDF, Literal

root = Path(__file__).resolve().parents[1]
pack = root / "pack"
nai = Namespace("https://ggen.dev/packs/nextjs-ai#")

def load(extra=None):
    graph = Graph(); graph.parse(pack / "ontology.ttl"); graph.parse(root / "schema/domain.ttl")
    if extra: graph.parse(data=extra, format="turtle")
    return graph

def run(graph):
    violations = {}
    for gate in sorted((pack / "gates").glob("*.rq")):
        rows = list(graph.query(gate.read_text()))
        violations[gate.name] = rows
        print(f"{gate.name}: {len(rows)} violation(s)")
    return violations

valid = load(); violations = run(valid)
if any(violations.values()): raise SystemExit("valid domain refused")
for tool in valid.subjects(RDF.type, nai.Tool):
    values = list(valid.objects(tool, nai.inputSchemaJson))
    if len(values) != 1: raise SystemExit(f"{tool} has {len(values)} input schemas")
    schema = json.loads(str(values[0]))
    if schema.get("type") != "object": raise SystemExit(f"{tool} input schema is not an object")

negative_graph = load()
negative_graph.parse(root / "tests/negative/mutating-tool-without-approval.ttl")
mutations = run(negative_graph)["040_mutation_approval.rq"]
if len(mutations) != 1 or str(mutations[0][0]) != "https://example.ggen.dev/bad#unsafe": raise SystemExit("negative mutation case was not decisively refused")
print(json.dumps({"status":"PARTIAL_ALIVE","scope":"rdf-admission","validGates":len(violations),"negativeMutationRefused":True}))
