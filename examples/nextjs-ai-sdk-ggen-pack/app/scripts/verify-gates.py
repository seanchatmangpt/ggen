from pathlib import Path
from rdflib import Graph

root = Path(__file__).resolve().parents[1]
pack = root.parent / "pack"

graph = Graph()
graph.parse(pack / "ontology.ttl")
graph.parse(root / "schema" / "domain.ttl")

failed = False
for gate in sorted((pack / "gates").glob("*.rq")):
    rows = list(graph.query(gate.read_text()))
    print(f"{gate.name}: {len(rows)} violation(s)")
    for row in rows: print("  ", tuple(str(v) for v in row))
    failed = failed or bool(rows)

if failed:
    raise SystemExit(1)
print("ALIVE: all admission gates returned zero rows")
