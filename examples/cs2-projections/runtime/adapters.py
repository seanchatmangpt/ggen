"""Pure construction adapters. No network actuation."""
from dataclasses import dataclass,asdict
from .model import canonical_set
@dataclass(frozen=True)
class JiraEnvelope: external_id:str; fields:dict; properties:tuple
@dataclass(frozen=True)
class A2AEnvelope: id:str; subject:str; task:str; payload:dict; provenance:dict
def jira(p):
    return JiraEnvelope(p.work_id,{"summary":f"{p.work_id}: {p.expected_projection}","description":f"Consumer={p.consumer}; ExactSubject={p.subject}","labels":["cs2","semantic-projection","construct-only"]},({"key":"chatman.cs2.subject","value":p.subject},{"key":"chatman.cs2.consumer","value":p.consumer},{"key":"chatman.cs2.digest","value":p.digest()}))
def a2a(p):
    return A2AEnvelope(p.work_id,p.subject,"construct-consumer-projection",{"expected_projection":p.expected_projection,"attributes":dict(p.attributes)},asdict(p.provenance))
def ndjson(projections,adapter):
    import json
    rows=[json.dumps(asdict(adapter(p)),sort_keys=True,separators=(",",":")) for p in canonical_set(projections)]
    return "\n".join(rows)+("\n" if rows else "")
