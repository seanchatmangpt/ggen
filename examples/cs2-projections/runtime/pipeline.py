"""Dependency-closed CS2 projection manufacturing pipeline."""
from dataclasses import dataclass,asdict
import json
from .model import canonical_set
from .adapters import jira,a2a
from .receipt import issue_receipt
@dataclass(frozen=True)
class Materialization: kind:str; rows:tuple; receipt:object
def encode(value): return json.dumps(asdict(value),sort_keys=True,separators=(",",":"))
def materialize(projections,kind):
    ps=canonical_set(projections)
    if kind=="jira": rows=tuple(encode(jira(p)) for p in ps)
    elif kind=="a2a": rows=tuple(encode(a2a(p)) for p in ps)
    elif kind=="canonical": rows=tuple(p.canonical_json() for p in ps)
    else: raise ValueError(f"unsupported materialization: {kind}")
    return Materialization(kind,rows,issue_receipt(ps,rows,f"cs2:{kind}:v1"))
def bundle(projections):
    ps=canonical_set(projections); mats=[materialize(ps,k) for k in ("canonical","jira","a2a")]
    return {"subject":ps[0].subject if ps else None,"authority_ceiling":"CONSTRUCT","projections":[p.canonical() for p in ps],"materializations":[{"kind":m.kind,"rows":list(m.rows),"receipt":asdict(m.receipt),"receipt_id":m.receipt.receipt_id()} for m in mats]}
