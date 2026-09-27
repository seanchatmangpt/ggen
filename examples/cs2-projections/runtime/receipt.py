"""Replayable receipts for CS2 construction."""
from dataclasses import dataclass,asdict
from hashlib import sha256
import json
from .model import canonical_set
@dataclass(frozen=True)
class ProjectionReceipt:
    subject:str; authority_ceiling:str; input_digest:str; output_digest:str; projection_count:int; consumer_ids:tuple; constructor:str; version:int=1
    def canonical_json(self): return json.dumps(asdict(self),sort_keys=True,separators=(",",":"))
    def receipt_id(self): return sha256(self.canonical_json().encode()).hexdigest()
def digest_rows(rows):
    h=sha256()
    for row in rows: h.update(row.encode()); h.update(bytes([10]))
    return h.hexdigest()
def issue_receipt(projections,outputs,constructor):
    ps=canonical_set(projections); rows=list(outputs)
    return ProjectionReceipt(ps[0].subject if ps else "",ps[0].authority_ceiling if ps else "CONSTRUCT",digest_rows(p.canonical_json() for p in ps),digest_rows(rows),len(ps),tuple(p.consumer for p in ps),constructor)
def replay_matches(receipt,projections,outputs): return issue_receipt(projections,outputs,receipt.constructor)==receipt
