"""Typed runtime model for exact-subject CS2 projections."""
from dataclasses import dataclass,field,asdict
from hashlib import sha256
import json
CS2_SUBJECT="https://chatman.ai/cs2#RFC-CS2-001"
class ProjectionError(ValueError): pass
class SubjectMismatch(ProjectionError): pass
class AuthorityExceeded(ProjectionError): pass
@dataclass(frozen=True,order=True)
class Provenance:
    source:str; sha256:str; observed_at:str|None=None
    def __post_init__(self):
        if not self.source or len(self.sha256)!=64: raise ProjectionError("valid provenance required")
@dataclass(frozen=True,order=True)
class ConsumerProjection:
    subject:str; authority_ceiling:str; consumer:str; work_id:str; expected_projection:str; provenance:Provenance
    attributes:dict=field(default_factory=dict)
    def __post_init__(self):
        if self.subject!=CS2_SUBJECT: raise SubjectMismatch(self.subject)
        if self.authority_ceiling!="CONSTRUCT": raise AuthorityExceeded(self.authority_ceiling)
        if not all((self.consumer,self.work_id,self.expected_projection)): raise ProjectionError("identity fields required")
    def canonical(self):
        v=asdict(self); v["attributes"]=dict(sorted(self.attributes.items())); return v
    def canonical_json(self): return json.dumps(self.canonical(),sort_keys=True,separators=(",",":"))
    def digest(self): return sha256(self.canonical_json().encode()).hexdigest()
def canonical_set(items):
    rows=sorted(items,key=lambda x:(x.work_id,x.consumer,x.digest())); seen=set()
    for row in rows:
        key=(row.work_id,row.consumer)
        if key in seen: raise ProjectionError(f"duplicate identity: {key}")
        seen.add(key)
    return rows
