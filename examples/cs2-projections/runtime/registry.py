"""Consumer registry for deterministic projection selection."""
from dataclasses import dataclass
from .model import ProjectionError
@dataclass(frozen=True)
class ConsumerRegistration:
    iri:str; work_id:str; projection_kind:str; enabled:bool=True
class ConsumerRegistry:
    def __init__(self,items=()): self.items={}; [self.register(x) for x in items]
    def register(self,r):
        if r.iri in self.items: raise ProjectionError("duplicate consumer")
        self.items[r.iri]=r; return self
    def require(self,iri):
        if iri not in self.items: raise ProjectionError("unknown consumer")
        r=self.items[iri]
        if not r.enabled: raise ProjectionError("disabled consumer")
        return r
    def route(self,p):
        r=self.require(p.consumer)
        if r.work_id!=p.work_id: raise ProjectionError("work mismatch")
        return r.projection_kind
    def manifest(self): return [vars(r) for r in sorted(self.items.values(),key=lambda x:x.iri)]
