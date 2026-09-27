"""Content-addressed local projection store."""
from dataclasses import dataclass
from pathlib import Path
import json
from .model import ConsumerProjection,Provenance,canonical_set
@dataclass
class ProjectionStore:
    root:Path
    def __post_init__(self): self.root.mkdir(parents=True,exist_ok=True)
    def put(self,p):
        target=self.root/p.digest()[:2]/f"{p.digest()}.json"; target.parent.mkdir(parents=True,exist_ok=True)
        if not target.exists(): target.write_text(p.canonical_json()+"\n",encoding="utf-8")
        return target
    def put_many(self,ps): return [self.put(p) for p in canonical_set(ps)]
    def get(self,digest):
        raw=json.loads((self.root/digest[:2]/f"{digest}.json").read_text())
        raw["provenance"]=Provenance(**raw["provenance"]); return ConsumerProjection(**raw)
    def iter_digests(self):
        for path in sorted(self.root.glob("*/*.json")): yield path.stem
    def manifest(self):
        ds=list(self.iter_digests()); return {"version":1,"count":len(ds),"digests":ds}
