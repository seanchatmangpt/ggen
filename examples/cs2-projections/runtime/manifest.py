"""Fleet-consumable manifest for constructed CS2 bundles."""
from dataclasses import dataclass,asdict
from hashlib import sha256
import json
@dataclass(frozen=True)
class Artifact:
    path:str; digest:str; media_type:str; consumer:str|None=None
@dataclass(frozen=True)
class BundleManifest:
    subject:str; authority_ceiling:str; artifacts:tuple; version:int=1
    def canonical_json(self): return json.dumps(asdict(self),sort_keys=True,separators=(",",":"))
    def digest(self): return sha256(self.canonical_json().encode()).hexdigest()
    def select(self,consumer): return tuple(a for a in self.artifacts if a.consumer in (None,consumer))
