"""Semantic delta between canonical projection sets."""
from dataclasses import dataclass
from .model import canonical_set
@dataclass(frozen=True)
class ProjectionDelta:
    added:tuple; removed:tuple; changed:tuple; unchanged:tuple
    @property
    def empty(self): return not (self.added or self.removed or self.changed)
def semantic_diff(before,after):
    left={(p.work_id,p.consumer):p for p in canonical_set(before)}
    right={(p.work_id,p.consumer):p for p in canonical_set(after)}
    added=[]; removed=[]; changed=[]; unchanged=[]
    for key in sorted(set(left)|set(right)):
        if key not in left: added.append(right[key].digest())
        elif key not in right: removed.append(left[key].digest())
        elif left[key].digest()!=right[key].digest(): changed.append((left[key].digest(),right[key].digest()))
        else: unchanged.append(left[key].digest())
    return ProjectionDelta(tuple(added),tuple(removed),tuple(changed),tuple(unchanged))
